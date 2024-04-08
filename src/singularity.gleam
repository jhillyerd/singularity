//// Singularity is a registry for shared singleton actors, in Gleam.
////
//// When an actor's process exits, it will be removed from the registry
//// automatically.  Fetching an actor using the [require](#require) function
//// will block until that actor has been registered.  These two properties
//// allow OTP supervisors to start your actors only when their dependencies
//// are ready.
////
//// ### Example Usage
////
//// ```
//// import actor_a
//// import actor_b
////
//// // Define a wrapper type for the actors (Subjects) you want register.
//// type Actors {
////   ActorA(Subject(actor_a.Message))
////   ActorB(Subject(actor_b.Message))
//// }
////
//// pub fn main() {
////   let assert Ok(registry) = singularity.start()
////   let assert Ok(actor_a_subj) = actor_a.start()
////   let assert Ok(actor_b_subj) = actor_b.start()
////
////   // Register the actors specifying your wrapper (`Actors`) variant.
////   singularity.register(registry, ActorA, actor_a_subj)
////   singularity.register(registry, ActorB, actor_b_subj)
////
////   // Retrieve registered actors, each will be returned via your
////   // wrapper type.
////   let assert ActorA(got_a) =
////     singularity.require(registry, ActorA, timeout_ms: 1000)
////   let assert ActorB(got_b) =
////     singularity.require(registry, ActorB, timeout_ms: 1000)
//// }
//// ```
////
//// The example above is for illustrative purposes only; it will not compile.
//// Please see the README and tests for concrete examples.

import gleam/dict.{type Dict}
import gleam/dynamic
import gleam/erlang/atom
import gleam/erlang/process.{
  type Pid, type ProcessMonitor, type Selector, type Subject,
}
import gleam/option.{type Option, None, Some}
import gleam/otp/actor
import gleam/result

const require_retry_delay_ms = 100

/// Registry state, parameterized by the users actor wrapper type.
///
type State(wrap) {
  State(
    /// Subject used to send retries, and rebuild selector.
    self: Subject(Message(wrap)),
    /// Current message selector, updated by process monitoring.
    selector: Selector(Message(wrap)),
    /// Registered actors.
    actors: Dict(String, Actor(wrap)),
  )
}

/// An individual registered actor.
///
type Actor(wrap) {
  Actor(actor: wrap, pid: Pid, monitor: ProcessMonitor)
}

/// Starts the registry.
///
/// Note: Gleam actors are linked to the parent process by default.
///
pub fn start() -> Result(Subject(Message(wrap)), actor.StartError) {
  actor.start_spec(actor.Spec(
    init: fn() {
      let self = process.new_subject()
      let selector =
        process.new_selector()
        |> process.selecting(self, fn(msg) { msg })
      let state = State(self: self, selector: selector, actors: dict.new())

      actor.Ready(state, selector)
    },
    loop: loop,
    init_timeout: 100,
  ))
}

/// Shuts down the registry.  Does not affect registered actors.
///
pub fn stop(actor: Subject(Message(wrap))) {
  actor.send(actor, Shutdown)
}

/// Registers an actor, using the actors wrapper type constructor as a key.
///
/// Registered actor processes will be monitored.  Processes that exit will
/// be removed from the registry automatically.
///
pub fn register(
  into actor: Subject(Message(wrap)),
  key variant: fn(Subject(msg)) -> wrap,
  insert subj: Subject(msg),
) -> Subject(msg) {
  let pid = process.subject_owner(subj)
  let wrapped = variant(subj)
  let key = get_act_variant_name(wrapped)

  actor.send(actor, Register(key, wrapped, pid))
  subj
}

/// Retrieves an actor, using the actors wrapper type constructor as a key.  If
/// the actor is not present in the registry, returns `None`.
///
pub fn try_get(
  into actor: Subject(Message(wrap)),
  key variant: fn(Subject(msg)) -> wrap,
) -> Result(wrap, Nil) {
  let key = cons_variant_name(variant)
  actor.call(actor, TryGet(_, key), 10)
}

/// Retrieves an actor, using the actors wrapper type constructor as a key.  If
/// the actor is not present in the registry, the request will be retried
/// every 100ms for a total of `timeout_ms` before crashing.
///
/// If your actors need to establish network connections during startup,
/// consider using long (5000ms+) timeouts for them and any actors that
/// depend on them.
///
pub fn require(
  into actor: Subject(Message(wrap)),
  key variant: fn(Subject(msg)) -> wrap,
  timeout_ms timeout: Int,
) -> wrap {
  let key = cons_variant_name(variant)
  actor.call(actor, Require(_, key, timeout), timeout)
}

/// Singularity's message/Subject type.
///
pub opaque type Message(wrap) {
  TryGet(reply_with: Subject(Result(wrap, Nil)), key: String)
  Require(reply_with: Subject(wrap), key: String, timeout: Int)
  Register(key: String, wrapped: wrap, pid: Pid)
  ActorExit(key: String, pdown: process.ProcessDown)
  Shutdown
}

fn loop(
  message: Message(wrap),
  state: State(wrap),
) -> actor.Next(Message(wrap), State(wrap)) {
  case message {
    TryGet(reply_with, key) -> {
      state.actors
      |> dict.get(key)
      |> result.map(fn(actor) { actor.actor })
      |> actor.send(reply_with, _)

      actor.continue(state)
    }

    Require(reply_with, key, timeout) ->
      get_with_retry(state, key, reply_with, timeout)

    Register(key, wrapped, pid) -> {
      let state = handle_register(state, key, wrapped, pid)
      state
      |> actor.continue()
      |> actor.with_selector(state.selector)
    }

    ActorExit(key, pdown) -> {
      let state = remove(state, key, Some(pdown.pid))

      state
      |> actor.continue()
      |> actor.with_selector(state.selector)
    }

    Shutdown -> {
      actor.Stop(process.Normal)
    }
  }
}

fn handle_register(
  state: State(wrap),
  key: String,
  wrapped: wrap,
  pid: Pid,
) -> State(wrap) {
  // Remove current actor if present.
  let state = remove(state, key, None)

  // Monitor process for exits.
  let monitor = process.monitor_process(pid)
  let selector =
    state.selector
    |> process.selecting_process_down(monitor, ActorExit(key: key, pdown: _))

  // Insert into registry.
  let actor = Actor(actor: wrapped, pid: pid, monitor: monitor)
  let actors = dict.insert(state.actors, for: key, insert: actor)

  State(..state, actors: actors, selector: selector)
}

/// Demonitors and removes the specified actor, then rebuilds our selector.
///
/// - when_pid: Specify `Some(pid)` to prevent a potential race condition from
///   removing a newly registered actor instead of the one it replaced.
///   Specifying `None` will always remove the actor.
///
fn remove(state: State(wrap), key: String, when_pid: Option(Pid)) -> State(wrap) {
  let rm = fn(actor: Actor(wrap)) {
    process.demonitor_process(actor.monitor)
    let actors = dict.delete(state.actors, key)
    let selector = build_selector(state.self, state.actors)

    State(..state, actors: actors, selector: selector)
  }

  case dict.get(state.actors, key) {
    Ok(Actor(actor: _, pid: pid, monitor: _) as actor) if when_pid == Some(pid) ->
      rm(actor)

    Ok(actor) if when_pid == None -> rm(actor)

    Ok(_) -> state

    Error(Nil) -> state
  }
}

/// Rebuilds the selector for the current set of monitored actors.
///
fn build_selector(
  self: Subject(Message(wrap)),
  actors: Dict(String, Actor(wrap)),
) -> Selector(Message(wrap)) {
  let base_selector =
    process.new_selector()
    |> process.selecting(self, fn(msg) { msg })

  dict.fold(over: actors, from: base_selector, with: fn(selector, key, actor) {
    process.selecting_process_down(selector, actor.monitor, ActorExit(
      key: key,
      pdown: _,
    ))
  })
}

fn get_with_retry(
  state: State(wrap),
  key: String,
  reply_with: Subject(wrap),
  millis timeout: Int,
) -> actor.Next(Message(wrap), State(wrap)) {
  case dict.get(state.actors, key) {
    Ok(actor) -> {
      // Actor found, respond to caller.
      actor.send(reply_with, actor.actor)
      Nil
    }

    Error(Nil) -> {
      // Actor not found, retry this request after a delay.
      case timeout > require_retry_delay_ms {
        True -> {
          process.send_after(
            state.self,
            require_retry_delay_ms,
            Require(reply_with, key, timeout - require_retry_delay_ms),
          )
          Nil
        }
        False -> Nil
      }
    }
  }

  actor.continue(state)
}

/// Extracts the name of this variant from the caller's actor wrapper.
///
fn get_act_variant_name(wrapped: wrap) -> String {
  let assert Ok(atom) =
    wrapped
    |> dynamic.from
    |> dynamic.element(0, atom.from_dynamic)

  atom.to_string(atom)
}

/// Extracts the name of this variant from the constructor.
///
fn cons_variant_name(varfn: fn(Subject(msg)) -> wrap) {
  // Use a fake Subject to extract the variant name from the constructor.
  // Coerce safety: no messages will be sent to the temporary Subject.
  #(Nil, Nil)
  |> dynamic.from
  |> dynamic.unsafe_coerce
  |> varfn
  |> get_act_variant_name
}
