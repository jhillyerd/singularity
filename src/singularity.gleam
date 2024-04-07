//// Registry for shared singleton actors.

import gleam/dict.{type Dict}
import gleam/dynamic
import gleam/erlang/atom
import gleam/erlang/process.{
  type Pid, type ProcessMonitor, type Selector, type Subject,
}
import gleam/option.{type Option, None, Some}
import gleam/otp/actor

const get_timeout_ms = 5000

const get_retry_delay_ms = 100

type State(act) {
  State(
    /// Subject used to send retries, and rebuild selector.
    self: Subject(Message(act)),
    /// Current message selector, updated by process monitoring.
    selector: Selector(Message(act)),
    /// Registered actors.
    actors: Dict(String, Actor(act)),
  )
}

type Actor(act) {
  Actor(actor: act, pid: Pid, monitor: ProcessMonitor)
}

/// Starts the registry actor.  Gleam actors are linked to the parent process
/// by default.
pub fn start() -> Result(Subject(Message(act)), actor.StartError) {
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
pub fn stop(actor: Subject(Message(act))) {
  actor.send(actor, Shutdown)
}

/// Registers an actor, using the actors type constructor as a key.
///
pub fn put(
  into actor: Subject(Message(act)),
  key variant: fn(Subject(msg)) -> act,
  insert subj: Subject(msg),
) -> Subject(msg) {
  let pid = process.subject_owner(subj)
  let value = variant(subj)
  let key = get_variant_name(value)

  actor.send(actor, Put(key, value, pid))
  subj
}

/// Retrieves an actor, using the actors type constructor as a key.  If the
/// actor is not present in the registry, the request will be retried for
/// `get_timeout_ms` before crashing.
///
pub fn get(
  into actor: Subject(Message(act)),
  key variant: fn(Subject(msg)) -> act,
) -> act {
  // Use a fake Subject to extract the variant name from the constructor.
  // Coerce safety: no messages will be sent to the temporary Subject.
  let key =
    #(Nil, Nil)
    |> dynamic.from
    |> dynamic.unsafe_coerce
    |> variant
    |> get_variant_name

  actor.call(actor, Get(_, key, get_timeout_ms), get_timeout_ms)
}

pub opaque type Message(act) {
  Get(reply_with: Subject(act), key: String, timeout: Int)
  Put(key: String, value: act, pid: Pid)
  ActorExit(key: String, pdown: process.ProcessDown)
  Shutdown
}

fn loop(
  message: Message(act),
  state: State(act),
) -> actor.Next(Message(act), State(act)) {
  case message {
    Get(reply_with, key, timeout) ->
      get_with_retry(state, key, reply_with, timeout)

    Put(key, value, pid) -> {
      let state = register(state, key, value, pid)
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

fn register(state: State(act), key: String, value: act, pid: Pid) -> State(act) {
  // Remove current actor if present.
  let state = remove(state, key, None)

  // Monitor process for exits.
  let monitor = process.monitor_process(pid)
  let selector =
    state.selector
    |> process.selecting_process_down(monitor, ActorExit(key: key, pdown: _))

  // Insert into registry.
  let actor = Actor(actor: value, pid: pid, monitor: monitor)
  let actors = dict.insert(state.actors, for: key, insert: actor)

  State(..state, actors: actors, selector: selector)
}

/// Demonitors and removes the specified actor, then rebuilds our selector.
///
/// - when_pid: Specify `Some(pid)` to prevent a potential race condition from
///   removing a newly registered actor instead of the one it replaced.
///   Specifying `None` will always remove the actor.
///
fn remove(state: State(act), key: String, when_pid: Option(Pid)) -> State(act) {
  let rm = fn(actor: Actor(act)) {
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
fn build_selector(
  self: Subject(Message(act)),
  actors: Dict(String, Actor(act)),
) -> Selector(Message(act)) {
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
  state: State(act),
  key: String,
  reply_with: Subject(act),
  millis timeout: Int,
) -> actor.Next(Message(act), State(act)) {
  case dict.get(state.actors, key) {
    Ok(value) -> {
      // Actor found, respond to caller.
      actor.send(reply_with, value.actor)
      Nil
    }

    Error(Nil) -> {
      // Actor not found, retry this request after a delay.
      case timeout > get_retry_delay_ms {
        True -> {
          process.send_after(
            state.self,
            get_retry_delay_ms,
            Get(reply_with, key, timeout - get_retry_delay_ms),
          )
          Nil
        }
        False -> Nil
      }
    }
  }

  actor.continue(state)
}

/// Extracts the name of this variant from the caller's actor type.
fn get_variant_name(value: act) -> String {
  let assert Ok(atom) =
    value
    |> dynamic.from
    |> dynamic.element(0, atom.from_dynamic)

  atom.to_string(atom)
}
