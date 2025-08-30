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

import gleam/bool
import gleam/dict.{type Dict}
import gleam/erlang/process.{type Monitor, type Pid, type Selector, type Subject}
import gleam/int
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
    /// Times used for restart delays: (start_time, prev_delay).
    times: Dict(String, #(Int, Int)),
  )
}

/// An individual registered actor.
///
type Actor(wrap) {
  Actor(actor: wrap, pid: Pid, monitor: Monitor)
}

/// Starts the registry.
///
/// Note: Gleam actors are linked to the parent process by default.
///
pub fn start() -> Result(Subject(Message(wrap)), actor.StartError) {
  actor.new_with_initialiser(100, fn(subj) {
    let selector =
      process.new_selector()
      |> process.select(subj)
    State(self: subj, selector:, actors: dict.new(), times: dict.new())
    |> actor.initialised
    |> actor.selecting(selector)
    |> actor.returning(subj)
    |> Ok
  })
  |> actor.on_message(handle_message)
  |> actor.start
  |> result.map(fn(started) { started.data })
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
/// Returns the subject unchanged, for ease of use in pipelines.
pub fn register(
  in actor: Subject(Message(wrap)),
  key variant: fn(Subject(msg)) -> wrap,
  subject subj: Subject(msg),
) -> Result(Subject(msg), Nil) {
  use pid <- result.try(process.subject_owner(subj))

  let wrapped = variant(subj)
  let key = get_act_variant_name(wrapped)

  actor.send(actor, Register(key, wrapped, pid))
  Ok(subj)
}

/// Retrieves an actor, using the actors wrapper type constructor as a key.  If
/// the actor is not present in the registry, returns `None`.
///
pub fn try_get(
  in actor: Subject(Message(wrap)),
  key variant: fn(Subject(msg)) -> wrap,
) -> Result(wrap, Nil) {
  let key = cons_variant_name(variant)
  actor.call(actor, 10, TryGet(_, key))
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
  in actor: Subject(Message(wrap)),
  key variant: fn(Subject(msg)) -> wrap,
  timeout_ms timeout: Int,
) -> wrap {
  let key = cons_variant_name(variant)
  actor.call(actor, timeout, Require(_, key, timeout))
}

/// A DelayFunc accepts two parameters:
///
/// - The uptime of the previous incarnation of this actor, in milliseconds.
/// - The previous delay in milliseconds, starting with 0.
pub type DelayFunc =
  fn(Int, Int) -> Int

/// Ignores inputs, always delays the same amount of time.
pub fn always_delay(ms delay: Int) -> DelayFunc {
  fn(_prev_up_ms: Int, _prev_delay_ms: Int) -> Int { delay }
}

/// Starts at initial_ms, doubling with each subsequent call.  If max_ms is
/// provided, delay will not exceed that value.  If the previous incarnation
/// lived for at least good_ms, the delay will be reset to initial_ms.
pub fn exponential_delay(
  good_ms good: Int,
  initial_ms initial: Int,
  max_ms max: Option(Int),
) -> DelayFunc {
  fn(prev_up_ms: Int, prev_delay_ms: Int) -> Int {
    use <- bool.guard(
      when: prev_delay_ms == 0 || prev_up_ms >= good,
      return: initial,
    )

    // Double previous delay and clamp to max if provided.
    let delay = prev_delay_ms * 2
    case max {
      Some(max) -> int.min(max, delay)
      None -> delay
    }
  }
}

/// Tracks and delays restarts for the actor `key` using the provided
/// `DelayFunc`.
///
/// See `always_delay` and `exponential_delay` for built-in delay functions,
/// or write your own for custom timings.
pub fn restart_delay(
  in actor: Subject(Message(wrap)),
  key variant: fn(Subject(msg)) -> wrap,
  with func: fn(Int, Int) -> Int,
) {
  let key = cons_variant_name(variant)
  let delay = actor.call(actor, 10, Delay(_, key, func))
  case delay {
    0 -> Nil
    ms -> process.sleep(ms)
  }
}

/// Singularity's message/Subject type.
///
pub opaque type Message(wrap) {
  TryGet(reply_with: Subject(Result(wrap, Nil)), key: String)
  Require(reply_with: Subject(wrap), key: String, timeout: Int)
  Register(key: String, wrapped: wrap, pid: Pid)
  Delay(reply_with: Subject(Int), key: String, func: DelayFunc)
  ActorExit(key: String, pdown: process.Down)
  Shutdown
}

fn handle_message(
  state: State(wrap),
  message: Message(wrap),
) -> actor.Next(State(wrap), Message(wrap)) {
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

    Delay(reply_with, key, delay_fn) -> {
      let now = system_time(Millisecond)
      let delay = case dict.get(state.times, key) {
        Error(_) -> {
          // First start, no delay required.
          0
        }
        Ok(#(start_time, prev_delay)) -> {
          // A restart, calculate delay using provided fn.
          let uptime = now - start_time - prev_delay |> int.max(0)
          delay_fn(uptime, prev_delay)
        }
      }

      actor.send(reply_with, delay)
      let value = #(now, delay)
      let times = dict.insert(state.times, key, value)
      State(..state, times:) |> actor.continue
    }

    ActorExit(key, pdown) -> {
      case pdown {
        process.ProcessDown(_, pid, _) -> {
          // Remove the actor from the registry.
          let state = remove(state, key, Some(pid))

          state
          |> actor.continue()
          |> actor.with_selector(state.selector)
        }
        process.PortDown(_, _, _) -> {
          actor.continue(state)
        }
      }
    }

    Shutdown -> {
      actor.stop()
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
  let monitor = process.monitor(pid)

  // TODO Look into `select_monitors` instead
  let selector =
    state.selector
    |> process.select_specific_monitor(monitor, ActorExit(key: key, pdown: _))

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
    |> process.select(self)

  dict.fold(over: actors, from: base_selector, with: fn(selector, key, actor) {
    process.select_specific_monitor(selector, actor.monitor, ActorExit(
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
) -> actor.Next(State(wrap), a) {
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

/// Extracts the name of this variant from the constructor.
///
@external(erlang, "singularity_ffi", "cons_variant_name")
fn cons_variant_name(varfn: fn(Subject(msg)) -> wrap) -> String

/// Extracts the name of this variant from the caller's actor wrapper.
///
@external(erlang, "singularity_ffi", "get_act_variant_name")
fn get_act_variant_name(wrapped: wrap) -> String

/// Returns the current OS system time.
///
/// <https://erlang.org/doc/apps/erts/time_correction.html#OS_System_Time>
@external(erlang, "os", "system_time")
pub fn system_time(a: TimeUnit) -> Int

pub type TimeUnit {
  Second
  Millisecond
  Microsecond
  Nanosecond
}
