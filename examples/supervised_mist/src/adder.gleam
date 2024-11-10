//// Adder is a very simple actor that adds values obtained from two counter actors.

import counter
import gleam/erlang/process.{type Subject}
import gleam/int
import gleam/otp/actor
import singularity

/// Type used to interact with registry.
pub type Actor {
  CounterA(Subject(counter.Message))
  CounterB(Subject(counter.Message))
  Adder(Subject(Message))
}

pub type State {
  State(
    // Singularity registry, note that its Message type is parameterized by
    // our Actor type above.
    registry: Subject(singularity.Message(Actor)),
  )
}

/// Accept two counter actors as dependencies.
pub fn start(registry: Subject(singularity.Message(Actor))) {
  State(registry)
  |> actor.start(handle_message)
}

pub fn next(actor: Subject(Message)) -> String {
  actor.call(actor, Next, 10)
}

pub type Message {
  Next(reply_with: Subject(String))
}

fn handle_message(msg: Message, state: State) {
  // Adder depends on two counters, fetch them. Doing this for every request is
  // not recommended for performance sensitive code.
  //
  // 1. Fetch dependencies for every request: high reliability, but slow.
  // 2. Fetch once and store in actor state: low reliability, but fast. This
  //    actor will crash while handling a request if any of its dependencies
  //    restart.  The supervisor will restart it after the crash, but the
  //    request will fail with an internal server error.
  // 3. Use RestForOne supervision + fetch & store: high reliability, and fast.
  //    Supervisor will pre-emptively restart if any of the previous children
  //    fail.
  let assert CounterA(counter_a) =
    singularity.require(state.registry, CounterA, timeout_ms: 1000)
  let assert CounterB(counter_b) =
    singularity.require(state.registry, CounterB, timeout_ms: 1000)

  case msg {
    Next(reply_with) -> {
      let a = counter.next(counter_a)
      let b = counter.next(counter_b)

      let result =
        int.to_string(a)
        <> " + "
        <> int.to_string(b)
        <> " = "
        <> int.to_string(a + b)

      actor.send(reply_with, result)
      actor.continue(state)
    }
  }
}
