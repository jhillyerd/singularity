//// Adder is a very simple actor that adds values obtained from two counter actors.

import counter
import gleam/erlang/process.{type Subject}
import gleam/int
import gleam/otp/actor

pub type State {
  State(
    counter_a: Subject(counter.Message),
    counter_b: Subject(counter.Message),
  )
}

/// Accept two counter actors as dependencies.
pub fn start(
  counter_a: Subject(counter.Message),
  counter_b: Subject(counter.Message),
) {
  State(counter_a, counter_b)
  |> actor.start(handle_message)
}

pub fn next(actor: Subject(Message)) -> String {
  actor.call(actor, Next, 10)
}

pub type Message {
  Next(reply_with: Subject(String))
}

fn handle_message(msg: Message, state: State) {
  case msg {
    Next(reply_with) -> {
      let a = counter.next(state.counter_a)
      let b = counter.next(state.counter_b)

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
