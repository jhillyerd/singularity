//// Count is a very simple actor that responds with an ever incrementing
//// integer.

import gleam/erlang/process.{type Subject}
import gleam/otp/actor

pub fn start(start_at: Int) {
  actor.start(start_at, handle_message)
}

pub fn next(actor: Subject(Message)) -> Int {
  actor.call(actor, Next, 10)
}

pub type Message {
  Next(reply_with: Subject(Int))
}

fn handle_message(msg: Message, count: Int) {
  case msg {
    Next(reply_with) -> {
      actor.send(reply_with, count)
      actor.continue(count + 1)
    }
  }
}
