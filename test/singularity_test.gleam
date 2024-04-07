import gleam/erlang/process.{type Subject}
import gleam/otp/actor
import gleeunit
import gleeunit/should
import singularity

pub fn main() {
  gleeunit.main()
}

type MsgA

type MsgB

type Actors {
  ActorA(Subject(MsgA))
  ActorB(Subject(MsgB))
}

pub fn example_test() {
  let assert Ok(actor_a) =
    actor.start(Nil, fn(_msg: MsgA, state) { actor.continue(state) })
  let assert Ok(actor_b) =
    actor.start(Nil, fn(_msg: MsgB, state) { actor.continue(state) })

  // Register a couple actors with different message types.
  let assert Ok(reg) = singularity.start()
  singularity.put(reg, ActorA, actor_a)
  singularity.put(reg, ActorB, actor_b)

  // Retrieve and verify registered actors.
  let assert ActorA(got_a) = singularity.get(reg, ActorA)
  let assert ActorB(got_b) = singularity.get(reg, ActorB)

  got_a
  |> should.equal(actor_a)

  got_b
  |> should.equal(actor_b)

  Nil
}
