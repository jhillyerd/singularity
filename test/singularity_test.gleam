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
  let assert Ok(registry) = singularity.start()

  // Create a couple dummy actors, having unique message types.
  let assert Ok(actor_a) =
    actor.start(Nil, fn(_msg: MsgA, state) { actor.continue(state) })
  let assert Ok(actor_b) =
    actor.start(Nil, fn(_msg: MsgB, state) { actor.continue(state) })

  // Register the actors specifying the wrapper (`Actors`) variant.
  singularity.register(registry, ActorA, actor_a)
  singularity.register(registry, ActorB, actor_b)

  // Retrieve and verify registered actors.
  let assert ActorA(got_a) = singularity.require(registry, ActorA)
  let assert ActorB(got_b) = singularity.require(registry, ActorB)

  got_a
  |> should.equal(actor_a)

  got_b
  |> should.equal(actor_b)
}

pub fn try_get_test() {
  let assert Ok(reg) = singularity.start()

  // Verify actors are not registered.
  singularity.try_get(reg, ActorA)
  |> should.be_error()
  singularity.try_get(reg, ActorB)
  |> should.be_error()

  // Register an actor.
  let assert Ok(actor_a) =
    actor.start(Nil, fn(_msg: MsgA, state) { actor.continue(state) })
  singularity.register(reg, ActorA, actor_a)

  // Retrieve and verify registered actors.
  let assert Ok(ActorA(got_a)) = singularity.try_get(reg, ActorA)
  let assert Error(Nil) = singularity.try_get(reg, ActorB)

  got_a
  |> should.equal(actor_a)
}
