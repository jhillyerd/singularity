# singularity

[![Package Version](https://img.shields.io/hexpm/v/singularity)](https://hex.pm/packages/singularity)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/singularity/)

Singularity is a registry for shared singleton actors, in Gleam.

When an actor's process exits, it will be removed from the registry
automatically.  Fetching an actor using the [require](#require) function
will block until that actor has been registered.  These two properties
allow OTP supervisors to start your actors only when their dependencies
are ready.

## Usage

```sh
gleam add gleam_erlang
gleam add gleam_otp
gleam add singularity
```

### Basic Example

This example should compile and makes a good starting point for experimentation.

```gleam
import gleam/erlang/process.{type Subject}
import gleam/otp/actor
import singularity

type MsgA

type MsgB

type Actors {
  ActorA(Subject(MsgA))
  ActorB(Subject(MsgB))
}

pub fn main() {
  let assert Ok(registry) = singularity.start()

  // Create a couple dummy actors, having unique message types.
  let assert Ok(actor_a) =
    actor.start(Nil, fn(_msg: MsgA, state) { actor.continue(state) })
  let assert Ok(actor_b) =
    actor.start(Nil, fn(_msg: MsgB, state) { actor.continue(state) })

  // Register the actors specifying the wrapper (`Actors`) variant.
  // Note that the `subject` argument is not wrapped in the Actors
  // type here.
  singularity.register(with: registry, key: ActorA, subject: actor_a)
  singularity.register(with: registry, key: ActorB, subject: actor_b)

  // Retrieve and verify registered actors.  These are wrapped.
  let assert ActorA(got_a) =
    singularity.require(from: registry, key: ActorA, timeout_ms: 1000)
  let assert ActorB(got_b) =
    singularity.require(from: registry, key: ActorB, timeout_ms: 1000)

  Nil
}
```

## Supervisor Example

This is an abstract example to illustrate usage within a supervisor.

```gleam
import gleam/erlang/process
import gleam/otp/supervisor
import gleam/result
import singularity
import inner_actor
import outer_actor

pub type Actor {
  Inner(inner_actor.Message)
  Outer(outer_actor.Message)
}

pub fn main() {
  let assert Ok(registry) = singularity.start()

  // Create worker init functions.
  let inner_child =
    supervisor.worker(fn(_state) {
      // Inner has no dependencies, start and register it.
      inner_actor.start()
      |> result.map(singularity.register(registry, Inner, subject: _))
    })

  let clock_child =
    supervisor.worker(fn(_state) {
      // Outer depends on Inner, fetch it.
      let assert Inner(inner) =
        singularity.require(registry, Inner, timeout_ms: 5000)

      // Start and register Outer.
      outer_actor.start(inner)
      |> result.map(singularity.register(registry, Outer, subject: _))
    })

  // Start the supervisor.
  let assert Ok(_) =
    supervisor.start_spec(
      supervisor.Spec(
        argument: Nil,
        frequency_period: 1,
        max_frequency: 5,
        init: fn(children) {
          children
          |> supervisor.add(inner_child)
          |> supervisor.add(clock_child)
        },
      ),
    )

  let assert Outer(outer) =
    singularity.require(registry, Outer, timeout_ms: 5000)

  outer_actor.do_something(outer)
}
```

Further documentation can be found at <https://hexdocs.pm/singularity>.

## Development

```sh
gleam test  # Run the tests
gleam shell # Run an Erlang shell
```
