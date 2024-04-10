# singularity

[![Package Version](https://img.shields.io/hexpm/v/singularity)](https://hex.pm/packages/singularity)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/singularity/)

Singularity is a registry for shared singleton actors, in Gleam.

## Purpose

An actor registry is a system for keeping track of actors over time.  A registry
is most useful when combined with OTP supervision, as an actor will have a
different PID and [Subject] after being restarted.  Messages sent to the old
Subject will yield no response.

By registering your actors within the supervisor [worker start] function,
Singularity will be kept up to date during actor restarts.

Singularity is designed to register singleton actors, in otherwords, a fixed
number of actors, each of which may have a different message type.  If you are
looking to manage many actors of the same message type, [chip] is a better
solution.

## Inspriation & Alternatives

Erlang has a built in registry, but it is not type safe, which makes it
difficult to use from Gleam.  The Erlang registry is also shared by all
processes in the BEAM VM, which can make unit tests brittle.

Gleam alternatives:

- [chip]

## Operation

Singularity is itself an actor, so you must start it and keep a reference to
it anywhere you wish to register or retrieve other actors.

```gleam
let assert Ok(registry) = singularity.start()
```

Type safety is ensured by utilizing a wrapper type that can hold any of the
registered actors, for example:

```gleam
type Actor {
    LightSwitch(Subject(switch.Message))
    LightBulbA(Subject(bulb.Message))
    LightBulbB(Subject(bulb.Message))
}
```

When storing or retrieving an actor, the relevant contstructor for the wrapper
type is used as a key:

```gleam
// Registration.
let assert Ok(bulb_a) = bulb.start()
singularity.register(with: registry, key: LightBulbA, subject: bulb_a)

// Retrieval.
let assert BulbA(bulb_a) =
  singularity.require(from: registry, key: LightBulbA, timeout_ms: 1000)
```

When an actor's process exits, it will be removed from the registry
automatically.  Fetching an actor using the [require](#require) function
will block until that actor has been registered.  These two properties
allow OTP supervisors to start your actors only when their dependencies
are ready.

When using Singularity in a request oriented service (i.e. a web API), you will
typically want to store a reference to singularity directly in your request
context custom type.  This allows your request handler functions to fetch the
dependencies they need during a request.

For scenarios with high request volumes, or a large number of dependencies,
you may prefer to have a supervisor construct your context with all dependencies
in place.  This will typically require your context, top-level handler, and
entire HTTP server stack (i.e. mist + wisp), to be created by a supervisor.

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

type Actor {
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

  // Register the actors specifying the wrapper (`Actor`) variant.
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

  let outer_child =
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
          |> supervisor.add(outer_child)
        },
      ),
    )

  let assert Outer(outer) =
    singularity.require(registry, Outer, timeout_ms: 5000)

  outer_actor.do_something(outer)
}
```

See the [examples](https://github.com/jhillyerd/singularity/tree/main/examples)
directory for more complete examples.

Further documentation can be found at <https://hexdocs.pm/singularity>.

## Development

```sh
gleam test  # Run the tests
gleam shell # Run an Erlang shell
```


[chip]:            https://hexdocs.pm/chip/
[Subject]:         https://hexdocs.pm/gleam_erlang/gleam/erlang/process.html#Subject
[worker start]:    https://hexdocs.pm/gleam_otp/gleam/otp/supervisor.html#worker 
