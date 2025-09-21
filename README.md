# singularity

[![Package Version](https://img.shields.io/hexpm/v/singularity)](https://hex.pm/packages/singularity)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/singularity/)

Singularity is a registry for shared singleton actors, in Gleam. Please note
that Singularity has been made mostly redundant by the addition of named actors
in [gleam_otp]; valid uses of Singularity should be rare.

## Purpose

An actor registry is a system for keeping track of actors over time.  A registry
is most useful when combined with OTP supervision, as an actor will have a
different PID and [Subject] after being restarted.  Messages sent to the old
Subject will yield no response.

By registering your actors within the supervisor [worker start] function,
Singularity will be kept up to date during actor restarts.

Singularity is designed to register singleton actors, in other words, a fixed
number of actors, each of which may have a different message type.  If you are
looking to manage many actors of the same message type, [chip] is a better
solution.

## Inspiration & Alternatives

Erlang has a built in registry, but it is not type safe, which makes it
difficult to use from Gleam.  The Erlang registry is also shared by all
processes in the BEAM VM, which can make unit tests brittle.

Gleam alternatives:

- [chip]
- named actors in [gleam_otp]

## Operation

Singularity is itself an actor, so you must start it and keep a reference to
it anywhere you wish to register or retrieve other actors.

```gleam
let assert Ok(actor.Started(_, registry)) = singularity.start()
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

When storing or retrieving an actor, the relevant constructor for the wrapper
type is used as a key:

```gleam
// Registration.
let assert Ok(actor.Started(_, bulb_a)) = bulb.start()
singularity.register(in: registry, key: LightBulbA, subject: bulb_a)

// Retrieval.
let assert BulbA(bulb_a) =
  singularity.require(in: registry, key: LightBulbA, timeout_ms: 1000)
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


## Restart Delays

Optionally, Singularity can delay the restart of supervised actors using a
fixed duration, an exponential back-off, or a user supplied algorithm.

Example use of exponential back-off with `static_supervisor`:

```gleam
supervisor.new(supervisor.OneForOne)
|> supervisor.restart_tolerance(intensity: 5, period: 60)
|> supervisor.add(
  supervision.worker(fn() {
    // Will delay startup after the first failure.
    singularity.restart_delay(
      in: registry,
      key: LightBulbA,
      with: singularity.exponential_delay(
        good_ms: 10_000,   // Consider the actor stable after 10s.
        initial_ms: 1_000, // Delay first restart by 1s.
        max_ms: None,      // Do not limit the delay duration.
      ),
    )

    bulb.start()
    |> singularity.map_started(in: registry, key: LightBulbA)
  }),
)
|> supervisor.start
```

Actors do not need to be registered with Singularity to use the restart delay
feature.


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
  let assert Ok(actor.Started(_, registry)) = singularity.start()

  // Create a couple dummy actors.
  let assert Ok(actor.Started(_, actor_a)) = actor.start(actor.new(Nil))
  let assert Ok(actor.Started(_, actor_b)) = actor.start(actor.new(Nil))

  // Register the actors specifying the wrapper (`Actors`) variant.
  // Note that the `subject` argument is not wrapped in the Actors
  // type here.
  let _ = singularity.register(in: registry, key: ActorA, subject: actor_a)
  let _ = singularity.register(in: registry, key: ActorB, subject: actor_b)

  // Retrieve and verify registered actors.  These are wrapped.
  let assert ActorA(got_a) =
    singularity.require(in: registry, key: ActorA, timeout_ms: 1000)
  let assert ActorB(got_b) =
    singularity.require(in: registry, key: ActorB, timeout_ms: 1000)

  Nil
}
```

## Supervisor Example

This is an abstract example to illustrate usage within a supervisor.

```gleam
import gleam/otp/supervisor
import gleam/otp/supervision
import gleam/result
import singularity
import inner_actor
import outer_actor

pub type Actor {
  Inner(inner_actor.Message)
  Outer(outer_actor.Message)
}

pub fn main() {
  let assert Ok(actor.Started(_, registry)) = singularity.start()

  // Create worker init functions.
  let inner_child =
    supervision.worker(fn() {
      // Inner has no dependencies, start and register it.
      inner_actor.start()
      |> singularity.map_started(in: registry, key: Inner)
    })

  let outer_child =
    supervision.worker(fn() {
      // Outer depends on Inner, fetch it.
      let assert Inner(inner) =
        singularity.require(in: registry, key: Inner, timeout_ms: 5000)

      // Start and register Outer.
      outer_actor.start(inner)
      |> singularity.map_started(in: registry, key: Outer)
    })

  // Start the supervisor.
  let assert Ok(_) =
    supervisor.new(supervisor.OneForOne)
    |> supervisor.add(inner_child)
    |> supervisor.add(outer_child)
    |> supervisor.start

  let assert Outer(outer) =
    singularity.require(in: registry, key: Outer, timeout_ms: 5000)

  outer_actor.do_something(outer)
}
```

Further documentation can be found at <https://hexdocs.pm/singularity>.

## Development

```sh
gleam test  # Run the tests
gleam shell # Run an Erlang shell
```


[chip]:            https://hexdocs.pm/chip/
[gleam_otp]:       https://hexdocs.pm/gleam_otp/
[Subject]:         https://hexdocs.pm/gleam_erlang/gleam/erlang/process.html#Subject
[worker start]:    https://hexdocs.pm/gleam_otp/gleam/otp/supervisor.html#worker 
