import gleam/erlang
import gleam/erlang/process.{type Subject}
import gleam/int
import gleam/option.{None, Some}
import gleam/otp/actor
import gleam/string
import gleeunit
import gleeunit/should
import singularity

const tolerance_ms: Int = 30

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
  // Note that the `subject` argument is not wrapped in the Actors
  // type here.
  singularity.register(with: registry, key: ActorA, subject: actor_a)
  singularity.register(with: registry, key: ActorB, subject: actor_b)

  // Retrieve and verify registered actors.  These are wrapped.
  let assert ActorA(got_a) =
    singularity.require(from: registry, key: ActorA, timeout_ms: 1000)
  let assert ActorB(got_b) =
    singularity.require(from: registry, key: ActorB, timeout_ms: 1000)

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

pub fn fixed_delay_test() {
  let assert Ok(reg) = singularity.start()

  let restart_func = fn() {
    singularity.restart_delay(reg, ActorA, singularity.always_delay(250))
  }

  // Measure three runs of restart delay, first call should not delay.
  time_fn(restart_func) |> should_approx(want: 0, within: tolerance_ms)
  time_fn(restart_func) |> should_approx(want: 250, within: tolerance_ms)
  time_fn(restart_func) |> should_approx(want: 250, within: tolerance_ms)
}

pub fn exponential_delay_unlimited_test() {
  let assert Ok(reg) = singularity.start()

  let restart_func = fn() {
    singularity.restart_delay(
      reg,
      ActorA,
      singularity.exponential_delay(
        good_ms: 10_000,
        initial_ms: 50,
        max_ms: None,
      ),
    )
  }

  // Measure runs of increasing restart delay, first call should not delay.
  time_fn(restart_func) |> should_approx(want: 0, within: tolerance_ms)
  time_fn(restart_func) |> should_approx(want: 50, within: tolerance_ms)
  time_fn(restart_func) |> should_approx(want: 100, within: tolerance_ms)
  time_fn(restart_func) |> should_approx(want: 200, within: tolerance_ms)
}

pub fn exponential_delay_limited_test() {
  let assert Ok(reg) = singularity.start()

  let restart_func = fn() {
    singularity.restart_delay(
      reg,
      ActorA,
      singularity.exponential_delay(
        good_ms: 10_000,
        initial_ms: 50,
        max_ms: Some(100),
      ),
    )
  }

  // Measure runs of increasing restart delay, first call should not delay.
  // Must cap at 100ms.
  time_fn(restart_func) |> should_approx(want: 0, within: tolerance_ms)
  time_fn(restart_func) |> should_approx(want: 50, within: tolerance_ms)
  time_fn(restart_func) |> should_approx(want: 100, within: tolerance_ms)
  time_fn(restart_func) |> should_approx(want: 100, within: tolerance_ms)
}

pub fn exponential_delay_resets_after_good_ms_test() {
  let assert Ok(reg) = singularity.start()

  let restart_func = fn() {
    singularity.restart_delay(
      reg,
      ActorA,
      singularity.exponential_delay(good_ms: 500, initial_ms: 50, max_ms: None),
    )
  }

  // Measure runs of increasing restart delay, first call should not delay.
  // Should reset to initial_ms after sleep.
  time_fn(restart_func) |> should_approx(want: 0, within: tolerance_ms)
  time_fn(restart_func) |> should_approx(want: 50, within: tolerance_ms)
  time_fn(restart_func) |> should_approx(want: 100, within: tolerance_ms)
  process.sleep(600)
  time_fn(restart_func) |> should_approx(want: 50, within: tolerance_ms)
  time_fn(restart_func) |> should_approx(want: 100, within: tolerance_ms)
}

pub fn exponential_good_doesnt_include_sleep_time_test() {
  let assert Ok(reg) = singularity.start()

  let restart_func = fn() {
    singularity.restart_delay(
      reg,
      ActorA,
      singularity.exponential_delay(good_ms: 200, initial_ms: 50, max_ms: None),
    )
  }

  // Measure runs of increasing restart delay, first call should not delay.
  // Should reset to initial_ms after sleep.
  time_fn(restart_func) |> should_approx(want: 0, within: tolerance_ms)
  time_fn(restart_func) |> should_approx(want: 50, within: tolerance_ms)
  time_fn(restart_func) |> should_approx(want: 100, within: tolerance_ms)
  time_fn(restart_func) |> should_approx(want: 200, within: tolerance_ms)

  // If the assertion below fails, then exponential_delay thought we had
  // good_ms of uptime, instead of a long restart delay.
  time_fn(restart_func) |> should_approx(want: 400, within: tolerance_ms)
}

fn time_fn(func) {
  let begin = erlang.system_time(erlang.Millisecond)
  func()
  let end = erlang.system_time(erlang.Millisecond)

  end - begin
}

fn should_approx(got: Int, want want: Int, within within: Int) {
  let diff = int.absolute_value(got - want)

  case diff > within {
    True -> {
      panic as {
        string.inspect(got)
        <> " was not within "
        <> string.inspect(within)
        <> " of "
        <> string.inspect(want)
      }
    }
    False -> Nil
  }
}

