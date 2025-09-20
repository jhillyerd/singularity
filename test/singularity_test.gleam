import gleam/erlang/process.{type Subject}
import gleam/option.{None, Some}
import gleam/order
import gleam/otp/actor
import gleam/string
import gleam/time/duration.{type Duration}
import gleam/time/timestamp
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
  let registry = registry.data

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

  got_a
  |> should.equal(actor_a)

  got_b
  |> should.equal(actor_b)
}

pub fn try_get_test() {
  let assert Ok(reg) = singularity.start()
  let reg = reg.data

  // Verify actors are not registered.
  singularity.try_get(reg, ActorA)
  |> should.be_error()
  singularity.try_get(reg, ActorB)
  |> should.be_error()

  // Register an actor via map_started.
  let assert Ok(actor_a) =
    actor.start(actor.new(Nil)) |> singularity.map_started(reg, ActorA)

  // Retrieve and verify registered actors.
  let assert Ok(ActorA(got_a)) = singularity.try_get(reg, ActorA)
  let assert Error(Nil) = singularity.try_get(reg, ActorB)

  got_a
  |> should.equal(actor_a.data)
}

pub fn fixed_delay_test() {
  let assert Ok(reg) = singularity.start()
  let reg = reg.data

  let restart_func = fn() {
    singularity.restart_delay(
      in: reg,
      key: ActorA,
      with: singularity.always_delay(250),
    )
  }

  // Measure three runs of restart delay, first call should not delay.
  time_fn(restart_func) |> should_take(ms: 0, within: tolerance_ms)
  time_fn(restart_func) |> should_take(ms: 250, within: tolerance_ms)
  time_fn(restart_func) |> should_take(ms: 250, within: tolerance_ms)
}

pub fn exponential_delay_unlimited_test() {
  let assert Ok(reg) = singularity.start()
  let reg = reg.data

  let restart_func = fn() {
    singularity.restart_delay(
      in: reg,
      key: ActorA,
      with: singularity.exponential_delay(
        good_ms: 10_000,
        initial_ms: 50,
        max_ms: None,
      ),
    )
  }

  // Measure runs of increasing restart delay, first call should not delay.
  time_fn(restart_func) |> should_take(ms: 0, within: tolerance_ms)
  time_fn(restart_func) |> should_take(ms: 50, within: tolerance_ms)
  time_fn(restart_func) |> should_take(ms: 100, within: tolerance_ms)
  time_fn(restart_func) |> should_take(ms: 200, within: tolerance_ms)
}

pub fn exponential_delay_limited_test() {
  let assert Ok(reg) = singularity.start()
  let reg = reg.data

  let restart_func = fn() {
    singularity.restart_delay(
      in: reg,
      key: ActorA,
      with: singularity.exponential_delay(
        good_ms: 10_000,
        initial_ms: 50,
        max_ms: Some(100),
      ),
    )
  }

  // Measure runs of increasing restart delay, first call should not delay.
  // Must cap at 100ms.
  time_fn(restart_func) |> should_take(ms: 0, within: tolerance_ms)
  time_fn(restart_func) |> should_take(ms: 50, within: tolerance_ms)
  time_fn(restart_func) |> should_take(ms: 100, within: tolerance_ms)
  time_fn(restart_func) |> should_take(ms: 100, within: tolerance_ms)
}

pub fn exponential_delay_resets_after_good_ms_test() {
  let assert Ok(reg) = singularity.start()
  let reg = reg.data

  let restart_func = fn() {
    singularity.restart_delay(
      in: reg,
      key: ActorA,
      with: singularity.exponential_delay(
        good_ms: 500,
        initial_ms: 50,
        max_ms: None,
      ),
    )
  }

  // Measure runs of increasing restart delay, first call should not delay.
  // Should reset to initial_ms after sleep.
  time_fn(restart_func) |> should_take(ms: 0, within: tolerance_ms)
  time_fn(restart_func) |> should_take(ms: 50, within: tolerance_ms)
  time_fn(restart_func) |> should_take(ms: 100, within: tolerance_ms)
  process.sleep(600)
  time_fn(restart_func) |> should_take(ms: 50, within: tolerance_ms)
  time_fn(restart_func) |> should_take(ms: 100, within: tolerance_ms)
}

pub fn exponential_good_doesnt_include_sleep_time_test() {
  let assert Ok(reg) = singularity.start()
  let reg = reg.data

  let restart_func = fn() {
    singularity.restart_delay(
      in: reg,
      key: ActorA,
      with: singularity.exponential_delay(
        good_ms: 200,
        initial_ms: 50,
        max_ms: None,
      ),
    )
  }

  // Measure runs of increasing restart delay, first call should not delay.
  // Should reset to initial_ms after sleep.
  time_fn(restart_func) |> should_take(ms: 0, within: tolerance_ms)
  time_fn(restart_func) |> should_take(ms: 50, within: tolerance_ms)
  time_fn(restart_func) |> should_take(ms: 100, within: tolerance_ms)
  time_fn(restart_func) |> should_take(ms: 200, within: tolerance_ms)

  // If the assertion below fails, then exponential_delay thought we had
  // good_ms of uptime, instead of a long restart delay.
  time_fn(restart_func) |> should_take(ms: 400, within: tolerance_ms)
}

fn time_fn(func: fn() -> a) -> Duration {
  let begin = timestamp.system_time()
  func()
  timestamp.difference(begin, timestamp.system_time())
}

fn should_take(got: Duration, ms want: Int, within within: Int) {
  let want = duration.milliseconds(want)
  let within = duration.milliseconds(within)
  let diff = case duration.compare(got, want) {
    order.Lt -> duration.difference(got, want)
    order.Eq -> duration.nanoseconds(0)
    order.Gt -> duration.difference(want, got)
  }

  let dur_fmt = fn(d: Duration) -> String {
    duration.approximate(d) |> string.inspect
  }

  case duration.compare(diff, within) {
    order.Gt -> {
      panic as {
          "Took "
          <> dur_fmt(got)
          <> ", which was not within "
          <> dur_fmt(within)
          <> " of "
          <> dur_fmt(want)
        }
    }
    _ -> Nil
  }
}
