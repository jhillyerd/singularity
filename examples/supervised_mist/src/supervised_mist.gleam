import adder
import counter
import gleam/bytes_builder
import gleam/erlang/process.{type Subject}
import gleam/http/request.{type Request}
import gleam/http/response.{type Response}
import gleam/io
import gleam/otp/supervisor
import gleam/result
import mist
import singularity

pub type Actor {
  CounterA(Subject(counter.Message))
  CounterB(Subject(counter.Message))
  Adder(Subject(adder.Message))
}

pub fn main() {
  let assert Ok(registry) = singularity.start()

  // Create worker init functions.
  let counter_a_child =
    supervisor.worker(fn(_state) {
      // Counter has no dependencies, start and register it.
      io.println("## Starting counter_a")
      counter.start(1)
      |> result.map(singularity.register(registry, CounterA, subject: _))
    })

  let counter_b_child =
    supervisor.worker(fn(_state) {
      io.println("## Starting counter_b")
      // Counter has no dependencies, start and register it.
      counter.start(10)
      |> result.map(singularity.register(registry, CounterB, subject: _))
    })

  let adder_child =
    supervisor.worker(fn(_state) {
      io.println("## Starting adder")
      // Adder depends on two counters, fetch them.
      let assert CounterA(counter_a) =
        singularity.require(registry, CounterA, timeout_ms: 1000)
      let assert CounterB(counter_b) =
        singularity.require(registry, CounterB, timeout_ms: 1000)

      // Start and register Adder.
      adder.start(counter_a, counter_b)
      |> result.map(singularity.register(registry, Adder, subject: _))
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
          |> supervisor.add(counter_a_child)
          |> supervisor.add(counter_b_child)
          |> supervisor.add(adder_child)
        },
      ),
    )

  let handler = fn(req) { handle_request(req, registry) }

  handler
  |> mist.new
  |> mist.port(3000)
  |> mist.start_http

  // Increment counter_b to make things interesting.
  let assert CounterB(counter_b) =
    singularity.require(registry, CounterB, timeout_ms: 1000)
  counter.next(counter_b)

  // Kill counter_a after a delay.
  process.sleep(2000)
  let assert CounterA(counter_a) =
    singularity.require(registry, CounterA, timeout_ms: 1000)
  counter_a
  |> process.subject_owner
  |> process.kill

  process.sleep_forever()
}

fn handle_request(
  _req: Request(mist.Connection),
  registry: Subject(singularity.Message(Actor)),
) -> Response(mist.ResponseData) {
  // Fetch the adder actor.
  let assert Adder(adder) =
    singularity.require(registry, Adder, timeout_ms: 1000)

  let body =
    adder.next(adder)
    |> bytes_builder.from_string
    |> bytes_builder.append_string("\n\nClick reload!")
    |> mist.Bytes

  response.new(200)
  |> response.set_header("content-type", "text/plain")
  |> response.set_body(body)
}
