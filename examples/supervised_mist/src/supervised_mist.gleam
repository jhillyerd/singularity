import adder
import counter
import gleam/bytes_builder
import gleam/erlang/process.{type Subject}
import gleam/http/request.{type Request}
import gleam/http/response.{type Response}
import gleam/io
import gleam/otp/actor
import gleam/otp/static_supervisor as supervisor
import gleam/result
import mist
import singularity

pub fn main() {
  let assert Ok(registry) = singularity.start()

  // Create worker init functions.
  let counter_a_child =
    supervisor.worker_child("counter_a", fn() {
      // Counter has no dependencies, start and register it.
      io.println("## Starting counter_a at 1")
      counter.start(1)
      |> result.map(singularity.register(registry, adder.CounterA, subject: _))
      |> actor.to_erlang_start_result
    })

  let counter_b_child =
    supervisor.worker_child("counter_b", fn() {
      io.println("## Starting counter_b at 10")
      // Counter has no dependencies, start and register it.
      counter.start(10)
      |> result.map(singularity.register(registry, adder.CounterB, subject: _))
      |> actor.to_erlang_start_result
    })

  let adder_child =
    supervisor.worker_child("adder", fn() {
      io.println("## Starting adder")

      // Start and register Adder, passing in the registry.
      adder.start(registry)
      |> result.map(singularity.register(registry, adder.Adder, subject: _))
      |> actor.to_erlang_start_result
    })

  // Start the supervisor.
  let assert Ok(_) =
    supervisor.new(supervisor.OneForOne)
    |> supervisor.add(counter_a_child)
    |> supervisor.add(counter_b_child)
    |> supervisor.add(adder_child)
    |> supervisor.start_link

  let handler = fn(req) { handle_request(req, registry) }

  let assert Ok(_) =
    handler
    |> mist.new
    |> mist.port(3000)
    |> mist.start_http

  // Increment both counters to make things interesting.
  let assert adder.CounterA(counter_a) =
    singularity.require(registry, adder.CounterA, timeout_ms: 1000)
  let assert adder.CounterB(counter_b) =
    singularity.require(registry, adder.CounterB, timeout_ms: 1000)
  counter.next(counter_a)
  counter.next(counter_b)

  // Kill counter_a after a delay.  This will restart its count at 1.
  process.sleep(8000)
  counter_a
  |> process.subject_owner
  |> process.kill

  process.sleep_forever()
}

fn handle_request(
  _req: Request(mist.Connection),
  registry: Subject(singularity.Message(adder.Actor)),
) -> Response(mist.ResponseData) {
  // Fetch the adder actor.
  let assert adder.Adder(adder) =
    singularity.require(registry, adder.Adder, timeout_ms: 1000)

  let body =
    adder.next(adder)
    |> bytes_builder.from_string
    |> bytes_builder.append_string("\n\nClick reload!")
    |> mist.Bytes

  response.new(200)
  |> response.set_header("content-type", "text/plain")
  |> response.set_body(body)
}
