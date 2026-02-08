import gleam/dynamic.{type Dynamic}
import gmnesia/node.{type Node}
import gmnesia/table.{type Table}

pub type Subscription {
  System
  Activity
  Table(Table, Detail)
}

pub type Detail {
  Simple
  Detailed
}

/// <https://www.erlang.org/doc/apps/mnesia/mnesia.html#subscribe/1>
/// 
/// The events are sent to the calling process from erlang.
/// 
/// You can not receive the events using a [`Subject`](https://hexdocs.pm/gleam_erlang/gleam/erlang/process.html#Subject).
/// 
/// To receive the events you need a catch-all handler. 
/// 
/// ## Examples
///
/// ```gleam
/// import gleam/erlang/atom
/// import gleam/erlang/process
/// import gleam/io
/// import gleam/string
/// import gmnesia/subscribe
/// import gmnesia/table
/// 
/// pub fn listen() {
///   process.new_selector()
///   |> process.select_other(fn(msg) {
///     io.println("Received Mnesia message: " <> string.inspect(msg))
///   })
///   |> process.selector_receive_forever
/// 
///   listen()
/// }
/// 
/// pub fn main() {
///   // Setup code ...
///   
///   let table = atom.create("person")
///   
///   // Make sure the table is ready
///   let assert Ok(_) = table.wait_for_tables([table], table.Finite(1000))
/// 
///   process.spawn(fn() {
///     let assert Ok(_) =
///       subscribe.subscribe(subscribe.Table(
///         table,
///         subscribe.Simple,
///       ))
///   
///     listen()
///   })
/// }
@external(erlang, "gmnesia_ffi", "subscribe")
pub fn subscribe(
  subscription subscription: Subscription,
) -> Result(Node, Dynamic)

/// <https://www.erlang.org/doc/apps/mnesia/mnesia.html#unsubscribe/1>
/// 
@external(erlang, "gmnesia_ffi", "unsubscribe")
pub fn unsubscribe(
  subscription subscription: Subscription,
) -> Result(Node, Dynamic)
