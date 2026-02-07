import gleam/dynamic.{type Dynamic}
import gmnesia/node.{type Node}
import gmnesia/table.{type Table}

pub type What {
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
/// 
@external(erlang, "gmnesia_ffi", "subscribe")
pub fn subscribe(what what: What) -> Result(Node, Dynamic)
