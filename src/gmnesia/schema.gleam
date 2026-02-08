import gleam/dynamic.{type Dynamic}
import gmnesia/node.{type Node}

/// <https://www.erlang.org/doc/apps/mnesia/mnesia.html#create_schema/1>
/// 
@external(erlang, "gmnesia_ffi", "create_schema")
pub fn create_schema(nodes nodes: List(Node)) -> Result(Nil, Dynamic)

/// <https://www.erlang.org/doc/apps/mnesia/mnesia.html#delete_schema/1>
/// 
@external(erlang, "gmnesia_ffi", "delete_schema")
pub fn delete_schema(nodes nodes: List(Node)) -> Result(Nil, Dynamic)

pub opaque type Builder {
  Builder(nodes: List(Node))
}

pub fn new(nodes nodes: List(Node)) -> Builder {
  Builder(nodes: nodes)
}

pub fn create(builder: Builder) -> Result(Nil, Dynamic) {
  let Builder(nodes) = builder
  create_schema(nodes)
}

pub fn delete(builder: Builder) -> Result(Nil, Dynamic) {
  let Builder(nodes) = builder
  delete_schema(nodes)
}
