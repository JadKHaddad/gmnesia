import gleam/dynamic.{type Dynamic}
import gleam/erlang/atom

pub type Table =
  atom.Atom

/// <https://www.erlang.org/doc/apps/mnesia/mnesia.html#create_table/2>
/// 
pub type Options {
  // TODO: More options
  Attributes(List(atom.Atom))
  Type(Type)
}

/// <https://www.erlang.org/doc/apps/mnesia/mnesia.html#create_table/2>
/// 
pub type Type {
  Set
  OrderedSet
  Bag
}

pub type Timeout {
  Infinity
  Finite(Int)
}

pub type WaitError {
  Error(Dynamic)
  Timeout(List(Table))
}

/// <https://www.erlang.org/doc/apps/mnesia/mnesia.html#create_table/2>
/// 
@external(erlang, "gmnesia_ffi", "create_table")
pub fn create_table(
  table table: Table,
  options options: List(Options),
) -> Result(Nil, Dynamic)

/// <https://www.erlang.org/doc/apps/mnesia/mnesia.html#delete_table/1>
/// 
@external(erlang, "gmnesia_ffi", "delete_table")
pub fn delete_table(table table: Table) -> Result(Nil, Dynamic)

/// <https://www.erlang.org/doc/apps/mnesia/mnesia.html#wait_for_tables/2>
///
@external(erlang, "gmnesia_ffi", "wait_for_tables")
pub fn wait_for_tables(
  tables tables: List(Table),
  timeout timeout: Timeout,
) -> Result(Nil, WaitError)
