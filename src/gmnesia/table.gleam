import gleam/dynamic.{type Dynamic}
import gleam/erlang/atom
import gmnesia/node.{type Node}
import gmnesia/storage.{type StorageType}

pub type Table =
  atom.Atom

/// A single user-defined property: {Key, Value}
/// <https://www.erlang.org/doc/apps/mnesia/mnesia.html#create_table/2>
///
pub type UserProperty =
  #(atom.Atom, Dynamic)

/// <https://www.erlang.org/doc/apps/mnesia/mnesia.html#table_info/2>
/// 
pub type TableInfo {
  All
  AccessModule
  // TODO: More info types
}

/// <https://www.erlang.org/doc/apps/mnesia/mnesia.html#create_table/2>
/// 
pub type Options {
  // TODO: More options
  Attributes(List(atom.Atom))
  Type(Type)
  DiscCopies(List(Node))
  DiscOnlyCopies(List(Node))
  UserProperties(List(UserProperty))
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

/// <https://www.erlang.org/doc/apps/mnesia/mnesia.html#all_keys/1>
/// 
@external(erlang, "gmnesia_ffi", "all_keys")
pub fn all_keys(table table: Table) -> List(Dynamic)

/// <https://www.erlang.org/doc/apps/mnesia/mnesia.html#first/1>
/// 
/// The table must be an ordered set for this function to make sense.
@external(erlang, "gmnesia_ffi", "first")
pub fn first(table table: Table) -> Dynamic

/// <https://www.erlang.org/doc/apps/mnesia/mnesia.html#last/1>
/// 
/// The table must be an ordered set for this function to make sense.
@external(erlang, "gmnesia_ffi", "last")
pub fn last(table table: Table) -> Dynamic

/// <https://www.erlang.org/doc/apps/mnesia/mnesia.html#add_table_copy/3>
@external(erlang, "gmnesia_ffi", "add_table_copy")
pub fn add_table_copy(
  table table: Table,
  node node: Node,
  storage_type storage_type: StorageType,
) -> Result(Nil, Dynamic)

/// <https://www.erlang.org/doc/apps/mnesia/mnesia.html#change_table_copy_type/3>
@external(erlang, "gmnesia_ffi", "change_table_copy_type")
pub fn change_table_copy_type(
  table table: Table,
  node node: Node,
  storage_type storage_type: StorageType,
) -> Result(Nil, Dynamic)

/// <https://www.erlang.org/doc/apps/mnesia/mnesia.html#set_master_nodes/1>
@external(erlang, "gmnesia_ffi", "set_master_nodes")
pub fn set_master_nodes_1(
  master_nodes master_nodes: List(Node),
) -> Result(Nil, Dynamic)

/// <https://www.erlang.org/doc/apps/mnesia/mnesia.html#set_master_nodes/2>
@external(erlang, "gmnesia_ffi", "set_master_nodes")
pub fn set_master_nodes_2(
  table table: Table,
  master_nodes master_nodes: List(Node),
) -> Result(Nil, Dynamic)
