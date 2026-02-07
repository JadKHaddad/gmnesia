import gleam/option.{type Option, None, Some}
import gmnesia/table.{type Table}

/// <https://www.erlang.org/doc/apps/mnesia/mnesia.html#t:write_locks/0>
/// 
pub type WriteLock {
  Write
  StickyWrite
}

/// <https://www.erlang.org/doc/apps/mnesia/mnesia.html#write/3>
/// 
@external(erlang, "gmnesia_ffi", "write")
pub fn write_3(table table: Table, value value: a, lock lock: WriteLock) -> Nil

/// <https://www.erlang.org/doc/apps/mnesia/mnesia.html#write/1>
/// 
@external(erlang, "gmnesia_ffi", "write")
pub fn write_1(value value: a) -> Nil

/// <https://www.erlang.org/doc/apps/mnesia/mnesia.html#delete/3>
/// 
@external(erlang, "gmnesia_ffi", "delete")
pub fn delete_3(table table: Table, key key: a, lock lock: WriteLock) -> Nil

pub opaque type Builder(a) {
  Builder(table: Option(Table), value: a, lock: WriteLock)
}

pub fn new(value value: a) -> Builder(a) {
  Builder(table: None, value: value, lock: Write)
}

pub fn lock(builder: Builder(a), lock lock: WriteLock) -> Builder(a) {
  Builder(table: builder.table, value: builder.value, lock: lock)
}

pub fn table(builder: Builder(a), table table: Table) -> Builder(a) {
  Builder(table: Some(table), value: builder.value, lock: builder.lock)
}

pub fn write(builder: Builder(a)) -> Nil {
  let Builder(table, value, lock) = builder
  case table {
    None -> write_1(value: value)
    Some(table) -> write_3(table, value: value, lock: lock)
  }
}
