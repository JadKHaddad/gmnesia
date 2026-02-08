import gmnesia/table.{type Table}
import gmnesia/write.{type WriteLock, Write}

/// <https://www.erlang.org/doc/apps/mnesia/mnesia.html#delete/3>
/// 
@external(erlang, "gmnesia_ffi", "delete")
pub fn delete_3(table table: Table, key key: a, lock lock: WriteLock) -> Nil

pub opaque type Builder(a) {
  Builder(table: Table, key: a, lock: WriteLock)
}

pub fn new(table table: Table, key key: a) -> Builder(a) {
  Builder(table: table, key: key, lock: Write)
}

pub fn lock(builder: Builder(a), lock lock: WriteLock) -> Builder(a) {
  Builder(table: builder.table, key: builder.key, lock: lock)
}

pub fn delete(builder: Builder(a)) -> Nil {
  let Builder(table, key, lock) = builder
  delete_3(table, key: key, lock: lock)
}
