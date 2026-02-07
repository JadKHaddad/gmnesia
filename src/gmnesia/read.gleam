import gmnesia/lock.{type Lock, Read}
import gmnesia/table.{type Table}

/// <https://www.erlang.org/doc/apps/mnesia/mnesia.html#read/3>
/// 
@external(erlang, "gmnesia_ffi", "read")
pub fn read_3(table table: Table, key key: a, lock lock: Lock) -> List(b)

pub opaque type Builder(a) {
  Builder(table: Table, key: a, lock: Lock)
}

pub fn new(table table: Table, key key: a) -> Builder(a) {
  Builder(table: table, key: key, lock: Read)
}

pub fn lock(builder: Builder(a), lock lock: Lock) -> Builder(a) {
  Builder(table: builder.table, key: builder.key, lock: lock)
}

pub fn read(builder: Builder(a)) -> List(b) {
  let Builder(table, key, lock) = builder
  read_3(table, key: key, lock: lock)
}
