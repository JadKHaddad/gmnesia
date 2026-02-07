import gmnesia/table.{type Table}

/// <https://www.erlang.org/doc/apps/mnesia/mnesia.html#t:write_locks/0>
/// 
pub type WriteLock {
  Write
  StickyWrite
}

/// <https://www.erlang.org/doc/apps/mnesia/mnesia.html#write/3>
/// 
@external(erlang, "gmnesia_ffi", "write_3")
pub fn write_3(table table: Table, record value: a, lock lock: WriteLock) -> Nil

/// <https://www.erlang.org/doc/apps/mnesia/mnesia.html#write/1>
/// 
@external(erlang, "gmnesia_ffi", "write_1")
pub fn write_1(record value: a) -> Nil

/// <https://www.erlang.org/doc/apps/mnesia/mnesia.html#delete/3>
/// 
@external(erlang, "gmnesia_ffi", "delete_3")
pub fn delete_3(table table: Table, key key: a, lock lock: WriteLock) -> Nil
