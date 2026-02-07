import gmnesia/lock.{type Lock}
import gmnesia/table.{type Table}

/// <https://www.erlang.org/doc/apps/mnesia/mnesia.html#read/3>
/// 
@external(erlang, "gmnesia_ffi", "read_3")
pub fn read_3(table table: Table, key key: a, lock lock: Lock) -> List(b)
