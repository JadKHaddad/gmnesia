import gleam/dynamic.{type Dynamic}

/// <https://www.erlang.org/doc/apps/mnesia/mnesia.html#transaction/1>
/// 
@external(erlang, "gmnesia_ffi", "transaction_1")
pub fn transaction_1(f f: fn() -> a) -> Result(a, Dynamic)
