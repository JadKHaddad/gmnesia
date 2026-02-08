import gleam/dynamic.{type Dynamic}

pub type Retries {
  Infinity
  Finite(Int)
}

/// <https://www.erlang.org/doc/apps/mnesia/mnesia.html#abort/1>
/// 
@external(erlang, "gmnesia_ffi", "abort")
pub fn abort(reason reason: Dynamic) -> Nil

/// <https://www.erlang.org/doc/apps/mnesia/mnesia.html#transaction/1>
/// 
@external(erlang, "gmnesia_ffi", "transaction")
pub fn transaction_1(f f: fn() -> a) -> Result(a, Dynamic)

/// <https://www.erlang.org/doc/apps/mnesia/mnesia.html#transaction/2>
/// 
@external(erlang, "gmnesia_ffi", "transaction")
pub fn transaction_2(
  f f: fn() -> a,
  retries retries: Retries,
) -> Result(a, Dynamic)

pub opaque type Builder(a) {
  Builder(f: fn() -> a, retries: Retries)
}

pub fn new(f f: fn() -> a) -> Builder(a) {
  Builder(f: f, retries: Infinity)
}

pub fn retries(builder: Builder(a), retries retries: Retries) -> Builder(a) {
  Builder(f: builder.f, retries: retries)
}

pub fn execute(builder: Builder(a)) -> Result(a, Dynamic) {
  let Builder(f, retries) = builder
  transaction_2(f, retries)
}
