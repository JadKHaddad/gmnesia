import gleam/dynamic.{type Dynamic}

@external(erlang, "unsplit_ffi", "start")
pub fn start() -> Result(Nil, Dynamic)
