import gleam/dynamic.{type Dynamic}

/// <https://www.erlang.org/doc/apps/mnesia/mnesia.html#system_info/1>
/// 
pub type SystemInfo {
  All
  AccessModule
  AutoRepair
  // TODO: More info types
}

/// <https://www.erlang.org/doc/apps/mnesia/mnesia.html#info/0>
/// 
@external(erlang, "gmnesia_ffi", "info")
pub fn info() -> Nil

// TODO: For every SystemInfo variant, we get a different type back.
// We have to create a distinct function for each variant e.g. system_info_all, system_info_access_module, etc.
// and map each return type to a concrete Gleam type.

/// <https://www.erlang.org/doc/apps/mnesia/mnesia.html#system_info/1>
/// 
@external(erlang, "gmnesia_ffi", "system_info")
pub fn system_info(info info: SystemInfo) -> Dynamic

/// <https://www.erlang.org/doc/apps/mnesia/mnesia.html#start/0>
/// 
@external(erlang, "gmnesia_ffi", "start")
pub fn start() -> Result(Nil, Dynamic)

/// <https://www.erlang.org/doc/apps/mnesia/mnesia.html#stop/0>
/// 
@external(erlang, "gmnesia_ffi", "stop")
pub fn stop() -> Result(Nil, Dynamic)
