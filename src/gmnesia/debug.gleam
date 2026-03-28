/// <https://www.erlang.org/doc/apps/mnesia/mnesia.html#t:debug_level/0>
/// 
pub type DebugLevel {
  None
  Verbose
  Debug
  Trace
}

/// <https://www.erlang.org/doc/apps/mnesia/mnesia.html#set_debug_level/1>
/// 
@external(erlang, "gmnesia_ffi", "set_debug_level")
pub fn set_debug_level(level level: DebugLevel) -> Nil
