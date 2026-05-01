import gleam/dynamic.{type Dynamic}
import gmnesia/node.{type Node}

/// <https://www.erlang.org/doc/apps/mnesia/mnesia.html#t:config_key/0>
/// 
pub type ConfigKey {
  ExtraDbNodes
  DcDumpLimit
}

/// <https://www.erlang.org/doc/apps/mnesia/mnesia.html#t:config_value/0>
/// 
pub type ConfigValue {
  List(List(Node))
  Int
  Float
}

/// <https://www.erlang.org/doc/apps/mnesia/mnesia.html#change_config/2>
/// 
@external(erlang, "gmnesia_ffi", "change_config")
pub fn change_config(
  key key: ConfigKey,
  value value: ConfigValue,
) -> Result(Nil, Dynamic)
