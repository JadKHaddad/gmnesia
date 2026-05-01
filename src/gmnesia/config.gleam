import gleam/dynamic.{type Dynamic}
import gleam/erlang/atom
import gleam/list
import gmnesia/node.{type Node}

/// <https://www.erlang.org/doc/apps/mnesia/mnesia.html#t:config_key/0>
/// 
type ConfigKey {
  ExtraDbNodes
  DcDumpLimit
}

/// <https://www.erlang.org/doc/apps/mnesia/mnesia.html#change_config/2>
/// 
@external(erlang, "gmnesia_ffi", "change_config")
fn change_config(
  key key: ConfigKey,
  value value: dynamic.Dynamic,
) -> Result(Nil, Dynamic)

pub fn change_config_extra_db_nodes(
  nodes nodes: List(Node),
) -> Result(Nil, Dynamic) {
  change_config(ExtraDbNodes, dynamic.list(nodes |> list.map(atom.to_dynamic)))
}

pub fn change_config_dc_dump_limit(limit limit: Float) -> Result(Nil, Dynamic) {
  change_config(DcDumpLimit, dynamic.float(limit))
}
