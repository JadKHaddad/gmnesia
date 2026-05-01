import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/atom
import gleam/list
import gleam/result
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
) -> Result(Dynamic, Dynamic)

pub fn change_config_extra_db_nodes(
  nodes nodes: List(Node),
) -> Result(List(Node), Dynamic) {
  use nodes <- result.try(change_config(
    ExtraDbNodes,
    dynamic.list(nodes |> list.map(atom.to_dynamic)),
  ))

  use nodes <- result.try(
    decode.run(nodes, decode.list(decode.dynamic))
    |> result.map_error(fn(_) {
      dynamic.string("Failed to decode extra_db_nodes")
    }),
  )

  let nodes =
    nodes
    |> list.map(atom.cast_from_dynamic)

  Ok(nodes)
}

pub fn change_config_dc_dump_limit(limit limit: Float) -> Result(Float, Dynamic) {
  use limit <- result.try(change_config(DcDumpLimit, dynamic.float(limit)))

  use limit <- result.try(
    decode.run(limit, decode.float)
    |> result.map_error(fn(_) {
      dynamic.string("Failed to decode dc_dump_limit")
    }),
  )

  Ok(limit)
}
