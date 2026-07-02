import gleam/dynamic.{type Dynamic}
import gleam/erlang/atom
import gmnesia/table.{type UserProperty}

/// Builds the {unsplit_method, {Module, Function, Args}} user property.
///
@external(erlang, "unsplit_ffi", "unsplit_method_property")
pub fn unsplit_method(
  module module: atom.Atom,
  function function: atom.Atom,
  args args: List(Dynamic),
) -> UserProperty

/// Merge conflicting rows by unioning them (deduplicated), via
/// `unsplit_lib:bag/2`.
///
pub fn unsplit_bag() -> UserProperty {
  unsplit_method(atom.create("unsplit_lib"), atom.create("bag"), [])
}
