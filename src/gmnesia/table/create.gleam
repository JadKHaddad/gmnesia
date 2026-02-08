import gleam/dynamic.{type Dynamic}
import gmnesia/table

pub opaque type Builder {
  Builder(table: table.Table, options: List(table.Options))
}

pub fn new(table table: table.Table) -> Builder {
  Builder(table: table, options: [])
}

pub fn options(
  builder: Builder,
  options options: List(table.Options),
) -> Builder {
  Builder(table: builder.table, options: options)
}

pub fn create(builder: Builder) -> Result(Nil, Dynamic) {
  let Builder(table, options) = builder
  table.create_table(table, options)
}
