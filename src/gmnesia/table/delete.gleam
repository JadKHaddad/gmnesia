import gleam/dynamic.{type Dynamic}
import gmnesia/table

pub opaque type Builder {
  Builder(table: table.Table)
}

pub fn new(table table: table.Table) -> Builder {
  Builder(table: table)
}

pub fn delete(builder: Builder) -> Result(Nil, Dynamic) {
  let Builder(table) = builder
  table.delete_table(table)
}
