import gmnesia/table

pub opaque type Builder {
  Builder(tables: List(table.Table), timeout: table.Timeout)
}

pub fn new(tables tables: List(table.Table)) -> Builder {
  Builder(tables: tables, timeout: table.Infinity)
}

pub fn timeout(builder: Builder, timeout timeout: table.Timeout) -> Builder {
  Builder(tables: builder.tables, timeout: timeout)
}

pub fn wait(builder: Builder) -> Result(Nil, table.WaitError) {
  let Builder(tables, timeout) = builder
  table.wait_for_tables(tables: tables, timeout: timeout)
}
