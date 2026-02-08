import gleam/erlang/atom
import gleam/erlang/node
import gleeunit
import gmnesia/delete
import gmnesia/lock
import gmnesia/read
import gmnesia/schema
import gmnesia/system
import gmnesia/table
import gmnesia/table/create as table_create
import gmnesia/table/delete as table_delete
import gmnesia/table/wait
import gmnesia/transaction
import gmnesia/write

pub fn main() -> Nil {
  gleeunit.main()
}

pub type Person {
  Person(id: String, name: String)
}

pub fn raw_ffi_test() {
  let assert Ok(_) = system.stop()

  let _ = schema.create_schema(nodes: [node.name(node.self())])

  let assert Ok(_) = system.start()

  let table = atom.create("person")

  let _ =
    table.create_table(table, [
      table.Attributes(
        // TODO: can we get the atoms of the Person struct? like erlang's record_info(fields, Person)
        [atom.create("id"), atom.create("name")],
      ),
      table.Type(table.Set),
    ])

  let assert Ok(_) =
    table.wait_for_tables(tables: [table], timeout: table.Finite(5000))

  system.info()

  let assert Ok(_) =
    transaction.transaction_1(fn() {
      write.write_1(value: Person("1", "Alice"))
    })

  let assert Ok(_) =
    transaction.transaction_1(fn() {
      write.write_3(table, value: Person("1", "Alice"), lock: write.Write)
    })

  let read = fn() -> List(Person) {
    read.read_3(table, key: "1", lock: lock.Read)
  }

  let assert Ok([Person("1", "Alice")]) = transaction.transaction_1(read)

  let assert Ok(_) =
    transaction.transaction_1(fn() {
      delete.delete_3(table, key: "1", lock: write.Write)
    })

  let assert Ok([]) = transaction.transaction_1(read)

  let assert Ok(_) = table.delete_table(table)

  let assert Ok(_) = system.stop()

  let assert Ok(_) = schema.delete_schema(nodes: [node.name(node.self())])
}

pub fn api_test() {
  let assert Ok(_) = system.stop()

  let _ = [node.name(node.self())] |> schema.new |> schema.create

  let assert Ok(_) = system.start()

  let table = atom.create("person")

  let _ =
    table
    |> table_create.new
    |> table_create.options([
      table.Attributes([atom.create("id"), atom.create("name")]),
      table.Type(table.Set),
    ])
    |> table_create.create

  let assert Ok(_) =
    [table]
    |> wait.new
    |> wait.timeout(table.Finite(5000))
    |> wait.wait

  let assert Ok(_) =
    fn() {
      Person("1", "Alice")
      |> write.new
      |> write.write
    }
    |> transaction.new
    |> transaction.execute

  let assert Ok(_) =
    fn() {
      Person("2", "Bob")
      |> write.new
      |> write.lock(write.StickyWrite)
      // Adding the table is optional, it will be inferred from the value
      // e.g. from the Person type it will be inferred as "person"
      |> write.table(table)
      |> write.write
    }
    |> transaction.new
    |> transaction.retries(transaction.Finite(3))
    |> transaction.execute

  let assert Ok([Person("1", "Alice")]) =
    fn() { table |> read.new(key: "1") |> read.read }
    |> transaction.new
    |> transaction.execute

  let assert Ok([Person("2", "Boba")]) =
    fn() {
      case table |> read.new(key: "2") |> read.lock(lock.Write) |> read.read {
        [Person(id, _), ..] -> {
          Person(id, "Boba")
          |> write.new
          |> write.table(table)
          |> write.lock(write.Write)
          |> write.write

          table |> read.new(key: "2") |> read.lock(lock.Read) |> read.read
        }

        [] -> []
      }
    }
    |> transaction.new
    |> transaction.execute

  let assert Ok([]) =
    fn() {
      table
      |> delete.new(key: "1")
      |> delete.lock(write.Write)
      |> delete.delete

      table
      |> read.new(key: "1")
      |> read.read
    }
    |> transaction.new
    |> transaction.execute

  let assert Ok(_) = table |> table_delete.new |> table_delete.delete

  let assert Ok(_) = system.stop()

  let _ = [node.name(node.self())] |> schema.new |> schema.delete
}
