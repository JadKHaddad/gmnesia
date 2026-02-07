import gleam/erlang/atom
import gleam/erlang/node
import gleam/erlang/process
import gleam/io
import gleeunit
import gmnesia/lock
import gmnesia/read
import gmnesia/schema
import gmnesia/subscribe
import gmnesia/system
import gmnesia/table
import gmnesia/transaction
import gmnesia/write

pub fn main() -> Nil {
  gleeunit.main()
}

pub type Person {
  Person(id: String, name: String)
}

pub fn listen() {
  let subject = process.new_subject()

  case process.receive(subject, 1000) {
    Ok(message) -> {
      io.println("Received message: " <> message)
      listen()
    }
    Error(_) -> io.println("No message received")
  }
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

  system.info()

  let assert Ok(_) =
    subscribe.subscribe(subscribe.Table(table, subscribe.Simple))

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
      write.delete_3(table, key: "1", lock: write.Write)
    })

  let assert Ok([]) = transaction.transaction_1(read)

  let assert Ok(_) = table.delete_table(table)

  let assert Ok(_) = system.stop()

  let assert Ok(_) = schema.delete_schema(nodes: [node.name(node.self())])

  listen()
}

pub fn api_test() {
  let assert Ok(_) = system.stop()

  let _ = schema.create_schema(nodes: [node.name(node.self())])

  let assert Ok(_) = system.start()

  let table = atom.create("person")

  let _ =
    table.create_table(table, [
      table.Attributes([atom.create("id"), atom.create("name")]),
      table.Type(table.Set),
    ])

  let assert Ok(_) =
    transaction.new(fn() {
      write.new(Person("1", "Alice"))
      |> write.write
    })
    |> transaction.execute

  let assert Ok(_) =
    transaction.new(fn() {
      write.new(Person("2", "Bob"))
      |> write.lock(write.StickyWrite)
      // Adding the table is optional, it will be inferred from the value
      // e.g. from the Person type it will be inferred as "person"
      |> write.table(table)
      |> write.write
    })
    |> transaction.retries(transaction.Finite(3))
    |> transaction.execute

  let assert Ok([Person("1", "Alice")]) =
    transaction.new(fn() { read.new(table, key: "1") |> read.read })
    |> transaction.execute

  let assert Ok([Person("2", "Boba")]) =
    transaction.new(fn() {
      case read.new(table, key: "2") |> read.lock(lock.Write) |> read.read {
        [Person(id, _), ..] -> {
          let boba = Person(id, "Boba")

          write.new(boba)
          |> write.table(table)
          |> write.lock(write.Write)
          |> write.write

          read.new(table, key: "2") |> read.lock(lock.Read) |> read.read
        }

        [] -> []
      }
    })
    |> transaction.execute
}
