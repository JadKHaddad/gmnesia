import gleam/erlang/atom
import gleam/erlang/node
import gleam/erlang/process
import gleam/io
import gleam/string
import gmnesia/lock
import gmnesia/read
import gmnesia/schema
import gmnesia/subscribe
import gmnesia/system
import gmnesia/table
import gmnesia/transaction
import gmnesia/write

pub type Person {
  Person(id: String, name: String)
}

pub fn listen() {
  let selector = process.new_selector()
  let selector =
    process.select_other(selector, fn(msg) {
      io.println("Received Mnesia message: " <> string.inspect(msg))
    })
  process.selector_receive_forever(selector)

  listen()
}

pub fn main() {
  let assert Ok(_) = system.stop()

  let _ = schema.create_schema(nodes: [node.name(node.self())])

  let assert Ok(_) = system.start()

  let table = atom.create("person")

  let _ =
    table.create_table(table, [
      table.Attributes([atom.create("id"), atom.create("name")]),
      table.Type(table.Set),
    ])

  let assert Ok(_) = table.wait_for_tables([table], table.Finite(1000))

  process.spawn(fn() {
    let assert Ok(_) =
      subscribe.subscribe(subscribe.Table(
        atom.create("person"),
        subscribe.Simple,
      ))
    listen()
  })

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

  process.sleep_forever()
}
