# gmnesia

An experimental, opinionated wrapper around the Erlang [Mnesia](https://www.erlang.org/doc/apps/mnesia/mnesia.html) database for functions ``I`` use.

## Usage

```gleam
import gleam/erlang/atom
import gleam/erlang/node
import gleeunit
import gmnesia/lock
import gmnesia/read
import gmnesia/schema
import gmnesia/system
import gmnesia/table
import gmnesia/transaction
import gmnesia/write

pub type Person {
  Person(id: String, name: String)
}

pub fn main()() {
  let assert Ok(_) = system.stop()

  let _ = schema.create_schema(nodes: [node.name(node.self())])

  let assert Ok(_) = system.start()

  let table = atom.create("person")

  let assert Ok(_) =
    table.create_table(table, [
      table.Attributes(
        [atom.create("id"), atom.create("name")],
      ),
      table.Type(table.Set),
    ])

  system.info()

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

  let assert Ok([Person("2", "Bob")]) =
    transaction.new(fn() {
      read.new(table, key: "2") |> read.lock(lock.Write) |> read.read
    })
    |> transaction.execute

  let assert Ok(_) = table.delete_table(table)

  let assert Ok(_) = system.stop()

  let assert Ok(_) = schema.delete_schema(nodes: [node.name(node.self())])
}
```

## Development

```sh
gleam test  # Run the tests
```
