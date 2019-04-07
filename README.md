Questions
=========
* How can I use getCurrentTime (or really and IO x) in the repl?
* Why does this allow me to have 2 competing types for Todo and not complain?

# todo-haskell
A small *to-do* app for practice

MVP Goals
------------
* Create a text todo with a simple signature

```ruby
{ todo: {id: UUID, task: Todo, createdAt: UTCTime} }
```

* Be able to do some simple CRUD, probably just save and retrieve a todo.

Extras
---------
* Use a web framework
  - Servant?

* Save them to a db
  - Beam?
