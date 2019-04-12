Questions
=========
* How can I use getCurrentTime (or really any IO x) in the repl?
  - just use bind and it works. No need to invent a closure for it
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

* Practice `Monoid` with these Todo types
  - maybe make a Todo monoid and implement `mappend`
  - could use it to combine similar todos or something?
  - might be tricky bc of the database stuff
