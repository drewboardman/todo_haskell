# todo-haskell
A small *to-do* app for practice

Setup sqlite
------------
```
sqlite3 todo1.db

CREATE TABLE todos (id VARCHAR NOT NULL, content VARCHAR NOT NULL, created_at TEXT NOT NULL, is_pending INTEGER NOT NULL, PRIMARY KEY( id ));
```

Questions
=========

MVP Goals
------------
* Create a text todo with a simple signature

* Be able to do some simple CRUD, probably just save and retrieve a todo.

Extras
---------
* Use a web framework
  - Servant?

* Practice `Monoid` with these Todo types
  - maybe make a Todo monoid and implement `mappend`
  - could use it to combine similar todos or something?
  - might be tricky bc of the database stuff
