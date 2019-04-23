# todo-haskell
A small *to-do* app for practice

Setup sqlite
------------
```
sqlite3 todo1.db

CREATE TABLE todos (id VARCHAR NOT NULL, content VARCHAR NOT NULL, created_at TEXT NOT NULL, finished_At Text NULL, PRIMARY KEY( id ));
```

Future goals
---------
* Complete a pending todo
* Update a Pending Todo
* Delete a Todo

Questions
=========
