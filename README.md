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
* Practice `Monoid` with these Todo types
  - maybe make a Todo monoid and implement `mappend`
  - could use it to combine similar todos or something?
  - might be tricky bc of the database stuff

Questions
=========
