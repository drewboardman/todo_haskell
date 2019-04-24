# todo-haskell
A small *to-do* app for practice

Setup sqlite
------------
```
sqlite3 todo1.db

CREATE TABLE todos (id VARCHAR NOT NULL, content VARCHAR NOT NULL, created_at TEXT NOT NULL, finished_At Text NULL, PRIMARY KEY( id ));
```

Testing endpoints
----------------
Many of the endpoints can be tested in the `todomaker_PRO`. For others you can
just use `curl`.

```
# complete a pending todo
curl http://localhost:8081/todo/complete\?uuid\="xxx-xxx-46db-901f-5bd3c65e30b5"
```

Future goals
---------
* Delete a Todo
