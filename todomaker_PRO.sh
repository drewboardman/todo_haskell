#! /bin/sh

curl --header "Content-Type: application/json" \
  --request POST \
  --data '{"contentText":"This is a new todo that you are using to show the post endpoint"}' \
  http://localhost:8081/todo/new
