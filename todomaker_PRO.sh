#! /bin/sh

curl --header "Content-Type: application/json" \
  --request POST \
  --data '{"contentText":"This is your first todo!!! You created it through your endpoint."}' \
  http://localhost:8081/todo/new
