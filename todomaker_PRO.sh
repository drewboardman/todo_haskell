#! /bin/sh

# MAKE A NEW PENDING TODO

# curl --header "Content-Type: application/json" \
#   --request POST \
#   --data '{"contentText":"This is a new todo that you are using to show the post endpoint"}' \
#   http://localhost:8081/todo/new

# UPDATE THE CONTENT OF A TODO

# curl --header "Content-Type: application/json" \
#   --request POST \
#   --data '{"_uuid":"252dc06e-679d-41c5-aed6-773307c239a2","_content":{"contentText":"I am updating this please for the love of god work."}}' \
#   http://localhost:8081/todo/update
