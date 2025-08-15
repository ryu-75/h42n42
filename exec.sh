#! /bin/bash

# Build the project
dune build bin/main.bc.js

# Run the server
ocsigenserver -c h42n42.conf