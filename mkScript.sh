#!/bin/bash

elm -mo $1.elm && echo "Concating js" && cat before.js elm-runtime.js build/$1.js after.js > $1.js
