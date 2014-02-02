#!/bin/bash

elm -mo $1.elm && echo "Concating js" && cat elm-runtime.js build/$1.js after.js > $1.js
