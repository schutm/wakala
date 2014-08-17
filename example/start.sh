#!/bin/sh
erl -pa ebin deps/*/ebin -s demo -eval "io:format(\"~nPoint your browser at http://localhost:8080/index.html~n\")"
