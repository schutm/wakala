erl -pa ebin deps\cowboy\ebin deps\cowlib\ebin deps\ranch\ebin deps\wakala\ebin -s demo -eval "io:format(\"~nPoint your browser at http://localhost:8080/index.html~n\")"
