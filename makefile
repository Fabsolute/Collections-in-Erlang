all:
	rm -f ebin/*.beam
	/usr/local/bin/erlc -o ./ebin src/*.erl
