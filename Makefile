
compile:
	rebar get-deps
	rebar compile

run:	compile
	erl -pa ebin deps/*/ebin -s aloha_demo start -s init stop

clean:
	rebar clean
