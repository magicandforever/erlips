all:
	(cd src;$(MAKE))

test: 
	(cd src;$(MAKE) TEST=true)
	(erl -pa ./ebin -eval "eunit:test(\"./ebin\", [verbose]), init:stop()")

clean:
	(cd src;$(MAKE) clean)
