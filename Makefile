.PHONY: all test edoc dialyzer clean
all:
	(cd src;$(MAKE))

test: 
	(cd src;$(MAKE) TEST=true)
	(erl -pa ./ebin -eval "eunit:test(\"./ebin\", [verbose]), init:stop()")

edoc: 
	(mkdir -p ./edoc)
	(cd src; $(MAKE) edoc)

dialyzer: clean
	(cd srr;$(MAKE) DEBUG=true)
	(dialyzer -Werror_handling -Wrace_conditions -Wunderspecs -r .)

clean:
	(cd src;$(MAKE) clean)
