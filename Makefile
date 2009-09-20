.PHONY: all test edoc clean
all:
	(cd src;$(MAKE))

test: 
	(cd src;$(MAKE) TEST=true)
	(erl -pa ./ebin -eval "eunit:test(\"./ebin\", [verbose]), init:stop()")

edoc: 
	(mkdir -p ./edoc)
	(cd src; $(MAKE) edoc)

clean:
	(cd src;$(MAKE) clean)
