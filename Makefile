SHELL := /bin/bash
.PHONY: all test edoc dialyzer clean
PLT=".dialyzer_plt"

all:
	(cd src;$(MAKE))

test: clean
	(cd src;$(MAKE) TEST=true)
	(erl -pa ./ebin -eval "eunit:test(\"./ebin\", [verbose]), init:stop()")

edoc: 
	(mkdir -p ./edoc)
	(cd src; $(MAKE) edoc)

plt : 
	(./scripts/gen_plt.sh -a sasl)

dialyzer: clean
	(cd src;$(MAKE) DEBUG=true)
	#(dialyzer --plt $(PLT) -Werror_handling -Wrace_conditions -Wunderspecs -r .)
	(dialyzer --plt $(PLT) -Werror_handling -Wrace_conditions  -r .)

clean:
	(cd src;$(MAKE) clean)

