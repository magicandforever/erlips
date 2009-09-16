include ../include.mk

APPLICATION=tldh
DOC_OPTS={dir,\"../../edoc/tldh\"}

all: $(EBIN_FILES)

debug:
	$(MAKE) DEBUG=-DDEBUG

clean:
	rm -rf $(EBIN_FILES)

edoc:
	$(ERL) -noshell -pa ../ebin \
		-eval "edoc:application($(APPLICATION), \".\", [$(DOC_OPTS)])" \
		-s init stop

test: all
	$(ERL) -noshell -pa ../ebin -s eunit test '.' -s init stop
