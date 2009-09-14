all:
	(cd src;$(MAKE) DEBUG=-DDEBUG)

clean:
	(cd src;$(MAKE) clean)
