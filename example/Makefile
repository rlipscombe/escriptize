REBAR ?= rebar

top:
	$(REBAR) get-deps compile
	../escriptize example

clean:
	$(REBAR) clean
	-rm example
