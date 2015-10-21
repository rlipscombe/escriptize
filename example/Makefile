ERLS := $(wildcard src/*.erl)
BEAMS := $(ERLS:src/%.erl=ebin/%.beam)

ebin/%.beam: src/%.erl
	erlc -o ebin $<

example: ebin $(BEAMS)
	../escriptize example

ebin:
	mkdir -p ebin

clean:
	-rm $(BEAMS)
	-rm example
