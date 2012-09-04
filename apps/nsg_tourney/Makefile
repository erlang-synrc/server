REBAR=../../rebar


.PHONY: compile clean cleanall check

compile:
	$(REBAR) compile

clean:
	$(REBAR) clean

cleanall: clean
	find . -name \*~ -delete

test:
	$(REBAR) compile
	$(REBAR) eunit xref
