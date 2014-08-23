.PHONY: test

DIALYZER=dialyzer
REBAR=./rebar
TYPER=typer

APPS = kernel stdlib erts crypto compiler hipe syntax_tools ssl \
       asn1 public_key cowboy
PLT = ./.wakala_plt

all: deps compile

deps:
	@REBAR_EXTRA_DEPS=1 $(REBAR) get-deps

clean:
	@rm -rf ebin/ logs erl_crash.dump $(PLT)

distclean: clean
	@rm -rf deps

examples:

compile: deps
	-@$(REBAR) compile

# Documentation targets
docs:
	-@$(REBAR) skip_deps=true doc

typer:
	-@$(TYPER) --plt $(PLT) -r src

# Test targets
test-deps:
	@REBAR_EXTRA_DEPS=1 $(REBAR) -C rebar.tests.config get-deps

test-compile: test-deps
	-@$(REBAR) -C rebar.tests.config compile

test: ct

ct: test-compile
	-@$(REBAR) skip_deps=true ct

eunit: test-compile
	-@$(REBAR) skip_deps=true eunit

qc: test-compile
	-@$(REBAR) skip_deps=true qc

# Check targets
check: xref dialyzer

xref:
	@$(REBAR) skip_deps=true xref

dialyzer: deps compile check_plt
	-@$(DIALYZER) -pa deps/*/ebin -Wno_return -Wunmatched_returns --plt $(PLT) ebin

check_plt: deps compile
	-@$(DIALYZER) -pa deps/*/ebin --check_plt --plt $(PLT) --apps $(APPS)

build_plt: deps compile
	-@$(DIALYZER) -pa deps/*/ebin --build_plt --output_plt $(PLT) --apps $(APPS) > /dev/null

clean_plt:
	@echo "Are you sure?  It takes a long time to re-build."
	@echo Deleting $(PLT) in 5 seconds.
	sleep 5
	rm $(PLT)
