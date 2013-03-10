REBAR=$(shell which rebar || echo ./rebar)

ENV=ERL_CRASH_DUMP_SECONDS 1
OPTS_COMMON=-pa ebin -pa deps/*/ebin -env $(ENV) -boot start_sasl

.PHONY: deps

all: $(REBAR)
	$(REBAR) get-deps compile

deps: $(REBAR)
	$(REBAR) get-deps update-deps

run: all
	erl $(OPTS_COMMON) -s serv

tests:  $(REBAR)
	$(REBAR) eunit skip_deps=true

test: tests

clean:
	$(REBAR) clean skip_deps=true

# Detect or download rebar

REBAR_URL=http://cloud.github.com/downloads/basho/rebar/rebar
./rebar:
	erl -noshell -s inets -s ssl \
		-eval 'httpc:request(get, {"$(REBAR_URL)", []}, [], [{stream, "./rebar"}])' \
		-s init stop
	chmod +x ./rebar

distclean:
	rm -f ./rebar
