REBAR?=$(if $(wildcard ./rebar), ./rebar, rebar)

.PHONY: deps src test doc

all: deps xref test

deps:
	@${REBAR} get-deps
	@${REBAR} compile

src:
	@${REBAR} skip_deps=true compile

xref: src
	@${REBAR} skip_deps=true xref

test: xref
	@${REBAR} skip_deps=true eunit

doc:
	@${REBAR} skip_deps=true doc

clean:
	@${REBAR} clean

realclean: clean
	@rm -rf deps/*
