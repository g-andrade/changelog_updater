SHELL := bash
.ONESHELL:
.SHELLFLAGS := -euc
.DELETE_ON_ERROR:
MAKEFLAGS += --warn-undefined-variables
MAKEFLAGS += --no-builtin-rules

## General Rules

all: compile doc-dry
.PHONY: all
.NOTPARALLEL: all

compile:
	@rebar3 compile
.PHONY: compile

clean:
	@rebar3 clean -a
.PHONY: clean

check: xref find-unused-code lint dialyzer
.NOTPARALLEL: check
.PHONY: check

test: eunit ct cover
.NOTPARALLEL: test
.PHONY: test

## Tests

ct:
	@rebar3 ct
.PHONY: ct

eunit:
	@rebar3 eunit
.PHONY: eunit

cover:
	@rebar3 cover
.PHONY: cover

## Checks

dialyzer:
	@rebar3 as test dialyzer
.PHONY: dialyzer

xref:
	@rebar3 as test xref
.PHONY: xref

lint:
	@rebar3 as test lint
.PHONY: lint

find-unused-code:
	@rebar3 as test hank
.PHONY: lint

## Shell, docs and publication

shell: export ERL_FLAGS = +pc unicode
shell:
	@rebar3 as test shell

doc-dry:
	@rebar3 hex docs --dry-run

publish:
	@rebar3 hex publish
