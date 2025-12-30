# Makefile for building Gleam ANPR example NIF on Linux
# The MIT License (MIT)
# Copyright © 2022-2025 TS-Solution Corp.

ERLANG_ROOT := $(shell erl -eval 'io:format("~s~n", [code:root_dir()])' -s init stop -noshell)
ERTS_VSN := $(shell erl -eval 'io:format("~s~n", [erlang:system_info(version)])' -s init stop -noshell)
ERTS_INCLUDE_DIR := $(ERLANG_ROOT)/erts-$(ERTS_VSN)/include

priv:
	mkdir -p priv

priv/tsanpr_nif.so: priv c_src/tsanpr_nif.c
	gcc -Wall -O3 -fPIC -I"$(ERTS_INCLUDE_DIR)" -shared -o $@ c_src/tsanpr_nif.c -ldl

.PHONY: clean

clean:
	rm -rf priv/
