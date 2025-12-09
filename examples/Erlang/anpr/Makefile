# Makefile for building Erlang ANPR example NIF
ERLANG_ROOT := $(shell erl -eval 'io:format("~s~n", [code:root_dir()])' -s init stop -noshell)
ERTS_VSN := $(shell erl -eval 'io:format("~s~n", [erlang:system_info(version)])' -s init stop -noshell)
ERTS_INCLUDE_DIR := $(ERLANG_ROOT)/erts-$(ERTS_VSN)/include

priv:
	mkdir -p priv

priv/tsanpr_nif.dll: priv c_src/tsanpr_nif.c
	gcc -Wall -O2 -I"$(ERTS_INCLUDE_DIR)" -shared -o $@ c_src/tsanpr_nif.c -L. -ltsanpr

priv/tsanpr_nif.so: priv c_src/tsanpr_nif.c
	gcc -Wall -O3 -fPIC -I"$(ERTS_INCLUDE_DIR)" -shared -o $@ c_src/tsanpr_nif.c -ldl

.PHONY: clean

clean:
	rm -rf priv/
