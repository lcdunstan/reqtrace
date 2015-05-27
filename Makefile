.PHONY: all clean distclean setup build doc install reinstall test
all: build

J ?= 2
PREFIX ?= /usr/local
NAME=mdns

TESTS ?= --enable-tests
COVERAGE ?=

-include Makefile.config

setup.data: setup.bin
	./setup.bin -configure $(TESTS) $(COVERAGE) --prefix $(PREFIX)

distclean: setup.data setup.bin
	./setup.bin -distclean $(OFLAGS)
	$(RM) setup.bin

setup: setup.data

build: setup.data  setup.bin
	./setup.bin -build -j $(J) $(OFLAGS)

clean:
	ocamlbuild -clean
	rm -f setup.data setup.bin

doc: setup.data setup.bin
	./setup.bin -doc -j $(J) $(OFLAGS)

install: 
	ocamlfind remove $(NAME) $(OFLAGS)
	./setup.bin -install

reinstall: clean build install

setup.bin: setup.ml
	ocamlopt.opt -o $@ $< || ocamlopt -o $@ $< || ocamlc -o $@ $<
	$(RM) setup.cmx setup.cmi setup.o setup.cmo

# https://forge.ocamlcore.org/tracker/index.php?func=detail&aid=1363&group_id=162&atid=730
test: build
	./setup.bin -test -runner sequential

coverage: build
	rm -f lib_test/ounit/bisect*.out
	./setup.bin -test -runner sequential
	bisect-report -html _build/coverage -I _build/ lib_test/ounit/bisect*.out

