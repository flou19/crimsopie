PATSHOME ?= /usr/lib/ats2-postiats-0.4.2
PATSCC := $(PATSHOME)/bin/patscc
PATSFLAGS := -IATS . -IATS $(PATSHOME) -DATS_MEMALLOC_LIBC -O2 -cleanaft
VERSION := 0.0.1-alpha

OBJS = \
  src/lexer/token_dats.o \
  src/lexer/lexer_dats.o \
  src/parser/ast_dats.o \
  src/parser/parser_dats.o \
  src/errors/error_dats.o \
  src/checker/env_dats.o \
  src/checker/verify_dats.o \
  src/emitter/pu4_dats.o \
  src/translator/pu4-to-ats2_dats.o \
  src/utils/io_dats.o \
  src/cli/args_dats.o \
  src/main_dats.o

.PHONY: all clean release test test-neg test-pos benchmark

all: crimsopie

%_dats.o: %.dats
	$(PATSCC) $(PATSFLAGS) -c $< -o $@

crimsopie: $(OBJS)
	$(PATSCC) $(PATSFLAGS) -o $@ $(OBJS)
	chmod 755 $@

clean:
	rm -f $(OBJS) crimsopie
	rm -f *_?ats.c src/*/*_?ats.c src/*/*_dats.c src/*_dats.c
	rm -f src/*/*_dats.o src/*_dats.o

test-neg: crimsopie
	@fail=0; for f in examples/negative/*.pu8; do \
	  ./crimsopie check "$$f" >/dev/null 2>&1; \
	  if [ $$? -eq 0 ]; then echo "FAIL $$f (expected EXIT:1)"; fail=1; else echo "OK $$f"; fi; \
	done; exit $$fail

test-pos: crimsopie
	@fail=0; for f in examples/parallel_sort.pu8 examples/commuting_operators.pu8 examples/dimension_ok.pu8 examples/inv_two.pu8 examples/inv_three.pu8 examples/pair.pu8; do \
	  ./crimsopie check "$$f" >/dev/null 2>&1; \
	  if [ $$? -ne 0 ]; then echo "FAIL $$f (expected EXIT:0)"; fail=1; else echo "OK $$f"; fi; \
	done; exit $$fail

test: test-neg test-pos

benchmark: crimsopie
	@if [ -f benchmark/bench_parallel_sort.pu8 ]; then \
	  /usr/bin/time -f "elapsed %e sec" ./crimsopie build benchmark/bench_parallel_sort.pu8; \
	else echo "benchmark/bench_parallel_sort.pu8 missing"; fi

release: clean all
	@echo "crimsopie v$(VERSION) built"
	@./crimsopie --version
