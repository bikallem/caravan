ML_FILES = $(shell find . \( -path ./_build -o -path ./.direnv -o -path ./.git \) -prune \
	-o \( -name '*.ml' -o -name '*.mli' \) -print)

# Test suites with mutation results to merge.
SUITES = test_core test_h1 test_h1_server test_net test_server test_server_stateful

.PHONY: build fmt test mutate coverage

build:
	dune build

fmt:
	ocp-indent -i $(ML_FILES)

test:
	dune test --force

# One domain: mutation testing forks per mutant, which OCaml forbids once a
# domain was spawned. A suite with survivors fails; the merged report after
# the loop lists them.
mutate:
	for t in $(SUITES); do \
	  TEST_DOMAINS=1 dune exec --instrument-with ppx_windtrap.mutate test/$$t.exe -- --mutate \
	    > /dev/null || true; \
	done
	dune exec --instrument-with ppx_windtrap.mutate windtrap -- mutants

coverage:
	find _build -name '*.coverage' -delete
	TEST_DOMAINS=1 dune runtest --force --instrument-with ppx_windtrap.coverage
	dune exec windtrap -- coverage
