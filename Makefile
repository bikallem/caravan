ML_FILES = $(shell find . \( -path ./_build -o -path ./.direnv -o -path ./.git \) -prune \
	-o \( -name '*.ml' -o -name '*.mli' \) -print)

# Test suites with mutation results to merge.
SUITES = test_core test_h1 test_h1_server test_net test_server test_server_stateful \
  test_websocket

.PHONY: build fmt test mutate coverage autobahn

build:
	dune build

fmt:
	ocp-indent -i $(ML_FILES)

test:
	dune test --force

# Mutation testing. Each suite runs on one domain: the runner forks for each
# mutant, and OCaml forbids a fork after a domain starts. The runner tests
# the mutants of one suite one at a time, and it keeps its verdicts in a file
# named from the path of the executable. Thus each suite runs as
# MUTATE_SHARDS copies, each copy for a group of source files, and all the
# copies run in parallel, MUTATE_JOBS at a time. The copies are beside the
# suites, because the signal tests start a program from there. They run
# directly, because dune lets only one build run. Each job keeps its output
# in _build/_mutate_logs. The target fails if a job gives no result, for
# example when its dry run fails. The report at the end lists the mutants
# that no job kills. Options:
# - MUTATE=websocket/ tests only the mutants of the files under this prefix.
#   A run without it first removes all verdicts.
# - MUTATE_TIMEOUT is the time limit of a test in seconds. A mutant that
#   makes a test hang fails at this limit.
MUTATE ?=
MUTATE_JOBS ?= $(shell nproc)
MUTATE_SHARDS ?= 4
MUTATE_TIMEOUT ?= 5
MUTATE_FILES = $(filter $(MUTATE)%,$(sort $(wildcard src/*.ml websocket/*.ml)))
MUTATE_LOGS = _build/_mutate_logs

mutate:
	dune build --instrument-with ppx_windtrap.mutate $(SUITES:%=./test/%.exe)
	$(if $(MUTATE),,rm -rf _build/_mutants)
	rm -rf $(MUTATE_LOGS) && mkdir -p $(MUTATE_LOGS)
	for t in $(SUITES); do \
	  for k in $$(seq $(MUTATE_SHARDS)); do \
	    files=$$(echo $(MUTATE_FILES) | tr ' ' '\n' | awk -v n=$(MUTATE_SHARDS) -v k=$$k \
	      '(NR - 1) % n == k - 1' | paste -sd, -); \
	    [ -n "$$files" ] && echo $$t $$k $$files; \
	  done; \
	done | xargs -P $(MUTATE_JOBS) -n 3 sh -c ' \
	  exe=_build/default/test/$$0.mutate-$$1.exe; \
	  cp -f _build/default/test/$$0.exe $$exe; \
	  TEST_DOMAINS=1 ./$$exe --mutate=$$2 --timeout=$(MUTATE_TIMEOUT) \
	    > $(MUTATE_LOGS)/$$0.$$1.log 2>&1 || true'
	@failed=$$(grep -L -E "^mutants|leaves no mutant|links no instrumented" $(MUTATE_LOGS)/*.log); \
	if [ -n "$$failed" ]; then \
	  echo "These jobs gave no result:"; \
	  for f in $$failed; do echo "  $$f: $$(tail -1 $$f)"; done; \
	  exit 1; \
	fi
	dune exec --instrument-with ppx_windtrap.mutate windtrap -- mutants

coverage:
	find _build -name '*.coverage' -delete
	TEST_DOMAINS=1 dune runtest --force --instrument-with ppx_windtrap.coverage
	dune exec windtrap -- coverage

# The Autobahn WebSocket test suite, in a container: podman by default,
# CONTAINER=docker for docker. All cases except compression (12.x, 13.x).
# The reports go to autobahn/reports. The target fails if a case fails.
CONTAINER ?= podman

autobahn:
	dune build test/autobahn_server.exe
	mkdir -p autobahn/reports
	: > autobahn/reports/server.log
	./_build/default/test/autobahn_server.exe > autobahn/reports/server.log 2>&1 & server=$$!; \
	trap 'kill $$server 2>/dev/null' EXIT INT TERM; \
	for i in $$(seq 50); do grep -q '^listening' autobahn/reports/server.log && break; sleep 0.1; done; \
	grep -q '^listening' autobahn/reports/server.log || { echo "The server did not start."; exit 1; }; \
	$(CONTAINER) run --rm --network host \
	  -v "$(CURDIR)/autobahn:/config:Z" -v "$(CURDIR)/autobahn/reports:/reports:Z" \
	  docker.io/crossbario/autobahn-testsuite \
	  wstest -m fuzzingclient -s /config/fuzzingclient.json > autobahn/reports/wstest.log 2>&1; \
	status=$$?; test $$status -eq 0
	jq -r '.caravan | to_entries | group_by(.value.behavior) | .[] | "\(.[0].value.behavior): \(length)"' \
	  autobahn/reports/servers/index.json
	jq -e '[.caravan[] | select(.behavior == "FAILED" or .behaviorClose == "FAILED")] | length == 0' \
	  autobahn/reports/servers/index.json > /dev/null
