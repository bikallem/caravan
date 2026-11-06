
.PHONY: build fmt test mutate coverage autobahn

build:
	dune build

fmt:
	dune fmt

test:
	dune test --force

# Mutation testing. dune runs each suite of test/ with its mutants, but not
# the suite of test/linux. That suite kills no mutant that the other suites do
# not kill, and it needs a new process for each mutant. windtrap tests the
# mutants of a suite in parallel children. It keeps the verdicts of each suite
# in a file of its own in _build/_mutants. The report at the end merges these
# files and lists the mutants that no suite kills.
# The target fails if the run of a suite without mutants fails, or if a
# mutant survives. Options:
# - MUTATE=websocket/ tests only the mutants of the files under this prefix.
#   A run without it first removes all verdicts.
# - MUTATE_JOBS is the number of children of each suite at the same time,
#   auto for one for each CPU.
# - MUTATE_STATEFUL_COUNT is the number of programs of the stateful test
#   (CARAVAN_STATEFUL_COUNT), 300 in a normal run. With less than 200, the
#   test can fail its own rule that each cover label must occur.
# - The race tests run 10 seeds for each mutant (CARAVAN_RACE_SEEDS), 100 in
#   a normal run.
MUTATE ?=
MUTATE_JOBS ?= auto
MUTATE_STATEFUL_COUNT ?= 200

mutate:
	dune build --instrument-with ppx_windtrap.mutate @@test/all
	$(if $(MUTATE),,rm -rf _build/_mutants)
	WINDTRAP_MUTATE=$(or $(MUTATE),1) WINDTRAP_MUTATE_JOBS=$(MUTATE_JOBS) \
	  CARAVAN_STATEFUL_COUNT=$(MUTATE_STATEFUL_COUNT) CARAVAN_RACE_SEEDS=10 \
	  dune build @@test/runtest --force --instrument-with ppx_windtrap.mutate
	dune exec --instrument-with ppx_windtrap.mutate windtrap -- mutants

coverage:
	find _build -name '*.coverage' -delete
	dune runtest test --force --instrument-with ppx_windtrap.coverage
	dune exec windtrap -- coverage

# The Autobahn WebSocket test suite, in a container: podman by default,
# CONTAINER=docker for docker. All cases except compression (12.x, 13.x).
# The reports go to autobahn/reports. The target fails if a case fails.
CONTAINER ?= podman

autobahn:
	dune build test/linux/autobahn_server.exe
	mkdir -p autobahn/reports
	: > autobahn/reports/server.log
	./_build/default/test/linux/autobahn_server.exe > autobahn/reports/server.log 2>&1 & server=$$!; \
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
