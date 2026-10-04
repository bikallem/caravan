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

# The Autobahn WebSocket test suite, in a container: podman by default,
# CONTAINER=docker for docker. All cases except compression (12.x, 13.x).
# The reports go to autobahn/reports. The target fails if a case fails.
CONTAINER ?= podman

autobahn:
	dune build test/autobahn_server.exe
	mkdir -p autobahn/reports
	./_build/default/test/autobahn_server.exe & server=$$!; \
	sleep 1; \
	$(CONTAINER) run --rm --network host \
	  -v "$(CURDIR)/autobahn:/config:Z" -v "$(CURDIR)/autobahn/reports:/reports:Z" \
	  docker.io/crossbario/autobahn-testsuite \
	  wstest -m fuzzingclient -s /config/fuzzingclient.json > autobahn/reports/wstest.log 2>&1; \
	status=$$?; kill $$server; test $$status -eq 0
	jq -r '.caravan | to_entries | group_by(.value.behavior) | .[] | "\(.[0].value.behavior): \(length)"' \
	  autobahn/reports/servers/index.json
	jq -e '[.caravan[] | select(.behavior == "FAILED" or .behaviorClose == "FAILED")] | length == 0' \
	  autobahn/reports/servers/index.json > /dev/null
