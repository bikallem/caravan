
# Test suites with mutation results to merge.
SUITES = test_core test_h1 test_h1_server test_net test_server test_server_stateful \
  test_stop_signal test_websocket test_client test_conformance

.PHONY: build fmt test mutate coverage autobahn conformance

build:
	dune build

fmt:
	dune fmt

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
#   makes a test hang fails at this limit. Under many new connections, a
#   loopback SYN can need a retransmission, and a connect then takes 1 s or
#   3 s. Thus the limit is more than 3 s.
# - MUTATE_STATEFUL_COUNT is the number of programs of the stateful test,
#   300 in a normal run. Each program opens many connections, and the check
#   runs the test again for each mutant. With less than 200, the test can
#   fail its own rule that each label must occur.
MUTATE ?=
MUTATE_JOBS ?= $(shell nproc)
MUTATE_SHARDS ?= 8
MUTATE_TIMEOUT ?= 5
MUTATE_STATEFUL_COUNT ?= 200
MUTATE_FILES = $(filter $(MUTATE)%,$(sort $(wildcard src/*.ml websocket/*.ml client/*.ml)))
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
	  TEST_DOMAINS=1 CARAVAN_STATEFUL_COUNT=$(MUTATE_STATEFUL_COUNT) \
	    ./$$exe --mutate=$$2 --timeout=$(MUTATE_TIMEOUT) \
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

# The HTTP conformance suite of cispa/http-conformance (commit 24f75be), in a
# container. The tool sends its probes through mitmproxy, and its direct tests
# over TCP, to conformance_server.exe on CONFORMANCE_PORT. It keeps its
# results in a Postgres container on 127.0.0.1:55432. The run takes about 15
# minutes, and the first build of the image takes some minutes more. All
# containers use the host network and no proxy. The upstream runner hides its
# failures, thus the target stops if the run is not valid
# (conformance/check.sql). The violations go to
# conformance/reports/violations.tsv, and the number of verdicts for each rule
# goes to conformance/reports/summary.tsv. The target fails for each violation
# that matches no row of conformance/expected.tsv, and for each row of
# expected.tsv that matches no violation (conformance/compare.awk). The target
# needs podman: it uses --http-proxy=false, which docker does not have.
CONFORMANCE_PORT ?= 9080
CONFORMANCE_IMAGE = localhost/caravan-conformance
CONFORMANCE_PG = caravan-conformance-postgres
# postgres:16, 2026-10-06.
CONFORMANCE_PG_IMAGE = docker.io/library/postgres@sha256:ca0bd484cb98bf4b24eb1010e73fb3fcbd6714d240fbc1a10eea5b7dbecb641d
CONFORMANCE_PSQL = $(CONTAINER) exec -i $(CONFORMANCE_PG) psql -X -q -v ON_ERROR_STOP=1 -U postgres -p 55432

conformance:
	@test "$(notdir $(CONTAINER))" = podman \
	  || { echo "make conformance needs podman (CONTAINER=podman)."; exit 1; }
	dune build test/conformance_server.exe
	mkdir -p conformance/reports
	$(CONTAINER) build -t $(CONFORMANCE_IMAGE) conformance > conformance/reports/build.log 2>&1 \
	  || { tail -20 conformance/reports/build.log; exit 1; }
	: > conformance/reports/server.log
	./_build/default/test/conformance_server.exe $(CONFORMANCE_PORT) \
	  > conformance/reports/server.log 2>&1 & server=$$!; \
	trap 'kill $$server 2>/dev/null; $(CONTAINER) rm -f $(CONFORMANCE_PG) > /dev/null 2>&1' EXIT; \
	trap 'exit 130' INT TERM; \
	pw=$$(od -An -N12 -tx1 /dev/urandom | tr -d ' \n'); \
	for i in $$(seq 50); do grep -q '^listening' conformance/reports/server.log && break; sleep 0.1; done; \
	grep -q '^listening' conformance/reports/server.log || { echo "The server did not start."; exit 1; }; \
	$(CONTAINER) rm -f $(CONFORMANCE_PG) > /dev/null 2>&1; \
	$(CONTAINER) run -d --rm --name $(CONFORMANCE_PG) --network host --http-proxy=false \
	  -e POSTGRES_PASSWORD=$$pw $(CONFORMANCE_PG_IMAGE) \
	  postgres -p 55432 -c listen_addresses=127.0.0.1 > /dev/null || exit 1; \
	for i in $$(seq 60); do \
	  $(CONTAINER) exec $(CONFORMANCE_PG) pg_isready -q -h 127.0.0.1 -p 55432 && break; sleep 1; \
	done; \
	$(CONTAINER) exec $(CONFORMANCE_PG) pg_isready -q -h 127.0.0.1 -p 55432 \
	  || { echo "Postgres did not start."; exit 1; }; \
	$(CONTAINER) run --rm --network host --http-proxy=false \
	  -e PGHOST=127.0.0.1 -e PGPORT=55432 -e PGUSER=postgres -e PGPASSWORD=$$pw \
	  $(CONFORMANCE_IMAGE) sh -c 'echo caravan_http_port=$(CONFORMANCE_PORT) > testbed/.env \
	    && exec poetry run python run_checks.py --mode=local --max_workers=1' \
	  > conformance/reports/run.log 2>&1 \
	  || { tail -20 conformance/reports/run.log; exit 1; }; \
	db=$$($(CONFORMANCE_PSQL) -tA -c "SELECT datname FROM pg_database WHERE datname LIKE 'results_local_%'"); \
	test $$(echo "$$db" | wc -w) -eq 1 || { echo "Expected one result database, found: $$db"; exit 1; }; \
	$(CONFORMANCE_PSQL) -d $$db < conformance/check.sql > conformance/reports/violations.tsv || exit 1; \
	$(CONFORMANCE_PSQL) -d $$db < conformance/summary.sql > conformance/reports/summary.tsv || exit 1; \
	echo "The run is valid. Violations for each rule and verdict:"; \
	cut -f 2,6 conformance/reports/violations.tsv | sort | uniq -c; \
	awk -f conformance/compare.awk conformance/expected.tsv conformance/reports/violations.tsv
