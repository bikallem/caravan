ML_FILES = $(shell find . \( -path ./_build -o -path ./.direnv -o -path ./.git \) -prune \
	-o \( -name '*.ml' -o -name '*.mli' \) -print)

.PHONY: build fmt test

build:
	dune build

fmt:
	ocp-indent -i $(ML_FILES)

test:
	dune test --force
