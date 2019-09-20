################################################################################

STACK:=stack $(STACK_ARGS)

################################################################################

hoogle: ## Launch a hoogle server
	stack hoogle --rebuild --server
.PHONY: hoogle

ghci:		## Launch ghci with some default settings
	stack ghci \
		--package=pretty-simple \
		--ghci-options=-interactive-print=Text.Pretty.Simple.pPrint
.PHONY: ghci

ghcid:  ## Launch ghcid
	ghcid \
		--command "stack ghci --test --bench --ghci-options='-fdefer-typed-holes'" \
			--restart package.yaml
.PHONY: ghcid

ghcid-splices:  ## Launch ghcid and dump TH splices on reload
	ghcid \
		--command "stack ghci --test --ghci-options='-fdefer-typed-holes -ddump-splices -dsuppress-uniques -dsuppress-module-prefixes'" \
			--restart package.yaml
.PHONY: ghcid-splices

ghcid-test:  ## Launch ghcid and automatically run all tests
	ghcid \
		--command "stack ghci --test --bench --main-is=:test --ghci-options='-fdefer-typed-holes'" \
		--test main \
		--restart package.yaml
.PHONY: ghcid-test

ghcid-unit:  ## Launch ghcid and automatically run unit tests
	ghcid \
		--command "stack ghci --test --bench --main-is=:test" \
		--test ":main --skip=/FlatBuffers.Integration" \
		--restart package.yaml
.PHONY: ghcid-unit

ghcid-integration:  ## Launch ghcid and automatically run integration tests
	ghcid \
		--command "stack ghci --test --bench --main-is=:test" \
		--test ":main --match=/FlatBuffers.Integration" \
		--restart package.yaml
.PHONY: ghcid-integration

flatb: ## Generate java flatbuffers
	flatc -o ./test-api/src/main/java/ --java \
		./test/Examples/schema.fbs \
		./test/Examples/vector_of_unions.fbs
.PHONY: flatb

test-api: ## Generate java flatbuffers and launch test-api
	make flatb
	cd ./test-api/ && \
		sbt "~reStart"
.PHONY: test-api

test-api-detached: ## Generate java flatbuffers and launch test-api in detached mode
	make flatb
	cd ./test-api/ && \
		sbt -Djline.terminal=jline.UnsupportedTerminal run &
.PHONY: test-api-detached



test-lts:  ## Build the library and run the tests using lts-12.14
	stack test \
		--stack-yaml=./stack/stack.lts-12.14.yaml \
		--work-dir ".stack-work-lts-12.14"
.PHONY: test-lts

test-min:  ## Build the library and run the tests using lowest possible dependency versions
	stack test \
		--stack-yaml=./stack/stack.min.yaml \
		--work-dir ".stack-work-min"
.PHONY: test-min


release:  ## Creates a release package
	make test-lts
	make test-min
	stack clean
	stack test
	stack sdist
.PHONY: release



hlint: ## Runs hlint on the project
	hlint .
.PHONY: hlint

docs:  ## Builds haddock documentation and watch files for changes
	$(STACK) haddock --no-haddock-deps --file-watch
.PHONY: docs

################################################################################

help:	## Display this message
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
.PHONY: help
.DEFAULT_GOAL := help
