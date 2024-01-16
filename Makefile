################################################################################

GHCI_OPTS:=\
	--ghci-options '-fdefer-typed-holes -funclutter-valid-hole-fits' \
	--work-dir '.stack-work-ghci' \
	--test \
	--bench \
	--main-is ":test"

GHCID_OPTS:=\
	--restart package.yaml \
	--allow-eval

################################################################################

hoogle: ## Launch a hoogle server
	stack hoogle --rebuild --server

ghci:		## Launch ghci with some default settings
	stack ghci \
		--package=pretty-simple \
		--ghci-options=-interactive-print=Text.Pretty.Simple.pPrint \
		$(GHCI_OPTS)

ghcid:  ## Launch ghcid
	ghcid \
		--command "stack ghci $(GHCI_OPTS)" \
		$(GHCID_OPTS)

ghcid-splices:  ## Launch ghcid and dump TH splices on reload
	ghcid \
		--command "stack ghci $(GHCI_OPTS) \
			--ghci-options='-ddump-splices -dsuppress-uniques -dsuppress-module-prefixes' " \
		$(GHCID_OPTS)

ghcid-test:  ## Launch ghcid and automatically run all tests
	ghcid \
		--command "stack ghci $(GHCI_OPTS)" \
		--test main \
		$(GHCID_OPTS)

ghcid-unit:  ## Launch ghcid and automatically run unit tests
	ghcid \
		--command "stack ghci $(GHCI_OPTS)" \
		--test ":main --skip=/FlatBuffers.Integration" \
		$(GHCID_OPTS)

ghcid-integration:  ## Launch ghcid and automatically run integration tests
	ghcid \
		--command "stack ghci $(GHCI_OPTS)" \
		--test ":main --match=/FlatBuffers.Integration" \
		$(GHCID_OPTS)

flatb: ## Generate java flatbuffers
	flatc -o ./test-api/src/main/java/ --java \
		./test/Examples/schema.fbs \
		./test/Examples/vector_of_unions.fbs

test-api: ## Generate java flatbuffers and launch test-api
	make flatb
	cd ./test-api/ && \
		sbt "~reStart"

test-api-detached: ## Generate java flatbuffers and launch test-api in detached mode
	make flatb
	cd ./test-api/ && \
		sbt -Djline.terminal=jline.UnsupportedTerminal run &



test-lts:  ## Build the library and run the tests using lts-12.14
	stack test \
		--stack-yaml=./stack/stack.lts-12.14.yaml \
		--work-dir ".stack-work-lts-12.14"

test-min:  ## Build the library and run the tests using lowest possible dependency versions
	stack test \
		--stack-yaml=./stack/stack.min.yaml \
		--work-dir ".stack-work-min"


release:  ## Creates a release package
	stack clean --stack-yaml=./stack/stack.lts-12.14.yaml --work-dir ".stack-work-lts-12.14"
	stack test  --stack-yaml=./stack/stack.lts-12.14.yaml --work-dir ".stack-work-lts-12.14"
	stack clean --stack-yaml=./stack/stack.min.yaml	      --work-dir ".stack-work-min"
	stack test  --stack-yaml=./stack/stack.min.yaml       --work-dir ".stack-work-min"
	stack clean
	stack test  --ghc-options=-Werror
	make hlint
	stack sdist



hlint: ## Runs hlint on the project
	hlint .

docs:  ## Builds haddock documentation and watch files for changes
	stack haddock --no-haddock-deps --file-watch

################################################################################

help:	## Display this message
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
.DEFAULT_GOAL := help

.PHONY: test-api
