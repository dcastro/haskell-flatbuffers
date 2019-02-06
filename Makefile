# Based on https://github.com/roman/Haskell-capataz/blob/master/Makefile

################################################################################

STACK:=stack $(STACK_ARGS)

################################################################################

ghcid:  ## Launch ghcid
	ghcid \
		--command "stack ghci" \
			--restart package.yaml
.PHONY: ghcid

ghcid-unit:  ## Launch ghcid and automatically run unit tests
	ghcid \
		--command "stack ghci \
			--test \
			--main-is :unit" \
		--test main \
		--restart package.yaml

ghcid-int:  ## Launch ghcid and automatically run integration tests
	ghcid \
		--command "stack ghci \
			--test \
			--main-is :integration" \
		--test main \
		--restart package.yaml

flatb: ## Generate java flatbuffers
	cd ./test/integration/ && \
	flatc -o ./test-api/src/main/java/ --java schema.fbs

test-api: ## Generate java flatbuffers and launch test-api
	make flatb && \
		cd ./test/integration/test-api/ && \
		sbt "~reStart"

test-api-detached: ## Generate java flatbuffers and launch test-api in detached mode
	make flatb && \
		cd ./test/integration/test-api/ && \
		sbt -Djline.terminal=jline.UnsupportedTerminal run &

docs:  ## Builds haddock documentation and watch files for changes
	$(STACK) haddock --no-haddock-deps --file-watch
.PHONY: docs

################################################################################

help:	## Display this message
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
.PHONY: help
.DEFAULT_GOAL := help
