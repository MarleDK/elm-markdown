

.PHONY: reactor
reactor: 
	elm-reactor


.PHONY: test 
test:
	@echo "http://localhost:8001/Tests.elm"
	cd tests; elm-reactor -p 8001
