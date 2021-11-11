.PHONY: test-chibi

# Testing through docker
# pulls in srfi 126 implementation
# which other wise is untested
test-chibi-docker:
	docker-compose run --rm chibi

test-gauche-docker:
	docker-compose run --rm gauche

test-kawa-docker:
	docker-compose run --rm kawa

test-chibi:
	chibi-scheme -I . srfi-225-test.scm

test-gauche:
	gosh -I . srfi-225-test.scm
