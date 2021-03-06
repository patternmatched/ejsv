all: build

build:
	rebar3 compile

clean:
	rebar3 clean

test:
	rebar3 do ct --cover, cover
	-@command -v links 2>&1 > /dev/null \
		&& links -dump $(PWD)/_build/test/cover/index.html \
		| sed '1,/ct summary/d;/^\s*$$/d'

.PHONY: all build clean test
