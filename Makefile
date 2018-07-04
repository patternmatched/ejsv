all: build

build:
	rebar3 compile

clean:
	rebar3 clean

test:
	rebar3 do ct --cover, cover

.PHONY: all build clean test
