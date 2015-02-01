all: compile

compile:
	bin/rebar compile

run: compile
	./run

start_console: compile
	./start_console

clean:
	@rm -R ebin/* || true
