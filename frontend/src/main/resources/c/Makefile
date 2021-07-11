CFLAGS += -std=c99
CFLAGS_RELEASE := -O3
CFLAGS_DEBUG := -g -fsanitize=address

LDFLAGS = -lgmp

.PHONY: run

build:
	mkdir -p $@

build/%.debug: %.c | build
	$(CC) $(CFLAGS) $(CFLAGS_DEBUG) $(LDFLAGS) $< -o $@

build/%.release: %.c | build
	$(CC) $(CFLAGS) $(CFLAGS_RELEASE) $(LDFLAGS) $< -o $@

run: build/main.release
	./build/main.release
