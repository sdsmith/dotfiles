BUILD ?= debug
CC ?= clang

CFLAGS.debug := -g
CPPFLAGS.debug := -DDEBUG

CFLAGS.release := -O3
CPPFLAGS.release := -DNDEBUG

CFLAGS.profile := $(CFLAGS.release) -fno-omit-frame-pointer
CPPFLAGS.profile := $(CPPFLAGS.release)

CFLAGS += -std=c99
CFLAGS += $(CFLAGS.$(BUILD))
CPPFLAGS += $(CPPFLAGS.$(BUILD))

all :: in_hg

in_hg: in_hg.c
	$(CC) $(CPPFLAGS) $(CFLAGS) $^ -o $@
clean ::
	@rm -f in_hg
