.PHONY: install bench

OS := $(shell uname)
ROOTDIR ?= $(shell git rev-parse --show-toplevel)
VERSION ?= $(shell $(ROOTDIR)/bosatsuj version -g)
CC ?= cc
PKG_CONFIG ?= pkg-config
PROFILE ?= release
CPPFLAGS ?=
LDFLAGS ?=
LIBS ?=

ifeq ($(PROFILE),debug)
	CFLAGS ?= -O0 -g3
else ifeq ($(PROFILE),release)
	CFLAGS ?= -O2
else
$(error PROFILE must be one of: release, debug)
endif

# Prefer pkg-config for portability.
GC_CFLAGS := $(shell $(PKG_CONFIG) --cflags bdw-gc 2>/dev/null)
GC_LIBS := $(shell $(PKG_CONFIG) --libs bdw-gc 2>/dev/null)

# Fallbacks for environments without pkg-config metadata.
ifeq ($(strip $(GC_CFLAGS)$(GC_LIBS)),)
ifeq ($(OS),Darwin)
ifneq ($(wildcard /opt/homebrew/opt/bdw-gc/include/gc.h),)
GC_CFLAGS += -I/opt/homebrew/opt/bdw-gc/include
GC_LIBS += -L/opt/homebrew/opt/bdw-gc/lib -lgc
else ifneq ($(wildcard /usr/local/opt/bdw-gc/include/gc.h),)
GC_CFLAGS += -I/usr/local/opt/bdw-gc/include
GC_LIBS += -L/usr/local/opt/bdw-gc/lib -lgc
else ifneq ($(wildcard /opt/local/include/gc.h),)
GC_CFLAGS += -I/opt/local/include
GC_LIBS += -L/opt/local/lib -lgc
else
GC_LIBS += -lgc
endif
else ifeq ($(OS),Linux)
GC_LIBS += -lgc
endif

endif

ALL_CPPFLAGS := $(CPPFLAGS) $(GC_CFLAGS)
ALL_LIBS := $(GC_LIBS) $(LIBS) -lm

all: bosatsu_runtime.o test_out bosatsu_ext_Bosatsu_l_Predef.o bosatsu_ext_Bosatsu_l_Prog.o bosatsu_ext_Bosatsu_l_IO_l_Std.o bosatsu_ext_Bosatsu_l_Collection_l_Array.o bosatsu_ext_Bosatsu_l_Num_l_Float64.o

bosatsu_generated.h: typegen.py
	python3 typegen.py impls > bosatsu_generated.h

bosatsu_decls_generated.h: typegen.py
	python3 typegen.py headers > bosatsu_decls_generated.h

bosatsu_runtime.o: bosatsu_runtime.h bosatsu_runtime.c bosatsu_decls_generated.h bosatsu_generated.h
	$(CC) $(ALL_CPPFLAGS) $(CFLAGS) -Wall -Werror -c bosatsu_runtime.c

# this will eventually have test code for the runtime and predef
test_exe: test.c bosatsu_runtime.o bosatsu_ext_Bosatsu_l_Num_l_Float64.o
	$(CC) $(ALL_CPPFLAGS) $(CFLAGS) -Wall -o test_exe test.c bosatsu_runtime.o bosatsu_ext_Bosatsu_l_Num_l_Float64.o $(LDFLAGS) $(ALL_LIBS)

test_out: test_exe
	./test_exe > output.log 2>&1 || { cat output.log; rm -f output.log; false; }
	touch test_out

bench_exe: bench.c bosatsu_runtime.o
	$(CC) $(ALL_CPPFLAGS) $(CFLAGS) -Wall -o bench_exe bench.c bosatsu_runtime.o $(LDFLAGS) $(ALL_LIBS)

bench: bench_exe
	./bench_exe

bosatsu_ext_Bosatsu_l_Predef.o: bosatsu_ext_Bosatsu_l_Predef.c bosatsu_runtime.o
	$(CC) $(ALL_CPPFLAGS) $(CFLAGS) -Wall -Werror -c bosatsu_ext_Bosatsu_l_Predef.c

bosatsu_ext_Bosatsu_l_Prog.o: bosatsu_ext_Bosatsu_l_Prog.c bosatsu_runtime.o
	$(CC) $(ALL_CPPFLAGS) $(CFLAGS) -Wall -Werror -c bosatsu_ext_Bosatsu_l_Prog.c

bosatsu_ext_Bosatsu_l_IO_l_Std.o: bosatsu_ext_Bosatsu_l_IO_l_Std.c bosatsu_runtime.o
	$(CC) $(ALL_CPPFLAGS) $(CFLAGS) -Wall -Werror -c bosatsu_ext_Bosatsu_l_IO_l_Std.c

bosatsu_ext_Bosatsu_l_Collection_l_Array.o: bosatsu_ext_Bosatsu_l_Collection_l_Array.c bosatsu_runtime.o
	$(CC) $(ALL_CPPFLAGS) $(CFLAGS) -Wall -Werror -c bosatsu_ext_Bosatsu_l_Collection_l_Array.c

bosatsu_ext_Bosatsu_l_Num_l_Float64.o: bosatsu_ext_Bosatsu_l_Num_l_Float64.c bosatsu_runtime.o
	$(CC) $(ALL_CPPFLAGS) $(CFLAGS) -Wall -Werror -c bosatsu_ext_Bosatsu_l_Num_l_Float64.c

bosatsu_platform.a: bosatsu_ext_Bosatsu_l_Predef.o bosatsu_ext_Bosatsu_l_Prog.o bosatsu_ext_Bosatsu_l_IO_l_Std.o bosatsu_ext_Bosatsu_l_Collection_l_Array.o bosatsu_ext_Bosatsu_l_Num_l_Float64.o bosatsu_runtime.o
	ar rcs bosatsu_platform.a bosatsu_ext_Bosatsu_l_Predef.o bosatsu_ext_Bosatsu_l_Prog.o bosatsu_ext_Bosatsu_l_IO_l_Std.o bosatsu_ext_Bosatsu_l_Collection_l_Array.o bosatsu_ext_Bosatsu_l_Num_l_Float64.o bosatsu_runtime.o

boehm_example: boehm_example.c
	$(CC) $(ALL_CPPFLAGS) $(CFLAGS) boehm_example.c $(LDFLAGS) $(ALL_LIBS) -o boehm_example

install: bosatsu_platform.a bosatsu_decls_generated.h bosatsu_generated.h bosatsu_ext_Bosatsu_l_Predef.h bosatsu_ext_Bosatsu_l_Prog.h bosatsu_ext_Bosatsu_l_IO_l_Std.h bosatsu_ext_Bosatsu_l_Collection_l_Array.h bosatsu_ext_Bosatsu_l_Num_l_Float64.h bosatsu_runtime.h
	python3 install.py --root $(ROOTDIR) --include bosatsu_decls_generated.h --include bosatsu_generated.h --include bosatsu_ext_Bosatsu_l_Predef.h --include bosatsu_ext_Bosatsu_l_Prog.h --include bosatsu_ext_Bosatsu_l_IO_l_Std.h --include bosatsu_ext_Bosatsu_l_Collection_l_Array.h --include bosatsu_ext_Bosatsu_l_Num_l_Float64.h --include bosatsu_runtime.h --lib bosatsu_platform.a --version $(VERSION) --cc "$(CC)" --cflags "$(CFLAGS)" --cppflags "$(ALL_CPPFLAGS)" --ldflags "$(LDFLAGS)" --libs "$(ALL_LIBS)" --pkg-config "$(PKG_CONFIG)" --profile "$(PROFILE)"
