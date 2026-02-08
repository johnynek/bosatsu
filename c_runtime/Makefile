.PHONY: install bench

OS := $(shell uname)
ROOTDIR ?= $(shell git rev-parse --show-toplevel)
VERSION ?= $(shell $(ROOTDIR)/bosatsuj version -g)
CFLAGS ?= -O2

# Platform-specific settings
ifneq ($(wildcard /opt/homebrew/Cellar/bdw-gc),)  # Check if the directory exists
    LATEST_VERSION := $(shell ls -v /opt/homebrew/Cellar/bdw-gc | tail -n 1)
    BOEHM_GC := /opt/homebrew/Cellar/bdw-gc/$(LATEST_VERSION)
endif

ifeq ($(OS), Darwin)
    CFLAGS += -I$(BOEHM_GC)/include
    LIBS += "$(BOEHM_GC)/lib/libgc.a"
else ifeq ($(OS), Linux)
    LIBS += -lgc
endif

all: bosatsu_runtime.o test_out bosatsu_ext_Bosatsu_l_Predef.o bosatsu_ext_Bosatsu_l_Prog.o bosatsu_ext_Bosatsu_l_IO_l_Std.o bosatsu_ext_Bosatsu_l_Collection_l_Array.o

bosatsu_generated.h: typegen.py
	python3 typegen.py impls > bosatsu_generated.h

bosatsu_decls_generated.h: typegen.py
	python3 typegen.py headers > bosatsu_decls_generated.h

bosatsu_runtime.o: bosatsu_runtime.h bosatsu_runtime.c bosatsu_decls_generated.h bosatsu_generated.h
	gcc -c $(CFLAGS) -Wall -Werror bosatsu_runtime.c

# this will eventually have test code for the runtime and predef
test_exe: test.c bosatsu_runtime.o
	gcc -O3 $(CFLAGS) -Wall -o test_exe test.c bosatsu_runtime.o $(LIBS)

test_out: test_exe
	./test_exe > output.log 2>&1 || { cat output.log; rm -f output.log; false; }
	touch test_out

bench_exe: bench.c bosatsu_runtime.o
	gcc $(CFLAGS) -Wall -o bench_exe bench.c bosatsu_runtime.o $(LIBS)

bench: bench_exe
	./bench_exe

bosatsu_ext_Bosatsu_l_Predef.o: bosatsu_ext_Bosatsu_l_Predef.c bosatsu_runtime.o
	gcc $(CFLAGS) -Wall -Werror -c bosatsu_ext_Bosatsu_l_Predef.c

bosatsu_ext_Bosatsu_l_Prog.o: bosatsu_ext_Bosatsu_l_Prog.c bosatsu_runtime.o
	gcc $(CFLAGS) -Wall -Werror -c bosatsu_ext_Bosatsu_l_Prog.c

bosatsu_ext_Bosatsu_l_IO_l_Std.o: bosatsu_ext_Bosatsu_l_IO_l_Std.c bosatsu_runtime.o
	gcc $(CFLAGS) -Wall -Werror -c bosatsu_ext_Bosatsu_l_IO_l_Std.c

bosatsu_ext_Bosatsu_l_Collection_l_Array.o: bosatsu_ext_Bosatsu_l_Collection_l_Array.c bosatsu_runtime.o
	gcc $(CFLAGS) -Wall -Werror -c bosatsu_ext_Bosatsu_l_Collection_l_Array.c

bosatsu_platform.a: bosatsu_ext_Bosatsu_l_Predef.o bosatsu_ext_Bosatsu_l_Prog.o bosatsu_ext_Bosatsu_l_IO_l_Std.o bosatsu_ext_Bosatsu_l_Collection_l_Array.o bosatsu_runtime.o
	ar rcs bosatsu_platform.a bosatsu_ext_Bosatsu_l_Predef.o bosatsu_ext_Bosatsu_l_Prog.o bosatsu_ext_Bosatsu_l_IO_l_Std.o bosatsu_ext_Bosatsu_l_Collection_l_Array.o bosatsu_runtime.o

boehm_example: boehm_example.c
	gcc $(CFLAGS) boehm_example.c $(LIBS) -o boehm_example

install: bosatsu_platform.a bosatsu_decls_generated.h bosatsu_generated.h bosatsu_ext_Bosatsu_l_Predef.h bosatsu_ext_Bosatsu_l_Prog.h bosatsu_ext_Bosatsu_l_IO_l_Std.h bosatsu_ext_Bosatsu_l_Collection_l_Array.h bosatsu_runtime.h
	python3 install.py --root $(ROOTDIR) --include bosatsu_decls_generated.h --include bosatsu_generated.h --include bosatsu_ext_Bosatsu_l_Predef.h --include bosatsu_ext_Bosatsu_l_Prog.h --include bosatsu_ext_Bosatsu_l_IO_l_Std.h --include bosatsu_ext_Bosatsu_l_Collection_l_Array.h --include bosatsu_runtime.h --lib bosatsu_platform.a --version $(VERSION)
