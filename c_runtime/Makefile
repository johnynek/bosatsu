all: bosatsu_runtime.o test

bosatsu_generated.h: typegen.py
	python3 typegen.py impls > bosatsu_generated.h

bosatsu_decls_generated.h: typegen.py
	python3 typegen.py headers > bosatsu_decls_generated.h

bosatsu_runtime.o: bosatsu_runtime.h bosatsu_runtime.c bosatsu_decls_generated.h bosatsu_generated.h
	gcc -c -Wall -Werror bosatsu_runtime.c

# this will eventually have test code for the runtime and predef
test: test.c bosatsu_runtime.o
	gcc -O3 -Wall -o test test.c bosatsu_runtime.o
