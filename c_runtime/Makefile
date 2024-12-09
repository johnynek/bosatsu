all: bosatsu_runtime.o test_out bosatsu_ext_Bosatsu_l_Predef.o bosatsu_ext_Bosatsu_l_Prog.o

bosatsu_generated.h: typegen.py
	python3 typegen.py impls > bosatsu_generated.h

bosatsu_decls_generated.h: typegen.py
	python3 typegen.py headers > bosatsu_decls_generated.h

bosatsu_runtime.o: bosatsu_runtime.h bosatsu_runtime.c bosatsu_decls_generated.h bosatsu_generated.h
	gcc -c -Wall -Werror bosatsu_runtime.c

# this will eventually have test code for the runtime and predef
test_exe: test.c bosatsu_runtime.o
	gcc -O3 -Wall -o test_exe test.c bosatsu_runtime.o

test_out: test_exe
	./test_exe > output.log 2>&1 || { cat output.log; rm -f output.log; false; }
	touch test_out

bosatsu_ext_Bosatsu_l_Predef.o: bosatsu_ext_Bosatsu_l_Predef.c bosatsu_runtime.o
	gcc -Wall -Werror -c bosatsu_ext_Bosatsu_l_Predef.c

bosatsu_ext_Bosatsu_l_Prog.o: bosatsu_ext_Bosatsu_l_Prog.c bosatsu_runtime.o
	gcc -Wall -Werror -c bosatsu_ext_Bosatsu_l_Prog.c

MAC_HOMEBREW_PATH = /opt/homebrew/Cellar/bdw-gc/8.2.8

boehm_example: boehm_example.c
	gcc -I$(MAC_HOMEBREW_PATH)/include boehm_example.c $(MAC_HOMEBREW_PATH)/lib/libgc.a -o boehm_example