import BuildLibrary as ___iBuildLibrary0

lib1 = ___iBuildLibrary0.library(___iBuildLibrary0.files((1,
            "file1.bosatsu",
            (1, "file2.bosatsu", (0,)))),
    ___iBuildLibrary0.empty)

lib2 = ___iBuildLibrary0.build((___iBuildLibrary0.files((0,)),
        ___iBuildLibrary0.build_all((1, lib1, (0,)))))