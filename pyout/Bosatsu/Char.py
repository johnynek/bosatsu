import unittest as ___iunittest0

___a0 = (1, "s")
if ___a0[0] == 1:
    ___a1 = ___a0[1]
    ___t0 = ___a1 == "s"
else:
    ___t0 = False
___a2 = (1, "👋")
if ___a2[0] == 1:
    ___a3 = ___a2[1]
    ___t1 = ___a3 == "👋"
else:
    ___t1 = False
str_to_char_tests = (1,
    "string_to_Char",
    (1, (0, ___t0, "s"), (1, (0, 1, "empty"), (1, (0, 1, "foo"), (1, (0, ___t1, "foo"), (0,))))))

def length_String(___bs0):
    def ___bloop0(___bs1, ___bacc0):
        ___a8 = ___bs1
        ___a10 = ___bacc0
        ___a5 = 1
        ___t3 = ___a5 == 1
        while ___t3:
            if ___a8 == "":
                ___a5 = 0
                ___a6 = ___a10
            else:
                ___t2 = 0
                ___t2 = ___t2 + 1
                ___a4 = ___a8[1:]
                ___btail0 = ___a4
                ___a7 = ___btail0
                ___a9 = ___a10 + 1
                ___a8 = ___a7
                ___a10 = ___a9
            ___t3 = ___a5 == 1
        return ___a6
    return ___bloop0(___bs0, 0)

___a11 = length_String("")
___a12 = length_String("x")
___a13 = length_String("hello")
___a14 = length_String("👋")
len_test = (1,
    "len tests",
    (1,
        (0, ___a11 == 0, "empty"),
        (1,
            (0, ___a12 == 1, "x"),
            (1, (0, ___a13 == 5, "hello"), (1, (0, ___a14 == 1, "👋"), (0,))))))

___a15 = (1, "x")
if ___a15[0] == 1:
    ___a16 = ___a15[1]
    ___t4 = ___a16 == "x"
else:
    ___t4 = False
___a17 = (1, "4")
if ___a17[0] == 1:
    ___a18 = ___a17[1]
    ___t5 = ___a18 == "4"
else:
    ___t5 = False
___a19 = (1, "👋")
if ___a19[0] == 1:
    ___a20 = ___a19[1]
    ___t6 = ___a20 == "👋"
else:
    ___t6 = False
last_tests = (1,
    "last_String",
    (1,
        (0, 1, "empty"),
        (1, (0, ___t4, "x"), (1, (0, ___t5, "1234"), (1, (0, ___t6, "👋"), (0,))))))

if "f":
    ___t7 = "foo".partition("f")
    ___a21 = (1, (___t7[0], ___t7[2])) if ___t7[1] else (0,)
else:
    ___a21 = (0,)
if ___a21[0] == 1:
    ___a22 = ___a21[1]
    ___a24 = ___a22[1]
    ___a23 = ___a22[0]
    ___t8 = (___a23 == "") and (___a24 == "oo")
else:
    ___t8 = False
if "":
    ___t9 = "foo".partition("")
    ___a25 = (1, (___t9[0], ___t9[2])) if ___t9[1] else (0,)
else:
    ___a25 = (0,)
if "x":
    ___t10 = "foo".partition("x")
    ___a26 = (1, (___t10[0], ___t10[2])) if ___t10[1] else (0,)
else:
    ___a26 = (0,)
if "o":
    ___t11 = "foo".rpartition("o")
    ___a27 = (1, (___t11[0], ___t11[2])) if ___t11[1] else (0,)
else:
    ___a27 = (0,)
if ___a27[0] == 1:
    ___a28 = ___a27[1]
    ___a30 = ___a28[1]
    ___a29 = ___a28[0]
    ___t12 = (___a29 == "fo") and (___a30 == "")
else:
    ___t12 = False
if "":
    ___t13 = "foo".rpartition("")
    ___a31 = (1, (___t13[0], ___t13[2])) if ___t13[1] else (0,)
else:
    ___a31 = (0,)
if "x":
    ___t14 = "foo".rpartition("x")
    ___a32 = (1, (___t14[0], ___t14[2])) if ___t14[1] else (0,)
else:
    ___a32 = (0,)
partition_tests = (1,
    "partition tests",
    (1,
        (0, ___t8, "foo partition_String f"),
        (1,
            (0, ___a25[0] == 0, "foo partition_String \"\""),
            (1,
                (0, ___a26[0] == 0, "foo partition_String x"),
                (1,
                    (0, ___t12, "foo rpartition_String o"),
                    (1,
                        (0, ___a31[0] == 0, "foo rpartition_String \"\""),
                        (1, (0, ___a32[0] == 0, "foo rpartition_String x"), (0,))))))))

match_tests = (1,
    "match tests",
    (1,
        (0, 1, "test matching 1"),
        (1,
            (0, 1, "test matching 2"),
            (1,
                (0, 1, "test matching 2.5"),
                (1,
                    (0, 1, "test matching 3"),
                    (1,
                        (0, 1, "test matching 4"),
                        (1, (0, 1, "test matching 5"), (1, (0, 1, "test matching 6"), (0,)))))))))

glob_match_tests = (1,
    "glob_match_suites",
    (1,
        (0, 1, "starts_with_foo(foobar)"),
        (1,
            (0, 1, "starts_with_foo(foobar)"),
            (1,
                (0, 1, "ends_with_foo(foobar)"),
                (1,
                    (0, 1, "ends_with_foo(foobar)"),
                    (1,
                        (0, 1, "contains_foo(foobar)"),
                        (1,
                            (0, 1, "contains_foo(barbar)"),
                            (1,
                                (0, 1, "there is foo and bar"),
                                (1,
                                    (0, 1, "there is foobar"),
                                    (1, (0, 1, "there is foo but not the other"), (0,)))))))))))

tests = (1,
    "Char tests",
    (1,
        str_to_char_tests,
        (1,
            len_test,
            (1, last_tests, (1, match_tests, (1, glob_match_tests, (1, partition_tests, (0,))))))))

class BosatsuTests(___iunittest0.TestCase):
    def test_all(self):
        def test_loop(value):
            if value[0] == 0:
                self.assertTrue(value[1], value[2])
            else:
                ___t15 = value[2]
                while ___t15[0] != 0:
                    test_loop(___t15[1])
                    ___t15 = ___t15[2]
        test_loop(tests)