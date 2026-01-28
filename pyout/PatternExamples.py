import unittest as ___iunittest0

___t0 = []
___t1 = (1, "foo: ", (1, "this is foo", (1, " bar: ", (1, "this is bar", (0,)))))
while ___t1[0] != 0:
    ___t0.append(___t1[1])
    ___t1 = ___t1[2]
combine = "".join(___t0)

___t2 = 0
if combine.startswith("foo: ", 0):
    ___t2 = ___t2 + 5
    ___t3 = combine[5:].partition(" bar: ")
    if ___t3[1] != "":
        ___a3 = ___t3[0]
        ___a4 = ___t3[2]
        ___bf0 = ___a3
        ___bb0 = ___a4
        ___a0 = 0 if ___bf0 < "this is foo" else 1 if ___bf0 == "this is foo" else 2
        ___a2 = ___a0 == 1
        if ___a2 == 1:
            ___a1 = 0 if ___bb0 < "this is bar" else 1 if ___bb0 == "this is bar" else 2
            fb = ___a1 == 1
        else:
            fb = 0
    else:
        fb = 0
else:
    fb = 0

test0 = (0, fb, "foo-bar match")

def get_foos(___bs0):
    ___t4 = 0
    ___t5 = 0
    ___t6 = False
    ___t7 = ___bs0.find("foo: (", ___t5)
    if ___t7 > -1:
        ___t8 = ___t7 + 6
        ___t9 = ___bs0[___t8:].partition(")")
        if ___t9[1] != "":
            ___a5 = ___t9[0]
            ___a6 = ___t9[2]
            ___t6 = True
            ___t5 = -1
        else:
            ___t5 = ___t7 + 1
    else:
        ___t5 = -1
    if ___t6:
        return (1, ___a5, get_foos(___a6))
    else:
        return (0,)

___a7 = get_foos("foo: (1) foo: (2)")
if ___a7[0] == 1:
    ___a8 = ___a7[1]
    ___a9 = ___a7[2]
    if ___a8 == "1":
        if ___a9[0] == 1:
            ___a10 = ___a9[1]
            ___a11 = ___a9[2]
            ___t13 = (___a10 == "2") and (___a11[0] == 0)
        else:
            ___t13 = False
    else:
        ___t13 = False
else:
    ___t13 = False
if ___t13:
    test1 = (0, 1, "get_foos")
else:
    if ___a7[0] == 1:
        ___a12 = ___a7[2]
        ___t12 = ___a12[0] == 0
    else:
        ___t12 = False
    if ___t12:
        ___bone0 = ___a7[1]
        ___t10 = []
        ___t11 = (1, "get_foos: ", (1, ___bone0, (0,)))
        while ___t11[0] != 0:
            ___t10.append(___t11[1])
            ___t11 = ___t11[2]
        test1 = (0, 0, "".join(___t10))
    else:
        test1 = (0, 0, "get_foos")

test2 = (0, 1, "test unnamed match")

tests = (1, "PatternExamples", (1, test0, (1, test1, (1, test2, (0,)))))

class BosatsuTests(___iunittest0.TestCase):
    def test_all(self):
        def test_loop(value):
            if value[0] == 0:
                self.assertTrue(value[1], value[2])
            else:
                ___t14 = value[2]
                while ___t14[0] != 0:
                    test_loop(___t14[1])
                    ___t14 = ___t14[2]
        test_loop(tests)