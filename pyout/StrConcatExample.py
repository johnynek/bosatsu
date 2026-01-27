import unittest as ___iunittest0

___t0 = []
___t1 = (1, "hello", (1, " ", (1, "atticus", (1, " and ", (1, "mahina", (0,))))))
while ___t1[0] != 0:
    ___t0.append(___t1[1])
    ___t1 = ___t1[2]
___a0 = "".join(___t0)
res0 = ___a0 == "hello atticus and mahina"

___t2 = []
___t3 = (1, "hello", (1, " ", (1, "atticus", (1, " and ", (1, "sarah", (0,))))))
while ___t3[0] != 0:
    ___t2.append(___t3[1])
    ___t3 = ___t3[2]
___a1 = "".join(___t2)
res1 = ___a1 == "hello atticus and sarah"

___t4 = []
___t5 = (1, "hello", (1, " ", (1, "atticus", (1, " and ", (1, "mahina", (0,))))))
while ___t5[0] != 0:
    ___t4.append(___t5[1])
    ___t5 = ___t5[2]
___a2 = "".join(___t4)
___t6 = 0
if ___a2.startswith("hello ", 0):
    ___t6 = ___t6 + 6
    ___a3 = ___a2[6:]
    ___brest0 = ___a3
    ___t7 = 0
    if ___brest0.startswith("atticus", 0):
        ___t7 = ___t7 + 7
        ___t8 = ___brest0.endswith("mahina")
    else:
        ___t8 = False
    res3 = ___t8
else:
    res3 = 0

___t9 = []
___t10 = (1, "hello", (1, " ", (1, "atticus", (1, " and ", (1, "mahina", (0,))))))
while ___t10[0] != 0:
    ___t9.append(___t10[1])
    ___t10 = ___t10[2]
___a7 = "".join(___t9)
___t11 = 0
___t12 = ___a7.partition("atticus")
if ___t12[1] != "":
    ___a8 = ___t12[0]
    ___a9 = ___t12[2]
    ___bleft0 = ___a8
    ___bright0 = ___a9
    ___a4 = (___bleft0, ___bright0)
    ___a6 = ___a4[1]
    ___a5 = ___a4[0]
    res4 = (___a5 == "hello ") and (___a6 == " and mahina")
else:
    res4 = 0

test = (1,
    "interpolation",
    (1,
        (0, res0, "res0"),
        (1,
            (0, res1, "res1"),
            (1, (0, 1, "res2"), (1, (0, res3, "res3"), (1, (0, res4, "res4"), (0,)))))))

class BosatsuTests(___iunittest0.TestCase):
    def test_all(self):
        def test_loop(value):
            if value[0] == 0:
                self.assertTrue(value[1], value[2])
            else:
                ___t13 = value[2]
                while ___t13[0] != 0:
                    test_loop(___t13[1])
                    ___t13 = ___t13[2]
        test_loop(test)