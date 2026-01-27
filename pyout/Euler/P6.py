import unittest as ___iunittest0

def sum(___bfn0, ___bn0):
    ___t1 = (___bfn0,)
    def ___t0(___bi0, ___br0):
        ___bi1 = ___bi0 + -1
        return (___bi1, ___br0 + ___t1[0](___bi1))
    ___t2 = 0 < ___bn0
    ___t3 = 0
    ___t4 = ___bn0
    ___t5 = 0
    while ___t2:
        ___t3 = ___t0(___t4, ___t5)
        ___t6 = ___t3[0]
        ___t5 = ___t3[1]
        ___t2 = (0 < ___t6) and (___t6 < ___t4)
        ___t4 = ___t6
    return ___t5

def diff(___bn1):
    def ___t7(___bx0):
        ___bx10 = ___bx0 + 1
        return ___bx0 * ___bx10 * ___bx10
    return sum(___t7, ___bn1)

test0 = (0, diff(10) == 2640, "matched problem")

test1 = (0, diff(100) == 25164150, "matched problem")

tests = (1, "two examples", (1, test0, (1, test1, (0,))))

class BosatsuTests(___iunittest0.TestCase):
    def test_all(self):
        def test_loop(value):
            if value[0] == 0:
                self.assertTrue(value[1], value[2])
            else:
                ___t8 = value[2]
                while ___t8[0] != 0:
                    test_loop(___t8[1])
                    ___t8 = ___t8[2]
        test_loop(tests)