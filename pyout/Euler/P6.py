import unittest as ___iunittest0

def sum(___bfn0, ___bn0):
    def ___t0(___bi0, ___br0):
        ___bi1 = ___bi0 + -1
        return (___bi1, ___br0 + ___bfn0(___bi1))
    ___t1 = 0 < ___bn0
    ___t2 = 0
    ___t3 = ___bn0
    ___t4 = 0
    while ___t1:
        ___t2 = ___t0(___t3, ___t4)
        ___t5 = ___t2[0]
        ___t4 = ___t2[1]
        ___t1 = (0 < ___t5) and (___t5 < ___t3)
        ___t3 = ___t5
    return ___t4

def diff(___bn1):
    def ___t6(___bx0):
        ___bx10 = ___bx0 + 1
        return ___bx0 * ___bx10 * ___bx10
    return sum(___t6, ___bn1)

test0 = (0, diff(10) == 2640, "matched problem")

test1 = (0, diff(100) == 25164150, "matched problem")

tests = (1, "two examples", (1, test0, (1, test1, (0,))))

class BosatsuTests(___iunittest0.TestCase):
    def test_all(self):
        def test_loop(value):
            if value[0] == 0:
                self.assertTrue(value[1], value[2])
            else:
                ___t7 = value[2]
                while ___t7[0] != 0:
                    test_loop(___t7[1])
                    ___t7 = ___t7[2]
        test_loop(tests)