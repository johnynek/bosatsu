import unittest as ___iunittest0

def smallest_factor(___bn0):
    if ___bn0 == 1:
        return 1
    else:
        def ___t0(___bi0, ___ba0):
            ___btrial0 = (___bn0 - ___bi0) + 2
            return (0,
                ___btrial0) if ((___bn0 % ___btrial0 if ___btrial0 else ___bn0) == 0) == 1 else (___bi0 + -1,
                -1)
        ___t1 = 0 < ___bn0
        ___t2 = -1
        ___t3 = ___bn0
        ___t4 = -1
        while ___t1:
            ___t2 = ___t0(___t3, ___t4)
            ___t5 = ___t2[0]
            ___t4 = ___t2[1]
            ___t1 = (0 < ___t5) and (___t5 < ___t3)
            ___t3 = ___t5
        return ___t4

def all_factors(___bn1):
    def ___t6(___bi1, ___bfacs0):
        ___bnext__factor0 = smallest_factor(___bi1)
        ___a1 = ___bi1 // ___bnext__factor0 if ___bnext__factor0 else 0
        if ___a1 == 1:
            return (0, (1, ___bnext__factor0, ___bfacs0))
        else:
            return (___a1, (1, ___bnext__factor0, ___bfacs0))
    ___t7 = 0 < ___bn1
    ___t8 = (0,)
    ___t9 = ___bn1
    ___t10 = (0,)
    while ___t7:
        ___t8 = ___t6(___t9, ___t10)
        ___t11 = ___t8[0]
        ___t10 = ___t8[1]
        ___t7 = (0 < ___t11) and (___t11 < ___t9)
        ___t9 = ___t11
    return ___t10

___a2 = all_factors(600851475143)
test = (0, (600851475143 if ___a2[0] == 0 else ___a2[1]) == 6857, "trial")

class BosatsuTests(___iunittest0.TestCase):
    def test_all(self):
        def test_loop(value):
            if value[0] == 0:
                self.assertTrue(value[1], value[2])
            else:
                ___t12 = value[2]
                while ___t12[0] != 0:
                    test_loop(___t12[1])
                    ___t12 = ___t12[2]
        test_loop(test)