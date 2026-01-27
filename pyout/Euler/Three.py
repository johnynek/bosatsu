import unittest as ___iunittest0

def smallest_factor(___bn0):
    if ___bn0 == 1:
        return 1
    else:
        ___t1 = (___bn0,)
        def ___t0(___bi0, ___ba0):
            ___btrial0 = (___t1[0] - ___bi0) + 2
            return (0,
                ___btrial0) if ((___t1[0] % ___btrial0 if ___btrial0 else ___t1[0]) == 0) == 1 else (___bi0 + -1,
                -1)
        ___t2 = 0 < ___bn0
        ___t3 = -1
        ___t4 = ___bn0
        ___t5 = -1
        while ___t2:
            ___t3 = ___t0(___t4, ___t5)
            ___t6 = ___t3[0]
            ___t5 = ___t3[1]
            ___t2 = (0 < ___t6) and (___t6 < ___t4)
            ___t4 = ___t6
        return ___t5

def all_factors(___bn1):
    def ___t7(___bi1, ___bfacs0):
        ___bnext__factor0 = smallest_factor(___bi1)
        ___a1 = ___bi1 // ___bnext__factor0 if ___bnext__factor0 else 0
        if ___a1 == 1:
            return (0, (1, ___bnext__factor0, ___bfacs0))
        else:
            return (___a1, (1, ___bnext__factor0, ___bfacs0))
    ___t8 = 0 < ___bn1
    ___t9 = (0,)
    ___t10 = ___bn1
    ___t11 = (0,)
    while ___t8:
        ___t9 = ___t7(___t10, ___t11)
        ___t12 = ___t9[0]
        ___t11 = ___t9[1]
        ___t8 = (0 < ___t12) and (___t12 < ___t10)
        ___t10 = ___t12
    return ___t11

___a2 = all_factors(600851475143)
test = (0, (600851475143 if ___a2[0] == 0 else ___a2[1]) == 6857, "trial")

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