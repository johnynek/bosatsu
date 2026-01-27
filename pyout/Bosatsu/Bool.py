import unittest as ___iunittest0

def ___nnot(___bb0):
    return ___bb0 != 1

test = (1, "not tests", (1, (0, 1, "not(True)"), (1, (0, 1, "not(False)"), (0,))))

class BosatsuTests(___iunittest0.TestCase):
    def test_all(self):
        def test_loop(value):
            if value[0] == 0:
                self.assertTrue(value[1], value[2])
            else:
                ___t0 = value[2]
                while ___t0[0] != 0:
                    test_loop(___t0[1])
                    ___t0 = ___t0[2]
        test_loop(test)