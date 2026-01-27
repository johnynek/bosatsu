import unittest as ___iunittest0

test = (0, 1, "trivial")

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