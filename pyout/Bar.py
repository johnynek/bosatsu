import Foo as ___iFoo0
import unittest as ___iunittest1

___a0 = 0 if ___iFoo0.x < "this is Foo" else 1 if ___iFoo0.x == "this is Foo" else 2
test = (0, ___a0 == 1, "got the right string")

class BosatsuTests(___iunittest1.TestCase):
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