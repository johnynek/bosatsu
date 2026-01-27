import Bosatsu.Predef as ___iPredef0

def merge(___bleft0, ___bright0, ___bfn0):
    ___t1 = (___bfn0,)
    def ___t0(___bd0, ___ba0):
        ___bk0 = ___ba0[0]
        ___bv0 = ___ba0[1]
        ___a0 = ___iPredef0.get_key(___bd0, ___bk0)
        if ___a0[0] == 0:
            return ___iPredef0.add_key(___bd0, ___bk0, ___bv0)
        else:
            return ___iPredef0.add_key(___bd0, ___bk0, ___t1[0](___a0[1], ___bv0))
    return ___iPredef0.foldl_List(___iPredef0.items(___bright0), ___bleft0, ___t0)