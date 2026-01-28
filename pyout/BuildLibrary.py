import Bosatsu.Predef as ___iPredef0

refl = lambda ___bx0: ___bx0

def map_Build(___bb0, ___bfn0):
    return (2, (___bb0, ___bfn0))

def map2_Build(___bba0, ___bbb0, ___bfn1):
    return (3, (___bba0, ___bbb0, ___bfn1))

def file(___bs0):
    return (0, ___bs0, refl)

empty = (1, (0,))

def build_all(___bitems0):
    if ___bitems0[0] == 0:
        return empty
    else:
        return (3,
            (___bitems0[1], build_all(___bitems0[2]), lambda ___t1, ___t2: (1, ___t1, ___t2)))

def files(___bfs0):
    return build_all(___iPredef0.map_List(___bfs0, lambda ___bf0: (0, ___bf0, refl)))

def library(___bsources0, ___bdeps0):
    return (4, ___bsources0, ___bdeps0, refl)

def build(___bargs0):
    return (4, ___bargs0[0], ___bargs0[1], refl)