

refl = lambda ___bx0: ___bx0

refl_sub = lambda ___bx1: ___bx1

refl_sup = lambda ___bx2: ___bx2

def substitute(___beq0, ___bfa0):
    return ___beq0(___bfa0)

def widen(___bs0, ___bfa1):
    return ___bs0(___bfa1)

def narrow(___bs1, ___bfa2):
    return ___bs1(___bfa2)

def sub_to_sup(___bsub1):
    return ___bsub1(refl_sup)

def sup_to_sub(___bsup1):
    return ___bsup1(refl_sub)

def eq_to_sub(___beq1):
    return ___beq1(refl_sub)

def eq_to_sup(___beq2):
    return ___beq2(refl_sup)

def cast(___bs2, ___ba0):
    return ___bs2(___ba0)

def upcast(___bs3, ___ba1):
    return ___bs3(___ba1)

def downcast(___bs4, ___ba2):
    return ___bs4(refl_sub)(___ba2)

def compose_sub(___bfirst0, ___bsecond0):
    return ___bsecond0(___bfirst0)

def compose_sup(___bfirst1, ___bsecond1):
    return ___bsecond1(___bfirst1)

def flip_eq(___beq3):
    return ___beq3(refl)

def compose_eq(___bfirst2, ___bsecond2):
    return ___bsecond2(___bfirst2)

ignore = (refl_sub, refl_sub, refl_sub, refl_sub, refl_sub, refl_sub)