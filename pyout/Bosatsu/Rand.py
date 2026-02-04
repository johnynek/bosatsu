import Bosatsu.BinNat as ___iBinNat2
import Bosatsu.Nat as ___iNat0
import Bosatsu.Predef as ___iPredef1

def ___h5(___brands1, ___bs5, ___bacc0):
    ___a18 = ___brands1
    ___a20 = ___bs5
    ___a22 = ___bacc0
    ___a15 = 1
    ___t6 = ___a15 == 1
    while ___t6:
        if ___a18[0] == 0:
            ___a15 = 0
            ___a16 = (___a20, ___iPredef1.reverse(___a22))
        else:
            ___a14 = ___a18[1]
            ___bhfn0 = ___a14
            ___brt0 = ___a18[2]
            ___a13 = ___bhfn0(___a20)
            ___bs14 = ___a13[0]
            ___bh0 = ___a13[1]
            ___a17 = ___brt0
            ___a19 = ___bs14
            ___a21 = (1, ___bh0, ___a22)
            ___a18 = ___a17
            ___a20 = ___a19
            ___a22 = ___a21
        ___t6 = ___a15 == 1
    return ___a16

def ___h33(___blist1, ___bacc3):
    ___a64 = ___blist1
    ___a66 = ___bacc3
    ___a61 = 1
    ___t34 = ___a61 == 1
    while ___t34:
        if ___a64[0] == 0:
            ___a61 = 0
            ___a62 = ___a66
        else:
            ___bt2 = ___a64[2]
            ___a63 = ___bt2
            ___a65 = ___iBinNat2.next(___a66)
            ___a64 = ___a63
            ___a66 = ___a65
        ___t34 = ___a61 == 1
    return ___a62

bitmask_64 = 18446744073709551615

def rotl(___bx0, ___bk0):
    return ((___bx0 << ___bk0) & bitmask_64) | ((___bx0 >> (64 - ___bk0)) & bitmask_64)

def next(___bstate0):
    ___bs00 = ___bstate0[0]
    ___bs10 = ___bstate0[1]
    ___bs20 = ___bstate0[2]
    ___bs30 = ___bstate0[3]
    ___bs21 = ___bs20 ^ ___bs00
    ___bs31 = ___bs30 ^ ___bs10
    return ((___bs00 ^ ___bs31,
            ___bs10 ^ ___bs21,
            ___bs21 ^ ((___bs10 << 17) & bitmask_64),
            rotl(___bs31, 45) & bitmask_64),
        (rotl((___bs10 + ___bs10 + ___bs10 + ___bs10 + ___bs10) & bitmask_64,
            7) + rotl((___bs10 + ___bs10 + ___bs10 + ___bs10 + ___bs10) & bitmask_64,
            7) + rotl((___bs10 + ___bs10 + ___bs10 + ___bs10 + ___bs10) & bitmask_64,
            7) + rotl((___bs10 + ___bs10 + ___bs10 + ___bs10 + ___bs10) & bitmask_64,
            7) + rotl((___bs10 + ___bs10 + ___bs10 + ___bs10 + ___bs10) & bitmask_64,
            7) + rotl((___bs10 + ___bs10 + ___bs10 + ___bs10 + ___bs10) & bitmask_64,
            7) + rotl((___bs10 + ___bs10 + ___bs10 + ___bs10 + ___bs10) & bitmask_64,
            7) + rotl((___bs10 + ___bs10 + ___bs10 + ___bs10 + ___bs10) & bitmask_64,
            7) + rotl((___bs10 + ___bs10 + ___bs10 + ___bs10 + ___bs10) & bitmask_64,
            7)) & bitmask_64)

def state_from_Int(___bi0):
    ___a0 = ___bi0 ^ 54564800212389664567110541424720236321503446907971127334425124977866121780221
    ___bnot__zero0 = 54564800212389664567110541424720236321503446907971127334425124977866121780221 if ___a0 == 0 else ___a0
    return (___bnot__zero0 & bitmask_64,
        (___bnot__zero0 >> 64) & bitmask_64,
        (___bnot__zero0 >> 128) & bitmask_64,
        (___bnot__zero0 >> 192) & bitmask_64)

def map_Rand(___br0, ___bfn0):
    ___bfna0 = ___br0
    def ___t0(___bs0):
        ___a1 = ___bfna0(___bs0)
        return (___a1[0], ___bfn0(___a1[1]))
    return ___t0

def flat_map_Rand(___br1, ___bfn1):
    ___bfna1 = ___br1
    def ___t1(___bs1):
        ___a3 = ___bfna1(___bs1)
        return ___bfn1(___a3[1])(___a3[0])
    return ___t1

def prod_Rand(___bra0, ___brb0):
    ___bfna2 = ___bra0
    ___bfnb1 = ___brb0
    def ___t2(___bs01):
        ___a5 = ___bfna2(___bs01)
        ___bs13 = ___a5[0]
        ___ba2 = ___a5[1]
        ___a4 = ___bfnb1(___bs13)
        return (___a4[0], (___ba2, ___a4[1]))
    return ___t2

def const_Rand(___ba3):
    return lambda ___bs2: (___bs2, ___ba3)

nat_2 = 2

def parity(___bn0, ___bi1):
    if ___bn0 == 0:
        return (___bi1 & 1) == 1
    else:
        ___a9 = ___bn0 - 1
        ___bp0 = ___a9
        ___a8 = parity(___bp0, ___bi1 >> ___iNat0.to_Int(___iNat0.exp(nat_2, ___bn0)))
        if ___a8 == 1:
            return parity(___bp0, ___bi1) != 1
        else:
            return parity(___bp0, ___bi1)

six = 6

def ___t4(___bs3):
    ___a10 = next(___bs3)
    return (___a10[0], parity(six, ___a10[1]))
bool_Rand = ___t4

def run_Rand(___brand0, ___bseed0):
    return ___brand0(state_from_Int(___bseed0))[1]

def sequence_Rand(___brands0):
    return lambda ___bs6: ___h5(___brands0, ___bs6, (0,))

uint64_Rand = next

def bit_count(___bi3):
    ___t9 = 0 < ___bi3
    ___t10 = 0
    ___t11 = ___bi3
    ___t12 = 0
    while ___t9:
        ___t13 = ___t11 >> 1
        ___t12 = ___t12 + 1
        ___t9 = (0 < ___t13) and (___t13 < ___t11)
        ___t11 = ___t13
    return ___t12

def to_big_Int(___bus0, ___bacc1):
    ___a27 = ___bus0
    ___a29 = ___bacc1
    ___a24 = 1
    ___t14 = ___a24 == 1
    while ___t14:
        if ___a27[0] == 0:
            ___a24 = 0
            ___a25 = ___a29
        else:
            ___a23 = ___a27[1]
            ___bh1 = ___a23
            ___bt0 = ___a27[2]
            ___a26 = ___bt0
            ___a28 = (___a29 << 64) | ___bh1
            ___a27 = ___a26
            ___a29 = ___a28
        ___t14 = ___a24 == 1
    return ___a25

nat30 = ___iNat0.to_Nat(30)

def resample(___brand__Int0, ___bhigh0, ___buints0):
    ___bfn3 = ___brand__Int0
    ___bboundary0 = ((1 << (___buints0 * 64)) // ___bhigh0 if ___bhigh0 else 0) * ___bhigh0
    def ___bnext0(___bs7, ___bfuel0):
        ___a36 = ___bs7
        ___a38 = ___bfuel0
        ___a33 = 1
        ___t15 = ___a33 == 1
        while ___t15:
            if ___a38 == 0:
                ___a33 = 0
                ___a34 = (___a36, (___bhigh0 + -1) >> 1)
            else:
                ___a32 = ___a38 - 1
                ___bn1 = ___a32
                ___a31 = ___bfn3(___a36)
                ___bs15 = ___a31[0]
                ___bi5 = ___a31[1]
                ___a30 = 0 if ___bi5 < ___bboundary0 else 1 if ___bi5 == ___bboundary0 else 2
                if ___a30 == 0:
                    ___a33 = 0
                    ___a34 = (___bs15, ___bi5 % ___bhigh0 if ___bhigh0 else ___bi5)
                else:
                    ___a35 = ___bs15
                    ___a37 = ___bn1
                    ___a36 = ___a35
                    ___a38 = ___a37
            ___t15 = ___a33 == 1
        return ___a34
    return lambda ___bs8: ___bnext0(___bs8, nat30)

const0 = lambda ___bs9: (___bs9, 0)

def int_range(___bhigh1):
    ___a40 = 0 if ___bhigh1 < 1 else 1 if ___bhigh1 == 1 else 2
    if ___a40 == 2:
        ___bbits1 = bit_count(___bhigh1)
        ___buint__count0 = (___bbits1 >> 6) + 1
        ___br2 = sequence_Rand(___iPredef1.replicate_List(uint64_Rand, ___buint__count0))
        ___bfna3 = ___br2
        def ___t18(___bs10):
            ___a39 = ___bfna3(___bs10)
            return (___a39[0], to_big_Int(___a39[1], 0))
        return resample(___t18, ___bhigh1, ___buint__count0)
    else:
        return const0

def nat_range(___bhigh2):
    ___br3 = int_range(___iNat0.to_Int(___bhigh2))
    ___bfna4 = ___br3
    def ___t19(___bs11):
        ___a41 = ___bfna4(___bs11)
        return (___a41[0], ___iNat0.to_Nat(___a41[1]))
    return ___t19

def geometric(___bdepth0, ___bacc2):
    if ___bdepth0 == 0:
        return lambda ___bs12: (___bs12, ___bacc2)
    else:
        ___a46 = ___bdepth0 - 1
        ___bprev0 = ___a46
        def ___t21(___bs13):
            ___a42 = next(___bs13)
            ___a43 = ___a42[1]
            ___bs18 = ___a42[0]
            ___bi6 = ___a43
            ___a45 = (___bs18, parity(six, ___bi6))
            ___bs19 = ___a45[0]
            ___ba6 = ___a45[1]
            if ___ba6 == 1:
                ___t23 = (___bacc2,)
                def ___t22(___bs14):
                    return (___bs14, ___t23[0])
                ___a44 = ___t22
            else:
                ___a44 = geometric(___bprev0, ___bacc2 + 1)
            return ___a44(___bs19)
        return ___t21

geometric_Int = geometric(nat30, 0)

def split_at(___blist0, ___bidx0):
    if ___blist0[0] == 0:
        return ((0,), (0,))
    else:
        ___bh2 = ___blist0[1]
        ___bt1 = ___blist0[2]
        if ___bidx0[0] == 0:
            return ((0,), ___blist0)
        else:
            ___a47 = split_at(___bt1, ___iBinNat2.prev(___bidx0))
            return ((1, ___bh2, ___a47[0]), ___a47[1])

def from_pair(___bleft1, ___bright1):
    def ___t24(___bs15):
        ___a48 = next(___bs15)
        ___a49 = ___a48[1]
        ___bs110 = ___a48[0]
        ___bi7 = ___a49
        ___a51 = (___bs110, parity(six, ___bi7))
        return (___bleft1 if ___a51[1] == 1 else ___bright1)(___a51[0])
    return ___t24

def one_of(___bhead0, ___btail0):
    def ___bloop0(___bitems__len0, ___bitems0):
        if ___bitems__len0[0] == 0:
            return ___bhead0
        else:
            if ___bitems__len0[0] == 1:
                ___a59 = ___bitems__len0[1]
                ___t32 = ___a59[0] == 0
            else:
                ___t32 = False
            if ___t32:
                return ___bitems0[1] if ___bitems0[0] == 1 else ___bhead0
            elif ___bitems__len0[0] == 1:
                ___bn2 = ___bitems__len0[1]
                if ___bitems0[0] == 1:
                    ___bfront0 = ___bitems0[1]
                    ___btail1 = ___bitems0[2]
                    ___a54 = split_at(___btail1, ___bn2)
                    ___bleft2 = ___a54[0]
                    ___bright2 = ___a54[1]
                    ___blrand0 = ___bloop0(___bn2, ___bleft2)
                    ___brrand0 = ___bloop0(___bn2, ___bright2)
                    ___bback0 = from_pair(___blrand0, ___brrand0)
                    ___br4 = int_range(___iBinNat2.toInt(___bitems__len0))
                    ___bfna5 = ___br4
                    ___t26 = (___bfna5, ___bback0, ___bfront0)
                    def ___t25(___bs16):
                        ___a53 = ___t26[0](___bs16)
                        return (___t26[2] if ___a53[1] == 0 else ___t26[1])(___a53[0])
                    return ___t25
                else:
                    return ___bhead0
            else:
                if ___bitems__len0[0] == 2:
                    ___a60 = ___bitems__len0[1]
                    ___t31 = ___a60[0] == 0
                else:
                    ___t31 = False
                if ___t31:
                    if ___bitems0[0] == 1:
                        ___a55 = ___bitems0[2]
                        ___t27 = ___a55[0] == 1
                    else:
                        ___t27 = False
                    if ___t27:
                        return from_pair(___bitems0[1], ___a55[1])
                    else:
                        return ___bhead0
                else:
                    ___bn3 = ___bitems__len0[1]
                    if ___bitems0[0] == 1:
                        ___a58 = ___bitems0[2]
                        ___t30 = ___a58[0] == 1
                    else:
                        ___t30 = False
                    if ___t30:
                        ___bf10 = ___bitems0[1]
                        ___bf20 = ___a58[1]
                        ___btail2 = ___a58[2]
                        ___bfront1 = from_pair(___bf10, ___bf20)
                        ___bback1 = ___bloop0(___bn3, ___btail2)
                        ___br5 = int_range(___iBinNat2.toInt(___bitems__len0) >> 1)
                        ___bfna6 = ___br5
                        ___t29 = (___bfna6, ___bback1, ___bfront1)
                        def ___t28(___bs17):
                            ___a57 = ___t29[0](___bs17)
                            return (___t29[2] if ___a57[1] == 0 else ___t29[1])(___a57[0])
                        return ___t28
                    else:
                        return ___bhead0
    ___bitems1 = (1, ___bhead0, ___btail0)
    return ___bloop0(___h33(___bitems1, (0,)), ___bitems1)