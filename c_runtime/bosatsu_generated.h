// STRUCTS
DEFINE_BSTS_OBJ(Struct2,BValue _0;BValue _1;);

BValue alloc_struct2(BValue b0, BValue b1) {
    Struct2* rc = GC_malloc(sizeof(Struct2));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_struct2");
        abort();
    }
    rc->_0 = b0;
    rc->_1 = b1;
    return BSTS_VALUE_FROM_PTR(rc);
}

DEFINE_BSTS_OBJ(Struct3,BValue _0;BValue _1;BValue _2;);

BValue alloc_struct3(BValue b0, BValue b1, BValue b2) {
    Struct3* rc = GC_malloc(sizeof(Struct3));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_struct3");
        abort();
    }
    rc->_0 = b0;
    rc->_1 = b1;
    rc->_2 = b2;
    return BSTS_VALUE_FROM_PTR(rc);
}

DEFINE_BSTS_OBJ(Struct4,BValue _0;BValue _1;BValue _2;BValue _3;);

BValue alloc_struct4(BValue b0, BValue b1, BValue b2, BValue b3) {
    Struct4* rc = GC_malloc(sizeof(Struct4));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_struct4");
        abort();
    }
    rc->_0 = b0;
    rc->_1 = b1;
    rc->_2 = b2;
    rc->_3 = b3;
    return BSTS_VALUE_FROM_PTR(rc);
}

DEFINE_BSTS_OBJ(Struct5,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;);

BValue alloc_struct5(BValue b0, BValue b1, BValue b2, BValue b3, BValue b4) {
    Struct5* rc = GC_malloc(sizeof(Struct5));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_struct5");
        abort();
    }
    rc->_0 = b0;
    rc->_1 = b1;
    rc->_2 = b2;
    rc->_3 = b3;
    rc->_4 = b4;
    return BSTS_VALUE_FROM_PTR(rc);
}

DEFINE_BSTS_OBJ(Struct6,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;);

BValue alloc_struct6(BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5) {
    Struct6* rc = GC_malloc(sizeof(Struct6));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_struct6");
        abort();
    }
    rc->_0 = b0;
    rc->_1 = b1;
    rc->_2 = b2;
    rc->_3 = b3;
    rc->_4 = b4;
    rc->_5 = b5;
    return BSTS_VALUE_FROM_PTR(rc);
}

DEFINE_BSTS_OBJ(Struct7,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;BValue _6;);

BValue alloc_struct7(BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5, BValue b6) {
    Struct7* rc = GC_malloc(sizeof(Struct7));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_struct7");
        abort();
    }
    rc->_0 = b0;
    rc->_1 = b1;
    rc->_2 = b2;
    rc->_3 = b3;
    rc->_4 = b4;
    rc->_5 = b5;
    rc->_6 = b6;
    return BSTS_VALUE_FROM_PTR(rc);
}

DEFINE_BSTS_OBJ(Struct8,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;BValue _6;BValue _7;);

BValue alloc_struct8(BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5, BValue b6, BValue b7) {
    Struct8* rc = GC_malloc(sizeof(Struct8));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_struct8");
        abort();
    }
    rc->_0 = b0;
    rc->_1 = b1;
    rc->_2 = b2;
    rc->_3 = b3;
    rc->_4 = b4;
    rc->_5 = b5;
    rc->_6 = b6;
    rc->_7 = b7;
    return BSTS_VALUE_FROM_PTR(rc);
}

DEFINE_BSTS_OBJ(Struct9,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;BValue _6;BValue _7;BValue _8;);

BValue alloc_struct9(BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5, BValue b6, BValue b7, BValue b8) {
    Struct9* rc = GC_malloc(sizeof(Struct9));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_struct9");
        abort();
    }
    rc->_0 = b0;
    rc->_1 = b1;
    rc->_2 = b2;
    rc->_3 = b3;
    rc->_4 = b4;
    rc->_5 = b5;
    rc->_6 = b6;
    rc->_7 = b7;
    rc->_8 = b8;
    return BSTS_VALUE_FROM_PTR(rc);
}

DEFINE_BSTS_OBJ(Struct10,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;BValue _6;BValue _7;BValue _8;BValue _9;);

BValue alloc_struct10(BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5, BValue b6, BValue b7, BValue b8, BValue b9) {
    Struct10* rc = GC_malloc(sizeof(Struct10));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_struct10");
        abort();
    }
    rc->_0 = b0;
    rc->_1 = b1;
    rc->_2 = b2;
    rc->_3 = b3;
    rc->_4 = b4;
    rc->_5 = b5;
    rc->_6 = b6;
    rc->_7 = b7;
    rc->_8 = b8;
    rc->_9 = b9;
    return BSTS_VALUE_FROM_PTR(rc);
}

DEFINE_BSTS_OBJ(Struct11,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;BValue _6;BValue _7;BValue _8;BValue _9;BValue _10;);

BValue alloc_struct11(BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5, BValue b6, BValue b7, BValue b8, BValue b9, BValue b10) {
    Struct11* rc = GC_malloc(sizeof(Struct11));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_struct11");
        abort();
    }
    rc->_0 = b0;
    rc->_1 = b1;
    rc->_2 = b2;
    rc->_3 = b3;
    rc->_4 = b4;
    rc->_5 = b5;
    rc->_6 = b6;
    rc->_7 = b7;
    rc->_8 = b8;
    rc->_9 = b9;
    rc->_10 = b10;
    return BSTS_VALUE_FROM_PTR(rc);
}

DEFINE_BSTS_OBJ(Struct12,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;BValue _6;BValue _7;BValue _8;BValue _9;BValue _10;BValue _11;);

BValue alloc_struct12(BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5, BValue b6, BValue b7, BValue b8, BValue b9, BValue b10, BValue b11) {
    Struct12* rc = GC_malloc(sizeof(Struct12));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_struct12");
        abort();
    }
    rc->_0 = b0;
    rc->_1 = b1;
    rc->_2 = b2;
    rc->_3 = b3;
    rc->_4 = b4;
    rc->_5 = b5;
    rc->_6 = b6;
    rc->_7 = b7;
    rc->_8 = b8;
    rc->_9 = b9;
    rc->_10 = b10;
    rc->_11 = b11;
    return BSTS_VALUE_FROM_PTR(rc);
}

DEFINE_BSTS_OBJ(Struct13,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;BValue _6;BValue _7;BValue _8;BValue _9;BValue _10;BValue _11;BValue _12;);

BValue alloc_struct13(BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5, BValue b6, BValue b7, BValue b8, BValue b9, BValue b10, BValue b11, BValue b12) {
    Struct13* rc = GC_malloc(sizeof(Struct13));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_struct13");
        abort();
    }
    rc->_0 = b0;
    rc->_1 = b1;
    rc->_2 = b2;
    rc->_3 = b3;
    rc->_4 = b4;
    rc->_5 = b5;
    rc->_6 = b6;
    rc->_7 = b7;
    rc->_8 = b8;
    rc->_9 = b9;
    rc->_10 = b10;
    rc->_11 = b11;
    rc->_12 = b12;
    return BSTS_VALUE_FROM_PTR(rc);
}

DEFINE_BSTS_OBJ(Struct14,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;BValue _6;BValue _7;BValue _8;BValue _9;BValue _10;BValue _11;BValue _12;BValue _13;);

BValue alloc_struct14(BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5, BValue b6, BValue b7, BValue b8, BValue b9, BValue b10, BValue b11, BValue b12, BValue b13) {
    Struct14* rc = GC_malloc(sizeof(Struct14));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_struct14");
        abort();
    }
    rc->_0 = b0;
    rc->_1 = b1;
    rc->_2 = b2;
    rc->_3 = b3;
    rc->_4 = b4;
    rc->_5 = b5;
    rc->_6 = b6;
    rc->_7 = b7;
    rc->_8 = b8;
    rc->_9 = b9;
    rc->_10 = b10;
    rc->_11 = b11;
    rc->_12 = b12;
    rc->_13 = b13;
    return BSTS_VALUE_FROM_PTR(rc);
}

DEFINE_BSTS_OBJ(Struct15,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;BValue _6;BValue _7;BValue _8;BValue _9;BValue _10;BValue _11;BValue _12;BValue _13;BValue _14;);

BValue alloc_struct15(BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5, BValue b6, BValue b7, BValue b8, BValue b9, BValue b10, BValue b11, BValue b12, BValue b13, BValue b14) {
    Struct15* rc = GC_malloc(sizeof(Struct15));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_struct15");
        abort();
    }
    rc->_0 = b0;
    rc->_1 = b1;
    rc->_2 = b2;
    rc->_3 = b3;
    rc->_4 = b4;
    rc->_5 = b5;
    rc->_6 = b6;
    rc->_7 = b7;
    rc->_8 = b8;
    rc->_9 = b9;
    rc->_10 = b10;
    rc->_11 = b11;
    rc->_12 = b12;
    rc->_13 = b13;
    rc->_14 = b14;
    return BSTS_VALUE_FROM_PTR(rc);
}

DEFINE_BSTS_OBJ(Struct16,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;BValue _6;BValue _7;BValue _8;BValue _9;BValue _10;BValue _11;BValue _12;BValue _13;BValue _14;BValue _15;);

BValue alloc_struct16(BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5, BValue b6, BValue b7, BValue b8, BValue b9, BValue b10, BValue b11, BValue b12, BValue b13, BValue b14, BValue b15) {
    Struct16* rc = GC_malloc(sizeof(Struct16));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_struct16");
        abort();
    }
    rc->_0 = b0;
    rc->_1 = b1;
    rc->_2 = b2;
    rc->_3 = b3;
    rc->_4 = b4;
    rc->_5 = b5;
    rc->_6 = b6;
    rc->_7 = b7;
    rc->_8 = b8;
    rc->_9 = b9;
    rc->_10 = b10;
    rc->_11 = b11;
    rc->_12 = b12;
    rc->_13 = b13;
    rc->_14 = b14;
    rc->_15 = b15;
    return BSTS_VALUE_FROM_PTR(rc);
}

DEFINE_BSTS_OBJ(Struct17,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;BValue _6;BValue _7;BValue _8;BValue _9;BValue _10;BValue _11;BValue _12;BValue _13;BValue _14;BValue _15;BValue _16;);

BValue alloc_struct17(BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5, BValue b6, BValue b7, BValue b8, BValue b9, BValue b10, BValue b11, BValue b12, BValue b13, BValue b14, BValue b15, BValue b16) {
    Struct17* rc = GC_malloc(sizeof(Struct17));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_struct17");
        abort();
    }
    rc->_0 = b0;
    rc->_1 = b1;
    rc->_2 = b2;
    rc->_3 = b3;
    rc->_4 = b4;
    rc->_5 = b5;
    rc->_6 = b6;
    rc->_7 = b7;
    rc->_8 = b8;
    rc->_9 = b9;
    rc->_10 = b10;
    rc->_11 = b11;
    rc->_12 = b12;
    rc->_13 = b13;
    rc->_14 = b14;
    rc->_15 = b15;
    rc->_16 = b16;
    return BSTS_VALUE_FROM_PTR(rc);
}

DEFINE_BSTS_OBJ(Struct18,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;BValue _6;BValue _7;BValue _8;BValue _9;BValue _10;BValue _11;BValue _12;BValue _13;BValue _14;BValue _15;BValue _16;BValue _17;);

BValue alloc_struct18(BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5, BValue b6, BValue b7, BValue b8, BValue b9, BValue b10, BValue b11, BValue b12, BValue b13, BValue b14, BValue b15, BValue b16, BValue b17) {
    Struct18* rc = GC_malloc(sizeof(Struct18));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_struct18");
        abort();
    }
    rc->_0 = b0;
    rc->_1 = b1;
    rc->_2 = b2;
    rc->_3 = b3;
    rc->_4 = b4;
    rc->_5 = b5;
    rc->_6 = b6;
    rc->_7 = b7;
    rc->_8 = b8;
    rc->_9 = b9;
    rc->_10 = b10;
    rc->_11 = b11;
    rc->_12 = b12;
    rc->_13 = b13;
    rc->_14 = b14;
    rc->_15 = b15;
    rc->_16 = b16;
    rc->_17 = b17;
    return BSTS_VALUE_FROM_PTR(rc);
}

DEFINE_BSTS_OBJ(Struct19,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;BValue _6;BValue _7;BValue _8;BValue _9;BValue _10;BValue _11;BValue _12;BValue _13;BValue _14;BValue _15;BValue _16;BValue _17;BValue _18;);

BValue alloc_struct19(BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5, BValue b6, BValue b7, BValue b8, BValue b9, BValue b10, BValue b11, BValue b12, BValue b13, BValue b14, BValue b15, BValue b16, BValue b17, BValue b18) {
    Struct19* rc = GC_malloc(sizeof(Struct19));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_struct19");
        abort();
    }
    rc->_0 = b0;
    rc->_1 = b1;
    rc->_2 = b2;
    rc->_3 = b3;
    rc->_4 = b4;
    rc->_5 = b5;
    rc->_6 = b6;
    rc->_7 = b7;
    rc->_8 = b8;
    rc->_9 = b9;
    rc->_10 = b10;
    rc->_11 = b11;
    rc->_12 = b12;
    rc->_13 = b13;
    rc->_14 = b14;
    rc->_15 = b15;
    rc->_16 = b16;
    rc->_17 = b17;
    rc->_18 = b18;
    return BSTS_VALUE_FROM_PTR(rc);
}

DEFINE_BSTS_OBJ(Struct20,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;BValue _6;BValue _7;BValue _8;BValue _9;BValue _10;BValue _11;BValue _12;BValue _13;BValue _14;BValue _15;BValue _16;BValue _17;BValue _18;BValue _19;);

BValue alloc_struct20(BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5, BValue b6, BValue b7, BValue b8, BValue b9, BValue b10, BValue b11, BValue b12, BValue b13, BValue b14, BValue b15, BValue b16, BValue b17, BValue b18, BValue b19) {
    Struct20* rc = GC_malloc(sizeof(Struct20));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_struct20");
        abort();
    }
    rc->_0 = b0;
    rc->_1 = b1;
    rc->_2 = b2;
    rc->_3 = b3;
    rc->_4 = b4;
    rc->_5 = b5;
    rc->_6 = b6;
    rc->_7 = b7;
    rc->_8 = b8;
    rc->_9 = b9;
    rc->_10 = b10;
    rc->_11 = b11;
    rc->_12 = b12;
    rc->_13 = b13;
    rc->_14 = b14;
    rc->_15 = b15;
    rc->_16 = b16;
    rc->_17 = b17;
    rc->_18 = b18;
    rc->_19 = b19;
    return BSTS_VALUE_FROM_PTR(rc);
}

DEFINE_BSTS_OBJ(Struct21,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;BValue _6;BValue _7;BValue _8;BValue _9;BValue _10;BValue _11;BValue _12;BValue _13;BValue _14;BValue _15;BValue _16;BValue _17;BValue _18;BValue _19;BValue _20;);

BValue alloc_struct21(BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5, BValue b6, BValue b7, BValue b8, BValue b9, BValue b10, BValue b11, BValue b12, BValue b13, BValue b14, BValue b15, BValue b16, BValue b17, BValue b18, BValue b19, BValue b20) {
    Struct21* rc = GC_malloc(sizeof(Struct21));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_struct21");
        abort();
    }
    rc->_0 = b0;
    rc->_1 = b1;
    rc->_2 = b2;
    rc->_3 = b3;
    rc->_4 = b4;
    rc->_5 = b5;
    rc->_6 = b6;
    rc->_7 = b7;
    rc->_8 = b8;
    rc->_9 = b9;
    rc->_10 = b10;
    rc->_11 = b11;
    rc->_12 = b12;
    rc->_13 = b13;
    rc->_14 = b14;
    rc->_15 = b15;
    rc->_16 = b16;
    rc->_17 = b17;
    rc->_18 = b18;
    rc->_19 = b19;
    rc->_20 = b20;
    return BSTS_VALUE_FROM_PTR(rc);
}

DEFINE_BSTS_OBJ(Struct22,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;BValue _6;BValue _7;BValue _8;BValue _9;BValue _10;BValue _11;BValue _12;BValue _13;BValue _14;BValue _15;BValue _16;BValue _17;BValue _18;BValue _19;BValue _20;BValue _21;);

BValue alloc_struct22(BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5, BValue b6, BValue b7, BValue b8, BValue b9, BValue b10, BValue b11, BValue b12, BValue b13, BValue b14, BValue b15, BValue b16, BValue b17, BValue b18, BValue b19, BValue b20, BValue b21) {
    Struct22* rc = GC_malloc(sizeof(Struct22));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_struct22");
        abort();
    }
    rc->_0 = b0;
    rc->_1 = b1;
    rc->_2 = b2;
    rc->_3 = b3;
    rc->_4 = b4;
    rc->_5 = b5;
    rc->_6 = b6;
    rc->_7 = b7;
    rc->_8 = b8;
    rc->_9 = b9;
    rc->_10 = b10;
    rc->_11 = b11;
    rc->_12 = b12;
    rc->_13 = b13;
    rc->_14 = b14;
    rc->_15 = b15;
    rc->_16 = b16;
    rc->_17 = b17;
    rc->_18 = b18;
    rc->_19 = b19;
    rc->_20 = b20;
    rc->_21 = b21;
    return BSTS_VALUE_FROM_PTR(rc);
}

DEFINE_BSTS_OBJ(Struct23,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;BValue _6;BValue _7;BValue _8;BValue _9;BValue _10;BValue _11;BValue _12;BValue _13;BValue _14;BValue _15;BValue _16;BValue _17;BValue _18;BValue _19;BValue _20;BValue _21;BValue _22;);

BValue alloc_struct23(BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5, BValue b6, BValue b7, BValue b8, BValue b9, BValue b10, BValue b11, BValue b12, BValue b13, BValue b14, BValue b15, BValue b16, BValue b17, BValue b18, BValue b19, BValue b20, BValue b21, BValue b22) {
    Struct23* rc = GC_malloc(sizeof(Struct23));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_struct23");
        abort();
    }
    rc->_0 = b0;
    rc->_1 = b1;
    rc->_2 = b2;
    rc->_3 = b3;
    rc->_4 = b4;
    rc->_5 = b5;
    rc->_6 = b6;
    rc->_7 = b7;
    rc->_8 = b8;
    rc->_9 = b9;
    rc->_10 = b10;
    rc->_11 = b11;
    rc->_12 = b12;
    rc->_13 = b13;
    rc->_14 = b14;
    rc->_15 = b15;
    rc->_16 = b16;
    rc->_17 = b17;
    rc->_18 = b18;
    rc->_19 = b19;
    rc->_20 = b20;
    rc->_21 = b21;
    rc->_22 = b22;
    return BSTS_VALUE_FROM_PTR(rc);
}

DEFINE_BSTS_OBJ(Struct24,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;BValue _6;BValue _7;BValue _8;BValue _9;BValue _10;BValue _11;BValue _12;BValue _13;BValue _14;BValue _15;BValue _16;BValue _17;BValue _18;BValue _19;BValue _20;BValue _21;BValue _22;BValue _23;);

BValue alloc_struct24(BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5, BValue b6, BValue b7, BValue b8, BValue b9, BValue b10, BValue b11, BValue b12, BValue b13, BValue b14, BValue b15, BValue b16, BValue b17, BValue b18, BValue b19, BValue b20, BValue b21, BValue b22, BValue b23) {
    Struct24* rc = GC_malloc(sizeof(Struct24));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_struct24");
        abort();
    }
    rc->_0 = b0;
    rc->_1 = b1;
    rc->_2 = b2;
    rc->_3 = b3;
    rc->_4 = b4;
    rc->_5 = b5;
    rc->_6 = b6;
    rc->_7 = b7;
    rc->_8 = b8;
    rc->_9 = b9;
    rc->_10 = b10;
    rc->_11 = b11;
    rc->_12 = b12;
    rc->_13 = b13;
    rc->_14 = b14;
    rc->_15 = b15;
    rc->_16 = b16;
    rc->_17 = b17;
    rc->_18 = b18;
    rc->_19 = b19;
    rc->_20 = b20;
    rc->_21 = b21;
    rc->_22 = b22;
    rc->_23 = b23;
    return BSTS_VALUE_FROM_PTR(rc);
}

DEFINE_BSTS_OBJ(Struct25,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;BValue _6;BValue _7;BValue _8;BValue _9;BValue _10;BValue _11;BValue _12;BValue _13;BValue _14;BValue _15;BValue _16;BValue _17;BValue _18;BValue _19;BValue _20;BValue _21;BValue _22;BValue _23;BValue _24;);

BValue alloc_struct25(BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5, BValue b6, BValue b7, BValue b8, BValue b9, BValue b10, BValue b11, BValue b12, BValue b13, BValue b14, BValue b15, BValue b16, BValue b17, BValue b18, BValue b19, BValue b20, BValue b21, BValue b22, BValue b23, BValue b24) {
    Struct25* rc = GC_malloc(sizeof(Struct25));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_struct25");
        abort();
    }
    rc->_0 = b0;
    rc->_1 = b1;
    rc->_2 = b2;
    rc->_3 = b3;
    rc->_4 = b4;
    rc->_5 = b5;
    rc->_6 = b6;
    rc->_7 = b7;
    rc->_8 = b8;
    rc->_9 = b9;
    rc->_10 = b10;
    rc->_11 = b11;
    rc->_12 = b12;
    rc->_13 = b13;
    rc->_14 = b14;
    rc->_15 = b15;
    rc->_16 = b16;
    rc->_17 = b17;
    rc->_18 = b18;
    rc->_19 = b19;
    rc->_20 = b20;
    rc->_21 = b21;
    rc->_22 = b22;
    rc->_23 = b23;
    rc->_24 = b24;
    return BSTS_VALUE_FROM_PTR(rc);
}

DEFINE_BSTS_OBJ(Struct26,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;BValue _6;BValue _7;BValue _8;BValue _9;BValue _10;BValue _11;BValue _12;BValue _13;BValue _14;BValue _15;BValue _16;BValue _17;BValue _18;BValue _19;BValue _20;BValue _21;BValue _22;BValue _23;BValue _24;BValue _25;);

BValue alloc_struct26(BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5, BValue b6, BValue b7, BValue b8, BValue b9, BValue b10, BValue b11, BValue b12, BValue b13, BValue b14, BValue b15, BValue b16, BValue b17, BValue b18, BValue b19, BValue b20, BValue b21, BValue b22, BValue b23, BValue b24, BValue b25) {
    Struct26* rc = GC_malloc(sizeof(Struct26));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_struct26");
        abort();
    }
    rc->_0 = b0;
    rc->_1 = b1;
    rc->_2 = b2;
    rc->_3 = b3;
    rc->_4 = b4;
    rc->_5 = b5;
    rc->_6 = b6;
    rc->_7 = b7;
    rc->_8 = b8;
    rc->_9 = b9;
    rc->_10 = b10;
    rc->_11 = b11;
    rc->_12 = b12;
    rc->_13 = b13;
    rc->_14 = b14;
    rc->_15 = b15;
    rc->_16 = b16;
    rc->_17 = b17;
    rc->_18 = b18;
    rc->_19 = b19;
    rc->_20 = b20;
    rc->_21 = b21;
    rc->_22 = b22;
    rc->_23 = b23;
    rc->_24 = b24;
    rc->_25 = b25;
    return BSTS_VALUE_FROM_PTR(rc);
}

DEFINE_BSTS_OBJ(Struct27,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;BValue _6;BValue _7;BValue _8;BValue _9;BValue _10;BValue _11;BValue _12;BValue _13;BValue _14;BValue _15;BValue _16;BValue _17;BValue _18;BValue _19;BValue _20;BValue _21;BValue _22;BValue _23;BValue _24;BValue _25;BValue _26;);

BValue alloc_struct27(BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5, BValue b6, BValue b7, BValue b8, BValue b9, BValue b10, BValue b11, BValue b12, BValue b13, BValue b14, BValue b15, BValue b16, BValue b17, BValue b18, BValue b19, BValue b20, BValue b21, BValue b22, BValue b23, BValue b24, BValue b25, BValue b26) {
    Struct27* rc = GC_malloc(sizeof(Struct27));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_struct27");
        abort();
    }
    rc->_0 = b0;
    rc->_1 = b1;
    rc->_2 = b2;
    rc->_3 = b3;
    rc->_4 = b4;
    rc->_5 = b5;
    rc->_6 = b6;
    rc->_7 = b7;
    rc->_8 = b8;
    rc->_9 = b9;
    rc->_10 = b10;
    rc->_11 = b11;
    rc->_12 = b12;
    rc->_13 = b13;
    rc->_14 = b14;
    rc->_15 = b15;
    rc->_16 = b16;
    rc->_17 = b17;
    rc->_18 = b18;
    rc->_19 = b19;
    rc->_20 = b20;
    rc->_21 = b21;
    rc->_22 = b22;
    rc->_23 = b23;
    rc->_24 = b24;
    rc->_25 = b25;
    rc->_26 = b26;
    return BSTS_VALUE_FROM_PTR(rc);
}

DEFINE_BSTS_OBJ(Struct28,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;BValue _6;BValue _7;BValue _8;BValue _9;BValue _10;BValue _11;BValue _12;BValue _13;BValue _14;BValue _15;BValue _16;BValue _17;BValue _18;BValue _19;BValue _20;BValue _21;BValue _22;BValue _23;BValue _24;BValue _25;BValue _26;BValue _27;);

BValue alloc_struct28(BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5, BValue b6, BValue b7, BValue b8, BValue b9, BValue b10, BValue b11, BValue b12, BValue b13, BValue b14, BValue b15, BValue b16, BValue b17, BValue b18, BValue b19, BValue b20, BValue b21, BValue b22, BValue b23, BValue b24, BValue b25, BValue b26, BValue b27) {
    Struct28* rc = GC_malloc(sizeof(Struct28));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_struct28");
        abort();
    }
    rc->_0 = b0;
    rc->_1 = b1;
    rc->_2 = b2;
    rc->_3 = b3;
    rc->_4 = b4;
    rc->_5 = b5;
    rc->_6 = b6;
    rc->_7 = b7;
    rc->_8 = b8;
    rc->_9 = b9;
    rc->_10 = b10;
    rc->_11 = b11;
    rc->_12 = b12;
    rc->_13 = b13;
    rc->_14 = b14;
    rc->_15 = b15;
    rc->_16 = b16;
    rc->_17 = b17;
    rc->_18 = b18;
    rc->_19 = b19;
    rc->_20 = b20;
    rc->_21 = b21;
    rc->_22 = b22;
    rc->_23 = b23;
    rc->_24 = b24;
    rc->_25 = b25;
    rc->_26 = b26;
    rc->_27 = b27;
    return BSTS_VALUE_FROM_PTR(rc);
}

DEFINE_BSTS_OBJ(Struct29,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;BValue _6;BValue _7;BValue _8;BValue _9;BValue _10;BValue _11;BValue _12;BValue _13;BValue _14;BValue _15;BValue _16;BValue _17;BValue _18;BValue _19;BValue _20;BValue _21;BValue _22;BValue _23;BValue _24;BValue _25;BValue _26;BValue _27;BValue _28;);

BValue alloc_struct29(BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5, BValue b6, BValue b7, BValue b8, BValue b9, BValue b10, BValue b11, BValue b12, BValue b13, BValue b14, BValue b15, BValue b16, BValue b17, BValue b18, BValue b19, BValue b20, BValue b21, BValue b22, BValue b23, BValue b24, BValue b25, BValue b26, BValue b27, BValue b28) {
    Struct29* rc = GC_malloc(sizeof(Struct29));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_struct29");
        abort();
    }
    rc->_0 = b0;
    rc->_1 = b1;
    rc->_2 = b2;
    rc->_3 = b3;
    rc->_4 = b4;
    rc->_5 = b5;
    rc->_6 = b6;
    rc->_7 = b7;
    rc->_8 = b8;
    rc->_9 = b9;
    rc->_10 = b10;
    rc->_11 = b11;
    rc->_12 = b12;
    rc->_13 = b13;
    rc->_14 = b14;
    rc->_15 = b15;
    rc->_16 = b16;
    rc->_17 = b17;
    rc->_18 = b18;
    rc->_19 = b19;
    rc->_20 = b20;
    rc->_21 = b21;
    rc->_22 = b22;
    rc->_23 = b23;
    rc->_24 = b24;
    rc->_25 = b25;
    rc->_26 = b26;
    rc->_27 = b27;
    rc->_28 = b28;
    return BSTS_VALUE_FROM_PTR(rc);
}

DEFINE_BSTS_OBJ(Struct30,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;BValue _6;BValue _7;BValue _8;BValue _9;BValue _10;BValue _11;BValue _12;BValue _13;BValue _14;BValue _15;BValue _16;BValue _17;BValue _18;BValue _19;BValue _20;BValue _21;BValue _22;BValue _23;BValue _24;BValue _25;BValue _26;BValue _27;BValue _28;BValue _29;);

BValue alloc_struct30(BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5, BValue b6, BValue b7, BValue b8, BValue b9, BValue b10, BValue b11, BValue b12, BValue b13, BValue b14, BValue b15, BValue b16, BValue b17, BValue b18, BValue b19, BValue b20, BValue b21, BValue b22, BValue b23, BValue b24, BValue b25, BValue b26, BValue b27, BValue b28, BValue b29) {
    Struct30* rc = GC_malloc(sizeof(Struct30));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_struct30");
        abort();
    }
    rc->_0 = b0;
    rc->_1 = b1;
    rc->_2 = b2;
    rc->_3 = b3;
    rc->_4 = b4;
    rc->_5 = b5;
    rc->_6 = b6;
    rc->_7 = b7;
    rc->_8 = b8;
    rc->_9 = b9;
    rc->_10 = b10;
    rc->_11 = b11;
    rc->_12 = b12;
    rc->_13 = b13;
    rc->_14 = b14;
    rc->_15 = b15;
    rc->_16 = b16;
    rc->_17 = b17;
    rc->_18 = b18;
    rc->_19 = b19;
    rc->_20 = b20;
    rc->_21 = b21;
    rc->_22 = b22;
    rc->_23 = b23;
    rc->_24 = b24;
    rc->_25 = b25;
    rc->_26 = b26;
    rc->_27 = b27;
    rc->_28 = b28;
    rc->_29 = b29;
    return BSTS_VALUE_FROM_PTR(rc);
}

DEFINE_BSTS_OBJ(Struct31,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;BValue _6;BValue _7;BValue _8;BValue _9;BValue _10;BValue _11;BValue _12;BValue _13;BValue _14;BValue _15;BValue _16;BValue _17;BValue _18;BValue _19;BValue _20;BValue _21;BValue _22;BValue _23;BValue _24;BValue _25;BValue _26;BValue _27;BValue _28;BValue _29;BValue _30;);

BValue alloc_struct31(BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5, BValue b6, BValue b7, BValue b8, BValue b9, BValue b10, BValue b11, BValue b12, BValue b13, BValue b14, BValue b15, BValue b16, BValue b17, BValue b18, BValue b19, BValue b20, BValue b21, BValue b22, BValue b23, BValue b24, BValue b25, BValue b26, BValue b27, BValue b28, BValue b29, BValue b30) {
    Struct31* rc = GC_malloc(sizeof(Struct31));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_struct31");
        abort();
    }
    rc->_0 = b0;
    rc->_1 = b1;
    rc->_2 = b2;
    rc->_3 = b3;
    rc->_4 = b4;
    rc->_5 = b5;
    rc->_6 = b6;
    rc->_7 = b7;
    rc->_8 = b8;
    rc->_9 = b9;
    rc->_10 = b10;
    rc->_11 = b11;
    rc->_12 = b12;
    rc->_13 = b13;
    rc->_14 = b14;
    rc->_15 = b15;
    rc->_16 = b16;
    rc->_17 = b17;
    rc->_18 = b18;
    rc->_19 = b19;
    rc->_20 = b20;
    rc->_21 = b21;
    rc->_22 = b22;
    rc->_23 = b23;
    rc->_24 = b24;
    rc->_25 = b25;
    rc->_26 = b26;
    rc->_27 = b27;
    rc->_28 = b28;
    rc->_29 = b29;
    rc->_30 = b30;
    return BSTS_VALUE_FROM_PTR(rc);
}

DEFINE_BSTS_OBJ(Struct32,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;BValue _6;BValue _7;BValue _8;BValue _9;BValue _10;BValue _11;BValue _12;BValue _13;BValue _14;BValue _15;BValue _16;BValue _17;BValue _18;BValue _19;BValue _20;BValue _21;BValue _22;BValue _23;BValue _24;BValue _25;BValue _26;BValue _27;BValue _28;BValue _29;BValue _30;BValue _31;);

BValue alloc_struct32(BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5, BValue b6, BValue b7, BValue b8, BValue b9, BValue b10, BValue b11, BValue b12, BValue b13, BValue b14, BValue b15, BValue b16, BValue b17, BValue b18, BValue b19, BValue b20, BValue b21, BValue b22, BValue b23, BValue b24, BValue b25, BValue b26, BValue b27, BValue b28, BValue b29, BValue b30, BValue b31) {
    Struct32* rc = GC_malloc(sizeof(Struct32));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_struct32");
        abort();
    }
    rc->_0 = b0;
    rc->_1 = b1;
    rc->_2 = b2;
    rc->_3 = b3;
    rc->_4 = b4;
    rc->_5 = b5;
    rc->_6 = b6;
    rc->_7 = b7;
    rc->_8 = b8;
    rc->_9 = b9;
    rc->_10 = b10;
    rc->_11 = b11;
    rc->_12 = b12;
    rc->_13 = b13;
    rc->_14 = b14;
    rc->_15 = b15;
    rc->_16 = b16;
    rc->_17 = b17;
    rc->_18 = b18;
    rc->_19 = b19;
    rc->_20 = b20;
    rc->_21 = b21;
    rc->_22 = b22;
    rc->_23 = b23;
    rc->_24 = b24;
    rc->_25 = b25;
    rc->_26 = b26;
    rc->_27 = b27;
    rc->_28 = b28;
    rc->_29 = b29;
    rc->_30 = b30;
    rc->_31 = b31;
    return BSTS_VALUE_FROM_PTR(rc);
}

// ENUMS
DEFINE_BSTS_ENUM(Enum1,BValue _0;);

BValue alloc_enum1(ENUM_TAG tag, BValue b0) {
    Enum1* rc = GC_malloc(sizeof(Enum1));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_enum1");
        abort();
    }
    rc->tag = tag;
    rc->_0 = b0;
    return BSTS_VALUE_FROM_PTR(rc);
}

DEFINE_BSTS_ENUM(Enum2,BValue _0;BValue _1;);

BValue alloc_enum2(ENUM_TAG tag, BValue b0, BValue b1) {
    Enum2* rc = GC_malloc(sizeof(Enum2));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_enum2");
        abort();
    }
    rc->tag = tag;
    rc->_0 = b0;
    rc->_1 = b1;
    return BSTS_VALUE_FROM_PTR(rc);
}

DEFINE_BSTS_ENUM(Enum3,BValue _0;BValue _1;BValue _2;);

BValue alloc_enum3(ENUM_TAG tag, BValue b0, BValue b1, BValue b2) {
    Enum3* rc = GC_malloc(sizeof(Enum3));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_enum3");
        abort();
    }
    rc->tag = tag;
    rc->_0 = b0;
    rc->_1 = b1;
    rc->_2 = b2;
    return BSTS_VALUE_FROM_PTR(rc);
}

DEFINE_BSTS_ENUM(Enum4,BValue _0;BValue _1;BValue _2;BValue _3;);

BValue alloc_enum4(ENUM_TAG tag, BValue b0, BValue b1, BValue b2, BValue b3) {
    Enum4* rc = GC_malloc(sizeof(Enum4));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_enum4");
        abort();
    }
    rc->tag = tag;
    rc->_0 = b0;
    rc->_1 = b1;
    rc->_2 = b2;
    rc->_3 = b3;
    return BSTS_VALUE_FROM_PTR(rc);
}

DEFINE_BSTS_ENUM(Enum5,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;);

BValue alloc_enum5(ENUM_TAG tag, BValue b0, BValue b1, BValue b2, BValue b3, BValue b4) {
    Enum5* rc = GC_malloc(sizeof(Enum5));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_enum5");
        abort();
    }
    rc->tag = tag;
    rc->_0 = b0;
    rc->_1 = b1;
    rc->_2 = b2;
    rc->_3 = b3;
    rc->_4 = b4;
    return BSTS_VALUE_FROM_PTR(rc);
}

DEFINE_BSTS_ENUM(Enum6,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;);

BValue alloc_enum6(ENUM_TAG tag, BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5) {
    Enum6* rc = GC_malloc(sizeof(Enum6));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_enum6");
        abort();
    }
    rc->tag = tag;
    rc->_0 = b0;
    rc->_1 = b1;
    rc->_2 = b2;
    rc->_3 = b3;
    rc->_4 = b4;
    rc->_5 = b5;
    return BSTS_VALUE_FROM_PTR(rc);
}

DEFINE_BSTS_ENUM(Enum7,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;BValue _6;);

BValue alloc_enum7(ENUM_TAG tag, BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5, BValue b6) {
    Enum7* rc = GC_malloc(sizeof(Enum7));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_enum7");
        abort();
    }
    rc->tag = tag;
    rc->_0 = b0;
    rc->_1 = b1;
    rc->_2 = b2;
    rc->_3 = b3;
    rc->_4 = b4;
    rc->_5 = b5;
    rc->_6 = b6;
    return BSTS_VALUE_FROM_PTR(rc);
}

DEFINE_BSTS_ENUM(Enum8,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;BValue _6;BValue _7;);

BValue alloc_enum8(ENUM_TAG tag, BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5, BValue b6, BValue b7) {
    Enum8* rc = GC_malloc(sizeof(Enum8));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_enum8");
        abort();
    }
    rc->tag = tag;
    rc->_0 = b0;
    rc->_1 = b1;
    rc->_2 = b2;
    rc->_3 = b3;
    rc->_4 = b4;
    rc->_5 = b5;
    rc->_6 = b6;
    rc->_7 = b7;
    return BSTS_VALUE_FROM_PTR(rc);
}

DEFINE_BSTS_ENUM(Enum9,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;BValue _6;BValue _7;BValue _8;);

BValue alloc_enum9(ENUM_TAG tag, BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5, BValue b6, BValue b7, BValue b8) {
    Enum9* rc = GC_malloc(sizeof(Enum9));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_enum9");
        abort();
    }
    rc->tag = tag;
    rc->_0 = b0;
    rc->_1 = b1;
    rc->_2 = b2;
    rc->_3 = b3;
    rc->_4 = b4;
    rc->_5 = b5;
    rc->_6 = b6;
    rc->_7 = b7;
    rc->_8 = b8;
    return BSTS_VALUE_FROM_PTR(rc);
}

DEFINE_BSTS_ENUM(Enum10,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;BValue _6;BValue _7;BValue _8;BValue _9;);

BValue alloc_enum10(ENUM_TAG tag, BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5, BValue b6, BValue b7, BValue b8, BValue b9) {
    Enum10* rc = GC_malloc(sizeof(Enum10));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_enum10");
        abort();
    }
    rc->tag = tag;
    rc->_0 = b0;
    rc->_1 = b1;
    rc->_2 = b2;
    rc->_3 = b3;
    rc->_4 = b4;
    rc->_5 = b5;
    rc->_6 = b6;
    rc->_7 = b7;
    rc->_8 = b8;
    rc->_9 = b9;
    return BSTS_VALUE_FROM_PTR(rc);
}

DEFINE_BSTS_ENUM(Enum11,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;BValue _6;BValue _7;BValue _8;BValue _9;BValue _10;);

BValue alloc_enum11(ENUM_TAG tag, BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5, BValue b6, BValue b7, BValue b8, BValue b9, BValue b10) {
    Enum11* rc = GC_malloc(sizeof(Enum11));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_enum11");
        abort();
    }
    rc->tag = tag;
    rc->_0 = b0;
    rc->_1 = b1;
    rc->_2 = b2;
    rc->_3 = b3;
    rc->_4 = b4;
    rc->_5 = b5;
    rc->_6 = b6;
    rc->_7 = b7;
    rc->_8 = b8;
    rc->_9 = b9;
    rc->_10 = b10;
    return BSTS_VALUE_FROM_PTR(rc);
}

DEFINE_BSTS_ENUM(Enum12,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;BValue _6;BValue _7;BValue _8;BValue _9;BValue _10;BValue _11;);

BValue alloc_enum12(ENUM_TAG tag, BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5, BValue b6, BValue b7, BValue b8, BValue b9, BValue b10, BValue b11) {
    Enum12* rc = GC_malloc(sizeof(Enum12));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_enum12");
        abort();
    }
    rc->tag = tag;
    rc->_0 = b0;
    rc->_1 = b1;
    rc->_2 = b2;
    rc->_3 = b3;
    rc->_4 = b4;
    rc->_5 = b5;
    rc->_6 = b6;
    rc->_7 = b7;
    rc->_8 = b8;
    rc->_9 = b9;
    rc->_10 = b10;
    rc->_11 = b11;
    return BSTS_VALUE_FROM_PTR(rc);
}

DEFINE_BSTS_ENUM(Enum13,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;BValue _6;BValue _7;BValue _8;BValue _9;BValue _10;BValue _11;BValue _12;);

BValue alloc_enum13(ENUM_TAG tag, BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5, BValue b6, BValue b7, BValue b8, BValue b9, BValue b10, BValue b11, BValue b12) {
    Enum13* rc = GC_malloc(sizeof(Enum13));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_enum13");
        abort();
    }
    rc->tag = tag;
    rc->_0 = b0;
    rc->_1 = b1;
    rc->_2 = b2;
    rc->_3 = b3;
    rc->_4 = b4;
    rc->_5 = b5;
    rc->_6 = b6;
    rc->_7 = b7;
    rc->_8 = b8;
    rc->_9 = b9;
    rc->_10 = b10;
    rc->_11 = b11;
    rc->_12 = b12;
    return BSTS_VALUE_FROM_PTR(rc);
}

DEFINE_BSTS_ENUM(Enum14,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;BValue _6;BValue _7;BValue _8;BValue _9;BValue _10;BValue _11;BValue _12;BValue _13;);

BValue alloc_enum14(ENUM_TAG tag, BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5, BValue b6, BValue b7, BValue b8, BValue b9, BValue b10, BValue b11, BValue b12, BValue b13) {
    Enum14* rc = GC_malloc(sizeof(Enum14));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_enum14");
        abort();
    }
    rc->tag = tag;
    rc->_0 = b0;
    rc->_1 = b1;
    rc->_2 = b2;
    rc->_3 = b3;
    rc->_4 = b4;
    rc->_5 = b5;
    rc->_6 = b6;
    rc->_7 = b7;
    rc->_8 = b8;
    rc->_9 = b9;
    rc->_10 = b10;
    rc->_11 = b11;
    rc->_12 = b12;
    rc->_13 = b13;
    return BSTS_VALUE_FROM_PTR(rc);
}

DEFINE_BSTS_ENUM(Enum15,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;BValue _6;BValue _7;BValue _8;BValue _9;BValue _10;BValue _11;BValue _12;BValue _13;BValue _14;);

BValue alloc_enum15(ENUM_TAG tag, BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5, BValue b6, BValue b7, BValue b8, BValue b9, BValue b10, BValue b11, BValue b12, BValue b13, BValue b14) {
    Enum15* rc = GC_malloc(sizeof(Enum15));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_enum15");
        abort();
    }
    rc->tag = tag;
    rc->_0 = b0;
    rc->_1 = b1;
    rc->_2 = b2;
    rc->_3 = b3;
    rc->_4 = b4;
    rc->_5 = b5;
    rc->_6 = b6;
    rc->_7 = b7;
    rc->_8 = b8;
    rc->_9 = b9;
    rc->_10 = b10;
    rc->_11 = b11;
    rc->_12 = b12;
    rc->_13 = b13;
    rc->_14 = b14;
    return BSTS_VALUE_FROM_PTR(rc);
}

DEFINE_BSTS_ENUM(Enum16,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;BValue _6;BValue _7;BValue _8;BValue _9;BValue _10;BValue _11;BValue _12;BValue _13;BValue _14;BValue _15;);

BValue alloc_enum16(ENUM_TAG tag, BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5, BValue b6, BValue b7, BValue b8, BValue b9, BValue b10, BValue b11, BValue b12, BValue b13, BValue b14, BValue b15) {
    Enum16* rc = GC_malloc(sizeof(Enum16));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_enum16");
        abort();
    }
    rc->tag = tag;
    rc->_0 = b0;
    rc->_1 = b1;
    rc->_2 = b2;
    rc->_3 = b3;
    rc->_4 = b4;
    rc->_5 = b5;
    rc->_6 = b6;
    rc->_7 = b7;
    rc->_8 = b8;
    rc->_9 = b9;
    rc->_10 = b10;
    rc->_11 = b11;
    rc->_12 = b12;
    rc->_13 = b13;
    rc->_14 = b14;
    rc->_15 = b15;
    return BSTS_VALUE_FROM_PTR(rc);
}

DEFINE_BSTS_ENUM(Enum17,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;BValue _6;BValue _7;BValue _8;BValue _9;BValue _10;BValue _11;BValue _12;BValue _13;BValue _14;BValue _15;BValue _16;);

BValue alloc_enum17(ENUM_TAG tag, BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5, BValue b6, BValue b7, BValue b8, BValue b9, BValue b10, BValue b11, BValue b12, BValue b13, BValue b14, BValue b15, BValue b16) {
    Enum17* rc = GC_malloc(sizeof(Enum17));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_enum17");
        abort();
    }
    rc->tag = tag;
    rc->_0 = b0;
    rc->_1 = b1;
    rc->_2 = b2;
    rc->_3 = b3;
    rc->_4 = b4;
    rc->_5 = b5;
    rc->_6 = b6;
    rc->_7 = b7;
    rc->_8 = b8;
    rc->_9 = b9;
    rc->_10 = b10;
    rc->_11 = b11;
    rc->_12 = b12;
    rc->_13 = b13;
    rc->_14 = b14;
    rc->_15 = b15;
    rc->_16 = b16;
    return BSTS_VALUE_FROM_PTR(rc);
}

DEFINE_BSTS_ENUM(Enum18,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;BValue _6;BValue _7;BValue _8;BValue _9;BValue _10;BValue _11;BValue _12;BValue _13;BValue _14;BValue _15;BValue _16;BValue _17;);

BValue alloc_enum18(ENUM_TAG tag, BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5, BValue b6, BValue b7, BValue b8, BValue b9, BValue b10, BValue b11, BValue b12, BValue b13, BValue b14, BValue b15, BValue b16, BValue b17) {
    Enum18* rc = GC_malloc(sizeof(Enum18));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_enum18");
        abort();
    }
    rc->tag = tag;
    rc->_0 = b0;
    rc->_1 = b1;
    rc->_2 = b2;
    rc->_3 = b3;
    rc->_4 = b4;
    rc->_5 = b5;
    rc->_6 = b6;
    rc->_7 = b7;
    rc->_8 = b8;
    rc->_9 = b9;
    rc->_10 = b10;
    rc->_11 = b11;
    rc->_12 = b12;
    rc->_13 = b13;
    rc->_14 = b14;
    rc->_15 = b15;
    rc->_16 = b16;
    rc->_17 = b17;
    return BSTS_VALUE_FROM_PTR(rc);
}

DEFINE_BSTS_ENUM(Enum19,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;BValue _6;BValue _7;BValue _8;BValue _9;BValue _10;BValue _11;BValue _12;BValue _13;BValue _14;BValue _15;BValue _16;BValue _17;BValue _18;);

BValue alloc_enum19(ENUM_TAG tag, BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5, BValue b6, BValue b7, BValue b8, BValue b9, BValue b10, BValue b11, BValue b12, BValue b13, BValue b14, BValue b15, BValue b16, BValue b17, BValue b18) {
    Enum19* rc = GC_malloc(sizeof(Enum19));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_enum19");
        abort();
    }
    rc->tag = tag;
    rc->_0 = b0;
    rc->_1 = b1;
    rc->_2 = b2;
    rc->_3 = b3;
    rc->_4 = b4;
    rc->_5 = b5;
    rc->_6 = b6;
    rc->_7 = b7;
    rc->_8 = b8;
    rc->_9 = b9;
    rc->_10 = b10;
    rc->_11 = b11;
    rc->_12 = b12;
    rc->_13 = b13;
    rc->_14 = b14;
    rc->_15 = b15;
    rc->_16 = b16;
    rc->_17 = b17;
    rc->_18 = b18;
    return BSTS_VALUE_FROM_PTR(rc);
}

DEFINE_BSTS_ENUM(Enum20,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;BValue _6;BValue _7;BValue _8;BValue _9;BValue _10;BValue _11;BValue _12;BValue _13;BValue _14;BValue _15;BValue _16;BValue _17;BValue _18;BValue _19;);

BValue alloc_enum20(ENUM_TAG tag, BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5, BValue b6, BValue b7, BValue b8, BValue b9, BValue b10, BValue b11, BValue b12, BValue b13, BValue b14, BValue b15, BValue b16, BValue b17, BValue b18, BValue b19) {
    Enum20* rc = GC_malloc(sizeof(Enum20));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_enum20");
        abort();
    }
    rc->tag = tag;
    rc->_0 = b0;
    rc->_1 = b1;
    rc->_2 = b2;
    rc->_3 = b3;
    rc->_4 = b4;
    rc->_5 = b5;
    rc->_6 = b6;
    rc->_7 = b7;
    rc->_8 = b8;
    rc->_9 = b9;
    rc->_10 = b10;
    rc->_11 = b11;
    rc->_12 = b12;
    rc->_13 = b13;
    rc->_14 = b14;
    rc->_15 = b15;
    rc->_16 = b16;
    rc->_17 = b17;
    rc->_18 = b18;
    rc->_19 = b19;
    return BSTS_VALUE_FROM_PTR(rc);
}

DEFINE_BSTS_ENUM(Enum21,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;BValue _6;BValue _7;BValue _8;BValue _9;BValue _10;BValue _11;BValue _12;BValue _13;BValue _14;BValue _15;BValue _16;BValue _17;BValue _18;BValue _19;BValue _20;);

BValue alloc_enum21(ENUM_TAG tag, BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5, BValue b6, BValue b7, BValue b8, BValue b9, BValue b10, BValue b11, BValue b12, BValue b13, BValue b14, BValue b15, BValue b16, BValue b17, BValue b18, BValue b19, BValue b20) {
    Enum21* rc = GC_malloc(sizeof(Enum21));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_enum21");
        abort();
    }
    rc->tag = tag;
    rc->_0 = b0;
    rc->_1 = b1;
    rc->_2 = b2;
    rc->_3 = b3;
    rc->_4 = b4;
    rc->_5 = b5;
    rc->_6 = b6;
    rc->_7 = b7;
    rc->_8 = b8;
    rc->_9 = b9;
    rc->_10 = b10;
    rc->_11 = b11;
    rc->_12 = b12;
    rc->_13 = b13;
    rc->_14 = b14;
    rc->_15 = b15;
    rc->_16 = b16;
    rc->_17 = b17;
    rc->_18 = b18;
    rc->_19 = b19;
    rc->_20 = b20;
    return BSTS_VALUE_FROM_PTR(rc);
}

DEFINE_BSTS_ENUM(Enum22,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;BValue _6;BValue _7;BValue _8;BValue _9;BValue _10;BValue _11;BValue _12;BValue _13;BValue _14;BValue _15;BValue _16;BValue _17;BValue _18;BValue _19;BValue _20;BValue _21;);

BValue alloc_enum22(ENUM_TAG tag, BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5, BValue b6, BValue b7, BValue b8, BValue b9, BValue b10, BValue b11, BValue b12, BValue b13, BValue b14, BValue b15, BValue b16, BValue b17, BValue b18, BValue b19, BValue b20, BValue b21) {
    Enum22* rc = GC_malloc(sizeof(Enum22));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_enum22");
        abort();
    }
    rc->tag = tag;
    rc->_0 = b0;
    rc->_1 = b1;
    rc->_2 = b2;
    rc->_3 = b3;
    rc->_4 = b4;
    rc->_5 = b5;
    rc->_6 = b6;
    rc->_7 = b7;
    rc->_8 = b8;
    rc->_9 = b9;
    rc->_10 = b10;
    rc->_11 = b11;
    rc->_12 = b12;
    rc->_13 = b13;
    rc->_14 = b14;
    rc->_15 = b15;
    rc->_16 = b16;
    rc->_17 = b17;
    rc->_18 = b18;
    rc->_19 = b19;
    rc->_20 = b20;
    rc->_21 = b21;
    return BSTS_VALUE_FROM_PTR(rc);
}

DEFINE_BSTS_ENUM(Enum23,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;BValue _6;BValue _7;BValue _8;BValue _9;BValue _10;BValue _11;BValue _12;BValue _13;BValue _14;BValue _15;BValue _16;BValue _17;BValue _18;BValue _19;BValue _20;BValue _21;BValue _22;);

BValue alloc_enum23(ENUM_TAG tag, BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5, BValue b6, BValue b7, BValue b8, BValue b9, BValue b10, BValue b11, BValue b12, BValue b13, BValue b14, BValue b15, BValue b16, BValue b17, BValue b18, BValue b19, BValue b20, BValue b21, BValue b22) {
    Enum23* rc = GC_malloc(sizeof(Enum23));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_enum23");
        abort();
    }
    rc->tag = tag;
    rc->_0 = b0;
    rc->_1 = b1;
    rc->_2 = b2;
    rc->_3 = b3;
    rc->_4 = b4;
    rc->_5 = b5;
    rc->_6 = b6;
    rc->_7 = b7;
    rc->_8 = b8;
    rc->_9 = b9;
    rc->_10 = b10;
    rc->_11 = b11;
    rc->_12 = b12;
    rc->_13 = b13;
    rc->_14 = b14;
    rc->_15 = b15;
    rc->_16 = b16;
    rc->_17 = b17;
    rc->_18 = b18;
    rc->_19 = b19;
    rc->_20 = b20;
    rc->_21 = b21;
    rc->_22 = b22;
    return BSTS_VALUE_FROM_PTR(rc);
}

DEFINE_BSTS_ENUM(Enum24,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;BValue _6;BValue _7;BValue _8;BValue _9;BValue _10;BValue _11;BValue _12;BValue _13;BValue _14;BValue _15;BValue _16;BValue _17;BValue _18;BValue _19;BValue _20;BValue _21;BValue _22;BValue _23;);

BValue alloc_enum24(ENUM_TAG tag, BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5, BValue b6, BValue b7, BValue b8, BValue b9, BValue b10, BValue b11, BValue b12, BValue b13, BValue b14, BValue b15, BValue b16, BValue b17, BValue b18, BValue b19, BValue b20, BValue b21, BValue b22, BValue b23) {
    Enum24* rc = GC_malloc(sizeof(Enum24));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_enum24");
        abort();
    }
    rc->tag = tag;
    rc->_0 = b0;
    rc->_1 = b1;
    rc->_2 = b2;
    rc->_3 = b3;
    rc->_4 = b4;
    rc->_5 = b5;
    rc->_6 = b6;
    rc->_7 = b7;
    rc->_8 = b8;
    rc->_9 = b9;
    rc->_10 = b10;
    rc->_11 = b11;
    rc->_12 = b12;
    rc->_13 = b13;
    rc->_14 = b14;
    rc->_15 = b15;
    rc->_16 = b16;
    rc->_17 = b17;
    rc->_18 = b18;
    rc->_19 = b19;
    rc->_20 = b20;
    rc->_21 = b21;
    rc->_22 = b22;
    rc->_23 = b23;
    return BSTS_VALUE_FROM_PTR(rc);
}

DEFINE_BSTS_ENUM(Enum25,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;BValue _6;BValue _7;BValue _8;BValue _9;BValue _10;BValue _11;BValue _12;BValue _13;BValue _14;BValue _15;BValue _16;BValue _17;BValue _18;BValue _19;BValue _20;BValue _21;BValue _22;BValue _23;BValue _24;);

BValue alloc_enum25(ENUM_TAG tag, BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5, BValue b6, BValue b7, BValue b8, BValue b9, BValue b10, BValue b11, BValue b12, BValue b13, BValue b14, BValue b15, BValue b16, BValue b17, BValue b18, BValue b19, BValue b20, BValue b21, BValue b22, BValue b23, BValue b24) {
    Enum25* rc = GC_malloc(sizeof(Enum25));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_enum25");
        abort();
    }
    rc->tag = tag;
    rc->_0 = b0;
    rc->_1 = b1;
    rc->_2 = b2;
    rc->_3 = b3;
    rc->_4 = b4;
    rc->_5 = b5;
    rc->_6 = b6;
    rc->_7 = b7;
    rc->_8 = b8;
    rc->_9 = b9;
    rc->_10 = b10;
    rc->_11 = b11;
    rc->_12 = b12;
    rc->_13 = b13;
    rc->_14 = b14;
    rc->_15 = b15;
    rc->_16 = b16;
    rc->_17 = b17;
    rc->_18 = b18;
    rc->_19 = b19;
    rc->_20 = b20;
    rc->_21 = b21;
    rc->_22 = b22;
    rc->_23 = b23;
    rc->_24 = b24;
    return BSTS_VALUE_FROM_PTR(rc);
}

DEFINE_BSTS_ENUM(Enum26,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;BValue _6;BValue _7;BValue _8;BValue _9;BValue _10;BValue _11;BValue _12;BValue _13;BValue _14;BValue _15;BValue _16;BValue _17;BValue _18;BValue _19;BValue _20;BValue _21;BValue _22;BValue _23;BValue _24;BValue _25;);

BValue alloc_enum26(ENUM_TAG tag, BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5, BValue b6, BValue b7, BValue b8, BValue b9, BValue b10, BValue b11, BValue b12, BValue b13, BValue b14, BValue b15, BValue b16, BValue b17, BValue b18, BValue b19, BValue b20, BValue b21, BValue b22, BValue b23, BValue b24, BValue b25) {
    Enum26* rc = GC_malloc(sizeof(Enum26));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_enum26");
        abort();
    }
    rc->tag = tag;
    rc->_0 = b0;
    rc->_1 = b1;
    rc->_2 = b2;
    rc->_3 = b3;
    rc->_4 = b4;
    rc->_5 = b5;
    rc->_6 = b6;
    rc->_7 = b7;
    rc->_8 = b8;
    rc->_9 = b9;
    rc->_10 = b10;
    rc->_11 = b11;
    rc->_12 = b12;
    rc->_13 = b13;
    rc->_14 = b14;
    rc->_15 = b15;
    rc->_16 = b16;
    rc->_17 = b17;
    rc->_18 = b18;
    rc->_19 = b19;
    rc->_20 = b20;
    rc->_21 = b21;
    rc->_22 = b22;
    rc->_23 = b23;
    rc->_24 = b24;
    rc->_25 = b25;
    return BSTS_VALUE_FROM_PTR(rc);
}

DEFINE_BSTS_ENUM(Enum27,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;BValue _6;BValue _7;BValue _8;BValue _9;BValue _10;BValue _11;BValue _12;BValue _13;BValue _14;BValue _15;BValue _16;BValue _17;BValue _18;BValue _19;BValue _20;BValue _21;BValue _22;BValue _23;BValue _24;BValue _25;BValue _26;);

BValue alloc_enum27(ENUM_TAG tag, BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5, BValue b6, BValue b7, BValue b8, BValue b9, BValue b10, BValue b11, BValue b12, BValue b13, BValue b14, BValue b15, BValue b16, BValue b17, BValue b18, BValue b19, BValue b20, BValue b21, BValue b22, BValue b23, BValue b24, BValue b25, BValue b26) {
    Enum27* rc = GC_malloc(sizeof(Enum27));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_enum27");
        abort();
    }
    rc->tag = tag;
    rc->_0 = b0;
    rc->_1 = b1;
    rc->_2 = b2;
    rc->_3 = b3;
    rc->_4 = b4;
    rc->_5 = b5;
    rc->_6 = b6;
    rc->_7 = b7;
    rc->_8 = b8;
    rc->_9 = b9;
    rc->_10 = b10;
    rc->_11 = b11;
    rc->_12 = b12;
    rc->_13 = b13;
    rc->_14 = b14;
    rc->_15 = b15;
    rc->_16 = b16;
    rc->_17 = b17;
    rc->_18 = b18;
    rc->_19 = b19;
    rc->_20 = b20;
    rc->_21 = b21;
    rc->_22 = b22;
    rc->_23 = b23;
    rc->_24 = b24;
    rc->_25 = b25;
    rc->_26 = b26;
    return BSTS_VALUE_FROM_PTR(rc);
}

DEFINE_BSTS_ENUM(Enum28,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;BValue _6;BValue _7;BValue _8;BValue _9;BValue _10;BValue _11;BValue _12;BValue _13;BValue _14;BValue _15;BValue _16;BValue _17;BValue _18;BValue _19;BValue _20;BValue _21;BValue _22;BValue _23;BValue _24;BValue _25;BValue _26;BValue _27;);

BValue alloc_enum28(ENUM_TAG tag, BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5, BValue b6, BValue b7, BValue b8, BValue b9, BValue b10, BValue b11, BValue b12, BValue b13, BValue b14, BValue b15, BValue b16, BValue b17, BValue b18, BValue b19, BValue b20, BValue b21, BValue b22, BValue b23, BValue b24, BValue b25, BValue b26, BValue b27) {
    Enum28* rc = GC_malloc(sizeof(Enum28));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_enum28");
        abort();
    }
    rc->tag = tag;
    rc->_0 = b0;
    rc->_1 = b1;
    rc->_2 = b2;
    rc->_3 = b3;
    rc->_4 = b4;
    rc->_5 = b5;
    rc->_6 = b6;
    rc->_7 = b7;
    rc->_8 = b8;
    rc->_9 = b9;
    rc->_10 = b10;
    rc->_11 = b11;
    rc->_12 = b12;
    rc->_13 = b13;
    rc->_14 = b14;
    rc->_15 = b15;
    rc->_16 = b16;
    rc->_17 = b17;
    rc->_18 = b18;
    rc->_19 = b19;
    rc->_20 = b20;
    rc->_21 = b21;
    rc->_22 = b22;
    rc->_23 = b23;
    rc->_24 = b24;
    rc->_25 = b25;
    rc->_26 = b26;
    rc->_27 = b27;
    return BSTS_VALUE_FROM_PTR(rc);
}

DEFINE_BSTS_ENUM(Enum29,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;BValue _6;BValue _7;BValue _8;BValue _9;BValue _10;BValue _11;BValue _12;BValue _13;BValue _14;BValue _15;BValue _16;BValue _17;BValue _18;BValue _19;BValue _20;BValue _21;BValue _22;BValue _23;BValue _24;BValue _25;BValue _26;BValue _27;BValue _28;);

BValue alloc_enum29(ENUM_TAG tag, BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5, BValue b6, BValue b7, BValue b8, BValue b9, BValue b10, BValue b11, BValue b12, BValue b13, BValue b14, BValue b15, BValue b16, BValue b17, BValue b18, BValue b19, BValue b20, BValue b21, BValue b22, BValue b23, BValue b24, BValue b25, BValue b26, BValue b27, BValue b28) {
    Enum29* rc = GC_malloc(sizeof(Enum29));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_enum29");
        abort();
    }
    rc->tag = tag;
    rc->_0 = b0;
    rc->_1 = b1;
    rc->_2 = b2;
    rc->_3 = b3;
    rc->_4 = b4;
    rc->_5 = b5;
    rc->_6 = b6;
    rc->_7 = b7;
    rc->_8 = b8;
    rc->_9 = b9;
    rc->_10 = b10;
    rc->_11 = b11;
    rc->_12 = b12;
    rc->_13 = b13;
    rc->_14 = b14;
    rc->_15 = b15;
    rc->_16 = b16;
    rc->_17 = b17;
    rc->_18 = b18;
    rc->_19 = b19;
    rc->_20 = b20;
    rc->_21 = b21;
    rc->_22 = b22;
    rc->_23 = b23;
    rc->_24 = b24;
    rc->_25 = b25;
    rc->_26 = b26;
    rc->_27 = b27;
    rc->_28 = b28;
    return BSTS_VALUE_FROM_PTR(rc);
}

DEFINE_BSTS_ENUM(Enum30,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;BValue _6;BValue _7;BValue _8;BValue _9;BValue _10;BValue _11;BValue _12;BValue _13;BValue _14;BValue _15;BValue _16;BValue _17;BValue _18;BValue _19;BValue _20;BValue _21;BValue _22;BValue _23;BValue _24;BValue _25;BValue _26;BValue _27;BValue _28;BValue _29;);

BValue alloc_enum30(ENUM_TAG tag, BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5, BValue b6, BValue b7, BValue b8, BValue b9, BValue b10, BValue b11, BValue b12, BValue b13, BValue b14, BValue b15, BValue b16, BValue b17, BValue b18, BValue b19, BValue b20, BValue b21, BValue b22, BValue b23, BValue b24, BValue b25, BValue b26, BValue b27, BValue b28, BValue b29) {
    Enum30* rc = GC_malloc(sizeof(Enum30));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_enum30");
        abort();
    }
    rc->tag = tag;
    rc->_0 = b0;
    rc->_1 = b1;
    rc->_2 = b2;
    rc->_3 = b3;
    rc->_4 = b4;
    rc->_5 = b5;
    rc->_6 = b6;
    rc->_7 = b7;
    rc->_8 = b8;
    rc->_9 = b9;
    rc->_10 = b10;
    rc->_11 = b11;
    rc->_12 = b12;
    rc->_13 = b13;
    rc->_14 = b14;
    rc->_15 = b15;
    rc->_16 = b16;
    rc->_17 = b17;
    rc->_18 = b18;
    rc->_19 = b19;
    rc->_20 = b20;
    rc->_21 = b21;
    rc->_22 = b22;
    rc->_23 = b23;
    rc->_24 = b24;
    rc->_25 = b25;
    rc->_26 = b26;
    rc->_27 = b27;
    rc->_28 = b28;
    rc->_29 = b29;
    return BSTS_VALUE_FROM_PTR(rc);
}

DEFINE_BSTS_ENUM(Enum31,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;BValue _6;BValue _7;BValue _8;BValue _9;BValue _10;BValue _11;BValue _12;BValue _13;BValue _14;BValue _15;BValue _16;BValue _17;BValue _18;BValue _19;BValue _20;BValue _21;BValue _22;BValue _23;BValue _24;BValue _25;BValue _26;BValue _27;BValue _28;BValue _29;BValue _30;);

BValue alloc_enum31(ENUM_TAG tag, BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5, BValue b6, BValue b7, BValue b8, BValue b9, BValue b10, BValue b11, BValue b12, BValue b13, BValue b14, BValue b15, BValue b16, BValue b17, BValue b18, BValue b19, BValue b20, BValue b21, BValue b22, BValue b23, BValue b24, BValue b25, BValue b26, BValue b27, BValue b28, BValue b29, BValue b30) {
    Enum31* rc = GC_malloc(sizeof(Enum31));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_enum31");
        abort();
    }
    rc->tag = tag;
    rc->_0 = b0;
    rc->_1 = b1;
    rc->_2 = b2;
    rc->_3 = b3;
    rc->_4 = b4;
    rc->_5 = b5;
    rc->_6 = b6;
    rc->_7 = b7;
    rc->_8 = b8;
    rc->_9 = b9;
    rc->_10 = b10;
    rc->_11 = b11;
    rc->_12 = b12;
    rc->_13 = b13;
    rc->_14 = b14;
    rc->_15 = b15;
    rc->_16 = b16;
    rc->_17 = b17;
    rc->_18 = b18;
    rc->_19 = b19;
    rc->_20 = b20;
    rc->_21 = b21;
    rc->_22 = b22;
    rc->_23 = b23;
    rc->_24 = b24;
    rc->_25 = b25;
    rc->_26 = b26;
    rc->_27 = b27;
    rc->_28 = b28;
    rc->_29 = b29;
    rc->_30 = b30;
    return BSTS_VALUE_FROM_PTR(rc);
}

DEFINE_BSTS_ENUM(Enum32,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;BValue _6;BValue _7;BValue _8;BValue _9;BValue _10;BValue _11;BValue _12;BValue _13;BValue _14;BValue _15;BValue _16;BValue _17;BValue _18;BValue _19;BValue _20;BValue _21;BValue _22;BValue _23;BValue _24;BValue _25;BValue _26;BValue _27;BValue _28;BValue _29;BValue _30;BValue _31;);

BValue alloc_enum32(ENUM_TAG tag, BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5, BValue b6, BValue b7, BValue b8, BValue b9, BValue b10, BValue b11, BValue b12, BValue b13, BValue b14, BValue b15, BValue b16, BValue b17, BValue b18, BValue b19, BValue b20, BValue b21, BValue b22, BValue b23, BValue b24, BValue b25, BValue b26, BValue b27, BValue b28, BValue b29, BValue b30, BValue b31) {
    Enum32* rc = GC_malloc(sizeof(Enum32));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_enum32");
        abort();
    }
    rc->tag = tag;
    rc->_0 = b0;
    rc->_1 = b1;
    rc->_2 = b2;
    rc->_3 = b3;
    rc->_4 = b4;
    rc->_5 = b5;
    rc->_6 = b6;
    rc->_7 = b7;
    rc->_8 = b8;
    rc->_9 = b9;
    rc->_10 = b10;
    rc->_11 = b11;
    rc->_12 = b12;
    rc->_13 = b13;
    rc->_14 = b14;
    rc->_15 = b15;
    rc->_16 = b16;
    rc->_17 = b17;
    rc->_18 = b18;
    rc->_19 = b19;
    rc->_20 = b20;
    rc->_21 = b21;
    rc->_22 = b22;
    rc->_23 = b23;
    rc->_24 = b24;
    rc->_25 = b25;
    rc->_26 = b26;
    rc->_27 = b27;
    rc->_28 = b28;
    rc->_29 = b29;
    rc->_30 = b30;
    rc->_31 = b31;
    return BSTS_VALUE_FROM_PTR(rc);
}

// FUNCTIONS

DEFINE_BSTS_OBJ(BoxedPureFn1, BPureFn1 fn; size_t slot_len;);

BValue alloc_closure1(size_t size, BValue* data, BClosure1 fn) {
    Closure1Data* rc = GC_malloc(closure_data_size(size));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_closure1");
        abort();
    }
    rc->fn = fn;
    rc->slot_len = size;
    BValue* closure_data = closure_data_of(rc);
    memcpy(closure_data, data, sizeof(BValue) * size);
    return BSTS_VALUE_FROM_PTR(rc);
}

BValue alloc_boxed_pure_fn1(BPureFn1 fn) {
    uintptr_t fn_int = (uintptr_t)fn;
    uintptr_t small_mask = UINTPTR_MAX >> 2;
    if (fn_int <= small_mask) {
      // can pack into a pure value
      return TO_PURE_VALUE(fn_int);
    }
    BoxedPureFn1* rc = GC_malloc(sizeof(BoxedPureFn1));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_boxed_pure_fn1");
        abort();
    }
    rc->fn = fn;
    rc->slot_len = 0;
    return BSTS_VALUE_FROM_PTR(rc);
}

BValue call_fn1(BValue fn, BValue arg0) {
  if (IS_PURE_VALUE(fn)) {
    // can pack into a pure value
    BPureFn1 pure = (BPureFn1)(uintptr_t)PURE_VALUE(fn);
    return pure(arg0);
  }
  BoxedPureFn1* purefn = BSTS_PTR(BoxedPureFn1, fn);
  if (purefn->slot_len == 0) {
    return purefn->fn(arg0);
  }
  else {
    // this must be a closure:
    Closure1Data* rc = BSTS_PTR(Closure1Data, fn);
    BValue* data = closure_data_of(rc);
    return rc->fn(data, arg0);
  }
}


DEFINE_BSTS_OBJ(Closure2Data, BClosure2 fn; size_t slot_len;);
DEFINE_BSTS_OBJ(BoxedPureFn2, BPureFn2 fn; size_t slot_len;);

BValue alloc_closure2(size_t size, BValue* data, BClosure2 fn) {
    Closure2Data* rc = GC_malloc(closure_data_size(size));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_closure2");
        abort();
    }
    rc->fn = fn;
    rc->slot_len = size;
    BValue* closure_data = closure_data_of((Closure1Data*)rc);
    memcpy(closure_data, data, sizeof(BValue) * size);
    return BSTS_VALUE_FROM_PTR(rc);
}

BValue alloc_boxed_pure_fn2(BPureFn2 fn) {
    uintptr_t fn_int = (uintptr_t)fn;
    uintptr_t small_mask = UINTPTR_MAX >> 2;
    if (fn_int <= small_mask) {
      // can pack into a pure value
      return TO_PURE_VALUE(fn_int);
    }
    BoxedPureFn2* rc = GC_malloc(sizeof(BoxedPureFn2));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_boxed_pure_fn2");
        abort();
    }
    rc->fn = fn;
    rc->slot_len = 0;
    return BSTS_VALUE_FROM_PTR(rc);
}

BValue call_fn2(BValue fn, BValue arg0, BValue arg1) {
  if (IS_PURE_VALUE(fn)) {
    // can pack into a pure value
    BPureFn2 pure = (BPureFn2)(uintptr_t)PURE_VALUE(fn);
    return pure(arg0, arg1);
  }
  BoxedPureFn2* purefn = BSTS_PTR(BoxedPureFn2, fn);
  if (purefn->slot_len == 0) {
    return purefn->fn(arg0, arg1);
  }
  else {
    // this must be a closure:
    Closure2Data* rc = BSTS_PTR(Closure2Data, fn);
    BValue* data = closure_data_of((Closure1Data*)rc);
    return rc->fn(data, arg0, arg1);
  }
}


DEFINE_BSTS_OBJ(Closure3Data, BClosure3 fn; size_t slot_len;);
DEFINE_BSTS_OBJ(BoxedPureFn3, BPureFn3 fn; size_t slot_len;);

BValue alloc_closure3(size_t size, BValue* data, BClosure3 fn) {
    Closure3Data* rc = GC_malloc(closure_data_size(size));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_closure3");
        abort();
    }
    rc->fn = fn;
    rc->slot_len = size;
    BValue* closure_data = closure_data_of((Closure1Data*)rc);
    memcpy(closure_data, data, sizeof(BValue) * size);
    return BSTS_VALUE_FROM_PTR(rc);
}

BValue alloc_boxed_pure_fn3(BPureFn3 fn) {
    uintptr_t fn_int = (uintptr_t)fn;
    uintptr_t small_mask = UINTPTR_MAX >> 2;
    if (fn_int <= small_mask) {
      // can pack into a pure value
      return TO_PURE_VALUE(fn_int);
    }
    BoxedPureFn3* rc = GC_malloc(sizeof(BoxedPureFn3));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_boxed_pure_fn3");
        abort();
    }
    rc->fn = fn;
    rc->slot_len = 0;
    return BSTS_VALUE_FROM_PTR(rc);
}

BValue call_fn3(BValue fn, BValue arg0, BValue arg1, BValue arg2) {
  if (IS_PURE_VALUE(fn)) {
    // can pack into a pure value
    BPureFn3 pure = (BPureFn3)(uintptr_t)PURE_VALUE(fn);
    return pure(arg0, arg1, arg2);
  }
  BoxedPureFn3* purefn = BSTS_PTR(BoxedPureFn3, fn);
  if (purefn->slot_len == 0) {
    return purefn->fn(arg0, arg1, arg2);
  }
  else {
    // this must be a closure:
    Closure3Data* rc = BSTS_PTR(Closure3Data, fn);
    BValue* data = closure_data_of((Closure1Data*)rc);
    return rc->fn(data, arg0, arg1, arg2);
  }
}


DEFINE_BSTS_OBJ(Closure4Data, BClosure4 fn; size_t slot_len;);
DEFINE_BSTS_OBJ(BoxedPureFn4, BPureFn4 fn; size_t slot_len;);

BValue alloc_closure4(size_t size, BValue* data, BClosure4 fn) {
    Closure4Data* rc = GC_malloc(closure_data_size(size));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_closure4");
        abort();
    }
    rc->fn = fn;
    rc->slot_len = size;
    BValue* closure_data = closure_data_of((Closure1Data*)rc);
    memcpy(closure_data, data, sizeof(BValue) * size);
    return BSTS_VALUE_FROM_PTR(rc);
}

BValue alloc_boxed_pure_fn4(BPureFn4 fn) {
    uintptr_t fn_int = (uintptr_t)fn;
    uintptr_t small_mask = UINTPTR_MAX >> 2;
    if (fn_int <= small_mask) {
      // can pack into a pure value
      return TO_PURE_VALUE(fn_int);
    }
    BoxedPureFn4* rc = GC_malloc(sizeof(BoxedPureFn4));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_boxed_pure_fn4");
        abort();
    }
    rc->fn = fn;
    rc->slot_len = 0;
    return BSTS_VALUE_FROM_PTR(rc);
}

BValue call_fn4(BValue fn, BValue arg0, BValue arg1, BValue arg2, BValue arg3) {
  if (IS_PURE_VALUE(fn)) {
    // can pack into a pure value
    BPureFn4 pure = (BPureFn4)(uintptr_t)PURE_VALUE(fn);
    return pure(arg0, arg1, arg2, arg3);
  }
  BoxedPureFn4* purefn = BSTS_PTR(BoxedPureFn4, fn);
  if (purefn->slot_len == 0) {
    return purefn->fn(arg0, arg1, arg2, arg3);
  }
  else {
    // this must be a closure:
    Closure4Data* rc = BSTS_PTR(Closure4Data, fn);
    BValue* data = closure_data_of((Closure1Data*)rc);
    return rc->fn(data, arg0, arg1, arg2, arg3);
  }
}


DEFINE_BSTS_OBJ(Closure5Data, BClosure5 fn; size_t slot_len;);
DEFINE_BSTS_OBJ(BoxedPureFn5, BPureFn5 fn; size_t slot_len;);

BValue alloc_closure5(size_t size, BValue* data, BClosure5 fn) {
    Closure5Data* rc = GC_malloc(closure_data_size(size));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_closure5");
        abort();
    }
    rc->fn = fn;
    rc->slot_len = size;
    BValue* closure_data = closure_data_of((Closure1Data*)rc);
    memcpy(closure_data, data, sizeof(BValue) * size);
    return BSTS_VALUE_FROM_PTR(rc);
}

BValue alloc_boxed_pure_fn5(BPureFn5 fn) {
    uintptr_t fn_int = (uintptr_t)fn;
    uintptr_t small_mask = UINTPTR_MAX >> 2;
    if (fn_int <= small_mask) {
      // can pack into a pure value
      return TO_PURE_VALUE(fn_int);
    }
    BoxedPureFn5* rc = GC_malloc(sizeof(BoxedPureFn5));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_boxed_pure_fn5");
        abort();
    }
    rc->fn = fn;
    rc->slot_len = 0;
    return BSTS_VALUE_FROM_PTR(rc);
}

BValue call_fn5(BValue fn, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4) {
  if (IS_PURE_VALUE(fn)) {
    // can pack into a pure value
    BPureFn5 pure = (BPureFn5)(uintptr_t)PURE_VALUE(fn);
    return pure(arg0, arg1, arg2, arg3, arg4);
  }
  BoxedPureFn5* purefn = BSTS_PTR(BoxedPureFn5, fn);
  if (purefn->slot_len == 0) {
    return purefn->fn(arg0, arg1, arg2, arg3, arg4);
  }
  else {
    // this must be a closure:
    Closure5Data* rc = BSTS_PTR(Closure5Data, fn);
    BValue* data = closure_data_of((Closure1Data*)rc);
    return rc->fn(data, arg0, arg1, arg2, arg3, arg4);
  }
}


DEFINE_BSTS_OBJ(Closure6Data, BClosure6 fn; size_t slot_len;);
DEFINE_BSTS_OBJ(BoxedPureFn6, BPureFn6 fn; size_t slot_len;);

BValue alloc_closure6(size_t size, BValue* data, BClosure6 fn) {
    Closure6Data* rc = GC_malloc(closure_data_size(size));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_closure6");
        abort();
    }
    rc->fn = fn;
    rc->slot_len = size;
    BValue* closure_data = closure_data_of((Closure1Data*)rc);
    memcpy(closure_data, data, sizeof(BValue) * size);
    return BSTS_VALUE_FROM_PTR(rc);
}

BValue alloc_boxed_pure_fn6(BPureFn6 fn) {
    uintptr_t fn_int = (uintptr_t)fn;
    uintptr_t small_mask = UINTPTR_MAX >> 2;
    if (fn_int <= small_mask) {
      // can pack into a pure value
      return TO_PURE_VALUE(fn_int);
    }
    BoxedPureFn6* rc = GC_malloc(sizeof(BoxedPureFn6));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_boxed_pure_fn6");
        abort();
    }
    rc->fn = fn;
    rc->slot_len = 0;
    return BSTS_VALUE_FROM_PTR(rc);
}

BValue call_fn6(BValue fn, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5) {
  if (IS_PURE_VALUE(fn)) {
    // can pack into a pure value
    BPureFn6 pure = (BPureFn6)(uintptr_t)PURE_VALUE(fn);
    return pure(arg0, arg1, arg2, arg3, arg4, arg5);
  }
  BoxedPureFn6* purefn = BSTS_PTR(BoxedPureFn6, fn);
  if (purefn->slot_len == 0) {
    return purefn->fn(arg0, arg1, arg2, arg3, arg4, arg5);
  }
  else {
    // this must be a closure:
    Closure6Data* rc = BSTS_PTR(Closure6Data, fn);
    BValue* data = closure_data_of((Closure1Data*)rc);
    return rc->fn(data, arg0, arg1, arg2, arg3, arg4, arg5);
  }
}


DEFINE_BSTS_OBJ(Closure7Data, BClosure7 fn; size_t slot_len;);
DEFINE_BSTS_OBJ(BoxedPureFn7, BPureFn7 fn; size_t slot_len;);

BValue alloc_closure7(size_t size, BValue* data, BClosure7 fn) {
    Closure7Data* rc = GC_malloc(closure_data_size(size));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_closure7");
        abort();
    }
    rc->fn = fn;
    rc->slot_len = size;
    BValue* closure_data = closure_data_of((Closure1Data*)rc);
    memcpy(closure_data, data, sizeof(BValue) * size);
    return BSTS_VALUE_FROM_PTR(rc);
}

BValue alloc_boxed_pure_fn7(BPureFn7 fn) {
    uintptr_t fn_int = (uintptr_t)fn;
    uintptr_t small_mask = UINTPTR_MAX >> 2;
    if (fn_int <= small_mask) {
      // can pack into a pure value
      return TO_PURE_VALUE(fn_int);
    }
    BoxedPureFn7* rc = GC_malloc(sizeof(BoxedPureFn7));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_boxed_pure_fn7");
        abort();
    }
    rc->fn = fn;
    rc->slot_len = 0;
    return BSTS_VALUE_FROM_PTR(rc);
}

BValue call_fn7(BValue fn, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6) {
  if (IS_PURE_VALUE(fn)) {
    // can pack into a pure value
    BPureFn7 pure = (BPureFn7)(uintptr_t)PURE_VALUE(fn);
    return pure(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
  }
  BoxedPureFn7* purefn = BSTS_PTR(BoxedPureFn7, fn);
  if (purefn->slot_len == 0) {
    return purefn->fn(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
  }
  else {
    // this must be a closure:
    Closure7Data* rc = BSTS_PTR(Closure7Data, fn);
    BValue* data = closure_data_of((Closure1Data*)rc);
    return rc->fn(data, arg0, arg1, arg2, arg3, arg4, arg5, arg6);
  }
}


DEFINE_BSTS_OBJ(Closure8Data, BClosure8 fn; size_t slot_len;);
DEFINE_BSTS_OBJ(BoxedPureFn8, BPureFn8 fn; size_t slot_len;);

BValue alloc_closure8(size_t size, BValue* data, BClosure8 fn) {
    Closure8Data* rc = GC_malloc(closure_data_size(size));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_closure8");
        abort();
    }
    rc->fn = fn;
    rc->slot_len = size;
    BValue* closure_data = closure_data_of((Closure1Data*)rc);
    memcpy(closure_data, data, sizeof(BValue) * size);
    return BSTS_VALUE_FROM_PTR(rc);
}

BValue alloc_boxed_pure_fn8(BPureFn8 fn) {
    uintptr_t fn_int = (uintptr_t)fn;
    uintptr_t small_mask = UINTPTR_MAX >> 2;
    if (fn_int <= small_mask) {
      // can pack into a pure value
      return TO_PURE_VALUE(fn_int);
    }
    BoxedPureFn8* rc = GC_malloc(sizeof(BoxedPureFn8));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_boxed_pure_fn8");
        abort();
    }
    rc->fn = fn;
    rc->slot_len = 0;
    return BSTS_VALUE_FROM_PTR(rc);
}

BValue call_fn8(BValue fn, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7) {
  if (IS_PURE_VALUE(fn)) {
    // can pack into a pure value
    BPureFn8 pure = (BPureFn8)(uintptr_t)PURE_VALUE(fn);
    return pure(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7);
  }
  BoxedPureFn8* purefn = BSTS_PTR(BoxedPureFn8, fn);
  if (purefn->slot_len == 0) {
    return purefn->fn(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7);
  }
  else {
    // this must be a closure:
    Closure8Data* rc = BSTS_PTR(Closure8Data, fn);
    BValue* data = closure_data_of((Closure1Data*)rc);
    return rc->fn(data, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7);
  }
}


DEFINE_BSTS_OBJ(Closure9Data, BClosure9 fn; size_t slot_len;);
DEFINE_BSTS_OBJ(BoxedPureFn9, BPureFn9 fn; size_t slot_len;);

BValue alloc_closure9(size_t size, BValue* data, BClosure9 fn) {
    Closure9Data* rc = GC_malloc(closure_data_size(size));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_closure9");
        abort();
    }
    rc->fn = fn;
    rc->slot_len = size;
    BValue* closure_data = closure_data_of((Closure1Data*)rc);
    memcpy(closure_data, data, sizeof(BValue) * size);
    return BSTS_VALUE_FROM_PTR(rc);
}

BValue alloc_boxed_pure_fn9(BPureFn9 fn) {
    uintptr_t fn_int = (uintptr_t)fn;
    uintptr_t small_mask = UINTPTR_MAX >> 2;
    if (fn_int <= small_mask) {
      // can pack into a pure value
      return TO_PURE_VALUE(fn_int);
    }
    BoxedPureFn9* rc = GC_malloc(sizeof(BoxedPureFn9));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_boxed_pure_fn9");
        abort();
    }
    rc->fn = fn;
    rc->slot_len = 0;
    return BSTS_VALUE_FROM_PTR(rc);
}

BValue call_fn9(BValue fn, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8) {
  if (IS_PURE_VALUE(fn)) {
    // can pack into a pure value
    BPureFn9 pure = (BPureFn9)(uintptr_t)PURE_VALUE(fn);
    return pure(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
  }
  BoxedPureFn9* purefn = BSTS_PTR(BoxedPureFn9, fn);
  if (purefn->slot_len == 0) {
    return purefn->fn(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
  }
  else {
    // this must be a closure:
    Closure9Data* rc = BSTS_PTR(Closure9Data, fn);
    BValue* data = closure_data_of((Closure1Data*)rc);
    return rc->fn(data, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
  }
}


DEFINE_BSTS_OBJ(Closure10Data, BClosure10 fn; size_t slot_len;);
DEFINE_BSTS_OBJ(BoxedPureFn10, BPureFn10 fn; size_t slot_len;);

BValue alloc_closure10(size_t size, BValue* data, BClosure10 fn) {
    Closure10Data* rc = GC_malloc(closure_data_size(size));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_closure10");
        abort();
    }
    rc->fn = fn;
    rc->slot_len = size;
    BValue* closure_data = closure_data_of((Closure1Data*)rc);
    memcpy(closure_data, data, sizeof(BValue) * size);
    return BSTS_VALUE_FROM_PTR(rc);
}

BValue alloc_boxed_pure_fn10(BPureFn10 fn) {
    uintptr_t fn_int = (uintptr_t)fn;
    uintptr_t small_mask = UINTPTR_MAX >> 2;
    if (fn_int <= small_mask) {
      // can pack into a pure value
      return TO_PURE_VALUE(fn_int);
    }
    BoxedPureFn10* rc = GC_malloc(sizeof(BoxedPureFn10));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_boxed_pure_fn10");
        abort();
    }
    rc->fn = fn;
    rc->slot_len = 0;
    return BSTS_VALUE_FROM_PTR(rc);
}

BValue call_fn10(BValue fn, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9) {
  if (IS_PURE_VALUE(fn)) {
    // can pack into a pure value
    BPureFn10 pure = (BPureFn10)(uintptr_t)PURE_VALUE(fn);
    return pure(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
  }
  BoxedPureFn10* purefn = BSTS_PTR(BoxedPureFn10, fn);
  if (purefn->slot_len == 0) {
    return purefn->fn(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
  }
  else {
    // this must be a closure:
    Closure10Data* rc = BSTS_PTR(Closure10Data, fn);
    BValue* data = closure_data_of((Closure1Data*)rc);
    return rc->fn(data, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
  }
}


DEFINE_BSTS_OBJ(Closure11Data, BClosure11 fn; size_t slot_len;);
DEFINE_BSTS_OBJ(BoxedPureFn11, BPureFn11 fn; size_t slot_len;);

BValue alloc_closure11(size_t size, BValue* data, BClosure11 fn) {
    Closure11Data* rc = GC_malloc(closure_data_size(size));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_closure11");
        abort();
    }
    rc->fn = fn;
    rc->slot_len = size;
    BValue* closure_data = closure_data_of((Closure1Data*)rc);
    memcpy(closure_data, data, sizeof(BValue) * size);
    return BSTS_VALUE_FROM_PTR(rc);
}

BValue alloc_boxed_pure_fn11(BPureFn11 fn) {
    uintptr_t fn_int = (uintptr_t)fn;
    uintptr_t small_mask = UINTPTR_MAX >> 2;
    if (fn_int <= small_mask) {
      // can pack into a pure value
      return TO_PURE_VALUE(fn_int);
    }
    BoxedPureFn11* rc = GC_malloc(sizeof(BoxedPureFn11));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_boxed_pure_fn11");
        abort();
    }
    rc->fn = fn;
    rc->slot_len = 0;
    return BSTS_VALUE_FROM_PTR(rc);
}

BValue call_fn11(BValue fn, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10) {
  if (IS_PURE_VALUE(fn)) {
    // can pack into a pure value
    BPureFn11 pure = (BPureFn11)(uintptr_t)PURE_VALUE(fn);
    return pure(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10);
  }
  BoxedPureFn11* purefn = BSTS_PTR(BoxedPureFn11, fn);
  if (purefn->slot_len == 0) {
    return purefn->fn(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10);
  }
  else {
    // this must be a closure:
    Closure11Data* rc = BSTS_PTR(Closure11Data, fn);
    BValue* data = closure_data_of((Closure1Data*)rc);
    return rc->fn(data, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10);
  }
}


DEFINE_BSTS_OBJ(Closure12Data, BClosure12 fn; size_t slot_len;);
DEFINE_BSTS_OBJ(BoxedPureFn12, BPureFn12 fn; size_t slot_len;);

BValue alloc_closure12(size_t size, BValue* data, BClosure12 fn) {
    Closure12Data* rc = GC_malloc(closure_data_size(size));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_closure12");
        abort();
    }
    rc->fn = fn;
    rc->slot_len = size;
    BValue* closure_data = closure_data_of((Closure1Data*)rc);
    memcpy(closure_data, data, sizeof(BValue) * size);
    return BSTS_VALUE_FROM_PTR(rc);
}

BValue alloc_boxed_pure_fn12(BPureFn12 fn) {
    uintptr_t fn_int = (uintptr_t)fn;
    uintptr_t small_mask = UINTPTR_MAX >> 2;
    if (fn_int <= small_mask) {
      // can pack into a pure value
      return TO_PURE_VALUE(fn_int);
    }
    BoxedPureFn12* rc = GC_malloc(sizeof(BoxedPureFn12));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_boxed_pure_fn12");
        abort();
    }
    rc->fn = fn;
    rc->slot_len = 0;
    return BSTS_VALUE_FROM_PTR(rc);
}

BValue call_fn12(BValue fn, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11) {
  if (IS_PURE_VALUE(fn)) {
    // can pack into a pure value
    BPureFn12 pure = (BPureFn12)(uintptr_t)PURE_VALUE(fn);
    return pure(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11);
  }
  BoxedPureFn12* purefn = BSTS_PTR(BoxedPureFn12, fn);
  if (purefn->slot_len == 0) {
    return purefn->fn(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11);
  }
  else {
    // this must be a closure:
    Closure12Data* rc = BSTS_PTR(Closure12Data, fn);
    BValue* data = closure_data_of((Closure1Data*)rc);
    return rc->fn(data, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11);
  }
}


DEFINE_BSTS_OBJ(Closure13Data, BClosure13 fn; size_t slot_len;);
DEFINE_BSTS_OBJ(BoxedPureFn13, BPureFn13 fn; size_t slot_len;);

BValue alloc_closure13(size_t size, BValue* data, BClosure13 fn) {
    Closure13Data* rc = GC_malloc(closure_data_size(size));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_closure13");
        abort();
    }
    rc->fn = fn;
    rc->slot_len = size;
    BValue* closure_data = closure_data_of((Closure1Data*)rc);
    memcpy(closure_data, data, sizeof(BValue) * size);
    return BSTS_VALUE_FROM_PTR(rc);
}

BValue alloc_boxed_pure_fn13(BPureFn13 fn) {
    uintptr_t fn_int = (uintptr_t)fn;
    uintptr_t small_mask = UINTPTR_MAX >> 2;
    if (fn_int <= small_mask) {
      // can pack into a pure value
      return TO_PURE_VALUE(fn_int);
    }
    BoxedPureFn13* rc = GC_malloc(sizeof(BoxedPureFn13));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_boxed_pure_fn13");
        abort();
    }
    rc->fn = fn;
    rc->slot_len = 0;
    return BSTS_VALUE_FROM_PTR(rc);
}

BValue call_fn13(BValue fn, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11, BValue arg12) {
  if (IS_PURE_VALUE(fn)) {
    // can pack into a pure value
    BPureFn13 pure = (BPureFn13)(uintptr_t)PURE_VALUE(fn);
    return pure(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12);
  }
  BoxedPureFn13* purefn = BSTS_PTR(BoxedPureFn13, fn);
  if (purefn->slot_len == 0) {
    return purefn->fn(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12);
  }
  else {
    // this must be a closure:
    Closure13Data* rc = BSTS_PTR(Closure13Data, fn);
    BValue* data = closure_data_of((Closure1Data*)rc);
    return rc->fn(data, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12);
  }
}


DEFINE_BSTS_OBJ(Closure14Data, BClosure14 fn; size_t slot_len;);
DEFINE_BSTS_OBJ(BoxedPureFn14, BPureFn14 fn; size_t slot_len;);

BValue alloc_closure14(size_t size, BValue* data, BClosure14 fn) {
    Closure14Data* rc = GC_malloc(closure_data_size(size));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_closure14");
        abort();
    }
    rc->fn = fn;
    rc->slot_len = size;
    BValue* closure_data = closure_data_of((Closure1Data*)rc);
    memcpy(closure_data, data, sizeof(BValue) * size);
    return BSTS_VALUE_FROM_PTR(rc);
}

BValue alloc_boxed_pure_fn14(BPureFn14 fn) {
    uintptr_t fn_int = (uintptr_t)fn;
    uintptr_t small_mask = UINTPTR_MAX >> 2;
    if (fn_int <= small_mask) {
      // can pack into a pure value
      return TO_PURE_VALUE(fn_int);
    }
    BoxedPureFn14* rc = GC_malloc(sizeof(BoxedPureFn14));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_boxed_pure_fn14");
        abort();
    }
    rc->fn = fn;
    rc->slot_len = 0;
    return BSTS_VALUE_FROM_PTR(rc);
}

BValue call_fn14(BValue fn, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11, BValue arg12, BValue arg13) {
  if (IS_PURE_VALUE(fn)) {
    // can pack into a pure value
    BPureFn14 pure = (BPureFn14)(uintptr_t)PURE_VALUE(fn);
    return pure(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13);
  }
  BoxedPureFn14* purefn = BSTS_PTR(BoxedPureFn14, fn);
  if (purefn->slot_len == 0) {
    return purefn->fn(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13);
  }
  else {
    // this must be a closure:
    Closure14Data* rc = BSTS_PTR(Closure14Data, fn);
    BValue* data = closure_data_of((Closure1Data*)rc);
    return rc->fn(data, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13);
  }
}


DEFINE_BSTS_OBJ(Closure15Data, BClosure15 fn; size_t slot_len;);
DEFINE_BSTS_OBJ(BoxedPureFn15, BPureFn15 fn; size_t slot_len;);

BValue alloc_closure15(size_t size, BValue* data, BClosure15 fn) {
    Closure15Data* rc = GC_malloc(closure_data_size(size));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_closure15");
        abort();
    }
    rc->fn = fn;
    rc->slot_len = size;
    BValue* closure_data = closure_data_of((Closure1Data*)rc);
    memcpy(closure_data, data, sizeof(BValue) * size);
    return BSTS_VALUE_FROM_PTR(rc);
}

BValue alloc_boxed_pure_fn15(BPureFn15 fn) {
    uintptr_t fn_int = (uintptr_t)fn;
    uintptr_t small_mask = UINTPTR_MAX >> 2;
    if (fn_int <= small_mask) {
      // can pack into a pure value
      return TO_PURE_VALUE(fn_int);
    }
    BoxedPureFn15* rc = GC_malloc(sizeof(BoxedPureFn15));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_boxed_pure_fn15");
        abort();
    }
    rc->fn = fn;
    rc->slot_len = 0;
    return BSTS_VALUE_FROM_PTR(rc);
}

BValue call_fn15(BValue fn, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11, BValue arg12, BValue arg13, BValue arg14) {
  if (IS_PURE_VALUE(fn)) {
    // can pack into a pure value
    BPureFn15 pure = (BPureFn15)(uintptr_t)PURE_VALUE(fn);
    return pure(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14);
  }
  BoxedPureFn15* purefn = BSTS_PTR(BoxedPureFn15, fn);
  if (purefn->slot_len == 0) {
    return purefn->fn(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14);
  }
  else {
    // this must be a closure:
    Closure15Data* rc = BSTS_PTR(Closure15Data, fn);
    BValue* data = closure_data_of((Closure1Data*)rc);
    return rc->fn(data, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14);
  }
}


DEFINE_BSTS_OBJ(Closure16Data, BClosure16 fn; size_t slot_len;);
DEFINE_BSTS_OBJ(BoxedPureFn16, BPureFn16 fn; size_t slot_len;);

BValue alloc_closure16(size_t size, BValue* data, BClosure16 fn) {
    Closure16Data* rc = GC_malloc(closure_data_size(size));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_closure16");
        abort();
    }
    rc->fn = fn;
    rc->slot_len = size;
    BValue* closure_data = closure_data_of((Closure1Data*)rc);
    memcpy(closure_data, data, sizeof(BValue) * size);
    return BSTS_VALUE_FROM_PTR(rc);
}

BValue alloc_boxed_pure_fn16(BPureFn16 fn) {
    uintptr_t fn_int = (uintptr_t)fn;
    uintptr_t small_mask = UINTPTR_MAX >> 2;
    if (fn_int <= small_mask) {
      // can pack into a pure value
      return TO_PURE_VALUE(fn_int);
    }
    BoxedPureFn16* rc = GC_malloc(sizeof(BoxedPureFn16));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_boxed_pure_fn16");
        abort();
    }
    rc->fn = fn;
    rc->slot_len = 0;
    return BSTS_VALUE_FROM_PTR(rc);
}

BValue call_fn16(BValue fn, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11, BValue arg12, BValue arg13, BValue arg14, BValue arg15) {
  if (IS_PURE_VALUE(fn)) {
    // can pack into a pure value
    BPureFn16 pure = (BPureFn16)(uintptr_t)PURE_VALUE(fn);
    return pure(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15);
  }
  BoxedPureFn16* purefn = BSTS_PTR(BoxedPureFn16, fn);
  if (purefn->slot_len == 0) {
    return purefn->fn(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15);
  }
  else {
    // this must be a closure:
    Closure16Data* rc = BSTS_PTR(Closure16Data, fn);
    BValue* data = closure_data_of((Closure1Data*)rc);
    return rc->fn(data, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15);
  }
}


DEFINE_BSTS_OBJ(Closure17Data, BClosure17 fn; size_t slot_len;);
DEFINE_BSTS_OBJ(BoxedPureFn17, BPureFn17 fn; size_t slot_len;);

BValue alloc_closure17(size_t size, BValue* data, BClosure17 fn) {
    Closure17Data* rc = GC_malloc(closure_data_size(size));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_closure17");
        abort();
    }
    rc->fn = fn;
    rc->slot_len = size;
    BValue* closure_data = closure_data_of((Closure1Data*)rc);
    memcpy(closure_data, data, sizeof(BValue) * size);
    return BSTS_VALUE_FROM_PTR(rc);
}

BValue alloc_boxed_pure_fn17(BPureFn17 fn) {
    uintptr_t fn_int = (uintptr_t)fn;
    uintptr_t small_mask = UINTPTR_MAX >> 2;
    if (fn_int <= small_mask) {
      // can pack into a pure value
      return TO_PURE_VALUE(fn_int);
    }
    BoxedPureFn17* rc = GC_malloc(sizeof(BoxedPureFn17));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_boxed_pure_fn17");
        abort();
    }
    rc->fn = fn;
    rc->slot_len = 0;
    return BSTS_VALUE_FROM_PTR(rc);
}

BValue call_fn17(BValue fn, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11, BValue arg12, BValue arg13, BValue arg14, BValue arg15, BValue arg16) {
  if (IS_PURE_VALUE(fn)) {
    // can pack into a pure value
    BPureFn17 pure = (BPureFn17)(uintptr_t)PURE_VALUE(fn);
    return pure(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16);
  }
  BoxedPureFn17* purefn = BSTS_PTR(BoxedPureFn17, fn);
  if (purefn->slot_len == 0) {
    return purefn->fn(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16);
  }
  else {
    // this must be a closure:
    Closure17Data* rc = BSTS_PTR(Closure17Data, fn);
    BValue* data = closure_data_of((Closure1Data*)rc);
    return rc->fn(data, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16);
  }
}


DEFINE_BSTS_OBJ(Closure18Data, BClosure18 fn; size_t slot_len;);
DEFINE_BSTS_OBJ(BoxedPureFn18, BPureFn18 fn; size_t slot_len;);

BValue alloc_closure18(size_t size, BValue* data, BClosure18 fn) {
    Closure18Data* rc = GC_malloc(closure_data_size(size));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_closure18");
        abort();
    }
    rc->fn = fn;
    rc->slot_len = size;
    BValue* closure_data = closure_data_of((Closure1Data*)rc);
    memcpy(closure_data, data, sizeof(BValue) * size);
    return BSTS_VALUE_FROM_PTR(rc);
}

BValue alloc_boxed_pure_fn18(BPureFn18 fn) {
    uintptr_t fn_int = (uintptr_t)fn;
    uintptr_t small_mask = UINTPTR_MAX >> 2;
    if (fn_int <= small_mask) {
      // can pack into a pure value
      return TO_PURE_VALUE(fn_int);
    }
    BoxedPureFn18* rc = GC_malloc(sizeof(BoxedPureFn18));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_boxed_pure_fn18");
        abort();
    }
    rc->fn = fn;
    rc->slot_len = 0;
    return BSTS_VALUE_FROM_PTR(rc);
}

BValue call_fn18(BValue fn, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11, BValue arg12, BValue arg13, BValue arg14, BValue arg15, BValue arg16, BValue arg17) {
  if (IS_PURE_VALUE(fn)) {
    // can pack into a pure value
    BPureFn18 pure = (BPureFn18)(uintptr_t)PURE_VALUE(fn);
    return pure(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17);
  }
  BoxedPureFn18* purefn = BSTS_PTR(BoxedPureFn18, fn);
  if (purefn->slot_len == 0) {
    return purefn->fn(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17);
  }
  else {
    // this must be a closure:
    Closure18Data* rc = BSTS_PTR(Closure18Data, fn);
    BValue* data = closure_data_of((Closure1Data*)rc);
    return rc->fn(data, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17);
  }
}


DEFINE_BSTS_OBJ(Closure19Data, BClosure19 fn; size_t slot_len;);
DEFINE_BSTS_OBJ(BoxedPureFn19, BPureFn19 fn; size_t slot_len;);

BValue alloc_closure19(size_t size, BValue* data, BClosure19 fn) {
    Closure19Data* rc = GC_malloc(closure_data_size(size));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_closure19");
        abort();
    }
    rc->fn = fn;
    rc->slot_len = size;
    BValue* closure_data = closure_data_of((Closure1Data*)rc);
    memcpy(closure_data, data, sizeof(BValue) * size);
    return BSTS_VALUE_FROM_PTR(rc);
}

BValue alloc_boxed_pure_fn19(BPureFn19 fn) {
    uintptr_t fn_int = (uintptr_t)fn;
    uintptr_t small_mask = UINTPTR_MAX >> 2;
    if (fn_int <= small_mask) {
      // can pack into a pure value
      return TO_PURE_VALUE(fn_int);
    }
    BoxedPureFn19* rc = GC_malloc(sizeof(BoxedPureFn19));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_boxed_pure_fn19");
        abort();
    }
    rc->fn = fn;
    rc->slot_len = 0;
    return BSTS_VALUE_FROM_PTR(rc);
}

BValue call_fn19(BValue fn, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11, BValue arg12, BValue arg13, BValue arg14, BValue arg15, BValue arg16, BValue arg17, BValue arg18) {
  if (IS_PURE_VALUE(fn)) {
    // can pack into a pure value
    BPureFn19 pure = (BPureFn19)(uintptr_t)PURE_VALUE(fn);
    return pure(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18);
  }
  BoxedPureFn19* purefn = BSTS_PTR(BoxedPureFn19, fn);
  if (purefn->slot_len == 0) {
    return purefn->fn(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18);
  }
  else {
    // this must be a closure:
    Closure19Data* rc = BSTS_PTR(Closure19Data, fn);
    BValue* data = closure_data_of((Closure1Data*)rc);
    return rc->fn(data, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18);
  }
}


DEFINE_BSTS_OBJ(Closure20Data, BClosure20 fn; size_t slot_len;);
DEFINE_BSTS_OBJ(BoxedPureFn20, BPureFn20 fn; size_t slot_len;);

BValue alloc_closure20(size_t size, BValue* data, BClosure20 fn) {
    Closure20Data* rc = GC_malloc(closure_data_size(size));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_closure20");
        abort();
    }
    rc->fn = fn;
    rc->slot_len = size;
    BValue* closure_data = closure_data_of((Closure1Data*)rc);
    memcpy(closure_data, data, sizeof(BValue) * size);
    return BSTS_VALUE_FROM_PTR(rc);
}

BValue alloc_boxed_pure_fn20(BPureFn20 fn) {
    uintptr_t fn_int = (uintptr_t)fn;
    uintptr_t small_mask = UINTPTR_MAX >> 2;
    if (fn_int <= small_mask) {
      // can pack into a pure value
      return TO_PURE_VALUE(fn_int);
    }
    BoxedPureFn20* rc = GC_malloc(sizeof(BoxedPureFn20));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_boxed_pure_fn20");
        abort();
    }
    rc->fn = fn;
    rc->slot_len = 0;
    return BSTS_VALUE_FROM_PTR(rc);
}

BValue call_fn20(BValue fn, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11, BValue arg12, BValue arg13, BValue arg14, BValue arg15, BValue arg16, BValue arg17, BValue arg18, BValue arg19) {
  if (IS_PURE_VALUE(fn)) {
    // can pack into a pure value
    BPureFn20 pure = (BPureFn20)(uintptr_t)PURE_VALUE(fn);
    return pure(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19);
  }
  BoxedPureFn20* purefn = BSTS_PTR(BoxedPureFn20, fn);
  if (purefn->slot_len == 0) {
    return purefn->fn(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19);
  }
  else {
    // this must be a closure:
    Closure20Data* rc = BSTS_PTR(Closure20Data, fn);
    BValue* data = closure_data_of((Closure1Data*)rc);
    return rc->fn(data, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19);
  }
}


DEFINE_BSTS_OBJ(Closure21Data, BClosure21 fn; size_t slot_len;);
DEFINE_BSTS_OBJ(BoxedPureFn21, BPureFn21 fn; size_t slot_len;);

BValue alloc_closure21(size_t size, BValue* data, BClosure21 fn) {
    Closure21Data* rc = GC_malloc(closure_data_size(size));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_closure21");
        abort();
    }
    rc->fn = fn;
    rc->slot_len = size;
    BValue* closure_data = closure_data_of((Closure1Data*)rc);
    memcpy(closure_data, data, sizeof(BValue) * size);
    return BSTS_VALUE_FROM_PTR(rc);
}

BValue alloc_boxed_pure_fn21(BPureFn21 fn) {
    uintptr_t fn_int = (uintptr_t)fn;
    uintptr_t small_mask = UINTPTR_MAX >> 2;
    if (fn_int <= small_mask) {
      // can pack into a pure value
      return TO_PURE_VALUE(fn_int);
    }
    BoxedPureFn21* rc = GC_malloc(sizeof(BoxedPureFn21));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_boxed_pure_fn21");
        abort();
    }
    rc->fn = fn;
    rc->slot_len = 0;
    return BSTS_VALUE_FROM_PTR(rc);
}

BValue call_fn21(BValue fn, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11, BValue arg12, BValue arg13, BValue arg14, BValue arg15, BValue arg16, BValue arg17, BValue arg18, BValue arg19, BValue arg20) {
  if (IS_PURE_VALUE(fn)) {
    // can pack into a pure value
    BPureFn21 pure = (BPureFn21)(uintptr_t)PURE_VALUE(fn);
    return pure(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20);
  }
  BoxedPureFn21* purefn = BSTS_PTR(BoxedPureFn21, fn);
  if (purefn->slot_len == 0) {
    return purefn->fn(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20);
  }
  else {
    // this must be a closure:
    Closure21Data* rc = BSTS_PTR(Closure21Data, fn);
    BValue* data = closure_data_of((Closure1Data*)rc);
    return rc->fn(data, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20);
  }
}


DEFINE_BSTS_OBJ(Closure22Data, BClosure22 fn; size_t slot_len;);
DEFINE_BSTS_OBJ(BoxedPureFn22, BPureFn22 fn; size_t slot_len;);

BValue alloc_closure22(size_t size, BValue* data, BClosure22 fn) {
    Closure22Data* rc = GC_malloc(closure_data_size(size));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_closure22");
        abort();
    }
    rc->fn = fn;
    rc->slot_len = size;
    BValue* closure_data = closure_data_of((Closure1Data*)rc);
    memcpy(closure_data, data, sizeof(BValue) * size);
    return BSTS_VALUE_FROM_PTR(rc);
}

BValue alloc_boxed_pure_fn22(BPureFn22 fn) {
    uintptr_t fn_int = (uintptr_t)fn;
    uintptr_t small_mask = UINTPTR_MAX >> 2;
    if (fn_int <= small_mask) {
      // can pack into a pure value
      return TO_PURE_VALUE(fn_int);
    }
    BoxedPureFn22* rc = GC_malloc(sizeof(BoxedPureFn22));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_boxed_pure_fn22");
        abort();
    }
    rc->fn = fn;
    rc->slot_len = 0;
    return BSTS_VALUE_FROM_PTR(rc);
}

BValue call_fn22(BValue fn, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11, BValue arg12, BValue arg13, BValue arg14, BValue arg15, BValue arg16, BValue arg17, BValue arg18, BValue arg19, BValue arg20, BValue arg21) {
  if (IS_PURE_VALUE(fn)) {
    // can pack into a pure value
    BPureFn22 pure = (BPureFn22)(uintptr_t)PURE_VALUE(fn);
    return pure(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, arg21);
  }
  BoxedPureFn22* purefn = BSTS_PTR(BoxedPureFn22, fn);
  if (purefn->slot_len == 0) {
    return purefn->fn(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, arg21);
  }
  else {
    // this must be a closure:
    Closure22Data* rc = BSTS_PTR(Closure22Data, fn);
    BValue* data = closure_data_of((Closure1Data*)rc);
    return rc->fn(data, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, arg21);
  }
}


DEFINE_BSTS_OBJ(Closure23Data, BClosure23 fn; size_t slot_len;);
DEFINE_BSTS_OBJ(BoxedPureFn23, BPureFn23 fn; size_t slot_len;);

BValue alloc_closure23(size_t size, BValue* data, BClosure23 fn) {
    Closure23Data* rc = GC_malloc(closure_data_size(size));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_closure23");
        abort();
    }
    rc->fn = fn;
    rc->slot_len = size;
    BValue* closure_data = closure_data_of((Closure1Data*)rc);
    memcpy(closure_data, data, sizeof(BValue) * size);
    return BSTS_VALUE_FROM_PTR(rc);
}

BValue alloc_boxed_pure_fn23(BPureFn23 fn) {
    uintptr_t fn_int = (uintptr_t)fn;
    uintptr_t small_mask = UINTPTR_MAX >> 2;
    if (fn_int <= small_mask) {
      // can pack into a pure value
      return TO_PURE_VALUE(fn_int);
    }
    BoxedPureFn23* rc = GC_malloc(sizeof(BoxedPureFn23));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_boxed_pure_fn23");
        abort();
    }
    rc->fn = fn;
    rc->slot_len = 0;
    return BSTS_VALUE_FROM_PTR(rc);
}

BValue call_fn23(BValue fn, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11, BValue arg12, BValue arg13, BValue arg14, BValue arg15, BValue arg16, BValue arg17, BValue arg18, BValue arg19, BValue arg20, BValue arg21, BValue arg22) {
  if (IS_PURE_VALUE(fn)) {
    // can pack into a pure value
    BPureFn23 pure = (BPureFn23)(uintptr_t)PURE_VALUE(fn);
    return pure(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, arg21, arg22);
  }
  BoxedPureFn23* purefn = BSTS_PTR(BoxedPureFn23, fn);
  if (purefn->slot_len == 0) {
    return purefn->fn(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, arg21, arg22);
  }
  else {
    // this must be a closure:
    Closure23Data* rc = BSTS_PTR(Closure23Data, fn);
    BValue* data = closure_data_of((Closure1Data*)rc);
    return rc->fn(data, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, arg21, arg22);
  }
}


DEFINE_BSTS_OBJ(Closure24Data, BClosure24 fn; size_t slot_len;);
DEFINE_BSTS_OBJ(BoxedPureFn24, BPureFn24 fn; size_t slot_len;);

BValue alloc_closure24(size_t size, BValue* data, BClosure24 fn) {
    Closure24Data* rc = GC_malloc(closure_data_size(size));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_closure24");
        abort();
    }
    rc->fn = fn;
    rc->slot_len = size;
    BValue* closure_data = closure_data_of((Closure1Data*)rc);
    memcpy(closure_data, data, sizeof(BValue) * size);
    return BSTS_VALUE_FROM_PTR(rc);
}

BValue alloc_boxed_pure_fn24(BPureFn24 fn) {
    uintptr_t fn_int = (uintptr_t)fn;
    uintptr_t small_mask = UINTPTR_MAX >> 2;
    if (fn_int <= small_mask) {
      // can pack into a pure value
      return TO_PURE_VALUE(fn_int);
    }
    BoxedPureFn24* rc = GC_malloc(sizeof(BoxedPureFn24));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_boxed_pure_fn24");
        abort();
    }
    rc->fn = fn;
    rc->slot_len = 0;
    return BSTS_VALUE_FROM_PTR(rc);
}

BValue call_fn24(BValue fn, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11, BValue arg12, BValue arg13, BValue arg14, BValue arg15, BValue arg16, BValue arg17, BValue arg18, BValue arg19, BValue arg20, BValue arg21, BValue arg22, BValue arg23) {
  if (IS_PURE_VALUE(fn)) {
    // can pack into a pure value
    BPureFn24 pure = (BPureFn24)(uintptr_t)PURE_VALUE(fn);
    return pure(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, arg21, arg22, arg23);
  }
  BoxedPureFn24* purefn = BSTS_PTR(BoxedPureFn24, fn);
  if (purefn->slot_len == 0) {
    return purefn->fn(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, arg21, arg22, arg23);
  }
  else {
    // this must be a closure:
    Closure24Data* rc = BSTS_PTR(Closure24Data, fn);
    BValue* data = closure_data_of((Closure1Data*)rc);
    return rc->fn(data, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, arg21, arg22, arg23);
  }
}


DEFINE_BSTS_OBJ(Closure25Data, BClosure25 fn; size_t slot_len;);
DEFINE_BSTS_OBJ(BoxedPureFn25, BPureFn25 fn; size_t slot_len;);

BValue alloc_closure25(size_t size, BValue* data, BClosure25 fn) {
    Closure25Data* rc = GC_malloc(closure_data_size(size));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_closure25");
        abort();
    }
    rc->fn = fn;
    rc->slot_len = size;
    BValue* closure_data = closure_data_of((Closure1Data*)rc);
    memcpy(closure_data, data, sizeof(BValue) * size);
    return BSTS_VALUE_FROM_PTR(rc);
}

BValue alloc_boxed_pure_fn25(BPureFn25 fn) {
    uintptr_t fn_int = (uintptr_t)fn;
    uintptr_t small_mask = UINTPTR_MAX >> 2;
    if (fn_int <= small_mask) {
      // can pack into a pure value
      return TO_PURE_VALUE(fn_int);
    }
    BoxedPureFn25* rc = GC_malloc(sizeof(BoxedPureFn25));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_boxed_pure_fn25");
        abort();
    }
    rc->fn = fn;
    rc->slot_len = 0;
    return BSTS_VALUE_FROM_PTR(rc);
}

BValue call_fn25(BValue fn, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11, BValue arg12, BValue arg13, BValue arg14, BValue arg15, BValue arg16, BValue arg17, BValue arg18, BValue arg19, BValue arg20, BValue arg21, BValue arg22, BValue arg23, BValue arg24) {
  if (IS_PURE_VALUE(fn)) {
    // can pack into a pure value
    BPureFn25 pure = (BPureFn25)(uintptr_t)PURE_VALUE(fn);
    return pure(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, arg21, arg22, arg23, arg24);
  }
  BoxedPureFn25* purefn = BSTS_PTR(BoxedPureFn25, fn);
  if (purefn->slot_len == 0) {
    return purefn->fn(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, arg21, arg22, arg23, arg24);
  }
  else {
    // this must be a closure:
    Closure25Data* rc = BSTS_PTR(Closure25Data, fn);
    BValue* data = closure_data_of((Closure1Data*)rc);
    return rc->fn(data, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, arg21, arg22, arg23, arg24);
  }
}


DEFINE_BSTS_OBJ(Closure26Data, BClosure26 fn; size_t slot_len;);
DEFINE_BSTS_OBJ(BoxedPureFn26, BPureFn26 fn; size_t slot_len;);

BValue alloc_closure26(size_t size, BValue* data, BClosure26 fn) {
    Closure26Data* rc = GC_malloc(closure_data_size(size));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_closure26");
        abort();
    }
    rc->fn = fn;
    rc->slot_len = size;
    BValue* closure_data = closure_data_of((Closure1Data*)rc);
    memcpy(closure_data, data, sizeof(BValue) * size);
    return BSTS_VALUE_FROM_PTR(rc);
}

BValue alloc_boxed_pure_fn26(BPureFn26 fn) {
    uintptr_t fn_int = (uintptr_t)fn;
    uintptr_t small_mask = UINTPTR_MAX >> 2;
    if (fn_int <= small_mask) {
      // can pack into a pure value
      return TO_PURE_VALUE(fn_int);
    }
    BoxedPureFn26* rc = GC_malloc(sizeof(BoxedPureFn26));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_boxed_pure_fn26");
        abort();
    }
    rc->fn = fn;
    rc->slot_len = 0;
    return BSTS_VALUE_FROM_PTR(rc);
}

BValue call_fn26(BValue fn, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11, BValue arg12, BValue arg13, BValue arg14, BValue arg15, BValue arg16, BValue arg17, BValue arg18, BValue arg19, BValue arg20, BValue arg21, BValue arg22, BValue arg23, BValue arg24, BValue arg25) {
  if (IS_PURE_VALUE(fn)) {
    // can pack into a pure value
    BPureFn26 pure = (BPureFn26)(uintptr_t)PURE_VALUE(fn);
    return pure(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, arg21, arg22, arg23, arg24, arg25);
  }
  BoxedPureFn26* purefn = BSTS_PTR(BoxedPureFn26, fn);
  if (purefn->slot_len == 0) {
    return purefn->fn(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, arg21, arg22, arg23, arg24, arg25);
  }
  else {
    // this must be a closure:
    Closure26Data* rc = BSTS_PTR(Closure26Data, fn);
    BValue* data = closure_data_of((Closure1Data*)rc);
    return rc->fn(data, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, arg21, arg22, arg23, arg24, arg25);
  }
}


DEFINE_BSTS_OBJ(Closure27Data, BClosure27 fn; size_t slot_len;);
DEFINE_BSTS_OBJ(BoxedPureFn27, BPureFn27 fn; size_t slot_len;);

BValue alloc_closure27(size_t size, BValue* data, BClosure27 fn) {
    Closure27Data* rc = GC_malloc(closure_data_size(size));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_closure27");
        abort();
    }
    rc->fn = fn;
    rc->slot_len = size;
    BValue* closure_data = closure_data_of((Closure1Data*)rc);
    memcpy(closure_data, data, sizeof(BValue) * size);
    return BSTS_VALUE_FROM_PTR(rc);
}

BValue alloc_boxed_pure_fn27(BPureFn27 fn) {
    uintptr_t fn_int = (uintptr_t)fn;
    uintptr_t small_mask = UINTPTR_MAX >> 2;
    if (fn_int <= small_mask) {
      // can pack into a pure value
      return TO_PURE_VALUE(fn_int);
    }
    BoxedPureFn27* rc = GC_malloc(sizeof(BoxedPureFn27));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_boxed_pure_fn27");
        abort();
    }
    rc->fn = fn;
    rc->slot_len = 0;
    return BSTS_VALUE_FROM_PTR(rc);
}

BValue call_fn27(BValue fn, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11, BValue arg12, BValue arg13, BValue arg14, BValue arg15, BValue arg16, BValue arg17, BValue arg18, BValue arg19, BValue arg20, BValue arg21, BValue arg22, BValue arg23, BValue arg24, BValue arg25, BValue arg26) {
  if (IS_PURE_VALUE(fn)) {
    // can pack into a pure value
    BPureFn27 pure = (BPureFn27)(uintptr_t)PURE_VALUE(fn);
    return pure(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, arg21, arg22, arg23, arg24, arg25, arg26);
  }
  BoxedPureFn27* purefn = BSTS_PTR(BoxedPureFn27, fn);
  if (purefn->slot_len == 0) {
    return purefn->fn(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, arg21, arg22, arg23, arg24, arg25, arg26);
  }
  else {
    // this must be a closure:
    Closure27Data* rc = BSTS_PTR(Closure27Data, fn);
    BValue* data = closure_data_of((Closure1Data*)rc);
    return rc->fn(data, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, arg21, arg22, arg23, arg24, arg25, arg26);
  }
}


DEFINE_BSTS_OBJ(Closure28Data, BClosure28 fn; size_t slot_len;);
DEFINE_BSTS_OBJ(BoxedPureFn28, BPureFn28 fn; size_t slot_len;);

BValue alloc_closure28(size_t size, BValue* data, BClosure28 fn) {
    Closure28Data* rc = GC_malloc(closure_data_size(size));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_closure28");
        abort();
    }
    rc->fn = fn;
    rc->slot_len = size;
    BValue* closure_data = closure_data_of((Closure1Data*)rc);
    memcpy(closure_data, data, sizeof(BValue) * size);
    return BSTS_VALUE_FROM_PTR(rc);
}

BValue alloc_boxed_pure_fn28(BPureFn28 fn) {
    uintptr_t fn_int = (uintptr_t)fn;
    uintptr_t small_mask = UINTPTR_MAX >> 2;
    if (fn_int <= small_mask) {
      // can pack into a pure value
      return TO_PURE_VALUE(fn_int);
    }
    BoxedPureFn28* rc = GC_malloc(sizeof(BoxedPureFn28));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_boxed_pure_fn28");
        abort();
    }
    rc->fn = fn;
    rc->slot_len = 0;
    return BSTS_VALUE_FROM_PTR(rc);
}

BValue call_fn28(BValue fn, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11, BValue arg12, BValue arg13, BValue arg14, BValue arg15, BValue arg16, BValue arg17, BValue arg18, BValue arg19, BValue arg20, BValue arg21, BValue arg22, BValue arg23, BValue arg24, BValue arg25, BValue arg26, BValue arg27) {
  if (IS_PURE_VALUE(fn)) {
    // can pack into a pure value
    BPureFn28 pure = (BPureFn28)(uintptr_t)PURE_VALUE(fn);
    return pure(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, arg21, arg22, arg23, arg24, arg25, arg26, arg27);
  }
  BoxedPureFn28* purefn = BSTS_PTR(BoxedPureFn28, fn);
  if (purefn->slot_len == 0) {
    return purefn->fn(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, arg21, arg22, arg23, arg24, arg25, arg26, arg27);
  }
  else {
    // this must be a closure:
    Closure28Data* rc = BSTS_PTR(Closure28Data, fn);
    BValue* data = closure_data_of((Closure1Data*)rc);
    return rc->fn(data, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, arg21, arg22, arg23, arg24, arg25, arg26, arg27);
  }
}


DEFINE_BSTS_OBJ(Closure29Data, BClosure29 fn; size_t slot_len;);
DEFINE_BSTS_OBJ(BoxedPureFn29, BPureFn29 fn; size_t slot_len;);

BValue alloc_closure29(size_t size, BValue* data, BClosure29 fn) {
    Closure29Data* rc = GC_malloc(closure_data_size(size));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_closure29");
        abort();
    }
    rc->fn = fn;
    rc->slot_len = size;
    BValue* closure_data = closure_data_of((Closure1Data*)rc);
    memcpy(closure_data, data, sizeof(BValue) * size);
    return BSTS_VALUE_FROM_PTR(rc);
}

BValue alloc_boxed_pure_fn29(BPureFn29 fn) {
    uintptr_t fn_int = (uintptr_t)fn;
    uintptr_t small_mask = UINTPTR_MAX >> 2;
    if (fn_int <= small_mask) {
      // can pack into a pure value
      return TO_PURE_VALUE(fn_int);
    }
    BoxedPureFn29* rc = GC_malloc(sizeof(BoxedPureFn29));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_boxed_pure_fn29");
        abort();
    }
    rc->fn = fn;
    rc->slot_len = 0;
    return BSTS_VALUE_FROM_PTR(rc);
}

BValue call_fn29(BValue fn, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11, BValue arg12, BValue arg13, BValue arg14, BValue arg15, BValue arg16, BValue arg17, BValue arg18, BValue arg19, BValue arg20, BValue arg21, BValue arg22, BValue arg23, BValue arg24, BValue arg25, BValue arg26, BValue arg27, BValue arg28) {
  if (IS_PURE_VALUE(fn)) {
    // can pack into a pure value
    BPureFn29 pure = (BPureFn29)(uintptr_t)PURE_VALUE(fn);
    return pure(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, arg21, arg22, arg23, arg24, arg25, arg26, arg27, arg28);
  }
  BoxedPureFn29* purefn = BSTS_PTR(BoxedPureFn29, fn);
  if (purefn->slot_len == 0) {
    return purefn->fn(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, arg21, arg22, arg23, arg24, arg25, arg26, arg27, arg28);
  }
  else {
    // this must be a closure:
    Closure29Data* rc = BSTS_PTR(Closure29Data, fn);
    BValue* data = closure_data_of((Closure1Data*)rc);
    return rc->fn(data, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, arg21, arg22, arg23, arg24, arg25, arg26, arg27, arg28);
  }
}


DEFINE_BSTS_OBJ(Closure30Data, BClosure30 fn; size_t slot_len;);
DEFINE_BSTS_OBJ(BoxedPureFn30, BPureFn30 fn; size_t slot_len;);

BValue alloc_closure30(size_t size, BValue* data, BClosure30 fn) {
    Closure30Data* rc = GC_malloc(closure_data_size(size));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_closure30");
        abort();
    }
    rc->fn = fn;
    rc->slot_len = size;
    BValue* closure_data = closure_data_of((Closure1Data*)rc);
    memcpy(closure_data, data, sizeof(BValue) * size);
    return BSTS_VALUE_FROM_PTR(rc);
}

BValue alloc_boxed_pure_fn30(BPureFn30 fn) {
    uintptr_t fn_int = (uintptr_t)fn;
    uintptr_t small_mask = UINTPTR_MAX >> 2;
    if (fn_int <= small_mask) {
      // can pack into a pure value
      return TO_PURE_VALUE(fn_int);
    }
    BoxedPureFn30* rc = GC_malloc(sizeof(BoxedPureFn30));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_boxed_pure_fn30");
        abort();
    }
    rc->fn = fn;
    rc->slot_len = 0;
    return BSTS_VALUE_FROM_PTR(rc);
}

BValue call_fn30(BValue fn, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11, BValue arg12, BValue arg13, BValue arg14, BValue arg15, BValue arg16, BValue arg17, BValue arg18, BValue arg19, BValue arg20, BValue arg21, BValue arg22, BValue arg23, BValue arg24, BValue arg25, BValue arg26, BValue arg27, BValue arg28, BValue arg29) {
  if (IS_PURE_VALUE(fn)) {
    // can pack into a pure value
    BPureFn30 pure = (BPureFn30)(uintptr_t)PURE_VALUE(fn);
    return pure(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, arg21, arg22, arg23, arg24, arg25, arg26, arg27, arg28, arg29);
  }
  BoxedPureFn30* purefn = BSTS_PTR(BoxedPureFn30, fn);
  if (purefn->slot_len == 0) {
    return purefn->fn(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, arg21, arg22, arg23, arg24, arg25, arg26, arg27, arg28, arg29);
  }
  else {
    // this must be a closure:
    Closure30Data* rc = BSTS_PTR(Closure30Data, fn);
    BValue* data = closure_data_of((Closure1Data*)rc);
    return rc->fn(data, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, arg21, arg22, arg23, arg24, arg25, arg26, arg27, arg28, arg29);
  }
}


DEFINE_BSTS_OBJ(Closure31Data, BClosure31 fn; size_t slot_len;);
DEFINE_BSTS_OBJ(BoxedPureFn31, BPureFn31 fn; size_t slot_len;);

BValue alloc_closure31(size_t size, BValue* data, BClosure31 fn) {
    Closure31Data* rc = GC_malloc(closure_data_size(size));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_closure31");
        abort();
    }
    rc->fn = fn;
    rc->slot_len = size;
    BValue* closure_data = closure_data_of((Closure1Data*)rc);
    memcpy(closure_data, data, sizeof(BValue) * size);
    return BSTS_VALUE_FROM_PTR(rc);
}

BValue alloc_boxed_pure_fn31(BPureFn31 fn) {
    uintptr_t fn_int = (uintptr_t)fn;
    uintptr_t small_mask = UINTPTR_MAX >> 2;
    if (fn_int <= small_mask) {
      // can pack into a pure value
      return TO_PURE_VALUE(fn_int);
    }
    BoxedPureFn31* rc = GC_malloc(sizeof(BoxedPureFn31));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_boxed_pure_fn31");
        abort();
    }
    rc->fn = fn;
    rc->slot_len = 0;
    return BSTS_VALUE_FROM_PTR(rc);
}

BValue call_fn31(BValue fn, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11, BValue arg12, BValue arg13, BValue arg14, BValue arg15, BValue arg16, BValue arg17, BValue arg18, BValue arg19, BValue arg20, BValue arg21, BValue arg22, BValue arg23, BValue arg24, BValue arg25, BValue arg26, BValue arg27, BValue arg28, BValue arg29, BValue arg30) {
  if (IS_PURE_VALUE(fn)) {
    // can pack into a pure value
    BPureFn31 pure = (BPureFn31)(uintptr_t)PURE_VALUE(fn);
    return pure(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, arg21, arg22, arg23, arg24, arg25, arg26, arg27, arg28, arg29, arg30);
  }
  BoxedPureFn31* purefn = BSTS_PTR(BoxedPureFn31, fn);
  if (purefn->slot_len == 0) {
    return purefn->fn(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, arg21, arg22, arg23, arg24, arg25, arg26, arg27, arg28, arg29, arg30);
  }
  else {
    // this must be a closure:
    Closure31Data* rc = BSTS_PTR(Closure31Data, fn);
    BValue* data = closure_data_of((Closure1Data*)rc);
    return rc->fn(data, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, arg21, arg22, arg23, arg24, arg25, arg26, arg27, arg28, arg29, arg30);
  }
}


DEFINE_BSTS_OBJ(Closure32Data, BClosure32 fn; size_t slot_len;);
DEFINE_BSTS_OBJ(BoxedPureFn32, BPureFn32 fn; size_t slot_len;);

BValue alloc_closure32(size_t size, BValue* data, BClosure32 fn) {
    Closure32Data* rc = GC_malloc(closure_data_size(size));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_closure32");
        abort();
    }
    rc->fn = fn;
    rc->slot_len = size;
    BValue* closure_data = closure_data_of((Closure1Data*)rc);
    memcpy(closure_data, data, sizeof(BValue) * size);
    return BSTS_VALUE_FROM_PTR(rc);
}

BValue alloc_boxed_pure_fn32(BPureFn32 fn) {
    uintptr_t fn_int = (uintptr_t)fn;
    uintptr_t small_mask = UINTPTR_MAX >> 2;
    if (fn_int <= small_mask) {
      // can pack into a pure value
      return TO_PURE_VALUE(fn_int);
    }
    BoxedPureFn32* rc = GC_malloc(sizeof(BoxedPureFn32));
    if (rc == NULL) {
        perror("GC_malloc failure in alloc_boxed_pure_fn32");
        abort();
    }
    rc->fn = fn;
    rc->slot_len = 0;
    return BSTS_VALUE_FROM_PTR(rc);
}

BValue call_fn32(BValue fn, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11, BValue arg12, BValue arg13, BValue arg14, BValue arg15, BValue arg16, BValue arg17, BValue arg18, BValue arg19, BValue arg20, BValue arg21, BValue arg22, BValue arg23, BValue arg24, BValue arg25, BValue arg26, BValue arg27, BValue arg28, BValue arg29, BValue arg30, BValue arg31) {
  if (IS_PURE_VALUE(fn)) {
    // can pack into a pure value
    BPureFn32 pure = (BPureFn32)(uintptr_t)PURE_VALUE(fn);
    return pure(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, arg21, arg22, arg23, arg24, arg25, arg26, arg27, arg28, arg29, arg30, arg31);
  }
  BoxedPureFn32* purefn = BSTS_PTR(BoxedPureFn32, fn);
  if (purefn->slot_len == 0) {
    return purefn->fn(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, arg21, arg22, arg23, arg24, arg25, arg26, arg27, arg28, arg29, arg30, arg31);
  }
  else {
    // this must be a closure:
    Closure32Data* rc = BSTS_PTR(Closure32Data, fn);
    BValue* data = closure_data_of((Closure1Data*)rc);
    return rc->fn(data, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, arg21, arg22, arg23, arg24, arg25, arg26, arg27, arg28, arg29, arg30, arg31);
  }
}

