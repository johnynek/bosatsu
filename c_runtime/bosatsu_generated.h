// STRUCTS
DEFINE_RC_STRUCT(Struct2,BValue _0;BValue _1;);

void free_struct2(Struct2* s) {
    release_value(s->_0);
    release_value(s->_1);
    free(s);
}
BValue alloc_struct2(BValue b0, BValue b1) {
    Struct2* rc = malloc(sizeof(Struct2));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = (FreeFn)free_struct2;
    rc->_0 = b0;
    rc->_1 = b1;
    return (BValue)rc;
}

DEFINE_RC_STRUCT(Struct3,BValue _0;BValue _1;BValue _2;);

void free_struct3(Struct3* s) {
    release_value(s->_0);
    release_value(s->_1);
    release_value(s->_2);
    free(s);
}
BValue alloc_struct3(BValue b0, BValue b1, BValue b2) {
    Struct3* rc = malloc(sizeof(Struct3));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = (FreeFn)free_struct3;
    rc->_0 = b0;
    rc->_1 = b1;
    rc->_2 = b2;
    return (BValue)rc;
}

DEFINE_RC_STRUCT(Struct4,BValue _0;BValue _1;BValue _2;BValue _3;);

void free_struct4(Struct4* s) {
    release_value(s->_0);
    release_value(s->_1);
    release_value(s->_2);
    release_value(s->_3);
    free(s);
}
BValue alloc_struct4(BValue b0, BValue b1, BValue b2, BValue b3) {
    Struct4* rc = malloc(sizeof(Struct4));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = (FreeFn)free_struct4;
    rc->_0 = b0;
    rc->_1 = b1;
    rc->_2 = b2;
    rc->_3 = b3;
    return (BValue)rc;
}

DEFINE_RC_STRUCT(Struct5,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;);

void free_struct5(Struct5* s) {
    release_value(s->_0);
    release_value(s->_1);
    release_value(s->_2);
    release_value(s->_3);
    release_value(s->_4);
    free(s);
}
BValue alloc_struct5(BValue b0, BValue b1, BValue b2, BValue b3, BValue b4) {
    Struct5* rc = malloc(sizeof(Struct5));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = (FreeFn)free_struct5;
    rc->_0 = b0;
    rc->_1 = b1;
    rc->_2 = b2;
    rc->_3 = b3;
    rc->_4 = b4;
    return (BValue)rc;
}

DEFINE_RC_STRUCT(Struct6,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;);

void free_struct6(Struct6* s) {
    release_value(s->_0);
    release_value(s->_1);
    release_value(s->_2);
    release_value(s->_3);
    release_value(s->_4);
    release_value(s->_5);
    free(s);
}
BValue alloc_struct6(BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5) {
    Struct6* rc = malloc(sizeof(Struct6));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = (FreeFn)free_struct6;
    rc->_0 = b0;
    rc->_1 = b1;
    rc->_2 = b2;
    rc->_3 = b3;
    rc->_4 = b4;
    rc->_5 = b5;
    return (BValue)rc;
}

DEFINE_RC_STRUCT(Struct7,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;BValue _6;);

void free_struct7(Struct7* s) {
    release_value(s->_0);
    release_value(s->_1);
    release_value(s->_2);
    release_value(s->_3);
    release_value(s->_4);
    release_value(s->_5);
    release_value(s->_6);
    free(s);
}
BValue alloc_struct7(BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5, BValue b6) {
    Struct7* rc = malloc(sizeof(Struct7));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = (FreeFn)free_struct7;
    rc->_0 = b0;
    rc->_1 = b1;
    rc->_2 = b2;
    rc->_3 = b3;
    rc->_4 = b4;
    rc->_5 = b5;
    rc->_6 = b6;
    return (BValue)rc;
}

DEFINE_RC_STRUCT(Struct8,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;BValue _6;BValue _7;);

void free_struct8(Struct8* s) {
    release_value(s->_0);
    release_value(s->_1);
    release_value(s->_2);
    release_value(s->_3);
    release_value(s->_4);
    release_value(s->_5);
    release_value(s->_6);
    release_value(s->_7);
    free(s);
}
BValue alloc_struct8(BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5, BValue b6, BValue b7) {
    Struct8* rc = malloc(sizeof(Struct8));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = (FreeFn)free_struct8;
    rc->_0 = b0;
    rc->_1 = b1;
    rc->_2 = b2;
    rc->_3 = b3;
    rc->_4 = b4;
    rc->_5 = b5;
    rc->_6 = b6;
    rc->_7 = b7;
    return (BValue)rc;
}

DEFINE_RC_STRUCT(Struct9,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;BValue _6;BValue _7;BValue _8;);

void free_struct9(Struct9* s) {
    release_value(s->_0);
    release_value(s->_1);
    release_value(s->_2);
    release_value(s->_3);
    release_value(s->_4);
    release_value(s->_5);
    release_value(s->_6);
    release_value(s->_7);
    release_value(s->_8);
    free(s);
}
BValue alloc_struct9(BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5, BValue b6, BValue b7, BValue b8) {
    Struct9* rc = malloc(sizeof(Struct9));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = (FreeFn)free_struct9;
    rc->_0 = b0;
    rc->_1 = b1;
    rc->_2 = b2;
    rc->_3 = b3;
    rc->_4 = b4;
    rc->_5 = b5;
    rc->_6 = b6;
    rc->_7 = b7;
    rc->_8 = b8;
    return (BValue)rc;
}

DEFINE_RC_STRUCT(Struct10,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;BValue _6;BValue _7;BValue _8;BValue _9;);

void free_struct10(Struct10* s) {
    release_value(s->_0);
    release_value(s->_1);
    release_value(s->_2);
    release_value(s->_3);
    release_value(s->_4);
    release_value(s->_5);
    release_value(s->_6);
    release_value(s->_7);
    release_value(s->_8);
    release_value(s->_9);
    free(s);
}
BValue alloc_struct10(BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5, BValue b6, BValue b7, BValue b8, BValue b9) {
    Struct10* rc = malloc(sizeof(Struct10));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = (FreeFn)free_struct10;
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
    return (BValue)rc;
}

DEFINE_RC_STRUCT(Struct11,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;BValue _6;BValue _7;BValue _8;BValue _9;BValue _10;);

void free_struct11(Struct11* s) {
    release_value(s->_0);
    release_value(s->_1);
    release_value(s->_2);
    release_value(s->_3);
    release_value(s->_4);
    release_value(s->_5);
    release_value(s->_6);
    release_value(s->_7);
    release_value(s->_8);
    release_value(s->_9);
    release_value(s->_10);
    free(s);
}
BValue alloc_struct11(BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5, BValue b6, BValue b7, BValue b8, BValue b9, BValue b10) {
    Struct11* rc = malloc(sizeof(Struct11));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = (FreeFn)free_struct11;
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
    return (BValue)rc;
}

DEFINE_RC_STRUCT(Struct12,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;BValue _6;BValue _7;BValue _8;BValue _9;BValue _10;BValue _11;);

void free_struct12(Struct12* s) {
    release_value(s->_0);
    release_value(s->_1);
    release_value(s->_2);
    release_value(s->_3);
    release_value(s->_4);
    release_value(s->_5);
    release_value(s->_6);
    release_value(s->_7);
    release_value(s->_8);
    release_value(s->_9);
    release_value(s->_10);
    release_value(s->_11);
    free(s);
}
BValue alloc_struct12(BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5, BValue b6, BValue b7, BValue b8, BValue b9, BValue b10, BValue b11) {
    Struct12* rc = malloc(sizeof(Struct12));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = (FreeFn)free_struct12;
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
    return (BValue)rc;
}

DEFINE_RC_STRUCT(Struct13,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;BValue _6;BValue _7;BValue _8;BValue _9;BValue _10;BValue _11;BValue _12;);

void free_struct13(Struct13* s) {
    release_value(s->_0);
    release_value(s->_1);
    release_value(s->_2);
    release_value(s->_3);
    release_value(s->_4);
    release_value(s->_5);
    release_value(s->_6);
    release_value(s->_7);
    release_value(s->_8);
    release_value(s->_9);
    release_value(s->_10);
    release_value(s->_11);
    release_value(s->_12);
    free(s);
}
BValue alloc_struct13(BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5, BValue b6, BValue b7, BValue b8, BValue b9, BValue b10, BValue b11, BValue b12) {
    Struct13* rc = malloc(sizeof(Struct13));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = (FreeFn)free_struct13;
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
    return (BValue)rc;
}

DEFINE_RC_STRUCT(Struct14,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;BValue _6;BValue _7;BValue _8;BValue _9;BValue _10;BValue _11;BValue _12;BValue _13;);

void free_struct14(Struct14* s) {
    release_value(s->_0);
    release_value(s->_1);
    release_value(s->_2);
    release_value(s->_3);
    release_value(s->_4);
    release_value(s->_5);
    release_value(s->_6);
    release_value(s->_7);
    release_value(s->_8);
    release_value(s->_9);
    release_value(s->_10);
    release_value(s->_11);
    release_value(s->_12);
    release_value(s->_13);
    free(s);
}
BValue alloc_struct14(BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5, BValue b6, BValue b7, BValue b8, BValue b9, BValue b10, BValue b11, BValue b12, BValue b13) {
    Struct14* rc = malloc(sizeof(Struct14));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = (FreeFn)free_struct14;
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
    return (BValue)rc;
}

DEFINE_RC_STRUCT(Struct15,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;BValue _6;BValue _7;BValue _8;BValue _9;BValue _10;BValue _11;BValue _12;BValue _13;BValue _14;);

void free_struct15(Struct15* s) {
    release_value(s->_0);
    release_value(s->_1);
    release_value(s->_2);
    release_value(s->_3);
    release_value(s->_4);
    release_value(s->_5);
    release_value(s->_6);
    release_value(s->_7);
    release_value(s->_8);
    release_value(s->_9);
    release_value(s->_10);
    release_value(s->_11);
    release_value(s->_12);
    release_value(s->_13);
    release_value(s->_14);
    free(s);
}
BValue alloc_struct15(BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5, BValue b6, BValue b7, BValue b8, BValue b9, BValue b10, BValue b11, BValue b12, BValue b13, BValue b14) {
    Struct15* rc = malloc(sizeof(Struct15));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = (FreeFn)free_struct15;
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
    return (BValue)rc;
}

DEFINE_RC_STRUCT(Struct16,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;BValue _6;BValue _7;BValue _8;BValue _9;BValue _10;BValue _11;BValue _12;BValue _13;BValue _14;BValue _15;);

void free_struct16(Struct16* s) {
    release_value(s->_0);
    release_value(s->_1);
    release_value(s->_2);
    release_value(s->_3);
    release_value(s->_4);
    release_value(s->_5);
    release_value(s->_6);
    release_value(s->_7);
    release_value(s->_8);
    release_value(s->_9);
    release_value(s->_10);
    release_value(s->_11);
    release_value(s->_12);
    release_value(s->_13);
    release_value(s->_14);
    release_value(s->_15);
    free(s);
}
BValue alloc_struct16(BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5, BValue b6, BValue b7, BValue b8, BValue b9, BValue b10, BValue b11, BValue b12, BValue b13, BValue b14, BValue b15) {
    Struct16* rc = malloc(sizeof(Struct16));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = (FreeFn)free_struct16;
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
    return (BValue)rc;
}

DEFINE_RC_STRUCT(Struct17,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;BValue _6;BValue _7;BValue _8;BValue _9;BValue _10;BValue _11;BValue _12;BValue _13;BValue _14;BValue _15;BValue _16;);

void free_struct17(Struct17* s) {
    release_value(s->_0);
    release_value(s->_1);
    release_value(s->_2);
    release_value(s->_3);
    release_value(s->_4);
    release_value(s->_5);
    release_value(s->_6);
    release_value(s->_7);
    release_value(s->_8);
    release_value(s->_9);
    release_value(s->_10);
    release_value(s->_11);
    release_value(s->_12);
    release_value(s->_13);
    release_value(s->_14);
    release_value(s->_15);
    release_value(s->_16);
    free(s);
}
BValue alloc_struct17(BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5, BValue b6, BValue b7, BValue b8, BValue b9, BValue b10, BValue b11, BValue b12, BValue b13, BValue b14, BValue b15, BValue b16) {
    Struct17* rc = malloc(sizeof(Struct17));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = (FreeFn)free_struct17;
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
    return (BValue)rc;
}

DEFINE_RC_STRUCT(Struct18,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;BValue _6;BValue _7;BValue _8;BValue _9;BValue _10;BValue _11;BValue _12;BValue _13;BValue _14;BValue _15;BValue _16;BValue _17;);

void free_struct18(Struct18* s) {
    release_value(s->_0);
    release_value(s->_1);
    release_value(s->_2);
    release_value(s->_3);
    release_value(s->_4);
    release_value(s->_5);
    release_value(s->_6);
    release_value(s->_7);
    release_value(s->_8);
    release_value(s->_9);
    release_value(s->_10);
    release_value(s->_11);
    release_value(s->_12);
    release_value(s->_13);
    release_value(s->_14);
    release_value(s->_15);
    release_value(s->_16);
    release_value(s->_17);
    free(s);
}
BValue alloc_struct18(BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5, BValue b6, BValue b7, BValue b8, BValue b9, BValue b10, BValue b11, BValue b12, BValue b13, BValue b14, BValue b15, BValue b16, BValue b17) {
    Struct18* rc = malloc(sizeof(Struct18));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = (FreeFn)free_struct18;
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
    return (BValue)rc;
}

DEFINE_RC_STRUCT(Struct19,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;BValue _6;BValue _7;BValue _8;BValue _9;BValue _10;BValue _11;BValue _12;BValue _13;BValue _14;BValue _15;BValue _16;BValue _17;BValue _18;);

void free_struct19(Struct19* s) {
    release_value(s->_0);
    release_value(s->_1);
    release_value(s->_2);
    release_value(s->_3);
    release_value(s->_4);
    release_value(s->_5);
    release_value(s->_6);
    release_value(s->_7);
    release_value(s->_8);
    release_value(s->_9);
    release_value(s->_10);
    release_value(s->_11);
    release_value(s->_12);
    release_value(s->_13);
    release_value(s->_14);
    release_value(s->_15);
    release_value(s->_16);
    release_value(s->_17);
    release_value(s->_18);
    free(s);
}
BValue alloc_struct19(BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5, BValue b6, BValue b7, BValue b8, BValue b9, BValue b10, BValue b11, BValue b12, BValue b13, BValue b14, BValue b15, BValue b16, BValue b17, BValue b18) {
    Struct19* rc = malloc(sizeof(Struct19));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = (FreeFn)free_struct19;
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
    return (BValue)rc;
}

DEFINE_RC_STRUCT(Struct20,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;BValue _6;BValue _7;BValue _8;BValue _9;BValue _10;BValue _11;BValue _12;BValue _13;BValue _14;BValue _15;BValue _16;BValue _17;BValue _18;BValue _19;);

void free_struct20(Struct20* s) {
    release_value(s->_0);
    release_value(s->_1);
    release_value(s->_2);
    release_value(s->_3);
    release_value(s->_4);
    release_value(s->_5);
    release_value(s->_6);
    release_value(s->_7);
    release_value(s->_8);
    release_value(s->_9);
    release_value(s->_10);
    release_value(s->_11);
    release_value(s->_12);
    release_value(s->_13);
    release_value(s->_14);
    release_value(s->_15);
    release_value(s->_16);
    release_value(s->_17);
    release_value(s->_18);
    release_value(s->_19);
    free(s);
}
BValue alloc_struct20(BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5, BValue b6, BValue b7, BValue b8, BValue b9, BValue b10, BValue b11, BValue b12, BValue b13, BValue b14, BValue b15, BValue b16, BValue b17, BValue b18, BValue b19) {
    Struct20* rc = malloc(sizeof(Struct20));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = (FreeFn)free_struct20;
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
    return (BValue)rc;
}

DEFINE_RC_STRUCT(Struct21,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;BValue _6;BValue _7;BValue _8;BValue _9;BValue _10;BValue _11;BValue _12;BValue _13;BValue _14;BValue _15;BValue _16;BValue _17;BValue _18;BValue _19;BValue _20;);

void free_struct21(Struct21* s) {
    release_value(s->_0);
    release_value(s->_1);
    release_value(s->_2);
    release_value(s->_3);
    release_value(s->_4);
    release_value(s->_5);
    release_value(s->_6);
    release_value(s->_7);
    release_value(s->_8);
    release_value(s->_9);
    release_value(s->_10);
    release_value(s->_11);
    release_value(s->_12);
    release_value(s->_13);
    release_value(s->_14);
    release_value(s->_15);
    release_value(s->_16);
    release_value(s->_17);
    release_value(s->_18);
    release_value(s->_19);
    release_value(s->_20);
    free(s);
}
BValue alloc_struct21(BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5, BValue b6, BValue b7, BValue b8, BValue b9, BValue b10, BValue b11, BValue b12, BValue b13, BValue b14, BValue b15, BValue b16, BValue b17, BValue b18, BValue b19, BValue b20) {
    Struct21* rc = malloc(sizeof(Struct21));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = (FreeFn)free_struct21;
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
    return (BValue)rc;
}

DEFINE_RC_STRUCT(Struct22,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;BValue _6;BValue _7;BValue _8;BValue _9;BValue _10;BValue _11;BValue _12;BValue _13;BValue _14;BValue _15;BValue _16;BValue _17;BValue _18;BValue _19;BValue _20;BValue _21;);

void free_struct22(Struct22* s) {
    release_value(s->_0);
    release_value(s->_1);
    release_value(s->_2);
    release_value(s->_3);
    release_value(s->_4);
    release_value(s->_5);
    release_value(s->_6);
    release_value(s->_7);
    release_value(s->_8);
    release_value(s->_9);
    release_value(s->_10);
    release_value(s->_11);
    release_value(s->_12);
    release_value(s->_13);
    release_value(s->_14);
    release_value(s->_15);
    release_value(s->_16);
    release_value(s->_17);
    release_value(s->_18);
    release_value(s->_19);
    release_value(s->_20);
    release_value(s->_21);
    free(s);
}
BValue alloc_struct22(BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5, BValue b6, BValue b7, BValue b8, BValue b9, BValue b10, BValue b11, BValue b12, BValue b13, BValue b14, BValue b15, BValue b16, BValue b17, BValue b18, BValue b19, BValue b20, BValue b21) {
    Struct22* rc = malloc(sizeof(Struct22));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = (FreeFn)free_struct22;
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
    return (BValue)rc;
}

DEFINE_RC_STRUCT(Struct23,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;BValue _6;BValue _7;BValue _8;BValue _9;BValue _10;BValue _11;BValue _12;BValue _13;BValue _14;BValue _15;BValue _16;BValue _17;BValue _18;BValue _19;BValue _20;BValue _21;BValue _22;);

void free_struct23(Struct23* s) {
    release_value(s->_0);
    release_value(s->_1);
    release_value(s->_2);
    release_value(s->_3);
    release_value(s->_4);
    release_value(s->_5);
    release_value(s->_6);
    release_value(s->_7);
    release_value(s->_8);
    release_value(s->_9);
    release_value(s->_10);
    release_value(s->_11);
    release_value(s->_12);
    release_value(s->_13);
    release_value(s->_14);
    release_value(s->_15);
    release_value(s->_16);
    release_value(s->_17);
    release_value(s->_18);
    release_value(s->_19);
    release_value(s->_20);
    release_value(s->_21);
    release_value(s->_22);
    free(s);
}
BValue alloc_struct23(BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5, BValue b6, BValue b7, BValue b8, BValue b9, BValue b10, BValue b11, BValue b12, BValue b13, BValue b14, BValue b15, BValue b16, BValue b17, BValue b18, BValue b19, BValue b20, BValue b21, BValue b22) {
    Struct23* rc = malloc(sizeof(Struct23));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = (FreeFn)free_struct23;
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
    return (BValue)rc;
}

DEFINE_RC_STRUCT(Struct24,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;BValue _6;BValue _7;BValue _8;BValue _9;BValue _10;BValue _11;BValue _12;BValue _13;BValue _14;BValue _15;BValue _16;BValue _17;BValue _18;BValue _19;BValue _20;BValue _21;BValue _22;BValue _23;);

void free_struct24(Struct24* s) {
    release_value(s->_0);
    release_value(s->_1);
    release_value(s->_2);
    release_value(s->_3);
    release_value(s->_4);
    release_value(s->_5);
    release_value(s->_6);
    release_value(s->_7);
    release_value(s->_8);
    release_value(s->_9);
    release_value(s->_10);
    release_value(s->_11);
    release_value(s->_12);
    release_value(s->_13);
    release_value(s->_14);
    release_value(s->_15);
    release_value(s->_16);
    release_value(s->_17);
    release_value(s->_18);
    release_value(s->_19);
    release_value(s->_20);
    release_value(s->_21);
    release_value(s->_22);
    release_value(s->_23);
    free(s);
}
BValue alloc_struct24(BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5, BValue b6, BValue b7, BValue b8, BValue b9, BValue b10, BValue b11, BValue b12, BValue b13, BValue b14, BValue b15, BValue b16, BValue b17, BValue b18, BValue b19, BValue b20, BValue b21, BValue b22, BValue b23) {
    Struct24* rc = malloc(sizeof(Struct24));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = (FreeFn)free_struct24;
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
    return (BValue)rc;
}

DEFINE_RC_STRUCT(Struct25,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;BValue _6;BValue _7;BValue _8;BValue _9;BValue _10;BValue _11;BValue _12;BValue _13;BValue _14;BValue _15;BValue _16;BValue _17;BValue _18;BValue _19;BValue _20;BValue _21;BValue _22;BValue _23;BValue _24;);

void free_struct25(Struct25* s) {
    release_value(s->_0);
    release_value(s->_1);
    release_value(s->_2);
    release_value(s->_3);
    release_value(s->_4);
    release_value(s->_5);
    release_value(s->_6);
    release_value(s->_7);
    release_value(s->_8);
    release_value(s->_9);
    release_value(s->_10);
    release_value(s->_11);
    release_value(s->_12);
    release_value(s->_13);
    release_value(s->_14);
    release_value(s->_15);
    release_value(s->_16);
    release_value(s->_17);
    release_value(s->_18);
    release_value(s->_19);
    release_value(s->_20);
    release_value(s->_21);
    release_value(s->_22);
    release_value(s->_23);
    release_value(s->_24);
    free(s);
}
BValue alloc_struct25(BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5, BValue b6, BValue b7, BValue b8, BValue b9, BValue b10, BValue b11, BValue b12, BValue b13, BValue b14, BValue b15, BValue b16, BValue b17, BValue b18, BValue b19, BValue b20, BValue b21, BValue b22, BValue b23, BValue b24) {
    Struct25* rc = malloc(sizeof(Struct25));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = (FreeFn)free_struct25;
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
    return (BValue)rc;
}

DEFINE_RC_STRUCT(Struct26,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;BValue _6;BValue _7;BValue _8;BValue _9;BValue _10;BValue _11;BValue _12;BValue _13;BValue _14;BValue _15;BValue _16;BValue _17;BValue _18;BValue _19;BValue _20;BValue _21;BValue _22;BValue _23;BValue _24;BValue _25;);

void free_struct26(Struct26* s) {
    release_value(s->_0);
    release_value(s->_1);
    release_value(s->_2);
    release_value(s->_3);
    release_value(s->_4);
    release_value(s->_5);
    release_value(s->_6);
    release_value(s->_7);
    release_value(s->_8);
    release_value(s->_9);
    release_value(s->_10);
    release_value(s->_11);
    release_value(s->_12);
    release_value(s->_13);
    release_value(s->_14);
    release_value(s->_15);
    release_value(s->_16);
    release_value(s->_17);
    release_value(s->_18);
    release_value(s->_19);
    release_value(s->_20);
    release_value(s->_21);
    release_value(s->_22);
    release_value(s->_23);
    release_value(s->_24);
    release_value(s->_25);
    free(s);
}
BValue alloc_struct26(BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5, BValue b6, BValue b7, BValue b8, BValue b9, BValue b10, BValue b11, BValue b12, BValue b13, BValue b14, BValue b15, BValue b16, BValue b17, BValue b18, BValue b19, BValue b20, BValue b21, BValue b22, BValue b23, BValue b24, BValue b25) {
    Struct26* rc = malloc(sizeof(Struct26));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = (FreeFn)free_struct26;
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
    return (BValue)rc;
}

DEFINE_RC_STRUCT(Struct27,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;BValue _6;BValue _7;BValue _8;BValue _9;BValue _10;BValue _11;BValue _12;BValue _13;BValue _14;BValue _15;BValue _16;BValue _17;BValue _18;BValue _19;BValue _20;BValue _21;BValue _22;BValue _23;BValue _24;BValue _25;BValue _26;);

void free_struct27(Struct27* s) {
    release_value(s->_0);
    release_value(s->_1);
    release_value(s->_2);
    release_value(s->_3);
    release_value(s->_4);
    release_value(s->_5);
    release_value(s->_6);
    release_value(s->_7);
    release_value(s->_8);
    release_value(s->_9);
    release_value(s->_10);
    release_value(s->_11);
    release_value(s->_12);
    release_value(s->_13);
    release_value(s->_14);
    release_value(s->_15);
    release_value(s->_16);
    release_value(s->_17);
    release_value(s->_18);
    release_value(s->_19);
    release_value(s->_20);
    release_value(s->_21);
    release_value(s->_22);
    release_value(s->_23);
    release_value(s->_24);
    release_value(s->_25);
    release_value(s->_26);
    free(s);
}
BValue alloc_struct27(BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5, BValue b6, BValue b7, BValue b8, BValue b9, BValue b10, BValue b11, BValue b12, BValue b13, BValue b14, BValue b15, BValue b16, BValue b17, BValue b18, BValue b19, BValue b20, BValue b21, BValue b22, BValue b23, BValue b24, BValue b25, BValue b26) {
    Struct27* rc = malloc(sizeof(Struct27));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = (FreeFn)free_struct27;
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
    return (BValue)rc;
}

DEFINE_RC_STRUCT(Struct28,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;BValue _6;BValue _7;BValue _8;BValue _9;BValue _10;BValue _11;BValue _12;BValue _13;BValue _14;BValue _15;BValue _16;BValue _17;BValue _18;BValue _19;BValue _20;BValue _21;BValue _22;BValue _23;BValue _24;BValue _25;BValue _26;BValue _27;);

void free_struct28(Struct28* s) {
    release_value(s->_0);
    release_value(s->_1);
    release_value(s->_2);
    release_value(s->_3);
    release_value(s->_4);
    release_value(s->_5);
    release_value(s->_6);
    release_value(s->_7);
    release_value(s->_8);
    release_value(s->_9);
    release_value(s->_10);
    release_value(s->_11);
    release_value(s->_12);
    release_value(s->_13);
    release_value(s->_14);
    release_value(s->_15);
    release_value(s->_16);
    release_value(s->_17);
    release_value(s->_18);
    release_value(s->_19);
    release_value(s->_20);
    release_value(s->_21);
    release_value(s->_22);
    release_value(s->_23);
    release_value(s->_24);
    release_value(s->_25);
    release_value(s->_26);
    release_value(s->_27);
    free(s);
}
BValue alloc_struct28(BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5, BValue b6, BValue b7, BValue b8, BValue b9, BValue b10, BValue b11, BValue b12, BValue b13, BValue b14, BValue b15, BValue b16, BValue b17, BValue b18, BValue b19, BValue b20, BValue b21, BValue b22, BValue b23, BValue b24, BValue b25, BValue b26, BValue b27) {
    Struct28* rc = malloc(sizeof(Struct28));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = (FreeFn)free_struct28;
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
    return (BValue)rc;
}

DEFINE_RC_STRUCT(Struct29,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;BValue _6;BValue _7;BValue _8;BValue _9;BValue _10;BValue _11;BValue _12;BValue _13;BValue _14;BValue _15;BValue _16;BValue _17;BValue _18;BValue _19;BValue _20;BValue _21;BValue _22;BValue _23;BValue _24;BValue _25;BValue _26;BValue _27;BValue _28;);

void free_struct29(Struct29* s) {
    release_value(s->_0);
    release_value(s->_1);
    release_value(s->_2);
    release_value(s->_3);
    release_value(s->_4);
    release_value(s->_5);
    release_value(s->_6);
    release_value(s->_7);
    release_value(s->_8);
    release_value(s->_9);
    release_value(s->_10);
    release_value(s->_11);
    release_value(s->_12);
    release_value(s->_13);
    release_value(s->_14);
    release_value(s->_15);
    release_value(s->_16);
    release_value(s->_17);
    release_value(s->_18);
    release_value(s->_19);
    release_value(s->_20);
    release_value(s->_21);
    release_value(s->_22);
    release_value(s->_23);
    release_value(s->_24);
    release_value(s->_25);
    release_value(s->_26);
    release_value(s->_27);
    release_value(s->_28);
    free(s);
}
BValue alloc_struct29(BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5, BValue b6, BValue b7, BValue b8, BValue b9, BValue b10, BValue b11, BValue b12, BValue b13, BValue b14, BValue b15, BValue b16, BValue b17, BValue b18, BValue b19, BValue b20, BValue b21, BValue b22, BValue b23, BValue b24, BValue b25, BValue b26, BValue b27, BValue b28) {
    Struct29* rc = malloc(sizeof(Struct29));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = (FreeFn)free_struct29;
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
    return (BValue)rc;
}

DEFINE_RC_STRUCT(Struct30,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;BValue _6;BValue _7;BValue _8;BValue _9;BValue _10;BValue _11;BValue _12;BValue _13;BValue _14;BValue _15;BValue _16;BValue _17;BValue _18;BValue _19;BValue _20;BValue _21;BValue _22;BValue _23;BValue _24;BValue _25;BValue _26;BValue _27;BValue _28;BValue _29;);

void free_struct30(Struct30* s) {
    release_value(s->_0);
    release_value(s->_1);
    release_value(s->_2);
    release_value(s->_3);
    release_value(s->_4);
    release_value(s->_5);
    release_value(s->_6);
    release_value(s->_7);
    release_value(s->_8);
    release_value(s->_9);
    release_value(s->_10);
    release_value(s->_11);
    release_value(s->_12);
    release_value(s->_13);
    release_value(s->_14);
    release_value(s->_15);
    release_value(s->_16);
    release_value(s->_17);
    release_value(s->_18);
    release_value(s->_19);
    release_value(s->_20);
    release_value(s->_21);
    release_value(s->_22);
    release_value(s->_23);
    release_value(s->_24);
    release_value(s->_25);
    release_value(s->_26);
    release_value(s->_27);
    release_value(s->_28);
    release_value(s->_29);
    free(s);
}
BValue alloc_struct30(BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5, BValue b6, BValue b7, BValue b8, BValue b9, BValue b10, BValue b11, BValue b12, BValue b13, BValue b14, BValue b15, BValue b16, BValue b17, BValue b18, BValue b19, BValue b20, BValue b21, BValue b22, BValue b23, BValue b24, BValue b25, BValue b26, BValue b27, BValue b28, BValue b29) {
    Struct30* rc = malloc(sizeof(Struct30));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = (FreeFn)free_struct30;
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
    return (BValue)rc;
}

DEFINE_RC_STRUCT(Struct31,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;BValue _6;BValue _7;BValue _8;BValue _9;BValue _10;BValue _11;BValue _12;BValue _13;BValue _14;BValue _15;BValue _16;BValue _17;BValue _18;BValue _19;BValue _20;BValue _21;BValue _22;BValue _23;BValue _24;BValue _25;BValue _26;BValue _27;BValue _28;BValue _29;BValue _30;);

void free_struct31(Struct31* s) {
    release_value(s->_0);
    release_value(s->_1);
    release_value(s->_2);
    release_value(s->_3);
    release_value(s->_4);
    release_value(s->_5);
    release_value(s->_6);
    release_value(s->_7);
    release_value(s->_8);
    release_value(s->_9);
    release_value(s->_10);
    release_value(s->_11);
    release_value(s->_12);
    release_value(s->_13);
    release_value(s->_14);
    release_value(s->_15);
    release_value(s->_16);
    release_value(s->_17);
    release_value(s->_18);
    release_value(s->_19);
    release_value(s->_20);
    release_value(s->_21);
    release_value(s->_22);
    release_value(s->_23);
    release_value(s->_24);
    release_value(s->_25);
    release_value(s->_26);
    release_value(s->_27);
    release_value(s->_28);
    release_value(s->_29);
    release_value(s->_30);
    free(s);
}
BValue alloc_struct31(BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5, BValue b6, BValue b7, BValue b8, BValue b9, BValue b10, BValue b11, BValue b12, BValue b13, BValue b14, BValue b15, BValue b16, BValue b17, BValue b18, BValue b19, BValue b20, BValue b21, BValue b22, BValue b23, BValue b24, BValue b25, BValue b26, BValue b27, BValue b28, BValue b29, BValue b30) {
    Struct31* rc = malloc(sizeof(Struct31));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = (FreeFn)free_struct31;
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
    return (BValue)rc;
}

DEFINE_RC_STRUCT(Struct32,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;BValue _6;BValue _7;BValue _8;BValue _9;BValue _10;BValue _11;BValue _12;BValue _13;BValue _14;BValue _15;BValue _16;BValue _17;BValue _18;BValue _19;BValue _20;BValue _21;BValue _22;BValue _23;BValue _24;BValue _25;BValue _26;BValue _27;BValue _28;BValue _29;BValue _30;BValue _31;);

void free_struct32(Struct32* s) {
    release_value(s->_0);
    release_value(s->_1);
    release_value(s->_2);
    release_value(s->_3);
    release_value(s->_4);
    release_value(s->_5);
    release_value(s->_6);
    release_value(s->_7);
    release_value(s->_8);
    release_value(s->_9);
    release_value(s->_10);
    release_value(s->_11);
    release_value(s->_12);
    release_value(s->_13);
    release_value(s->_14);
    release_value(s->_15);
    release_value(s->_16);
    release_value(s->_17);
    release_value(s->_18);
    release_value(s->_19);
    release_value(s->_20);
    release_value(s->_21);
    release_value(s->_22);
    release_value(s->_23);
    release_value(s->_24);
    release_value(s->_25);
    release_value(s->_26);
    release_value(s->_27);
    release_value(s->_28);
    release_value(s->_29);
    release_value(s->_30);
    release_value(s->_31);
    free(s);
}
BValue alloc_struct32(BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5, BValue b6, BValue b7, BValue b8, BValue b9, BValue b10, BValue b11, BValue b12, BValue b13, BValue b14, BValue b15, BValue b16, BValue b17, BValue b18, BValue b19, BValue b20, BValue b21, BValue b22, BValue b23, BValue b24, BValue b25, BValue b26, BValue b27, BValue b28, BValue b29, BValue b30, BValue b31) {
    Struct32* rc = malloc(sizeof(Struct32));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = (FreeFn)free_struct32;
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
    return (BValue)rc;
}

// ENUMS
DEFINE_RC_ENUM(Enum1,BValue _0;);

void free_enum1(Enum1* s) {
    release_value(s->_0);
    free(s);
}
BValue alloc_enum1(ENUM_TAG tag, BValue b0) {
    Enum1* rc = malloc(sizeof(Enum1));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = (FreeFn)free_enum1;
    rc->tag = tag;
    rc->_0 = b0;
    return (BValue)rc;
}

DEFINE_RC_ENUM(Enum2,BValue _0;BValue _1;);

void free_enum2(Enum2* s) {
    release_value(s->_0);
    release_value(s->_1);
    free(s);
}
BValue alloc_enum2(ENUM_TAG tag, BValue b0, BValue b1) {
    Enum2* rc = malloc(sizeof(Enum2));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = (FreeFn)free_enum2;
    rc->tag = tag;
    rc->_0 = b0;
    rc->_1 = b1;
    return (BValue)rc;
}

DEFINE_RC_ENUM(Enum3,BValue _0;BValue _1;BValue _2;);

void free_enum3(Enum3* s) {
    release_value(s->_0);
    release_value(s->_1);
    release_value(s->_2);
    free(s);
}
BValue alloc_enum3(ENUM_TAG tag, BValue b0, BValue b1, BValue b2) {
    Enum3* rc = malloc(sizeof(Enum3));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = (FreeFn)free_enum3;
    rc->tag = tag;
    rc->_0 = b0;
    rc->_1 = b1;
    rc->_2 = b2;
    return (BValue)rc;
}

DEFINE_RC_ENUM(Enum4,BValue _0;BValue _1;BValue _2;BValue _3;);

void free_enum4(Enum4* s) {
    release_value(s->_0);
    release_value(s->_1);
    release_value(s->_2);
    release_value(s->_3);
    free(s);
}
BValue alloc_enum4(ENUM_TAG tag, BValue b0, BValue b1, BValue b2, BValue b3) {
    Enum4* rc = malloc(sizeof(Enum4));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = (FreeFn)free_enum4;
    rc->tag = tag;
    rc->_0 = b0;
    rc->_1 = b1;
    rc->_2 = b2;
    rc->_3 = b3;
    return (BValue)rc;
}

DEFINE_RC_ENUM(Enum5,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;);

void free_enum5(Enum5* s) {
    release_value(s->_0);
    release_value(s->_1);
    release_value(s->_2);
    release_value(s->_3);
    release_value(s->_4);
    free(s);
}
BValue alloc_enum5(ENUM_TAG tag, BValue b0, BValue b1, BValue b2, BValue b3, BValue b4) {
    Enum5* rc = malloc(sizeof(Enum5));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = (FreeFn)free_enum5;
    rc->tag = tag;
    rc->_0 = b0;
    rc->_1 = b1;
    rc->_2 = b2;
    rc->_3 = b3;
    rc->_4 = b4;
    return (BValue)rc;
}

DEFINE_RC_ENUM(Enum6,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;);

void free_enum6(Enum6* s) {
    release_value(s->_0);
    release_value(s->_1);
    release_value(s->_2);
    release_value(s->_3);
    release_value(s->_4);
    release_value(s->_5);
    free(s);
}
BValue alloc_enum6(ENUM_TAG tag, BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5) {
    Enum6* rc = malloc(sizeof(Enum6));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = (FreeFn)free_enum6;
    rc->tag = tag;
    rc->_0 = b0;
    rc->_1 = b1;
    rc->_2 = b2;
    rc->_3 = b3;
    rc->_4 = b4;
    rc->_5 = b5;
    return (BValue)rc;
}

DEFINE_RC_ENUM(Enum7,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;BValue _6;);

void free_enum7(Enum7* s) {
    release_value(s->_0);
    release_value(s->_1);
    release_value(s->_2);
    release_value(s->_3);
    release_value(s->_4);
    release_value(s->_5);
    release_value(s->_6);
    free(s);
}
BValue alloc_enum7(ENUM_TAG tag, BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5, BValue b6) {
    Enum7* rc = malloc(sizeof(Enum7));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = (FreeFn)free_enum7;
    rc->tag = tag;
    rc->_0 = b0;
    rc->_1 = b1;
    rc->_2 = b2;
    rc->_3 = b3;
    rc->_4 = b4;
    rc->_5 = b5;
    rc->_6 = b6;
    return (BValue)rc;
}

DEFINE_RC_ENUM(Enum8,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;BValue _6;BValue _7;);

void free_enum8(Enum8* s) {
    release_value(s->_0);
    release_value(s->_1);
    release_value(s->_2);
    release_value(s->_3);
    release_value(s->_4);
    release_value(s->_5);
    release_value(s->_6);
    release_value(s->_7);
    free(s);
}
BValue alloc_enum8(ENUM_TAG tag, BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5, BValue b6, BValue b7) {
    Enum8* rc = malloc(sizeof(Enum8));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = (FreeFn)free_enum8;
    rc->tag = tag;
    rc->_0 = b0;
    rc->_1 = b1;
    rc->_2 = b2;
    rc->_3 = b3;
    rc->_4 = b4;
    rc->_5 = b5;
    rc->_6 = b6;
    rc->_7 = b7;
    return (BValue)rc;
}

DEFINE_RC_ENUM(Enum9,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;BValue _6;BValue _7;BValue _8;);

void free_enum9(Enum9* s) {
    release_value(s->_0);
    release_value(s->_1);
    release_value(s->_2);
    release_value(s->_3);
    release_value(s->_4);
    release_value(s->_5);
    release_value(s->_6);
    release_value(s->_7);
    release_value(s->_8);
    free(s);
}
BValue alloc_enum9(ENUM_TAG tag, BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5, BValue b6, BValue b7, BValue b8) {
    Enum9* rc = malloc(sizeof(Enum9));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = (FreeFn)free_enum9;
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
    return (BValue)rc;
}

DEFINE_RC_ENUM(Enum10,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;BValue _6;BValue _7;BValue _8;BValue _9;);

void free_enum10(Enum10* s) {
    release_value(s->_0);
    release_value(s->_1);
    release_value(s->_2);
    release_value(s->_3);
    release_value(s->_4);
    release_value(s->_5);
    release_value(s->_6);
    release_value(s->_7);
    release_value(s->_8);
    release_value(s->_9);
    free(s);
}
BValue alloc_enum10(ENUM_TAG tag, BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5, BValue b6, BValue b7, BValue b8, BValue b9) {
    Enum10* rc = malloc(sizeof(Enum10));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = (FreeFn)free_enum10;
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
    return (BValue)rc;
}

DEFINE_RC_ENUM(Enum11,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;BValue _6;BValue _7;BValue _8;BValue _9;BValue _10;);

void free_enum11(Enum11* s) {
    release_value(s->_0);
    release_value(s->_1);
    release_value(s->_2);
    release_value(s->_3);
    release_value(s->_4);
    release_value(s->_5);
    release_value(s->_6);
    release_value(s->_7);
    release_value(s->_8);
    release_value(s->_9);
    release_value(s->_10);
    free(s);
}
BValue alloc_enum11(ENUM_TAG tag, BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5, BValue b6, BValue b7, BValue b8, BValue b9, BValue b10) {
    Enum11* rc = malloc(sizeof(Enum11));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = (FreeFn)free_enum11;
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
    return (BValue)rc;
}

DEFINE_RC_ENUM(Enum12,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;BValue _6;BValue _7;BValue _8;BValue _9;BValue _10;BValue _11;);

void free_enum12(Enum12* s) {
    release_value(s->_0);
    release_value(s->_1);
    release_value(s->_2);
    release_value(s->_3);
    release_value(s->_4);
    release_value(s->_5);
    release_value(s->_6);
    release_value(s->_7);
    release_value(s->_8);
    release_value(s->_9);
    release_value(s->_10);
    release_value(s->_11);
    free(s);
}
BValue alloc_enum12(ENUM_TAG tag, BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5, BValue b6, BValue b7, BValue b8, BValue b9, BValue b10, BValue b11) {
    Enum12* rc = malloc(sizeof(Enum12));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = (FreeFn)free_enum12;
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
    return (BValue)rc;
}

DEFINE_RC_ENUM(Enum13,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;BValue _6;BValue _7;BValue _8;BValue _9;BValue _10;BValue _11;BValue _12;);

void free_enum13(Enum13* s) {
    release_value(s->_0);
    release_value(s->_1);
    release_value(s->_2);
    release_value(s->_3);
    release_value(s->_4);
    release_value(s->_5);
    release_value(s->_6);
    release_value(s->_7);
    release_value(s->_8);
    release_value(s->_9);
    release_value(s->_10);
    release_value(s->_11);
    release_value(s->_12);
    free(s);
}
BValue alloc_enum13(ENUM_TAG tag, BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5, BValue b6, BValue b7, BValue b8, BValue b9, BValue b10, BValue b11, BValue b12) {
    Enum13* rc = malloc(sizeof(Enum13));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = (FreeFn)free_enum13;
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
    return (BValue)rc;
}

DEFINE_RC_ENUM(Enum14,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;BValue _6;BValue _7;BValue _8;BValue _9;BValue _10;BValue _11;BValue _12;BValue _13;);

void free_enum14(Enum14* s) {
    release_value(s->_0);
    release_value(s->_1);
    release_value(s->_2);
    release_value(s->_3);
    release_value(s->_4);
    release_value(s->_5);
    release_value(s->_6);
    release_value(s->_7);
    release_value(s->_8);
    release_value(s->_9);
    release_value(s->_10);
    release_value(s->_11);
    release_value(s->_12);
    release_value(s->_13);
    free(s);
}
BValue alloc_enum14(ENUM_TAG tag, BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5, BValue b6, BValue b7, BValue b8, BValue b9, BValue b10, BValue b11, BValue b12, BValue b13) {
    Enum14* rc = malloc(sizeof(Enum14));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = (FreeFn)free_enum14;
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
    return (BValue)rc;
}

DEFINE_RC_ENUM(Enum15,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;BValue _6;BValue _7;BValue _8;BValue _9;BValue _10;BValue _11;BValue _12;BValue _13;BValue _14;);

void free_enum15(Enum15* s) {
    release_value(s->_0);
    release_value(s->_1);
    release_value(s->_2);
    release_value(s->_3);
    release_value(s->_4);
    release_value(s->_5);
    release_value(s->_6);
    release_value(s->_7);
    release_value(s->_8);
    release_value(s->_9);
    release_value(s->_10);
    release_value(s->_11);
    release_value(s->_12);
    release_value(s->_13);
    release_value(s->_14);
    free(s);
}
BValue alloc_enum15(ENUM_TAG tag, BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5, BValue b6, BValue b7, BValue b8, BValue b9, BValue b10, BValue b11, BValue b12, BValue b13, BValue b14) {
    Enum15* rc = malloc(sizeof(Enum15));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = (FreeFn)free_enum15;
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
    return (BValue)rc;
}

DEFINE_RC_ENUM(Enum16,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;BValue _6;BValue _7;BValue _8;BValue _9;BValue _10;BValue _11;BValue _12;BValue _13;BValue _14;BValue _15;);

void free_enum16(Enum16* s) {
    release_value(s->_0);
    release_value(s->_1);
    release_value(s->_2);
    release_value(s->_3);
    release_value(s->_4);
    release_value(s->_5);
    release_value(s->_6);
    release_value(s->_7);
    release_value(s->_8);
    release_value(s->_9);
    release_value(s->_10);
    release_value(s->_11);
    release_value(s->_12);
    release_value(s->_13);
    release_value(s->_14);
    release_value(s->_15);
    free(s);
}
BValue alloc_enum16(ENUM_TAG tag, BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5, BValue b6, BValue b7, BValue b8, BValue b9, BValue b10, BValue b11, BValue b12, BValue b13, BValue b14, BValue b15) {
    Enum16* rc = malloc(sizeof(Enum16));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = (FreeFn)free_enum16;
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
    return (BValue)rc;
}

DEFINE_RC_ENUM(Enum17,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;BValue _6;BValue _7;BValue _8;BValue _9;BValue _10;BValue _11;BValue _12;BValue _13;BValue _14;BValue _15;BValue _16;);

void free_enum17(Enum17* s) {
    release_value(s->_0);
    release_value(s->_1);
    release_value(s->_2);
    release_value(s->_3);
    release_value(s->_4);
    release_value(s->_5);
    release_value(s->_6);
    release_value(s->_7);
    release_value(s->_8);
    release_value(s->_9);
    release_value(s->_10);
    release_value(s->_11);
    release_value(s->_12);
    release_value(s->_13);
    release_value(s->_14);
    release_value(s->_15);
    release_value(s->_16);
    free(s);
}
BValue alloc_enum17(ENUM_TAG tag, BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5, BValue b6, BValue b7, BValue b8, BValue b9, BValue b10, BValue b11, BValue b12, BValue b13, BValue b14, BValue b15, BValue b16) {
    Enum17* rc = malloc(sizeof(Enum17));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = (FreeFn)free_enum17;
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
    return (BValue)rc;
}

DEFINE_RC_ENUM(Enum18,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;BValue _6;BValue _7;BValue _8;BValue _9;BValue _10;BValue _11;BValue _12;BValue _13;BValue _14;BValue _15;BValue _16;BValue _17;);

void free_enum18(Enum18* s) {
    release_value(s->_0);
    release_value(s->_1);
    release_value(s->_2);
    release_value(s->_3);
    release_value(s->_4);
    release_value(s->_5);
    release_value(s->_6);
    release_value(s->_7);
    release_value(s->_8);
    release_value(s->_9);
    release_value(s->_10);
    release_value(s->_11);
    release_value(s->_12);
    release_value(s->_13);
    release_value(s->_14);
    release_value(s->_15);
    release_value(s->_16);
    release_value(s->_17);
    free(s);
}
BValue alloc_enum18(ENUM_TAG tag, BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5, BValue b6, BValue b7, BValue b8, BValue b9, BValue b10, BValue b11, BValue b12, BValue b13, BValue b14, BValue b15, BValue b16, BValue b17) {
    Enum18* rc = malloc(sizeof(Enum18));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = (FreeFn)free_enum18;
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
    return (BValue)rc;
}

DEFINE_RC_ENUM(Enum19,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;BValue _6;BValue _7;BValue _8;BValue _9;BValue _10;BValue _11;BValue _12;BValue _13;BValue _14;BValue _15;BValue _16;BValue _17;BValue _18;);

void free_enum19(Enum19* s) {
    release_value(s->_0);
    release_value(s->_1);
    release_value(s->_2);
    release_value(s->_3);
    release_value(s->_4);
    release_value(s->_5);
    release_value(s->_6);
    release_value(s->_7);
    release_value(s->_8);
    release_value(s->_9);
    release_value(s->_10);
    release_value(s->_11);
    release_value(s->_12);
    release_value(s->_13);
    release_value(s->_14);
    release_value(s->_15);
    release_value(s->_16);
    release_value(s->_17);
    release_value(s->_18);
    free(s);
}
BValue alloc_enum19(ENUM_TAG tag, BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5, BValue b6, BValue b7, BValue b8, BValue b9, BValue b10, BValue b11, BValue b12, BValue b13, BValue b14, BValue b15, BValue b16, BValue b17, BValue b18) {
    Enum19* rc = malloc(sizeof(Enum19));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = (FreeFn)free_enum19;
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
    return (BValue)rc;
}

DEFINE_RC_ENUM(Enum20,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;BValue _6;BValue _7;BValue _8;BValue _9;BValue _10;BValue _11;BValue _12;BValue _13;BValue _14;BValue _15;BValue _16;BValue _17;BValue _18;BValue _19;);

void free_enum20(Enum20* s) {
    release_value(s->_0);
    release_value(s->_1);
    release_value(s->_2);
    release_value(s->_3);
    release_value(s->_4);
    release_value(s->_5);
    release_value(s->_6);
    release_value(s->_7);
    release_value(s->_8);
    release_value(s->_9);
    release_value(s->_10);
    release_value(s->_11);
    release_value(s->_12);
    release_value(s->_13);
    release_value(s->_14);
    release_value(s->_15);
    release_value(s->_16);
    release_value(s->_17);
    release_value(s->_18);
    release_value(s->_19);
    free(s);
}
BValue alloc_enum20(ENUM_TAG tag, BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5, BValue b6, BValue b7, BValue b8, BValue b9, BValue b10, BValue b11, BValue b12, BValue b13, BValue b14, BValue b15, BValue b16, BValue b17, BValue b18, BValue b19) {
    Enum20* rc = malloc(sizeof(Enum20));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = (FreeFn)free_enum20;
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
    return (BValue)rc;
}

DEFINE_RC_ENUM(Enum21,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;BValue _6;BValue _7;BValue _8;BValue _9;BValue _10;BValue _11;BValue _12;BValue _13;BValue _14;BValue _15;BValue _16;BValue _17;BValue _18;BValue _19;BValue _20;);

void free_enum21(Enum21* s) {
    release_value(s->_0);
    release_value(s->_1);
    release_value(s->_2);
    release_value(s->_3);
    release_value(s->_4);
    release_value(s->_5);
    release_value(s->_6);
    release_value(s->_7);
    release_value(s->_8);
    release_value(s->_9);
    release_value(s->_10);
    release_value(s->_11);
    release_value(s->_12);
    release_value(s->_13);
    release_value(s->_14);
    release_value(s->_15);
    release_value(s->_16);
    release_value(s->_17);
    release_value(s->_18);
    release_value(s->_19);
    release_value(s->_20);
    free(s);
}
BValue alloc_enum21(ENUM_TAG tag, BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5, BValue b6, BValue b7, BValue b8, BValue b9, BValue b10, BValue b11, BValue b12, BValue b13, BValue b14, BValue b15, BValue b16, BValue b17, BValue b18, BValue b19, BValue b20) {
    Enum21* rc = malloc(sizeof(Enum21));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = (FreeFn)free_enum21;
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
    return (BValue)rc;
}

DEFINE_RC_ENUM(Enum22,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;BValue _6;BValue _7;BValue _8;BValue _9;BValue _10;BValue _11;BValue _12;BValue _13;BValue _14;BValue _15;BValue _16;BValue _17;BValue _18;BValue _19;BValue _20;BValue _21;);

void free_enum22(Enum22* s) {
    release_value(s->_0);
    release_value(s->_1);
    release_value(s->_2);
    release_value(s->_3);
    release_value(s->_4);
    release_value(s->_5);
    release_value(s->_6);
    release_value(s->_7);
    release_value(s->_8);
    release_value(s->_9);
    release_value(s->_10);
    release_value(s->_11);
    release_value(s->_12);
    release_value(s->_13);
    release_value(s->_14);
    release_value(s->_15);
    release_value(s->_16);
    release_value(s->_17);
    release_value(s->_18);
    release_value(s->_19);
    release_value(s->_20);
    release_value(s->_21);
    free(s);
}
BValue alloc_enum22(ENUM_TAG tag, BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5, BValue b6, BValue b7, BValue b8, BValue b9, BValue b10, BValue b11, BValue b12, BValue b13, BValue b14, BValue b15, BValue b16, BValue b17, BValue b18, BValue b19, BValue b20, BValue b21) {
    Enum22* rc = malloc(sizeof(Enum22));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = (FreeFn)free_enum22;
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
    return (BValue)rc;
}

DEFINE_RC_ENUM(Enum23,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;BValue _6;BValue _7;BValue _8;BValue _9;BValue _10;BValue _11;BValue _12;BValue _13;BValue _14;BValue _15;BValue _16;BValue _17;BValue _18;BValue _19;BValue _20;BValue _21;BValue _22;);

void free_enum23(Enum23* s) {
    release_value(s->_0);
    release_value(s->_1);
    release_value(s->_2);
    release_value(s->_3);
    release_value(s->_4);
    release_value(s->_5);
    release_value(s->_6);
    release_value(s->_7);
    release_value(s->_8);
    release_value(s->_9);
    release_value(s->_10);
    release_value(s->_11);
    release_value(s->_12);
    release_value(s->_13);
    release_value(s->_14);
    release_value(s->_15);
    release_value(s->_16);
    release_value(s->_17);
    release_value(s->_18);
    release_value(s->_19);
    release_value(s->_20);
    release_value(s->_21);
    release_value(s->_22);
    free(s);
}
BValue alloc_enum23(ENUM_TAG tag, BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5, BValue b6, BValue b7, BValue b8, BValue b9, BValue b10, BValue b11, BValue b12, BValue b13, BValue b14, BValue b15, BValue b16, BValue b17, BValue b18, BValue b19, BValue b20, BValue b21, BValue b22) {
    Enum23* rc = malloc(sizeof(Enum23));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = (FreeFn)free_enum23;
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
    return (BValue)rc;
}

DEFINE_RC_ENUM(Enum24,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;BValue _6;BValue _7;BValue _8;BValue _9;BValue _10;BValue _11;BValue _12;BValue _13;BValue _14;BValue _15;BValue _16;BValue _17;BValue _18;BValue _19;BValue _20;BValue _21;BValue _22;BValue _23;);

void free_enum24(Enum24* s) {
    release_value(s->_0);
    release_value(s->_1);
    release_value(s->_2);
    release_value(s->_3);
    release_value(s->_4);
    release_value(s->_5);
    release_value(s->_6);
    release_value(s->_7);
    release_value(s->_8);
    release_value(s->_9);
    release_value(s->_10);
    release_value(s->_11);
    release_value(s->_12);
    release_value(s->_13);
    release_value(s->_14);
    release_value(s->_15);
    release_value(s->_16);
    release_value(s->_17);
    release_value(s->_18);
    release_value(s->_19);
    release_value(s->_20);
    release_value(s->_21);
    release_value(s->_22);
    release_value(s->_23);
    free(s);
}
BValue alloc_enum24(ENUM_TAG tag, BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5, BValue b6, BValue b7, BValue b8, BValue b9, BValue b10, BValue b11, BValue b12, BValue b13, BValue b14, BValue b15, BValue b16, BValue b17, BValue b18, BValue b19, BValue b20, BValue b21, BValue b22, BValue b23) {
    Enum24* rc = malloc(sizeof(Enum24));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = (FreeFn)free_enum24;
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
    return (BValue)rc;
}

DEFINE_RC_ENUM(Enum25,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;BValue _6;BValue _7;BValue _8;BValue _9;BValue _10;BValue _11;BValue _12;BValue _13;BValue _14;BValue _15;BValue _16;BValue _17;BValue _18;BValue _19;BValue _20;BValue _21;BValue _22;BValue _23;BValue _24;);

void free_enum25(Enum25* s) {
    release_value(s->_0);
    release_value(s->_1);
    release_value(s->_2);
    release_value(s->_3);
    release_value(s->_4);
    release_value(s->_5);
    release_value(s->_6);
    release_value(s->_7);
    release_value(s->_8);
    release_value(s->_9);
    release_value(s->_10);
    release_value(s->_11);
    release_value(s->_12);
    release_value(s->_13);
    release_value(s->_14);
    release_value(s->_15);
    release_value(s->_16);
    release_value(s->_17);
    release_value(s->_18);
    release_value(s->_19);
    release_value(s->_20);
    release_value(s->_21);
    release_value(s->_22);
    release_value(s->_23);
    release_value(s->_24);
    free(s);
}
BValue alloc_enum25(ENUM_TAG tag, BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5, BValue b6, BValue b7, BValue b8, BValue b9, BValue b10, BValue b11, BValue b12, BValue b13, BValue b14, BValue b15, BValue b16, BValue b17, BValue b18, BValue b19, BValue b20, BValue b21, BValue b22, BValue b23, BValue b24) {
    Enum25* rc = malloc(sizeof(Enum25));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = (FreeFn)free_enum25;
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
    return (BValue)rc;
}

DEFINE_RC_ENUM(Enum26,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;BValue _6;BValue _7;BValue _8;BValue _9;BValue _10;BValue _11;BValue _12;BValue _13;BValue _14;BValue _15;BValue _16;BValue _17;BValue _18;BValue _19;BValue _20;BValue _21;BValue _22;BValue _23;BValue _24;BValue _25;);

void free_enum26(Enum26* s) {
    release_value(s->_0);
    release_value(s->_1);
    release_value(s->_2);
    release_value(s->_3);
    release_value(s->_4);
    release_value(s->_5);
    release_value(s->_6);
    release_value(s->_7);
    release_value(s->_8);
    release_value(s->_9);
    release_value(s->_10);
    release_value(s->_11);
    release_value(s->_12);
    release_value(s->_13);
    release_value(s->_14);
    release_value(s->_15);
    release_value(s->_16);
    release_value(s->_17);
    release_value(s->_18);
    release_value(s->_19);
    release_value(s->_20);
    release_value(s->_21);
    release_value(s->_22);
    release_value(s->_23);
    release_value(s->_24);
    release_value(s->_25);
    free(s);
}
BValue alloc_enum26(ENUM_TAG tag, BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5, BValue b6, BValue b7, BValue b8, BValue b9, BValue b10, BValue b11, BValue b12, BValue b13, BValue b14, BValue b15, BValue b16, BValue b17, BValue b18, BValue b19, BValue b20, BValue b21, BValue b22, BValue b23, BValue b24, BValue b25) {
    Enum26* rc = malloc(sizeof(Enum26));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = (FreeFn)free_enum26;
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
    return (BValue)rc;
}

DEFINE_RC_ENUM(Enum27,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;BValue _6;BValue _7;BValue _8;BValue _9;BValue _10;BValue _11;BValue _12;BValue _13;BValue _14;BValue _15;BValue _16;BValue _17;BValue _18;BValue _19;BValue _20;BValue _21;BValue _22;BValue _23;BValue _24;BValue _25;BValue _26;);

void free_enum27(Enum27* s) {
    release_value(s->_0);
    release_value(s->_1);
    release_value(s->_2);
    release_value(s->_3);
    release_value(s->_4);
    release_value(s->_5);
    release_value(s->_6);
    release_value(s->_7);
    release_value(s->_8);
    release_value(s->_9);
    release_value(s->_10);
    release_value(s->_11);
    release_value(s->_12);
    release_value(s->_13);
    release_value(s->_14);
    release_value(s->_15);
    release_value(s->_16);
    release_value(s->_17);
    release_value(s->_18);
    release_value(s->_19);
    release_value(s->_20);
    release_value(s->_21);
    release_value(s->_22);
    release_value(s->_23);
    release_value(s->_24);
    release_value(s->_25);
    release_value(s->_26);
    free(s);
}
BValue alloc_enum27(ENUM_TAG tag, BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5, BValue b6, BValue b7, BValue b8, BValue b9, BValue b10, BValue b11, BValue b12, BValue b13, BValue b14, BValue b15, BValue b16, BValue b17, BValue b18, BValue b19, BValue b20, BValue b21, BValue b22, BValue b23, BValue b24, BValue b25, BValue b26) {
    Enum27* rc = malloc(sizeof(Enum27));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = (FreeFn)free_enum27;
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
    return (BValue)rc;
}

DEFINE_RC_ENUM(Enum28,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;BValue _6;BValue _7;BValue _8;BValue _9;BValue _10;BValue _11;BValue _12;BValue _13;BValue _14;BValue _15;BValue _16;BValue _17;BValue _18;BValue _19;BValue _20;BValue _21;BValue _22;BValue _23;BValue _24;BValue _25;BValue _26;BValue _27;);

void free_enum28(Enum28* s) {
    release_value(s->_0);
    release_value(s->_1);
    release_value(s->_2);
    release_value(s->_3);
    release_value(s->_4);
    release_value(s->_5);
    release_value(s->_6);
    release_value(s->_7);
    release_value(s->_8);
    release_value(s->_9);
    release_value(s->_10);
    release_value(s->_11);
    release_value(s->_12);
    release_value(s->_13);
    release_value(s->_14);
    release_value(s->_15);
    release_value(s->_16);
    release_value(s->_17);
    release_value(s->_18);
    release_value(s->_19);
    release_value(s->_20);
    release_value(s->_21);
    release_value(s->_22);
    release_value(s->_23);
    release_value(s->_24);
    release_value(s->_25);
    release_value(s->_26);
    release_value(s->_27);
    free(s);
}
BValue alloc_enum28(ENUM_TAG tag, BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5, BValue b6, BValue b7, BValue b8, BValue b9, BValue b10, BValue b11, BValue b12, BValue b13, BValue b14, BValue b15, BValue b16, BValue b17, BValue b18, BValue b19, BValue b20, BValue b21, BValue b22, BValue b23, BValue b24, BValue b25, BValue b26, BValue b27) {
    Enum28* rc = malloc(sizeof(Enum28));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = (FreeFn)free_enum28;
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
    return (BValue)rc;
}

DEFINE_RC_ENUM(Enum29,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;BValue _6;BValue _7;BValue _8;BValue _9;BValue _10;BValue _11;BValue _12;BValue _13;BValue _14;BValue _15;BValue _16;BValue _17;BValue _18;BValue _19;BValue _20;BValue _21;BValue _22;BValue _23;BValue _24;BValue _25;BValue _26;BValue _27;BValue _28;);

void free_enum29(Enum29* s) {
    release_value(s->_0);
    release_value(s->_1);
    release_value(s->_2);
    release_value(s->_3);
    release_value(s->_4);
    release_value(s->_5);
    release_value(s->_6);
    release_value(s->_7);
    release_value(s->_8);
    release_value(s->_9);
    release_value(s->_10);
    release_value(s->_11);
    release_value(s->_12);
    release_value(s->_13);
    release_value(s->_14);
    release_value(s->_15);
    release_value(s->_16);
    release_value(s->_17);
    release_value(s->_18);
    release_value(s->_19);
    release_value(s->_20);
    release_value(s->_21);
    release_value(s->_22);
    release_value(s->_23);
    release_value(s->_24);
    release_value(s->_25);
    release_value(s->_26);
    release_value(s->_27);
    release_value(s->_28);
    free(s);
}
BValue alloc_enum29(ENUM_TAG tag, BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5, BValue b6, BValue b7, BValue b8, BValue b9, BValue b10, BValue b11, BValue b12, BValue b13, BValue b14, BValue b15, BValue b16, BValue b17, BValue b18, BValue b19, BValue b20, BValue b21, BValue b22, BValue b23, BValue b24, BValue b25, BValue b26, BValue b27, BValue b28) {
    Enum29* rc = malloc(sizeof(Enum29));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = (FreeFn)free_enum29;
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
    return (BValue)rc;
}

DEFINE_RC_ENUM(Enum30,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;BValue _6;BValue _7;BValue _8;BValue _9;BValue _10;BValue _11;BValue _12;BValue _13;BValue _14;BValue _15;BValue _16;BValue _17;BValue _18;BValue _19;BValue _20;BValue _21;BValue _22;BValue _23;BValue _24;BValue _25;BValue _26;BValue _27;BValue _28;BValue _29;);

void free_enum30(Enum30* s) {
    release_value(s->_0);
    release_value(s->_1);
    release_value(s->_2);
    release_value(s->_3);
    release_value(s->_4);
    release_value(s->_5);
    release_value(s->_6);
    release_value(s->_7);
    release_value(s->_8);
    release_value(s->_9);
    release_value(s->_10);
    release_value(s->_11);
    release_value(s->_12);
    release_value(s->_13);
    release_value(s->_14);
    release_value(s->_15);
    release_value(s->_16);
    release_value(s->_17);
    release_value(s->_18);
    release_value(s->_19);
    release_value(s->_20);
    release_value(s->_21);
    release_value(s->_22);
    release_value(s->_23);
    release_value(s->_24);
    release_value(s->_25);
    release_value(s->_26);
    release_value(s->_27);
    release_value(s->_28);
    release_value(s->_29);
    free(s);
}
BValue alloc_enum30(ENUM_TAG tag, BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5, BValue b6, BValue b7, BValue b8, BValue b9, BValue b10, BValue b11, BValue b12, BValue b13, BValue b14, BValue b15, BValue b16, BValue b17, BValue b18, BValue b19, BValue b20, BValue b21, BValue b22, BValue b23, BValue b24, BValue b25, BValue b26, BValue b27, BValue b28, BValue b29) {
    Enum30* rc = malloc(sizeof(Enum30));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = (FreeFn)free_enum30;
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
    return (BValue)rc;
}

DEFINE_RC_ENUM(Enum31,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;BValue _6;BValue _7;BValue _8;BValue _9;BValue _10;BValue _11;BValue _12;BValue _13;BValue _14;BValue _15;BValue _16;BValue _17;BValue _18;BValue _19;BValue _20;BValue _21;BValue _22;BValue _23;BValue _24;BValue _25;BValue _26;BValue _27;BValue _28;BValue _29;BValue _30;);

void free_enum31(Enum31* s) {
    release_value(s->_0);
    release_value(s->_1);
    release_value(s->_2);
    release_value(s->_3);
    release_value(s->_4);
    release_value(s->_5);
    release_value(s->_6);
    release_value(s->_7);
    release_value(s->_8);
    release_value(s->_9);
    release_value(s->_10);
    release_value(s->_11);
    release_value(s->_12);
    release_value(s->_13);
    release_value(s->_14);
    release_value(s->_15);
    release_value(s->_16);
    release_value(s->_17);
    release_value(s->_18);
    release_value(s->_19);
    release_value(s->_20);
    release_value(s->_21);
    release_value(s->_22);
    release_value(s->_23);
    release_value(s->_24);
    release_value(s->_25);
    release_value(s->_26);
    release_value(s->_27);
    release_value(s->_28);
    release_value(s->_29);
    release_value(s->_30);
    free(s);
}
BValue alloc_enum31(ENUM_TAG tag, BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5, BValue b6, BValue b7, BValue b8, BValue b9, BValue b10, BValue b11, BValue b12, BValue b13, BValue b14, BValue b15, BValue b16, BValue b17, BValue b18, BValue b19, BValue b20, BValue b21, BValue b22, BValue b23, BValue b24, BValue b25, BValue b26, BValue b27, BValue b28, BValue b29, BValue b30) {
    Enum31* rc = malloc(sizeof(Enum31));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = (FreeFn)free_enum31;
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
    return (BValue)rc;
}

DEFINE_RC_ENUM(Enum32,BValue _0;BValue _1;BValue _2;BValue _3;BValue _4;BValue _5;BValue _6;BValue _7;BValue _8;BValue _9;BValue _10;BValue _11;BValue _12;BValue _13;BValue _14;BValue _15;BValue _16;BValue _17;BValue _18;BValue _19;BValue _20;BValue _21;BValue _22;BValue _23;BValue _24;BValue _25;BValue _26;BValue _27;BValue _28;BValue _29;BValue _30;BValue _31;);

void free_enum32(Enum32* s) {
    release_value(s->_0);
    release_value(s->_1);
    release_value(s->_2);
    release_value(s->_3);
    release_value(s->_4);
    release_value(s->_5);
    release_value(s->_6);
    release_value(s->_7);
    release_value(s->_8);
    release_value(s->_9);
    release_value(s->_10);
    release_value(s->_11);
    release_value(s->_12);
    release_value(s->_13);
    release_value(s->_14);
    release_value(s->_15);
    release_value(s->_16);
    release_value(s->_17);
    release_value(s->_18);
    release_value(s->_19);
    release_value(s->_20);
    release_value(s->_21);
    release_value(s->_22);
    release_value(s->_23);
    release_value(s->_24);
    release_value(s->_25);
    release_value(s->_26);
    release_value(s->_27);
    release_value(s->_28);
    release_value(s->_29);
    release_value(s->_30);
    release_value(s->_31);
    free(s);
}
BValue alloc_enum32(ENUM_TAG tag, BValue b0, BValue b1, BValue b2, BValue b3, BValue b4, BValue b5, BValue b6, BValue b7, BValue b8, BValue b9, BValue b10, BValue b11, BValue b12, BValue b13, BValue b14, BValue b15, BValue b16, BValue b17, BValue b18, BValue b19, BValue b20, BValue b21, BValue b22, BValue b23, BValue b24, BValue b25, BValue b26, BValue b27, BValue b28, BValue b29, BValue b30, BValue b31) {
    Enum32* rc = malloc(sizeof(Enum32));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = (FreeFn)free_enum32;
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
    return (BValue)rc;
}

// FUNCTIONS

DEFINE_RC_STRUCT(BoxedPureFn1, BPureFn1 fn; size_t slot_len;);

BValue alloc_closure1(size_t size, BValue* data, BClosure1 fn) {
    Closure1Data* rc = malloc(closure_data_size(size));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = (FreeFn)free_closure;
    rc->fn = fn;
    rc->slot_len = size;
    BValue* closure_data = closure_data_of(rc);
    for (size_t i = 0; i < size; i++) {
      closure_data[i] = data[i];
    }
    return (BValue)rc;
}

BValue alloc_boxed_pure_fn1(BPureFn1 fn) {
    BoxedPureFn1* rc = (BoxedPureFn1*)malloc(sizeof(BoxedPureFn1));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = free;
    rc->fn = fn;
    rc->slot_len = 0;
    return (BValue)rc;
}

BValue call_fn1(BValue fn, BValue arg0) {
  BValue ptr = (BValue)TO_POINTER(fn);
  BoxedPureFn1* purefn = (BoxedPureFn1*)ptr;
  if (purefn->slot_len == 0) {
    return purefn->fn(arg0);
  }
  else {
    // this must be a closure:
    Closure1Data* rc = (Closure1Data*)ptr;
    BValue* data = closure_data_of(rc);
    return rc->fn(data, arg0);
  }
}


DEFINE_RC_STRUCT(Closure2Data, BClosure2 fn; size_t slot_len;);
DEFINE_RC_STRUCT(BoxedPureFn2, BPureFn2 fn; size_t slot_len;);

BValue alloc_closure2(size_t size, BValue* data, BClosure2 fn) {
    Closure2Data* rc = malloc(closure_data_size(size));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = (FreeFn)free_closure;
    rc->fn = fn;
    rc->slot_len = size;
    BValue* closure_data = closure_data_of((Closure1Data*)rc);
    for (size_t i = 0; i < size; i++) {
      closure_data[i] = data[i];
    }
    return (BValue)rc;
}

BValue alloc_boxed_pure_fn2(BPureFn2 fn) {
    BoxedPureFn2* rc = (BoxedPureFn2*)malloc(sizeof(BoxedPureFn2));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = free;
    rc->fn = fn;
    rc->slot_len = 0;
    return (BValue)rc;
}

BValue call_fn2(BValue fn, BValue arg0, BValue arg1) {
  BValue ptr = (BValue)TO_POINTER(fn);
  BoxedPureFn2* purefn = (BoxedPureFn2*)ptr;
  if (purefn->slot_len == 0) {
    return purefn->fn(arg0, arg1);
  }
  else {
    // this must be a closure:
    Closure2Data* rc = (Closure2Data*)ptr;
    BValue* data = closure_data_of((Closure1Data*)rc);
    return rc->fn(data, arg0, arg1);
  }
}


DEFINE_RC_STRUCT(Closure3Data, BClosure3 fn; size_t slot_len;);
DEFINE_RC_STRUCT(BoxedPureFn3, BPureFn3 fn; size_t slot_len;);

BValue alloc_closure3(size_t size, BValue* data, BClosure3 fn) {
    Closure3Data* rc = malloc(closure_data_size(size));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = (FreeFn)free_closure;
    rc->fn = fn;
    rc->slot_len = size;
    BValue* closure_data = closure_data_of((Closure1Data*)rc);
    for (size_t i = 0; i < size; i++) {
      closure_data[i] = data[i];
    }
    return (BValue)rc;
}

BValue alloc_boxed_pure_fn3(BPureFn3 fn) {
    BoxedPureFn3* rc = (BoxedPureFn3*)malloc(sizeof(BoxedPureFn3));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = free;
    rc->fn = fn;
    rc->slot_len = 0;
    return (BValue)rc;
}

BValue call_fn3(BValue fn, BValue arg0, BValue arg1, BValue arg2) {
  BValue ptr = (BValue)TO_POINTER(fn);
  BoxedPureFn3* purefn = (BoxedPureFn3*)ptr;
  if (purefn->slot_len == 0) {
    return purefn->fn(arg0, arg1, arg2);
  }
  else {
    // this must be a closure:
    Closure3Data* rc = (Closure3Data*)ptr;
    BValue* data = closure_data_of((Closure1Data*)rc);
    return rc->fn(data, arg0, arg1, arg2);
  }
}


DEFINE_RC_STRUCT(Closure4Data, BClosure4 fn; size_t slot_len;);
DEFINE_RC_STRUCT(BoxedPureFn4, BPureFn4 fn; size_t slot_len;);

BValue alloc_closure4(size_t size, BValue* data, BClosure4 fn) {
    Closure4Data* rc = malloc(closure_data_size(size));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = (FreeFn)free_closure;
    rc->fn = fn;
    rc->slot_len = size;
    BValue* closure_data = closure_data_of((Closure1Data*)rc);
    for (size_t i = 0; i < size; i++) {
      closure_data[i] = data[i];
    }
    return (BValue)rc;
}

BValue alloc_boxed_pure_fn4(BPureFn4 fn) {
    BoxedPureFn4* rc = (BoxedPureFn4*)malloc(sizeof(BoxedPureFn4));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = free;
    rc->fn = fn;
    rc->slot_len = 0;
    return (BValue)rc;
}

BValue call_fn4(BValue fn, BValue arg0, BValue arg1, BValue arg2, BValue arg3) {
  BValue ptr = (BValue)TO_POINTER(fn);
  BoxedPureFn4* purefn = (BoxedPureFn4*)ptr;
  if (purefn->slot_len == 0) {
    return purefn->fn(arg0, arg1, arg2, arg3);
  }
  else {
    // this must be a closure:
    Closure4Data* rc = (Closure4Data*)ptr;
    BValue* data = closure_data_of((Closure1Data*)rc);
    return rc->fn(data, arg0, arg1, arg2, arg3);
  }
}


DEFINE_RC_STRUCT(Closure5Data, BClosure5 fn; size_t slot_len;);
DEFINE_RC_STRUCT(BoxedPureFn5, BPureFn5 fn; size_t slot_len;);

BValue alloc_closure5(size_t size, BValue* data, BClosure5 fn) {
    Closure5Data* rc = malloc(closure_data_size(size));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = (FreeFn)free_closure;
    rc->fn = fn;
    rc->slot_len = size;
    BValue* closure_data = closure_data_of((Closure1Data*)rc);
    for (size_t i = 0; i < size; i++) {
      closure_data[i] = data[i];
    }
    return (BValue)rc;
}

BValue alloc_boxed_pure_fn5(BPureFn5 fn) {
    BoxedPureFn5* rc = (BoxedPureFn5*)malloc(sizeof(BoxedPureFn5));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = free;
    rc->fn = fn;
    rc->slot_len = 0;
    return (BValue)rc;
}

BValue call_fn5(BValue fn, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4) {
  BValue ptr = (BValue)TO_POINTER(fn);
  BoxedPureFn5* purefn = (BoxedPureFn5*)ptr;
  if (purefn->slot_len == 0) {
    return purefn->fn(arg0, arg1, arg2, arg3, arg4);
  }
  else {
    // this must be a closure:
    Closure5Data* rc = (Closure5Data*)ptr;
    BValue* data = closure_data_of((Closure1Data*)rc);
    return rc->fn(data, arg0, arg1, arg2, arg3, arg4);
  }
}


DEFINE_RC_STRUCT(Closure6Data, BClosure6 fn; size_t slot_len;);
DEFINE_RC_STRUCT(BoxedPureFn6, BPureFn6 fn; size_t slot_len;);

BValue alloc_closure6(size_t size, BValue* data, BClosure6 fn) {
    Closure6Data* rc = malloc(closure_data_size(size));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = (FreeFn)free_closure;
    rc->fn = fn;
    rc->slot_len = size;
    BValue* closure_data = closure_data_of((Closure1Data*)rc);
    for (size_t i = 0; i < size; i++) {
      closure_data[i] = data[i];
    }
    return (BValue)rc;
}

BValue alloc_boxed_pure_fn6(BPureFn6 fn) {
    BoxedPureFn6* rc = (BoxedPureFn6*)malloc(sizeof(BoxedPureFn6));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = free;
    rc->fn = fn;
    rc->slot_len = 0;
    return (BValue)rc;
}

BValue call_fn6(BValue fn, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5) {
  BValue ptr = (BValue)TO_POINTER(fn);
  BoxedPureFn6* purefn = (BoxedPureFn6*)ptr;
  if (purefn->slot_len == 0) {
    return purefn->fn(arg0, arg1, arg2, arg3, arg4, arg5);
  }
  else {
    // this must be a closure:
    Closure6Data* rc = (Closure6Data*)ptr;
    BValue* data = closure_data_of((Closure1Data*)rc);
    return rc->fn(data, arg0, arg1, arg2, arg3, arg4, arg5);
  }
}


DEFINE_RC_STRUCT(Closure7Data, BClosure7 fn; size_t slot_len;);
DEFINE_RC_STRUCT(BoxedPureFn7, BPureFn7 fn; size_t slot_len;);

BValue alloc_closure7(size_t size, BValue* data, BClosure7 fn) {
    Closure7Data* rc = malloc(closure_data_size(size));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = (FreeFn)free_closure;
    rc->fn = fn;
    rc->slot_len = size;
    BValue* closure_data = closure_data_of((Closure1Data*)rc);
    for (size_t i = 0; i < size; i++) {
      closure_data[i] = data[i];
    }
    return (BValue)rc;
}

BValue alloc_boxed_pure_fn7(BPureFn7 fn) {
    BoxedPureFn7* rc = (BoxedPureFn7*)malloc(sizeof(BoxedPureFn7));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = free;
    rc->fn = fn;
    rc->slot_len = 0;
    return (BValue)rc;
}

BValue call_fn7(BValue fn, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6) {
  BValue ptr = (BValue)TO_POINTER(fn);
  BoxedPureFn7* purefn = (BoxedPureFn7*)ptr;
  if (purefn->slot_len == 0) {
    return purefn->fn(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
  }
  else {
    // this must be a closure:
    Closure7Data* rc = (Closure7Data*)ptr;
    BValue* data = closure_data_of((Closure1Data*)rc);
    return rc->fn(data, arg0, arg1, arg2, arg3, arg4, arg5, arg6);
  }
}


DEFINE_RC_STRUCT(Closure8Data, BClosure8 fn; size_t slot_len;);
DEFINE_RC_STRUCT(BoxedPureFn8, BPureFn8 fn; size_t slot_len;);

BValue alloc_closure8(size_t size, BValue* data, BClosure8 fn) {
    Closure8Data* rc = malloc(closure_data_size(size));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = (FreeFn)free_closure;
    rc->fn = fn;
    rc->slot_len = size;
    BValue* closure_data = closure_data_of((Closure1Data*)rc);
    for (size_t i = 0; i < size; i++) {
      closure_data[i] = data[i];
    }
    return (BValue)rc;
}

BValue alloc_boxed_pure_fn8(BPureFn8 fn) {
    BoxedPureFn8* rc = (BoxedPureFn8*)malloc(sizeof(BoxedPureFn8));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = free;
    rc->fn = fn;
    rc->slot_len = 0;
    return (BValue)rc;
}

BValue call_fn8(BValue fn, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7) {
  BValue ptr = (BValue)TO_POINTER(fn);
  BoxedPureFn8* purefn = (BoxedPureFn8*)ptr;
  if (purefn->slot_len == 0) {
    return purefn->fn(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7);
  }
  else {
    // this must be a closure:
    Closure8Data* rc = (Closure8Data*)ptr;
    BValue* data = closure_data_of((Closure1Data*)rc);
    return rc->fn(data, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7);
  }
}


DEFINE_RC_STRUCT(Closure9Data, BClosure9 fn; size_t slot_len;);
DEFINE_RC_STRUCT(BoxedPureFn9, BPureFn9 fn; size_t slot_len;);

BValue alloc_closure9(size_t size, BValue* data, BClosure9 fn) {
    Closure9Data* rc = malloc(closure_data_size(size));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = (FreeFn)free_closure;
    rc->fn = fn;
    rc->slot_len = size;
    BValue* closure_data = closure_data_of((Closure1Data*)rc);
    for (size_t i = 0; i < size; i++) {
      closure_data[i] = data[i];
    }
    return (BValue)rc;
}

BValue alloc_boxed_pure_fn9(BPureFn9 fn) {
    BoxedPureFn9* rc = (BoxedPureFn9*)malloc(sizeof(BoxedPureFn9));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = free;
    rc->fn = fn;
    rc->slot_len = 0;
    return (BValue)rc;
}

BValue call_fn9(BValue fn, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8) {
  BValue ptr = (BValue)TO_POINTER(fn);
  BoxedPureFn9* purefn = (BoxedPureFn9*)ptr;
  if (purefn->slot_len == 0) {
    return purefn->fn(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
  }
  else {
    // this must be a closure:
    Closure9Data* rc = (Closure9Data*)ptr;
    BValue* data = closure_data_of((Closure1Data*)rc);
    return rc->fn(data, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
  }
}


DEFINE_RC_STRUCT(Closure10Data, BClosure10 fn; size_t slot_len;);
DEFINE_RC_STRUCT(BoxedPureFn10, BPureFn10 fn; size_t slot_len;);

BValue alloc_closure10(size_t size, BValue* data, BClosure10 fn) {
    Closure10Data* rc = malloc(closure_data_size(size));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = (FreeFn)free_closure;
    rc->fn = fn;
    rc->slot_len = size;
    BValue* closure_data = closure_data_of((Closure1Data*)rc);
    for (size_t i = 0; i < size; i++) {
      closure_data[i] = data[i];
    }
    return (BValue)rc;
}

BValue alloc_boxed_pure_fn10(BPureFn10 fn) {
    BoxedPureFn10* rc = (BoxedPureFn10*)malloc(sizeof(BoxedPureFn10));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = free;
    rc->fn = fn;
    rc->slot_len = 0;
    return (BValue)rc;
}

BValue call_fn10(BValue fn, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9) {
  BValue ptr = (BValue)TO_POINTER(fn);
  BoxedPureFn10* purefn = (BoxedPureFn10*)ptr;
  if (purefn->slot_len == 0) {
    return purefn->fn(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
  }
  else {
    // this must be a closure:
    Closure10Data* rc = (Closure10Data*)ptr;
    BValue* data = closure_data_of((Closure1Data*)rc);
    return rc->fn(data, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
  }
}


DEFINE_RC_STRUCT(Closure11Data, BClosure11 fn; size_t slot_len;);
DEFINE_RC_STRUCT(BoxedPureFn11, BPureFn11 fn; size_t slot_len;);

BValue alloc_closure11(size_t size, BValue* data, BClosure11 fn) {
    Closure11Data* rc = malloc(closure_data_size(size));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = (FreeFn)free_closure;
    rc->fn = fn;
    rc->slot_len = size;
    BValue* closure_data = closure_data_of((Closure1Data*)rc);
    for (size_t i = 0; i < size; i++) {
      closure_data[i] = data[i];
    }
    return (BValue)rc;
}

BValue alloc_boxed_pure_fn11(BPureFn11 fn) {
    BoxedPureFn11* rc = (BoxedPureFn11*)malloc(sizeof(BoxedPureFn11));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = free;
    rc->fn = fn;
    rc->slot_len = 0;
    return (BValue)rc;
}

BValue call_fn11(BValue fn, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10) {
  BValue ptr = (BValue)TO_POINTER(fn);
  BoxedPureFn11* purefn = (BoxedPureFn11*)ptr;
  if (purefn->slot_len == 0) {
    return purefn->fn(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10);
  }
  else {
    // this must be a closure:
    Closure11Data* rc = (Closure11Data*)ptr;
    BValue* data = closure_data_of((Closure1Data*)rc);
    return rc->fn(data, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10);
  }
}


DEFINE_RC_STRUCT(Closure12Data, BClosure12 fn; size_t slot_len;);
DEFINE_RC_STRUCT(BoxedPureFn12, BPureFn12 fn; size_t slot_len;);

BValue alloc_closure12(size_t size, BValue* data, BClosure12 fn) {
    Closure12Data* rc = malloc(closure_data_size(size));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = (FreeFn)free_closure;
    rc->fn = fn;
    rc->slot_len = size;
    BValue* closure_data = closure_data_of((Closure1Data*)rc);
    for (size_t i = 0; i < size; i++) {
      closure_data[i] = data[i];
    }
    return (BValue)rc;
}

BValue alloc_boxed_pure_fn12(BPureFn12 fn) {
    BoxedPureFn12* rc = (BoxedPureFn12*)malloc(sizeof(BoxedPureFn12));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = free;
    rc->fn = fn;
    rc->slot_len = 0;
    return (BValue)rc;
}

BValue call_fn12(BValue fn, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11) {
  BValue ptr = (BValue)TO_POINTER(fn);
  BoxedPureFn12* purefn = (BoxedPureFn12*)ptr;
  if (purefn->slot_len == 0) {
    return purefn->fn(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11);
  }
  else {
    // this must be a closure:
    Closure12Data* rc = (Closure12Data*)ptr;
    BValue* data = closure_data_of((Closure1Data*)rc);
    return rc->fn(data, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11);
  }
}


DEFINE_RC_STRUCT(Closure13Data, BClosure13 fn; size_t slot_len;);
DEFINE_RC_STRUCT(BoxedPureFn13, BPureFn13 fn; size_t slot_len;);

BValue alloc_closure13(size_t size, BValue* data, BClosure13 fn) {
    Closure13Data* rc = malloc(closure_data_size(size));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = (FreeFn)free_closure;
    rc->fn = fn;
    rc->slot_len = size;
    BValue* closure_data = closure_data_of((Closure1Data*)rc);
    for (size_t i = 0; i < size; i++) {
      closure_data[i] = data[i];
    }
    return (BValue)rc;
}

BValue alloc_boxed_pure_fn13(BPureFn13 fn) {
    BoxedPureFn13* rc = (BoxedPureFn13*)malloc(sizeof(BoxedPureFn13));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = free;
    rc->fn = fn;
    rc->slot_len = 0;
    return (BValue)rc;
}

BValue call_fn13(BValue fn, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11, BValue arg12) {
  BValue ptr = (BValue)TO_POINTER(fn);
  BoxedPureFn13* purefn = (BoxedPureFn13*)ptr;
  if (purefn->slot_len == 0) {
    return purefn->fn(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12);
  }
  else {
    // this must be a closure:
    Closure13Data* rc = (Closure13Data*)ptr;
    BValue* data = closure_data_of((Closure1Data*)rc);
    return rc->fn(data, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12);
  }
}


DEFINE_RC_STRUCT(Closure14Data, BClosure14 fn; size_t slot_len;);
DEFINE_RC_STRUCT(BoxedPureFn14, BPureFn14 fn; size_t slot_len;);

BValue alloc_closure14(size_t size, BValue* data, BClosure14 fn) {
    Closure14Data* rc = malloc(closure_data_size(size));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = (FreeFn)free_closure;
    rc->fn = fn;
    rc->slot_len = size;
    BValue* closure_data = closure_data_of((Closure1Data*)rc);
    for (size_t i = 0; i < size; i++) {
      closure_data[i] = data[i];
    }
    return (BValue)rc;
}

BValue alloc_boxed_pure_fn14(BPureFn14 fn) {
    BoxedPureFn14* rc = (BoxedPureFn14*)malloc(sizeof(BoxedPureFn14));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = free;
    rc->fn = fn;
    rc->slot_len = 0;
    return (BValue)rc;
}

BValue call_fn14(BValue fn, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11, BValue arg12, BValue arg13) {
  BValue ptr = (BValue)TO_POINTER(fn);
  BoxedPureFn14* purefn = (BoxedPureFn14*)ptr;
  if (purefn->slot_len == 0) {
    return purefn->fn(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13);
  }
  else {
    // this must be a closure:
    Closure14Data* rc = (Closure14Data*)ptr;
    BValue* data = closure_data_of((Closure1Data*)rc);
    return rc->fn(data, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13);
  }
}


DEFINE_RC_STRUCT(Closure15Data, BClosure15 fn; size_t slot_len;);
DEFINE_RC_STRUCT(BoxedPureFn15, BPureFn15 fn; size_t slot_len;);

BValue alloc_closure15(size_t size, BValue* data, BClosure15 fn) {
    Closure15Data* rc = malloc(closure_data_size(size));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = (FreeFn)free_closure;
    rc->fn = fn;
    rc->slot_len = size;
    BValue* closure_data = closure_data_of((Closure1Data*)rc);
    for (size_t i = 0; i < size; i++) {
      closure_data[i] = data[i];
    }
    return (BValue)rc;
}

BValue alloc_boxed_pure_fn15(BPureFn15 fn) {
    BoxedPureFn15* rc = (BoxedPureFn15*)malloc(sizeof(BoxedPureFn15));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = free;
    rc->fn = fn;
    rc->slot_len = 0;
    return (BValue)rc;
}

BValue call_fn15(BValue fn, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11, BValue arg12, BValue arg13, BValue arg14) {
  BValue ptr = (BValue)TO_POINTER(fn);
  BoxedPureFn15* purefn = (BoxedPureFn15*)ptr;
  if (purefn->slot_len == 0) {
    return purefn->fn(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14);
  }
  else {
    // this must be a closure:
    Closure15Data* rc = (Closure15Data*)ptr;
    BValue* data = closure_data_of((Closure1Data*)rc);
    return rc->fn(data, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14);
  }
}


DEFINE_RC_STRUCT(Closure16Data, BClosure16 fn; size_t slot_len;);
DEFINE_RC_STRUCT(BoxedPureFn16, BPureFn16 fn; size_t slot_len;);

BValue alloc_closure16(size_t size, BValue* data, BClosure16 fn) {
    Closure16Data* rc = malloc(closure_data_size(size));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = (FreeFn)free_closure;
    rc->fn = fn;
    rc->slot_len = size;
    BValue* closure_data = closure_data_of((Closure1Data*)rc);
    for (size_t i = 0; i < size; i++) {
      closure_data[i] = data[i];
    }
    return (BValue)rc;
}

BValue alloc_boxed_pure_fn16(BPureFn16 fn) {
    BoxedPureFn16* rc = (BoxedPureFn16*)malloc(sizeof(BoxedPureFn16));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = free;
    rc->fn = fn;
    rc->slot_len = 0;
    return (BValue)rc;
}

BValue call_fn16(BValue fn, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11, BValue arg12, BValue arg13, BValue arg14, BValue arg15) {
  BValue ptr = (BValue)TO_POINTER(fn);
  BoxedPureFn16* purefn = (BoxedPureFn16*)ptr;
  if (purefn->slot_len == 0) {
    return purefn->fn(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15);
  }
  else {
    // this must be a closure:
    Closure16Data* rc = (Closure16Data*)ptr;
    BValue* data = closure_data_of((Closure1Data*)rc);
    return rc->fn(data, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15);
  }
}


DEFINE_RC_STRUCT(Closure17Data, BClosure17 fn; size_t slot_len;);
DEFINE_RC_STRUCT(BoxedPureFn17, BPureFn17 fn; size_t slot_len;);

BValue alloc_closure17(size_t size, BValue* data, BClosure17 fn) {
    Closure17Data* rc = malloc(closure_data_size(size));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = (FreeFn)free_closure;
    rc->fn = fn;
    rc->slot_len = size;
    BValue* closure_data = closure_data_of((Closure1Data*)rc);
    for (size_t i = 0; i < size; i++) {
      closure_data[i] = data[i];
    }
    return (BValue)rc;
}

BValue alloc_boxed_pure_fn17(BPureFn17 fn) {
    BoxedPureFn17* rc = (BoxedPureFn17*)malloc(sizeof(BoxedPureFn17));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = free;
    rc->fn = fn;
    rc->slot_len = 0;
    return (BValue)rc;
}

BValue call_fn17(BValue fn, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11, BValue arg12, BValue arg13, BValue arg14, BValue arg15, BValue arg16) {
  BValue ptr = (BValue)TO_POINTER(fn);
  BoxedPureFn17* purefn = (BoxedPureFn17*)ptr;
  if (purefn->slot_len == 0) {
    return purefn->fn(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16);
  }
  else {
    // this must be a closure:
    Closure17Data* rc = (Closure17Data*)ptr;
    BValue* data = closure_data_of((Closure1Data*)rc);
    return rc->fn(data, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16);
  }
}


DEFINE_RC_STRUCT(Closure18Data, BClosure18 fn; size_t slot_len;);
DEFINE_RC_STRUCT(BoxedPureFn18, BPureFn18 fn; size_t slot_len;);

BValue alloc_closure18(size_t size, BValue* data, BClosure18 fn) {
    Closure18Data* rc = malloc(closure_data_size(size));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = (FreeFn)free_closure;
    rc->fn = fn;
    rc->slot_len = size;
    BValue* closure_data = closure_data_of((Closure1Data*)rc);
    for (size_t i = 0; i < size; i++) {
      closure_data[i] = data[i];
    }
    return (BValue)rc;
}

BValue alloc_boxed_pure_fn18(BPureFn18 fn) {
    BoxedPureFn18* rc = (BoxedPureFn18*)malloc(sizeof(BoxedPureFn18));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = free;
    rc->fn = fn;
    rc->slot_len = 0;
    return (BValue)rc;
}

BValue call_fn18(BValue fn, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11, BValue arg12, BValue arg13, BValue arg14, BValue arg15, BValue arg16, BValue arg17) {
  BValue ptr = (BValue)TO_POINTER(fn);
  BoxedPureFn18* purefn = (BoxedPureFn18*)ptr;
  if (purefn->slot_len == 0) {
    return purefn->fn(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17);
  }
  else {
    // this must be a closure:
    Closure18Data* rc = (Closure18Data*)ptr;
    BValue* data = closure_data_of((Closure1Data*)rc);
    return rc->fn(data, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17);
  }
}


DEFINE_RC_STRUCT(Closure19Data, BClosure19 fn; size_t slot_len;);
DEFINE_RC_STRUCT(BoxedPureFn19, BPureFn19 fn; size_t slot_len;);

BValue alloc_closure19(size_t size, BValue* data, BClosure19 fn) {
    Closure19Data* rc = malloc(closure_data_size(size));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = (FreeFn)free_closure;
    rc->fn = fn;
    rc->slot_len = size;
    BValue* closure_data = closure_data_of((Closure1Data*)rc);
    for (size_t i = 0; i < size; i++) {
      closure_data[i] = data[i];
    }
    return (BValue)rc;
}

BValue alloc_boxed_pure_fn19(BPureFn19 fn) {
    BoxedPureFn19* rc = (BoxedPureFn19*)malloc(sizeof(BoxedPureFn19));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = free;
    rc->fn = fn;
    rc->slot_len = 0;
    return (BValue)rc;
}

BValue call_fn19(BValue fn, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11, BValue arg12, BValue arg13, BValue arg14, BValue arg15, BValue arg16, BValue arg17, BValue arg18) {
  BValue ptr = (BValue)TO_POINTER(fn);
  BoxedPureFn19* purefn = (BoxedPureFn19*)ptr;
  if (purefn->slot_len == 0) {
    return purefn->fn(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18);
  }
  else {
    // this must be a closure:
    Closure19Data* rc = (Closure19Data*)ptr;
    BValue* data = closure_data_of((Closure1Data*)rc);
    return rc->fn(data, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18);
  }
}


DEFINE_RC_STRUCT(Closure20Data, BClosure20 fn; size_t slot_len;);
DEFINE_RC_STRUCT(BoxedPureFn20, BPureFn20 fn; size_t slot_len;);

BValue alloc_closure20(size_t size, BValue* data, BClosure20 fn) {
    Closure20Data* rc = malloc(closure_data_size(size));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = (FreeFn)free_closure;
    rc->fn = fn;
    rc->slot_len = size;
    BValue* closure_data = closure_data_of((Closure1Data*)rc);
    for (size_t i = 0; i < size; i++) {
      closure_data[i] = data[i];
    }
    return (BValue)rc;
}

BValue alloc_boxed_pure_fn20(BPureFn20 fn) {
    BoxedPureFn20* rc = (BoxedPureFn20*)malloc(sizeof(BoxedPureFn20));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = free;
    rc->fn = fn;
    rc->slot_len = 0;
    return (BValue)rc;
}

BValue call_fn20(BValue fn, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11, BValue arg12, BValue arg13, BValue arg14, BValue arg15, BValue arg16, BValue arg17, BValue arg18, BValue arg19) {
  BValue ptr = (BValue)TO_POINTER(fn);
  BoxedPureFn20* purefn = (BoxedPureFn20*)ptr;
  if (purefn->slot_len == 0) {
    return purefn->fn(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19);
  }
  else {
    // this must be a closure:
    Closure20Data* rc = (Closure20Data*)ptr;
    BValue* data = closure_data_of((Closure1Data*)rc);
    return rc->fn(data, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19);
  }
}


DEFINE_RC_STRUCT(Closure21Data, BClosure21 fn; size_t slot_len;);
DEFINE_RC_STRUCT(BoxedPureFn21, BPureFn21 fn; size_t slot_len;);

BValue alloc_closure21(size_t size, BValue* data, BClosure21 fn) {
    Closure21Data* rc = malloc(closure_data_size(size));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = (FreeFn)free_closure;
    rc->fn = fn;
    rc->slot_len = size;
    BValue* closure_data = closure_data_of((Closure1Data*)rc);
    for (size_t i = 0; i < size; i++) {
      closure_data[i] = data[i];
    }
    return (BValue)rc;
}

BValue alloc_boxed_pure_fn21(BPureFn21 fn) {
    BoxedPureFn21* rc = (BoxedPureFn21*)malloc(sizeof(BoxedPureFn21));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = free;
    rc->fn = fn;
    rc->slot_len = 0;
    return (BValue)rc;
}

BValue call_fn21(BValue fn, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11, BValue arg12, BValue arg13, BValue arg14, BValue arg15, BValue arg16, BValue arg17, BValue arg18, BValue arg19, BValue arg20) {
  BValue ptr = (BValue)TO_POINTER(fn);
  BoxedPureFn21* purefn = (BoxedPureFn21*)ptr;
  if (purefn->slot_len == 0) {
    return purefn->fn(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20);
  }
  else {
    // this must be a closure:
    Closure21Data* rc = (Closure21Data*)ptr;
    BValue* data = closure_data_of((Closure1Data*)rc);
    return rc->fn(data, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20);
  }
}


DEFINE_RC_STRUCT(Closure22Data, BClosure22 fn; size_t slot_len;);
DEFINE_RC_STRUCT(BoxedPureFn22, BPureFn22 fn; size_t slot_len;);

BValue alloc_closure22(size_t size, BValue* data, BClosure22 fn) {
    Closure22Data* rc = malloc(closure_data_size(size));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = (FreeFn)free_closure;
    rc->fn = fn;
    rc->slot_len = size;
    BValue* closure_data = closure_data_of((Closure1Data*)rc);
    for (size_t i = 0; i < size; i++) {
      closure_data[i] = data[i];
    }
    return (BValue)rc;
}

BValue alloc_boxed_pure_fn22(BPureFn22 fn) {
    BoxedPureFn22* rc = (BoxedPureFn22*)malloc(sizeof(BoxedPureFn22));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = free;
    rc->fn = fn;
    rc->slot_len = 0;
    return (BValue)rc;
}

BValue call_fn22(BValue fn, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11, BValue arg12, BValue arg13, BValue arg14, BValue arg15, BValue arg16, BValue arg17, BValue arg18, BValue arg19, BValue arg20, BValue arg21) {
  BValue ptr = (BValue)TO_POINTER(fn);
  BoxedPureFn22* purefn = (BoxedPureFn22*)ptr;
  if (purefn->slot_len == 0) {
    return purefn->fn(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, arg21);
  }
  else {
    // this must be a closure:
    Closure22Data* rc = (Closure22Data*)ptr;
    BValue* data = closure_data_of((Closure1Data*)rc);
    return rc->fn(data, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, arg21);
  }
}


DEFINE_RC_STRUCT(Closure23Data, BClosure23 fn; size_t slot_len;);
DEFINE_RC_STRUCT(BoxedPureFn23, BPureFn23 fn; size_t slot_len;);

BValue alloc_closure23(size_t size, BValue* data, BClosure23 fn) {
    Closure23Data* rc = malloc(closure_data_size(size));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = (FreeFn)free_closure;
    rc->fn = fn;
    rc->slot_len = size;
    BValue* closure_data = closure_data_of((Closure1Data*)rc);
    for (size_t i = 0; i < size; i++) {
      closure_data[i] = data[i];
    }
    return (BValue)rc;
}

BValue alloc_boxed_pure_fn23(BPureFn23 fn) {
    BoxedPureFn23* rc = (BoxedPureFn23*)malloc(sizeof(BoxedPureFn23));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = free;
    rc->fn = fn;
    rc->slot_len = 0;
    return (BValue)rc;
}

BValue call_fn23(BValue fn, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11, BValue arg12, BValue arg13, BValue arg14, BValue arg15, BValue arg16, BValue arg17, BValue arg18, BValue arg19, BValue arg20, BValue arg21, BValue arg22) {
  BValue ptr = (BValue)TO_POINTER(fn);
  BoxedPureFn23* purefn = (BoxedPureFn23*)ptr;
  if (purefn->slot_len == 0) {
    return purefn->fn(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, arg21, arg22);
  }
  else {
    // this must be a closure:
    Closure23Data* rc = (Closure23Data*)ptr;
    BValue* data = closure_data_of((Closure1Data*)rc);
    return rc->fn(data, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, arg21, arg22);
  }
}


DEFINE_RC_STRUCT(Closure24Data, BClosure24 fn; size_t slot_len;);
DEFINE_RC_STRUCT(BoxedPureFn24, BPureFn24 fn; size_t slot_len;);

BValue alloc_closure24(size_t size, BValue* data, BClosure24 fn) {
    Closure24Data* rc = malloc(closure_data_size(size));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = (FreeFn)free_closure;
    rc->fn = fn;
    rc->slot_len = size;
    BValue* closure_data = closure_data_of((Closure1Data*)rc);
    for (size_t i = 0; i < size; i++) {
      closure_data[i] = data[i];
    }
    return (BValue)rc;
}

BValue alloc_boxed_pure_fn24(BPureFn24 fn) {
    BoxedPureFn24* rc = (BoxedPureFn24*)malloc(sizeof(BoxedPureFn24));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = free;
    rc->fn = fn;
    rc->slot_len = 0;
    return (BValue)rc;
}

BValue call_fn24(BValue fn, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11, BValue arg12, BValue arg13, BValue arg14, BValue arg15, BValue arg16, BValue arg17, BValue arg18, BValue arg19, BValue arg20, BValue arg21, BValue arg22, BValue arg23) {
  BValue ptr = (BValue)TO_POINTER(fn);
  BoxedPureFn24* purefn = (BoxedPureFn24*)ptr;
  if (purefn->slot_len == 0) {
    return purefn->fn(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, arg21, arg22, arg23);
  }
  else {
    // this must be a closure:
    Closure24Data* rc = (Closure24Data*)ptr;
    BValue* data = closure_data_of((Closure1Data*)rc);
    return rc->fn(data, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, arg21, arg22, arg23);
  }
}


DEFINE_RC_STRUCT(Closure25Data, BClosure25 fn; size_t slot_len;);
DEFINE_RC_STRUCT(BoxedPureFn25, BPureFn25 fn; size_t slot_len;);

BValue alloc_closure25(size_t size, BValue* data, BClosure25 fn) {
    Closure25Data* rc = malloc(closure_data_size(size));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = (FreeFn)free_closure;
    rc->fn = fn;
    rc->slot_len = size;
    BValue* closure_data = closure_data_of((Closure1Data*)rc);
    for (size_t i = 0; i < size; i++) {
      closure_data[i] = data[i];
    }
    return (BValue)rc;
}

BValue alloc_boxed_pure_fn25(BPureFn25 fn) {
    BoxedPureFn25* rc = (BoxedPureFn25*)malloc(sizeof(BoxedPureFn25));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = free;
    rc->fn = fn;
    rc->slot_len = 0;
    return (BValue)rc;
}

BValue call_fn25(BValue fn, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11, BValue arg12, BValue arg13, BValue arg14, BValue arg15, BValue arg16, BValue arg17, BValue arg18, BValue arg19, BValue arg20, BValue arg21, BValue arg22, BValue arg23, BValue arg24) {
  BValue ptr = (BValue)TO_POINTER(fn);
  BoxedPureFn25* purefn = (BoxedPureFn25*)ptr;
  if (purefn->slot_len == 0) {
    return purefn->fn(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, arg21, arg22, arg23, arg24);
  }
  else {
    // this must be a closure:
    Closure25Data* rc = (Closure25Data*)ptr;
    BValue* data = closure_data_of((Closure1Data*)rc);
    return rc->fn(data, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, arg21, arg22, arg23, arg24);
  }
}


DEFINE_RC_STRUCT(Closure26Data, BClosure26 fn; size_t slot_len;);
DEFINE_RC_STRUCT(BoxedPureFn26, BPureFn26 fn; size_t slot_len;);

BValue alloc_closure26(size_t size, BValue* data, BClosure26 fn) {
    Closure26Data* rc = malloc(closure_data_size(size));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = (FreeFn)free_closure;
    rc->fn = fn;
    rc->slot_len = size;
    BValue* closure_data = closure_data_of((Closure1Data*)rc);
    for (size_t i = 0; i < size; i++) {
      closure_data[i] = data[i];
    }
    return (BValue)rc;
}

BValue alloc_boxed_pure_fn26(BPureFn26 fn) {
    BoxedPureFn26* rc = (BoxedPureFn26*)malloc(sizeof(BoxedPureFn26));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = free;
    rc->fn = fn;
    rc->slot_len = 0;
    return (BValue)rc;
}

BValue call_fn26(BValue fn, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11, BValue arg12, BValue arg13, BValue arg14, BValue arg15, BValue arg16, BValue arg17, BValue arg18, BValue arg19, BValue arg20, BValue arg21, BValue arg22, BValue arg23, BValue arg24, BValue arg25) {
  BValue ptr = (BValue)TO_POINTER(fn);
  BoxedPureFn26* purefn = (BoxedPureFn26*)ptr;
  if (purefn->slot_len == 0) {
    return purefn->fn(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, arg21, arg22, arg23, arg24, arg25);
  }
  else {
    // this must be a closure:
    Closure26Data* rc = (Closure26Data*)ptr;
    BValue* data = closure_data_of((Closure1Data*)rc);
    return rc->fn(data, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, arg21, arg22, arg23, arg24, arg25);
  }
}


DEFINE_RC_STRUCT(Closure27Data, BClosure27 fn; size_t slot_len;);
DEFINE_RC_STRUCT(BoxedPureFn27, BPureFn27 fn; size_t slot_len;);

BValue alloc_closure27(size_t size, BValue* data, BClosure27 fn) {
    Closure27Data* rc = malloc(closure_data_size(size));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = (FreeFn)free_closure;
    rc->fn = fn;
    rc->slot_len = size;
    BValue* closure_data = closure_data_of((Closure1Data*)rc);
    for (size_t i = 0; i < size; i++) {
      closure_data[i] = data[i];
    }
    return (BValue)rc;
}

BValue alloc_boxed_pure_fn27(BPureFn27 fn) {
    BoxedPureFn27* rc = (BoxedPureFn27*)malloc(sizeof(BoxedPureFn27));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = free;
    rc->fn = fn;
    rc->slot_len = 0;
    return (BValue)rc;
}

BValue call_fn27(BValue fn, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11, BValue arg12, BValue arg13, BValue arg14, BValue arg15, BValue arg16, BValue arg17, BValue arg18, BValue arg19, BValue arg20, BValue arg21, BValue arg22, BValue arg23, BValue arg24, BValue arg25, BValue arg26) {
  BValue ptr = (BValue)TO_POINTER(fn);
  BoxedPureFn27* purefn = (BoxedPureFn27*)ptr;
  if (purefn->slot_len == 0) {
    return purefn->fn(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, arg21, arg22, arg23, arg24, arg25, arg26);
  }
  else {
    // this must be a closure:
    Closure27Data* rc = (Closure27Data*)ptr;
    BValue* data = closure_data_of((Closure1Data*)rc);
    return rc->fn(data, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, arg21, arg22, arg23, arg24, arg25, arg26);
  }
}


DEFINE_RC_STRUCT(Closure28Data, BClosure28 fn; size_t slot_len;);
DEFINE_RC_STRUCT(BoxedPureFn28, BPureFn28 fn; size_t slot_len;);

BValue alloc_closure28(size_t size, BValue* data, BClosure28 fn) {
    Closure28Data* rc = malloc(closure_data_size(size));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = (FreeFn)free_closure;
    rc->fn = fn;
    rc->slot_len = size;
    BValue* closure_data = closure_data_of((Closure1Data*)rc);
    for (size_t i = 0; i < size; i++) {
      closure_data[i] = data[i];
    }
    return (BValue)rc;
}

BValue alloc_boxed_pure_fn28(BPureFn28 fn) {
    BoxedPureFn28* rc = (BoxedPureFn28*)malloc(sizeof(BoxedPureFn28));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = free;
    rc->fn = fn;
    rc->slot_len = 0;
    return (BValue)rc;
}

BValue call_fn28(BValue fn, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11, BValue arg12, BValue arg13, BValue arg14, BValue arg15, BValue arg16, BValue arg17, BValue arg18, BValue arg19, BValue arg20, BValue arg21, BValue arg22, BValue arg23, BValue arg24, BValue arg25, BValue arg26, BValue arg27) {
  BValue ptr = (BValue)TO_POINTER(fn);
  BoxedPureFn28* purefn = (BoxedPureFn28*)ptr;
  if (purefn->slot_len == 0) {
    return purefn->fn(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, arg21, arg22, arg23, arg24, arg25, arg26, arg27);
  }
  else {
    // this must be a closure:
    Closure28Data* rc = (Closure28Data*)ptr;
    BValue* data = closure_data_of((Closure1Data*)rc);
    return rc->fn(data, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, arg21, arg22, arg23, arg24, arg25, arg26, arg27);
  }
}


DEFINE_RC_STRUCT(Closure29Data, BClosure29 fn; size_t slot_len;);
DEFINE_RC_STRUCT(BoxedPureFn29, BPureFn29 fn; size_t slot_len;);

BValue alloc_closure29(size_t size, BValue* data, BClosure29 fn) {
    Closure29Data* rc = malloc(closure_data_size(size));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = (FreeFn)free_closure;
    rc->fn = fn;
    rc->slot_len = size;
    BValue* closure_data = closure_data_of((Closure1Data*)rc);
    for (size_t i = 0; i < size; i++) {
      closure_data[i] = data[i];
    }
    return (BValue)rc;
}

BValue alloc_boxed_pure_fn29(BPureFn29 fn) {
    BoxedPureFn29* rc = (BoxedPureFn29*)malloc(sizeof(BoxedPureFn29));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = free;
    rc->fn = fn;
    rc->slot_len = 0;
    return (BValue)rc;
}

BValue call_fn29(BValue fn, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11, BValue arg12, BValue arg13, BValue arg14, BValue arg15, BValue arg16, BValue arg17, BValue arg18, BValue arg19, BValue arg20, BValue arg21, BValue arg22, BValue arg23, BValue arg24, BValue arg25, BValue arg26, BValue arg27, BValue arg28) {
  BValue ptr = (BValue)TO_POINTER(fn);
  BoxedPureFn29* purefn = (BoxedPureFn29*)ptr;
  if (purefn->slot_len == 0) {
    return purefn->fn(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, arg21, arg22, arg23, arg24, arg25, arg26, arg27, arg28);
  }
  else {
    // this must be a closure:
    Closure29Data* rc = (Closure29Data*)ptr;
    BValue* data = closure_data_of((Closure1Data*)rc);
    return rc->fn(data, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, arg21, arg22, arg23, arg24, arg25, arg26, arg27, arg28);
  }
}


DEFINE_RC_STRUCT(Closure30Data, BClosure30 fn; size_t slot_len;);
DEFINE_RC_STRUCT(BoxedPureFn30, BPureFn30 fn; size_t slot_len;);

BValue alloc_closure30(size_t size, BValue* data, BClosure30 fn) {
    Closure30Data* rc = malloc(closure_data_size(size));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = (FreeFn)free_closure;
    rc->fn = fn;
    rc->slot_len = size;
    BValue* closure_data = closure_data_of((Closure1Data*)rc);
    for (size_t i = 0; i < size; i++) {
      closure_data[i] = data[i];
    }
    return (BValue)rc;
}

BValue alloc_boxed_pure_fn30(BPureFn30 fn) {
    BoxedPureFn30* rc = (BoxedPureFn30*)malloc(sizeof(BoxedPureFn30));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = free;
    rc->fn = fn;
    rc->slot_len = 0;
    return (BValue)rc;
}

BValue call_fn30(BValue fn, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11, BValue arg12, BValue arg13, BValue arg14, BValue arg15, BValue arg16, BValue arg17, BValue arg18, BValue arg19, BValue arg20, BValue arg21, BValue arg22, BValue arg23, BValue arg24, BValue arg25, BValue arg26, BValue arg27, BValue arg28, BValue arg29) {
  BValue ptr = (BValue)TO_POINTER(fn);
  BoxedPureFn30* purefn = (BoxedPureFn30*)ptr;
  if (purefn->slot_len == 0) {
    return purefn->fn(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, arg21, arg22, arg23, arg24, arg25, arg26, arg27, arg28, arg29);
  }
  else {
    // this must be a closure:
    Closure30Data* rc = (Closure30Data*)ptr;
    BValue* data = closure_data_of((Closure1Data*)rc);
    return rc->fn(data, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, arg21, arg22, arg23, arg24, arg25, arg26, arg27, arg28, arg29);
  }
}


DEFINE_RC_STRUCT(Closure31Data, BClosure31 fn; size_t slot_len;);
DEFINE_RC_STRUCT(BoxedPureFn31, BPureFn31 fn; size_t slot_len;);

BValue alloc_closure31(size_t size, BValue* data, BClosure31 fn) {
    Closure31Data* rc = malloc(closure_data_size(size));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = (FreeFn)free_closure;
    rc->fn = fn;
    rc->slot_len = size;
    BValue* closure_data = closure_data_of((Closure1Data*)rc);
    for (size_t i = 0; i < size; i++) {
      closure_data[i] = data[i];
    }
    return (BValue)rc;
}

BValue alloc_boxed_pure_fn31(BPureFn31 fn) {
    BoxedPureFn31* rc = (BoxedPureFn31*)malloc(sizeof(BoxedPureFn31));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = free;
    rc->fn = fn;
    rc->slot_len = 0;
    return (BValue)rc;
}

BValue call_fn31(BValue fn, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11, BValue arg12, BValue arg13, BValue arg14, BValue arg15, BValue arg16, BValue arg17, BValue arg18, BValue arg19, BValue arg20, BValue arg21, BValue arg22, BValue arg23, BValue arg24, BValue arg25, BValue arg26, BValue arg27, BValue arg28, BValue arg29, BValue arg30) {
  BValue ptr = (BValue)TO_POINTER(fn);
  BoxedPureFn31* purefn = (BoxedPureFn31*)ptr;
  if (purefn->slot_len == 0) {
    return purefn->fn(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, arg21, arg22, arg23, arg24, arg25, arg26, arg27, arg28, arg29, arg30);
  }
  else {
    // this must be a closure:
    Closure31Data* rc = (Closure31Data*)ptr;
    BValue* data = closure_data_of((Closure1Data*)rc);
    return rc->fn(data, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, arg21, arg22, arg23, arg24, arg25, arg26, arg27, arg28, arg29, arg30);
  }
}


DEFINE_RC_STRUCT(Closure32Data, BClosure32 fn; size_t slot_len;);
DEFINE_RC_STRUCT(BoxedPureFn32, BPureFn32 fn; size_t slot_len;);

BValue alloc_closure32(size_t size, BValue* data, BClosure32 fn) {
    Closure32Data* rc = malloc(closure_data_size(size));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = (FreeFn)free_closure;
    rc->fn = fn;
    rc->slot_len = size;
    BValue* closure_data = closure_data_of((Closure1Data*)rc);
    for (size_t i = 0; i < size; i++) {
      closure_data[i] = data[i];
    }
    return (BValue)rc;
}

BValue alloc_boxed_pure_fn32(BPureFn32 fn) {
    BoxedPureFn32* rc = (BoxedPureFn32*)malloc(sizeof(BoxedPureFn32));
    // this is safe to do outside atomic because no other thread can see this yet
    rc->ref_count = 1;
    rc->free = free;
    rc->fn = fn;
    rc->slot_len = 0;
    return (BValue)rc;
}

BValue call_fn32(BValue fn, BValue arg0, BValue arg1, BValue arg2, BValue arg3, BValue arg4, BValue arg5, BValue arg6, BValue arg7, BValue arg8, BValue arg9, BValue arg10, BValue arg11, BValue arg12, BValue arg13, BValue arg14, BValue arg15, BValue arg16, BValue arg17, BValue arg18, BValue arg19, BValue arg20, BValue arg21, BValue arg22, BValue arg23, BValue arg24, BValue arg25, BValue arg26, BValue arg27, BValue arg28, BValue arg29, BValue arg30, BValue arg31) {
  BValue ptr = (BValue)TO_POINTER(fn);
  BoxedPureFn32* purefn = (BoxedPureFn32*)ptr;
  if (purefn->slot_len == 0) {
    return purefn->fn(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, arg21, arg22, arg23, arg24, arg25, arg26, arg27, arg28, arg29, arg30, arg31);
  }
  else {
    // this must be a closure:
    Closure32Data* rc = (Closure32Data*)ptr;
    BValue* data = closure_data_of((Closure1Data*)rc);
    return rc->fn(data, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, arg21, arg22, arg23, arg24, arg25, arg26, arg27, arg28, arg29, arg30, arg31);
  }
}

