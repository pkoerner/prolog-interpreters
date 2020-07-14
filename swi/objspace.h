#ifndef OBJSPACE_H_INCLUDED
#define OBJSPACE_H_INCLUDED

enum Type {Integer, Boolean};

typedef struct value {
    int type;
    union {
        int val;
    };
} Value;

int bool_to_int(Value x);
Value is_falsey(Value x);
Value make_int(int x);

Value not(Value x);

Value mod(Value x, Value y);
Value mul(Value x, Value y);
Value sub(Value x, Value y);
Value add(Value x, Value y);

Value eq(Value x, Value y);
Value le(Value x, Value y);
Value lt(Value x, Value y);
Value ge(Value x, Value y);
Value gt(Value x, Value y);

#endif
