#include <assert.h>

#include "objspace.h"

int bool_to_int(Value x) {
    return x.val != 0;
}

Value is_falsey(Value x) {
    Value res;
    res.type = Boolean;
    res.val = (x.val == 0);
    return res;
}

Value make_int(int x) {
    Value res;
    res.type = Integer;
    res.val  = x;
    return res;
}

Value not(Value x) {
    assert(x.type == Boolean);
    Value res;
    res.type = Boolean;
    res.val = !(res.val);
    return res;
}

Value mod(Value x, Value y) {
    assert(x.type == Integer);
    assert(y.type == Integer);
    assert(y.val != 0);
    Value res;
    res.type = Integer;
    res.val = x.val % y.val;
    return res;
}

Value mul(Value x, Value y) {
    assert(x.type == Integer);
    assert(y.type == Integer);
    Value res;
    res.type = Integer;
    res.val = x.val * y.val;
    return res;
}

Value sub(Value x, Value y) {
    assert(x.type == Integer);
    assert(y.type == Integer);
    Value res;
    res.type = Integer;
    res.val = x.val - y.val;
    return res;
}

Value add(Value x, Value y) {
    assert(x.type == Integer);
    assert(y.type == Integer);
    Value res;
    res.type = Integer;
    res.val = x.val + y.val;
    return res;
}

Value eq(Value x, Value y) {
    assert(x.type == Integer);
    assert(y.type == Integer);
    Value res;
    res.type = Boolean;
    res.val = (x.val == y.val);
    return res;
}

Value le(Value x, Value y) {
    assert(x.type == Integer);
    assert(y.type == Integer);
    Value res;
    res.type = Boolean;
    res.val = (x.val <= y.val);
    return res;
}

Value lt(Value x, Value y) {
    assert(x.type == Integer);
    assert(y.type == Integer);
    Value res;
    res.type = Boolean;
    res.val = (x.val < y.val);
    return res;
}

Value ge(Value x, Value y) {
    assert(x.type == Integer);
    assert(y.type == Integer);
    Value res;
    res.type = Boolean;
    res.val = (x.val >= y.val);
    return res;
}

Value gt(Value x, Value y) {
    assert(x.type == Integer);
    assert(y.type == Integer);
    Value res;
    res.type = Boolean;
    res.val = (x.val > y.val);
    return res;
}
