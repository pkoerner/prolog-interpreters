#include <stdlib.h>
#include <assert.h>

#include "stack.h"

#define INITIAL_SIZE    16


stack *new_stack(void) {
    stack *res = (stack*) malloc(sizeof(stack));
    res->vals = (Value*) calloc(INITIAL_SIZE, sizeof(Value));
    res->len = 0;
    res->size = INITIAL_SIZE;
    return res;
}

void push(stack *s, Value x) {
    s->len++;
    if (s->len == s->size) {
        s->vals = realloc(s->vals, s->size * 2 * sizeof(Value));
        assert(s->vals);
        s->size *= 2;
    }
    s->vals[s->len] = x;
}

Value pop(stack *s) {
    s->len--;
    return s->vals[s->len + 1];
}

void teardown_stack(stack **s) {
    free((*s)->vals);
    free(*s);
    *s = NULL;
}
