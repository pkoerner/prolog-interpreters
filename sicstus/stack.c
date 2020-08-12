#include <stdlib.h>
#include <assert.h>
#ifdef C_INTERPRETER
#define SP_malloc malloc
#define SP_calloc calloc
#define SP_free free
#define SP_realloc realloc
#else
#include <sicstus/sicstus.h>
#endif

#include "stack.h"

#define INITIAL_SIZE    16


stack *new_stack(void) {
    stack *res = (stack*) SP_malloc(sizeof(stack));
    res->vals = (Value*) SP_calloc(INITIAL_SIZE, sizeof(Value));
    res->len = 0;
    res->size = INITIAL_SIZE;
    return res;
}

void push(stack *s, Value x) {
    s->len++;
    if (s->len == s->size) {
        s->vals = SP_realloc(s->vals, s->size * 2 * sizeof(Value));
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
    SP_free((*s)->vals);
    SP_free(*s);
    *s = NULL;
}
