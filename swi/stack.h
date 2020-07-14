#ifndef STACK_H_INCLUDED
#define STACK_H_INCLUDED

#ifdef PROLOG_INTERPRETER
typedef term_t Value;
#else
#include "objspace.h"
#endif

typedef struct stack {
    Value *vals;
    size_t len;
    size_t size;
} stack;

stack *new_stack(void);
void push(stack *s, Value x);
Value pop(stack *s);
void teardown_stack(stack **s);

#endif
