#include <sicstus/sicstus.h>
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#include <sys/time.h>

#define PROLOG_INTERPRETER
#include "stack.h"

static int decode_integer1(unsigned char *bytes) {
    if (bytes[0] >= 128) {
        return -(((~bytes[0]) & 255) +1);
    } else {
        return bytes[0];
    }
}

static int decode_integer4(unsigned char *bytes) {
    int val;
    if (bytes[3] >= 128) {
        val = -(((~bytes[3]) & 255) + 1) << 24;
    } else {
        val = bytes[3] << 24;
    }
    val |= (bytes[0] | (bytes[1] << 8) | (bytes[2] << 16));
    return val;
}


static SP_pred_ref init_predicate(const char *name, int arity, const char *module) {
    SP_pred_ref pred = SP_predicate(name,arity,module);
    if (pred == NULL) {
        fprintf(stderr, "Unable to find callback module %s:%s/%i for c interpreter, exiting.\n",
        module, name, arity);
        exit(1);
    }
    return pred;
}

// why use a map if we can just use an array?
static SP_pred_ref pred_table[256];
enum {
      JMP    = 10,
      JMP_F  = 11,
      PUSH1  = 20,
      PUSH4  = 21,
      LOAD   = 40,
      ASSIGN = 45,
      MOD    = 197,
      MUL    = 198,
      SUB    = 199,
      ADD    = 200,
      NOT    = 240,
      EQ     = 251,
      LE     = 252,
      LT     = 253,
      GE     = 254,
      GT     = 255
     };

void init_pred_table(SP_atom objspace) {
    char const *module = SP_string_from_atom(objspace);
    pred_table[JMP_F]  = init_predicate("is_falsey",      1, module);
    pred_table[23]     = init_predicate("create_integer", 2, module);
    pred_table[LOAD]   = init_predicate("lookup",         3, module);
    pred_table[41]     = init_predicate("int_to_varname", 3, module);
    pred_table[ASSIGN] = init_predicate("store",          4, module);
    pred_table[MOD]    = init_predicate("mod",            3, module);
    pred_table[MUL]    = init_predicate("mul",            3, module);
    pred_table[SUB]    = init_predicate("sub",            3, module);
    pred_table[ADD]    = init_predicate("add",            3, module);
    pred_table[NOT]    = init_predicate("not",            2, module);
    pred_table[EQ]     = init_predicate("eq",             3, module);
    pred_table[LE]     = init_predicate("le",             3, module);
    pred_table[LT]     = init_predicate("lt",             3, module);
    pred_table[GE]     = init_predicate("ge",             3, module);
    pred_table[GT]     = init_predicate("gt",             3, module);
}

unsigned char *list_to_char_ptr(SP_term_ref bclist, size_t len) {
    unsigned char *bc = (unsigned char*) SP_malloc(len+1); // FIXME: len SHOULD be enough
    size_t count;
    SP_term_ref tail = SP_new_term_ref();
    SP_get_list_n_bytes(bclist, tail, len, &count, bc);
    return bc;
}

void free_char_ptr(unsigned char *bc) {
    SP_free(bc);
}


SP_term_ref arr_to_list(SP_term_ref *arr, int len) {
    SP_term_ref tail = SP_new_term_ref();
    SP_term_ref list;
    int i;
    for (i = len-1; i >= 0; i--) {
        list = SP_new_term_ref();
        SP_cons_list(list, arr[i], tail);
        tail = list;
    }
    return tail;
}

SP_term_ref *environment = NULL;
int environment_length = 0;


SP_term_ref get_ref(stack *unused_refs) {
    if (unused_refs->len != 0) {
        return pop(unused_refs);
    }
    return SP_new_term_ref();
}


void run_bc2(unsigned char *bc, int len) {
    assert(environment != NULL);
    int pc = 0;
    int rc;
    unsigned char op;
    stack *s = new_stack();

    stack *unused_refs = new_stack();

    while (pc < len) {
        op = bc[pc];
        switch(op) {
            case JMP:
                pc = decode_integer4(bc + pc + 1);
                break;
            case JMP_F: {
                    SP_term_ref arg = pop(s);
                    rc = SP_query_cut_fail(pred_table[op], arg);
                    if (rc == SP_SUCCESS) {
                        pc = decode_integer4(bc + pc + 1);
                    } else if (rc == SP_FAILURE) {
                        pc += 5;
                    } else {
                        // something VERY bad just happened
                        assert(0);
                    }
                    push(unused_refs, arg);
                    break;
                }
            case PUSH1: {
                    SP_term_ref val = get_ref(unused_refs);
                    SP_put_integer(val, decode_integer1(bc + pc + 1));
                    SP_term_ref var = get_ref(unused_refs);
                    SP_put_variable(var);
                    rc = SP_query(pred_table[23], val, var);
                    assert(rc == SP_SUCCESS);
                    push(s, var);
                    push(unused_refs, val);
                }
                pc += 2;
                break;
            case PUSH4: {
                    SP_term_ref val = get_ref(unused_refs);
                    SP_put_integer(val, decode_integer4(bc + pc + 1));
                    SP_term_ref var = get_ref(unused_refs);
                    SP_put_variable(var);
                    rc = SP_query(pred_table[23], val, var);
                    assert(rc == SP_SUCCESS);
                    push(s, var);
                    push(unused_refs, val);
                }
                pc += 5;
                break;
            case LOAD: {
                    int index = decode_integer4(bc + pc + 1);
                    SP_term_ref val = get_ref(unused_refs);
                    SP_put_term(val, environment[index]);
                    push(s, val);
                }
                pc += 5;
                break;
            case ASSIGN: {
                    int index = decode_integer4(bc + pc + 1);
                    push(unused_refs, environment[index]);
                    environment[index] = pop(s);
                }
                pc += 5;
                break;
            case NOT: {
                    SP_term_ref arg = pop(s);
                    SP_term_ref var = get_ref(unused_refs);
                    SP_put_variable(var);
                    rc = SP_query(pred_table[op], arg, var);
                    assert(rc == SP_SUCCESS);
                    push(s, var);
                    push(unused_refs, arg);
                }
                pc++;
                break;
            case MOD:
            case MUL:
            case SUB:
            case ADD:
            case EQ:
            case LE:
            case LT:
            case GE:
            case GT: {
                    SP_term_ref arg2 = pop(s);
                    SP_term_ref arg1 = pop(s);
                    SP_term_ref var = get_ref(unused_refs);
                    SP_put_variable(var);
                    rc = SP_query(pred_table[op], arg1, arg2, var);
                    assert(rc == SP_SUCCESS);
                    push(s, var);
                    push(unused_refs, arg1);
                    push(unused_refs, arg2);
                }
                pc++;
                break;
            default:
                printf("Illegal instruction: %d\n", pc[bc]);
                assert(0);
                break;
        }
    }
    assert(s->len == 0);
    teardown_stack(&s);
    teardown_stack(&unused_refs);
}

void setup_environment(unsigned char *bc, int len) {
    // we only support push command at the beginning
    if (bc[0] == PUSH1) {
        assert(bc[2] == 45);
        environment_length = decode_integer4(bc + 1 + 1 + 1) + 1;
    } else if (bc[0] == PUSH4) {
        assert(bc[5] == 45);
        environment_length = decode_integer4(bc + 1 + 4 + 1) + 1;
    } else {
        assert(0);
    }
    // try valloc
    environment = (SP_term_ref*) SP_calloc(len, sizeof(SP_term_ref));
    int i;
    for (i = 0; i < len; i++) {
        environment[i] = SP_new_term_ref();
    }
    run_bc2(bc, len);
}

SP_term_ref retrieve_environment(void) {
    SP_term_ref renv = arr_to_list(environment, environment_length);
    return renv;
}

SP_term_ref run_bc(unsigned char *bc, int len, unsigned char *bc_env, int envlen) {
    setup_environment(bc_env, envlen);
    
    puts("C here");
    struct timeval tval_before, tval_after, tval_result;
    gettimeofday(&tval_before, NULL);
    run_bc2(bc, len);

    gettimeofday(&tval_after, NULL);
    timersub(&tval_after, &tval_before, &tval_result);
    puts("done");
    printf("Time elapsed: %ld.%06ld\n", (long int)tval_result.tv_sec, (long int)tval_result.tv_usec);
    return retrieve_environment();
}
