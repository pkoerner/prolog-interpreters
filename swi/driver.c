//#include <sicstus/sicstus.h>
#include <SWI-Prolog.h>
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
    val += bytes[0] + (bytes[1] << 8) + (bytes[2] << 16);
    return val;
}


static predicate_t init_predicate(const char *name, int arity, const char *module) {
    predicate_t pred = PL_predicate(name,arity,module);
    if (pred == NULL) {
        fprintf(stderr, "Unable to find callback module %s:%s/%i for c interpreter, exiting.\n",
        module, name, arity);
        exit(1);
    }
    return pred;
}

// why use a map if we can just use an array?
static predicate_t pred_table[256];
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

foreign_t init_pred_table(term_t objspace) {
    assert(PL_is_atom(objspace));
    char *module;
    int rc = PL_get_atom_chars(objspace, &module);
    assert(rc == TRUE);
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
    PL_succeed;
}

static foreign_t pl_transform_list(term_t list, term_t bytecode_ptr) {
    assert(PL_is_list(list));
    assert(PL_is_variable(bytecode_ptr));

    unsigned char *dest;
    int rc = PL_get_chars(list, (char**) &dest, CVT_LIST | BUF_MALLOC);
    assert(rc == TRUE);

    term_t tmp = PL_new_term_ref();
    rc = PL_put_pointer(tmp, dest);
    assert(rc == TRUE);
    rc = PL_unify(tmp, bytecode_ptr);
    assert(rc == TRUE);
    PL_succeed;
}

foreign_t free_char_ptr(term_t bc_ptr) {
    assert(PL_is_integer(bc_ptr));
    void *bc;
    int rc = PL_get_pointer(bc_ptr, &bc);
    assert(rc == TRUE);
    PL_free(bc);
    PL_succeed;
}


term_t arr_to_list(term_t *arr, int len) {
    term_t list = PL_new_term_ref();
    PL_put_nil(list);

    int i;
    for (i = len-1; i >= 0; i--) {
        int rc = PL_cons_list(list, arr[i], list);
        assert(rc == TRUE);
    }
    return list;
}

term_t *environment = NULL;
int environment_length = 0;



void run_bc2(unsigned char *bc, int len) {
    assert(environment != NULL);

    int pc = 0;
    int rc;
    stack *s = new_stack();

    fid_t frame;
    const int flags = PL_Q_NORMAL;
    while (pc < len) {
        if (s->len == 0) {
            frame = PL_open_foreign_frame();
        }
        switch(bc[pc]) {
            case JMP:
                pc = decode_integer4(bc + pc + 1);
                break;
            case JMP_F: {
                    term_t arg = pop(s);
                    // TODO: query arguments have to be consecutive?
                    rc = PL_call_predicate(NULL, flags, pred_table[bc[pc]], arg);
                    if (rc == TRUE) {
                        pc = decode_integer4(bc + pc + 1);
                    } else if (rc == FALSE) {
                        pc += 5;
                    } else {
                        // something VERY bad just happened
                        assert(0);
                    }
                    break;
                }
            case PUSH1: {
                    term_t val = PL_new_term_refs(2);
                    term_t var = val+1;
                    rc = PL_put_integer(val, decode_integer1(bc + pc + 1));
                    assert(rc == TRUE);
                    rc = PL_call_predicate(NULL, flags, pred_table[23], val);
                    assert(rc == TRUE);
                    push(s, var);
                }
                pc += 2;
                break;
            case PUSH4: {
                    term_t val = PL_new_term_refs(2);
                    term_t var = val+1;
                    rc = PL_put_integer(val, decode_integer4(bc + pc + 1));
                    assert(rc == TRUE);
                    rc = PL_call_predicate(NULL, flags, pred_table[23], val);
                    assert(rc == TRUE);
                    push(s, var);
                }
                pc += 5;
                break;
            case LOAD: {
                    int index = decode_integer4(bc + pc + 1);
                    push(s, environment[index]);
                }
                pc += 5;
                break;
            case ASSIGN: {
                    int index = decode_integer4(bc + pc + 1);
                    term_t tmp = pop(s);
                    rc = PL_put_term(environment[index], tmp);
                    assert(rc == TRUE);
                }
                pc += 5;
                break;
            case NOT: {
                    term_t arg = PL_new_term_refs(2);
                    term_t var = arg+1;
                    rc = PL_put_term(arg, pop(s));
                    assert(rc == TRUE);
                    rc = PL_call_predicate(NULL, flags, pred_table[bc[pc]], arg);
                    assert(rc == TRUE);
                    push(s, var);
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
                    term_t arg1 = PL_new_term_refs(3);
                    term_t arg2 = arg1 + 1;
                    term_t var = arg1 + 2;
                    PL_put_term(arg2, pop(s));
                    PL_put_term(arg1, pop(s));
                    rc = PL_call_predicate(NULL, flags, pred_table[bc[pc]], arg1);
                    assert(rc == TRUE);
                    push(s, var);
                }
                pc++;
                break;
            default:
                printf("Illegal instruction: %d\n", pc[bc]);
                assert(0);
                break;
        }

        if (s->len == 0) {
            PL_close_foreign_frame(frame);
        }
    }
    assert(s->len == 0);
    teardown_stack(&s);
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
    int i;
    environment = (term_t*) calloc(len, sizeof(term_t));
    for (i = 0; i < environment_length; i++) {
        environment[i] = PL_new_term_ref();
    }

    run_bc2(bc, len);
}

term_t retrieve_environment(void) {
    term_t renv = arr_to_list(environment, environment_length);
    return renv;
}

foreign_t run_bc(term_t bc_ptr, term_t lent, term_t bc_env_ptr, term_t envlent, term_t res_env) {
    unsigned char *bc_env;
    int rc = PL_get_pointer(bc_env_ptr, (void**) &bc_env);
    assert(rc == TRUE);
    int envlen;
    rc = PL_get_integer(envlent, &envlen);
    assert(rc == TRUE);

    setup_environment(bc_env, envlen);
    
    unsigned char *bc;
    rc = PL_get_pointer(bc_ptr, (void**) &bc);
    assert(rc == TRUE);
    int len;
    rc = PL_get_integer(lent, &len);
    assert(rc == TRUE);

    puts("C here");
    struct timeval tval_before, tval_after, tval_result;
    gettimeofday(&tval_before, NULL);
    run_bc2(bc, len);

    gettimeofday(&tval_after, NULL);
    timersub(&tval_after, &tval_before, &tval_result);
    puts("done");
    printf("Time elapsed: %ld.%06ld\n", (long int)tval_result.tv_sec, (long int)tval_result.tv_usec);

    rc = PL_unify(res_env, retrieve_environment());
    assert(rc == TRUE);
    PL_succeed;
}

install_t install_driver() {
    PL_register_foreign("run_bc", 5, run_bc, 0);
    PL_register_foreign("list_to_char_ptr", 2, pl_transform_list, 0);
    PL_register_foreign("init_pred_table", 1, init_pred_table, 0);
    PL_register_foreign("free_char_ptr", 1, free_char_ptr, 0);
}
