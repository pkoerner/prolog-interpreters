#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "stack.h"
#include "objspace.h"

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



// why use a map if we can just use an array?
static Value (*function_table_unary[256])(Value);
static Value (*function_table_binary[256])(Value, Value);
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

// TODO: fill tables

void init_table(void) {
    function_table_binary[MOD] = mod;
    function_table_binary[MUL] = mul;
    function_table_binary[SUB] = sub;
    function_table_binary[ADD] = add;
    function_table_binary[EQ] = eq;
    function_table_binary[LE] = le;
    function_table_binary[LT] = lt;
    function_table_binary[GE] = ge;
    function_table_binary[GT] = gt;
}

Value *environment;
int environment_length;

void run_bc(unsigned char *bc, int bc_len) {
    int pc = 0;
    Value res;
    stack *s = new_stack();

    while (pc < bc_len) {
        switch(bc[pc]) {
            case JMP:
                pc = decode_integer4(bc + pc + 1);
                break;
            case JMP_F: {
                    Value x = pop(s);
                    res = is_falsey(x);
                    if (bool_to_int(res)) {
                        pc = decode_integer4(bc + pc + 1);
                    } else {
                        pc += 5;
                    }
                    break;
                }
            case PUSH1: {
                    int tmp = decode_integer1(bc + pc + 1);
                    Value val = make_int(tmp);
                    push(s, val);
                }
                pc += 2;
                break;
            case PUSH4: {
                    int x = decode_integer4(bc + pc + 1);
                    Value val = make_int(x);
                    push(s, val);
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
                    environment[index] = pop(s);
                }
                pc += 5;
                break;
            case NOT: {
                    Value arg = pop(s);
                    Value val = (*function_table_unary[bc[pc]])(arg);
                    push(s, val);
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
                    Value arg2 = pop(s);
                    Value arg1 = pop(s);
                    Value val = (*function_table_binary[bc[pc]])(arg1, arg2);
                    push(s, val);
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
    free(s);
}

unsigned char *read_bytecode_from_file(char *filename, int *sz) {
    FILE *f = fopen(filename, "r");
    int rc = fseek(f, 0, SEEK_END);
    assert(rc == 0);

    
    *sz = ftell(f);
    rc = fseek(f, 0, SEEK_SET);

    unsigned char *res = (unsigned char*) malloc(*sz);
    assert(res);

    rc = fread(res, 1, *sz, f);
    assert(rc == *sz);

    fclose(f);

    return res;
}

void init_environment(unsigned char *bc, int len) {
    if (bc[0] == PUSH1) {
        assert(bc[2] == 45);
        environment_length = decode_integer4(bc + 1 + 1 + 1) + 1;
    } else if (bc[0] == PUSH4) {
        assert(bc[5] == 45);
        environment_length = decode_integer4(bc + 1 + 4 + 1) + 1;
    } else {
        assert(0);
    }
    environment = (Value*) calloc(len, sizeof(Value));
    run_bc(bc, len);
}

void destroy_environment(void) {
    int i;
    for (i = 0; i < environment_length; i++) {
        Value x = environment[i];
        assert(x.type == Integer);
        printf("%d\n", x.val);
    }
    free(environment);
}


int main(int argc, char *argv[]) {
    if (argc < 2) {
        fprintf(stderr, "Usage: ./c_interpreter <path_to_bytecode>");
        assert(0);
    }
    init_table();
    char *bc_loc = argv[1];
    int name_len = strlen(bc_loc) + 4 + 1;
    char env_loc[name_len];
    memcpy(env_loc, bc_loc, strlen(bc_loc));
    memcpy(env_loc + strlen(bc_loc), "_env", 4);
    env_loc[name_len-1] = 0;

    int env_bc_len;
    int bc_len;
    unsigned char *env_bc = read_bytecode_from_file(env_loc, &env_bc_len);
    unsigned char *bc = read_bytecode_from_file(bc_loc, &bc_len);

    init_environment(env_bc, env_bc_len);
    run_bc(bc, bc_len);


    destroy_environment();


    return 0;
}
