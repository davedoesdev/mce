#define _GNU_SOURCE
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdbool.h>
#include <math.h>
#include <assert.h>
#include <errno.h>
#include <string.h>
#include <unistd.h>
#include <inttypes.h>
#include <argp.h>

#define DEFAULT_MEMORY_SIZE         10
#define MEBIBYTES                   (1024 * 1024)

#define true_code                   '0'
#define false_code                  '1'
#define number_code                 '2'
#define vector_code                 '3'

#define binary_code                 'a'
#define unmemoized_code             'b'
#define result_code                 'c'
#define step_contn_code             'd'
#define transfer_code               'e'
#define gc_code                     'z'

#define symbol_lookup_form          0
#define send_value_form             1
#define constructed_function0_form  2
#define constructed_function1_form  3
#define global_lambda_form          4
#define if0_form                    5
#define if1_form                    6
#define sclis0_form                 7
#define sclis1_form                 8
#define sclis2_form                 9
#define scseq0_form                 10
#define scseq1_form                 11
#define lambda0_form                12
#define lambda1_form                13
#define improper_lambda0_form       14
#define improper_lambda1_form       15
#define letcc0_form                 16
#define letcc1_form                 17
#define define0_form                18
#define define1_form                19
#define application0_form           20
#define application1_form           21
#define evalx_initial_form          22

/*
vector is:
0: vector_code
1: length
9: for each element:
     0: index
9 + length * 8: first element (when deserialized)
*/

enum core_globals {
    g_result,
    g_applyx,
    g_is_boolean,
    g_is_number,
    g_less_than,
    g_greater_than,
    g_add,
    g_subtract,
    g_multiply,
    g_divide,
    g_is_number_equal,
    g_floor,
    g_make_vector,
    g_is_vector,
    g_vector_length,
    g_vector_ref,
    g_vector_set,
    g_is_procedure,
    g_make_binary,
    g_is_binary,
    g_binary_length,
    g_binary_ref,
    g_binary_set,
    g_error,
    g_is_same_object,
    g_transfer,
    g_transfer_test
};

typedef struct {
    uint64_t capacity;
    uint64_t size;
    uint8_t *bytes;
    uint64_t nil;
    bool gc_callback_set;
    uint64_t gc_callback;
    bool calling_gc_callback;
} memory;

typedef struct {
    bool succeeded;
    uint64_t v;
} allocation;

allocation allocate(memory *mem, uint64_t size) {
    uint64_t cur_size = mem->size;
    if (mem->capacity - cur_size < size) {
        return (allocation) { false, 0 };
    };
    mem->size += size;
    return (allocation) { true, cur_size };
}

typedef struct {
    uint64_t src_state;
    uint64_t dest_state;
} gc_todo;

/* Note: src is modified during gc() */
uint64_t gc(memory *src, uint64_t state, memory *dest) {
    gc_todo *todos = (gc_todo*) malloc((src->size + 1 /* for gc_callback */) * sizeof(gc_todo));
    assert(todos);
    uint64_t num_todos = 0;

    assert(state < src->size);
    uint64_t r = dest->size;

    if (src->gc_callback_set) {
        assert(num_todos < src->size + 1);
        todos[num_todos].src_state = src->gc_callback;
        todos[num_todos++].dest_state = 0; /* not used */
    }
    assert(num_todos < src->size + 1);
    todos[num_todos].src_state = state;
    todos[num_todos++].dest_state = 0; /* not used */

    while (num_todos > 0) {
        uint64_t src_state = todos[--num_todos].src_state;
        uint64_t dest_state = todos[num_todos].dest_state;
        if (src->gc_callback_set && (src_state == src->gc_callback)) {
            dest->gc_callback_set = true;
            dest->gc_callback = dest->size;
        } else if (src_state != state) {
            *(uint64_t*)&dest->bytes[dest_state] = dest->size;
        }

        assert(dest->size < dest->capacity);
        uint8_t code = src->bytes[src_state];
        dest->bytes[dest->size++] = code;

        switch (code) {
            case true_code:
            case false_code:
                break;

            case number_code:
                assert(src_state + sizeof(double) < src->size);
                assert(dest->size + sizeof(double) <= dest->capacity);
                memcpy(&dest->bytes[dest->size], &src->bytes[src_state + 1], sizeof(double));
                dest->size += sizeof(double);
                break;

            case binary_code: {
                assert(src_state + 8 < src->size);
                uint64_t len = *(uint64_t*)&src->bytes[src_state + 1];

                assert(dest->size + 8 <= dest->capacity);
                *(uint64_t*)&dest->bytes[dest->size] = len;
                dest->size += 8;

                assert(src_state + 8 + len < src->size);
                assert(dest->size + len <= dest->capacity);
                memcpy(&dest->bytes[dest->size], &src->bytes[src_state + 9], len);
                dest->size += len;
                break;
            }

            case vector_code: {
                assert(src_state + 8 < src->size);
                uint64_t len = *(uint64_t*)&src->bytes[src_state + 1];

                if (len == 0) {
                    dest->nil = dest->size - 1;
                }

                src->bytes[src_state] = gc_code;
                *(uint64_t*)&src->bytes[src_state + 1] = dest->size - 1;

                assert(dest->size + 8 <= dest->capacity);
                *(uint64_t*)&dest->bytes[dest->size] = len;
                dest->size += 8;

                assert(src_state + 8 + len * 8 < src->size);
                assert(dest->size + len * 8 <= dest->capacity);

                uint64_t pos = dest->size;
                dest->size += len * 8;

                for (uint64_t i = 0; i < len; ++i) {
                    assert(num_todos < src->size + 1);
                    todos[num_todos].src_state = *(uint64_t*)&src->bytes[src_state + 9 + i * 8];
                    todos[num_todos++].dest_state = pos + i * 8;
                }
                break;
            }

            case unmemoized_code:
            case result_code:
            case step_contn_code:
            case transfer_code: {
                assert(src_state + 8 < src->size);
                uint64_t vec_state = *(uint64_t*)&src->bytes[src_state + 1];

                src->bytes[src_state] = gc_code;
                *(uint64_t*)&src->bytes[src_state + 1] = dest->size - 1;

                assert(dest->size + 8 <= dest->capacity);
                uint64_t pos = dest->size;
                dest->size += 8;

                assert(num_todos < src->size + 1);
                todos[num_todos].src_state = vec_state;
                todos[num_todos++].dest_state = pos;
                break;
            }

            case gc_code:
                assert(src_state + 8 < src->size);
                if (src->gc_callback_set && (src_state == src->gc_callback)) {
                    dest->gc_callback_set = true;
                    dest->gc_callback = *(uint64_t*)&src->bytes[src_state + 1];
                } else if (src_state != state) {
                    *(uint64_t*)&dest->bytes[dest_state] = *(uint64_t*)&src->bytes[src_state + 1];
                }
                break;

            default:
                assert_perror(EINVAL);
        }
    }

    free(todos);
    assert(dest->size > 0);
    return r;
}

allocation make_uninitialised_vector(memory *mem, uint64_t n) {
    allocation state = allocate(mem, 1 + 8 + n * 8);
    if (!state.succeeded) {
        return state;
    }
    mem->bytes[state.v] = vector_code;
    *(uint64_t*)&mem->bytes[state.v + 1] = n;
    return state;
}

allocation make_vector(memory *mem, uint64_t n) {
    allocation state = make_uninitialised_vector(mem, n);
    if (!state.succeeded) {
        return state;
    }
    for (uint64_t i = 0; i < n; ++i) {
        *(uint64_t*)&mem->bytes[state.v + 9 + i * 8] = mem->nil;
    }
    return state;
}

uint64_t vector_size(memory *mem, uint64_t v) {
    assert(v + 8 < mem->size);
    assert(mem->bytes[v] == vector_code);
    uint64_t size = *(uint64_t*)&mem->bytes[v + 1];
    assert(v + 8 + size * 8 < mem->size);
    return size;
}

uint64_t vector_ref(memory *mem, uint64_t v, uint64_t i) {
    assert(v + 8 < mem->size);
    assert(mem->bytes[v] == vector_code);
    assert(i < *(uint64_t*)&mem->bytes[v + 1]);
    assert(v + 8 + (i + 1) * 8 < mem->size);
    return *(uint64_t*)&mem->bytes[v + 9 + i * 8];
}

uint64_t vector_set(memory *mem, uint64_t v, uint64_t i, uint64_t val) {
    assert(v + 8 < mem->size);
    assert(mem->bytes[v] == vector_code);
    assert(i < *(uint64_t*)&mem->bytes[v + 1]);
    assert(v + 8 + (i + 1) * 8 < mem->size);
    *(uint64_t*)&mem->bytes[v + 9 + i * 8] = val;
    return mem->nil;
}

allocation vc(memory *mem, uint64_t first, uint64_t second) {
    allocation state = make_uninitialised_vector(mem, 2);
    if (!state.succeeded) {
        return state;
    }
    vector_set(mem, state.v, 0, first);
    vector_set(mem, state.v, 1, second);
    return state;
}

allocation vc_va(memory *mem, uint64_t first, allocation second) {
    if (!second.succeeded) {
        return second;
    }
    return vc(mem, first, second.v);
}

allocation vc_av(memory* mem, allocation first, uint64_t second) {
    if (!first.succeeded) {
        return first;
    }
    return vc(mem, first.v, second);
}

allocation vc_aa(memory* mem, allocation first, allocation second) {
    if (!first.succeeded) {
        return first;
    }
    if (!second.succeeded) {
        return second;
    }
    return vc(mem, first.v, second.v);
}

#define send vc
#define send_va vc_va

allocation sendv(memory *mem, uint64_t k, uint64_t v) {
    return send_va(mem, k, vc(mem, v, mem->nil));
}

allocation sendv_va(memory *mem, uint64_t k, allocation v) {
    if (!v.succeeded) {
        return v;
    }
    return sendv(mem, k, v.v);
}

uint64_t vlist_ref(memory *mem, uint64_t vl, uint64_t i) {
    while (i > 0) {
        vl = vector_ref(mem, vl, 1);
        --i;
    }
    return vector_ref(mem, vl, 0);
}

uint64_t vlist_rest(memory *mem, uint64_t vl, uint64_t i) {
    while (i > 0) {
        vl = vector_ref(mem, vl, 1);
        --i;
    }
    return vector_ref(mem, vl, 1);
}

void vlist_set(memory *mem, uint64_t vl, uint64_t i, uint64_t val) {
    while (i > 0) {
        vl = vector_ref(mem, vl, 1);
        --i;   
    }
    vector_set(mem, vl, 0, val);
}

double double_val(memory *mem, uint64_t state) {
    assert(state + sizeof(double) < mem->size);
    assert(mem->bytes[state] == number_code);
    return *(double*)&mem->bytes[state + 1];
}

allocation make_double(memory *mem, double v) {
    allocation state = allocate(mem, 1 + sizeof(double));
    if (!state.succeeded) {
        return state;
    }
    mem->bytes[state.v] = number_code;
    *(double*)&mem->bytes[state.v + 1] = v;
    return state;
}

bool boolean_val(memory *mem, uint64_t state) {
    assert(state < mem->size);
    uint8_t code = mem->bytes[state];
    assert(code == true_code || code == false_code);
    return code == true_code;
}

allocation make_boolean(memory *mem, bool v) {
    allocation state = allocate(mem, 1);
    if (!state.succeeded) {
        return state;
    }
    mem->bytes[state.v] = v ? true_code : false_code;
    return state;
}

allocation make_binary(memory *mem, uint64_t len) {
    allocation state = allocate(mem, 1 + 8 + len);
    if (!state.succeeded) {
        return state;
    }
    mem->bytes[state.v] = binary_code;
    *(uint64_t*)&mem->bytes[state.v + 1] = len;
    return state;
}

uint64_t binary_size(memory *mem, uint64_t b) {
    assert(b + 8 < mem->size);
    assert(mem->bytes[b] == binary_code);
    uint64_t size = *(uint64_t*)&mem->bytes[b + 1];
    assert(b + 8 + size < mem->size);
    return size;
}

allocation binary_ref(memory *mem, uint64_t b, uint64_t i) {
    assert(b + 8 < mem->size);
    assert(mem->bytes[b] == binary_code);
    assert(i < *(uint64_t*)&mem->bytes[b + 1]);
    assert(b + 9 + i < mem->size);
    return make_double(mem, mem->bytes[b + 9 + i]);
}

uint64_t binary_set(memory *mem, uint64_t b, uint64_t i, uint8_t val) {
    assert(b + 8 < mem->size);
    assert(mem->bytes[b] == binary_code);
    assert(i < *(uint64_t*)&mem->bytes[b + 1]);
    assert(b + 9 + i < mem->size);
    mem->bytes[b + 9 + i] = val;
    return mem->nil;
}

allocation make_result(memory *mem, uint64_t val) {
    allocation state = allocate(mem, 1 + 8 + 1 + 8 + 8);
    if (!state.succeeded) {
        return state;
    }
    mem->bytes[state.v] = result_code;
    *(uint64_t*)&mem->bytes[state.v + 1] = state.v + 9;
    mem->bytes[state.v + 9] = vector_code;
    *(uint64_t*)&mem->bytes[state.v + 10] = 1;
    *(uint64_t*)&mem->bytes[state.v + 18] = val;
    return state;
}

uint64_t rtenv_lookup(memory *mem,
                      uint64_t i,
                      uint64_t env) {
    uint64_t first = (uint64_t)double_val(mem, vector_ref(mem, i, 0));
    uint64_t second = (uint64_t)double_val(mem, vector_ref(mem, i, 1));
    uint64_t v = vlist_ref(mem, env, first);
    if (second < vector_size(mem, v)) {
        return vector_ref(mem, v, second);
    }
    return mem->nil;
}

allocation rtenv_setvar(memory *mem,
                        uint64_t i,
                        uint64_t val,
                        uint64_t env) {
    uint64_t first = (uint64_t)double_val(mem, vector_ref(mem, i, 0));
    uint64_t second = (uint64_t)double_val(mem, vector_ref(mem, i, 1));
    uint64_t v = vlist_ref(mem, env, first);
    uint64_t size = vector_size(mem, v);
    if (second >= size) {
        allocation v2 = make_uninitialised_vector(mem, second + 1);
        if (!v2.succeeded) {
            return v2;
        }
        for (uint64_t i = 0; i < second; ++i) {
            vector_set(mem, v2.v, i, i < size ? vector_ref(mem, v, i) : mem->nil);
        }
        vlist_set(mem, env, first, v2.v);
        v = v2.v;
    }
    vector_set(mem, v, second, val);
    return (allocation) { true, val };
}

allocation save(memory *mem, uint64_t state) {
    memory copy = *mem;
    copy.bytes = (uint8_t*) malloc(mem->size);
    assert(copy.bytes);
    memcpy(copy.bytes, mem->bytes, mem->size);

    memory dest = {
        mem->size,
        0,
        (uint8_t*) malloc(mem->size),
        0,
        false,
        0,
        false
    };
    assert(dest.bytes);

    gc(&copy, state, &dest);
    free(copy.bytes);

    allocation b = make_binary(mem, 8 + dest.size);
    if (b.succeeded) {
        memcpy(&mem->bytes[b.v + 9], &dest.size, 8);
        memcpy(&mem->bytes[b.v + 9 + 8], dest.bytes, dest.size);
    }
    free(dest.bytes);
    return b;
}

uint64_t restore(memory *mem, uint64_t b) {
    uint64_t size = binary_size(mem, b);
    assert(b + 8 + size < mem->size);
    size -= 8;
    assert(*(uint64_t*)&mem->bytes[b + 9] == size);
    uint8_t *bytes = (uint8_t*) malloc(size);
    assert(bytes);
    memcpy(bytes, &mem->bytes[b + 9 + 8], size);

    memory src = {
        size,
        size,
        bytes,
        0,
        false,
        0,
        false
    };

    size = mem->size;
    gc(&src, 0, mem);

    return size;
}

allocation vlist_to_vector(memory *mem, uint64_t vl, uint64_t len) {
    allocation v = make_uninitialised_vector(mem, len);
    if (!v.succeeded) {
        return v;
    }

    uint64_t i = 0;

    while (assert(vl < mem->size), (i < len)) {
        bool is_vector = mem->bytes[vl] == vector_code;
        uint64_t size = is_vector ? vector_size(mem, vl) : 0;

        if (is_vector && (size == 2)) {
            vector_set(mem, v.v, i++, vector_ref(mem, vl, 0));
            vl = vector_ref(mem, vl, 1);
        } else {
            if (!(is_vector && (size == 0))) {
                vector_set(mem, v.v, i++, vl);
            }
            break;
        }
    }

    while (i < len) {
        vector_set(mem, v.v, i++, mem->nil);
    }

    return v;
}

allocation extend_rtenv(memory *mem,
                        uint64_t env,
                        uint64_t len,
                        uint64_t values) {
    return vc_av(mem, vlist_to_vector(mem, values, len), env);
}

allocation extend_rtenv_a(memory *mem,
                          uint64_t env,
                          uint64_t len,
                          allocation values) {
    if (!values.succeeded) {
        return values;
    }
    return extend_rtenv(mem, env, len, values.v);
}

allocation improper_extend_rtenv(memory *mem,
                                 uint64_t env,
                                 uint64_t len,
                                 uint64_t values) {
    allocation v = make_uninitialised_vector(mem, len);
    if (!v.succeeded) {
        return v;
    }

    uint64_t i = 0;

    while (assert(values < mem->size), (i < len)) {
        bool is_vector = mem->bytes[values] == vector_code;
        uint64_t size = is_vector ? vector_size(mem, values) : 0;

        if ((i == (len - 1)) || !is_vector || (size != 2)) {
            if (!is_vector || (size != 0)) {
                vector_set(mem, v.v, i++, values);
            }
            break;
        }

        vector_set(mem, v.v, i++, vector_ref(mem, values, 0));
        values = vector_ref(mem, values, 1);
    }

    while (i < len) {
        vector_set(mem, v.v, i++, mem->nil);
    }

    return vc(mem, v.v, env);
}

allocation make_step_contn(memory *mem, uint64_t k, uint64_t env, uint64_t args) {
    allocation state = allocate(mem, 1 + 8 + 1 + 8 + 8 + 8 + 8);
    if (!state.succeeded) {
        return state;
    }
    mem->bytes[state.v] = step_contn_code;
    *(uint64_t*)&mem->bytes[state.v + 1] = state.v + 9;
    mem->bytes[state.v + 9] = vector_code;
    *(uint64_t*)&mem->bytes[state.v + 10] = 3;
    *(uint64_t*)&mem->bytes[state.v + 18] = k;
    *(uint64_t*)&mem->bytes[state.v + 26] = env;
    *(uint64_t*)&mem->bytes[state.v + 34] = args;
    return state;
}

uint64_t step_contn_k(memory *mem, uint64_t state) {
    assert(state + 8 < mem->size);
    assert(mem->bytes[state] == step_contn_code);
    return vector_ref(mem, *(uint64_t*)&mem->bytes[state + 1], 0);
}

uint64_t step_contn_env(memory *mem, uint64_t state) {
    assert(state + 8 < mem->size);
    assert(mem->bytes[state] == step_contn_code);
    return vector_ref(mem, *(uint64_t*)&mem->bytes[state + 1], 1);
}

uint64_t step_contn_args(memory *mem, uint64_t state) {
    assert(state + 8 < mem->size);
    assert(mem->bytes[state] == step_contn_code);
    return vector_ref(mem, *(uint64_t*)&mem->bytes[state + 1], 2);
}

allocation transfer(memory *mem, uint64_t args) {
    allocation state = allocate(mem, 1 + 8 + 1 + 8 + 8);
    if (!state.succeeded) {
        return state;
    }
    mem->bytes[state.v] = transfer_code;
    *(uint64_t*)&mem->bytes[state.v + 1] = state.v + 9;
    mem->bytes[state.v + 9] = vector_code;
    *(uint64_t*)&mem->bytes[state.v + 10] = 1;
    *(uint64_t*)&mem->bytes[state.v + 18] = vector_ref(mem, args, 1);
    return send(mem, vector_ref(mem, args, 0), state.v);
}

uint64_t transfer_args(memory *mem, uint64_t state) {
    assert(state + 8 < mem->size);
    assert(mem->bytes[state] == transfer_code);
    return vector_ref(mem, *(uint64_t*)&mem->bytes[state + 1], 0);
}

allocation less_than(memory *mem, uint64_t args) {
    return make_boolean(mem, 
        double_val(mem, vlist_ref(mem, args, 0)) <
        double_val(mem, vlist_ref(mem, args, 1)));
}

allocation greater_than(memory *mem, uint64_t args) {
    return make_boolean(mem,
        double_val(mem, vlist_ref(mem, args, 0)) >
        double_val(mem, vlist_ref(mem, args, 1)));
}

allocation add(memory *mem, uint64_t args) {
    return make_double(mem,
        double_val(mem, vlist_ref(mem, args, 0)) +
        double_val(mem, vlist_ref(mem, args, 1)));
}

allocation subtract(memory *mem, uint64_t args) {
    return make_double(mem,
        double_val(mem, vlist_ref(mem, args, 0)) -
        double_val(mem, vlist_ref(mem, args, 1)));
}

allocation multiply(memory *mem, uint64_t args) {
    return make_double(mem,
        double_val(mem, vlist_ref(mem, args, 0)) *
        double_val(mem, vlist_ref(mem, args, 1)));
}

allocation divide(memory *mem, uint64_t args) {
    return make_double(mem,
        double_val(mem, vlist_ref(mem, args, 0)) /
        double_val(mem, vlist_ref(mem, args, 1)));
}

allocation is_same_object(memory *mem, uint64_t args) {
    uint64_t x = vlist_ref(mem, args, 0);
    uint64_t y = vlist_ref(mem, args, 1);

    assert(x < mem->size);
    assert(y < mem->size);

    return make_boolean(mem, x == y);
}

uint64_t error(memory *mem, uint64_t args) {
    uint64_t proc = vlist_ref(mem, args, 0);
    uint64_t proc_len = binary_size(mem, proc);
    fwrite(&mem->bytes[proc + 10], 1, proc_len - 1, stderr);
    fprintf(stderr, ": ");

    uint64_t msg = vlist_ref(mem, args, 1);
    uint64_t msg_len = binary_size(mem, msg);
    fwrite(&mem->bytes[msg + 10], 1, msg_len - 1, stderr);
    fprintf(stderr, "\n");

    assert(false);
    return mem->nil;
}

uint64_t output_binary(memory *mem, uint64_t args, FILE *stream) {
    uint64_t b = vlist_ref(mem, args, 0);
    uint64_t b_len = binary_size(mem, b);

    uint64_t start = (uint64_t) double_val(mem, vlist_ref(mem, args, 1));
    uint64_t end = (uint64_t) double_val(mem, vlist_ref(mem, args, 2));

    assert(start <= end);
    assert(end <= b_len);

    fwrite(&mem->bytes[b + 9 + start], 1, end - start, stream);

    return mem->nil;
}

allocation is_vector(memory *mem, uint64_t args) {
    uint64_t arg = vector_ref(mem, args, 0);
    assert(arg < mem->size);
    return make_boolean(mem, mem->bytes[arg] == vector_code);
}

allocation is_binary(memory *mem, uint64_t args) {
    uint64_t arg = vector_ref(mem, args, 0);
    assert(arg < mem->size);
    return make_boolean(mem, mem->bytes[arg] == binary_code);
}

allocation is_procedure(memory *mem, uint64_t args) {
    uint64_t arg = vector_ref(mem, args, 0);
    assert(arg < mem->size);
    return make_boolean(mem, mem->bytes[arg] == unmemoized_code);
}

allocation is_boolean(memory *mem, uint64_t args) {
    uint64_t arg = vector_ref(mem, args, 0);
    assert(arg < mem->size);
    uint8_t code = mem->bytes[arg];
    return make_boolean(mem, code == true_code || code == false_code);
}

allocation is_number(memory *mem, uint64_t args) {
    uint64_t arg = vector_ref(mem, args, 0);
    assert(arg < mem->size);
    return make_boolean(mem, mem->bytes[arg] == number_code);
}

allocation is_number_equal(memory *mem, uint64_t args) {
    return make_boolean(mem,
        double_val(mem, vlist_ref(mem, args, 0)) ==
        double_val(mem, vlist_ref(mem, args, 1)));
}

allocation symbol_lookup(memory *mem,
                         uint64_t form_args,
                         uint64_t args) {
    uint64_t i = vlist_ref(mem, form_args, 0);
    uint64_t k = vlist_ref(mem, args, 0);
    uint64_t env = vlist_ref(mem, args, 1);
    return sendv(mem, k, rtenv_lookup(mem, i, env));
}

allocation send_value(memory *mem,
                      uint64_t form_args,
                      uint64_t args) {
    uint64_t exp = vlist_ref(mem, form_args, 0);
    uint64_t k = vlist_ref(mem, args, 0);
    return sendv(mem, k, exp);
}

allocation make_form(memory *mem, double form, uint64_t args) {
    allocation state = allocate(mem, 1 + 8 + 1 + 8 + 8);
    if (!state.succeeded) {
        return state;
    }
    allocation form_state = vc_av(mem, make_double(mem, form), args);
    if (!form_state.succeeded) {
        return form_state;
    }
    mem->bytes[state.v] = unmemoized_code;
    *(uint64_t*)&mem->bytes[state.v + 1] = state.v + 9;
    mem->bytes[state.v + 9] = vector_code;
    *(uint64_t*)&mem->bytes[state.v + 10] = 1;
    *(uint64_t*)&mem->bytes[state.v + 18] = form_state.v;
    return state;
}

allocation make_form_a(memory* mem, double form, allocation args) {
    if (!args.succeeded) {
        return args;
    }
    return make_form(mem, form, args.v);
}

allocation make_global_rtenv(memory *mem) {
    return vc(mem, mem->nil, mem->nil);
}

allocation applyx(memory *mem,
                  uint64_t k,
                  uint64_t env,
                  uint64_t fn,
                  uint64_t args) {
    return send_va(mem, fn, make_step_contn(mem, k, env, args));
}

allocation applyx_a(memory *mem,
                    allocation k,
                    allocation env,
                    uint64_t fn,
                    allocation args) {
    if (!k.succeeded) {
        return k;
    }
    if (!env.succeeded) {
        return env;
    }
    if (!args.succeeded) {
        return args;
    }
    return applyx(mem, k.v, env.v, fn, args.v);
}

allocation transfer_test(memory *mem, uint64_t args) {
    allocation form_state = make_form_a(mem, global_lambda_form,
                                        vc_av(mem, make_double(mem, g_result), mem->nil));
    if (!form_state.succeeded) {
        return form_state;
    }
    allocation genv_state = make_global_rtenv(mem);
    if (!genv_state.succeeded) {
        return genv_state;
    }
    return applyx(mem,
                  form_state.v,
                  genv_state.v,
                  vector_ref(mem, args, 0),
                  vector_ref(mem, args, 1));
}

/* Hack for cf-test-aux */
allocation make_symbol(memory *mem, char *s) {
    size_t len = strlen(s); 
    allocation b = make_binary(mem, len + 1);
    if (!b.succeeded) {
        return b;
    }
    mem->bytes[b.v + 9] = 'D';
    memcpy(&mem->bytes[b.v + 10], s, len);
    return b;
}

allocation cf_test(memory *mem, uint64_t args) {
    double n = double_val(mem, vlist_ref(mem, args, 0));
    uint64_t x = vlist_ref(mem, args, 1);
    if (n == 0) {
        /* Note since we lack the duality of the other implementations
           (where we can return a function which has logic and can also return
           its own definition when requested), we have to split the logic into
           a separate helper global, cf-test-aux - which is not interoperable
           with the other implementations. */
        return make_form_a(mem, global_lambda_form,
                           vc_aa(mem, make_symbol(mem, "cf-test-aux"),
                                 vc(mem, x, mem->nil)));
    }
    return make_double(mem, double_val(mem, x) + n);
}

allocation cf_test_aux(memory *mem, uint64_t x, uint64_t args) {
    allocation state = vc_va(mem, vlist_ref(mem, args, 0), vc(mem, x, mem->nil));
    if (!state.succeeded) {
        return state;
    }
    return cf_test(mem, state.v);
}

allocation constructed_function0(memory *mem,
                               uint64_t form_args,
                               uint64_t args) {
    uint64_t args2 = vlist_ref(mem, form_args, 0);
    uint64_t cf = vlist_ref(mem, form_args, 1);
    return applyx_a(mem, make_form_a(mem, constructed_function1_form, vc(mem, args, mem->nil)),
                    make_global_rtenv(mem), cf, (allocation) { true, args2 });
}

allocation constructed_function1(memory *mem,
                                 uint64_t form_args,
                                 uint64_t args) {
    uint64_t args2 = vlist_ref(mem, form_args, 0);
    uint64_t f = vlist_ref(mem, args, 0);
    return send(mem, f, args2);
}

bool symbol_equals(memory *mem, uint64_t state, char *s) {
    size_t len = strlen(s);
    return (binary_size(mem, state) == (len + 1)) &&
           (memcmp(&mem->bytes[state + 10], s, len) == 0);
}

allocation global_lambda(memory *mem,
                         uint64_t form_args,
                         uint64_t args) {
    uint64_t defn = vlist_ref(mem, form_args, 0);
    assert(defn < mem->size);
    double g = mem->bytes[defn] == number_code ? double_val(mem, defn) : -1;

    assert(args < mem->size);
    if (mem->bytes[args] == transfer_code) {
        args = transfer_args(mem, args);

        if (g == g_transfer_test) {
            return applyx_a(mem,
                            make_form_a(mem, global_lambda_form,
                                        vc_av(mem, make_double(mem, g_result), mem->nil)),
                            make_global_rtenv(mem),
                            vlist_ref(mem, args, 0),
                            (allocation) { true, vlist_rest(mem, args, 0) });
        }

        assert_perror(EINVAL);
        return (allocation) { false, 0 };
    }

    if (g == g_result) {
        return make_result(mem, vector_ref(mem, args, 0));
    }

    uint64_t k = step_contn_k(mem, args);
    uint64_t env = step_contn_env(mem, args);
    args = step_contn_args(mem, args);

    switch ((int) g) {
        case g_applyx:
            return applyx(mem,
                          k,
                          env,
                          vlist_ref(mem, args, 0),
                          vlist_ref(mem, args, 1));

        case g_is_boolean:
            return sendv_va(mem, k, is_boolean(mem, args));

        case g_is_number:
            return sendv_va(mem, k, is_number(mem, args));

        case g_less_than:
            return sendv_va(mem, k, less_than(mem, args));

        case g_greater_than:
            return sendv_va(mem, k, greater_than(mem, args));

        case g_add:
            return sendv_va(mem, k, add(mem, args));

        case g_subtract:
            return sendv_va(mem, k, subtract(mem, args));

        case g_multiply:
            return sendv_va(mem, k, multiply(mem, args));

        case g_divide:
            return sendv_va(mem, k, divide(mem, args));

        case g_is_number_equal:
            return sendv_va(mem, k, is_number_equal(mem, args));

        case g_floor:
            return sendv_va(mem, k, make_double(mem, floor(double_val(mem, vector_ref(mem, args, 0)))));

        case g_make_vector:
            return sendv_va(mem, k, make_vector(mem, double_val(mem, vector_ref(mem, args, 0))));

        case g_is_vector:
            return sendv_va(mem, k, is_vector(mem, args));

        case g_vector_length:
            return sendv_va(mem, k, make_double(mem, vector_size(mem, vector_ref(mem, args, 0))));

        case g_vector_ref:
            return sendv(mem, k, vector_ref(mem,
                                            vlist_ref(mem, args, 0),
                                            double_val(mem, vlist_ref(mem, args, 1))));

        case g_vector_set:
            return sendv(mem, k, vector_set(mem,
                                            vlist_ref(mem,args, 0),
                                            double_val(mem, vlist_ref(mem, args, 1)),
                                            vlist_ref(mem, args, 2)));

        case g_is_procedure:
            return sendv_va(mem, k, is_procedure(mem, args));

        case g_make_binary:
            return sendv_va(mem, k, make_binary(mem, double_val(mem, vector_ref(mem, args, 0))));

        case g_is_binary:
            return sendv_va(mem, k, is_binary(mem, args));

        case g_binary_length:
            return sendv_va(mem, k, make_double(mem, binary_size(mem, vector_ref(mem, args, 0))));

        case g_binary_ref:
            return sendv_va(mem, k, binary_ref(mem,
                                             vlist_ref(mem, args, 0),
                                             double_val(mem, vlist_ref(mem, args, 1))));

        case g_binary_set:
            return sendv(mem, k, binary_set(mem,
                                            vlist_ref(mem,args, 0),
                                            double_val(mem, vlist_ref(mem, args, 1)),
                                            double_val(mem, vlist_ref(mem, args, 2))));

        case g_error:
            return sendv(mem, k, error(mem, args));

        case g_is_same_object:
            return sendv_va(mem, k, is_same_object(mem, args));

        case g_transfer:
            return transfer(mem, args);
    }

    if (symbol_equals(mem, defn, "save")) {
        return sendv_va(mem, k, save(mem, vector_ref(mem, args, 0)));
    }

    if (symbol_equals(mem, defn, "restore")) {
        return sendv(mem, k, restore(mem, vector_ref(mem, args, 0)));
    }

    if (symbol_equals(mem, defn, "getpid")) {
        return sendv_va(mem, k, make_double(mem, getpid()));
    }

    if (symbol_equals(mem, defn, "cf-test")) {
        return sendv_va(mem, k, cf_test(mem, args));
    }

    if (symbol_equals(mem, defn, "cf-test-aux")) {
        return sendv_va(mem, k, cf_test_aux(mem, vlist_ref(mem, form_args, 1), args));
    }

    if (symbol_equals(mem, defn, "set-gc-callback!")) {
        mem->gc_callback = vlist_ref(mem, args, 0);
        mem->gc_callback_set = true;
        return sendv(mem, k, mem->nil);
    }

    if (symbol_equals(mem, defn, "output-binary-to-stdout")) {
        return sendv(mem, k, output_binary(mem, args, stdout));
    }

    if (symbol_equals(mem, defn, "output-binary-to-stderr")) {
        return sendv(mem, k, output_binary(mem, args, stderr));
    }

    assert_perror(EINVAL);
    return (allocation) { false, 0 };
}

allocation if0(memory *mem,
               uint64_t form_args,
               uint64_t args) {
    uint64_t scan0 = vlist_ref(mem, form_args, 0);
    uint64_t scan1 = vlist_ref(mem, form_args, 1);
    uint64_t scan2 = vlist_ref(mem, form_args, 2);
    uint64_t k = vlist_ref(mem, args, 0);
    uint64_t env = vlist_ref(mem, args, 1);
    return send_va(mem, scan0,
        vc_aa(mem, make_form_a(mem, if1_form,
                               vc_va(mem, k,
                                     vc_va(mem, env, vc_va(mem, scan1, vc(mem, scan2, mem->nil))))),
              vc(mem, env, mem->nil)));
}

allocation if1(memory *mem,
               uint64_t form_args,
               uint64_t args) {
    uint64_t k = vlist_ref(mem, form_args, 0);
    uint64_t env = vlist_ref(mem, form_args, 1);
    uint64_t scan1 = vlist_ref(mem, form_args, 2);
    uint64_t scan2 = vlist_ref(mem, form_args, 3);
    bool v = boolean_val(mem, vlist_ref(mem, args, 0));
    return send_va(mem, v ? scan1 : scan2, vc_va(mem, k, vc(mem, env, mem->nil)));
}

allocation sclis0(memory *mem,
                  uint64_t form_args,
                  uint64_t args) {
    uint64_t first = vlist_ref(mem, form_args, 0);
    uint64_t rest = vlist_ref(mem, form_args, 1);
    uint64_t k = vlist_ref(mem, args, 0);
    uint64_t env = vlist_ref(mem, args, 1);
    return send_va(mem, first,
        vc_aa(mem, make_form_a(mem, sclis1_form,
                               vc_va(mem, k, vc_va(mem, env, vc(mem, rest, mem->nil)))),
              vc(mem, env, mem->nil)));
}

allocation sclis1(memory *mem,
                  uint64_t form_args,
                  uint64_t args) {
    uint64_t k = vlist_ref(mem, form_args, 0);
    uint64_t env = vlist_ref(mem, form_args, 1);
    uint64_t rest = vlist_ref(mem, form_args, 2);
    uint64_t v = vlist_ref(mem, args, 0);
    return send_va(mem, rest,
        vc_aa(mem, make_form_a(mem, sclis2_form, vc_va(mem, k, vc(mem, v, mem->nil))),
              vc(mem, env, mem->nil)));
}

allocation sclis2(memory *mem,
                uint64_t form_args,
                uint64_t args) {
    uint64_t k = vlist_ref(mem, form_args, 0);
    uint64_t v = vlist_ref(mem, form_args, 1);
    uint64_t w = vlist_ref(mem, args, 0);
    return sendv_va(mem, k, vc(mem, v, w));
}

allocation scseq0(memory *mem,
                  uint64_t form_args,
                  uint64_t args) {
    uint64_t first = vlist_ref(mem, form_args, 0);
    uint64_t rest = vlist_ref(mem, form_args, 1);
    uint64_t k = vlist_ref(mem, args, 0);
    uint64_t env = vlist_ref(mem, args, 1);
    return send_va(mem, first,
        vc_aa(mem, make_form_a(mem, scseq1_form,
                               vc_va(mem, k, vc_va(mem, env, vc(mem, rest, mem->nil)))),
              vc(mem, env, mem->nil)));
}

allocation scseq1(memory *mem,
                  uint64_t form_args) {
    uint64_t k = vlist_ref(mem, form_args, 0);
    uint64_t env = vlist_ref(mem, form_args, 1);
    uint64_t rest = vlist_ref(mem, form_args, 2);
    return send_va(mem, rest, vc_va(mem, k, vc(mem, env, mem->nil)));
}

allocation lambda0(memory *mem,
                   uint64_t form_args,
                   uint64_t args) {
    uint64_t len = vlist_ref(mem, form_args, 0);
    uint64_t scanned = vlist_ref(mem, form_args, 1);
    uint64_t k = vlist_ref(mem, args, 0);
    uint64_t env = vlist_ref(mem, args, 1);
    return sendv_va(mem, k,
                    make_form_a(mem, lambda1_form,
                                vc_va(mem, len, vc_va(mem, scanned, vc(mem, env, mem->nil)))));
}

allocation lambda1(memory *mem,
                   uint64_t form_args,
                   uint64_t args) {
    uint64_t len = (uint64_t)double_val(mem, vlist_ref(mem, form_args, 0));
    uint64_t scanned = vlist_ref(mem, form_args, 1);
    uint64_t env = vlist_ref(mem, form_args, 2);

    return send_va(mem, scanned,
        vc_va(mem, step_contn_k(mem, args),
              vc_av(mem, extend_rtenv(mem, env, len, step_contn_args(mem, args)),
                    mem->nil)));
}

allocation improper_lambda0(memory *mem,
                            uint64_t form_args,
                            uint64_t args) {
    uint64_t len = vlist_ref(mem, form_args, 0);
    uint64_t scanned = vlist_ref(mem, form_args, 1);
    uint64_t k = vlist_ref(mem, args, 0);
    uint64_t env = vlist_ref(mem, args, 1);
    return sendv_va(mem, k,
                    make_form_a(mem, improper_lambda1_form,
                                vc_va(mem, len, vc_va(mem, scanned, vc(mem, env, mem->nil)))));
}

allocation improper_lambda1(memory *mem,
                            uint64_t form_args,
                            uint64_t args) {
    uint64_t len = (uint64_t)double_val(mem, vlist_ref(mem, form_args, 0));
    uint64_t scanned = vlist_ref(mem, form_args, 1);
    uint64_t env = vlist_ref(mem, form_args, 2);

    return send_va(mem, scanned,
        vc_va(mem, step_contn_k(mem, args),
              vc_av(mem, improper_extend_rtenv(mem, env, len, step_contn_args(mem, args)),
                    mem->nil)));
}

allocation letcc0(memory *mem,
                  uint64_t form_args,
                  uint64_t args) {
    uint64_t scanned = vlist_ref(mem, form_args, 0); 
    uint64_t k = vlist_ref(mem, args, 0);
    uint64_t env = vlist_ref(mem, args, 1);
    return send_va(mem, scanned,
        vc_va(mem, k,
              vc_av(mem, extend_rtenv_a(mem,
                                        env,
                                        1,
                                        vc_av(mem, make_form_a(mem, letcc1_form, vc(mem, k, mem->nil)),
                                              mem->nil)),
              mem->nil)));
}

allocation letcc1(memory *mem,
                  uint64_t form_args,
                  uint64_t args) {
    uint64_t k = vlist_ref(mem, form_args, 0);

    assert(args < mem->size);
    if (mem->bytes[args] == transfer_code) {
        return send(mem, k, transfer_args(mem, args));
    }

    return send(mem, k, step_contn_args(mem, args));
}

allocation define0(memory *mem,
                   uint64_t form_args,
                   uint64_t args) {
    uint64_t i = vlist_ref(mem, form_args, 0);
    uint64_t scanned = vlist_ref(mem, form_args, 1);
    uint64_t k = vlist_ref(mem, args, 0);
    uint64_t env = vlist_ref(mem, args, 1);
    return send_va(mem, scanned,
        vc_aa(mem, make_form_a(mem, define1_form,
                               vc_va(mem, k, vc_va(mem, env, vc(mem, i, mem->nil)))),
              vc(mem, env, mem->nil)));
}

allocation define1(memory *mem,
                   uint64_t form_args,
                   uint64_t args) {
    uint64_t k = vlist_ref(mem, form_args, 0);
    uint64_t env = vlist_ref(mem, form_args, 1);
    uint64_t i = vlist_ref(mem, form_args, 2);
    uint64_t v = vlist_ref(mem, args, 0);
    return sendv_va(mem, k, rtenv_setvar(mem, i, v, env));
}

allocation application0(memory *mem,
                        uint64_t form_args,
                        uint64_t args) {
    uint64_t scanned = vlist_ref(mem, form_args, 0);
    uint64_t k = vlist_ref(mem, args, 0);
    uint64_t env = vlist_ref(mem, args,  1);
    return send_va(mem, scanned,
        vc_aa(mem, make_form_a(mem, application1_form,
                               vc_va(mem, k, vc(mem, env, mem->nil))),
              vc(mem, env, mem->nil)));
}

allocation application1(memory *mem,
                        uint64_t form_args,
                        uint64_t args) {
    uint64_t k = vlist_ref(mem, form_args, 0);
    uint64_t env = vlist_ref(mem, form_args, 1);
    uint64_t v = vlist_ref(mem, args, 0);
    return applyx(mem, k, env, vector_ref(mem, v, 0), vector_ref(mem, v, 1));
}

allocation evalx_initial(memory *mem,
                         uint64_t form_args) {
    uint64_t k = vlist_ref(mem, form_args, 0);
    uint64_t env = vlist_ref(mem, form_args, 1);
    uint64_t scanned = vlist_ref(mem, form_args, 2);
    return send_va(mem, scanned, vc_va(mem, k, vc(mem, env, mem->nil)));
}

allocation handle_form(memory *mem,
                       double form_n,
                       uint64_t form_args,
                       uint64_t args) {
    switch ((int)form_n) {
        case symbol_lookup_form:
            return symbol_lookup(mem, form_args, args);

        case send_value_form:
            return send_value(mem, form_args, args);

        case constructed_function0_form:
            return constructed_function0(mem, form_args, args);

        case constructed_function1_form:
            return constructed_function1(mem, form_args, args);

        case global_lambda_form:
            return global_lambda(mem, form_args, args);

        case if0_form:
            return if0(mem, form_args, args);

        case if1_form:
            return if1(mem, form_args, args);

        case sclis0_form:
            return sclis0(mem, form_args, args);

        case sclis1_form:
            return sclis1(mem, form_args, args);

        case sclis2_form:
            return sclis2(mem, form_args, args);

        case scseq0_form:
            return scseq0(mem, form_args, args);

        case scseq1_form:
            return scseq1(mem, form_args);

        case lambda0_form:
            return lambda0(mem, form_args, args);

        case lambda1_form:
            return lambda1(mem, form_args, args);

        case improper_lambda0_form:
            return improper_lambda0(mem, form_args, args);

        case improper_lambda1_form:
            return improper_lambda1(mem, form_args, args);

        case letcc0_form:
            return letcc0(mem, form_args, args);

        case letcc1_form:
            return letcc1(mem, form_args, args);

        case define0_form:
            return define0(mem, form_args, args);

        case define1_form:
            return define1(mem, form_args, args);

        case application0_form:
            return application0(mem, form_args, args);

        case application1_form:
            return application1(mem, form_args, args);

        case evalx_initial_form:
            return evalx_initial(mem, form_args);

        default:
            assert_perror(EINVAL);
    }

    return (allocation) { false, 0 };
}

allocation handle_unmemoized(memory *mem, uint64_t state, uint64_t args) {
    assert(state + 8 < mem->size);
    assert(mem->bytes[state] == unmemoized_code);
    uint64_t exp = vector_ref(mem, *(uint64_t*)&mem->bytes[state + 1], 0);
    return handle_form(mem, double_val(mem, vector_ref(mem, exp, 0)), vector_ref(mem, exp, 1), args);
}

uint64_t run(memory *mem, uint64_t state);

allocation call_gc(memory *mem, uint64_t state) {
    memory dest = {
        mem->capacity,
        0,
        (uint8_t*) malloc(mem->capacity),
        0,
        false,
        0,
        false
    };
    assert(dest.bytes);
    state = gc(mem, state, &dest);
    if ((dest.nil >= dest.size) ||
        (dest.bytes[dest.nil] != vector_code) ||
        (vector_size(&dest, dest.nil) != 0)) {
        allocation nil_state = make_uninitialised_vector(&dest, 0);
        if (!nil_state.succeeded) {
            return nil_state;
        }
        dest.nil = nil_state.v;
    }
    free(mem->bytes);
    *mem = dest;
    if (mem->gc_callback_set) {
        allocation cb_state = applyx_a(mem,
           make_form_a(mem, global_lambda_form,
                       vc_av(mem, make_double(mem, g_result), mem->nil)),
           make_global_rtenv(mem),
           mem->gc_callback,
           vc_av(mem, make_double(mem, mem->size), mem->nil));
        if (!cb_state.succeeded) {
            return cb_state;
        }
        mem->calling_gc_callback = true;
        uint64_t r = run(mem, cb_state.v);
        mem->calling_gc_callback = false;
        assert(r < mem->size);
        assert(mem->bytes[r] != false_code);
    }
    return (allocation) { true, state };
}

allocation step(memory *mem, uint64_t state) {
    return handle_unmemoized(mem, vector_ref(mem, state, 0), vector_ref(mem, state, 1));
}

uint64_t run(memory *mem, uint64_t state) {
    while (assert(state < mem->size),
           mem->bytes[state] != result_code) {
        allocation new_state = step(mem, state);
        if (!new_state.succeeded && !mem->calling_gc_callback) {
            new_state = call_gc(mem, state);
            if (new_state.succeeded) {
                new_state = step(mem, new_state.v);
            }
        }
        assert(new_state.succeeded);
        state = new_state.v;
    }
    assert(state + 8 < mem->size);
    return vector_ref(mem, *(uint64_t*)&mem->bytes[state + 1], 0);
}

uint64_t start(memory *mem, uint64_t state) {
    assert(state < mem->size);
    if (mem->bytes[state] == unmemoized_code) {
        allocation form_state = applyx_a(mem,
            make_form_a(mem, global_lambda_form,
                        vc_av(mem, make_double(mem, g_result), mem->nil)),
            make_global_rtenv(mem),
            state,
            (allocation) { true, mem->nil });
        assert(form_state.succeeded);
        return run(mem, form_state.v);
    }
    return run(mem, state);
}

const char *argp_program_version = "mce 1.0";
const char *argp_program_bug_address = "dave@davedoesdev.com";
const char argp_doc[] = "mce -- Metacircular Evaluator";
struct argp_option options[] = {
    {"memory-capacity", 'm', "MEBIBYTES", 0, "Maximum memory size", 0},
    {0}
};
struct arguments {
    uint64_t memory_capacity;
};
error_t parse_opt(int key, char *arg, struct argp_state *state) {
    struct arguments *arguments = state->input;
    switch (key) {
        case 'm':
            arguments->memory_capacity = strtoull(arg, NULL, 0) * MEBIBYTES;
            break;
        default:
            return ARGP_ERR_UNKNOWN;
    }
    return 0;
}
struct argp argp = { options, parse_opt, NULL, argp_doc, NULL, NULL, NULL };

int main(int argc, char **argv) {
    /* Get memory capacity */
    struct arguments arguments = {
        DEFAULT_MEMORY_SIZE * MEBIBYTES,
    };
    argp_parse(&argp, argc, argv, 0, 0, &arguments);

    /* Read size of state */
    uint64_t size;
    assert(fread(&size, 1, sizeof(size), stdin) == sizeof(size));
    assert(size <= arguments.memory_capacity);

    /* Allocate all memory */
    uint8_t *bytes = (uint8_t*) malloc(arguments.memory_capacity);
    assert(bytes);

    /* Read state */
    assert(fread(&bytes[0], 1, size, stdin) == size);

    /* Make state */
    memory mem = {
        arguments.memory_capacity,
        size,
        bytes,
        0,
        false,
        0,
        false
    };

    /* Make an index for nil */
    allocation nil_state = make_uninitialised_vector(&mem, 0);
    assert(nil_state.succeeded);
    mem.nil = nil_state.v;

    /* Run state */
    start(&mem, 0);

    return 0;
}
