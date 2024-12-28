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
#define DEFAULT_GC_THRESHOLD_SIZE   8
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
    uint64_t gc_threshold;
    uint8_t *bytes;
    uint64_t nil;
} memory;

uint64_t allocate(memory *mem, uint64_t size) {
    uint64_t cur_size = mem->size;
    assert(cur_size + size <= mem->capacity);
    mem->size += size;
    return cur_size;
}

bool gc_callback_set = false;
uint64_t gc_callback;
bool calling_gc_callback = false;

/* Note: src is modified during gc() and gc_aux() */
uint64_t gc_aux(memory *src, uint64_t state, memory *dest) {
    uint64_t size = dest->size;

    assert(state < src->size);
    uint8_t code = src->bytes[state];

    assert(dest->size < dest->capacity);
    dest->bytes[dest->size++] = code;

    switch (code) {
        case true_code:
        case false_code:
            break;

        case number_code:
            assert(state + sizeof(double) < src->size);
            assert(dest->size + sizeof(double) <= dest->capacity);
            memcpy(&dest->bytes[dest->size], &src->bytes[state + 1], sizeof(double));
            dest->size += sizeof(double);
            break;

        case binary_code: {
            assert(state + 8 < src->size);
            uint64_t len = *(uint64_t*)&src->bytes[state + 1];

            assert(dest->size + 8 <= dest->capacity);
            *(uint64_t*)&dest->bytes[dest->size] = len;
            dest->size += 8;

            assert(state + 8 + len < src->size);
            assert(dest->size + len <= dest->capacity);
            memcpy(&dest->bytes[dest->size], &src->bytes[state + 9], len);
            dest->size += len;
            break;
        }

        case vector_code: {
            assert(state + 8 < src->size);
            uint64_t len = *(uint64_t*)&src->bytes[state + 1];

            if (len == 0) {
                dest->nil = dest->size - 1;
            }

            src->bytes[state] = gc_code;
            *(uint64_t*)&src->bytes[state + 1] = dest->size - 1;

            assert(dest->size + 8 <= dest->capacity);
            *(uint64_t*)&dest->bytes[dest->size] = len;
            dest->size += 8;

            assert(state + 8 + len * 8 < src->size);
            assert(dest->size + len * 8 <= dest->capacity);

            uint64_t pos = dest->size;
            dest->size += len * 8;

            for (uint64_t i = 0; i < len; ++i) {
                *(uint64_t*)&dest->bytes[pos + i * 8] = gc_aux(
                    src, *(uint64_t*)&src->bytes[state + 9 + i * 8], dest);
            }
            break;
        }

        case unmemoized_code:
        case result_code:
        case step_contn_code:
        case transfer_code: {
            assert(state + 8 < src->size);
            uint64_t vec_state = *(uint64_t*)&src->bytes[state + 1];

            src->bytes[state] = gc_code;
            *(uint64_t*)&src->bytes[state + 1] = dest->size - 1;

            assert(dest->size + 8 <= dest->capacity);
            uint64_t pos = dest->size;
            dest->size += 8;

            *(uint64_t*)&dest->bytes[pos] = gc_aux(src, vec_state, dest);
            break;
        }

        case gc_code:
            assert(state + 8 < src->size);
            return *(uint64_t*)&src->bytes[state + 1];

        default:
            assert_perror(EINVAL);
    }

    return size;
}

void gc(memory *src, uint64_t state, memory *dest) {
    gc_aux(src, state, dest);
    assert(dest->size > 0);
}

uint64_t make_uninitialised_vector(memory *mem, uint64_t n) {
    uint64_t state = allocate(mem, 1 + 8 + n * 8);
    mem->bytes[state] = vector_code;
    *(uint64_t*)&mem->bytes[state + 1] = n;
    return state;
}

uint64_t make_vector(memory *mem, uint64_t n) {
    uint64_t state = make_uninitialised_vector(mem, n);
    for (uint64_t i = 0; i < n; ++i) {
        *(uint64_t*)&mem->bytes[state + 9 + i * 8] = mem->nil;
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

uint64_t vc(memory *mem, uint64_t first, uint64_t second) {
    uint64_t v = make_uninitialised_vector(mem, 2);
    vector_set(mem, v, 0, first);
    vector_set(mem, v, 1, second);
    return v;
}

#define send vc

uint64_t sendv(memory *mem, uint64_t k, uint64_t v) {
    return send(mem, k, vc(mem, v, mem->nil));
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

uint64_t make_double(memory *mem, double v) {
    uint64_t state = allocate(mem, 1 + sizeof(double));
    mem->bytes[state] = number_code;
    *(double*)&mem->bytes[state + 1] = v;
    return state;
}

bool boolean_val(memory *mem, uint64_t state) {
    assert(state < mem->size);
    uint8_t code = mem->bytes[state];
    assert(code == true_code || code == false_code);
    return code == true_code;
}

uint64_t make_boolean(memory *mem, bool v) {
    uint64_t state = allocate(mem, 1);
    mem->bytes[state] = v ? true_code : false_code;
    return state;
}

uint64_t make_binary(memory *mem, uint64_t len) {
    uint64_t state = allocate(mem, 1 + 8 + len);
    mem->bytes[state] = binary_code;
    *(uint64_t*)&mem->bytes[state + 1] = len;
    return state;
}

uint64_t binary_size(memory *mem, uint64_t b) {
    assert(b + 8 < mem->size);
    assert(mem->bytes[b] == binary_code);
    uint64_t size = *(uint64_t*)&mem->bytes[b + 1];
    assert(b + 8 + size < mem->size);
    return size;
}

uint64_t binary_ref(memory *mem, uint64_t b, uint64_t i) {
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

uint64_t make_result(memory *mem, uint64_t val) {
    uint64_t state = allocate(mem, 1 + 8 + 1 + 8 + 8);
    mem->bytes[state] = result_code;
    *(uint64_t*)&mem->bytes[state + 1] = state + 9;
    mem->bytes[state + 9] = vector_code;
    *(uint64_t*)&mem->bytes[state + 10] = 1;
    *(uint64_t*)&mem->bytes[state + 18] = val;
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

uint64_t rtenv_setvar(memory *mem,
                      uint64_t i,
                      uint64_t val,
                      uint64_t env) {
    uint64_t first = (uint64_t)double_val(mem, vector_ref(mem, i, 0));
    uint64_t second = (uint64_t)double_val(mem, vector_ref(mem, i, 1));
    uint64_t v = vlist_ref(mem, env, first);
    uint64_t size = vector_size(mem, v);
    if (second >= size) {
        uint64_t v2 = make_uninitialised_vector(mem, second + 1);
        for (uint64_t i = 0; i < second; ++i) {
            vector_set(mem, v2, i, i < size ? vector_ref(mem, v, i) : mem->nil);
        }
        vlist_set(mem, env, first, v2);
        v = v2;
    }
    vector_set(mem, v, second, val);
    return val;
}

uint64_t save(memory *mem, uint64_t state) {
    memory copy = *mem;
    copy.bytes = (uint8_t*) malloc(mem->size);
    assert(copy.bytes);
    memcpy(copy.bytes, mem->bytes, mem->size);

    memory dest = {
        mem->size,
        0,
        0,
        (uint8_t*) malloc(mem->size),
        0
    };
    assert(dest.bytes);

    gc(&copy, state, &dest);
    free(copy.bytes);

    uint64_t b = make_binary(mem, 8 + dest.size);
    memcpy(&mem->bytes[b + 9], &dest.size, 8);
    memcpy(&mem->bytes[b + 9 + 8], dest.bytes, dest.size);
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
        0,
        bytes,
        0
    };

    size = mem->size;
    gc(&src, 0, mem);

    return size;
}

uint64_t vlist_to_vector(memory *mem, uint64_t vl, uint64_t len) {
    uint64_t v = make_uninitialised_vector(mem, len);
    uint64_t i = 0;

    while (assert(vl < mem->size), (i < len)) {
        bool is_vector = mem->bytes[vl] == vector_code;
        uint64_t size = is_vector ? vector_size(mem, vl) : 0;

        if (is_vector && (size == 2)) {
            vector_set(mem, v, i++, vector_ref(mem, vl, 0));
            vl = vector_ref(mem, vl, 1);
        } else {
            if (!(is_vector && (size == 0))) {
                vector_set(mem, v, i++, vl);
            }
            break;
        }
    }

    while (i < len) {
        vector_set(mem, v, i++, mem->nil);
    }

    return v;
}

uint64_t extend_rtenv(memory *mem,
                      uint64_t env,
                      uint64_t len,
                      uint64_t values) {
    return vc(mem, vlist_to_vector(mem, values, len), env);
}

uint64_t improper_extend_rtenv(memory *mem,
                               uint64_t env,
                               uint64_t len,
                               uint64_t values) {
    uint64_t v = make_uninitialised_vector(mem, len);
    uint64_t i = 0;

    while (assert(values < mem->size), (i < len)) {
        bool is_vector = mem->bytes[values] == vector_code;
        uint64_t size = is_vector ? vector_size(mem, values) : 0;

        if ((i == (len - 1)) || !is_vector || (size != 2)) {
            if (!is_vector || (size != 0)) {
                vector_set(mem, v, i++, values);
            }
            break;
        }

        vector_set(mem, v, i++, vector_ref(mem, values, 0));
        values = vector_ref(mem, values, 1);
    }

    while (i < len) {
        vector_set(mem, v, i++, mem->nil);
    }

    return vc(mem, v, env);
}

uint64_t make_step_contn(memory *mem, uint64_t k, uint64_t env, uint64_t args) {
    uint64_t state = allocate(mem, 1 + 8 + 1 + 8 + 8 + 8 + 8);
    mem->bytes[state] = step_contn_code;
    *(uint64_t*)&mem->bytes[state + 1] = state + 9;
    mem->bytes[state + 9] = vector_code;
    *(uint64_t*)&mem->bytes[state + 10] = 3;
    *(uint64_t*)&mem->bytes[state + 18] = k;
    *(uint64_t*)&mem->bytes[state + 26] = env;
    *(uint64_t*)&mem->bytes[state + 34] = args;
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

uint64_t transfer(memory *mem, uint64_t args) {
    uint64_t state = allocate(mem, 1 + 8 + 1 + 8 + 8);
    mem->bytes[state] = transfer_code;
    *(uint64_t*)&mem->bytes[state + 1] = state + 9;
    mem->bytes[state + 9] = vector_code;
    *(uint64_t*)&mem->bytes[state + 10] = 1;
    *(uint64_t*)&mem->bytes[state + 18] = vector_ref(mem, args, 1);
    return send(mem, vector_ref(mem, args, 0), state);
}

uint64_t transfer_args(memory *mem, uint64_t state) {
    assert(state + 8 < mem->size);
    assert(mem->bytes[state] == transfer_code);
    return vector_ref(mem, *(uint64_t*)&mem->bytes[state + 1], 0);
}

uint64_t less_than(memory *mem, uint64_t args) {
    return make_boolean(mem, 
        double_val(mem, vlist_ref(mem, args, 0)) <
        double_val(mem, vlist_ref(mem, args, 1)));
}

uint64_t greater_than(memory *mem, uint64_t args) {
    return make_boolean(mem,
        double_val(mem, vlist_ref(mem, args, 0)) >
        double_val(mem, vlist_ref(mem, args, 1)));
}

uint64_t add(memory *mem, uint64_t args) {
    return make_double(mem, 
        double_val(mem, vlist_ref(mem, args, 0)) +
        double_val(mem, vlist_ref(mem, args, 1)));
}

uint64_t subtract(memory *mem, uint64_t args) {
    return make_double(mem, 
        double_val(mem, vlist_ref(mem, args, 0)) -
        double_val(mem, vlist_ref(mem, args, 1)));
}

uint64_t multiply(memory *mem, uint64_t args) {
    return make_double(mem, 
        double_val(mem, vlist_ref(mem, args, 0)) *
        double_val(mem, vlist_ref(mem, args, 1)));
}

uint64_t divide(memory *mem, uint64_t args) {
    return make_double(mem, 
        double_val(mem, vlist_ref(mem, args, 0)) /
        double_val(mem, vlist_ref(mem, args, 1)));
}

uint64_t is_same_object(memory *mem, uint64_t args) {
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

uint64_t is_vector(memory *mem, uint64_t args) {
    uint64_t arg = vector_ref(mem, args, 0);
    assert(arg < mem->size);
    return make_boolean(mem, mem->bytes[arg] == vector_code);
}

uint64_t is_binary(memory *mem, uint64_t args) {
    uint64_t arg = vector_ref(mem, args, 0);
    assert(arg < mem->size);
    return make_boolean(mem, mem->bytes[arg] == binary_code);
}

uint64_t is_procedure(memory *mem, uint64_t args) {
    uint64_t arg = vector_ref(mem, args, 0);
    assert(arg < mem->size);
    return make_boolean(mem, mem->bytes[arg] == unmemoized_code);
}

uint64_t is_boolean(memory *mem, uint64_t args) {
    uint64_t arg = vector_ref(mem, args, 0);
    assert(arg < mem->size);
    uint8_t code = mem->bytes[arg];
    return make_boolean(mem, code == true_code || code == false_code);
}

uint64_t is_number(memory *mem, uint64_t args) {
    uint64_t arg = vector_ref(mem, args, 0);
    assert(arg < mem->size);
    return make_boolean(mem, mem->bytes[arg] == number_code);
}

uint64_t is_number_equal(memory *mem, uint64_t args) {
    return make_boolean(mem,
        double_val(mem, vlist_ref(mem, args, 0)) ==
        double_val(mem, vlist_ref(mem, args, 1)));
}

uint64_t symbol_lookup(memory *mem,
                       uint64_t form_args,
                       uint64_t args) {
    uint64_t i = vlist_ref(mem, form_args, 0);
    uint64_t k = vlist_ref(mem, args, 0);
    uint64_t env = vlist_ref(mem, args, 1);
    return sendv(mem, k, rtenv_lookup(mem, i, env));
}

uint64_t send_value(memory *mem,
                    uint64_t form_args,
                    uint64_t args) {
    uint64_t exp = vlist_ref(mem, form_args, 0);
    uint64_t k = vlist_ref(mem, args, 0);
    return sendv(mem, k, exp);
}

uint64_t make_form(memory *mem, double form, uint64_t args) {
    uint64_t state = allocate(mem, 1 + 8 + 1 + 8 + 8);
    mem->bytes[state] = unmemoized_code;
    *(uint64_t*)&mem->bytes[state + 1] = state + 9;
    mem->bytes[state + 9] = vector_code;
    *(uint64_t*)&mem->bytes[state + 10] = 1;
    *(uint64_t*)&mem->bytes[state + 18] = vc(mem, make_double(mem, form), args);
    return state;
}

uint64_t make_global_rtenv(memory *mem) {
    return vc(mem, make_vector(mem, 0), mem->nil);
}

uint64_t applyx(memory *mem,
                uint64_t k,
                uint64_t env,
                uint64_t fn,
                uint64_t args) {
    return send(mem, fn, make_step_contn(mem, k, env, args));
}

uint64_t transfer_test(memory *mem, uint64_t args) {
    return applyx(mem,
                  make_form(mem, global_lambda_form,
                            vc(mem, make_double(mem, g_result), mem->nil)),
                  make_global_rtenv(mem),
                  vector_ref(mem, args, 0),
                  vector_ref(mem, args, 1));
}

/* Hack for cf-test-aux */
uint64_t make_symbol(memory *mem, char *s) {
    size_t len = strlen(s); 
    uint64_t b = make_binary(mem, len + 1);
    mem->bytes[b + 9] = 'D';
    memcpy(&mem->bytes[b + 10], s, len);
    return b;
}

uint64_t cf_test(memory *mem, uint64_t args) {
    double n = double_val(mem, vlist_ref(mem, args, 0));
    uint64_t x = vlist_ref(mem, args, 1);
    if (n == 0) {
        /* Note since we lack the duality of the other implementations
           (where we can return a function which has logic and can also return
           its own definition when requested), we have to split the logic into
           a separate helper global, cf-test-aux - which is not interoperable
           with the other implementations. */
        return make_form(mem, global_lambda_form,
                         vc(mem, make_symbol(mem, "cf-test-aux"),
                            vc(mem, x, mem->nil)));
    }
    return make_double(mem, double_val(mem, x) + n);
}

uint64_t cf_test_aux(memory *mem, uint64_t x, uint64_t args) {
    return cf_test(mem, vc(mem, vlist_ref(mem, args, 0), vc(mem, x, mem->nil)));
}

uint64_t constructed_function0(memory *mem,
                               uint64_t form_args,
                               uint64_t args) {
    uint64_t args2 = vlist_ref(mem, form_args, 0);
    uint64_t cf = vlist_ref(mem, form_args, 1);
    return applyx(mem, make_form(mem, constructed_function1_form, vc(mem, args, mem->nil)),
                  make_global_rtenv(mem), cf, args2);
}

uint64_t constructed_function1(memory *mem,
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

uint64_t global_lambda(memory *mem,
                       uint64_t form_args,
                       uint64_t args) {
    uint64_t defn = vlist_ref(mem, form_args, 0);
    assert(defn < mem->size);
    double g = mem->bytes[defn] == number_code ? double_val(mem, defn) : -1;

    assert(args < mem->size);
    if (mem->bytes[args] == transfer_code) {
        args = transfer_args(mem, args);

        if (g == g_transfer_test) {
            return applyx(mem,
                          make_form(mem, global_lambda_form,
                                    vc(mem, make_double(mem, g_result), mem->nil)),
                          make_global_rtenv(mem),
                          vlist_ref(mem, args, 0),
                          vlist_rest(mem, args, 0));
        }

        /*xdisplay(mem, defn, stderr, false); fprintf(stderr, " ");*/
        assert_perror(EINVAL);
        return 0;
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
            return sendv(mem, k, is_boolean(mem, args));

        case g_is_number:
            return sendv(mem, k, is_number(mem, args));

        case g_less_than:
            return sendv(mem, k, less_than(mem, args));

        case g_greater_than:
            return sendv(mem, k, greater_than(mem, args));

        case g_add:
            return sendv(mem, k, add(mem, args));

        case g_subtract:
            return sendv(mem, k, subtract(mem, args));

        case g_multiply:
            return sendv(mem, k, multiply(mem, args));

        case g_divide:
            return sendv(mem, k, divide(mem, args));

        case g_is_number_equal:
            return sendv(mem, k, is_number_equal(mem, args));

        case g_floor:
            return sendv(mem, k, make_double(mem, floor(double_val(mem, vector_ref(mem, args, 0)))));

        case g_make_vector:
            return sendv(mem, k, make_vector(mem, double_val(mem, vector_ref(mem, args, 0))));

        case g_is_vector:
            return sendv(mem, k, is_vector(mem, args));

        case g_vector_length:
            return sendv(mem, k, make_double(mem, vector_size(mem, vector_ref(mem, args, 0))));

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
            return sendv(mem, k, is_procedure(mem, args));

        case g_make_binary:
            return sendv(mem, k, make_binary(mem, double_val(mem, vector_ref(mem, args, 0))));

        case g_is_binary:
            return sendv(mem, k, is_binary(mem, args));

        case g_binary_length:
            return sendv(mem, k, make_double(mem, binary_size(mem, vector_ref(mem, args, 0))));

        case g_binary_ref:
            return sendv(mem, k, binary_ref(mem,
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
            return sendv(mem, k, is_same_object(mem, args));

        case g_transfer:
            return transfer(mem, args);
    }

    if (symbol_equals(mem, defn, "save")) {
        return sendv(mem, k, save(mem, vector_ref(mem, args, 0)));
    }

    if (symbol_equals(mem, defn, "restore")) {
        return sendv(mem, k, restore(mem, vector_ref(mem, args, 0)));
    }

    if (symbol_equals(mem, defn, "getpid")) {
        return sendv(mem, k, make_double(mem, getpid()));
    }

    if (symbol_equals(mem, defn, "cf-test")) {
        return sendv(mem, k, cf_test(mem, args));
    }

    if (symbol_equals(mem, defn, "cf-test-aux")) {
        return sendv(mem, k, cf_test_aux(mem, vlist_ref(mem, form_args, 1), args));
    }

    if (symbol_equals(mem, defn, "set-gc-callback!")) {
        gc_callback = vlist_ref(mem, args, 0);
        gc_callback_set = true;
        return sendv(mem, k, mem->nil);
    }

    if (symbol_equals(mem, defn, "output-binary-to-stdout")) {
        return sendv(mem, k, output_binary(mem, args, stdout));
    }

    if (symbol_equals(mem, defn, "output-binary-to-stderr")) {
        return sendv(mem, k, output_binary(mem, args, stderr));
    }

    /*xdisplay(mem, defn, stderr, false); fprintf(stderr, " ");*/
    assert_perror(EINVAL);
    return 0;
}

uint64_t if0(memory *mem,
             uint64_t form_args,
             uint64_t args) {
    uint64_t scan0 = vlist_ref(mem, form_args, 0);
    uint64_t scan1 = vlist_ref(mem, form_args, 1);
    uint64_t scan2 = vlist_ref(mem, form_args, 2);
    uint64_t k = vlist_ref(mem, args, 0);
    uint64_t env = vlist_ref(mem, args, 1);
    return send(mem, scan0,
        vc(mem, make_form(mem, if1_form,
                          vc(mem, k,
                             vc(mem, env, vc(mem, scan1, vc(mem, scan2, mem->nil))))),
           vc(mem, env, mem->nil)));
}

uint64_t if1(memory *mem,
             uint64_t form_args,
             uint64_t args) {
    uint64_t k = vlist_ref(mem, form_args, 0);
    uint64_t env = vlist_ref(mem, form_args, 1);
    uint64_t scan1 = vlist_ref(mem, form_args, 2);
    uint64_t scan2 = vlist_ref(mem, form_args, 3);
    bool v = boolean_val(mem, vlist_ref(mem, args, 0));
    return send(mem, v ? scan1 : scan2, vc(mem, k, vc(mem, env, mem->nil)));
}

uint64_t sclis0(memory *mem,
                uint64_t form_args,
                uint64_t args) {
    uint64_t first = vlist_ref(mem, form_args, 0);
    uint64_t rest = vlist_ref(mem, form_args, 1);
    uint64_t k = vlist_ref(mem, args, 0);
    uint64_t env = vlist_ref(mem, args, 1);
    return send(mem, first,
        vc(mem, make_form(mem, sclis1_form,
                          vc(mem, k, vc(mem, env, vc(mem, rest, mem->nil)))),
           vc(mem, env, mem->nil)));
}

uint64_t sclis1(memory *mem,
                uint64_t form_args,
                uint64_t args) {
    uint64_t k = vlist_ref(mem, form_args, 0);
    uint64_t env = vlist_ref(mem, form_args, 1);
    uint64_t rest = vlist_ref(mem, form_args, 2);
    uint64_t v = vlist_ref(mem, args, 0);
    return send(mem, rest,
        vc(mem, make_form(mem, sclis2_form, vc(mem, k, vc(mem, v, mem->nil))),
           vc(mem, env, mem->nil)));
}

uint64_t sclis2(memory *mem,
                uint64_t form_args,
                uint64_t args) {
    uint64_t k = vlist_ref(mem, form_args, 0);
    uint64_t v = vlist_ref(mem, form_args, 1);
    uint64_t w = vlist_ref(mem, args, 0);
    return sendv(mem, k, vc(mem, v, w));
}

uint64_t scseq0(memory *mem,
                uint64_t form_args,
                uint64_t args) {
    uint64_t first = vlist_ref(mem, form_args, 0);
    uint64_t rest = vlist_ref(mem, form_args, 1);
    uint64_t k = vlist_ref(mem, args, 0);
    uint64_t env = vlist_ref(mem, args, 1);
    return send(mem, first,
        vc(mem, make_form(mem, scseq1_form,
                          vc(mem, k, vc(mem, env, vc(mem, rest, mem->nil)))),
           vc(mem, env, mem->nil)));
}

uint64_t scseq1(memory *mem,
                uint64_t form_args) {
    uint64_t k = vlist_ref(mem, form_args, 0);
    uint64_t env = vlist_ref(mem, form_args, 1);
    uint64_t rest = vlist_ref(mem, form_args, 2);
    return send(mem, rest, vc(mem, k, vc(mem, env, mem->nil)));
}

uint64_t lambda0(memory *mem,
                 uint64_t form_args,
                 uint64_t args) {
    uint64_t len = vlist_ref(mem, form_args, 0);
    uint64_t scanned = vlist_ref(mem, form_args, 1);
    uint64_t k = vlist_ref(mem, args, 0);
    uint64_t env = vlist_ref(mem, args, 1);
    return sendv(mem, k,
                 make_form(mem, lambda1_form,
                           vc(mem, len, vc(mem, scanned, vc(mem, env, mem->nil)))));
}

uint64_t lambda1(memory *mem,
                 uint64_t form_args,
                 uint64_t args) {
    uint64_t len = (uint64_t)double_val(mem, vlist_ref(mem, form_args, 0));
    uint64_t scanned = vlist_ref(mem, form_args, 1);
    uint64_t env = vlist_ref(mem, form_args, 2);

    return send(mem, scanned,
        vc(mem, step_contn_k(mem, args),
           vc(mem, extend_rtenv(mem, env, len, step_contn_args(mem, args)),
              mem->nil)));
}

uint64_t improper_lambda0(memory *mem,
                          uint64_t form_args,
                          uint64_t args) {
    uint64_t len = vlist_ref(mem, form_args, 0);
    uint64_t scanned = vlist_ref(mem, form_args, 1);
    uint64_t k = vlist_ref(mem, args, 0);
    uint64_t env = vlist_ref(mem, args, 1);
    return sendv(mem, k,
                 make_form(mem, improper_lambda1_form,
                           vc(mem, len, vc(mem, scanned, vc(mem, env, mem->nil)))));
}

uint64_t improper_lambda1(memory *mem,
                          uint64_t form_args,
                          uint64_t args) {
    uint64_t len = (uint64_t)double_val(mem, vlist_ref(mem, form_args, 0));
    uint64_t scanned = vlist_ref(mem, form_args, 1);
    uint64_t env = vlist_ref(mem, form_args, 2);

    return send(mem, scanned,
        vc(mem, step_contn_k(mem, args),
           vc(mem, improper_extend_rtenv(mem, env, len, step_contn_args(mem, args)),
              mem->nil)));
}

uint64_t letcc0(memory *mem,
                uint64_t form_args,
                uint64_t args) {
    uint64_t scanned = vlist_ref(mem, form_args, 0); 
    uint64_t k = vlist_ref(mem, args, 0);
    uint64_t env = vlist_ref(mem, args, 1);
    return send(mem, scanned,
        vc(mem, k,
           vc(mem, extend_rtenv(mem,
                                env,
                                1,
                                vc(mem, make_form(mem, letcc1_form, vc(mem, k, mem->nil)),
                                   mem->nil)),
              mem->nil)));
}

uint64_t letcc1(memory *mem,
                uint64_t form_args,
                uint64_t args) {
    uint64_t k = vlist_ref(mem, form_args, 0);

    assert(args < mem->size);
    if (mem->bytes[args] == transfer_code) {
        return send(mem, k, transfer_args(mem, args));
    }

    return send(mem, k, step_contn_args(mem, args));
}

uint64_t define0(memory *mem,
                 uint64_t form_args,
                 uint64_t args) {
    uint64_t i = vlist_ref(mem, form_args, 0);
    uint64_t scanned = vlist_ref(mem, form_args, 1);
    uint64_t k = vlist_ref(mem, args, 0);
    uint64_t env = vlist_ref(mem, args, 1);
    return send(mem, scanned,
        vc(mem, make_form(mem, define1_form,
                          vc(mem, k, vc(mem, env, vc(mem, i, mem->nil)))),
           vc(mem, env, mem->nil)));
}

uint64_t define1(memory *mem,
                 uint64_t form_args,
                 uint64_t args) {
    uint64_t k = vlist_ref(mem, form_args, 0);
    uint64_t env = vlist_ref(mem, form_args, 1);
    uint64_t i = vlist_ref(mem, form_args, 2);
    uint64_t v = vlist_ref(mem, args, 0);
    return sendv(mem, k, rtenv_setvar(mem, i, v, env));
}

uint64_t application0(memory *mem,
                      uint64_t form_args,
                      uint64_t args) {
    uint64_t scanned = vlist_ref(mem, form_args, 0);
    uint64_t k = vlist_ref(mem, args, 0);
    uint64_t env = vlist_ref(mem, args,  1);
    return send(mem, scanned,
        vc(mem, make_form(mem, application1_form,
                          vc(mem, k, vc(mem, env, mem->nil))),
           vc(mem, env, mem->nil)));
}

uint64_t application1(memory *mem,
                      uint64_t form_args,
                      uint64_t args) {
    uint64_t k = vlist_ref(mem, form_args, 0);
    uint64_t env = vlist_ref(mem, form_args, 1);
    uint64_t v = vlist_ref(mem, args, 0);
    return applyx(mem, k, env, vector_ref(mem, v, 0), vector_ref(mem, v, 1));
}

uint64_t evalx_initial(memory *mem,
                       uint64_t form_args) {
    uint64_t k = vlist_ref(mem, form_args, 0);
    uint64_t env = vlist_ref(mem, form_args, 1);
    uint64_t scanned = vlist_ref(mem, form_args, 2);
    return send(mem, scanned, vc(mem, k, vc(mem, env, mem->nil)));
}

uint64_t handle_form(memory *mem,
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

    return 0;
}

uint64_t handle_unmemoized(memory *mem, uint64_t state, uint64_t args) {
    assert(state + 8 < mem->size);
    assert(mem->bytes[state] == unmemoized_code);
    uint64_t exp = vector_ref(mem, *(uint64_t*)&mem->bytes[state + 1], 0);
    return handle_form(mem, double_val(mem, vector_ref(mem, exp, 0)), vector_ref(mem, exp, 1), args);
}

uint64_t run(memory *mem, uint64_t state);

uint64_t maybe_gc(memory *mem, uint64_t state) {
    if (mem->size > mem->gc_threshold && !calling_gc_callback) {
        if (gc_callback_set) {
            state = vc(mem, state, gc_callback);
        }
        memory dest = {
            mem->capacity,
            0,
            mem->gc_threshold,
            (uint8_t*) malloc(mem->capacity),
            0
        };
        assert(dest.bytes);
        gc(mem, state, &dest);
        if ((dest.nil >= dest.size) ||
            (dest.bytes[dest.nil] != vector_code) ||
            (vector_size(&dest, dest.nil) != 0)) {
            dest.nil = make_uninitialised_vector(&dest, 0);
        }
        free(mem->bytes);
        *mem = dest;
        if (gc_callback_set) {
            state = vector_ref(mem, 0, 0);
            gc_callback = vector_ref(mem, 0, 1);
            calling_gc_callback = true;
            uint64_t r = run(mem, applyx(mem,
               make_form(mem, global_lambda_form,
                         vc(mem, make_double(mem, g_result), mem->nil)),
               make_global_rtenv(mem),
               gc_callback,
               vc(mem, make_double(mem, mem->size), mem->nil)));
            calling_gc_callback = false;
            assert(r < mem->size);
            assert(mem->bytes[r] != false_code);
        } else {
            state = 0;
        }
    }
    return state;
}

uint64_t step(memory *mem, uint64_t state) {
    return handle_unmemoized(mem, vector_ref(mem, state, 0), vector_ref(mem, state, 1));
}

uint64_t run(memory *mem, uint64_t state) {
    while (assert(state < mem->size),
           mem->bytes[state] != result_code) {
        state = maybe_gc(mem, step(mem, state));
    }
    assert(state + 8 < mem->size);
    return vector_ref(mem, *(uint64_t*)&mem->bytes[state + 1], 0);
}

uint64_t start(memory *mem, uint64_t state) {
    assert(state < mem->size);
    if (mem->bytes[state] == unmemoized_code) {
        return run(mem, applyx(mem,
            make_form(mem, global_lambda_form,
                      vc(mem, make_double(mem, g_result), mem->nil)),
            make_global_rtenv(mem),
            state,
            mem->nil));
    }
    return run(mem, state);
}

const char *argp_program_version = "mce 1.0";
const char *argp_program_bug_address = "dave@davedoesdev.com";
const char argp_doc[] = "mce -- Metacircular Evaluator";
struct argp_option options[] = {
    {"memory-capacity", 'm', "MEBIBYTES", 0, "Maximum memory size", 0},
    {"gc-threshold", 'g', "MEBIBYTES", 0, "GC when memory exceeds this size", 0},
    {0}
};
struct arguments {
    uint64_t memory_capacity;
    uint64_t gc_threshold;
};
error_t parse_opt(int key, char *arg, struct argp_state *state) {
    struct arguments *arguments = state->input;
    switch (key) {
        case 'm':
            arguments->memory_capacity = strtoull(arg, NULL, 0) * MEBIBYTES;
            break;
        case 'g':
            arguments->gc_threshold = strtoull(arg, NULL, 0) * MEBIBYTES;
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
        DEFAULT_GC_THRESHOLD_SIZE * MEBIBYTES
    };
    argp_parse(&argp, argc, argv, 0, 0, &arguments);

    /* Read size of state */
    uint64_t size;
    assert(fread(&size, 1, sizeof(size), stdin) == sizeof(size));
    assert(size + 1 <= arguments.memory_capacity);

    /* Allocate all memory */
    uint8_t *bytes = (uint8_t*) malloc(arguments.memory_capacity);
    assert(bytes);

    /* Read state */
    assert(fread(&bytes[0], 1, size, stdin) == size);

    /* Make state */
    memory mem = {
        arguments.memory_capacity,
        size,
        arguments.gc_threshold,
        bytes,
        0
    };

    /* Make an index for nil */
    mem.nil = make_uninitialised_vector(&mem, 0);

    /* Run state */
    start(&mem, 0);

    return 0;
}
