#define _GNU_SOURCE
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>
#include <errno.h>
#include <string.h>
#include <quotearg.h>
#include <argp.h>

#define DEFAULT_MEMORY_SIZE         10
#define MEBIBYTES                   (1024 * 1024)

#define null_code                   'a'
#define boolean_code                'b'
#define number_code                 'c'
#define char_code                   'd'
#define string_code                 'e'
#define symbol_code                 'f'
#define pair_code                   'g'
#define vector_code                 'h'

#define unmemoized_code             '0'
#define result_code                 '1'
#define step_contn_code             '2'
#define transfer_code               '3'

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

typedef struct {
    uint64_t capacity;
    uint64_t size;
    unsigned char *bytes;
} memory;

uint64_t nil;

uint64_t allocate(memory *mem, uint64_t size) {
    uint64_t cur_size = mem->size;
    assert(cur_size + size <= mem->capacity);
    mem->size += size;
    return cur_size;
}

/*
pair is:
0: pair_code
1: first value index
9: second value index
17: first value (when deserialized)

vector is:
0: vector_code
1: length
9: for each element:
     0: index
9 + length * 8: first element (when deserialized)
*/

#if 0
void deserialize_aux(unsigned char *state, uint64_t istate,
                     uint64_t *refs, uint64_t nrefs, uint64_t *iref) {
    /* note: assume always called with only indexes in state */

    if (state[istate] == pair_code) {
        assert(state[istate + 1] == 0);
        assert(*(uint64_t*)&state[istate + 2] == istate + 19);
        assert(state[istate + 10] == 0);
        assert(*iref < nrefs);
        refs[(*iref)++] = istate;
        deserialize_aux(state, istate + 19, refs, nrefs, iref);
        deserialize_aux(state, *(uint64_t*)&state[istate + 11], refs, nrefs, iref);
    }

    if (state[0] == vector_code) {

    }
}

void deserialize(unsigned char *state, uint64_t size) {
    /* state is pair containing serialized state and number of references */

    assert(state[0] == pair_code);
    assert(state[1] == 0);
    assert(*(uint64_t*)&state[2] == 19);
    assert(state[10] == 0); 
    uint64_t index = *(uint64_t*)&state[11];
    assert(state[index] == number_code);
    double nrefs = *(double*)&state[index + 1];
    assert(nrefs < size);

    uint64_t *refs = (uint64_t*) malloc(nrefs * sizeof(uint64_t));
    assert(refs);

    uint64_t iref = 0UL;
    deserialize_aux(state, 19, refs, nrefs, &iref);
    assert(iref == nrefs);

    free(refs);
}
#endif

uint64_t car(memory *mem, uint64_t state) {
    assert(mem->bytes[state] == pair_code);
    return *(uint64_t*)&mem->bytes[state + 1];
}

uint64_t cdr(memory *mem, uint64_t state) {
    assert(mem->bytes[state] == pair_code);
    return *(uint64_t*)&mem->bytes[state + 9];
}

void set_car(memory *mem, uint64_t state, uint64_t val) {
    assert(mem->bytes[state] == pair_code);
    *(uint64_t*)&mem->bytes[state + 1] = val;
}

void set_cdr(memory *mem, uint64_t state, uint64_t val) {
    assert(mem->bytes[state] == pair_code);
    *(uint64_t*)&mem->bytes[state + 9] = val;
}

uint64_t cons(memory *mem, uint64_t car, uint64_t cdr) {
    uint64_t state = allocate(mem, 1 + 8 + 8);
    mem->bytes[state] = pair_code;
    *(uint64_t*)&mem->bytes[state + 1] = car;
    *(uint64_t*)&mem->bytes[state + 9] = cdr;
    return state;
}

#define send cons

uint64_t sendv(memory *mem, uint64_t k, uint64_t v) {
    return send(mem, k, cons(mem, v, nil));
}

uint64_t list_ref(memory *mem, uint64_t l, uint64_t i) {
    while (i > 0) {
        l = cdr(mem, l);
        --i;
    }
    return car(mem, l);
}

uint64_t list_rest(memory *mem, uint64_t l, uint64_t i) {
    while (i > 0) {
        l = cdr(mem, l);
        --i;
    }
    return cdr(mem, l);
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
        *(uint64_t*)&mem->bytes[state + 9 + i * 8] = nil;
    }
    return state;
}

uint64_t vector_size(memory *mem, uint64_t v) {
    assert(mem->bytes[v] == vector_code);
    return *(uint64_t*)&mem->bytes[v + 1];
}

uint64_t vector_ref(memory *mem, uint64_t v, uint64_t i) {
    assert(mem->bytes[v] == vector_code);
    assert(i < *(uint64_t*)&mem->bytes[v + 1]);
    return *(uint64_t*)&mem->bytes[v + 9 + i * 8];
}

void vector_set(memory *mem, uint64_t v, uint64_t i, uint64_t val) {
    assert(mem->bytes[v] == vector_code);
    assert(i < *(uint64_t*)&mem->bytes[v + 1]);
    *(uint64_t*)&mem->bytes[v + 9 + i * 8] = val;
}

double double_val(memory *mem, uint64_t state) {
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
    assert(mem->bytes[state] == boolean_code);
    return mem->bytes[state + 1] == 1;
}

uint64_t make_boolean(memory *mem, bool v) {
    uint64_t state = allocate(mem, 1 + 1);
    mem->bytes[state] = boolean_code;
    mem->bytes[state + 1] = v ? 1 : 0;
    return state;
}

char char_val(memory *mem, uint64_t state) {
    assert(mem->bytes[state] == char_code);
    return mem->bytes[state + 1];
}

uint64_t make_symbol(memory *mem, char *s) {
    size_t len = strlen(s);
    uint64_t state = allocate(mem, 1 + 8 + len);
    mem->bytes[state] = symbol_code;
    *(uint64_t*)&mem->bytes[state + 1] = len;
    memcpy(&mem->bytes[state + 9], s, len);
    return state;
}

bool symbol_equals(memory *mem, uint64_t state, char *s) {
    assert(mem->bytes[state] == symbol_code);
    size_t len = strlen(s);
    return ((*(uint64_t*)&mem->bytes[state + 1] == len) &&
            (memcmp(&mem->bytes[state + 9], s, len) == 0));
}

uint64_t make_result(memory *mem, uint64_t val) {
    uint64_t state = allocate(mem, 1 + 8);
    mem->bytes[state] = result_code;
    *(uint64_t*)&mem->bytes[state + 1] = val;
    return state;
}

uint64_t ctenv_lookup(memory *mem,
                      uint64_t i,
                      uint64_t env) {
    uint64_t first = (uint64_t)double_val(mem, car(mem, i));
    uint64_t second = (uint64_t)double_val(mem, cdr(mem, i));
    uint64_t bindings = list_ref(mem, env, first);
    uint64_t v = cdr(mem, bindings);
    if (second < vector_size(mem, v)) {
        return vector_ref(mem, v, second);
    }
    return nil;
}

uint64_t ctenv_setvar(memory *mem,
                      uint64_t name,
                      uint64_t i,
                      uint64_t val,
                      uint64_t env) {
    uint64_t first = (uint64_t)double_val(mem, car(mem, i));
    uint64_t second = (uint64_t)double_val(mem, cdr(mem, i));
    uint64_t bindings = list_ref(mem, env, first);
    uint64_t v = cdr(mem, bindings);
    uint64_t size = vector_size(mem, v);

    if (second >= size) {
        uint64_t v2 = make_uninitialised_vector(mem, second + 1);
        for (uint64_t i = 0; i < second; ++i) {
            vector_set(mem, v2, i, i < size ? vector_ref(mem, v, i) : nil);
        }
        set_cdr(mem, bindings, v2);
        v = v2;
    }
    vector_set(mem, v, second, val);

    if (mem->bytes[car(mem, bindings)] == null_code) {
        set_car(mem, bindings, cons(mem, nil, nil));
    }
    uint64_t p = car(mem, bindings);
    while (second > 0) {
        if (mem->bytes[cdr(mem, p)] == null_code) {
            set_cdr(mem, p, cons(mem, nil, nil));
        }
        p = cdr(mem, p);
        --second;
    }
    set_car(mem, p, name);
    
    return val;
}

uint64_t list_to_vector(memory *mem, uint64_t l, uint64_t len) {
    uint64_t v = make_uninitialised_vector(mem, len);
    uint64_t i = 0;
    while ((mem->bytes[l] == pair_code) && (i < len)) {
        vector_set(mem, v, i++, car(mem, l));
        l = cdr(mem, l);
    }
    return v;
}

uint64_t extend_env(memory *mem,
                    uint64_t env,
                    uint64_t syms,
                    uint64_t len,
                    uint64_t values) {
    return cons(mem, cons(mem, syms, list_to_vector(mem, values, len)), env);
}

uint64_t improper_extend_env(memory *mem,
                             uint64_t env,
                             uint64_t syms,
                             uint64_t len,
                             uint64_t values) {
    uint64_t s = nil;
    uint64_t ps = nil;
    uint64_t v = make_uninitialised_vector(mem, len);
    uint64_t i = 0;

    while ((i < len) && (mem->bytes[syms] != null_code) && (mem->bytes[values] != null_code)) {
        if (mem->bytes[syms] != pair_code) {
            uint64_t ns = cons(mem, syms, nil);
            if (mem->bytes[s] == null_code) {
                s = ns;
            } else {
                set_cdr(mem, ps, ns);
            }
            vector_set(mem, v, i, values);
            break;
        }

        uint64_t ns = cons(mem, car(mem, syms), nil);
        if (mem->bytes[s] == null_code) {
            s = ps = ns;
        } else {
            set_cdr(mem, ps, ns);
            ps = ns;
        }
        vector_set(mem, v, i++, car(mem, values));
        syms = cdr(mem, syms);
        values = cdr(mem, values);
    }

    return cons(mem, cons(mem, s, v), env);
}

uint64_t make_step_contn(memory *mem, uint64_t k, uint64_t env, uint64_t args) {
    uint64_t state = allocate(mem, 1 + 8 + 8 + 8);
    mem->bytes[state] = step_contn_code;
    *(uint64_t*)&mem->bytes[state + 1] = k;
    *(uint64_t*)&mem->bytes[state + 9] = env;
    *(uint64_t*)&mem->bytes[state + 17] = args;
    return state;
}

uint64_t step_contn_k(memory *mem, uint64_t state) {
    assert(mem->bytes[state] == step_contn_code);
    return *(uint64_t*)&mem->bytes[state + 1];
}

uint64_t step_contn_env(memory *mem, uint64_t state) {
    assert(mem->bytes[state] == step_contn_code);
    return *(uint64_t*)&mem->bytes[state + 9];
}

uint64_t step_contn_args(memory *mem, uint64_t state) {
    assert(mem->bytes[state] == step_contn_code);
    return *(uint64_t*)&mem->bytes[state + 17];
}

uint64_t transfer(memory *mem, uint64_t args) {
    uint64_t state = allocate(mem, 1 + 8);
    mem->bytes[state] = transfer_code;
    *(uint64_t*)&mem->bytes[state + 1] = cdr(mem, args);
    return send(mem, car(mem, args), state);
}

uint64_t transfer_args(memory *mem, uint64_t state) {
    assert(mem->bytes[state] == transfer_code);
    return *(uint64_t*)&mem->bytes[state + 1];
}

uint64_t less_than(memory *mem, uint64_t args) {
    return make_boolean(mem, 
        double_val(mem, list_ref(mem, args, 0)) <
        double_val(mem, list_ref(mem, args, 1)));
}

uint64_t greater_than(memory *mem, uint64_t args) {
    return make_boolean(mem,
        double_val(mem, list_ref(mem, args, 0)) >
        double_val(mem, list_ref(mem, args, 1)));
}

uint64_t plus(memory *mem, uint64_t args) {
    double r = 0;
    while (mem->bytes[args] == pair_code) {
        r += double_val(mem, car(mem, args));
        args = cdr(mem, args);
    }
    return make_double(mem, r);
}

uint64_t multiply(memory *mem, uint64_t args) {
    double r = 1;
    while (mem->bytes[args] == pair_code) {
        r *= double_val(mem, car(mem, args));
        args = cdr(mem, args);
    }
    return make_double(mem, r);
}

uint64_t is_null(memory *mem, uint64_t args) {
    return make_boolean(mem,
        mem->bytes[car(mem, args)] == null_code);
}

uint64_t is_eq(memory *mem, uint64_t args) {
    uint64_t x = list_ref(mem, args, 0);
    uint64_t y = list_ref(mem, args, 1);

    unsigned char x_type = mem->bytes[x];
    unsigned char y_type = mem->bytes[y];

    if (x_type == null_code) {
        return make_boolean(mem, y_type == null_code);
    }

    if (y_type == null_code) {
        return make_boolean(mem, false);
    }

    if (x_type != y_type) {
        return make_boolean(mem, false);
    }

    if (x_type == boolean_code) {
        return make_boolean(mem, boolean_val(mem, x) == boolean_val(mem, y));
    }

    if (x_type == char_code) {
        return make_boolean(mem, char_val(mem, x) == char_val(mem, y));
    }

    if (x_type == number_code) {
        return make_boolean(mem, double_val(mem, x) == double_val(mem, y));
    }

    if ((x_type == string_code) || (x_type == symbol_code)) {
        uint64_t len = *(uint64_t*)&mem->bytes[x + 1];
        return make_boolean(mem,
            (*(uint64_t*)&mem->bytes[y + 1] == len) &&
            (memcmp(&mem->bytes[x + 9], &mem->bytes[y + 9], len) == 0));

    }

    return make_boolean(mem, x == y);
}

uint64_t is_string(memory *mem, uint64_t args) {
    return make_boolean(mem,
        mem->bytes[car(mem, args)] == string_code);
}

uint64_t is_pair(memory *mem, uint64_t args) {
    return make_boolean(mem,
        mem->bytes[car(mem, args)] == pair_code);
}

uint64_t is_vector(memory *mem, uint64_t args) {
    return make_boolean(mem,
        mem->bytes[car(mem, args)] == vector_code);
}

uint64_t is_procedure(memory *mem, uint64_t args) {
    return make_boolean(mem,
        mem->bytes[car(mem, args)] == unmemoized_code);
}

uint64_t is_number_equal(memory *mem, uint64_t args) {
    return make_boolean(mem,
        double_val(mem, list_ref(mem, args, 0)) ==
        double_val(mem, list_ref(mem, args, 1)));
}

uint64_t xdisplay(memory *mem,
                  uint64_t exp,
                  FILE *out,
                  bool is_write)
{
    switch (mem->bytes[exp]) {
        case null_code:
            fprintf(out, "()");
            break;

        case boolean_code:
            fprintf(out, mem->bytes[exp + 1] == 1 ? "#t" : "#f");
            break;

        case number_code:
            fprintf(out, "%g", double_val(mem, exp));
            break;

        case char_code:
            if (is_write) {
                fprintf(out, "#\\x%x", char_val(mem, exp));
            } else {
                fprintf(out, "%c", char_val(mem, exp));
            }
            break;

        case string_code:
            if (is_write) {
                struct quoting_options *options = clone_quoting_options(NULL);
                char *quoted = quotearg_alloc((char*)&mem->bytes[exp + 9],
                                              *(uint64_t*)&mem->bytes[exp + 1],
                                              options);
                fprintf(out, "\"%s\"", quoted);
                free(quoted);
                free(options);
            } else {
                fwrite(&mem->bytes[exp + 9], 1, *(uint64_t*)&mem->bytes[exp + 1], out);
            }
            break;

        case symbol_code:
            fwrite(&mem->bytes[exp + 9], 1, *(uint64_t*)&mem->bytes[exp + 1], out);
            break;

        case pair_code: {
            bool first = true;
            fprintf(out, "(");
            while (mem->bytes[exp] == pair_code) {
                if (!first) {
                    fprintf(out, " ");
                }
                first = false;
                xdisplay(mem, car(mem, exp), out, is_write);
                exp = cdr(mem, exp);
            }
            if (mem->bytes[exp] != null_code) {
                fprintf(out, " . ");
                xdisplay(mem, exp, out, is_write);
            }
            fprintf(out, ")");
            break;
        }
        case vector_code: {
            bool first = true;
            fprintf(out, "#(");
            uint64_t size = vector_size(mem, exp);
            for (uint64_t i = 0; i < size; ++i) {
                if (!first) {
                    fprintf(out, " ");
                }
                first = false;
                xdisplay(mem, vector_ref(mem, exp, i), out, is_write);
            }
            fprintf(out, ")");
            break;
        }
        case unmemoized_code:
            fprintf(out, "#<procedure>");
            break;

        default:
            assert_perror(EINVAL);        
    }

    return exp;
}

uint64_t xdisplay_args(memory *mem,
                       uint64_t args,
                       FILE *out,
                       bool is_write) {
    uint64_t r = nil;
    while (mem->bytes[args] == pair_code) {
        r = xdisplay(mem, car(mem, args), out, is_write);
        args = cdr(mem, args);
    }
    if (!is_write) {
        fprintf(out, "\n");
    }
    return r; 
}

uint64_t print(memory *mem, uint64_t args) {
    return xdisplay_args(mem, args, stdout, false);
}

uint64_t symbol_lookup(memory *mem,
                       uint64_t form_args,
                       uint64_t args) {
    uint64_t i = list_ref(mem, form_args, 0);
    uint64_t k = list_ref(mem, args, 0);
    uint64_t env = list_ref(mem, args, 1);
    return sendv(mem, k, ctenv_lookup(mem, i, env));
}

uint64_t send_value(memory *mem,
                    uint64_t form_args,
                    uint64_t args) {
    uint64_t exp = list_ref(mem, form_args, 0);
    uint64_t k = list_ref(mem, args, 0);
    return sendv(mem, k, exp);
}

uint64_t make_form(memory *mem, double form, uint64_t args) {
    uint64_t state = allocate(mem, 1 + 1 + 8 + 8);
    mem->bytes[state] = unmemoized_code;
    mem->bytes[state + 1] = pair_code;
    *(uint64_t*)&mem->bytes[state + 2] = make_double(mem, form);
    *(uint64_t*)&mem->bytes[state + 10] = args;
    return state;
}

uint64_t gresult;

uint64_t make_global_env(memory *mem) {
    return cons(mem, cons(mem, nil, make_vector(mem, 0)), nil);
}

uint64_t applyx(memory *mem,
                uint64_t k,
                uint64_t env,
                uint64_t fn,
                uint64_t args) {
    return send(mem, fn, make_step_contn(mem, k, env, args));
}

uint64_t transfer_test(memory *mem,
                       uint64_t args) {
    return applyx(mem,
                  make_form(mem, global_lambda_form, cons(mem, make_symbol(mem, "result"), nil)),
                  make_global_env(mem),
                  car(mem, args),
                  cdr(mem, args));
}

uint64_t constructed_function0(memory *mem,
                               uint64_t form_args,
                               uint64_t args) {
    uint64_t args2 = list_ref(mem, form_args, 0);
    uint64_t cf = list_ref(mem, form_args, 1);
    return applyx(mem, make_form(mem, constructed_function1_form, cons(mem, args, nil)),
                  make_global_env(mem), cf, args2);
}

uint64_t constructed_function1(memory *mem,
                               uint64_t form_args,
                               uint64_t args) {
    uint64_t args2 = list_ref(mem, form_args, 0);
    uint64_t f = list_ref(mem, args, 0);
    return send(mem, f, args2);
}

uint64_t global_lambda(memory *mem,
                       uint64_t form_args,
                       uint64_t args) {
    uint64_t defn = list_ref(mem, form_args, 0);

    if (mem->bytes[args] == transfer_code) {
        args = transfer_args(mem, args);

        if (symbol_equals(mem, defn, "transfer-test")) {
            return applyx(mem,
                          gresult,
                          make_global_env(mem),
                          list_ref(mem, args, 0),
                          list_rest(mem, args, 0));
        }

        xdisplay(mem, defn, stderr, false); fprintf(stderr, " ");
        assert_perror(EINVAL);
        return 0;
    }

    if (symbol_equals(mem, defn, "result")) {
        return make_result(mem, car(mem, args));
    }

    uint64_t k = step_contn_k(mem, args);
    uint64_t env = step_contn_env(mem, args);
    args = step_contn_args(mem, args);

    if (symbol_equals(mem, defn, "transfer")) {
        return transfer(mem, args);
    }

    if (symbol_equals(mem, defn, "<")) {
        return sendv(mem, k, less_than(mem, args));
    }

    if (symbol_equals(mem, defn, ">")) {
        return sendv(mem, k, greater_than(mem, args));
    }

    if (symbol_equals(mem, defn, "+")) {
        return sendv(mem, k, plus(mem, args));
    }

    if (symbol_equals(mem, defn, "*")) {
        return sendv(mem, k, multiply(mem, args));
    }

    if (symbol_equals(mem, defn, "null?")) {
        return sendv(mem, k, is_null(mem, args));
    }

    if (symbol_equals(mem, defn, "car")) {
        return sendv(mem, k, car(mem, car(mem, args)));
    }

    if (symbol_equals(mem, defn, "cdr")) {
        return sendv(mem, k, cdr(mem, car(mem, args)));
    }

    if (symbol_equals(mem, defn, "cons")) {
        return sendv(mem, k, cons(mem, list_ref(mem, args, 0),
                                       list_ref(mem, args, 1)));
    }

    if (symbol_equals(mem, defn, "eq?")) {
        return sendv(mem, k, is_eq(mem, args));
    }

    if (symbol_equals(mem, defn, "print")) {
        return sendv(mem, k, print(mem, args));
    }

    if (symbol_equals(mem, defn, "string?")) {
        return sendv(mem, k, is_string(mem, args));
    }

    if (symbol_equals(mem, defn, "pair?")) {
        return sendv(mem, k, is_pair(mem, args));
    }

    if (symbol_equals(mem, defn, "vector?")) {
        return sendv(mem, k, is_vector(mem, args));
    }

    if (symbol_equals(mem, defn, "procedure?")) {
        return sendv(mem, k, is_procedure(mem, args));
    }

    if (symbol_equals(mem, defn, "vector-length")) {
        return sendv(mem, k, make_double(mem, vector_size(mem, car(mem, args))));
    }

    if (symbol_equals(mem, defn, "=")) {
        return sendv(mem, k, is_number_equal(mem, args));
    }

    if (symbol_equals(mem, defn, "vector-ref")) {
        return sendv(mem, k, vector_ref(mem,
                                        list_ref(mem, args, 0),
                                        double_val(mem, list_ref(mem, args, 1))));
    }

    if (symbol_equals(mem, defn, "apply")) {
        return applyx(mem,
                      k,
                      env,
                      list_ref(mem, args, 0),
                      list_ref(mem, args, 1));
    }

    if (symbol_equals(mem, defn, "transfer-test")) {
        return sendv(mem, k, transfer_test(mem, args));
    }

    xdisplay(mem, defn, stderr, false); fprintf(stderr, " ");
    assert_perror(EINVAL);        
    return 0;
}

uint64_t if0(memory *mem,
             uint64_t form_args,
             uint64_t args) {
    uint64_t scan0 = list_ref(mem, form_args, 0);
    uint64_t scan1 = list_ref(mem, form_args, 1);
    uint64_t scan2 = list_ref(mem, form_args, 2);
    uint64_t k = list_ref(mem, args, 0);
    uint64_t env = list_ref(mem, args, 1);
    return send(mem, scan0,
        cons(mem, make_form(mem, if1_form,
                            cons(mem, k,
                                 cons(mem, env, cons(mem, scan1, cons(mem, scan2, nil))))),
             cons(mem, env, nil)));
}

uint64_t if1(memory *mem,
             uint64_t form_args,
             uint64_t args) {
    uint64_t k = list_ref(mem, form_args, 0);
    uint64_t env = list_ref(mem, form_args, 1);
    uint64_t scan1 = list_ref(mem, form_args, 2);
    uint64_t scan2 = list_ref(mem, form_args, 3);
    bool v = boolean_val(mem, list_ref(mem, args, 0));
    return send(mem, v ? scan1 : scan2, cons(mem, k, cons(mem, env, nil)));
}

uint64_t sclis0(memory *mem,
                uint64_t form_args,
                uint64_t args) {
    uint64_t first = list_ref(mem, form_args, 0);
    uint64_t rest = list_ref(mem, form_args, 1);
    uint64_t k = list_ref(mem, args, 0);
    uint64_t env = list_ref(mem, args, 1);
    return send(mem, first,
        cons(mem, make_form(mem, sclis1_form,
                            cons(mem, k, cons(mem, env, cons(mem, rest, nil)))),
             cons(mem, env, nil)));
}

uint64_t sclis1(memory *mem,
                uint64_t form_args,
                uint64_t args) {
    uint64_t k = list_ref(mem, form_args, 0);
    uint64_t env = list_ref(mem, form_args, 1);
    uint64_t rest = list_ref(mem, form_args, 2);
    uint64_t v = list_ref(mem, args, 0);
    return send(mem, rest,
        cons(mem, make_form(mem, sclis2_form, cons(mem, k, cons(mem, v, nil))),
             cons(mem, env, nil)));
}

uint64_t sclis2(memory *mem,
                uint64_t form_args,
                uint64_t args) {
    uint64_t k = list_ref(mem, form_args, 0);
    uint64_t v = list_ref(mem, form_args, 1);
    uint64_t w = list_ref(mem, args, 0);
    return sendv(mem, k, cons(mem, v, w));
}

uint64_t scseq0(memory *mem,
                uint64_t form_args,
                uint64_t args) {
    uint64_t first = list_ref(mem, form_args, 0);
    uint64_t rest = list_ref(mem, form_args, 1);
    uint64_t k = list_ref(mem, args, 0);
    uint64_t env = list_ref(mem, args, 1);
    return send(mem, first,
        cons(mem, make_form(mem, scseq1_form,
                            cons(mem, k, cons(mem, env, cons(mem, rest, nil)))),
             cons(mem, env, nil)));
}

uint64_t scseq1(memory *mem,
                uint64_t form_args) {
    uint64_t k = list_ref(mem, form_args, 0);
    uint64_t env = list_ref(mem, form_args, 1);
    uint64_t rest = list_ref(mem, form_args, 2);
    return send(mem, rest, cons(mem, k, cons(mem, env, nil)));
}

uint64_t lambda0(memory *mem,
                 uint64_t form_args,
                 uint64_t args) {
    uint64_t params = list_ref(mem, form_args, 0);
    uint64_t len = list_ref(mem, form_args, 1);
    uint64_t scanned = list_ref(mem, form_args, 2);
    uint64_t k = list_ref(mem, args, 0);
    uint64_t env = list_ref(mem, args, 1);
    return sendv(mem, k,
                 make_form(mem, lambda1_form,
                           cons(mem, params,
                                cons(mem, len, cons(mem, scanned, cons(mem, env, nil))))));
}

uint64_t lambda1(memory *mem,
                 uint64_t form_args,
                 uint64_t args) {
    uint64_t params = list_ref(mem, form_args, 0);
    uint64_t len = (uint64_t)double_val(mem, list_ref(mem, form_args, 1));
    uint64_t scanned = list_ref(mem, form_args, 2);
    uint64_t env = list_ref(mem, form_args, 3);

    return send(mem, scanned,
        cons(mem, step_contn_k(mem, args),
             cons(mem, extend_env(mem, env, params, len, step_contn_args(mem, args)),
                  nil)));
}

uint64_t improper_lambda0(memory *mem,
                          uint64_t form_args,
                          uint64_t args) {
    uint64_t params = list_ref(mem, form_args, 0);
    uint64_t len = list_ref(mem, form_args, 1);
    uint64_t scanned = list_ref(mem, form_args, 2);
    uint64_t k = list_ref(mem, args, 0);
    uint64_t env = list_ref(mem, args, 1);
    return sendv(mem, k,
                 make_form(mem, improper_lambda1_form,
                           cons(mem, params,
                                cons(mem, len, cons(mem, scanned, cons(mem, env, nil))))));
}

uint64_t improper_lambda1(memory *mem,
                          uint64_t form_args,
                          uint64_t args) {
    uint64_t params = list_ref(mem, form_args, 0);
    uint64_t len = (uint64_t)double_val(mem, list_ref(mem, form_args, 1));
    uint64_t scanned = list_ref(mem, form_args, 2);
    uint64_t env = list_ref(mem, form_args, 3);

    return send(mem, scanned,
        cons(mem, step_contn_k(mem, args),
             cons(mem, improper_extend_env(mem, env, params, len, step_contn_args(mem, args)),
                  nil)));
}

uint64_t letcc0(memory *mem,
                uint64_t form_args,
                uint64_t args) {
    uint64_t name = list_ref(mem, form_args, 0);
    uint64_t scanned = list_ref(mem, form_args, 1); 
    uint64_t k = list_ref(mem, args, 0);
    uint64_t env = list_ref(mem, args, 1);
    return send(mem, scanned,
        cons(mem, k,
             cons(mem, extend_env(mem,
                                  env,
                                  cons(mem, name, nil),
                                  1,
                                  cons(mem, make_form(mem, letcc1_form, cons(mem, k, nil)),
                                       nil)),
                  nil)));
}

uint64_t letcc1(memory *mem,
                uint64_t form_args,
                uint64_t args) {
    uint64_t k = list_ref(mem, form_args, 0);

    if (mem->bytes[args] == transfer_code) {
        return send(mem, k, transfer_args(mem, args));
    }

    return send(mem, k, step_contn_args(mem, args));
}

uint64_t define0(memory *mem,
                 uint64_t form_args,
                 uint64_t args) {
    uint64_t name = list_ref(mem, form_args, 0);
    uint64_t i = list_ref(mem, form_args, 1);
    uint64_t scanned = list_ref(mem, form_args, 2);
    uint64_t k = list_ref(mem, args, 0);
    uint64_t env = list_ref(mem, args, 1);
    return send(mem, scanned,
        cons(mem, make_form(mem, define1_form,
                            cons(mem, k, cons(mem, env, cons(mem, name, cons(mem, i, nil))))),
             cons(mem, env, nil)));
}

uint64_t define1(memory *mem,
                 uint64_t form_args,
                 uint64_t args) {
    uint64_t k = list_ref(mem, form_args, 0);
    uint64_t env = list_ref(mem, form_args, 1);
    uint64_t name = list_ref(mem, form_args, 2);
    uint64_t i = list_ref(mem, form_args, 3);
    uint64_t v = list_ref(mem, args, 0);
    return sendv(mem, k, ctenv_setvar(mem, name, i, v, env));
}

uint64_t application0(memory *mem,
                      uint64_t form_args,
                      uint64_t args) {
    uint64_t scanned = list_ref(mem, form_args, 0);
    uint64_t k = list_ref(mem, args, 0);
    uint64_t env = list_ref(mem, args,  1);
    return send(mem, scanned,
        cons(mem, make_form(mem, application1_form,
                            cons(mem, k, cons(mem, env, nil))),
             cons(mem, env, nil)));
}

uint64_t application1(memory *mem,
                      uint64_t form_args,
                      uint64_t args) {
    uint64_t k = list_ref(mem, form_args, 0);
    uint64_t env = list_ref(mem, form_args, 1);
    uint64_t v = list_ref(mem, args, 0);
    return applyx(mem, k, env, car(mem, v), cdr(mem, v));
}

uint64_t evalx_initial(memory *mem,
                       uint64_t form_args) {
    uint64_t k = list_ref(mem, form_args, 0);
    uint64_t env = list_ref(mem, form_args, 1);
    uint64_t scanned = list_ref(mem, form_args, 2);
    return send(mem, scanned, cons(mem, k, cons(mem, env, nil)));
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
    assert(mem->bytes[state] == unmemoized_code);
    return handle_form(mem,
                       double_val(mem, car(mem, state + 1)),
                       cdr(mem, state + 1),
                       args);
}

uint64_t step(memory *mem, uint64_t state) {
    return handle_unmemoized(mem, car(mem, state), cdr(mem, state));
}

uint64_t run(memory *mem, uint64_t state) {
    while (mem->bytes[state] != result_code) {
        state = step(mem, state);
    }
    return *(uint64_t*)&mem->bytes[state + 1];
}

uint64_t start(memory *mem, uint64_t state) {
    if (mem->bytes[state] == unmemoized_code) {
        return run(mem, send(mem, state, nil));
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
    struct arguments arguments = { DEFAULT_MEMORY_SIZE * MEBIBYTES };
    argp_parse(&argp, argc, argv, 0, 0, &arguments);

    /* Read size of state */
    uint64_t size;
    assert(fread(&size, 1, sizeof(size), stdin) == sizeof(size));
    assert(size + 1 <= arguments.memory_capacity);

    /* Allocate all memory */
    unsigned char *bytes = (unsigned char*) malloc(arguments.memory_capacity);
    assert(bytes);

    /* Read state */
    assert(fread(&bytes[0], 1, size, stdin) == size);

    /* Make an index for null */
    nil = size;
    bytes[nil] = null_code;

    /* Run state */
    memory mem = { arguments.memory_capacity, 1 + size, bytes };
    gresult = make_form(&mem, global_lambda_form,
                        cons(&mem, make_symbol(&mem, "result"), nil));
    start(&mem, 0);

    return 0;
}
