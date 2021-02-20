#define _GNU_SOURCE
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>
#include <errno.h>
#include <string.h>
#include <quotearg.h>

#define null_code                   'a'
#define boolean_code                'b'
#define number_code                 'c'
#define char_code                   'd'
#define string_code                 'e'
#define symbol_code                 'f'
#define pair_code                   'g'
#define vector_code                 'h'

#define unmemoized_code             '0'
#define redirect_code               '1'
#define result_code                 '2'
#define step_contn_code             '3'
#define transfer_code               '4'

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

unsigned char nil[] = { null_code };

/*
pair is:
0: pair_code
1: first value: whether index(0) or address(1)
2: first value: index/address
10: second value: whether index(0) or address(1)
11: second value: index/address
19: first value (if index)

vector is:
0: vector_code
1: length
9: for each element:
     0: whether index(0) or address(1)
     1: index/address
9 + length * 9: first element (if index)
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

unsigned char *car(unsigned char *initial_state, unsigned char *state) {
    assert(state[0] == pair_code);
    uint64_t i = *(uint64_t*)&state[2];
    return state[1] ? (unsigned char*) i : &initial_state[i];
}

unsigned char *cdr(unsigned char *initial_state, unsigned char *state) {
    assert(state[0] == pair_code);
    uint64_t i = *(uint64_t*)&state[11];
    return state[10] ? (unsigned char*) i : &initial_state[i];
}

void set_car(unsigned char *state, unsigned char *val) {
    assert(state[0] == pair_code);
    state[1] = 1;
    *(uint64_t*)&state[2] = (uint64_t)val;
}

void set_cdr(unsigned char *state, unsigned char *val) {
    assert(state[0] == pair_code);
    state[10] = 1;
    *(uint64_t*)&state[11] = (uint64_t)val;
}

unsigned char *cons(unsigned char *car, unsigned char *cdr) {
    unsigned char *state = (unsigned char*) malloc(1 + 1 + 8 + 1 + 8);
    assert(state);
    state[0] = pair_code;
    state[1] = 1;
    *(uint64_t*)&state[2] = (uint64_t)car;
    state[10] = 1;
    *(uint64_t*)&state[11] = (uint64_t)cdr;
    return state;
}

#define send cons

unsigned char *sendv(unsigned char *k, unsigned char *v) {
    return send(k, cons(v, nil));
}

unsigned char *list_ref(unsigned char *initial_state, unsigned char *l, uint64_t i) {
    while (i > 0) {
        l = cdr(initial_state, l);
        --i;
    }
    return car(initial_state, l);
}

unsigned char *list_rest(unsigned char *initial_state, unsigned char *l, uint64_t i) {
    while (i > 0) {
        l = cdr(initial_state, l);
        --i;
    }
    return cdr(initial_state, l);
}

unsigned char *make_uninitialised_vector(uint64_t n) {
    unsigned char *state = (unsigned char*) malloc(1 + 8 + n * 9);
    assert(state);
    state[0] = vector_code;
    *(uint64_t*)&state[1] = n;
    return state;
}

unsigned char *make_vector(uint64_t n) {
    unsigned char *state = make_uninitialised_vector(n);
    for (uint64_t i = 0; i < n; ++i) {
        uint64_t eli = 9 + i * 9;
        state[eli] = 1;
        *(uint64_t*)&state[eli + 1] = (uint64_t)nil;
    }
    return state;
}

uint64_t vector_size(unsigned char *v) {
    assert(v[0] == vector_code);
    return *(uint64_t*)&v[1];
}

unsigned char *vector_ref(unsigned char *initial_state, unsigned char *v, uint64_t i) {
    assert(v[0] == vector_code);
    unsigned char *el = &v[9 + i * 9];
    uint64_t eli = *(uint64_t*)&el[1];
    return el[0] ? (unsigned char*) eli : &initial_state[eli];
}

void vector_set(unsigned char *v, uint64_t i, unsigned char *val) {
    assert(v[0] == vector_code);
    unsigned char *el = &v[9 + i * 9];
    el[0] = 1;
    *(uint64_t*)&el[1] = (uint64_t)val;
}

double double_val(unsigned char *state) {
    assert(state[0] == number_code);
    return *(double*)&state[1];
}

unsigned char *make_double(double v) {
    unsigned char *state = (unsigned char*) malloc(1 + sizeof(double));
    assert(state);
    state[0] = number_code;
    *(double*)&state[1] = v;
    return state;
}

bool boolean_val(unsigned char *state) {
    assert(state[0] == boolean_code);
    return state[1] == 1;
}

unsigned char *make_boolean(bool v) {
    unsigned char *state = (unsigned char*) malloc(1 + 1);
    assert(state);
    state[0] = boolean_code;
    state[1] = v ? 1 : 0;
    return state;
}

unsigned char *make_symbol(char *s) {
    size_t len = strlen(s);
    unsigned char *state = (unsigned char*) malloc(1 + 8 + len);
    assert(state);
    state[0] = symbol_code;
    *(uint64_t*)&state[1] = len;
    memcpy(&state[9], s, len);
    return state;
}

bool symbol_equals(unsigned char *state, char *s) {
    assert(state[0] == symbol_code);
    size_t len = strlen(s);
    return ((*(uint64_t*)&state[1] == len) &&
            (memcmp(&state[9], s, len) == 0));
}

unsigned char *make_result(unsigned char *val) {
    unsigned char *state = (unsigned char*) malloc(1 + 1 + 8);
    assert(state);
    state[0] = result_code;
    state[1] = 1;
    *(uint64_t*)&state[2] = (uint64_t)val;
    return state;
}

unsigned char *ctenv_lookup(unsigned char *initial_state,
                            unsigned char *i,
                            unsigned char *env) {
    uint64_t first = (uint64_t)double_val(car(initial_state, i));
    uint64_t second = (uint64_t)double_val(cdr(initial_state, i));
    unsigned char *bindings = list_ref(initial_state, env, first);
    unsigned char *v = cdr(initial_state, bindings);
    if (second < vector_size(v)) {
        return vector_ref(initial_state, v, second);
    }
    return nil;
}

unsigned char *ctenv_setvar(unsigned char *initial_state,
                            unsigned char *name,
                            unsigned char *i,
                            unsigned char *val,
                            unsigned char *env) {
    uint64_t first = (uint64_t)double_val(car(initial_state, i));
    uint64_t second = (uint64_t)double_val(cdr(initial_state, i));
    unsigned char *bindings = list_ref(initial_state, env, first);
    unsigned char *v = cdr(initial_state, bindings);
    uint64_t size = vector_size(v);

    if (second >= size) {
        unsigned char *v2 = make_uninitialised_vector(second + 1);
        for (uint64_t i = 0; i < second; ++i) {
            vector_set(v2, i, i < size ? vector_ref(initial_state, v, i) : nil);
        }
        set_cdr(bindings, v2);
        v = v2;
    }
    vector_set(v, second, val);

    if (car(initial_state, bindings)[0] == null_code) {
        set_car(bindings, cons(nil, nil));
    }
    unsigned char *p = car(initial_state, bindings);
    while (second > 0) {
        if (cdr(initial_state, p)[0] == null_code) {
            set_cdr(p, cons(nil, nil));
        }
        p = cdr(initial_state, p);
        --second;
    }
    set_car(p, name);
    
    return val;
}

unsigned char *list_to_vector(unsigned char *initial_state,
                              unsigned char *l,
                              uint64_t len) {
    unsigned char *v = make_uninitialised_vector(len);
    uint64_t i = 0;
    while ((l[0] == pair_code) && (i < len)) {
        vector_set(v, i, car(initial_state, l));
        l = cdr(initial_state, l);
    }
    return v;
}

unsigned char *extend_env(unsigned char *initial_state,
                          unsigned char *env,
                          unsigned char *syms,
                          uint64_t len,
                          unsigned char *values) {
    return cons(cons(syms, list_to_vector(initial_state, values, len)), env);
}

unsigned char *improper_extend_env(unsigned char *initial_state,
                                   unsigned char *env,
                                   unsigned char *syms,
                                   uint64_t len,
                                   unsigned char *values) {
    unsigned char *s = nil;
    unsigned char *ps = nil;
    unsigned char *v = make_uninitialised_vector(len);
    uint64_t i = 0;

    while ((i < len) && (syms[0] != null_code) && (values[0] != null_code)) {
        if (syms[0] != pair_code) {
            unsigned char *ns = cons(syms, nil);
            if (s[0] == null_code) {
                s = ns;
            } else {
                set_cdr(ps, ns);
            }
            vector_set(v, i, values);
            break;
        }

        unsigned char *ns = cons(car(initial_state, syms), nil);
        if (s[0] == null_code) {
            s = ps = ns;
        } else {
            set_cdr(ps, ns);
            ps = ns;
        }
        vector_set(v, i++, car(initial_state, values));
        syms = cdr(initial_state, syms);
        values = cdr(initial_state, values);
    }

    return cons(cons(s, v), env);
}

unsigned char *make_step_contn(unsigned char *k,
                               unsigned char *env,
                               unsigned char *args) {
    unsigned char *state = (unsigned char*) malloc(1 + 1 + 8 + 1 + 8 + 1 + 8);
    assert(state);
    state[0] = step_contn_code;
    state[1] = 1;
    *(uint64_t*)&state[2] = (uint64_t)k;
    state[10] = 1;
    *(uint64_t*)&state[11] = (uint64_t)env;
    state[19] = 1;
    *(uint64_t*)&state[20] = (uint64_t)args;
    return state;
}

unsigned char *step_contn_k(unsigned char *initial_state,
                            unsigned char *state) {
    assert(state[0] == step_contn_code);
    uint64_t i = *(uint64_t*)&state[2];
    return state[1] ? (unsigned char*) i : &initial_state[i];
}

unsigned char *step_contn_env(unsigned char *initial_state,
                              unsigned char *state) {
    assert(state[0] == step_contn_code);
    uint64_t i = *(uint64_t*)&state[11];
    return state[10] ? (unsigned char*) i : &initial_state[i];
}

unsigned char *step_contn_args(unsigned char *initial_state,
                               unsigned char *state) {
    assert(state[0] == step_contn_code);
    uint64_t i = *(uint64_t*)&state[20];
    return state[19] ? (unsigned char*) i : &initial_state[i];
}

unsigned char *transfer(unsigned char *initial_state,
                        unsigned char *args) {
    unsigned char *state = (unsigned char*) malloc(1 + 1 + 8);
    assert(state);
    state[0] = transfer_code;
    state[1] = 1;
    *(uint64_t*)&state[2] = (uint64_t) cdr(initial_state, args);
    return send(car(initial_state, args), state);
}

unsigned char *transfer_args(unsigned char *initial_state,
                             unsigned char *state) {
    assert(state[0] == transfer_code);
    uint64_t i = *(uint64_t*)&state[2];
    return state[1] ? (unsigned char*) i : &initial_state[i];
}

unsigned char *less_than(unsigned char *initial_state,
                         unsigned char *args) {
    return make_boolean(double_val(list_ref(initial_state, args, 0)) <
                        double_val(list_ref(initial_state, args, 1)));
}

unsigned char *plus(unsigned char *initial_state,
                    unsigned char *args) {
    double r = 0;
    while (args[0] == pair_code) {
        r += double_val(car(initial_state, args));
        args = cdr(initial_state, args);
    }
    return make_double(r);
}

unsigned char *xdisplay(unsigned char *initial_state,
                        unsigned char *exp,
                        FILE *out,
                        bool is_write)
{
    switch (exp[0]) {
        case null_code:
            fprintf(out, "()");
            break;

        case boolean_code:
            fprintf(out, exp[1] == 1 ? "#t" : "#f");
            break;

        case number_code:
            fprintf(out, "%g", double_val(exp));
            break;

        case char_code:
            if (is_write) {
                fprintf(out, "#\\x%x", exp[1]);
            } else {
                fprintf(out, "%c", exp[1]);
            }
            break;

        case string_code:
            if (is_write) {
                struct quoting_options *options = clone_quoting_options(NULL);
                char *quoted = quotearg_alloc((char*)&exp[9], *(uint64_t*)&exp[1], options);
                fprintf(out, "\"%s\"", quoted);
                free(quoted);
                free(options);
            } else {
                fwrite(&exp[9], 1, *(uint64_t*)&exp[1], out);
            }
            break;

        case symbol_code:
            fwrite(&exp[9], 1, *(uint64_t*)&exp[1], out);
            break;

        case pair_code: {
            bool first = true;
            fprintf(out, "(");
            while (exp[0] == pair_code) {
                if (!first) {
                    fprintf(out, " ");
                }
                first = false;
                xdisplay(initial_state, car(initial_state, exp), out, is_write);
                exp = cdr(initial_state, exp);
            }
            if (exp[0] != null_code) {
                fprintf(out, " . ");
                xdisplay(initial_state, exp, out, is_write);
            }
            fprintf(out, ")");
            break;
        }
        case vector_code: {
            bool first = true;
            fprintf(out, "#(");
            uint64_t size = vector_size(exp);
            for (uint64_t i = 0; i < size; ++i) {
                if (!first) {
                    fprintf(out, " ");
                }
                first = false;
                xdisplay(initial_state, vector_ref(initial_state, exp, i), out, is_write);
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

unsigned char *xdisplay_args(unsigned char *initial_state,
                             unsigned char *args,
                             FILE *out,
                             bool is_write) {
    unsigned char *r = nil;
    while (args[0] == pair_code) {
        r = xdisplay(initial_state, car(initial_state, args), out, is_write);
        args = cdr(initial_state, args);
    }
    if (!is_write) {
        fprintf(out, "\n");
    }
    return r; 
}

unsigned char *print(unsigned char *initial_state,
                     unsigned char *args) {
    return xdisplay_args(initial_state, args, stdout, false);
}

unsigned char *symbol_lookup(unsigned char *initial_state,
                             unsigned char *form_args,
                             unsigned char *args) {
    unsigned char *i = list_ref(initial_state, form_args, 0);
    unsigned char *k = list_ref(initial_state, args, 0);
    unsigned char *env = list_ref(initial_state, args, 1);
    return sendv(k, ctenv_lookup(initial_state, i, env));
}

unsigned char *send_value(unsigned char *initial_state,
                          unsigned char *form_args,
                          unsigned char *args) {
    unsigned char *exp = list_ref(initial_state, form_args, 0);
    unsigned char *k = list_ref(initial_state, args, 0);
    return sendv(k, exp);
}

unsigned char *handle_unmemoized(unsigned char *initial_state,
                                 unsigned char *state,
                                 unsigned char *args);
unsigned char *run(unsigned char *initial_state, unsigned char *state);

unsigned char *make_form(double form, unsigned char *args) {
    unsigned char *state = (unsigned char*) malloc(1 + 1 + 1 + 8 + 1 + 8);
    assert(state);
    state[0] = unmemoized_code;
    state[1] = pair_code;
    state[2] = 1;
    *(uint64_t*)&state[3] = (uint64_t)make_double(form);
    state[11] = 1;
    *(uint64_t*)&state[12] = (uint64_t)args;
    return state;
}

unsigned char *gresult;

unsigned char *make_global_env() {
    return cons(cons(nil, make_vector(0)), nil);
}

unsigned char *applyx(unsigned char *k,
                      unsigned char *env,
                      unsigned char *fn,
                      unsigned char *args) {
    return send(fn, make_step_contn(k, env, args));
}

unsigned char *constructed_function0(unsigned char *initial_state,
                                     unsigned char *form_args,
                                     unsigned char *args) {
    unsigned char *args2 = list_ref(initial_state, form_args, 0);
    unsigned char *cf = list_ref(initial_state, form_args, 1);
    return applyx(make_form(constructed_function1_form, cons(args, nil)),
                  make_global_env(), cf, args2);
}

unsigned char *constructed_function1(unsigned char *initial_state,
                                     unsigned char *form_args,
                                     unsigned char *args) {
    unsigned char *args2 = list_ref(initial_state, form_args, 0);
    unsigned char *f = list_ref(initial_state, args, 0);
    return send(f, args2);
}

unsigned char *global_lambda(unsigned char *initial_state,
                             unsigned char *form_args,
                             unsigned char *args) {
    unsigned char *defn = list_ref(initial_state, form_args, 0);

    if (args[0] == transfer_code) {
        args = transfer_args(initial_state, args);

        if (symbol_equals(defn, "transfer_test")) {
            return applyx(gresult,
                          make_global_env(),
                          list_ref(initial_state, args, 0),
                          list_rest(initial_state, args, 0));
        }

        xdisplay(initial_state, defn, stderr, false); fprintf(stderr, " ");
        assert_perror(EINVAL);
        return NULL;
    }

    unsigned char *k = step_contn_k(initial_state, args);
    //unsigned char *env = step_contn_env(initial_state, args);
    args = step_contn_args(initial_state, args);

    if (symbol_equals(defn, "result")) {
        return sendv(k, make_result(list_ref(initial_state, args, 2)));
    }

    if (symbol_equals(defn, "transfer")) {
        return transfer(initial_state, args);
    }

    if (symbol_equals(defn, "<")) {
        return sendv(k, less_than(initial_state, args));
    }

    if (symbol_equals(defn, "+")) {
        return sendv(k, plus(initial_state, args));
    }

    if (symbol_equals(defn, "print")) {
        return sendv(k, print(initial_state, args));
    }

    xdisplay(initial_state, defn, stderr, false); fprintf(stderr, " ");
    assert_perror(EINVAL);        
    return NULL;
}

unsigned char *if0(unsigned char *initial_state,
                   unsigned char *form_args,
                   unsigned char *args) {
    unsigned char *scan0 = list_ref(initial_state, form_args, 0);
    unsigned char *scan1 = list_ref(initial_state, form_args, 1);
    unsigned char *scan2 = list_ref(initial_state, form_args, 2);
    unsigned char *k = list_ref(initial_state, args, 0);
    unsigned char *env = list_ref(initial_state, args, 1);
    return send(scan0,
        cons(make_form(if1_form,
                       cons(k, cons(env, cons(scan1, cons(scan2, nil))))),
             cons(env, nil)));
}

unsigned char *if1(unsigned char *initial_state,
                   unsigned char *form_args,
                   unsigned char *args) {
    unsigned char *k = list_ref(initial_state, form_args, 0);
    unsigned char *env = list_ref(initial_state, form_args, 1);
    unsigned char *scan1 = list_ref(initial_state, form_args, 2);
    unsigned char *scan2 = list_ref(initial_state, form_args, 3);
    bool v = boolean_val(list_ref(initial_state, args, 0));
    return send(v ? scan1 : scan2, cons(k, cons(env, nil)));
}

unsigned char *sclis0(unsigned char *initial_state,
                      unsigned char *form_args,
                      unsigned char *args) {
    unsigned char *first = list_ref(initial_state, form_args, 0);
    unsigned char *rest = list_ref(initial_state, form_args, 1);
    unsigned char *k = list_ref(initial_state, args, 0);
    unsigned char *env = list_ref(initial_state, args, 1);
    return send(first,
        cons(make_form(sclis1_form,
                       cons(k, cons(env, cons(rest, nil)))),
             cons(env, nil)));
}

unsigned char *sclis1(unsigned char *initial_state,
                      unsigned char *form_args,
                      unsigned char *args) {
    unsigned char *k = list_ref(initial_state, form_args, 0);
    unsigned char *env = list_ref(initial_state, form_args, 1);
    unsigned char *rest = list_ref(initial_state, form_args, 2);
    unsigned char *v = list_ref(initial_state, args, 0);
    return send(rest,
        cons(make_form(sclis2_form, cons(k, cons(v, nil))),
             cons(env, nil)));
}

unsigned char *sclis2(unsigned char *initial_state,
                      unsigned char *form_args,
                      unsigned char *args) {
    unsigned char *k = list_ref(initial_state, form_args, 0);
    unsigned char *v = list_ref(initial_state, form_args, 1);
    unsigned char *w = list_ref(initial_state, args, 0);
    return sendv(k, cons(v, w));
}

unsigned char *scseq0(unsigned char *initial_state,
                      unsigned char *form_args,
                      unsigned char *args) {
    unsigned char *first = list_ref(initial_state, form_args, 0);
    unsigned char *rest = list_ref(initial_state, form_args, 1);
    unsigned char *k = list_ref(initial_state, args, 0);
    unsigned char *env = list_ref(initial_state, args, 1);
    return send(first,
        cons(make_form(scseq1_form,
                       cons(k, cons(env, cons(rest, nil)))),
             cons(env, nil)));
}

unsigned char *scseq1(unsigned char *initial_state,
                      unsigned char *form_args) {
    unsigned char *k = list_ref(initial_state, form_args, 0);
    unsigned char *env = list_ref(initial_state, form_args, 1);
    unsigned char *rest = list_ref(initial_state, form_args, 2);
    return send(rest, cons(k, cons(env, nil)));
}

unsigned char *lambda0(unsigned char *initial_state,
                       unsigned char *form_args,
                       unsigned char *args) {
    unsigned char *params = list_ref(initial_state, form_args, 0);
    unsigned char *len = list_ref(initial_state, form_args, 1);
    unsigned char *scanned = list_ref(initial_state, form_args, 2);
    unsigned char *k = list_ref(initial_state, args, 0);
    unsigned char *env = list_ref(initial_state, args, 1);
    return sendv(k,
                 make_form(lambda1_form,
                           cons(params, cons(len, cons(scanned, cons(env, nil))))));
}

unsigned char *lambda1(unsigned char *initial_state,
                       unsigned char *form_args,
                       unsigned char *args) {
    unsigned char *params = list_ref(initial_state, form_args, 0);
    uint64_t len = (uint64_t)double_val(list_ref(initial_state, form_args, 1));
    unsigned char *scanned = list_ref(initial_state, form_args, 2);
    unsigned char *env = list_ref(initial_state, form_args, 3);

    return send(scanned,
        cons(step_contn_k(initial_state, args),
             cons(extend_env(initial_state, env, params, len, step_contn_args(initial_state, args)),
                  nil)));
}

unsigned char *improper_lambda0(unsigned char *initial_state,
                                unsigned char *form_args,
                                unsigned char *args) {
    unsigned char *params = list_ref(initial_state, form_args, 0);
    unsigned char *len = list_ref(initial_state, form_args, 1);
    unsigned char *scanned = list_ref(initial_state, form_args, 2);
    unsigned char *k = list_ref(initial_state, args, 0);
    unsigned char *env = list_ref(initial_state, args, 1);
    return sendv(k,
                 make_form(improper_lambda1_form,
                           cons(params, cons(len, cons(scanned, cons(env, nil))))));
}

unsigned char *improper_lambda1(unsigned char *initial_state,
                                unsigned char *form_args,
                                unsigned char *args) {
    unsigned char *params = list_ref(initial_state, form_args, 0);
    uint64_t len = (uint64_t)double_val(list_ref(initial_state, form_args, 1));
    unsigned char *scanned = list_ref(initial_state, form_args, 2);
    unsigned char *env = list_ref(initial_state, form_args, 3);

    return send(scanned,
        cons(step_contn_k(initial_state, args),
             cons(improper_extend_env(initial_state, env, params, len, step_contn_args(initial_state, args)),
                  nil)));
}

unsigned char *letcc0(unsigned char *initial_state,
                      unsigned char *form_args,
                      unsigned char *args) {
    unsigned char *name = list_ref(initial_state, form_args, 0);
    unsigned char *scanned = list_ref(initial_state, form_args, 1); 
    unsigned char *k = list_ref(initial_state, args, 0);
    unsigned char *env = list_ref(initial_state, args, 1);
    return send(scanned,
        cons(k,
             cons(extend_env(initial_state,
                             env,
                             cons(name, nil),
                             1,
                             cons(make_form(letcc1_form, cons(k, nil)),
                                  nil)),
                  nil)));
}

unsigned char *letcc1(unsigned char *initial_state,
                      unsigned char *form_args,
                      unsigned char *args) {
    unsigned char *k = list_ref(initial_state, form_args, 0);

    if (args[0] == transfer_code) {
        return send(k, transfer_args(initial_state, args));
    }

    return send(k, step_contn_args(initial_state, args));
}

unsigned char *define0(unsigned char *initial_state,
                       unsigned char *form_args,
                       unsigned char *args) {
    unsigned char *name = list_ref(initial_state, form_args, 0);
    unsigned char *i = list_ref(initial_state, form_args, 1);
    unsigned char *scanned = list_ref(initial_state, form_args, 2);
    unsigned char *k = list_ref(initial_state, args, 0);
    unsigned char *env = list_ref(initial_state, args, 1);
    return send(scanned,
        cons(make_form(define1_form,
                       cons(k, cons(env, cons(name, cons(i, nil))))),
             cons(env, nil)));
}

unsigned char *define1(unsigned char *initial_state,
                       unsigned char *form_args,
                       unsigned char *args) {
    unsigned char *k = list_ref(initial_state, form_args, 0);
    unsigned char *env = list_ref(initial_state, form_args, 1);
    unsigned char *name = list_ref(initial_state, form_args, 2);
    unsigned char *i = list_ref(initial_state, form_args, 3);
    unsigned char *v = list_ref(initial_state, args, 0);
    return sendv(k, ctenv_setvar(initial_state, name, i, v, env));
}

unsigned char *application0(unsigned char *initial_state,
                            unsigned char *form_args,
                            unsigned char *args) {
    unsigned char *scanned = list_ref(initial_state, form_args, 0);
    unsigned char *k = list_ref(initial_state, args, 0);
    unsigned char *env = list_ref(initial_state, args,  1);
    return send(scanned,
        cons(make_form(application1_form, cons(k, cons(env, nil))),
             cons(env, nil)));
}

unsigned char *application1(unsigned char *initial_state,
                            unsigned char *form_args,
                            unsigned char *args) {
    unsigned char *k = list_ref(initial_state, form_args, 0);
    unsigned char *env = list_ref(initial_state, form_args, 1);
    unsigned char *v = list_ref(initial_state, args, 0);
    return applyx(k, env, car(initial_state, v), cdr(initial_state, v));
}

unsigned char *evalx_initial(unsigned char *initial_state,
                             unsigned char *form_args) {
    unsigned char *k = list_ref(initial_state, form_args, 0);
    unsigned char *env = list_ref(initial_state, form_args, 1);
    unsigned char *scanned = list_ref(initial_state, form_args, 2);
    return send(scanned, cons(k, cons(env, nil)));
}

unsigned char *handle_form(unsigned char *initial_state,
                           double form_n,
                           unsigned char *form_args,
                           unsigned char *args) {
    switch ((int)form_n) {
        case symbol_lookup_form:
            return symbol_lookup(initial_state, form_args, args);

        case send_value_form:
            return send_value(initial_state, form_args, args);

        case constructed_function0_form:
            return constructed_function0(initial_state, form_args, args);

        case constructed_function1_form:
            return constructed_function1(initial_state, form_args, args);

        case global_lambda_form:
            return global_lambda(initial_state, form_args, args);

        case if0_form:
            return if0(initial_state, form_args, args);

        case if1_form:
            return if1(initial_state, form_args, args);

        case sclis0_form:
            return sclis0(initial_state, form_args, args);

        case sclis1_form:
            return sclis1(initial_state, form_args, args);

        case sclis2_form:
            return sclis2(initial_state, form_args, args);

        case scseq0_form:
            return scseq0(initial_state, form_args, args);

        case scseq1_form:
            return scseq1(initial_state, form_args);

        case lambda0_form:
            return lambda0(initial_state, form_args, args);

        case lambda1_form:
            return lambda1(initial_state, form_args, args);

        case improper_lambda0_form:
            return improper_lambda0(initial_state, form_args, args);

        case improper_lambda1_form:
            return improper_lambda1(initial_state, form_args, args);

        case letcc0_form:
            return letcc0(initial_state, form_args, args);

        case letcc1_form:
            return letcc1(initial_state, form_args, args);

        case define0_form:
            return define0(initial_state, form_args, args);

        case define1_form:
            return define1(initial_state, form_args, args);

        case application0_form:
            return application0(initial_state, form_args, args);

        case application1_form:
            return application1(initial_state, form_args, args);

        case evalx_initial_form:
            return evalx_initial(initial_state, form_args);

        default:
            assert_perror(EINVAL);        
    }

    return NULL;
}

unsigned char *handle_unmemoized(unsigned char *initial_state,
                                 unsigned char *state,
                                 unsigned char *args) {
    assert(state[0] == unmemoized_code);
    return handle_form(initial_state,
                       double_val(car(initial_state, &state[1])),
                       cdr(initial_state, &state[1]),
                       args);
}

unsigned char *step(unsigned char *initial_state, unsigned char *state) {
    unsigned char *unmemoized = car(initial_state, state);
    return handle_unmemoized(initial_state,
                             unmemoized,
                             cdr(initial_state, state));
}

unsigned char *run(unsigned char *initial_state, unsigned char *state) {
    while (state[0] != result_code) {
        state = step(initial_state, state);
    }
    uint64_t i = *(uint64_t*)&state[2];
    return state[1] ? (unsigned char*) i : &initial_state[i];
}

unsigned char *start(unsigned char *state) {
    if (state[0] == unmemoized_code) {
        return run(state, send(state, nil));
    }
    return run(state, state);
}

int main(void) {
    /* Read size of state */
    uint64_t size;
    for (size_t i = 0; i < sizeof(size); ++i) {
        int c = getchar();
        assert(c != EOF);
        ((unsigned char*)(&size))[i] = (unsigned char) c;
    }

    /* Allocate memory for state */
    unsigned char *state = (unsigned char*) malloc(size * sizeof(*state));
    assert(state);

    /* Read state */
    assert(fread(state, sizeof(*state), size, stdin) == size);

    /* Run state */
    gresult = make_form(global_lambda_form, cons(make_symbol("result"), nil));
    start(state);

    return 0;
}
