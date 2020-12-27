#define _GNU_SOURCE
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>
#include <errno.h>

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

#define symbol_lookup_form          0
#define send_value_form             1
#define constructed_function_form   2
#define global_lambda_form          3
#define if0_form                    4
#define if1_form                    5
#define sclis0_form                 6
#define sclis1_form                 7
#define sclis2_form                 8
#define scseq0_form                 9
#define scseq1_form                 10
#define lambda0_form                11
#define lambda1_form                12
#define improper_lambda0_form       13
#define improper_lambda1_form       14
#define letcc0_form                 15
#define letcc1_form                 16
#define define0_form                17
#define define1_form                18
#define application0_form           19
#define application1_form           20
#define evalx_initial_form          21

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

unsigned char *list_ref(unsigned char *initial_state, unsigned char *l, uint64_t i) {
    while (i > 0) {
        l = cdr(l);
        --i;
    }
    return car(l);
}

unsigned char *list_rest(unsigned char *initial_state, unsigned char *l, uint64_t i) {
    while (i > 0) {
        l = cdr(l);
        --i;
    }
    return cdr(l);
}

uint64_t vector_size(unsigned char *v) {
    assert(state[0] == vector_code);
    return *(uint64_t*)&v[1];
}

unsigned char *vector_ref(unsigned char *initial_state, unsigned char *v, uint64_t i) {
    assert(v[0] == vector_code);
    unsigned char *el = &v[9 + i * 9];
    uint64_t eli = *(uint64_t*)&v[1];
    return el[0] ? (unsigned char*) eli : &initial_state[eli];
}

double double_val(unsigned char *state) {
    assert(state[0] == number_code);
    return *(double*)&state[1];
}

unsigned char *make_double(double v) {
    unsigned char *state = (unsigned char*) malloc(1 + sizeof(double));
    assert(state);
    state[0] = double_code;
    *(double*)&state[1] = v;
    return state;
}

bool boolean_val(unsigned char *state) {
    assert(state[0] == boolean_code);
    return state[1] == 1;
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

unsigned char *make_result(unsigned char *state) {
    unsigned char *state = (unsigned char*) malloc(1 + 1 + 8);
    assert(state);
    state[0] = result_code;
    state[1] = 1;
    *(uint64_t*)&state[2] = (uint64_t)state;
    return state;
}

unsigned char *ctenv_lookup(unsigned char *initial_state,
                            unsigned char *i,
                            unsigned char *env) {
    uint64_t first = (uint64_t)double_val(car(initial_state, i));
    uint64_t second = (uint64_t)double_val(cdr(initial_state, i));
    unsigned char *bindings = list_ref(initial_state, env, first);
    unsigned char *v = cdr(bindings);
    if (second < vector_size(v)) {
        return vector_ref(initial_state, v, second);
    }
    return NULL;
}

unsigned char *symbol_lookup(unsigned char *initial_state,
                             unsigned char *form_args,
                             unsigned char *args) {
    unsigned char *i = list_ref(initial_state, form_args, 0);
    unsigned char *k = list_ref(initial_state, args, 0);
    unsigned char *env = list_ref(initial_state, args, 1);
    return send(k, ctenv_lookup(initial_state, i, env));
}

unsigned char *send_value(unsigned char *initial_state,
                          unsigned char *form_args,
                          unsigned char *args) {
    unsigned char *exp = list_ref(initial_state, form_args, 0);
    unsigned char *k = list_ref(initial_state, args, 0);
    return send(k, exp);
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

unsigned char *constructed_function(unsigned char *initial_state,
                                    unsigned char *form_args,
                                    unsigned char *args) {
    unsigned char *args2 = list_ref(initial_state, form_args, 0);
    unsigned char *cf = list_ref(initial_state, form_args, 1);
    unsigned char *f = run(initial_state, cons(cf, cons(gresult, cons(NULL, args2))));
    return handle_unmemoized(initial_state, f, args);
}

unsigned char *global_lambda(unsigned char *initial_state,
                             unsigned char *form_args,
                             unsigned char *args) {
    unsigned char *defn = list_ref(initial_state, form_args, 0);

    if (symbol_equals(defn, "result")) {
        return make_result(list_ref(initial_state, args, 2));
    }

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
    return handle_unmemoized(initial_state, scan0,
        cons(make_form(if1_form, cons(k, cons(env, cons(scan1, cons(scan2, NULL))))),
             cons(env, NULL)));
}

unsigned char *if1(unsigned char *initial_state,
                   unsigned char *form_args,
                   unsigned char *args) {
    unsigned char *k = list_ref(form_args, 0);
    unsigned char *env = list_ref(form_args, 1);
    unsigned char *scan1 = list_ref(form_args, 2);
    unsigned char *scan2 = list_ref(form_args, 3);
    unsigned char *v = boolean_val(list_ref(args, 0));
    return handle_unmemoized(initial_state, v ? scan1 : scan2,
        cons(k, cons(env, NULL)));
}

unsigned char *handle_form(unsigned char *initial_state,
                           double form_n,
                           unsigned char *form_args,
                           unsigned char *args) {
    switch ((int)n) {
        case symbol_lookup_form:
            return symbol_lookup(initial_state, form_args, args);

        case send_value_form:
            return send_value(initial_state, form_args, args);

        case constructed_function_form:
            return constructed_function(initial_state, form_args, args);

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
            return scseq1(initial_state, form_args, args);

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
            return evalx_initial(initial_state, form_args, args);

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
                             cons(cdr(initial_state, state), NULL));
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
        // TODO: call lambda with args
        return NULL;
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
    gresult = make_form(global_lambda_form, cons(make_symbol(name), NULL));
    start(state);

    return 0;
}
