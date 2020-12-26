#define _GNU_SOURCE
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>
#include <errno.h>

#define null_code       'a'
#define boolean_code    'b'
#define number_code     'c'
#define char_code       'd'
#define string_code     'e'
#define symbol_code     'f'
#define pair_code       'g'
#define vector_code     'h'

#define unmemoized_code '0'
#define redirect_code   '1'
#define result_code     '2'

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
    state[0] = double_code;
    *(double*)&state[1] = v;
    return state;
}

unsigned char *make_symbol(char *s) {
    size_t len = strlen(s);
    unsigned char *state = (unsigned char*) malloc(1 + 8 + len);
    state[0] = symbol_code;
    *(uint64_t*)&state[1] = len;
    memcpy(&state[9], s, len);
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
unsigned char *step(unsigned char *initial_state, unsigned char *state);
unsigned char *run(unsigned char *initial_state, unsigned char *state);

unsigned char *make_global(char *name) {
    unsigned char *state = (unsigned char*) malloc(1 + 1 + 1 + 8 + 1 + 8);
    assert(state);
    state[0] = unmemoized_code;
    state[1] = pair_code;
    state[2] = 1;
    *(uint64_t*)&state[3] = (uint64_t)make_double(3/*global_lambda*/);
    state[11] = 1;
    *(uint64_t*)&state[12] = (uint64_t)cons(make_symbol(name), NULL);
    return state;
}

unsigned char *constructed_function(unsigned char *initial_state,
                                    unsigned char *form_args,
                                    unsigned char *args) {
    unsigned char *args2 = list_ref(initial_state, form_args, 0);
    unsigned char *cf = list_ref(initial_state, form_args, 1);
    unsigned char *f = run(initial_state, cons(cf, cons(make_global("result"), cons(NULL, args2))));
    return step(initial_state, cons(f, args));
}

unsigned char *global_lambda(unsigned char *initial_state,
                             unsigned char *form_args,
                             unsigned char *args) {

}

unsigned char *handle_form(unsigned char *initial_state,
                           double form_n,
                           unsigned char *form_args,
                           unsigned char *args) {
    switch ((int)n) {
        case 0:
            return symbol_lookup(initial_state, form_args, args);

        case 1:
            return send_value(initial_state, form_args, args);

        case 2:
            return constructed_function(initial_state, form_args, args);

        case 3:
            return global_lambda(initial_state, form_args, args);

        case 4:
            return if0(initial_state, form_args, args);

        case 5:
            return if1(initial_state, form_args, args);

        case 6:
            return sclis0(initial_state, form_args, args);

        case 7:
            return sclis1(initial_state, form_args, args);

        case 8:
            return sclis2(initial_state, form_args, args);

        case 9:
            return scseq0(initial_state, form_args, args);

        case 10:
            return scseq1(initial_state, form_args, args);

        case 11:
            return lambda0(initial_state, form_args, args);

        case 12:
            return lambda1(initial_state, form_args, args);

        case 13:
            return improper_lambda0(initial_state, form_args, args);

        case 14:
            return improper_lambda1(initial_state, form_args, args);

        case 15:
            return letcc0(initial_state, form_args, args);

        case 16:
            return letcc1(initial_state, form_args, args);

        case 17:
            return define0(initial_state, form_args, args);

        case 18:
            return define1(initial_state, form_args, args);

        case 19:
            return application0(initial_state, form_args, args);

        case 20:
            return application1(initial_state, form_args, args);

        case 21:
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
    return &state[1];
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
    start(state);

    return 0;
}
