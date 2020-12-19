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

double double_val(unsigned char *state) {
    assert(state[0] == number_code);
    return *(double*)&state[1];
}

unsigned char *handle_form(unsigned char *initial_state,
                           double n,
                           unsigned char *args)
{
    switch ((int)n) {
        case 0:
            return symbol_lookup(initial_state, n, args);

        case 1:
            return send_value(initial_state, n, args);

        case 2:
            return constructed_function(initial_state, n, args);

        case 3:
            return global_lambda(initial_state, n, args);

        case 4:
            return if0(initial_state, n, args);

        case 5:
            return if1(initial_state, n, args);

        case 6:
            return sclis0(initial_state, n, args);

        case 7:
            return sclis1(initial_state, n, args);

        case 8:
            return sclis2(initial_state, n, args);

        case 9:
            return scseq0(initial_state, n, args);

        case 10:
            return scseq1(initial_state, n, args);

        case 11:
            return lambda0(initial_state, n, args);

        case 12:
            return lambda1(initial_state, n, args);

        case 13:
            return improper_lambda0(initial_state, n, args);

        case 14:
            return improper_lambda1(initial_state, n, args);

        case 15:
            return letcc0(initial_state, n, args);

        case 16:
            return letcc1(initial_state, n, args);

        case 17:
            return define0(initial_state, n, args);

        case 18:
            return define1(initial_state, n, args);

        case 19:
            return application0(initial_state, n, args);

        case 20:
            return application1(initial_state, n, args);

        case 21:
            return evalx_initial(initial_state, n, args);

        default:
            assert_perror(EINVAL);        
    }

    return NULL;
}

unsigned char *step(unsigned char *initial_state, unsigned char *state) {
    unsigned char *unmemoized = car(initial_state, state);
    assert(unmemoized[0] == unmemoized_code);
    return handle_form(initial_state,
                       double_val(car(initial_state, &unmemoized[1])),
                       cons(cdr(initial_state, &unmemoized[1]), NULL));
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
