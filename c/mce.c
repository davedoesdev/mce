#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

int main(void) {
    /* Read size of state */
    uint64_t size;
    for (size_t i = 0; i < sizeof(size); ++i) {
        int c = getchar();
        if (c == EOF) {
            fprintf(stderr, "Can't read length\n");
            return 1;
        }
        ((unsigned char*)(&size))[i] = (unsigned char) c;
    }

    /* Allocate memory for state */
    unsigned char *state = (unsigned char*) malloc(size * sizeof(*state));
    if (!state) {
        fprintf(stderr, "Failed to allocate memory for state\n");
        return 2;
    }

    /* Read state */
    if (fread(state, sizeof(*state), size, stdin) != size) {
        fprintf(stderr, "Failed to read state\n");
        return 3;
    }

    // deserialize
    // memoize (what data structure for lambda?)
    // run

    return 0;
}
