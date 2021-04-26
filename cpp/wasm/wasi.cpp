#include <stdlib.h>
#include "wasi.hpp"

pid_t getpid() {
    return -1;
}

// Hack exception handling until wasm supports them
// https://github.com/WebAssembly/proposals/issues/4
// https://github.com/WebAssembly/exception-handling/

extern "C" {

void* __cxa_allocate_exception(size_t) {
    abort();
}

void __cxa_throw(void*, void*, void*) {
    abort();
}

void* __cxa_begin_catch(void*) {
    abort();
}

}
