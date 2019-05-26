all: mce.wasm

SOURCES = ../main.cpp ../mce.cpp wasi.cpp
HEADERS = ../mce.hpp ../json.hpp ../cxxopts.hpp

CXX=/opt/wasi-sdk/bin/clang++

mce.wasm: $(SOURCES) $(HEADERS)
	$(CXX) -g --include=wasi.hpp -Wextra -o $@ $(SOURCES)

clean:
	rm -f mce.wasm

# Currently won't compile with -Wall because of
# https://bugs.llvm.org/show_bug.cgi?id=40412
# Also, when we come to making a module, we should do
# /opt/wasi-sdk/bin/clang++ -c wup.cpp
# /opt/wasi-sdk/bin/wasm-ld -o wup.wasm --no-entry wup.o
# Since only wasm-ld can do the exports
# See here for more info: https://github.com/PetterS/clang-wasm
# including potentially more compile flags.
# Note that uses default visibility to mark exports, which has the downside of
# also exporting some library functions:
# https://github.com/WebAssembly/tool-conventions/issues/64
# However, that issue fixed this by allowing llvm.used attribute to specify
# symbol should be exported so we should be able to use:
# #define WASM_EXPORT __attribute__((used)) extern "C"
# and thus not need --export-dynamic
# Both these things will need wasi-sdk to be updated to llvm 9 (late 2019)
