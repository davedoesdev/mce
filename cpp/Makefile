all: mce

SOURCES = main.cpp mce.cpp
HEADERS = mce.hpp json.hpp cxxopts.hpp

mce: $(SOURCES) $(HEADERS)
	$(CXX) --std=c++17 -g -Wall -Wextra -I scope-guard/include -o $@ $(SOURCES)

clean:
	rm -f mce
