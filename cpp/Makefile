all: mce

SOURCES = main.cpp mce.cpp
HEADERS = mce.hpp json.hpp cxxopts.hpp

mce: $(SOURCES) $(HEADERS)
	$(CXX) --std=c++17 -g -Wall -Wextra -Werror -o $@ $(SOURCES)

clean:
	rm -f mce
