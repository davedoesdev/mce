all: mce

SOURCES = main.cpp mce.cpp
HEADERS = mce.hpp json.hpp cxxopts.hpp

mce: $(SOURCES) $(HEADERS)
	$(CXX) -g -Wall -Wextra -o $@ $(SOURCES)

clean:
	rm -f mce
