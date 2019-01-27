all: mce

SOURCES = main.cpp mce.cpp
HEADERS = mce.hpp

mce: $(SOURCES) $(HEADERS)
	$(CXX) -g -Wall -Wextra -o $@ $(SOURCES) -lboost_program_options

clean:
	rm -f mce
