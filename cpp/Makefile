all: mce

%: %.cpp
	$(CXX) -g -o $@ $^ -lboost_program_options

clean:
	rm -f mce
