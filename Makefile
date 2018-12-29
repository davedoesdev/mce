all: mce expand

%: %.scm
	bigloo -o $@ $^

test-loop: test-loop.scm
	./expand < $^ | ./mce

test-state: test-state.scm
	./expand < $^ | ./mce
