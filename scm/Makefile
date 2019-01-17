all: mce expand scan

%: %.scm
	bigloo -o $@ $^

scan: mce
	ln -s $^ $@

test-loop: test-loop.scm
	./expand < $^ | ./mce

test-state: test-state.scm
	./expand < $^ | ./mce

clean:
	rm -f mce expand *.o
