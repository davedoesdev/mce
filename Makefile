all: mce expand

%: %.scm
	bigloo -o $@ $^

test: test.scm
	./expand < test.scm | ./mce
