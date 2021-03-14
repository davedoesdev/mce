all: mce scan expand

%.o: %.scm
	bigloo -g -c $^

main.o: mce.o

mce: main.o mce.o
	bigloo -g -o $@ $^

scan: mce
	ln -s $^ $@

expand: expand.o
	bigloo -g -o $@ $^

clean:
	rm -f mce scan expand *.o
