all: mce scan expand

%.o: %.scm
	bigloo -c $^

main.o: mce.o

mce: main.o mce.o
	bigloo -o $@ $^

scan: mce
	ln -s $^ $@

expand: expand.o
	bigloo -o $@ $^

clean:
	rm -f mce scan expand *.o
