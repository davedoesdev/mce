all: mce

SOURCES = mce.c
LIBS = -lm

mce: $(SOURCES)
	$(CC) -g -Wall -Wextra -Werror -I. -o $@ $^ $(LIBS)

clean:
	rm -f mce
