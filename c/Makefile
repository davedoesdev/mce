all: mce

SOURCES = mce.c
HEADERS = config.h
LIBS = -lm

mce: $(SOURCES) $(HEADERS)
	$(CC) -g -Wall -Wextra -Werror -I. -o $@ $(SOURCES) $(LIBS)

clean:
	rm -f mce
