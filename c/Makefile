all: mce

GNULIB_DIR = /usr/share/gnulib/lib
SOURCES = mce.c \
          $(GNULIB_DIR)/quotearg.c \
	  $(GNULIB_DIR)/xmalloc.c \
	  $(GNULIB_DIR)/xalloc-die.c \
          $(GNULIB_DIR)/exitfail.c \
          $(GNULIB_DIR)/localcharset.c \
          $(GNULIB_DIR)/c-strcasecmp.c
HEADERS = config.h
# Disable warnings for $(GNULIB_DIR)/localcharset.c
CFLAGS = -Wno-misleading-indentation -Wno-unused-const-variable

mce: $(SOURCES) $(HEADERS)
	$(CC) $(CFLAGS) -g -Wall -Wextra -Werror -I. -I$(GNULIB_DIR) -o $@ $(SOURCES)

clean:
	rm -f mce
