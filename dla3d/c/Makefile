CC=gcc
CFLAGS=-O3 -Wall
SEXP_HOME=/home/matt/packages/sexpr_1.2.1/src
INCLUDES=-I$(SEXP_HOME)
LIBS=-L$(SEXP_HOME) -lsexp -lm

main:
	$(CC) $(CFLAGS) $(INCLUDES) -c *.c
	$(CC) $(CFLAGS) -o ktm *.o $(LIBS)

clean:
	rm -f *.o ktm
