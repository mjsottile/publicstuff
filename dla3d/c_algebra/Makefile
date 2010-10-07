CC=gcc
CFLAGS=-O3 -Wall
SEXP_HOME=/home/matt/projects/sexpr/src
MT_HOME=/home/matt/packages/haskell/mersenne-random-pure64-0.2.0.3
INCLUDES=-I$(SEXP_HOME) -I$(MT_HOME)/include
LIBS=-L$(SEXP_HOME) -lsexp -lm

main:
	$(CC) $(CFLAGS) $(INCLUDES) -c *.c
	$(CC) $(CFLAGS) $(INCLUDES) $(MT_HOME)/cbits/*.c -o ktm *.o $(LIBS)

clean:
	rm -f *.o ktm
