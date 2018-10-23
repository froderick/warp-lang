CC = cc
CFLAGS = -g -ansi -std=c11 -fno-pie

all: test lexer-repl

lexer.o: lexer.c
	$(CC) $(CFLAGS) -c $^ -o $@

test: lexer.o test.c 
	$(CC) $(CFLAGS) $^ -o $@ `pkg-config --cflags --libs check`
	@./$@ && echo "\033[1;32mTests Passed\033[0m\n" || echo "\033[1;31mTests Failed\033[0m\n" #omg colors

lexer-repl: lexer.o lexer-repl.c
	$(CC) $(CFLAGS) $^ -o $@  

clean:
	rm -rf lexer.o test lexer-repl *.dSYM

# sudo apt-get install check
