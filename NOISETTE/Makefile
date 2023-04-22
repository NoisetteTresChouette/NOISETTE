CC = ocamlc

all: NOISETTE.exe clean_noisette

test.exe: ./src/test.ml
	$(CC) -o $@ $<

clean:
	rm -f ./src/*.cm*

clean_noisette:
	rm -f ./src/noisette.cm*

test: test.exe clean
	./test.exe

NOISETTE.exe: ./src/noisette.ml
	$(CC) -o $@ $^

try: NOISETTE.exe clean
	./NOISETTE.exe 3 ./examples/test.txt > ./examples/txt2prog.prog
	./NOISETTE.exe 2 ./examples/test.prog > ./examples/prog2txt.txt



