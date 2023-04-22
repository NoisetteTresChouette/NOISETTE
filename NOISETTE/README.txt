W(H);R;W(e);R;F(2,[W(l);R;]);W(o);R;W( );R;W(W);R;W(o);R;W(r);R;W(l);R;W(d);R;W( );R;W(!);R;

NOISETTE manual

NAME
	./NOISETTE.exe - convert .txt format from and into .prog adapted for 
"2Aien" and "3aien". 
	If it doesn't work, try to type make in the main 
directory to recompile the executable.

USAGE
	./NOISETTE.exe <phase> <file>

DESCRIPTION
	./NOISETTE.exe - either write a message encoded by .prog document
	or create such a program, with a low amount of instructions, to write 
	the text given by a .txt file given in input.

SYNOPSIS
	phase - <1|2|3>
	
	phase = 1
		a program file should be read, with only the phase 1 instructions.
	phase = 2
		a program file should be read, and all all instructions can
		be used.
	phase = 3
		a text file should be read, resulting in a program.


	file - file should be a prog if phase is 1 or 2, a text otherwise.

NOTE
	- prog format
		a .prog file corresponds to a program with different 
possible instructions that modify an infinite zipper, eventually resulting in 
a message. This could be compared to Brainduck. 
There is two class of instructions
	
	Phase 1 instructions:
		
		R
			move the cursor towards the right in the zipper.
		L
			move the cursor towards the left in the zipper.
		W(c)
			write the character c at the cursor position. 
			Overtakes the current one if there is one.
		F(n,i)
			repet the instruction list i n times.

	Phase 2 instructions:
	
		I
			invert the current zipper. The character placed at
			the cursor stays the same.
		D(c)
			delete all instances of the character c in the current
			zipper.
		C(n)
			apply the caesar code the current zipper, shifting
			all the leters of n places in the alphabet.
			( C(3) : 'a' -> 'd' )

	comments exist on prog. To do so, use - # -. The comment ends as soon
as you change line.

	
	- MAKEFILE
		a makefile is given with different commands available.
	
	make - type make to generate the executable NOISETTE.exe. The .cmo and
.cmi files will be removed from ./src

	make NOISETTE.exe - type this to generate the executable and keep the
.cmo and .cmi files in ./src

	make test - create test.exe in the main directory and run a series of
tests. Run test.exe to run these tests again.

	make test.exe - create test.exe in the main directory and keep the files
test.cmi and test.cmo in ./src. ./test.exe runs a series of tests.

	make try - execute NOISETTE.exe on ./exemples/test.txt to write the
result in ./exemples/txt2prog.prog, and on ./exemples/test.prog to 
./exemples/prog2txt.txt. It does compile NOISETTE.exe before execution.

	make clean - remove all .cmi and .cmo files from ./src

	make clean_noisette - remove noisette.cmi and noisette.cmo from ./src

	
	- EXEMPLES
		you can find multiple exemples of .prog and .txt files in the 
exemples directory.
