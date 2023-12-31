![glados](GLaDOS.png)

GLaDOS is a lisp interpreter with the goal to emulate the behaviour of shezscheme.

# Installation
To compile GLaDOS, use the MAKEFILE at the root of the directory.

To use a Makefile, just use the "make" command.
this makefile has other rules you can call by using make + [the command of your choice]:
```
all (default)   ->  just compiles GLaDOS
clean           ->  cleans the repository of unnecessary files
fclean          ->  cleans the repository of unnecesary files and of the executable
re              ->  calls fclean then all
quick           ->  compiles without stack
format          ->  formats the source code according to our coding style
format-check    ->  checks if the code is to our coding style's standards
test-run        ->  lanches a battery of unit tests
funct-tests     ->  launches a battery of functional tests
tests           ->  launches both the unit and functional tests
```
For instance, if you want to recompile GLaDOS, you can use
```
$ make re
```

# Usage
```
$ ./glados
```
Opens the interractive command interpreter
while the programm is running you can type your commands, press tab for autocompletion on keywords and defined variables, and press the up and down arrow to access the command history.


```
$ ./glados < yourfile
```
Executes the instructions in the file

---

GLaDOS can only interpret Lisp. to read more about lisp.
here's a basic Lisp tutorial to get you started:
https://lisp-lang.org/learn/

there are also examples in the test/samples directory.
