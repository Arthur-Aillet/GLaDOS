##
## EPITECH PROJECT, 2023
## Makefile
## File description:
## GLaDOS Makefile
##

NAME			=	glados

all:
	stack --local-bin-path . install

clean:
	stack clean

fclean: clean
	rm -f $(NAME)

re: fclean all

quick:
	ghc app/*.hs src/*.hs -o $(NAME)

format:
	IFS=$$'\n' sourceFiles=("$$(find src test -type f -name "*.hs")"); \
	ormolu -m inplace $${sourceFiles[*]}

format_check:
	IFS=$$'\n' sourceFiles=("$$(find src test -type f -name "*.hs")"); \
	ormolu -m 'check' $${sourceFiles[*]}

test-run:
	stack test --coverage
	stack hpc report --all --destdir test/coverage

funct-tests:
	./test/functional-tests.sh

test: test-run funct-tests

.PHONY: all clean fclean re quick test test-run funct-tests format format-check
