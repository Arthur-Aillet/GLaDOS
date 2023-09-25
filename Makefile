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

test-run:
	stack test --pedantic --coverage
	stack hpc report --all --destdir test/coverage

test: test-run

.PHONY: all clean fclean re quick test-run
