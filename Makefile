##
## EPITECH PROJECT, 2023
## Makefile
## File description:
## GLaDOS Makefile
##

BIN_PATH		:=	$(shell stack path --local-install-root)
NAME			=	glados

all:
		stack build
		stack --local-bin-path . install

clean:
	stack clean

fclean: clean
	rm -f $(NAME)

re: fclean all

quick:
	ghc app/*.hs src/*.hs -o $(NAME)

.PHONY: all clean fclean re
