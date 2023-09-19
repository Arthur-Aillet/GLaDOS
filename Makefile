##
## EPITECH PROJECT, 2023
## Makefile
## File description:
## Wolfram Malefile
##

BIN_PATH		:=	$(shell stack path --local-install-root)
NAME			=	glados

all:
		stack build
		cp $(BIN_PATH)/bin/$(NAME)-exe ./$(NAME)

clean:
	stack clean

fclean: clean
	rm -f $(NAME)

re: fclean all

quick:
	ghc app/*.hs src/*.hs -o $(NAME)

.PHONY: all clean fclean re
