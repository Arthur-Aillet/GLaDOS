##
## EPITECH PROJECT, 2023
## Makefile
## File description:
## GLaDOS Makefile
##

NAME			=	glados-bs

all: $(NAME)

$(NAME):
	ghc src/*.hs -o $(NAME)

clean:
	rm src/*.o
	rm src/*.hi

fclean: clean
	rm -f $(NAME)

re: fclean all

quick:

.PHONY: all clean fclean re
