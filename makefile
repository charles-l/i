all:
	csc i.scm

get-deps:
	sudo chicken-install irc posix posix-extras ncurses
