##
## Makefile for Makefile in /home/lopez_t/ocaml_a-maze-ing
## 
## Made by Thibaut Lopez
## Login   <lopez_t@epitech.net>
## 
## Started on  Thu Apr 30 00:19:29 2015 Thibaut Lopez
## Last update Fri May  1 12:39:51 2015 Thibaut Lopez
##

export OCAMLMAKEFILE = OCamlMakefile

export CAMLOPT	=	ocamlc -w Aelz

export LIBS     = 	bigarray sdl sdlmixer

export INCDIRS 	= 	+sdl

define PROJ_step1

  SOURCES	=	Maze.ml		\
			step1.ml

  RESULT	=	step1
endef
export PROJ_step1

define PROJ_step2

  SOURCES	=	Maze.ml		\
			SDL.mli		\
			SDL.ml		\
			step2.ml

  RESULT	=	step2
endef
export PROJ_step2

define PROJ_step3

  SOURCES	=	Maze.ml		\
			SDL.mli		\
			SDL.ml		\
			Solver.mli	\
			Solver.ml	\
			step3.ml

  RESULT	=	step3
endef
export PROJ_step3

export SUBPROJS = step1 step2 step3

all: bc

%:
	@$(MAKE) -f $(OCAMLMAKEFILE) subprojs SUBTARGET=$@
