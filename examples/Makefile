export OCAMLMAKEFILE = ../OCamlMakefile

INCDIRS = ../src
LIBS = kyotocaml
BCSUFFIX = .byte
NCSUFFIX = .nat
export INCDIRS
export LIBS
export BCSUFFIX
export NCSUFFIX
 
define PROJ_exString
  SOURCES = exString.ml
  RESULT = exString
endef
export PROJ_exString

define PROJ_exHF1
  SOURCES = exHF1.ml
  RESULT = exHF1
endef
export PROJ_exHF1

define PROJ_exHF1a
  SOURCES = exHF1a.ml
  RESULT = exHF1a
endef
export PROJ_exHF1a

define PROJ_exDBM
  LIBS = $(LIBS) kcdbm 
  SOURCES = exDBM.ml
  RESULT = exDBM
endef
export PROJ_exDBM

ifndef SUBPROJS
  export SUBPROJS = exString exHF1 exHF1a exDBM
endif

# Default target to use
all:  bc

.PHONY: clear
clear:
	rm -f -R *.kc[df] *.dump
	rm -f *.kc[htx]

# Catch-all target will be applied to all subprojects automatically
%:
	@$(MAKE) -f $(OCAMLMAKEFILE) subprojs SUBTARGET=$@
