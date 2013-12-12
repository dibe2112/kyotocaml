export OCAMLMAKEFILE = ../OCamlMakefile

INCDIRS = +oUnit ../src 
LIBS = unix oUnit kyotocaml
BCSUFFIX = .byte
NCSUFFIX = .nat
export INCDIRS
export LIBS
export BCSUFFIX
export NCSUFFIX

define PROJ_test1
  SOURCES = test1.ml
  RESULT = test1
endef 
export PROJ_test1

define PROJ_test2o
  SOURCES = test2o.ml
  RESULT = test2o
endef 
export PROJ_test2o

define PROJ_test2x
  SOURCES = test2x.ml
  RESULT = test2x
endef 
export PROJ_test2x

define PROJ_test3o
  SOURCES = test3o.ml
  RESULT = test3o
endef 
export PROJ_test3o

define PROJ_test3x
  SOURCES = test3x.ml
  RESULT = test3x
endef 
export PROJ_test3x

define PROJ_test4o
  SOURCES = test4o.ml
  RESULT = test4o
endef 
export PROJ_test4o

define PROJ_test4x
  SOURCES = test4x.ml
  RESULT = test4x
endef 
export PROJ_test4x

define PROJ_test5
  SOURCES = test5.ml
  RESULT = test5
endef 
export PROJ_test5

define PROJ_test6
  SOURCES = test6.ml
  RESULT = test6
endef 
export PROJ_test6

ifndef SUBPROJS
  export SUBPROJS = \
   test1 \
   test2o test2x \
   test3o test3x \
   test4o test4x \
   test5 \
   test6
endif


all: bc nc

.PHONY: clear
clear:
	rm -f -R *.kc[df] *.dump *.log
	rm -f *.kc[htx]

.PHONY: cleandata
cleandata: clear

# Catch-all target will be applied to all subprojects automatically
%:
	@$(MAKE) -f $(OCAMLMAKEFILE) subprojs SUBTARGET=$@

