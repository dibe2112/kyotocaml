export OCAMLMAKEFILE = ../OCamlMakefile
export MAKEFILEINSTALL = ./Makefile.install

C_FILES = ocinterf-util.c kcstring.c kcdbcallback.c kcdb.c kccur.c kcidx.c

CFLAGS := -Wall -O3 -fPIC -DNDEBUG $(CFLAGSADD)
#CFLAGS := -Wall -O1 -fPIC -DDEBUG $(CFLAGSADD)
export CFLAGS

THREADS = no

DOC_DIR = ../doc
export DOC_DIR

define PROJ_kyotocaml
  CLIBS = kyotocabinet
  SOURCES = $(C_FILES) kyotocaml.ml kyotocaml.mli
  RESULT = kyotocaml
endef
export PROJ_kyotocaml

define PROJ_kcdbm
  LIBDIRS = ./  # neccessary to build kcdbm.top
  CLIBS = kyotocaml_stubs kyotocabinet   # order crucial!
  SOURCES = kcdbm.ml kcdbm.mli
  RESULT = kcdbm
endef
export PROJ_kcdbm

ifndef SUBPROJS
  export SUBPROJS = kyotocaml kcdbm
endif

# Default target to use
all: bcl ncl top
all-but-top: bcl ncl
top: top
%:
	@$(MAKE) -f $(OCAMLMAKEFILE) subprojs SUBTARGET=$@

.PHONY:
install:
	@$(MAKE) -f $(MAKEFILEINSTALL) install-kc
	@$(MAKE) -f $(MAKEFILEINSTALL) install-dbm

.PHONY:
uninstall:
	@$(MAKE) -f $(MAKEFILEINSTALL) uninstall-dbm
	@$(MAKE) -f $(MAKEFILEINSTALL) uninstall-kc

