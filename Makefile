# This Makefile is part of the kyotocaml project

VERSION = 0.0.0

all: lib top examples tests

all-without-tests: lib top examples

lib:
	@$(MAKE) -C ./src all

top: lib
	@$(MAKE) -C ./src top

examples: lib
	@$(MAKE) -C ./examples all

tests: lib
	@$(MAKE) -C ./tests all

install:
	@$(MAKE) -C ./src install 

uninstall:
	@$(MAKE) -C ./src uninstall

libinstall rawinstall:
	@printf "\nTarget $@ not supported. Use 'install' instead!\n\n"

libuninstall rawuninstall:
	@printf "\nTarget $@ not supported. Use 'uninstall' instead!\n\n"

doc htdoc ladoc psdoc pdfdoc clean-doc:
	@$(MAKE) -C ./src $@

clean clean-all:
	@$(MAKE) -C ./src $@
	@$(MAKE) -C ./examples $@
	@$(MAKE) -C ./tests $@

clear:
	@$(MAKE) -C ./tests $@
	@$(MAKE) -C ./examples $@


tar.gz:
	tar -czf kyotocaml-$(VERSION).tar.gz \
		*Makefile Makefile.old README.md ChangeLog LICENSE \
		src/*.c src/*.h src/*.ml src/*.mli src/Makefile src/Makefile.install src/Makefile.old \
		tests/*.ml tests/Makefile tests/Makefile.old tests/README.md \
		examples/*.ml examples/Makefile examples/Makefile.old

.PHONY: all all-without-tests lib top examples test \
	doc htdoc ladoc psdoc pdfdoc \
	clean clean-all clean-doc clear tar.gz
