NAME    := chicken-chicken-test
DEPS    := test

CSI     ?= csi
INSTALL ?= chicken-install

all: deps test

test: chicken-chicken
	$(CSI) spec.scm

deps:
	$(INSTALL) $(DEPS)

chicken-chicken:
	cp ../chicken-chicken .

.PHONY: deps test
