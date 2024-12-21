NAME    := chicken-chicken-test
DEPS    := test

CSI     ?= csi
INSTALL ?= chicken-install

all: deps test

test:
	$(CSI) spec.scm

deps:
	$(INSTALL) $(DEPS)

.PHONY: deps test
