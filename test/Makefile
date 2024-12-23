DEPS    := test
CSI     ?= csi
INSTALL ?= chicken-install

all: deps test

test: chicken-chicken
	$(CSI) -s spec.scm

deps:
	$(INSTALL) $(DEPS)

chicken-chicken: ../chicken-chicken
	cp $< .

.PHONY: deps test
