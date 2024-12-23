BIN     := chicken-chicken
DEPS    := test

CSI     ?= csi
INSTALL ?= chicken-install

all: deps test

test: $(BIN)
	$(CSI) -s spec.scm

deps:
	$(INSTALL) $(DEPS)

$(BIN): ../$(BIN)
	cp $< .

clean:
	rm -f $(BIN)

.PHONY: deps test clean
