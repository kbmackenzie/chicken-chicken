NAME      := chicken-chicken

CSC       ?= csc
INSTALL   ?= chicken-install
DEPS      := srfi-1 srfi-13 monad getopt-long

COMPILER  := chicken-chicken.compiler
VM        := chicken-chicken.vm
UTILS     := chicken-chicken.utils

JS_DIR    := js
VM_SOURCE := $(JS_DIR)/vm.min.js

PREFIX    ?= $(HOME)/.local/bin

all: deps build
build: $(NAME)

$(NAME): $(COMPILER).o $(UTILS).o src/main.scm
	$(CSC) -static -o $@ $^ -uses $(COMPILER) -uses $(UTILS)

$(COMPILER).o: $(VM).o $(UTILS).o src/compiler.scm
	$(CSC) -static -c -J $^ -unit $(COMPILER) -o $@ -uses $(VM) -uses $(UTILS)

$(VM).o: src/vm.scm
	$(CSC) -static -c -J $^ -unit $(VM) -o $@

$(UTILS).o: src/utils.scm
	$(CSC) -static -c -J $^ -unit $(UTILS) -o $@

src/vm.scm: $(VM_SOURCE)
	echo '(module (chicken-chicken vm) (vm)' > $@
	echo '(import scheme)'   >> $@
	echo '(define vm #<<END' >> $@
	cat $<     >> $@
	echo 'END' >> $@
	echo '))'  >> $@

$(VM_SOURCE): $(JS_DIR)/vm.js
	cd $(JS_DIR) && npm install && npm run build

deps:
	$(INSTALL) $(DEPS)

install: deps build
	chmod +x $(NAME)
	cp $(NAME) $(PREFIX)/$(NAME)

uninstall:
	[ -f $(PREFIX)/$(NAME) ] && rm $(PREFIX)/$(NAME)

clean:
	rm -f *.o *.import.scm *.link

fclean: clean
	rm -f $(VM_SOURCE) $(NAME)

.PHONY: build deps install uninstall clean fclean
