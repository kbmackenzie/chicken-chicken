NAME    	:= chicken-chicken

CSC     	?= csc
INSTALL 	?= chicken-install
DEPS      := srfi-1 srfi-13 monad

COMPILER  := chicken-chicken.compiler
VM        := chicken-chicken.vm

JS_DIR 		:= js
VM_SOURCE := $(JS_DIR)/vm.min.js

PREFIX    ?= $(HOME)/.local/bin

all: deps build
build: $(NAME)

$(NAME): $(COMPILER).o
	$(CSC) -static -o $@ $^ -uses $(COMPILER) src/main.scm

$(COMPILER).o: $(VM).o src/compiler.scm
	$(CSC) -static -c -J $^ -unit $(COMPILER) -o $@ -uses $(VM)

$(VM).o: src/vm.scm
	$(CSC) -static -c -J $^ -unit $(VM) -o $@

src/vm.scm: $(VM_SOURCE)
	echo '(module (chicken-chicken vm) (vm)' > $@
	echo '(import scheme)'   >> $@
	echo '(define vm #<<END' >> $@
	cat $<     >> $@
	echo 'END' >> $@
	echo '))'  >> $@

$(VM_SOURCE):
	cd $(JS_DIR) && npm install && npm run build

deps:
	chicken-install $(DEPS)

install: build
	chmod +x $(NAME)
	cp $(NAME) $(PREFIX)/$(NAME)

uninstall:
	[ -f $(PREFIX)/$(NAME) ] && rm $(PREFIX)/$(NAME)

clean:
	rm -f *.o *.import.scm *.link

fclean: clean
	rm -f $(VM_SOURCE) $(NAME)

.PHONY: build deps install uninstall clean fclean
