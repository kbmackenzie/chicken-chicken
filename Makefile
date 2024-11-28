NAME    	:= chicken-chicken

CSC     	?= csc
INSTALL 	?= chicken-install
DEPS      := srfi-1 srfi-13 monad

COMPILER  := chicken-to-js
VM        := chicken-vm

JS_DIR 		:= js
VM_SOURCE := $(JS_DIR)/vm.min.js

all: deps build
build: $(NAME)

$(NAME): $(COMPILER).o
	$(CSC) -static -o $@ $^ -uses $(COMPILER) src/main.scm

$(COMPILER).o: $(VM).o src/$(COMPILER).scm
	$(CSC) -static -c -J $^ -unit $* -o $@ -uses $(VM)

$(VM).o: src/$(VM).scm
	$(CSC) -static -c -J $^ -unit $* -o $@

src/$(VM).scm: $(VM_SOURCE)
	echo '(module' $(VM) '(vm)' > $@
	echo '(import scheme)'   >> $@
	echo '(define vm #<<END' >> $@
	cat $<     >> $@
	echo 'END' >> $@
	echo '))'  >> $@

$(VM_SOURCE):
	cd $(JS_DIR) && npm install && npm run build

deps:
	chicken-install $(DEPS)

clean:
	rm -f *.o *.import.scm *.link

fclean: clean
	rm -f $(VM_SOURCE) $(NAME)

.PHONY: build deps clean fclean
