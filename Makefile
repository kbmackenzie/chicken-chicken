NAME   := chicken-to-js

VM_JS  := js/vm.min.js
VM_SCM := src/vm.scm

all: build

build: $(VM_SCM)
	henhen build --verbose

$(VM_SCM): $(VM_JS)
	echo '(define vm #<<END' > $@
	cat $< >> $@
	echo 'END' >> $@
	echo ')'   >> $@

$(VM_JS):
	cd ./js && npm install && npm run build

clean:
	rm -f $(VM_SCM) $(VM_JS)

.PHONY: clean build
