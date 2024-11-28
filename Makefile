NAME   := chicken-to-js
VM_JS  := js/vm.min.js
VM_SCM := src/vm.scm

all: $(VM_SCM)

$(VM_SCM): $(VM_JS)
	echo '(define vm #<<END' > $@
	cat $< >> $@
	echo 'END' >> $@
	echo ')'   >> $@

$(VM_JS):
	cd ./js && npm install && npm run build

clean:
	rm -f $(VM_SCM) $(VM_JS)

.PHONY: clean
