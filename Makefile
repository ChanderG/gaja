all: naga hasti gaja

bindir:
	mkdir -p bin

hasti: bindir
	sbcl --load bytecode.lisp --load compiler.lisp --eval "(sb-ext:save-lisp-and-die \"bin/hasti\" :executable t :toplevel 'hasti)"

gaja: bindir
	sbcl --load bytecode.lisp --load vm.lisp --eval "(sb-ext:save-lisp-and-die \"bin/gaja\" :executable t :toplevel 'gaja)"

naga: bindir
	make -C naga/
	cp naga/parser.so ./bin
	cp naga/parser.lua ./bin/naga

clean:
	rm -rf bin
	make -C naga/ clean
	rm -f out.hasti out.gaja
