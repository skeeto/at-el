.POSIX:
EMACS = emacs

elc = @.elc \
    @-mixins.elc \
    lib/@heap.elc \
    lib/@queue.elc \
    lib/@stack.elc \
    lib/@vector.elc
test = @-tests.elc \
    lib/@heap-tests.elc \
    lib/@queue-tests.elc \
    lib/@stack-tests.elc \
    lib/@vector-tests.elc

compile: $(elc)

check: test
test: $(test)
	$(EMACS) -batch -Q -L . -L lib/ \
	    -l @-tests.elc \
	    -l lib/@heap-tests.elc \
	    -l lib/@queue-tests.elc \
	    -l lib/@stack-tests.elc \
	    -l lib/@vector-tests.elc \
	    -f ert-run-tests-batch

@-mixins.elc: @-mixins.el @.elc
lib/@heap.elc: lib/@heap.el lib/@vector.elc @.elc
lib/@queue.elc: lib/@queue.el @.elc
lib/@stack.elc: lib/@stack.el @.elc
lib/@vector.elc: lib/@vector.el @.elc
@-tests.elc: @-tests.el @.elc
lib/@heap-tests.elc: lib/@heap-tests.el lib/@heap.elc
lib/@queue-tests.elc: lib/@queue-tests.el lib/@queue.elc lib/@stack.elc
lib/@stack-tests.elc: lib/@stack-tests.el lib/@stack.elc
lib/@vector-tests.elc: lib/@vector-tests.el lib/@vector.elc

clean:
	rm -f $(elc) $(test)

.SUFFIXES: .el .elc
.el.elc:
	$(EMACS) -batch -Q -L . -L lib/ -f batch-byte-compile $<
