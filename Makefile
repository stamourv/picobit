all: compiler vm

# compiler can always be run, raco make will figure the rest out
.PHONY: compiler vm

compiler: vm
	raco make compiler/picobit.rkt

vm:
	cd vm && make
	cp vm/picobit-vm .

clean:
	cd vm && make clean

test: compiler vm
	raco make run-tests.rkt
	racket run-tests.rkt
