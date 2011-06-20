all:
	cd vm && make
	cp vm/picobit-vm .

clean:
	cd vm && make clean
