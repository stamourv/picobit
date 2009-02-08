# TODO make libpcap an option

all: picobit-vm

picobit-vm: picobit-vm.o gc.o bignums.o debug.o primitives.o
	cc -o picobit-vm picobit-vm.o gc.o bignums.o debug.o primitives.o -lpcap

.c.o:
	$(CC) -O -c $*.c

# TODO instead, maybe have these targets change a variable (CFLAGS ?) and call the all target ?
debug:
	cc -O -c -g -DDEBUG picobit-vm.c
	cc -O -c -g -DDEBUG gc.c
	cc -O -c -g -DDEBUG bignums.c
	cc -O -c -g -DDEBUG debug.c
	cc -O -c -g -DDEBUG primitives.c
	make picobit-vm

debug-gc:
	cc -O -c -g -DDEBUG -DDEBUG_GC picobit-vm.c
	cc -O -c -g -DDEBUG -DDEBUG_GC gc.c
	cc -O -c -g -DDEBUG -DDEBUG_GC bignums.c
	cc -O -c -g -DDEBUG -DDEBUG_GC debug.c
	cc -O -c -g -DDEBUG -DDEBUG_GC primitives.c
	make picobit-vm

clean:
	rm -f *.o *~ picobit-vm
