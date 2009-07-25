# TODO make libpcap an option, can use -DNETWORKING

all: picobit-vm

picobit-vm: picobit-vm.o gc.o bignums.o debug.o primitives.o dispatch.o
	cc -o picobit-vm picobit-vm.o gc.o bignums.o debug.o primitives.o dispatch.o # -lpcap

.c.o:
	$(CC) -O -c $*.c

# TODO instead, maybe have these targets change a variable (CFLAGS ?) and call the all target ?
debug:
	cc -O -c -g -DDEBUG picobit-vm.c
	cc -O -c -g -DDEBUG gc.c
	cc -O -c -g -DDEBUG bignums.c
	cc -O -c -g -DDEBUG debug.c
	cc -O -c -g -DDEBUG primitives.c
	cc -O -c -g -DDEBUG dispatch.c
	make picobit-vm

debug-gc:
	cc -O -c -g -DDEBUG -DDEBUG_GC picobit-vm.c
	cc -O -c -g -DDEBUG -DDEBUG_GC gc.c
	cc -O -c -g -DDEBUG -DDEBUG_GC bignums.c
	cc -O -c -g -DDEBUG -DDEBUG_GC debug.c
	cc -O -c -g -DDEBUG -DDEBUG_GC primitives.c
	cc -O -c -g -DDEBUG -DDEBUG_GC dispatch.c
	make picobit-vm

mcc: picoboard2.c picobit-vm.c gc.c bignums.c debug.c primitives.c dispatch.c picobit-vm.h
	cpp -DPICOBOARD2 picobit-vm.c | /bin/grep -v '^#' > picobit-vm.mcc.c
	wine ~/.wine/drive_c/MCC18/bin/mcc18.exe -I=C:/MCC18/h -p=18f4550 picoboard2.c
	wine ~/.wine/drive_c/MCC18/bin/mplink.exe -lC:/MCC18/lib -mpicoboard2.map picoboard2.lkr picoboard2.o -o picoboard2.hex

hi_tech_c: picobit-vm.c gc.c bignums.c debug.c primitives.c dispatch.c picobit-vm.h
	picc18 -DHI_TECH_C --chip=18F4550 picobit-vm.c

clean:
	rm -f *.o *~ picobit-vm
