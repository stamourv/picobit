all: picobit-vm

picobit-vm: picobit-vm.o gc.o bignums.o debug.o primitives.o dispatch.o
	cc $(CFLAGS) -o picobit-vm picobit-vm.o gc.o bignums.o debug.o primitives.o dispatch.o

.c.o:
	$(CC) $(CFLAGS) -O -c $*.c

mcc18: picoboard2.c picobit-vm.c gc.c bignums.c debug.c primitives.c dispatch.c picobit-vm.h
	wine ~/.wine/drive_c/MCC18/bin/mcc18.exe -DMCC18 -DLESS_MACROS -I=C:/MCC18/h -p=18f4550 picoboard2.c
	wine ~/.wine/drive_c/MCC18/bin/mplink.exe -lC:/MCC18/lib -mpicoboard2.map picoboard2.lkr picoboard2.o -o picoboard2.hex

hi_tech_c: picobit-vm.c gc.c bignums.c debug.c primitives.c dispatch.c picobit-vm.h
	picc18 -DHI_TECH_C -DLESS_MACROS --chip=18F4550 picobit-vm.c

clean:
	rm -f *.o *~ picobit-vm
