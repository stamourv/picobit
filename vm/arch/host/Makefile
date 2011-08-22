arch-y += entry.c primitives.c

arch-all: picobit-vm

picobit-vm: $(obj-y)
	cc $(CFLAGS) -o $@ $^
