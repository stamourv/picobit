arch-y += entry.c primitives.c

arch-all: picobit-vm

picobit-vm: $(obj-y) .config
	cc $(CFLAGS) -g -o $@ $(obj-y)
