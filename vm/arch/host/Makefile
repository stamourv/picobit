arch-y += entry.c primitives.c

load-address := \#x8000

arch-all: picobit-vm

picobit-vm: $(obj-y) .config
	cc $(CFLAGS) -g -o $@ $(obj-y)
