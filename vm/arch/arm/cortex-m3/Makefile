ifdef CONFIG_ARM_CPU_STM32F100RB
ldscript := stm32f100rb.ld
endif

CFLAGS += -mcpu=cortex-m3 -mthumb -g -fno-hosted -nostdlib \
          -L$(arch)/lib -T$(ldscript)

arch-y += init.s entry.c primitives.c

arch-all: picobit-vm.hex

picobit-vm.hex: picobit-vm.elf
	$(CROSS)objcopy -O ihex $^ $@

picobit-vm.elf: $(obj-y)
	$(CROSS)cc $(CFLAGS) -o $@ $^
