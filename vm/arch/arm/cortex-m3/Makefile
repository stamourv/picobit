ifdef CONFIG_ARM_CPU_STM32F100RB
ldscript := stm32f100rb.ld
endif

CFLAGS += -mcpu=cortex-m3 -mthumb -g -fno-hosted -nostdlib \
          -L$(arch)/lib -T$(ldscript) -Wl,-warn-common

arch-y += init.s
arch-$(CONFIG_ARM_BOARD_STM32_VLDISCOVERY) += board-vldiscovery.c

arch-all: picobit.hex

picobit.hex: picobit.elf
	$(CROSS)objcopy -O ihex $^ $@

picobit.elf: $(obj-y) .config
	$(CROSS)cc $(CFLAGS) -o $@ $(obj-y)
