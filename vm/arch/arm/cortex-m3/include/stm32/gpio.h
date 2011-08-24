#ifndef _STM32_GPIO_H_
#define _STM32_GPIO_H_

#include <arch/stm32/bits.h>

struct regs_GPIO {
	reg CRL;
	reg CRH;
	reg IDR;
	reg ODR;
	reg BSRR;
	reg BRR;
	reg LCKR;
};

CONFIG_AREA(regs_GPIO, GPIOA, 0x40010800);
CONFIG_AREA(regs_GPIO, GPIOB, 0x40010C00);
CONFIG_AREA(regs_GPIO, GPIOC, 0x40011000);
CONFIG_AREA(regs_GPIO, GPIOD, 0x40011800);
CONFIG_AREA(regs_GPIO, GPIOE, 0x40011C00);

#endif