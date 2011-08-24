#ifndef _STM32_AFIO_H_
#define _STM32_AFIO_H_

#include <arch/stm32/bits.h>

struct regs_AFIO {
	reg EVCR;
#define		EVOE		BIT(7)
#define		R_PORT(v)	R_BITS(v, 4, 6)
#define		W_PORT(v, x)	W_BITS(v, x, 4, 6)
#define		PORT(x)		BITS(v, 4, 6)
#define		R_PIN(v)	R_BITS(v, 0, 3)
#define		W_PIN(v, x)	W_BITS(v, x, 0, 3)
#define		PIN(x)		BITS(x, 0, 3)
	reg MAPR;
#define		R_SWJ_CFG(v)		R_BITS(v, 24, 26)
#define		W_SWJ_CFG(v, x)		W_BITS(v, x, 24, 26)
#define		SWJ_CFG(x)		BITS(x, 24, 26)
#define			SWJ_CFG_SWJ		0
#define			SWJ_CFG_SWJ_NRST	1
#define			SWJ_CFG_SW		2
#define			SWJ_CFG_NONE		4
#define		TIM5CH4_IREMAP		BIT(16)
#define		PD01_REMAP		BIT(15)
#define		TIM4_REMAP		BIT(12)
#define		R_TIM3_REMAP(v)		R_BITS(v, 10, 11)
#define		W_TIM3_REMAP(v, x)	W_BITS(v, x, 10, 11)
#define		TIM3_REMAP(x)		BITS(x, 10, 11)
#define		R_TIM2_REMAP(v)		R_BITS(v, 8, 9)
#define		W_TIM2_REMAP(v, x)	W_BITS(v, x, 8, 9)
#define		TIM2_REMAP(x)		BITS(x, 8, 9)
#define		R_TIM1_REMAP(v)		R_BITS(v, 6, 7)
#define		W_TIM1_REMAP(v, x)	W_BITS(v, x, 6, 7)
#define		TIM1_REMAP(x)		BITS(x, 6, 7)
#define		R_USART3_REMAP(v)	R_BITS(v, 4, 5)
#define		W_USART3_REMAP(v, x)	W_BITS(v, x, 4, 5)
#define		USART3_REMAP(x)		BITS(x, 4, 5)
#define		USART2_REMAP		BIT(3)
#define		USART1_REMAP		BIT(2)
#define		I2C1_REMAP		BIT(1)
#define		SPI1_REMAP		BIT(0)
	reg EXTICR1;
	reg EXTICR2;
	reg EXTICR3;
	reg EXTICR4;
	reg MAPR2;
#define		MISC_REMAP		BIT(13)
#define		TIM12_REMAP		BIT(12)
#define		TIM76_DAC_DMA_REMAP	BIT(11)
#define		FSMC_NADV		BIT(10)
#define		TIM14_REMAP		BIT(9)
#define		TIM13_REMAP		BIT(8)
#define		TIM1_DMA_REMAP		BIT(4)
#define		CEC_REMAP		BIT(3)
#define		TIM17_REMAP		BIT(2)
#define		TIM16_REMAP		BIT(1)
#define		TIM15_REMAP		BIT(0)
};

CONFIG_AREA(regs_AFIO, AFIO, 0x40010000);

#endif