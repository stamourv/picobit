#ifndef _STM32_SPI_H_
#define _STM32_SPI_H_

#include <arch/stm32/bits.h>

struct regs_SPI {
	reg CR1;
#define		SPI_BIDIMODE	BIT(15)
#define		SPI_BIDIOE	BIT(14)
#define		SPI_CRCEN	BIT(13)
#define		SPI_CRCNEXT	BIT(12)
#define		SPI_DFF		BIT(11)
#define		SPI_RXONLY	BIT(10)
#define		SPI_SSM		BIT(9)
#define		SPI_SSI		BIT(8)
#define		SPI_LSBFIRST	BIT(7)
#define		SPI_SPE		BIT(6)
#define		SPI_R_BR(v)	R_BITS(v, 3, 5)
#define		SPI_W_BR(v, x)	W_BITS(v, x, 3, 5)
#define		SPI_BR(x)	BITS(x, 3, 5)
#define		SPI_MSTR	BIT(2)
#define		SPI_CPOL	BIT(1)
#define		SPI_CPHA	BIT(0)
	reg CR2;
#define		SPI_TXEIE	BIT(7)
#define		SPI_RXNEIE	BIT(6)
#define		SPI_ERRIE	BIT(5)
#define		SPI_SSOE	BIT(2)
#define		SPI_TXDMAEN	BIT(1)
#define		SPI_RXDMAEN	BIT(0)
	reg SR;
#define		SPI_BSY		BIT(7)
#define		SPI_OVR		BIT(6)
#define		SPI_MODF	BIT(5)
#define		SPI_CRCERR	BIT(4)
#define		SPI_TXE		BIT(1)
#define		SPI_RXNE	BIT(0)
	reg DR;
	reg CRCPR;
	reg RXCRCR;
	reg TXCRCR;
};

CONFIG_AREA(regs_SPI, SPI1, 0x40013000);
CONFIG_AREA(regs_SPI, SPI2, 0x40003800);
CONFIG_AREA(regs_SPI, SPI3, 0x40003C00);

#endif