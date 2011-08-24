#ifndef _STM32_USART_H_
#define _STM32_USART_H_

#include <arch/stm32/bits.h>

struct regs_USART {
	reg SR;
#define		USART_CTS	BIT(9)
#define		USART_LBD	BIT(8)
#define		USART_TXE	BIT(7)
#define		USART_TC	BIT(6)
#define		USART_RXNE	BIT(5)
#define		USART_IDLE	BIT(4)
#define		USART_ORE	BIT(3)
#define		USART_NF	BIT(2)
#define		USART_FE	BIT(1)
#define		USART_PE	BIT(0)
	reg DR;
	reg BRR;
	reg CR1;
#define		USART_OVER8	BIT(15)
#define		USART_UE	BIT(13)
#define		USART_M		BIT(12)
#define		USART_WAKE	BIT(11)
#define		USART_PCE	BIT(10)
#define		USART_PS	BIT(9)
#define		USART_PEIE	BIT(8)
#define		USART_TXEIE	BIT(7)
#define		USART_TCIE	BIT(6)
#define		USART_RXNEIE	BIT(5)
#define		USART_IDLEIE	BIT(4)
#define		USART_TE	BIT(3)
#define		USART_RE	BIT(2)
#define		USART_RWU	BIT(1)
#define		USART_SBK	BIT(0)
	reg CR2;
#define		USART_LINEN	BIT(14)
#define		USART_R_STOP(v)		R_BITS(v, 12, 13)
#define		USART_W_STOP(v, x)	W_BITS(v, x, 12, 13)
#define		USART_STOP(x)		BITS(x, 12, 13)
#define		USART_CLKEN	BIT(11)
#define		USART_CPOL	BIT(10)
#define		USART_CPHA	BIT(9)
#define		USART_LBCL	BIT(8)
#define		USART_LBDIE	BIT(6)
#define		USART_LBDL	BIT(5)
#define		USART_R_ADD(v)		R_BITS(v, 0, 3)
#define		USART_W_ADD(v, x)	W_BITS(v, x, 0, 3)
#define		USART_ADD(x)		BITS(x, 0, 3)
	reg CR3;
#define		USART_ONEBITE	BIT(11)
#define		USART_CTSIE	BIT(10)
#define		USART_CTSE	BIT(9)
#define		USART_RTSE	BIT(8)
#define		USART_DMAT	BIT(7)
#define		USART_DMAR	BIT(6)
#define		USART_SCEN	BIT(5)
#define		USART_NACK	BIT(4)
#define		USART_HDSEL	BIT(3)
#define		USART_IRLP	BIT(2)
#define		USART_IREN	BIT(1)
#define		USART_EIE	BIT(0
	reg GPTR;
};

CONFIG_AREA(regs_USART, USART1, 0x40013800);
CONFIG_AREA(regs_USART, USART2, 0x40004400);
CONFIG_AREA(regs_USART, USART3, 0x40004800);

#endif