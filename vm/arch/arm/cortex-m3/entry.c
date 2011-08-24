#include <arch/stm32/rcc.h>
#include <arch/stm32/gpio.h>

void halt_with_error ()
{
	while(1);
}

void main () {
	RCC->APB2ENR |= IOPCEN;
	GPIOC->CRH = 0x44444411;
	GPIOC->ODR = BIT(8);

	while(1);
}