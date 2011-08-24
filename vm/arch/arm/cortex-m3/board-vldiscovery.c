#include <picobit.h>
#include <dispatch.h>
#include <arch/stm32/rcc.h>
#include <arch/stm32/gpio.h>

void halt_with_error ()
{
	GPIOC->ODR |= BIT(8);

	while(1);
}

void main ()
{
	RCC->APB2ENR |= IOPCEN;
	GPIOC->CRH = 0x44444411;

	interpreter();
}