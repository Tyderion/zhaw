/* ----------------------------------------------------------------------------
 * --  _____       ______  _____                                              -
 * -- |_   _|     |  ____|/ ____|                                             -
 * --   | |  _ __ | |__  | (___    Institute of Embedded Systems              -
 * --   | | | '_ \|  __|  \___ \   Zurich University of                       -
 * --  _| |_| | | | |____ ____) |  Applied Sciences                           -
 * -- |_____|_| |_|______|_____/   8401 Winterthur, Switzerland               -
 * ----------------------------------------------------------------------------
 * -- $Id: hal_exti.c 2800 2015-12-09 11:59:23Z mady $
 * ------------------------------------------------------------------------- */

#include <reg_stm32f4xx.h>
#include "hal_exti.h"


/* type definitions -------------------------------------------------------- */
#define EXTI_LINE_0   (uint32_t)0x00001                // External interrupt line 0
#define EXTI0_IRQn    (uint8_t)6                       // EXTI0 is channel 6
#define EXTI_PA0_MASK (uint32_t)0xfffffffe             // Port A0 ISR mask


/* public function declarations -------------------------------------------- */

/*
 * according to description in header file
 */
void hal_exti_init(void)
{
    GPIOA->MODER = 0xa8000000;                                  // set gpioa in default state
    GPIOA->OTYPER = 0x00000000;
    GPIOA->OSPEEDR = 0x00000000;
    GPIOA->PUPDR = 0x64000000;
    GPIOA->ODR = 0x00000000;
    GPIOA->BSRR[0] = 0x00000000;
    GPIOA->LCKR = 0x00000000;
    GPIOA->AFR[0] = 0x00000000;
    GPIOA->AFR[1] = 0x00000000;

    RCC->AHBENR[0] |= (0x1 << 0);                           // start clock on GPIO A
    RCC->APBENR[1] |= (0x1 << 14);                          // Enable SYSCFG clock

    GPIOA->OSPEEDR |= 0x00000003;                                   // set PA0 in high speed mode
    GPIOA->PUPDR |= 0x00000002;                                     // Set pull-down for PA0

    SYSCFG->EXTICR[0] |= (0x0 << 0);                        // Connect PA.0 with EXTI0

    EXTI->IMR |= 0x00000001;                                // Reset Interrupt Mask Register, set PA0 as not masked
    EXTI->RTSR |= 0x00000001;                               // Trigger on rising edge

    NVIC->ISER[0] = (0x1 << EXTI0_IRQn);                    // Add IRQ vector to NVIC
}

/*
 * according to description in header file
 */
void hal_exti_deactivate(void)
{
    EXTI->IMR &= EXTI_PA0_MASK;                             // set PA0 as masked
    NVIC->ISER[0] |= (0x01 << EXTI0_IRQn);                    // Remove IRQ vector to NVIC
}

/*
 * according to description in header file
 */
void hal_exti_clear_irq_flag(void)
{
    EXTI->PR |= EXTI_LINE_0;
}

/*
 * according to description in header file
 */
hal_bool_t hal_exti_check_for_irq(void)
{
    hal_bool_t status = FALSE;

    if (EXTI->PR & EXTI_LINE_0) {
        status = TRUE;
    }

    return status;
}
