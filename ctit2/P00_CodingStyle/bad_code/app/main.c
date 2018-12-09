/* ----------------------------------------------------------------------------
 * --  _____       ______  _____                                              -
 * -- |_   _|     |  ____|/ ____|                                             -
 * --   | |  _ __ | |__  | (___    Institute of Embedded Systems              -
 * --   | | | '_ \|  __|  \___ \   Zurich University of                       -
 * --  _| |_| | | | |____ ____) |  Applied Sciences                           -
 * -- |_____|_| |_|______|_____/   8401 Winterthur, Switzerland               -
 * ------------------------------------------------------------------------- */
/**
 *  \brief  Peripheral Test.
 *
 *  Code NOT following coding style guidelines.
 *
 *  $Id: main.c 2949 2016-02-10 12:41:24Z feur $
 * ------------------------------------------------------------------------- */

/* User includes */
#include <stdint.h>
#include <reg_STM32F4xx.h>
#include <hal_rcc.h>
#include <hal_gpio.h>


/* -- Local function declarations
 * ------------------------------------------------------------------------- */

void wait(uint32_t value);
void make_button_work(void);
extern void init_FSM(void);
extern uint8_t get_next_state(void);
extern void state_machine(uint8_t state);


/* -- M A I N
 * ------------------------------------------------------------------------- */

int main(void)
{
    /* Variable for state */
    char state = 0u;

    /* Initialize Button and FSM */
    make_button_work();
    init_FSM();

    /* Do forever */
    while (1) {
        /* Read User Button */
        if (hal_gpio_input_read(GPIOA) & 0x1) {
            /* Wait */
            wait(0x3fffff);
            /* Get next state */
            state = get_next_state();
        }
        /* Process next state */
        state_machine(state);
    }
}


/* -- Local function definitions
 * ------------------------------------------------------------------------- */

/**
 *  \brief  Initialise User Button on PA.0
 */
void make_button_work(void)
{
    hal_gpio_input_t gpio_init;

    /* Enable GPIOA clock */
    GPIOA_ENABLE();

    /* Configure GPIOA pin 0 as input */
    gpio_init.pins = 0x1;
    gpio_init.pupd = HAL_GPIO_PUPD_DOWN;
    hal_gpio_init_input(GPIOA, gpio_init);
}

/**
 *  \brief  Wait for supplied value of for loops.
 *  \param  value : Number of loops to wait.
 */
void wait(uint32_t value)
{
    for (; value > 0; value--);
}
