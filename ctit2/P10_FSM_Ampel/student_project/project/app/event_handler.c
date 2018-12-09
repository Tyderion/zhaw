/* ----------------------------------------------------------------------------
 * --  _____       ______  _____                                              -
 * -- |_   _|     |  ____|/ ____|                                             -
 * --   | |  _ __ | |__  | (___    Institute of Embedded Systems              -
 * --   | | | '_ \|  __|  \___ \   Zurich University of                       -
 * --  _| |_| | | | |____ ____) |  Applied Sciences                           -
 * -- |_____|_| |_|______|_____/   8401 Winterthur, Switzerland               -
 * ----------------------------------------------------------------------------
 * --
 * -- Description:  Implementation of module event_handler.
 * --
 * -- $Id: event_handler.c 3754 2016-11-01 15:06:13Z kesr $
 * ------------------------------------------------------------------------- */

/* standard includes */
#include <stdint.h>
#include <reg_stm32f4xx.h>

/* user includes */
#include "event_handler.h"
#include "timer.h"
#include "hal_gpio.h"
#include "hal_rcc.h"


/* -- Macros
 * ------------------------------------------------------------------------- */

#define PINS_PORT_B (0x3f)


/* -- Macros to mask input ports
 * ------------------------------------------------------------------------- */

#define MASK_CAR_W  (0x01)
#define MASK_CAR_S  (0x02)
#define MASK_CAR_E1 (0x04)
#define MASK_CAR_E2 (0x08)
#define MASK_PED_W  (0x10)
#define MASK_PED_E  (0x20)


/* Public function definitions
 * ------------------------------------------------------------------------- */

/*
 * See header file
 */
void eh_init(void)
{
    hal_gpio_input_t gpio_init;

    /* Enable peripheral */
    GPIOB_ENABLE();

    /* Configure input port (GPIO B) */
    //   gpio_init.pins = PINS_PORT_B;
    gpio_init.pupd = HAL_GPIO_PUPD_DOWN;

    hal_gpio_init_input(GPIOB, gpio_init);
}


/*
 * See header file
 */
event_t eh_get_event(void)
{
    event_t event = NO_EVENT;

    static uint8_t port_value_old = 0xff;
    uint8_t port_value;
    uint8_t edge_positive;

    static uint16_t timer_old = 0u;
    uint16_t timer;

    /* update old timer before decrementing! */
    /* bugfix from when it was done afterwards */
    timer_old = timer_read();

    /* Execute a timer tick at each execution of get_event */
    timer_decrement();

    /* Read the input port */
    port_value = (uint8_t)hal_gpio_input_read(GPIOB);

    /* Detect edges: Set the corresponding bit for each detected edge */
    edge_positive = ~port_value_old & port_value;

    /* Get the current timer value */
    timer = timer_read();

    /// STUDENTS: To be programmed
    if (edge_positive & MASK_CAR_W) {
        event = EV_CAR_W;
    } else if (edge_positive & MASK_CAR_S) {
        event = EV_CAR_S;
    }else if (edge_positive & MASK_CAR_E1) {
        event = EV_CAR_E1;
    }else if (edge_positive & MASK_CAR_E1) {
        event = EV_CAR_E1;
    }else if (edge_positive & MASK_CAR_E2) {
        event = EV_CAR_E2;
    }else if (edge_positive & MASK_PED_W) {
        event = EV_PED_W;
    }else if (edge_positive & MASK_PED_E) {
        event = EV_PED_E;
    }
		
		if (timer == 0 && timer_old != 0) {
			event = TIME_OUT;
		}


    /// END: To be programmed

    /* Update port_state_old */
    port_value_old = port_value;

    return event;
}
