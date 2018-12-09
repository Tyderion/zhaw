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
 * -- $Id: event_handler.c 3241 2016-04-20 06:18:33Z feur $
 * ------------------------------------------------------------------------- */

/* standard includes */
#include <stdint.h>
#include <reg_stm32f4xx.h>
#include <reg_ctboard.h>

/* user includes */
#include "event_handler.h"
#include "timer.h"


/* -- Macros for accessing CT Board
 * ------------------------------------------------------------------------- */

#define PORT_INPUT (CT_GPIO->IN.BYTE.P1)

/* -- Macros to mask input ports
 * ------------------------------------------------------------------------- */

#define MASK_FLOATER_HIGH (0x01)
#define MASK_FLOATER_LOW  (0x04)
#define MASK_BUTTON_STOP  (0x08)
#define MASK_BUTTON_SPIN  (0x10)
#define MASK_BUTTON_WASH  (0x20)
#define MASK_THERMOSTAT   (0x40)
#define MASK_DOOR_OPEN    (0x80)


/* Public function definitions
 * ------------------------------------------------------------------------- */

/*
 * See header file
 */
event_t eh_get_event(void)
{
    event_t event = NO_EVENT;

    static uint8_t port_value_old = 0xff;
    uint8_t port_value;
    uint8_t edge_positive;
    uint8_t edge_negative;

    static uint16_t timer_old = 0u;
    uint16_t timer;

    /* update old timer before decrementing! */
    /* bugfix from when it was done afterwards */
    timer_old = timer_read();
    
    /* Execute a timer tick at each execution of get_event */
    timer_decrement();

    /* Read the input port */
    port_value = PORT_INPUT;

    /* Detect edges: Set the corresponding bit for each detected edge */
    edge_positive = ~port_value_old & port_value;
    edge_negative = ~(~port_value_old | port_value);

    /* Get the current timer value */
    timer = timer_read();

    /// STUDENTS: To be programmed
    if (timer_old != 0 && timer_read == 0) {
        event = TIME_OUT;
    } else if (edge_negative & MASK_DOOR_OPEN) {
        event = DOOR_CLOSED   
    } else if (edge_positive & MASK_DOOR_OPEN) {
        event = DOOR_OPEN;
    }
    



    /// END: To be programmed

    /* Update port_state_old */
    port_value_old = port_value;

    return event;
}
