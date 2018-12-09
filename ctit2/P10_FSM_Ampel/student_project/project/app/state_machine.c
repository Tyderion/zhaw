/* ----------------------------------------------------------------------------
 * --  _____       ______  _____                                              -
 * -- |_   _|     |  ____|/ ____|                                             -
 * --   | |  _ __ | |__  | (___    Institute of Embedded Systems              -
 * --   | | | '_ \|  __|  \___ \   Zurich University of                       -
 * --  _| |_| | | | |____ ____) |  Applied Sciences                           -
 * -- |_____|_| |_|______|_____/   8401 Winterthur, Switzerland               -
 * ----------------------------------------------------------------------------
 * --
 * -- Description:  Implementation of module state_machine.
 * --
 * -- $Id: state_machine.c 3282 2016-05-01 09:35:44Z ruan $
 * ------------------------------------------------------------------------- */

/* standard includes */
#include <stdint.h>
#include <reg_stm32f4xx.h>

/* user includes */
#include "state_machine.h"
#include "action_handler.h"
#include "timer.h"


/* -- Macros used by student code
 * ------------------------------------------------------------------------- */

#define TIMER_DURATION       200u

#define TEXT_INIT            "INIT            "

#define TEXT_CAR_S           "CAR_S           "
#define TEXT_CAR_E_W         "CAR_E_W         "
#define TEXT_PED_E_W         "PED_E_W         "
#define TEXT_CAR_E_W_O       "CAR_E_W_O       "
#define TEXT_PED_E_W_O       "PED_E_W_O       "
#define TEXT_CAR_S_CAR_E_W   "CAR_S_CAR_E_W   "
#define TEXT_CAR_E_W_CAR_S   "CAR_E_W_CAR_S   "
#define TEXT_CAR_E_W_PED_E_W "CAR_E_W_PED_E_W "
#define TEXT_PED_E_W_CAR_E_W "PED_E_W_CAR_E_W "
#define TEXT_PED_E_W_CAR_S   "PED_E_W_CAR_S   "
#define TEXT_CAR_S_PED_E_W   "CAR_S_PED_E_W   "



/* Local variables
 * ------------------------------------------------------------------------- */

static state_t state = INIT;


/* Public function definitions
 * ------------------------------------------------------------------------- */

/*
 * See header file
 */
void fsm_init(void)
{
    action_handler_init();

    /* Set initial signal color */

    /// STUDENTS: To be programmed

    /// END: To be programmed

    state = INIT;
}


/*
 * See header file
 */
void fsm_handle_event(event_t event)
{
    /// STUDENTS: To be programmed
	
		// 4.1
    switch (state) {
        case INIT:
            // If in init state, do default transition
            ah_set_signal(SIGNAL_CAR_W, COLOR_GREEN);
            ah_set_signal(SIGNAL_CAR_E1, COLOR_GREEN);
            ah_set_signal(SIGNAL_PED_W, COLOR_RED);
            ah_set_signal(SIGNAL_PED_E, COLOR_RED);
            ah_lcd_write(TEXT_CAR_E_W);
            state = CAR_E_W;
        // Fall through to event handling
        case CAR_E_W:
            if (event == EV_PED_E || event == EV_PED_W) {
                ah_set_signal(SIGNAL_CAR_W, COLOR_RED);
                ah_set_signal(SIGNAL_CAR_E1, COLOR_RED);
                ah_set_signal(SIGNAL_PED_W, COLOR_GREEN);
                ah_set_signal(SIGNAL_PED_E, COLOR_GREEN);
                ah_lcd_write(TEXT_PED_E_W);
                state = PED_E_W;
            }
            break;
        case PED_E_W:
            if (event == EV_CAR_E1 || event == EV_CAR_W) {
                ah_set_signal(SIGNAL_CAR_W, COLOR_GREEN);
                ah_set_signal(SIGNAL_CAR_E1, COLOR_GREEN);
                ah_set_signal(SIGNAL_PED_W, COLOR_RED);
                ah_set_signal(SIGNAL_PED_E, COLOR_RED);
                ah_lcd_write(TEXT_CAR_E_W);
                state = CAR_E_W;
            }
            break;
        default:
            // do nothing;
    }
		
		// 4.2
		switch (state) {
        case INIT:
            // If in init state, do default transition
            ah_set_signal(SIGNAL_CAR_W, COLOR_GREEN);
            ah_set_signal(SIGNAL_CAR_E1, COLOR_GREEN);
            ah_set_signal(SIGNAL_PED_W, COLOR_RED);
            ah_set_signal(SIGNAL_PED_E, COLOR_RED);
            ah_lcd_write(TEXT_CAR_E_W);
            state = CAR_E_W;
        // Fall through to event handling
				case CAR_E_W:
            if (event == EV_PED_E || event == EV_PED_W) {
                ah_set_signal(SIGNAL_CAR_W, COLOR_YELLOW);
                ah_set_signal(SIGNAL_CAR_E1, COLOR_YELLOW);
                ah_lcd_write(TEXT_CAR_E_W_O);
                state = CAR_E_W_O;
								timer_start(TIMER_DURATION);
            }
            break;
        case PED_E_W:
            if (event == EV_CAR_E1 || event == EV_CAR_W) {
                ah_set_signal(SIGNAL_PED_W, COLOR_YELLOW);
                ah_set_signal(SIGNAL_PED_E, COLOR_YELLOW);
                ah_lcd_write(TEXT_PED_E_W_O);
                state = PED_E_W_O;
								timer_start(TIMER_DURATION);
            }
            break;
        case CAR_E_W_O:
            if (event == TIME_OUT) {
                ah_set_signal(SIGNAL_CAR_W, COLOR_RED);
                ah_set_signal(SIGNAL_CAR_E1, COLOR_RED);
                ah_set_signal(SIGNAL_PED_W, COLOR_GREEN);
                ah_set_signal(SIGNAL_PED_E, COLOR_GREEN);
                ah_lcd_write(TEXT_PED_E_W);
                state = PED_E_W;
            }
            break;
        case PED_E_W_O:
            if (event == TIME_OUT) {
                ah_set_signal(SIGNAL_CAR_W, COLOR_GREEN);
                ah_set_signal(SIGNAL_CAR_E1, COLOR_GREEN);
                ah_set_signal(SIGNAL_PED_W, COLOR_RED);
                ah_set_signal(SIGNAL_PED_E, COLOR_RED);
                ah_lcd_write(TEXT_CAR_E_W);
                state = CAR_E_W;
            }
            break;
        default:
            // do nothing;
    }

    /// END: To be programmed
}
