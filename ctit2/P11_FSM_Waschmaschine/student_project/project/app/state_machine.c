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
 * -- $Id: state_machine.c 1991 2015-04-29 04:43:29Z ruan $
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

#define TIMER_DURATION_LEFT  400u
#define TIMER_DURATION_RIGHT 400u
#define TIMER_DURATION_SPIN  500u

#define TEXT_OPENED          "OPENED         "
#define TEXT_CLOSED          "CLOSED         "
#define TEXT_WATER_INTAKE    "WATER INTAKE   "
#define TEXT_WATER_HEATING   "WATER HEATING  "
#define TEXT_WATER_OUTTAKE   "WATER OUTTAKE  "
#define TEXT_WASHING_LEFT    "WASHING LEFT   "
#define TEXT_WASHING_RIGHT   "WASHING RIGHT  "
#define TEXT_FAST_SPIN       "FAST SPIN      "


/* Local variables
 * ------------------------------------------------------------------------- */

static state_t state = OPENED;


/* Public function definitions
 * ------------------------------------------------------------------------- */

/*
 * See header file
 */
void fsm_init(void)
{
    action_handler_init();
    ah_lcd_write(TEXT_OPENED);
    state = OPENED;
}


/*
 * See header file
 */
void fsm_handle_event(event_t event)
{
    /// STUDENTS: To be programmed
    switch(state) {
        case OPENED: 
            if (event == DOOR_CLOSED) {
                ah_lcd_write(TEXT_CLOSED);
                state = CLOSED;
            } 
            break;
        case CLOSED: 
            if (event == DOOR_OPEN) {
                ah_lcd_write(TEXT_OPENED);
                state = OPENED;
            } else if (event == BUTTON_SPIN) {
                ah_lcd_write(TEXT_FAST_SPIN);
                ah_door_lock();
                ah_motor_fast_left();
                timer_start(TIMER_DURATION_SPIN);
                state = FAST_SPIN;
            } else if (event == BUTTON_WASH) {
                ah_lcd_write(TEXT_WATER_INTAKE);
                ah_door_lock();
                ah_valve_open();
                state = WATER_INTAKE;
            }
            break;
        case FAST_SPIN: 
            if (event == BUTTON_STOP || event == TIME_OUT) {
                timer_stop();
                ah_door_unlock();
                ah_motor_stop();
                ah_lcd_write(TEXT_CLOSED);
                state = CLOSED;
                }
            break;
        case WATER_INTAKE:
            if (event == FLOATER_HIGH_ON) {
                ah_lcd_write(TEXT_WATER_HEATING);
                        ah_valve_close();
                        ah_heater_on();
                        state = WATER_HEATING;
            }
            break;
        case WATER_HEATING: 
            if (event == THERMOSTAT_HOT) {
                ah_lcd_write(TEXT_WASHING_LEFT);
                        ah_heater_off();
                        ah_motor_slow_left();
                        timer_start(TIMER_DURATION_LEFT);
                        state = WASHING_LEFT;
            }
            break;
        case WASHING_LEFT:
            if (event == TIME_OUT) {
                timer_stop();
                ah_motor_slow_right();
                timer_start(TIMER_DURATION_RIGHT);
                ah_lcd_write(TEXT_WASHING_RIGHT);
                state = WASHING_RIGHT;
            }
            break;
        case WASHING_RIGHT:
            if (event == TIME_OUT) {
                timer_stop();
                ah_motor_stop();
                ah_pump_on();
                ah_lcd_write(TEXT_WATER_OUTTAKE);
                state = WATER_OUTTAKE;
                break;
            }
            break;
        case WATER_OUTTAKE: 
        if (event == FLOATER_LOW_OFF) {
            timer_stop();
            ah_pump_off();
            ah_motor_fast_left();
            timer_start(TIMER_DURATION_SPIN);
            ah_lcd_write(TEXT_FAST_SPIN);
            state = FAST_SPIN;

        }
        break;
        default:
    }



    /// END: To be programmed
}
