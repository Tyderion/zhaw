/* ----------------------------------------------------------------------------
 * --  _____       ______  _____                                              -
 * -- |_   _|     |  ____|/ ____|                                             -
 * --   | |  _ __ | |__  | (___    Institute of Embedded Systems              -
 * --   | | | '_ \|  __|  \___ \   Zurich University of                       -
 * --  _| |_| | | | |____ ____) |  Applied Sciences                           -
 * -- |_____|_| |_|______|_____/   8401 Winterthur, Switzerland               -
 * ----------------------------------------------------------------------------
 * --
 * -- Description:  Interface of module action_handler.
 * --
 * -- Sets or clears pins on outport.
 * --
 * -- $Id: action_handler.h 1991 2015-04-29 04:43:29Z ruan $
 * ------------------------------------------------------------------------- */

/* re-definition guard */
#ifndef _ACTION_HANDLER_H
#define _ACTION_HANDLER_H

/* standard includes */
#include <stdint.h>


/* -- Public function declarations
 * ------------------------------------------------------------------------- */

/*
 * Initializes the outport.
 */
void action_handler_init(void);


/*
 * Control the door.
 */
void ah_door_lock(void);
void ah_door_unlock(void);


/*
 * Control the motor.
 */
void ah_motor_fast_left(void);
void ah_motor_slow_left(void);
void ah_motor_slow_right(void);
void ah_motor_stop(void);


/*
 * Control the valve.
 */
void ah_valve_open(void);
void ah_valve_close(void);


/*
 * Control the heater.
 */
void ah_heater_on(void);
void ah_heater_off(void);


/*
 * Control the pump.
 */
void ah_pump_on(void);
void ah_pump_off(void);


/*
 * Writes a string at the given position.
 */
void ah_lcd_write(char text[]);
#endif
