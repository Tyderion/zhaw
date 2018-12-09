/* ----------------------------------------------------------------------------
 * --  _____       ______  _____                                              -
 * -- |_   _|     |  ____|/ ____|                                             -
 * --   | |  _ __ | |__  | (___    Institute of Embedded Systems              -
 * --   | | | '_ \|  __|  \___ \   Zurich University of                       -
 * --  _| |_| | | | |____ ____) |  Applied Sciences                           -
 * -- |_____|_| |_|______|_____/   8401 Winterthur, Switzerland               -
 * ----------------------------------------------------------------------------
 * --
 * -- Description:  Implementation of module action_handler.
 * --
 * -- $Id: action_handler.c 3790 2016-11-22 10:20:17Z kesr $
 * ------------------------------------------------------------------------- */

/* standard includes */
#include <stdint.h>
#include <reg_stm32f4xx.h>
#include <reg_ctboard.h>
#include <hal_ct_lcd.h>

/* user includes */
#include "action_handler.h"


/* -- Macros for accessing CT Board
 * ------------------------------------------------------------------------- */

#define PORT_OUTPUT (CT_GPIO->OUT.BYTE.P1)


/* -- Macros for accessing LCD Display
 * ------------------------------------------------------------------------- */

#define LCD_CTRL       (*((volatile uint8_t *)(0x60000300)))
#define LCD_DATA       (*((volatile uint8_t *)(0x60000302)))

#define LCD_CMD_WRITE  (0x01)
#define LCD_CMD_STOP   (0x00)

#define LCD_CTRL_MORE  (0x80)               // After data another control byte
#define LCD_CTRL_TEXT  (0x40)               // Data accessing char ram

#define LCD_ADDR_LINE1 (0x00)
#define LCD_ADDR_LINE2 (0x40)

#define LCD_WAIT       (0x1fff)

#define LCD_CLEAR      "                    "


/* -- Macros for washing machine
 * ------------------------------------------------------------------------- */

#define MASK_DOOR_LOCK   (0x7f)
#define MASK_VALVE_OPEN  (0xbf)
#define MASK_HEATER_ON   (0xdf)
#define MASK_MOTOR_SLOW  (0xf7)
#define MASK_MOTOR_FAST  (0xfb)
#define MASK_MOTOR_RIGHT (0xfd)
#define MASK_PUMP_ON     (0xef)

#define ENABLE_BITS(bits) PORT_OUTPUT &= (bits)
#define DISABLE_BITS(bits) PORT_OUTPUT |= ~(bits)


/* Public function definitions
 * ------------------------------------------------------------------------- */

/*
 * See header file
 */
void action_handler_init(void)
{
    /// STUDENTS: To be programmed

PORT_OUTPUT = 0xff;


    /// END: To be programmed
}


/*
 * See header file
 */
void ah_door_lock(void)
{
    /// STUDENTS: To be programmed


ENABLE_BITS(MASK_DOOR_LOCK);

    /// END: To be programmed
}


/*
 * See header file
 */
void ah_door_unlock(void)
{
    /// STUDENTS: To be programmed

DISABLE_BITS(MASK_DOOR_LOCK);


    /// END: To be programmed
}


/*
 * See header file
 */
void ah_motor_fast_left(void)
{
    /// STUDENTS: To be programmed

DISABLE_BITS(MASK_MOTOR_SLOW & MASK_MOTOR_RIGHT);
		ENABLE_BITS(MASK_MOTOR_FAST);


    /// END: To be programmed
}


/*
 * See header file
 */
void ah_motor_slow_left(void)
{
    /// STUDENTS: To be programmed

DISABLE_BITS(MASK_MOTOR_FAST & MASK_MOTOR_RIGHT);
		ENABLE_BITS(MASK_MOTOR_SLOW);



    /// END: To be programmed
}


/*
 * See header file
 */
void ah_motor_slow_right(void)
{
    /// STUDENTS: To be programmed
DISABLE_BITS(MASK_MOTOR_FAST);
		ENABLE_BITS(MASK_MOTOR_SLOW & MASK_MOTOR_RIGHT);




    /// END: To be programmed
}


/*
 * See header file
 */
void ah_motor_stop(void)
{
    /// STUDENTS: To be programmed

	DISABLE_BITS(MASK_MOTOR_FAST & MASK_MOTOR_SLOW & MASK_MOTOR_SLOW);


    /// END: To be programmed
}


/*
 * See header file
 */
void ah_valve_open(void)
{
    /// STUDENTS: To be programmed

ENABLE_BITS(MASK_VALVE_OPEN);


    /// END: To be programmed
}


/*
 * See header file
 */
void ah_valve_close(void)
{
    /// STUDENTS: To be programmed

DISABLE_BITS(MASK_VALVE_OPEN);


    /// END: To be programmed
}


/*
 * See header file
 */
void ah_heater_on(void)
{
    /// STUDENTS: To be programmed

ENABLE_BITS(MASK_HEATER_ON);


    /// END: To be programmed
}


/*
 * See header file
 */
void ah_heater_off(void)
{
    /// STUDENTS: To be programmed


		DISABLE_BITS(MASK_HEATER_ON);


    /// END: To be programmed
}


/*
 * See header file
 */
void ah_pump_on(void)
{
    /// STUDENTS: To be programmed

ENABLE_BITS(MASK_PUMP_ON);


    /// END: To be programmed
}


/*
 * See header file
 */
void ah_pump_off(void)
{
    /// STUDENTS: To be programmed

		DISABLE_BITS(MASK_PUMP_ON);



    /// END: To be programmed
}


/*
 * See header file
 */
void ah_lcd_write(char text[])
{
    hal_ct_lcd_clear();
    hal_ct_lcd_write(0, text);
}
