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
 * -- $Id: action_handler.c 3754 2016-11-01 15:06:13Z kesr $
 * ------------------------------------------------------------------------- */

/* standard includes */
#include <stdint.h>
#include <reg_stm32f4xx.h>

/* user includes */
#include "action_handler.h"
#include "hal_gpio.h"
#include "hal_rcc.h"
#include "hal_ct_lcd.h"


/* -- Macros
 * ------------------------------------------------------------------------- */

#define PINS_PORT_A (0xfff)


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

#define LCD_CLEAR      "                    "


/* Public function definitions
 * ------------------------------------------------------------------------- */

/*
 * See header file
 */
void action_handler_init(void)
{
    hal_gpio_output_t gpio_init;

    /* Enable peripheral */
    GPIOA_ENABLE();

    /* Configure output port (GPIO A) */
    gpio_init.pins = PINS_PORT_A;
    gpio_init.pupd = HAL_GPIO_PUPD_DOWN;
    gpio_init.out_speed = HAL_GPIO_OUT_SPEED_2MHZ;
    gpio_init.out_type = HAL_GPIO_OUT_TYPE_PP;

    hal_gpio_init_output(GPIOA, gpio_init);
}


/*
 * See header file
 */
void ah_set_signal(signal_t signal, color_t color)
{
    /// STUDENTS: To be programmed
    uint16_t value = hal_gpio_output_read(GPIOA);
    value &= ~(0x03 << signal);
    value |= color << signal;

    hal_gpio_output_write(GPIOA, value);
    /// END: To be programmed
}


/*
 * See header file
 */
void ah_lcd_write(char text[])
{
#ifndef CPPUTEST
    hal_ct_lcd_write(0, text);
#endif
}
