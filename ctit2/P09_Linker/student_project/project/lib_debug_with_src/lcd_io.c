/* ------------------------------------------------------------------
 * --  _____       ______  _____                                    -
 * -- |_   _|     |  ____|/ ____|                                   -
 * --   | |  _ __ | |__  | (___    Institute of Embedded Systems    -
 * --   | | | '_ \|  __|  \___ \   Zuercher Hochschule Winterthur   -
 * --  _| |_| | | | |____ ____) |  (University of Applied Sciences) -
 * -- |_____|_| |_|______|_____/   8401 Winterthur, Switzerland     -
 * ------------------------------------------------------------------
 * --
 * -- Project     : CT2 lab - SPI Display
 * -- Description : Contains the implementations of the functions
 * --               to write and read frames from and to the TFT-LCD
 * --               display EAeDIPTFT43-A.
 * --
 * -- $Id$
 * ------------------------------------------------------------------
 */
#include <reg_stm32f4xx.h>
#include "lcd_io.h"
#include "hal_spi.h"

#define ACK_CHAR         (uint8_t)0x06
#define DC1_CHAR         (uint8_t)0x11
#define DC2_CHAR         (uint8_t)0x12
#define ESC_CHAR         (uint8_t)0x1B
#define ONE_CHAR         (uint8_t)0x01

#define NOTHING_RECEIVED (uint8_t)0
#define SBUF_HIGH        (uint16_t)0x0100

/* ------------------------------------------------------------------
 * -- Function prototypes
 * ------------------------------------------------------------------
 */
static void send_read_display_buffer_request(void);
static uint8_t display_has_data_to_send(void);
static void init_touch_screen(void);

/* ------------------------------------------------------------------
 * -- Function implementations
 * ------------------------------------------------------------------
 */

/*
 * according to description in header file
 */
void lcd_io_init_display_interface(void)
{
    hal_spi_init();
    init_touch_screen();
}

/*
 * according to description in header file
 */
uint8_t lcd_io_read_display_buffer(uint8_t *readBuffer)
{
    uint8_t i;
    uint8_t length;

    if (!display_has_data_to_send()) {
        return NOTHING_RECEIVED;
    }

    send_read_display_buffer_request();

    if (hal_spi_read_write(0x00) != ACK_CHAR) {
        return NOTHING_RECEIVED;
    }

    if (hal_spi_read_write(0x00) != DC1_CHAR) {
        return NOTHING_RECEIVED;
    }

    length = hal_spi_read_write(0x00);
    length += 1;       // add one for the checksum

    for (i = 0; i < length; i++) {
        readBuffer[i] = hal_spi_read_write(0x00);
    }

    return length;
}

/*
 * according to description in header file
 */
uint8_t lcd_io_write_cmd_to_display(const uint8_t *cmdBuffer, uint8_t length)
{
    uint8_t i;
    uint16_t checksum = 0;

    hal_spi_read_write(DC1_CHAR);
    checksum += DC1_CHAR;

    hal_spi_read_write(length + 1);
    checksum += length + 1;

    hal_spi_read_write(ESC_CHAR);
    checksum += ESC_CHAR;

    for (i = 0; i < length; i++) {
        hal_spi_read_write(cmdBuffer[i]);
        checksum += cmdBuffer[i];
    }
    checksum %= 256;
    hal_spi_read_write((uint8_t)checksum);

    return hal_spi_read_write(0x00) != ACK_CHAR;
}

/*
 * Assemble and send a packet to trigger the reading of the display buffer
 * Uses the sequence "<DC2>, 0x01, 0x53, checksum" according to datasheet
 */
static void send_read_display_buffer_request()
{
    uint16_t checksum = 0;

    hal_spi_read_write(DC2_CHAR);
    checksum += DC2_CHAR;

    hal_spi_read_write(ONE_CHAR);    // The protocol requires the value one
    checksum += ONE_CHAR;            // after DC2

    hal_spi_read_write(0x53);
    checksum += 0x53;

    checksum = checksum % 256;
    hal_spi_read_write((uint8_t)checksum);
}

/*
 * Initialize Port 5.9 as input to receive SBUF-signal from display
 */
static void init_touch_screen(void)
{
    REG_RCC_AHB1ENR |= 0x00000001;                  // start clock on GPIO A
    REG_GPIOx_OSPEEDR(GPIOA_BASE) |= 0x00030000;    // set P8 to 100 MHz
    REG_GPIOx_MODER(GPIOA_BASE) &= 0xFFFCFFFF;      // set input mode on P8
    REG_GPIOx_PUPDR(GPIOA_BASE) &= 0xFFFCFFFF;      // no pull-up/pull-down for P8
}

/*
 * The function checks the SBUF pin of the display. It returns TRUE if the pin
 * indicates that the display has data to send; FALSE otherwise
 */
static uint8_t display_has_data_to_send(void)
{
    uint16_t input_port_a = REG_GPIOx_IDR(GPIOA_BASE);
    return (input_port_a &= SBUF_HIGH) ? 0 : 1;
}
