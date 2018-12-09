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
 * -- $Id: lcd_io.c 3158 2016-03-17 12:32:34Z ruan $
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


enum Boolean { FALSE = 0, TRUE = 1 };
enum ReturnValues { SUCCESS = 0, ERRORCODE = 1 };

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
void init_display_interface(void)
{
    hal_spi_init();
    init_touch_screen();
}

/*
 * according to description in header file
 */
uint8_t read_display_buffer(uint8_t *readBuffer)
{
    /// STUDENTS: To be programmed
   if(!display_has_data_to_send()) 
	{
		return 0;
	}
	uint8_t bcc = 0;
	send_read_display_buffer_request();
	uint8_t dc1 = hal_spi_read_write(0);
	if (dc1 != DC1_CHAR) 
	{
		return 0;
	}
	bcc += dc1;
	uint8_t len = hal_spi_read_write(0);
	bcc += len;
	for(uint8_t i = 0; i < len; i++) 
	{
		readBuffer[i] = hal_spi_read_write(0);
		bcc += readBuffer[i];
	}
	uint8_t bccrec = hal_spi_read_write(0);
	if (bccrec != bcc) 
	{
		return 0;
	}
	return len;
    /// END: To be programmed
}

/*
 * according to description in header file
 */
uint8_t write_cmd_to_display(const uint8_t *cmdBuffer, uint8_t length)
{
    /// STUDENTS: To be programmed
    uint8_t bcc = 0;
	uint8_t len = length + 1;
	hal_spi_read_write(DC1_CHAR);
	bcc += DC1_CHAR;
	hal_spi_read_write(len);
	bcc += len;
	hal_spi_read_write(ESC_CHAR);
	bcc += ESC_CHAR;
	for (uint8_t i = 0; i < length; i++) {
		hal_spi_read_write(cmdBuffer[i]);
		bcc += cmdBuffer[i];
	}
	hal_spi_read_write(bcc);
	uint8_t correct = hal_spi_read_write(0);
	if (correct == ACK_CHAR) 
	{
		return 0;
	} else 
	{
		return 1;
	}
    /// END: To be programmed
}

/*
 * Assemble and send a packet to trigger the reading of the display buffer
 * Uses the sequence "<DC2>, 0x01, 0x53, checksum" according to datasheet
 */
static void send_read_display_buffer_request()
{
    /// STUDENTS: To be programmed

    uint8_t bcc = (DC2_CHAR + 1 + 'S');
	
    // Send command
    // Send Control DC2
    hal_spi_read_write(DC2_CHAR);
    // send length = 1
    hal_spi_read_write(1);
    // send command
    hal_spi_read_write('S');
    // send bcc
    hal_spi_read_write(bcc);

    uint8_t rec = hal_spi_read_write(0);
    /// END: To be programmed
}

/*
 * Initialize Port 5.9 as input to receive SBUF-signal from display
 */
static void init_touch_screen(void)
{
    RCC->AHBENR[0] |= 0x00000001;         // start clock on GPIO A
    GPIOA->OSPEEDR |= 0x00030000;         // set P8 to 100 MHz
    GPIOA->MODER &= 0xFFFCFFFF;           // set input mode on P8
    GPIOA->PUPDR &= 0xFFFCFFFF;           // no pull-up/pull-down for P8
}

/*
 * The function checks the SBUF pin of the display. It returns TRUE if the pin
 * indicates that the display has data to send; FALSE otherwise
 */
static uint8_t display_has_data_to_send(void)
{
    uint16_t input_port_a = GPIOA->IDR;
    return (input_port_a &= SBUF_HIGH) ? FALSE : TRUE;
}
