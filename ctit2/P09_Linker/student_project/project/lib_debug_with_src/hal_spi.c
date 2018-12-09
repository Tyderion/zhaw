/* ------------------------------------------------------------------
 * --  _____       ______  _____                                    -
 * -- |_   _|     |  ____|/ ____|                                   -
 * --   | |  _ __ | |__  | (___    Institute of Embedded Systems    -
 * --   | | | '_ \|  __|  \___ \   Zuercher Hochschule Winterthur   -
 * --  _| |_| | | | |____ ____) |  (University of Applied Sciences) -
 * -- |_____|_| |_|______|_____/   8401 Winterthur, Switzerland     -
 * ------------------------------------------------------------------
 * --
 * -- Module      : SPI Library
 * --
 * -- $Id$
 * ------------------------------------------------------------------
 */
#include <reg_stm32f4xx.h>
#include "hal_spi.h"

#define BIT_TXE  (uint32_t)0x00000002
#define BIT_RXNE (uint32_t)0x00000001
#define BIT_BSY  (uint32_t)0x00000080

static void set_ss_pin_low(void);
static void set_ss_pin_high(void);
static void wait_10_us(void);

/*
 * according to description in header file
 */
void hal_spi_init(void)
{
    REG_RCC_APB2ENR |= 0x00001000;                  // enable SPI clock
    REG_RCC_AHB1ENR |= 0x00000001;                  // start clock on GPIO A
    REG_GPIOx_OSPEEDR(GPIOA_BASE) &= 0xFFFF00FF;    // clear P4 to P7
    REG_GPIOx_OSPEEDR(GPIOA_BASE) |= 0x0000FF00;    // set P4 to P7 to 100 MHz
    REG_GPIOx_MODER(GPIOA_BASE) &= 0xFFFF00FF;      // clear mode on P5 to P7
    REG_GPIOx_MODER(GPIOA_BASE) |= 0x0000A900;      // Set alternate function mode on
                                                    // P5 to P7, P4 output mode
    REG_GPIOx_AFRL(GPIOA_BASE) &= 0x0000FFFF;       // clear alternate function
    REG_GPIOx_AFRL(GPIOA_BASE) |= 0x55550000;       // Set SPI1 alternate function

    REG_SPIx_CR2(SPI1_BASE) = 0x0000;               // set spi to default state
    REG_SPIx_CR1(SPI1_BASE) = 0x0000;               // set spi to default state

    REG_SPIx_CR1(SPI1_BASE) |= 0x0004;              // set Master Mode
    REG_SPIx_CR1(SPI1_BASE) |= 0x0038;              // set clockrate 45 MHz / 256 = 175 kHz
    REG_SPIx_CR1(SPI1_BASE) |= 0x0300;              // set SS Pin software controlled
    REG_SPIx_CR1(SPI1_BASE) |= 0x0040;              // enable SPI Controller

    set_ss_pin_high();
}

/*
 * according to description in header file
 */
uint8_t hal_spi_read_write(uint8_t send_byte)
{
    set_ss_pin_low();

    REG_SPIx_DR(SPI1_BASE) = send_byte;             // copy data to output buffer
    while (!(REG_SPIx_SR(SPI1_BASE) & BIT_TXE));     // wait for transmission to complete
    while (!(REG_SPIx_SR(SPI1_BASE) & BIT_RXNE));    // wait for received data to complete
    while ((REG_SPIx_SR(SPI1_BASE) & BIT_BSY));      // wait for transfer to complete

    wait_10_us();                                   // because display is very slow
    set_ss_pin_high();
    return (uint8_t)REG_SPIx_DR(SPI1_BASE);
}

/**
 * Set Slave-Select Pin (Port A, Pin 5) low
 *
 * No parameters
 *
 * No returns
 */
static void set_ss_pin_low(void)
{
    REG_GPIOx_BSRR(GPIOA_BASE) |= 0x00100000;       // Set Port A, Pin 5 low
}

/**
 * Set Slave-Select Pin (Port A, Pin 5) high
 *
 * No parameters
 *
 * No returns
 */
static void set_ss_pin_high(void)
{
    REG_GPIOx_BSRR(GPIOA_BASE) |= 0x00000010;        // Set Port A, Pin 5 high
}

/**
 * Wait for approximately 10us
 *
 * No parameters
 *
 * No returns
 */
static void wait_10_us(void)
{
    volatile uint16_t counter = 0;
    while (counter < 24000) {
        counter++;
    }
}
