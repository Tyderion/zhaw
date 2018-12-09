/* ----------------------------------------------------------------------------
 * --  _____       ______  _____                                              -
 * -- |_   _|     |  ____|/ ____|                                             -
 * --   | |  _ __ | |__  | (___    Institute of Embedded Systems              -
 * --   | | | '_ \|  __|  \___ \   Zurich University of                       -
 * --  _| |_| | | | |____ ____) |  Applied Sciences                           -
 * -- |_____|_| |_|______|_____/   8401 Winterthur, Switzerland               -
 * ------------------------------------------------------------------------- */
/**
 *  \brief  Implementation of module fsm.
 *
 *  Code NOT following coding style guidelines.
 *
 *  $Id: fsm.c 2949 2016-02-10 12:41:24Z feur $
 * ------------------------------------------------------------------------- */

/* User includes */
#include <reg_ctboard.h>
#include "fsm.h"


/* -- Macros (LCD)
 * ------------------------------------------------------------------------- */

#define Reset      0u
#define DipSw      1u
#define HexSw      2u
#define Gpio       3u
#define MaxState   Gpio

#define TOGGLE_MAX 20000u


/* -- Local function declarations
 * ------------------------------------------------------------------------- */

static uint16_t ConvertBinaryValueTo7SegmentCode(uint8_t value);
static uint32_t Create32BitFrom4BitValue(char value);


/* -- Public function definitions
 * ------------------------------------------------------------------------- */

/*
 * See header file
 */
void init_FSM(void)
{
    state_machine(Reset);
}


/*
 * See header file
 */
uint8_t get_next_state(void)
{
    static uint8_t state = 0;
    static const int reset = -3;
    state++;
    if (state > MaxState) state += reset;
    return state;
}


/*
 * See header file
 */
void state_machine(uint8_t state)
{
    static uint32_t toggle_count = 0u;
    int TempButton, DipSwData, ButtonData, TempHexData, DataHexSw;
    int GpioOuput, GpioInput;
    toggle_count++;
    if (toggle_count >= TOGGLE_MAX) toggle_count = 0u;
    switch (state) {
        case 0:
            CT_LED->WORD = 0x00000000;
            CT_SEG7->RAW.WORD = 0xffffffff;
            break;
        case 1:
            CT_LCD->BG.RED = 0xffff;
            CT_LCD->BG.GREEN = 0x0000;
            CT_LCD->BG.BLUE = 0x0000;
            DipSwData = CT_DIPSW->WORD;
            TempButton = CT_BUTTON;
            CT_LED->WORD = DipSwData;
            ButtonData = Create32BitFrom4BitValue(TempButton);
            CT_SEG7->RAW.WORD = ButtonData;
            break;
        case 2:
            CT_LCD->BG.RED = 0x0000;
            CT_LCD->BG.GREEN = 0xffff;
            CT_LCD->BG.BLUE = 0x0000;
            TempHexData = CT_HEXSW;
            DataHexSw = ConvertBinaryValueTo7SegmentCode(TempHexData);
            CT_SEG7->RAW.BYTE.DS0 = DataHexSw;
            CT_SEG7->RAW.BYTE.DS1 = DataHexSw;
            CT_SEG7->RAW.BYTE.DS2 = DataHexSw;
            CT_SEG7->RAW.BYTE.DS3 = DataHexSw;
            break;
        case 3:
            CT_LCD->BG.RED = 0x0000;
            CT_LCD->BG.GREEN = 0x0000;
            CT_LCD->BG.BLUE = 0xffff;
            if (toggle_count >= (TOGGLE_MAX / 2u)) {
                GpioOuput = 0xaaaaaaaa;
            } else {
                GpioOuput = 0x55555555;
            }
            CT_GPIO->OUT.WORD = GpioOuput;
            GpioInput = CT_GPIO->IN.WORD;
            CT_LED->WORD = GpioInput;
            break;
        default:
            break;
    }
}


/* -- Local function definitions
 * ------------------------------------------------------------------------- */


/**
 *  \brief  Translates binary values into 7 segment code.
 *  \param  value: 8 bit binary value.
 *  \return Converted 16 bit 7 segment code.
 */
static uint16_t ConvertBinaryValueTo7SegmentCode(uint8_t value)
{
    static uint8_t tab[16] = { 0xC0, 0xF9, 0xA4, 0xB0,
                               0x99, 0x92, 0x82, 0xF8,
                               0x80, 0x98, 0x88, 0x83,
                               0xA7, 0xA1, 0x86, 0x8E };
    uint16_t ret_value = 0;
    ret_value |= tab[(value % 16)];
    ret_value |= tab[(value / 16)] << 8;
    return ret_value;
}


/**
 *  \brief  Expand 4 bit value to 32 bit: 0b0101 -> 0x00FF00FF.
 *  \param  value: Pattern to expand.
 *  \retval Convreted 32 bit value.
 */
static uint32_t Create32BitFrom4BitValue(char value)
{
    uint8_t i;
    uint32_t out = 0;
    for (i = 0; i < 4; i++) {
        if (value & (1 << i)) {
            out |= 0xff << (i << 3);
        }
    }
    return out;
}
