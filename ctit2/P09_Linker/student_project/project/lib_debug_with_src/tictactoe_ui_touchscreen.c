/* ------------------------------------------------------------------
 * --  _____       ______  _____                                    -
 * -- |_   _|     |  ____|/ ____|                                   -
 * --   | |  _ __ | |__  | (___    Institute of Embedded Systems    -
 * --   | | | '_ \|  __|  \___ \   Zuercher Hochschule Winterthur   -
 * --  _| |_| | | | |____ ____) |  (University of Applied Sciences) -
 * -- |_____|_| |_|______|_____/   8401 Winterthur, Switzerland     -
 * ------------------------------------------------------------------
 * --
* -- Project     : CT2 lab - Linking
 * --
 * -- $Id$
 * ------------------------------------------------------------------
 */
 
#include <stdint.h>
#include <stdlib.h>
#include "tictactoe_ui.h"
#include "cmd_lcd.h"
#include "cmd_touch.h"
#include "lcd_io.h"

#define SCREEN_W 480
#define SCREEN_H 272
#define FIELD_W 80
#define FIELD_H 60
#define SPACING 2
#define TOPOFFSET 50
#define LEFTOFFSET 50
#define RIGHTOFFSET 50

#define MIN_LENGTH_BUFFER_MESSAGE 3

typedef union _input_t
{
    uint8_t raw;
    struct { uint8_t row:2, col:2, field:1, restart:1, end:1, up:1; } val;
} _input_t;

static uint8_t wait_for_input(void);
static uint16_t col_to_field_x(size_t col);
static uint16_t row_to_field_y(size_t row);
static uint16_t button_to_x(void);
static uint16_t button_to_y(size_t row);
static void create_field(size_t row, size_t col, uint8_t c);
static void create_button(uint8_t restart);
static void remove_button(uint8_t restart);
static uint8_t create_field_ret(size_t row, size_t col, uint8_t up);
static uint8_t create_button_ret(uint8_t restart, uint8_t up);
static void wait_long_enough(void);



// draws the empty board and control buttons and the needed touch areas
void tictactoe_ui_init(void)
{
    cmd_lcd_init_display();
    cmd_touch_reset_all_touch_buttons(0, 0);
    cmd_lcd_set_display_color(COLOR_BLACK, COLOR_BLACK);
    cmd_lcd_set_cursor_on_off(CURSOR_OFF);
    cmd_lcd_set_font_zoom_factor(3, 2);
    cmd_lcd_set_font_color(COLOR_BLUE, COLOR_TRANSPARENT);
    cmd_lcd_set_display_font(FONT_CHICAGO14_PROP);

    cmd_touch_set_touch_font_color(COLOR_WHITE, COLOR_WHITE);
    cmd_touch_set_touch_panel_color(COLOR_GREEN, COLOR_GREEN, COLOR_BLACK, COLOR_GREEN, COLOR_GREEN, COLOR_BLACK);
    cmd_touch_set_touch_font_zoom_factor(2, 2);
    cmd_touch_set_touch_font(FONT_CHICAGO14_PROP);
    cmd_touch_set_touch_enable(TOUCH_ON);
    cmd_touch_set_touch_panel_color(COLOR_RED, COLOR_RED, COLOR_RED, COLOR_RED, COLOR_RED, COLOR_RED);

    cmd_lcd_print_text_on_display(80, 5, (uint8_t *)"Tic - Tac - Toe");
    create_button(0);
    create_button(1);
}

// redraws the empty board
void tictactoe_ui_reset(void)
{
    size_t row;
    size_t col;
    
    cmd_lcd_clear_display();
    cmd_touch_reset_all_touch_buttons(0, 0);
    cmd_touch_set_touch_panel_color(COLOR_RED, COLOR_RED, COLOR_RED, COLOR_RED, COLOR_RED, COLOR_RED);

    cmd_lcd_print_text_on_display(80, 5, (uint8_t *)"Tic - Tac - Toe");
    create_button(0);
    create_button(1);

    cmd_touch_set_touch_panel_color(COLOR_GREEN, COLOR_GREEN, COLOR_BLACK, COLOR_GREEN, COLOR_GREEN, COLOR_BLACK);
    for (row = 0; row < 3; row++) {
        for (col = 0; col < 3; col++) {
            create_field(row, col, ' ');
            wait_long_enough();
        }
    }
}

// draws the player a or b in the given field of the board (player_a == 0 --> b, otherwise a)
void tictactoe_ui_set_player(size_t row, size_t col, uint8_t player_a)
{
    cmd_touch_reset_all_touch_buttons(create_field_ret(row, col, 0), 0);
    cmd_touch_reset_all_touch_buttons(create_field_ret(row, col, 1), 0);
    create_field(row, col, player_a ? 'X' : 'O');
}

// display the winner
void tictactoe_ui_winner(uint8_t player_a)
{
    uint16_t x = button_to_x() - 30;
    uint16_t y = button_to_y(1) + 30;
    char text[] = "  wins";
    text[0] = player_a ? 'X' : 'O';
    cmd_lcd_print_text_on_display(x, y, (uint8_t*)text);
}

// blocking waits for any input and returns the affected button --> if end or restart is set, row/col is undefined
tictactoe_ui_input_t tictactoe_ui_wait_for_input(void)
{
    _input_t ret;
    tictactoe_ui_input_t input;
    
    ret.raw = wait_for_input();
    
    input.row = ret.val.row;
    input.col = ret.val.col;
    input.restart = ret.val.restart;
    input.end = ret.val.end;
    input.up = ret.val.up;
    return input;
}

void tictactoe_ui_end(void)
{
    uint16_t x = button_to_x();
    uint16_t y = button_to_y(1);
    cmd_lcd_print_text_on_display(x, y, (uint8_t*)"Done");
    remove_button(0);
    remove_button(1);
}

static uint8_t wait_for_input(void)
{
    uint8_t readBuffer[256];
    while(1) {
        uint8_t n = lcd_io_read_display_buffer(readBuffer);
        if (n >= MIN_LENGTH_BUFFER_MESSAGE) {
            break;
        }
    }
    return readBuffer[3];
}

static uint16_t col_to_field_x(size_t col)
{
    return LEFTOFFSET + col*(FIELD_W + SPACING);
}

static uint16_t row_to_field_y(size_t row)
{
    return TOPOFFSET + row*(FIELD_H + SPACING);
}

static uint16_t button_to_x(void)
{
    return SCREEN_W - RIGHTOFFSET - FIELD_W;
}

static uint16_t button_to_y(size_t row)
{
    return row_to_field_y(row);
}

static void create_field(size_t row, size_t col, uint8_t c)
{
    uint8_t text[2];
    uint16_t x1 = col_to_field_x(col);
    uint16_t y1 = row_to_field_y(row);
    uint16_t x2 = x1 + FIELD_W;
    uint16_t y2 = y1 + FIELD_H;
    text[0] = c;
    text[1] = 0;
    cmd_touch_define_touch_button(x1, y1, x2, y2, create_field_ret(row, col, 1), create_field_ret(row, col, 0), text);
}

static void create_button(uint8_t restart)
{
    uint16_t x1 = button_to_x();
    uint16_t y1 = button_to_y(restart ? 0 : 2);
    uint16_t x2 = x1 + FIELD_W;
    uint16_t y2 = y1 + FIELD_H;
    char* s = restart ? "Start" : "Stop";
    cmd_touch_define_touch_button(x1, y1, x2, y2, create_button_ret(restart, 1), create_button_ret(restart, 0), (uint8_t*)s);
}
static void remove_button(uint8_t restart)
{
    cmd_touch_reset_all_touch_buttons(create_button_ret(restart, 0), 1);
    cmd_touch_reset_all_touch_buttons(create_button_ret(restart, 1), 1);
}


static uint8_t create_field_ret(size_t row, size_t col, uint8_t up)
{
    _input_t ret;
    ret.raw = 0;
    ret.val.field = 1;
    ret.val.col = col;
    ret.val.row = row;
    ret.val.up = up;
    return ret.raw;
}
static uint8_t create_button_ret(uint8_t restart, uint8_t up)
{
    _input_t ret;
    ret.raw = 0;
    ret.val.restart = restart;
    ret.val.end = !restart;
    ret.val.up = up;
    return ret.raw;
}
static void wait_long_enough(void)
{
    volatile uint16_t counter = 0;
    while (counter < 32000) {
        counter++;
    }
}
