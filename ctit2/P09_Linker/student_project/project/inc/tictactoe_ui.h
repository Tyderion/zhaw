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
 
#ifndef _GAME_UI_H_
#define _GAME_UI_H_
#include <stdlib.h>
#include <stdint.h>

// input button value
typedef struct tictactoe_ui_input_t {
    uint8_t row;
    uint8_t col;
    uint8_t restart;
    uint8_t end;
    uint8_t up;
} tictactoe_ui_input_t;

// draws the empty board and control buttons
void tictactoe_ui_init(void);

// redraws the empty board
void tictactoe_ui_reset(void);

// draws the player a or b in the given field of the board (player_a == 0 --> b, otherwise a)
void tictactoe_ui_set_player(size_t row, size_t col, uint8_t player_a);

// display the winner
void tictactoe_ui_winner(uint8_t player_a);

// blocking waits for any input and returns the affected button --> if end or restart is set, row/col is undefined
tictactoe_ui_input_t tictactoe_ui_wait_for_input(void);

// report end of game
void tictactoe_ui_end(void);

#endif
