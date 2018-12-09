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
#ifndef _TICTACTOE_MODEL_H_
#define _TICTACTOE_MODEL_H_
#include <stdint.h>
#include <stdlib.h>

// players
typedef enum tictactoe_model_player_t {
    TICTACTOE_MODEL_NOPLAYER,
    TICTACTOE_MODEL_PLAYER_A,
    TICTACTOE_MODEL_PLAYER_B,
} tictactoe_model_player_t;
// field of the play board
typedef struct tictactoe_model_field_t {
    size_t row;
    size_t col;
    tictactoe_model_player_t stone;
    uint8_t is_winner;
} tictactoe_model_field_t;

// init/reset game status
void tictactoe_model_reset(void);
// do a move if the field is free
tictactoe_model_player_t tictactoe_model_do_move(size_t row, size_t col, tictactoe_model_player_t player);
// returns the winner player or TICTACTOE_MODEL_NOPLAYER
tictactoe_model_player_t tictactoe_model_eval_winner(void);
// get the field
tictactoe_model_field_t tictactoe_model_get(size_t row, size_t col);

#endif
