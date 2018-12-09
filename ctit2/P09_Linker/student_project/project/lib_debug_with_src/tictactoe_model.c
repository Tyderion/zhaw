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

#include <stdlib.h>
#include <stdint.h>
#include <assert.h>
#include "tictactoe_model.h"

static tictactoe_model_field_t _board[3][3];

static tictactoe_model_field_t* get_field(size_t row, size_t col);

// init/reset game status
void tictactoe_model_reset(void)
{
    size_t row;
    size_t col;
    for (row = 0; row < 3; row++) {
        for (col = 0; col < 3; col++) {
            tictactoe_model_field_t* field = get_field(row, col);
            assert(field);
            field->row = row;
            field->col = col;
            field->stone = TICTACTOE_MODEL_NOPLAYER;
            field->is_winner = 0;
        }
    }
}

// do a move if the field is free
tictactoe_model_player_t tictactoe_model_do_move(size_t row, size_t col, tictactoe_model_player_t player)
{
    tictactoe_model_field_t* field = get_field(row, col);
    assert(field);
    assert(player != TICTACTOE_MODEL_NOPLAYER);
    if (field->stone == TICTACTOE_MODEL_NOPLAYER) {
        field->stone = player;
        player = player == TICTACTOE_MODEL_PLAYER_A ? TICTACTOE_MODEL_PLAYER_B : TICTACTOE_MODEL_PLAYER_A;
    }
    return player;
}

// returns the winner player or TICTACTOE_MODEL_NOPLAYER
tictactoe_model_player_t tictactoe_model_eval_winner(void)
{
    const static struct { size_t row, col; } _wins[8][3] = {
        // rows
        { {0,0}, {0,1}, {0,2} },
        { {1,0}, {1,1}, {1,2} },
        { {2,0}, {2,1}, {2,2} },
        // cols
        { {0,0}, {1,0}, {2,0} },
        { {0,1}, {1,1}, {2,1} },
        { {0,2}, {1,2}, {2,2} },
        // diag
        { {0,0}, {1,1}, {2,2} },
        { {0,2}, {1,1}, {2,0} },
    };
    
    size_t check;
    size_t step;
    tictactoe_model_player_t player = TICTACTOE_MODEL_NOPLAYER;
    tictactoe_model_player_t winner = TICTACTOE_MODEL_NOPLAYER;
    for (check = 0; check < 8; check++) {
        tictactoe_model_field_t* field = get_field(_wins[check][0].row, _wins[check][0].col);
        assert(field);
        player = field->stone;
        if (player != TICTACTOE_MODEL_NOPLAYER) {
            for (step = 1; step < 3; step++) {
                field = get_field(_wins[check][step].row, _wins[check][step].col);
                assert(field);
                if (player != field->stone)
                {
                    player = TICTACTOE_MODEL_NOPLAYER;
                    break;
                }
            }
        }
        if (player != TICTACTOE_MODEL_NOPLAYER)
        {
            for (step = 0; step < 3; step++) {
                get_field(_wins[check][step].row, _wins[check][step].col)->is_winner = 1;
            }
            assert(winner == TICTACTOE_MODEL_NOPLAYER || winner == player);
            winner = player;
        }
    }
    return winner;
}

// get the field
tictactoe_model_field_t tictactoe_model_get(size_t row, size_t col)
{
    tictactoe_model_field_t* field = get_field(row, col);
    assert(field);
    return *field;
}

static tictactoe_model_field_t* get_field(size_t row, size_t col)
{
    return row < 3 && col < 3 ? &(_board[row][col]) : 0;
}
