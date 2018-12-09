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
#include "tictactoe_model.h"
#include "tictactoe_ui.h"
#include "game.h"

static tictactoe_model_player_t winner = TICTACTOE_MODEL_NOPLAYER;

tictactoe_model_player_t game_init(void)
{
    tictactoe_model_reset();
    tictactoe_ui_init();
    //tictactoe_ui_reset();
    return TICTACTOE_MODEL_PLAYER_A;
}
tictactoe_model_player_t game_do_move(tictactoe_model_player_t player)
{
    tictactoe_ui_input_t input = tictactoe_ui_wait_for_input();
    if (input.up) {
        // Nop - only active on button down.
    } else if (input.end) {
        player = TICTACTOE_MODEL_NOPLAYER;
    } else if (input.restart) {
        tictactoe_model_reset();
        tictactoe_ui_reset();
        player = TICTACTOE_MODEL_PLAYER_A;
        winner = TICTACTOE_MODEL_NOPLAYER;
    } else if (winner == TICTACTOE_MODEL_NOPLAYER) {
        tictactoe_model_player_t next_player = tictactoe_model_do_move(
            input.row, input.col, player);
        if (player != next_player) {
            tictactoe_ui_set_player(input.row, input.col,
                                    player == TICTACTOE_MODEL_PLAYER_A);
            player = next_player;
            winner = tictactoe_model_eval_winner();
            if (winner != TICTACTOE_MODEL_NOPLAYER) {
                tictactoe_ui_winner(winner == TICTACTOE_MODEL_PLAYER_A);
            }
        }
    }
    return player;
}
void game_end(void)
{
    tictactoe_ui_end();
}
