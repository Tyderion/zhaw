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
 * -- Usage:
 * -- int main(void)
 * -- {
 * --    tictactoe_model_player_t player = game_init();
 * --    while(player != TICTACTOE_MODEL_NOPLAYER)
 * --    {
 * --        player = game_do_move(player);
 * --    }
 * --    game_end();
 * -- }
 * --
 * -- $Id$
 * ------------------------------------------------------------------
 */
#ifndef _GAME_H_
#define _GAME_H_

// init tictactoe game
enum tictactoe_model_player_t game_init(void);

// do next move and return new player (or current if no move)
// returns TICTACTOE_MODEL_NOPLAYER to indicate end of game
enum tictactoe_model_player_t game_do_move(enum tictactoe_model_player_t player);

// to be called at end of the game
void game_end(void);
#endif
