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

// Tasks:
// 1. compile without change --> errors
// 2. adjust the project properties for C/C++ to also use the local
//    inc path for includes
// 3. compile and link --> errors
// 4. implement first the main function accroding to game.h
// 5. compile and link --> fix if includes are missing in main.c
// 6. compile and link --> link errors
// 7. adjust the project properties for Linker to also use the libraries
//    in lib or lib_debug
// 8. compile and link --> fix remaining errors until all libraries are
//    added to the Linker additional control

/// STUDENTS: To be programmed
#include "game.h"
#include "tictactoe_model.h"

int main(void)
{
    tictactoe_model_player_t player = game_init();
    while (player != TICTACTOE_MODEL_NOPLAYER) {
        player = game_do_move(player);
    }
    game_end();
}


/// END: To be programmed
