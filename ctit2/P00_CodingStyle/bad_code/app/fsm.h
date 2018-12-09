/* ----------------------------------------------------------------------------
 * --  _____       ______  _____                                              -
 * -- |_   _|     |  ____|/ ____|                                             -
 * --   | |  _ __ | |__  | (___    Institute of Embedded Systems              -
 * --   | | | '_ \|  __|  \___ \   Zurich University of                       -
 * --  _| |_| | | | |____ ____) |  Applied Sciences                           -
 * -- |_____|_| |_|______|_____/   8401 Winterthur, Switzerland               -
 * ------------------------------------------------------------------------- */
/**
 *  \brief  Interface of module fsm.
 *
 *  Code NOT following coding style guidelines.
 *
 *  $Id: fsm.h 2949 2016-02-10 12:41:24Z feur $
 * ------------------------------------------------------------------------- */

/* Re-definition guard */
#ifndef _FSM_H
#define _FSM_H


/* Standard includes */
#include <stdint.h>


/* -- Public function declarations
 * ------------------------------------------------------------------------- */

/**
 *  \brief  Initialises the FSM.
 */
void init_FSM(void);

/**
 *  \brief  Switch to next state.
 */
uint8_t get_next_state(void);

/**
 *  \brief  Process an event in the state machine.
 *  \param  state: State to process.
 */
void state_machine(uint8_t state);
#endif
