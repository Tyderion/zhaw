/* ----------------------------------------------------------------------------
 * --  _____       ______  _____                                              -
 * -- |_   _|     |  ____|/ ____|                                             -
 * --   | |  _ __ | |__  | (___    Institute of Embedded Systems              -
 * --   | | | '_ \|  __|  \___ \   Zurich University of                       -
 * --  _| |_| | | | |____ ____) |  Applied Sciences                           -
 * -- |_____|_| |_|______|_____/   8401 Winterthur, Switzerland               -
 * ----------------------------------------------------------------------------
 * -- $Id: hal_exti.h 2798 2015-12-09 09:29:20Z mady $
 * ------------------------------------------------------------------------- */

#ifndef _HAL_EXTI_H
#define _HAL_EXTI_H

#include <stdint.h>
#include "hal_common.h"

/* public function declarations -------------------------------------------- */

/*
 * Initializes Pin A0 (PORT5, Pin 0 on CT Board) as external interrupt
 */
void hal_exti_init(void);

/*
 * Clears the exti interrupt flag for Pin A0 in the NVIC
 */
void hal_exti_clear_irq_flag(void);

/*
 * Returns true if exti interrupt bit for Pin A0 is set in NVIC
 */
hal_bool_t hal_exti_check_for_irq(void);

/*
 * Deactivate exti interrupt for Pin A0 in NVIC
 */
void hal_exti_deactivate(void);
#endif
