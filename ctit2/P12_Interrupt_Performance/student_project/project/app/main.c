/* ----------------------------------------------------------------------------
 * --  _____       ______  _____                                              -
 * -- |_   _|     |  ____|/ ____|                                             -
 * --   | |  _ __ | |__  | (___    Institute of Embedded Systems              -
 * --   | | | '_ \|  __|  \___ \   Zurich University of                       -
 * --  _| |_| | | | |____ ____) |  Applied Sciences                           -
 * -- |_____|_| |_|______|_____/   8401 Winterthur, Switzerland               -
 * ----------------------------------------------------------------------------
 * -- $Id: main.c 3811 2016-12-01 15:59:26Z kesr $
 * ------------------------------------------------------------------------- */

#include <stdint.h>
#include <stdio.h>
#include <reg_stm32f4xx.h>
#include <reg_ctboard.h>
#include "hal_common.h"
#include "hal_exti.h"
#include "hal_ct_lcd.h"
#include "hal_timer.h"
#include "hal_rcc.h"

/* -- defines
 * ------------------------------------------------------------------------- */
#define NUMBER_OF_TIMER_2_INTERRUPTS (uint32_t)500
#define RELOAD_VALUE                 (uint32_t)84000
#define COUNTER_VALUE                TIM2->CNT


/* -- local function declarations
 * ------------------------------------------------------------------------- */
static void print_results(void);
static uint8_t convert_uint32_t_to_string(char ret_val[], uint32_t value);


/* -- local variable declarations
 * ------------------------------------------------------------------------- */
static volatile hal_bool_t measurement_done = FALSE;
static uint32_t exti_interrupt_counter = 0;
static uint32_t tim2_interrupt_counter = 0;
static uint32_t min_latency = 100000;
static uint32_t max_latency = 0;
static uint32_t avg_latency = 0;
static uint32_t sum_latency = 0;
static uint32_t latency = 0;


/* -- M A I N
 * ------------------------------------------------------------------------- */

int main(void)
{
    hal_timer_base_init_t timer_init;
    
    // init display
    hal_ct_lcd_clear();
    hal_ct_lcd_color(HAL_LCD_RED, 0xffff);
    hal_ct_lcd_color(HAL_LCD_BLUE, 0u);
    hal_ct_lcd_color(HAL_LCD_GREEN, 0u);

    /* Enable peripheral */
    TIM2_ENABLE();

    // init timer & exti
    TIM2_RESET();

    timer_init.mode = HAL_TIMER_MODE_DOWN;
    timer_init.run_mode = HAL_TIMER_RUN_CONTINOUS;
    timer_init.prescaler = 0u;
    timer_init.count = RELOAD_VALUE;
    
    hal_timer_init_base(TIM2, timer_init);
    hal_timer_irq_set(TIM2, HAL_TIMER_IRQ_UE, ENABLE);

    hal_exti_init();

    // start timer
    hal_timer_start(TIM2);
    
    // wait for measurement to finish
    while(!measurement_done);
    
    // print out measurement
    avg_latency = sum_latency / tim2_interrupt_counter;
    print_results();

    while (1);
}

/* -- interrupt service routines
 * ------------------------------------------------------------------------- */

void TIM2_IRQHandler(void)
{
/// STUDENTS: To be programmed




/// END: To be programmed

    /* Check if interrupt fired and clear interrupt */

    if (hal_timer_irq_status(TIM2, HAL_TIMER_IRQ_UE)) {
        hal_timer_irq_clear(TIM2, HAL_TIMER_IRQ_UE);

/// STUDENTS: To be programmed




/// END: To be programmed
    }
}

void EXTI0_IRQHandler(void)
{
    /* Check if interrupt fired and clear interrupt */
    if (hal_exti_check_for_irq()) {
        hal_exti_clear_irq_flag();
        exti_interrupt_counter++;
    }
}

/* -- local function definition
 * ------------------------------------------------------------------------- */

/*
 * Prints the minimal, maximal and average interrupt latency and the number
 * of occured external interrupts to the display
 */
static void print_results()
{
/// STUDENTS: To be programmed




/// END: To be programmed
}

/*
 * Converts the uint32_t value into a string. The result of the conversion is stored
 * in ret_val[]. ret_val[] must be large enough to hold the string
 * The functions returns a negative value in the case of an error. Otherwise the number
 * of characters written is returned.
 */
static uint8_t convert_uint32_t_to_string(char ret_val[], uint32_t value)
{
    return sprintf(ret_val, "%d", value);
}
