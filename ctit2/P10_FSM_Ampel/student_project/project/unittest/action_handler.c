#include "CppUTest/TestHarness.h"

#include "stdio.h"
#include "stdlib.h"
#include "reg_stm32f4xx.h"
#include "action_handler.h"

#include "hal_gpio.h"

TEST_GROUP(action_handler)
{
    void setup()
    {
        printf( "\n====================\n" );
        printf( "action_handler test\n" );
        printf( "====================\n" );
        
        // reset original value in register
        GPIOA->ODR = 0x00000000;
    }

    void teardown()
    {
    }
};

TEST( action_handler, port_writing_1 )
{
    uint16_t port_old;
    uint16_t port_new;
    
    printf( "port_writing_1\n" );
    
    /// STUDENTS: To be programmed
    
    port_old = hal_gpio_output_read( GPIOA );
    printf( "Initial state of GPIOA->ODR: %04X\n", port_old );
    
    ah_set_signal(SIGNAL_CAR_W, COLOR_RED);
    port_new = hal_gpio_output_read( GPIOA );
    printf( "New state of GPIOA->ODR: %04X", port_new );
    
    CHECK_TEXT( port_old != port_new, "old and new port should not be equal" );
    CHECK_TEXT( COLOR_RED == port_new, "old and new port should not be equal" );
    
    /// END: To be programmed
    
    printf( "\n" );
}

TEST( action_handler, port_writing_2)
{
    uint16_t port_old;
    uint16_t port_new;
    
    printf( "port_writing_2\n" );

    /// STUDENTS: To be programmed
    
    port_old = hal_gpio_output_read( GPIOA );
    printf( "Initial state of GPIOA->ODR: %04X\n", port_old );
    ah_set_signal(SIGNAL_CAR_W, COLOR_GREEN);
    
    // check register values identical
    port_new = hal_gpio_output_read( GPIOA );
    printf( "Current GPIOA->ODR state: %04X\n", port_new );
    CHECK_TEXT( COLOR_GREEN == port_new, "Value and Port should be the same" );
    
    // set all other port the same and check for identical output
    ah_set_signal(SIGNAL_CAR_W, COLOR_GREEN);
    ah_set_signal(SIGNAL_CAR_S, COLOR_GREEN);
    ah_set_signal(SIGNAL_CAR_E1, COLOR_GREEN);
    ah_set_signal(SIGNAL_CAR_E2, COLOR_GREEN);
    ah_set_signal(SIGNAL_PED_W, COLOR_GREEN);
    ah_set_signal(SIGNAL_PED_E, COLOR_GREEN);
    
    // check if all the bits combined still represent the given value
    port_new = hal_gpio_output_read( GPIOA );
    printf( "Current GPIOA->ODR state: %04X\n", port_new );
    port_old = ( COLOR_GREEN ) | ( COLOR_GREEN << 2 ) | ( COLOR_GREEN << 4 ) |
               ( COLOR_GREEN << 6 ) | ( COLOR_GREEN << 8 ) | ( COLOR_GREEN << 10 );
    CHECK_TEXT( port_old == port_new, "Value and Port should be the same" );
    
    /// END: To be programmed
    
    printf( "\n" );
}