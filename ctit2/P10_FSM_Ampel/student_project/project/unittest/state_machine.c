#include "CppUTest/TestHarness.h"

#include "stdio.h"
#include "action_handler.h"
#include "event_handler.h"
#include "hal_gpio.h"
#include "state_machine.h"
#include "timer.h"

TEST_GROUP(state_machine)
{
    void setup()
    {
        printf( "\n===================\n" );
        printf( "state_machine test\n" );
        printf( "===================\n" );
    }

    void teardown()
    {
    }
};

TEST( state_machine, check_state_change )
{
    printf( "check_state_change\n" );
    
    /// STUDENTS: To be programmed

    uint16_t i;
    uint16_t port;
    event_t event;
    
    // check initial state all orange
    fsm_init();
    timer_start( 200 );
    port = hal_gpio_output_read(GPIOA);
    printf( "GPIOA->ODR state is: %04X\n", port );
    
    // check through all colors set
    CHECK_TEXT( COLOR_NO == ( port & 0x03 ), "Should be no color" );
    CHECK_TEXT( COLOR_NO == ( port >> 2 & 0x03 ), "Should be no color" );
    for( i = 2; i < 6; i++ )
    {
        printf( "counter is %d color is 0x%02X: \n", i, port >> 2 * i & 0x03 );
        CHECK_TEXT( COLOR_YELLOW == ( port >> 2 * i & 0x03 ), 
            "Color should be Yellow on all relevant ports" ); 
    }
    
    // check changing of state occurs after desired number of calls
    port = hal_gpio_output_read(GPIOA);
    printf( "current output of GPIOA->ODR is 0x%04X\n", port );
    for( i = 0; i < 250; i++ )
    {
        event = eh_get_event();        
        fsm_handle_event( event );
        if( port != hal_gpio_output_read( GPIOA ) )
        {
            break;
        }
    }
    port = hal_gpio_output_read( GPIOA );
    printf( "timeout occured after %d fsm_handle_event calls\n", i );
    printf( "New port value GPIOA->ODR is: 0x%04X\n", port );
    
    // check new state of leds all corresponds to new settings
    CHECK_TEXT( COLOR_NO    == ( port >> SIGNAL_CAR_W  & 0x03 ), "Should be no color" );
    CHECK_TEXT( COLOR_NO    == ( port >> SIGNAL_CAR_S  & 0x03 ), "Should be no color" );
    CHECK_TEXT( COLOR_RED   == ( port >> SIGNAL_CAR_E1 & 0x03 ), "Should be red color" );
    CHECK_TEXT( COLOR_GREEN == ( port >> SIGNAL_CAR_E2 & 0x03 ), "Should be green color" );
    CHECK_TEXT( COLOR_GREEN == ( port >> SIGNAL_PED_W  & 0x03 ), "Should be green color" );
    CHECK_TEXT( COLOR_RED   == ( port >> SIGNAL_PED_E  & 0x03 ), "Should be red color" );
    
    /// END: To be programmed
    
    printf( "\n" );
}