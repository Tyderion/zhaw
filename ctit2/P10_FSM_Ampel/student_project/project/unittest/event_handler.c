#include "CppUTest/TestHarness.h"

#include "stdio.h"
#include "hal_gpio.h"
#include "event_handler.h"
#include "timer.h"

TEST_GROUP(event_handler)
{
    void setup()
    {
        printf( "\n===================\n" );
        printf( "event_handler test\n" );
        printf( "===================\n" );
    }

    void teardown()
    {
    }
};

TEST( event_handler, test_timeout_event )
{
    printf( "test_timeout_event\n" );
    
    /// STUDENTS: To be programmed
    uint16_t i;
    event_t event;
    
    timer_start(5);
    for( i = 0; i < 5; i++ )
    {
        printf( "Iteration of Loop: %d ", i );
        event = eh_get_event();
     
        printf( "is Timeout Event: %d\n", event == TIME_OUT ? 1 : 0 );
    }
    
    CHECK_TEXT( i == 5 && event == TIME_OUT, "Timeout should occur after 5 counts" );
    
    /// END: To be programmed
    
    printf( "\n" );
}

TEST( event_handler, test_edge_and_signal_processing )
{
    printf( "test_edge_and_signal_processing\n" );
    
    /// STUDENTS: To be programmed
    event_t event;
    
    // first test: no event
    event = eh_get_event();
    CHECK_TEXT( NO_EVENT == event, "No event expected" );
    
    // second test: one event setting
    GPIOB->IDR |= 0x01;
    event = eh_get_event();
    printf( "Event is: %d\n", event );
    CHECK_TEXT( EV_CAR_W == event, "No event expected" );
    
    // now reset event: no event should happen
    GPIOB->IDR & ~0x01;
    event = eh_get_event();
    printf( "Event is: %d\n", event );
    CHECK_TEXT( NO_EVENT == event, "No event expected" );
    
    /// END: To be programmed
    
    printf( "\n" );
}