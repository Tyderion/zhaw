/* ----------------------------------------------------------------------------
 * --  _____       ______  _____                                              -
 * -- |_   _|     |  ____|/ ____|                                             -
 * --   | |  _ __ | |__  | (___    Institute of Embedded Systems              -
 * --   | | | '_ \|  __|  \___ \   Zuercher Hochschule Winterthur             -
 * --  _| |_| | | | |____ ____) |  (University of Applied Sciences)           -
 * -- |_____|_| |_|______|_____/   8401 Winterthur, Switzerland               -
 * ----------------------------------------------------------------------------
 */
/**
 * @file
 * @brief Lab implementation
 */
// begin students to add code for task 4.1

#include <stdbool.h>
#include "rectang.h"
// end students to add code
bool Rectangular(int a, int b, int c) {
    
    int aS = a*a;
    int bS = b*b;
    int cS = c*c;
    
    bool isRightAngled;
    if ((a == 0) && (b == 0) && (c == 0))
        isRightAngled = false;
    else if ((aS + bS) == cS)
        isRightAngled = true;
    else if ((aS + cS) == bS)
        isRightAngled = true;
    else if ((bS + cS) == aS)
        isRightAngled = true;
    else
        isRightAngled = false;

    return isRightAngled;
}