#include <stdio.h>
#include <stdlib.h>
#include "date.h"

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
 * @brief Main Entry point wiht the main functino which gets called when the program is executed.
 */

/**
 * @brief Main entry point.
 * @param[in] argc  The size of the argv array.
 * @param[in] argv  The command line arguments
 *                  with argv[0] being the command call
 *                  argv[1] the 1st argument, ...
 *                  argv[argc-1] the last argument.
 * @returns Returns EXIT_SUCCESS (=0) on success,
 *                  EXIT_FAILURE (=1) if more than one argument is given.
 */
 int main(int argc, char* argv[])
 {
     (void)printf("Please enter a date in the format '15.5.2007'.\n");
     int day, month, year;
     int successful = scanf("%d.%d.%d", &day, &month, &year);
     if (successful != 3) {
         (void)printf("Format wrong, please use the format 'day.month.year'.\n");
         return EXIT_FAILURE;
     }
     Date date = { day, month, year };
     if (!valid_date(&date)) {
         (void)printf("The date '%d.%d.%d' is invalid.\n", day, month, year);
         return EXIT_FAILURE;
     } else {
         Date next = next_date(&date);
         (void)printf("The date of the next day is: %d.%d.%d.\n", next.day, next.month, next.year);
         return EXIT_SUCCESS;
     }
 }