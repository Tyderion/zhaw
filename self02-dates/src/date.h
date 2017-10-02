#include <stdbool.h>
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
 * @brief Header FIle with date definitions and function defintions.
 */

/**
 * @brief Represents a Month
*/
typedef enum Months {
    Jan=1, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec
} Month;

/**
 * @brief Represents a date
*/
typedef struct {
    /** 
     *@brief represents the day in the month
    */
    int day;
    /** 
     *@brief represents the month in the year
    */
    Month month;
    /** 
     *@brief represents the year
    */
    int year;
} Date;

/**
 * @brief   computes next day
 * @param[in] date the date to copmute the next of
 * @returns the next day
 */
 Date next_date(const Date* date);

 /**
 * @brief   validates the given date
 * @param[in] date the date to validate
 * @returns true if the date is valid, false otherwise
 */
 bool valid_date(const Date* date);
 