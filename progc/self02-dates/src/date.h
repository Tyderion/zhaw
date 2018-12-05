#include <stdbool.h>
/**
 * @file
 * @brief Header File with Date definitions and Function defintions.
 */

/**
 * @brief Represents a Month
*/
typedef enum {
    Jan = 1,
    Feb,
    Mar,
    Apr,
    May,
    Jun,
    Jul,
    Aug,
    Sep,
    Oct,
    Nov,
    Dec
} Month;

/**
 * @brief Represents a date
*/
typedef struct
{
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
Date next_date(const Date *date);

/**
 * @brief   validates the given date
 * @param[in] date the date to validate
 * @returns true if the date is valid, false otherwise
 */
bool valid_date(const Date *date);
