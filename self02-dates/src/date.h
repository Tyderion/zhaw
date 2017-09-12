#include <stdbool.h>

typedef enum Months {
    Jan=1, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec
} Month;

typedef struct {
    int day;
    Month month;
    int year;
} Date;

/**
 * @brief   computes next day
 * @param[in] date the date to copmute the next of
 * @returns the next day
 */
 Date next_date(Date* date);

 /**
 * @brief   validates the given date
 * @param[in] date the date to validate
 * @returns true if the date is valid, false otherwise
 */
 bool valid_date(const Date* date);
 