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
#include <stdio.h>
#include <stdlib.h>
#include "date.h"

static const int MONTHS_LENGTHS[12] = { 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };


static bool leap_year(int year) {
	return (year % 4 == 0 && year % 100 != 0) || year % 400 == 0;
}

static const int month_length(const Date* date) {
	int length = MONTHS_LENGTHS[date->month-1];
	if (Feb == date->month && leap_year(date->year)) {
		return length + 1;
	}
	return length;
}

static const bool valid_day(const Date* date) {
	int length = month_length(date);
	return date->day <= length;
}

bool valid_date(const Date* date) {
	return date->year >= 1583 
	&& date->month <= Dec
	&& valid_day(date);
}

Date next_date(const Date* date) {
	Date new = { date->day, date->month, date->year };
	if (month_length(date) > date->day){
		new.day++;
	} else if (date->month != Dec) {
		new.month++;
		new.day = 1;
	} else {
		new.month = Jan;
		new.day = 1;
		new.year++;
	}
	return new;
}

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
		return 0;
	}
	Date date = { day, month, year };
	if (!valid_date(&date)) {
		(void)printf("The date '%d.%d.%d' is invalid.\n", day, month, year);
	} else {
		Date next = next_date(&date);
		(void)printf("The date of the next day is: %d.%d.%d.\n", next.day, next.month, next.year);
	}
	return EXIT_SUCCESS;
}
