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

bool valid_date(const Date* date) {
	return true;
}

static const int getMonthLenght(const Date* date) {
	int length = MONTHS_LENGTHS[date->month-1];
	if (Feb == date-> month && leap_year(date->year)) {
		return length + 1;
	}
	return length;
}

Date next_date(const Date* date) {
	Date new = { date->day, date->month, date->year };
	if (getMonthLenght(date) > date->day){
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
	return EXIT_SUCCESS;
}
