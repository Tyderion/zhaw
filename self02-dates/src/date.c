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
 * @brief Implementation File, See documentation in header.
 */
#include <stdio.h>
#include <stdlib.h>
#include "date.h"

static const int MONTHS_LENGTHS[Dec] = { 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };


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
