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
 * @brief Test suite for the given package.
 */
#include <stdio.h>
#include <stdlib.h>
#include "CUnit/Basic.h"
#include "test_utils.h"
#include "../src/date.h"

#ifndef TARGET // must be given by the make file --> see test target
#error missing TARGET define
#endif

/// @brief The name of the STDOUT text file.
#define OUTFILE "stdout.txt"
/// @brief The name of the STDERR text file.
#define ERRFILE "stderr.txt"


/// @brief The stimulus for a simple date
#define INFILE_VALID_DATE_SIMPLE "stim-valid-date-simple.input"
/// @brief The stimulus for an invalid date in a leap year
#define INFILE_VALID_DATE_INVALID_LEAP "stim-valid-date-invalid-leap.input"
/// @brief The stimulus for an invalid date because the year is out of range (too old)
#define INFILE_VALID_DATE_INVALID_AGE "stim-valid-date-invalid-age.input"
/// @brief The stimulus for an invalid date which is no date but a simple string.
#define INFILE_VALID_DATE_INVALID_NO_DATE "stim-valid-date-invalid-no-date.input"

// setup & cleanup
static int setup(void)
{
	remove_file_if_exists(OUTFILE);
	remove_file_if_exists(ERRFILE);
	return 0; // success
}

static int teardown(void)
{
	// Do nothing.
	// Especially: do not remove result files - they are removed in int setup(void) *before* running a test.
	return 0; // success
}

// tests
static void test_next_date_simple(void)
{
	// arrange
	Date date = { 1, Jan, 2000 };
	// act
	Date next = next_date(&date);
	// assert
	CU_ASSERT_EQUAL(next.day, 2);
	CU_ASSERT_EQUAL(next.month, Jan);
	CU_ASSERT_EQUAL(next.year, 2000);
}

static void test_next_date_end_of_jan(void)
{
	// arrange
	Date date = { 31, Jan, 2000 };
	// act
	Date next = next_date(&date);
	// assert
	CU_ASSERT_EQUAL(next.day, 1);
	CU_ASSERT_EQUAL(next.month, Feb);
	CU_ASSERT_EQUAL(next.year, 2000);
}

static void test_next_date_end_of_june(void)
{
	// arrange
	Date date = { 30, Jun, 2000 };
	// act
	Date next = next_date(&date);
	// assert
	CU_ASSERT_EQUAL(next.day, 1);
	CU_ASSERT_EQUAL(next.month, Jul);
	CU_ASSERT_EQUAL(next.year, 2000);
}

static void test_next_date_leap_year(void)
{
	// arrange
	Date date = { 28, Feb, 2000 };
	// act
	Date next = next_date(&date);
	// assert
	CU_ASSERT_EQUAL(next.day, 29);
	CU_ASSERT_EQUAL(next.month, Feb);
	CU_ASSERT_EQUAL(next.year, 2000);
}

static void test_next_date_not_leap_year(void)
{
	// arrange
	Date date = { 28, Feb, 2001 };
	// act
	Date next = next_date(&date);
	// assert
	CU_ASSERT_EQUAL(next.day, 1);
	CU_ASSERT_EQUAL(next.month, Mar);
	CU_ASSERT_EQUAL(next.year, 2001);
}

static void test_next_date_end_of_year(void)
{
	// arrange
	Date date = { 31, Dec, 2001 };
	// act
	Date next = next_date(&date);
	// assert
	CU_ASSERT_EQUAL(next.day, 1);
	CU_ASSERT_EQUAL(next.month, Jan);
	CU_ASSERT_EQUAL(next.year, 2002);
}


static void test_valid_date_too_old(void)
{
	// arrange
	Date date = { 31, Dec, 1582 };
	// act
	bool result = valid_date(&date);
	// assert
	CU_ASSERT_EQUAL(result, false);
}

static void test_valid_date_oldest(void)
{
	// arrange
	Date date = { 1, Jan, 1582 };
	// act
	bool result = valid_date(&date);
	// assert
	CU_ASSERT_EQUAL(result, false);
}

static void test_valid_date_jan(void)
{
	// arrange
	Date date_valid = { 31, Jan, 2000 };
	Date date_invalid = { 32, Jan, 2000 };
	// act
	bool valid = valid_date(&date_valid);
	bool invalid = valid_date(&date_invalid);
	// assert
	CU_ASSERT_EQUAL(valid, true);
	CU_ASSERT_EQUAL(invalid, false);
}

static void test_valid_date_feb_no_leap(void)
{
	// arrange
	Date date_valid = { 28, Feb, 1999 };
	Date date_invalid = { 29, Feb, 1999 };
	// act
	bool valid = valid_date(&date_valid);
	bool invalid = valid_date(&date_invalid);
	// assert
	CU_ASSERT_EQUAL(valid, true);
	CU_ASSERT_EQUAL(invalid, false);
}

static void test_valid_date_feb_leap(void)
{
	// arrange
	Date date_valid = { 29, Feb, 2000 };
	Date date_invalid = { 30, Feb, 2000 };
	// act
	bool valid = valid_date(&date_valid);
	bool invalid = valid_date(&date_invalid);
	// assert
	CU_ASSERT_EQUAL(valid, true);
	CU_ASSERT_EQUAL(invalid, false);
}

static void test_valid_date_march(void)
{
	// arrange
	Date date_valid = { 31, Mar, 2000 };
	Date date_invalid = { 32, Mar, 2000 };
	// act
	bool valid = valid_date(&date_valid);
	bool invalid = valid_date(&date_invalid);
	// assert
	CU_ASSERT_EQUAL(valid, true);
	CU_ASSERT_EQUAL(invalid, false);
}

static void test_valid_date_april(void)
{
	// arrange
	Date date_valid = { 30, Apr, 2000 };
	Date date_invalid = { 31, Apr, 2000 };
	// act
	bool valid = valid_date(&date_valid);
	bool invalid = valid_date(&date_invalid);
	// assert
	CU_ASSERT_EQUAL(valid, true);
	CU_ASSERT_EQUAL(invalid, false);
}

static void test_valid_date_may(void)
{
	// arrange
	Date date_valid = { 31, May, 2000 };
	Date date_invalid = { 32, May, 2000 };
	// act
	bool valid = valid_date(&date_valid);
	bool invalid = valid_date(&date_invalid);
	// assert
	CU_ASSERT_EQUAL(valid, true);
	CU_ASSERT_EQUAL(invalid, false);
}

static void test_valid_date_june(void)
{
	// arrange
	Date date_valid = { 30, Jun, 2000 };
	Date date_invalid = { 31, Jun, 2000 };
	// act
	bool valid = valid_date(&date_valid);
	bool invalid = valid_date(&date_invalid);
	// assert
	CU_ASSERT_EQUAL(valid, true);
	CU_ASSERT_EQUAL(invalid, false);
}

static void test_valid_date_july(void)
{
	// arrange
	Date date_valid = { 31, Jul, 2000 };
	Date date_invalid = { 32, Jul, 2000 };
	// act
	bool valid = valid_date(&date_valid);
	bool invalid = valid_date(&date_invalid);
	// assert
	CU_ASSERT_EQUAL(valid, true);
	CU_ASSERT_EQUAL(invalid, false);
}

static void test_valid_date_august(void)
{
	// arrange
	Date date_valid = { 31, Aug, 2000 };
	Date date_invalid = { 32, Aug, 2000 };
	// act
	bool valid = valid_date(&date_valid);
	bool invalid = valid_date(&date_invalid);
	// assert
	CU_ASSERT_EQUAL(valid, true);
	CU_ASSERT_EQUAL(invalid, false);
}

static void test_valid_date_september(void)
{
	// arrange
	Date date_valid = { 30, Sep, 2000 };
	Date date_invalid = { 31, Sep, 2000 };
	// act
	bool valid = valid_date(&date_valid);
	bool invalid = valid_date(&date_invalid);
	// assert
	CU_ASSERT_EQUAL(valid, true);
	CU_ASSERT_EQUAL(invalid, false);
}

static void test_valid_date_october(void)
{
	// arrange
	Date date_valid = { 31, Oct, 2000 };
	Date date_invalid = { 32, Oct, 2000 };
	// act
	bool valid = valid_date(&date_valid);
	bool invalid = valid_date(&date_invalid);
	// assert
	CU_ASSERT_EQUAL(valid, true);
	CU_ASSERT_EQUAL(invalid, false);
}

static void test_valid_date_november(void)
{
	// arrange
	Date date_valid = { 30, Nov, 2000 };
	Date date_invalid = { 31,Nov, 2000 };
	// act
	bool valid = valid_date(&date_valid);
	bool invalid = valid_date(&date_invalid);
	// assert
	CU_ASSERT_EQUAL(valid, true);
	CU_ASSERT_EQUAL(invalid, false);
}

static void test_valid_date_december(void)
{
	// arrange
	Date date_valid = { 31, Dec, 2000 };
	Date date_invalid = { 32, Dec, 2000 };
	// act
	bool valid = valid_date(&date_valid);
	bool invalid = valid_date(&date_invalid);
	// assert
	CU_ASSERT_EQUAL(valid, true);
	CU_ASSERT_EQUAL(invalid, false);
}

// tests
static void test_date_main_valid(void)
{
	// arrange
	const char *out_txt[] = { 
		"Please enter a date in the format '15.5.2007'.\n",
		"The date of the next day is: 2.1.2001.\n"
	 };
	const char *err_txt[] = { };
	// act
	int exit_code = system(XSTR(TARGET) " 1>" OUTFILE " 2>" ERRFILE " < " INFILE_VALID_DATE_SIMPLE);
	// assert
	CU_ASSERT_EQUAL(exit_code, 0);
	assert_lines(OUTFILE, out_txt, sizeof(out_txt)/sizeof(*out_txt));
	assert_lines(ERRFILE, err_txt, sizeof(err_txt)/sizeof(*err_txt));
}

static void test_date_main_invalid_year(void)
{
	// arrange
	const char *out_txt[] = { 
		"Please enter a date in the format '15.5.2007'.\n",
		"The date '1.1.1322' is invalid.\n"
	 };
	const char *err_txt[] = { };
	// act
	int exit_code = system(XSTR(TARGET) " 1>" OUTFILE " 2>" ERRFILE " < " INFILE_VALID_DATE_INVALID_AGE);
	// assert
	CU_ASSERT_NOT_EQUAL(exit_code, 0);
	assert_lines(OUTFILE, out_txt, sizeof(out_txt)/sizeof(*out_txt));
	assert_lines(ERRFILE, err_txt, sizeof(err_txt)/sizeof(*err_txt));
}

static void test_date_main_invalid_leap(void)
{
	// arrange
	const char *out_txt[] = { 
		"Please enter a date in the format '15.5.2007'.\n",
		"The date '29.2.2001' is invalid.\n"
	 };
	const char *err_txt[] = { };
	// act
	int exit_code = system(XSTR(TARGET) " 1>" OUTFILE " 2>" ERRFILE " < " INFILE_VALID_DATE_INVALID_LEAP);
	// assert
	CU_ASSERT_NOT_EQUAL(exit_code, 0);
	assert_lines(OUTFILE, out_txt, sizeof(out_txt)/sizeof(*out_txt));
	assert_lines(ERRFILE, err_txt, sizeof(err_txt)/sizeof(*err_txt));
}

static void test_date_main_invalid_no_date_input(void)
{
	// arrange
	const char *out_txt[] = { 
		"Please enter a date in the format '15.5.2007'.\n",
		"Format wrong, please use the format 'day.month.year'.\n"
	 };
	const char *err_txt[] = { };
	// act
	int exit_code = system(XSTR(TARGET) " 1>" OUTFILE " 2>" ERRFILE " < " INFILE_VALID_DATE_INVALID_NO_DATE);
	// assert
	CU_ASSERT_NOT_EQUAL(exit_code, 0);
	assert_lines(OUTFILE, out_txt, sizeof(out_txt)/sizeof(*out_txt));
	assert_lines(ERRFILE, err_txt, sizeof(err_txt)/sizeof(*err_txt));
}

/**
 * @brief Registers and runs the tests.
 */
int main(void)
{
	// setup, run, teardown
	TestMainBasic("Selbstudium 02  - Dates", setup, teardown
				  , test_next_date_simple
				  , test_next_date_end_of_jan
				  , test_next_date_leap_year
				  , test_next_date_not_leap_year
				  , test_next_date_end_of_year
				  , test_next_date_end_of_june
				  , test_valid_date_too_old
				  , test_valid_date_oldest
				  , test_valid_date_jan
				  , test_valid_date_feb_no_leap
				  , test_valid_date_feb_leap
				  , test_valid_date_march
				  , test_valid_date_april
				  , test_valid_date_may
				  , test_valid_date_june
				  , test_valid_date_july
				  , test_valid_date_august
				  , test_valid_date_september
				  , test_valid_date_october
				  , test_valid_date_november
				  , test_valid_date_december
				  , test_date_main_valid
				  , test_date_main_invalid_year
				  , test_date_main_invalid_leap
				  , test_date_main_invalid_no_date_input
				  );
}
