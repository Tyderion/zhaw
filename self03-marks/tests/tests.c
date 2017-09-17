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
#include <stdbool.h>
#include "CUnit/Basic.h"
#include "test_utils.h"
#include "../src/marks.h"

#ifndef TARGET // must be given by the make file --> see test target
#error missing TARGET define
#endif

/// @brief The name of the STDOUT text file.
#define OUTFILE "stdout.txt"
/// @brief The name of the STDERR text file.
#define ERRFILE "stderr.txt"

#define INFILE "infile.input"

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

static void test_get_mark_above_6(void)
{
    Mark mark = get_mark(70, 60);
    CU_ASSERT_EQUAL(mark, Six);
}

static void test_get_mark_exact_5(void)
{
    Mark mark = get_mark(50, 60);
    CU_ASSERT_EQUAL(mark, Five);
}

static void test_get_mark_round_up_6(void)
{
    Mark mark = get_mark(55, 60);
    CU_ASSERT_EQUAL(mark, Six);
}

static void test_get_mark_round_down_5(void)
{
    Mark mark = get_mark(54, 60);
    CU_ASSERT_EQUAL(mark, Five);
}

static void test_get_mark_minimal(void)
{
    Mark mark = get_mark(0, 60);
    CU_ASSERT_EQUAL(mark, One);
}

static void test_statistics_basic(void) {
    int* grades[2] = {60, 40};
    Statistic t = compute_statistics(
        grades, 2, 60
    );

    CU_ASSERT_EQUAL(t.best_mark, 6);
    CU_ASSERT_EQUAL(t.worst_mark, 4);
    CU_ASSERT_EQUAL(t.num_students, 2);
    CU_ASSERT_EQUAL(t.p6, 60);
    CU_ASSERT_TRUE(t.average > 4.99);
    CU_ASSERT_TRUE(t.average < 5.01);
    CU_ASSERT_EQUAL(t.above_4, 2);
    CU_ASSERT_EQUAL(t.distribution[0], 0);
    CU_ASSERT_EQUAL(t.distribution[1], 0);
    CU_ASSERT_EQUAL(t.distribution[2], 0);
    CU_ASSERT_EQUAL(t.distribution[3], 1);
    CU_ASSERT_EQUAL(t.distribution[4], 0);
    CU_ASSERT_EQUAL(t.distribution[5], 1);
}

static void test_statistics_more_grades(void) {
    const int num_students = 6;
    int* grades[6] = {60, 43, 24, 25, 57, 77};
    Statistic t = compute_statistics(
        grades, num_students, 60
    );

    CU_ASSERT_EQUAL(t.best_mark, 6);
    CU_ASSERT_EQUAL(t.worst_mark, 4);
    CU_ASSERT_EQUAL(t.num_students, num_students);
    CU_ASSERT_EQUAL(t.p6, 60);
    CU_ASSERT_TRUE(t.average > 4.49);
    CU_ASSERT_TRUE(t.average < 4.51);
    CU_ASSERT_EQUAL(t.above_4, 2);
    CU_ASSERT_EQUAL(t.distribution[0], 0);
    CU_ASSERT_EQUAL(t.distribution[1], 1);
    CU_ASSERT_EQUAL(t.distribution[2], 1);
    CU_ASSERT_EQUAL(t.distribution[3], 1);
    CU_ASSERT_EQUAL(t.distribution[4], 0);
    CU_ASSERT_EQUAL(t.distribution[5], 1);
}

// static void test_date_main_invalid_leap(void)
// {
// 	// arrange
// 	const char *out_txt[] = {
// 		"Please enter a date in the format '15.5.2007'.\n",
// 		"The date '29.2.2001' is invalid.\n"
// 	 };
// 	const char *err_txt[] = { };
// 	// act
// 	int exit_code = system(XSTR(TARGET) " 1>" OUTFILE " 2>" ERRFILE " < " INFILE);
// 	// assert
// 	CU_ASSERT_NOT_EQUAL(exit_code, 0);
// 	assert_lines(OUTFILE, out_txt, sizeof(out_txt)/sizeof(*out_txt));
// 	assert_lines(ERRFILE, err_txt, sizeof(err_txt)/sizeof(*err_txt));
// }

/**
  * @brief Registers and runs the tests.
  */
int main(void)
{
    // setup, run, teardown
    TestMainBasic("Selbstudium 03  - Marks - Statistics", setup, teardown
        , test_get_mark_above_6
        , test_get_mark_exact_5
        , test_get_mark_round_down_5
        , test_get_mark_round_up_6
        , test_get_mark_minimal
        , test_statistics_basic
        , test_statistics_more_grades
    );
}
