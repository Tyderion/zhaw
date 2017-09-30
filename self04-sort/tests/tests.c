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
#include "../src/sorting.h"

#ifndef TARGET // must be given by the make file --> see test target
#error missing TARGET define
#endif

/// @brief The name of the STDOUT text file.
#define OUTFILE "stdout.txt"
/// @brief The name of the STDERR text file.
#define ERRFILE "stderr.txt"

#define INFILE_NO_ERROR "infile_no_error.input"
#define INFILE_ERRORS "infile_errors.input"

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

static void test_sort_list(void)
{
    char* list[] = { "b", "c", "a" };
    sort_wordlist(list, 3);
    CU_ASSERT_EQUAL(list[0], "a");
    CU_ASSERT_EQUAL(list[1], "b");
    CU_ASSERT_EQUAL(list[2], "c");
}


static void test_sort_list_sorted(void)
{
    char* list[] = { "a", "b", "c" };
    sort_wordlist(list, 3);
    CU_ASSERT_EQUAL(list[0], "a");
    CU_ASSERT_EQUAL(list[1], "b");
    CU_ASSERT_EQUAL(list[2], "c");
}

static void test_sort_list_2(void)
{
    char* list[] = { "c", "b", "a" };
    sort_wordlist(list, 3);
    CU_ASSERT_EQUAL(list[0], "a");
    CU_ASSERT_EQUAL(list[1], "b");
    CU_ASSERT_EQUAL(list[2], "c");
}

static void test_sort_list_3(void)
{
    char* list[] = { "a", "c", "b" };
    sort_wordlist(list, 3);
    CU_ASSERT_EQUAL(list[0], "a");
    CU_ASSERT_EQUAL(list[1], "b");
    CU_ASSERT_EQUAL(list[2], "c");
}




/*
static void test_main_no_error(void)
{
	// arrange
	const char *out_txt[] = {
        "Please enter the next score. Enter -1 to end entering scores.\n",
        "Please enter the next score. Enter -1 to end entering scores.\n",
        "Please enter the next score. Enter -1 to end entering scores.\n",
        "Please enter the next score. Enter -1 to end entering scores.\n",
        "Please enter the next score. Enter -1 to end entering scores.\n",
        "Please enter the next score. Enter -1 to end entering scores.\n",
        "Please enter the next score. Enter -1 to end entering scores.\n",
        "Please enter the minimal points for grade 6\n",
		"--------------------------------------------------------\n",
        "Statistics (6 students, 60 points for mark 6.\n",
        "\n",
        "Grade 1: 0\n",
        "Grade 2: 1\n",
        "Grade 3: 1\n",
        "Grade 4: 1\n",
        "Grade 5: 0\n",
        "Grade 6: 3\n",
        "\n",
        "Best Mark: 6\n",
        "Worst Mark: 2\n",
        "Average: 4.500000\n",
        "Mark >= 4: 4 students (67%)\n",
        "--------------------------------------------------------\n"
	 };
	const char *err_txt[] = { };
	// act
	int exit_code = system(XSTR(TARGET) " 1>" OUTFILE " 2>" ERRFILE " < " INFILE_NO_ERROR);
	// assert
	CU_ASSERT_EQUAL(exit_code, 0);
	assert_lines(OUTFILE, out_txt, sizeof(out_txt)/sizeof(*out_txt));
	assert_lines(ERRFILE, err_txt, sizeof(err_txt)/sizeof(*err_txt));
}


static void test_main_errors(void)
{
	// arrange
	const char *out_txt[] = {
        "Please enter the next score. Enter -1 to end entering scores.\n",
        "Please enter numbers for the scores.\n",
        "Please enter the next score. Enter -1 to end entering scores.\n",
        "Students cannot score negative points.\n",
        "Please enter positive numbers or -1 to abort.\n",
        "Please enter the next score. Enter -1 to end entering scores.\n",
        "No scores have been entered.\n"
	 };
	const char *err_txt[] = { };
	// act
	int exit_code = system(XSTR(TARGET) " 1>" OUTFILE " 2>" ERRFILE " < " INFILE_ERRORS);
	// assert
	CU_ASSERT_EQUAL(exit_code, 0);
	assert_lines(OUTFILE, out_txt, sizeof(out_txt)/sizeof(*out_txt));
	assert_lines(ERRFILE, err_txt, sizeof(err_txt)/sizeof(*err_txt));
}
*/
/**
  * @brief Registers and runs the tests.
  */
int main(void)
{
    // setup, run, teardown
    TestMainBasic("Selbstudium 03  - Marks - Statistics", setup, teardown
        , test_sort_list
        , test_sort_list_2
        , test_sort_list_3
        , test_sort_list_sorted
    );
}
