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
#include "../src/list.h"
#include "../src/person.h"

#ifndef TARGET // must be given by the make file --> see test target
#error missing TARGET define
#endif

/// @brief The name of the STDOUT text file.
#define OUTFILE "stdout.txt"
/// @brief The name of the STDERR text file.
#define ERRFILE "stderr.txt"
/// @brief The stimulus without any duplicate words
#define INFILE_NO_DUPLICATES "infile_no_duplicates.input"
/// @brief The stimulus with at least one duplicate
#define INFILE_DUPLICATE "infile_duplicate.input"

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

// Test person.h/person.c
static void test_compare_person_simple(void)
{
    Person p1 = {
        "Hans", "Müller", 20};
    Person p2 = {
        "Hans",
        "Meier",
        20};
    int comparison = compare_person(&p1, &p2);

    CU_ASSERT_EQUAL(comparison, 1);
}

static void test_compare_person_equal(void)
{
    Person p1 = {
        "Hans", "Müller", 20};
    Person p2 = {
        "Hans",
        "Müller",
        20};
    int comparison = compare_person(&p1, &p2);

    CU_ASSERT_EQUAL(comparison, 0);
}

static void test_compare_person_by_firstname(void)
{
    Person p1 = {
        "Hans", "Müller", 20};
    Person p2 = {
        "Manuel",
        "Müller",
        20};
    int comparison = compare_person(&p1, &p2);

    CU_ASSERT_EQUAL(comparison, -1);
}

static void test_compare_person_by_age(void)
{
    Person p1 = {
        "Manuel", "Müller", 21};
    Person p2 = {
        "Manuel",
        "Müller",
        20};
    int comparison = compare_person(&p1, &p2);

    CU_ASSERT_EQUAL(comparison, 1);
}

static void test_string_person(void)
{
    Person p = {
        "Peter", "Muster", 20};
    char *str = string_person(&p);
    CU_ASSERT_EQUAL(str, "Muster Peter, 20");
}

// Test list.h/list.c
static void test_empty_list(void)
{
    ListElement *next = le.next;
    CU_ASSERT_EQUAL(next, &le);
}

static void test_insert_person(void)
{
    Person p = {
        "Peter", "Muster", 20};
    insert_person(&p);
    Person inside = le.next->content;
    CU_ASSERT_EQUAL(inside.firstname, p.firstname);
    CU_ASSERT_EQUAL(inside.name, p.name);
    CU_ASSERT_EQUAL(inside.age, p.age);
}

static void test_remove_person(void)
{
    Person p = {
        "Peter", "Muster", 20};
    insert_person(&p);
    Person inside = le.next->content;
    CU_ASSERT_EQUAL(inside.name, p.name);

    remove_person(0);
    CU_ASSERT_EQUAL(le.next, &le);
}

static void test_clear_people(void)
{
    Person p = {
        "Peter", "Muster", 20};
    insert_person(&p);

    Person inside = le.next->content;
    CU_ASSERT_EQUAL(inside.name, p.name);
    clear_people();
    CU_ASSERT_EQUAL(le.next, &le);
}

// Test main functionality
// TODO
/*

static void test_main_no_duplicates(void)
{
	// arrange
	const char *out_txt[] = {
        "Enter word (max 30chars):\n",  
        "Enter word (max 30chars):\n",
        "Enter word (max 30chars):\n",
        "Enter word (max 30chars):\n",
        "Enter word (max 30chars):\n",
        "Sorted:\n",
        "a\n",
        "b\n",
        "c\n",
        "d\n"
	 };
	const char *err_txt[] = { };
	// act
	int exit_code = system(XSTR(TARGET) " 1>" OUTFILE " 2>" ERRFILE " < " INFILE_NO_DUPLICATES);
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
    TestMainBasic("Selbstudium 05  - Person Administration", setup, teardown
        , test_compare_person_simple
        , test_compare_person_by_firstname
        , test_compare_person_equal
        , test_compare_person_by_age
        , test_empty_list
        , test_insert_person
        , test_string_person
        , test_remove_person
        , test_clear_people
    );
}
