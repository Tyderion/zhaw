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
#include <string.h>
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
#define INFILE_ALL_OPERATIONS "infile_do_all_operations.input"

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
        "A", "A", 20};
    Person p2 = {
        "B",
        "B",
        20};
    int comparison = compare_person(&p1, &p2);

    int comparison2 = compare_person(&p2, &p1);
    CU_ASSERT_TRUE(comparison2 > 0);
}

static void test_compare_person_equal(void)
{
    Person p1 = {
        "Müller", "Hans", 20};
    Person p2 = {
        "Müller",
        "Hans",
        20};
    int comparison = compare_person(&p1, &p2);

    CU_ASSERT_EQUAL(comparison, 0);
}

static void test_compare_person_by_firstname(void)
{
    Person p1 = {
        "Müller", "Hans", 20};
    Person p2 = {
        "Müller",
        "Manuel",
        20};
    int comparison = compare_person(&p1, &p2);

    CU_ASSERT_TRUE(comparison < 0);
}

static void test_compare_person_by_age(void)
{
    Person p1 = {
        "Müller", "Manuel", 21};
    Person p2 = {
        "Müller",
        "Manuel",
        20};
    int comparison = compare_person(&p1, &p2);

    CU_ASSERT_TRUE(comparison > 0);
}

static void test_string_person(void)
{
    Person p = {
        "Muster", "Peter", 20};
    char *str = string_person(&p);
    CU_ASSERT_EQUAL(strcmp("Muster Peter, 20", str), 0);
}

static void test_string_person_long_name(void)
{
    Person p = {
        "Und Vielen Nachname", "Person mit Vornamen", 105};
    char *str = string_person(&p);
    CU_ASSERT_EQUAL(strcmp("Und Vielen Nachname Person mit Vornamen, 105", str), 0);
}

// Test list.h/list.c
static void test_empty_list(void)
{
    clear_people();
    ListElement *next = le.next;
    CU_ASSERT_EQUAL(next, &le);
}

static void test_insert_person(void)
{
    clear_people();
    Person p = {
        "Peter", "Muster", 20};
    insert_person(&p);
    Person inside = le.next->content;
    CU_ASSERT_EQUAL(strcmp(string_person(&inside), string_person(&p)), 0);
}

static void test_insert_multiple_people(void)
{
    clear_people();
    Person p1 = {
        "Peter", "Muster", 20};
    Person p2 = {
        "Patrick", "Muster", 20};
    insert_person(&p1);
    insert_person(&p2);
    CU_ASSERT_EQUAL(strcmp(string_person(&le.next->content), string_person(&p2)), 0);
    CU_ASSERT_EQUAL(strcmp(string_person(&le.next->next->content), string_person(&p1)), 0);
}

static void test_remove_person(void)
{
    clear_people();
    Person p = {
        "Peter", "Muster", 20};
    insert_person(&p);
    remove_person(1);
    CU_ASSERT_EQUAL(le.next, &le);
}

static void test_remove_second_person(void)
{
    clear_people();
    Person p = {
        "Muster", "Peter", 20};
    Person p2 = {
        "Hermann", "Peter", 20};
    insert_person(&p);
    insert_person(&p2);

    remove_person(2);
    CU_ASSERT_EQUAL(strcmp(le.next->content.name, "Hermann"), 0);;
    CU_ASSERT_EQUAL(le.next->next, &le);
}

static void test_clear_people(void)
{
    Person p = {
        "Muster", "Peter", 20};
    insert_person(&p);
    clear_people();
    CU_ASSERT_EQUAL(le.next, &le);
}

// Test main functionality

static void test_main_all_operations(void)
{
    // arrange
    const char *out_txt[] = {
        "Please choose your operation from I(nsert), R(emove), S(how), C(lear) and E(nd):\n",
        "Register new person.\n",
        "Firstname:\n",
        "Name:\n",
        "Age:\n",
        "Max Muster inserted.\n",
        "Please choose your operation from I(nsert), R(emove), S(how), C(lear) and E(nd):\n",
        "Register new person.\n",
        "Firstname:\n",
        "Name:\n",
        "Age:\n",
        "Markus Muster inserted.\n",
        "Please choose your operation from I(nsert), R(emove), S(how), C(lear) and E(nd):\n",
        "Currently the following people are stored:\n",
        "1: Muster Markus, 25\n",
        "2: Muster Max, 24\n",
        "Please choose your operation from I(nsert), R(emove), S(how), C(lear) and E(nd):\n",
        "Please enter the index of the person to remove\n"
        "Removed Muster Max, 24 from the list.\n",
        "Please choose your operation from I(nsert), R(emove), S(how), C(lear) and E(nd):\n",
        "Currently the following people are stored:\n",
        "1: Muster Markus, 25\n",
        "Please choose your operation from I(nsert), R(emove), S(how), C(lear) and E(nd):\n",
        "The list was cleared.\n",
        "Please choose your operation from I(nsert), R(emove), S(how), C(lear) and E(nd):\n",
        "The list is empty.\n",
        "Please choose your operation from I(nsert), R(emove), S(how), C(lear) and E(nd):\n",
        "Bye.\n"};
    const char *err_txt[] = {};
    // act
    int exit_code = system(XSTR(TARGET) " 1>" OUTFILE " 2>" ERRFILE " < " INFILE_ALL_OPERATIONS);
    // assert
    CU_ASSERT_EQUAL(exit_code, 0);
    assert_lines(OUTFILE, out_txt, sizeof(out_txt) / sizeof(*out_txt));
    assert_lines(ERRFILE, err_txt, sizeof(err_txt) / sizeof(*err_txt));
}

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
          , test_insert_multiple_people
          , test_string_person
          , test_string_person_long_name
          , test_remove_person
          , test_remove_second_person
          , test_clear_people
        //   , test_main_all_operations
      );
  }
