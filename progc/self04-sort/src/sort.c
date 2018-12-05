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
 * @brief Implementation, see header file for docs.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "sort.h"

/**
 * @brief  swaps the pointers inplace
 * @param[in] xp pointer x
 * @param[in] yp pointer y
 */
static void swap(char **xp, char **yp)
{
   char* temp = *xp;
   *xp = *yp;
   *yp = temp;
}

/**
 * @brief  sorts the list inplace with a very simple selectionsort like algorithm
 * @param[in] wordlist the list of words to sort
 * @param[in] n the size of the list to sort
 */
static void selectionSort(char** wordlist, const size_t n) {
  int i, j;
  for (i = 0; i < n-1; i++)  {
	  for (j = i+1; j < n; j++) {
		  if (strcmp(wordlist[i], wordlist[j]) > 0) {
			  swap(&wordlist[i], &wordlist[j]);
			}	
		}
	}
}

 void sort_wordlist(char** wordlist, const size_t length) {
	 selectionSort(wordlist, length);
 }

 void print_wordlist(const char* const * wordlist, const size_t length) {
	 printf("Sorted:\n");
	for (int i = 0; i < length; i++) {
		printf("%s\n", wordlist[i]);
	}
 }
