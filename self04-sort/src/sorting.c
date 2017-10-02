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
#include <string.h>
#include <math.h>
#include "sorting.h"

void swap(char **xp, char **yp)
{
   char* temp = *xp;
   *xp = *yp;
   *yp = temp;
}

void bubbleSort(char* wordlist[], const size_t n) {
  int i, j;
  for (i = 0; i < n-1; i++)  {
	  for (j = i+1; j < n; j++) {
		  if (strcmp(wordlist[i], wordlist[j]) > 0) {
			  swap(&wordlist[i], &wordlist[j]);
			}	
		}
	}
}

/**
 * @brief   sorts the list in place
 * @param[in] wordlist the list to sort
 */
 void sort_wordlist(char* wordlist[100], const size_t length) {
	 bubbleSort(wordlist, length);
 }
 
 /**
  * @brief   prints the wordlist to stdout
  * @param[in] wordlist the list to print
  */
 void print_wordlist(char* wordlist[100], const size_t length) {
	 printf("Sorted:\n");
	for (int i = 0; i < length; i++) {
		printf("%s\n", wordlist[i]);
	}
 }
