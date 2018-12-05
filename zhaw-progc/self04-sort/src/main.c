#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "sort.h"

/**
 * @file
 * @brief Main Entry point with the main function which gets called when the program is executed.
 */


/**
 * @brief Checks if the list already contains word
 * @param[in] wordlist the list of words the user entered
 * @param[in] length the length of the list
 * @param[in] word the newest entered word
 * @returns true if the word is not already in the wordlist
 */
static bool contains(char** wordlist, const size_t length, char* word) {
	for (int i = 0; i < length; i++) {
		if (strcmp(wordlist[i], word) == 0) {
			return true;
		}
	}
	return false;
}

/**
 * @brief Stores the word in the list at the correct position by allocating enough memory
 * @param[in] word the newest entered word
 * @param[in] wordlist the list of words the user entered
 * @param[in] index the index to store the word at
 */
static void store(char* word, char** wordlist, const size_t index) {
	const size_t length = strlen(word);
	wordlist[index] = malloc((length+1) * sizeof(char));
	strcpy(wordlist[index], word);
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
 int main(int argc, char *argv[]) {
	size_t count = 0;
	 char *wordlist[100];
	 while (count < 100) {
		 printf("Enter word (max 30chars):\n");
		 char word[30];
		 scanf("%s", word);
		 if (strcmp(word, "ZZZ") == 0) {
			 break;
		 }
		 if (!contains(wordlist, count, word)) {
			store(word, wordlist, count);
			count++;
		 } else {
			 printf("Duplicate word %s\n", word);
		 }
	 }
	 sort_wordlist(wordlist, count);
	 print_wordlist((char const * const *)wordlist, count);
 }