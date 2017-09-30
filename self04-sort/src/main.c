#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "sorting.h"

void flush_stdin() {
	// Flushing stdin: https://stackoverflow.com/questions/7898215/how-to-clear-input-buffer-in-c
	char c;
	while ((c = getchar()) != '\n' && c != EOF) { }	
}

bool contains(char** wordlist, const size_t length, char* word) {
	for (int i = 0; i < length; i++) {
		if (strcmp(wordlist[i], word) == 0) {
			return true;
		}
	}
	return false;
}

void store(char* word, char** wordlist, const size_t index) {
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
		 printf("Enter word (max 30chars):");
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
	 print_wordlist(wordlist, count);
 }