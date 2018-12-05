#include <stdio.h>
#include <stdlib.h>
#include "marks.h"
/**
 * @file
 * @brief Main Entry point wiht the main functino which gets called when the program is executed.
 */
/**
 * @brief Flushes standard input so that no more characters are stored in the buffer
*/
static void flush_stdin() {
	// Flushing stdin: https://stackoverflow.com/questions/7898215/how-to-clear-input-buffer-in-c
	char c;
	while ((c = getchar()) != '\n' && c != EOF) { }	
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
 int main(int argc, char *argv[])
 {
	 int grades[100];
	 int grade;
	 int count = 0;
	 while (true) {
		 if (count >= 100) {
			 break;
		 }
		printf("Please enter the next score. Enter -1 to end entering scores.\n");
		int scanned = scanf("%d", &grade);
		if (scanned != 1) {
			printf("Please enter numbers for the scores.\n");
			// We need to flush stdin because the entered string is not cleared when scanf can't parse the input.
			flush_stdin();
			continue;
		}
		if (grade == -1) {
			break;
		}
		if (grade < -1) {
			printf("Students cannot score negative points.\nPlease enter positive numbers or -1 to abort.\n");
			continue;
		}
		grades[count] = grade;
		count ++;
	 }
	 if (count > 0) {
		int p6;
		printf("Please enter the minimal points for grade 6 (-1 to skip the statistics and end the program)\n");
		 do {
			scanf("%d", &p6);
			if (p6 == -1) {
				break;
			}
			Statistic statistic = compute_statistics(grades, count, p6);
			print_statistic(statistic);
			printf("Do you want to compute the statistic with a different number of points? (-1 to end program)\n");
		 } while (true);
	 } else {
		 printf("No scores have been entered.\n");
	 }
	 return EXIT_SUCCESS;
 }