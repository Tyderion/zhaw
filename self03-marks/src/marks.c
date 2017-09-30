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
#include <math.h>
#include "marks.h"

Mark get_mark(const int p, const int p6)
{
	const double mark_raw = 1 + (5 * p) / (double)p6;
	Mark rounded = floor(mark_raw + 0.5);
	if (rounded > Six) {
		return Six;
	}
	return rounded;
}

Statistic compute_statistics(const int *const points, const int student_count, const int p6)
{
	Statistic statistic = {
		student_count, p6, 0, 7, 0, 0.0, { 0, 0, 0, 0, 0, 0}
	};
	float sum = 0;
	for (int i = 0; i < student_count; i++) {
		int grade = get_mark(points[i], p6);
		statistic.distribution[grade-1]++;
		sum += grade;
	}

	for (int grade = 1; grade <= 6; grade++) {
		int distribution = statistic.distribution[grade-1];
		if (distribution > 0){

			if (grade < statistic.worst_mark) {
				statistic.worst_mark = grade;
			}
			if (grade > statistic.best_mark) {
				statistic.best_mark = grade;
			}
			if (grade >= 4) {
				statistic.above_4 += distribution;
			}

		}
	}
	statistic.average = sum / student_count; 
	
	return statistic;
}


void print_statistic(Statistic statistic) {
	printf("--------------------------------------------------------\n");
	printf("Statistics (%d students, %d points for mark 6.\n", statistic.num_students, statistic.p6);
	printf("\n");
	for (int grade = 1; grade <= 6; grade++) {
		printf("Grade %d: %d\n", grade, statistic.distribution[grade-1]);
	}
	printf("\n");

	printf("Best Mark: %d\n", statistic.best_mark);
	printf("Worst Mark: %d\n", statistic.worst_mark);
	printf("Average: %f\n", statistic.average);
	printf("Mark >= 4: %d students (%d%%)\n", statistic.above_4, (int)floor(100 * (float)statistic.above_4/(float)statistic.num_students + 0.5));

	printf("--------------------------------------------------------\n");
}


void flush_stdin() {
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
		printf("Please enter the minimal points for grade 6\n");
		scanf("%d", &p6);

		Statistic statistic = compute_statistics(grades, count, p6);
		print_statistic(statistic);
	 } else {
		 printf("No scores have been entered.\n");
	 }
	 return EXIT_SUCCESS;
 }