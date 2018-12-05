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

	statistic.average = sum / student_count; 

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
