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
	//printf("points: %d, p6: %d\n", p, p6);
	const double mark_raw = 1 + (5 * p) / (double)p6;
	Mark rounded = floor(mark_raw + 0.5);
	//printf("points: %d, rounded: %d\n", p, rounded);
	//printf("Rounded: %d, raw: %f, points: %d, p6: %d\n", rounded, mark_raw, p, p6);
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
 }