#include <stdbool.h>

typedef enum Marks {
    One = 1,
    Two,
    Three,
    Four,
    Five,
    Six
} Mark;

typedef struct
{
    int num_students, p6, best_mark, worst_mark, above_4;
    double average;
    int distribution[6];
} Statistic;

/**
 * @brief   computes the mark
 * @param[in] p the points the student has
 * @param[in] p6 the points needed for a 6
 * @returns the mark of the student
 */
Mark get_mark(const int p, const int p6);

/**
 * @brief computes statistics for the students
 * @param[in] points a list of points the students got
 * @param[in] student_count the number of students
 * @param[in] p6 the points needed for a 6
 * @returns statistic about this class
 */
Statistic compute_statistics(const int *const points, const int student_count, const int p6);