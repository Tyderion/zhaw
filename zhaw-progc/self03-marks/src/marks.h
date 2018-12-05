#include <stdbool.h>
/**
 * @file
 * @brief Defines the datatypes and functions used in this module
 */
/**
 * @brief Represents a Mark between 1 and 6
*/
typedef enum Marks {
    One = 1,
    Two,
    Three,
    Four,
    Five,
    Six
} Mark;

/**
 * @brief Represents the statistics of one class
*/
typedef struct
{
        /** 
     *@brief represents the numbers of students
    */
    int num_students,
        /** 
     *@brief The minimal amount of points for a 6
    */
     p6,     
     /** 
     *@brief The best mark in the class
    */
     best_mark,
     /** 
     *@brief The worst mark in the class
    */ 
     worst_mark, 
     /** 
     *@brief The amount of grades above 4
    */
     above_4;

     /** 
     *@brief The average grade of the class
    */
    double average;

     /** 
     *@brief A list of how many students got which grade
    */
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

/**
 * @brief computes statistics for the students
 * @param[in] statistic the statistic to print to stout
 */
void print_statistic(Statistic statistic);    