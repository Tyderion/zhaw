#include <stdbool.h>

typedef enum Marks {
    One = 1, Two, Three, Four, Five, Six
} Mark;

/**
 * @brief   computes the mark
 * @param[in] p the points the student has
 * @param[in] p6 the points needed for a 6
 * @returns the mark of the student
 */
 Mark get_mark(const int p, const int p6);
 