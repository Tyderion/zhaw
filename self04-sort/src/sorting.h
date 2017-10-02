#include <stdbool.h>
/**
 * @file
 * @brief File with the definitions of the functions we use in this module
*/
/**
 * @brief   sorts the list in place
 * @param[in] wordlist the list to sort
 * @param[in] length the length of the list
 */
void sort_wordlist(char* wordlist[100], const size_t length);

/**
 * @brief   prints the wordlist to stdout
 * @param[in] wordlist the list to print
 * @param[in] length the length of the list
 */
void print_wordlist(char* wordlist[100], const size_t length);
