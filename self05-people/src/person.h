typedef struct {
    char name[20];
    char firstname[20];
    unsigned age;
} Person;

/**
 * @brief  computes a string representation of the person (useful for outputting on the console)
 * @param[in] person the person to convert to a string
 * @returns the string representation
 */
char* person_toString(const Person* person);

/**
 * @brief   compares two persons for sorting purposes
 * @param[in] wordlist the list to print
 * @param[in] length the length of the list
 * @returns 0 if both are equal, -1 if p1 is lexigraphically earlier than p2 and 1 if p1 is after p2
 */
int compare(const Person* p1, const Person* p2);