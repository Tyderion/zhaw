#include "person.h"

#ifndef LIST_H
#define LIST_H

typedef struct LE ListElement; // ListElement anstelle von struct LE verwendbar.
struct LE
{
    Person content;    // In diesem Listenelement gespeicherte Person.
    ListElement *next; // Pointer auf das nächstfolgende Element in der Liste.
};

ListElement le;

/**
 * @brief   inserts a single person into the list
 * @param[in] person the person to insert
 */
void insert_person(const Person *person);

/**
 * @brief   removes the person at index index from he list
 * @param[in] index the index of the person to remove
 */
void remove_person(const int index);

/**
 * @brief  clears the list and removes all registered Persons.
 */
void clear_people();

#endif /* LIST_H */

