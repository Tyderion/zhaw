#include <stdio.h>
#include "list.h"
#include "person.h"

/**
 * @brief   inserts a single person into the list
 * @param[in] person the person to insert
 */
void insert_person(const Person *person)
{
  printf("insert_person not yet  implemented!\n");
}

/**
  * @brief   removes the person at index index from he list
  * @param[in] person the person to remove from the list, all 3 properties (firstname, lastname, age) have to match
  */
void remove_person(const int index)
{
  printf("remove_person not yet  implemented!\n");
}

/**
  * @brief  clears the list and removes all registered Persons.
  */
void clear_people()
{
  printf("clear_people not yet implemented!\n");
}


ListElement le = {
  next: &le
};