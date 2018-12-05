#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "list.h"
#include "person.h"

/**
 * @brief   inserts a single person into the list
 * @param[in] person the person to insert
 */
void insert_person(const Person *person)
{
  ListElement *nextElement = malloc(sizeof(ListElement));
  strcpy(nextElement->content.name, person->name);
  strcpy(nextElement->content.firstname, person->firstname);
  nextElement->content.age = person->age;

  // ListElement *current = &le;
  ListElement *previous = &le;
  while (previous->next != &le)
  {
    int comparison = compare_person(person, &previous->next->content);
    if (comparison < 0)
    {
      break;
    }
    previous = previous->next;
  }
  nextElement->next = previous->next;
  previous->next = nextElement;
}

/**
  * @brief   removes the person at index index from he list
  * @param[in] person the person to remove from the list, all 3 properties (firstname, lastname, age) have to match
  */
void remove_person(const int index)
{
  ListElement *ele = &le;
  int count = 0;
  while (ele->next != &le)
  {
    count++;
    if (index == count)
    {
      break;
    }
    ele = ele->next;
  }
  ListElement *to_remove = ele->next;
  ele->next = to_remove->next;
  free(to_remove);
}

/**
  * @brief  clears the list and removes all registered Persons.
  */
void clear_people()
{
  if (le.next == &le)
  {
    // List is empty
    return;
  }
  ListElement *ele = le.next;
  ListElement *next;
  while (ele->next != &le)
  {
    next = ele->next;
    free(ele);
    ele = next;
  }
  le.next = &le;
}

ListElement le = {
  next : &le
};