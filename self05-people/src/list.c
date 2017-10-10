#include "list.h"
#include "person.h"

/**
 * @brief   inserts a single person into the list
 * @param[in] person the person to insert
 */
void insert_person(const Person *person)
{

  ListElement *ele = &le;
  while (ele->next != &le) {
    ele = ele->next;
    int comparison = compare_person( &ele->content, person);
    if (comparison == 0) {

    } else if (comparison == 1){

    } else {

    }
    ListElement newEle = {
      *person, ele->next
    };
    ele->next = &newEle;
  } 

}

/**
  * @brief   removes the person at index index from he list
  * @param[in] person the person to remove from the list, all 3 properties (firstname, lastname, age) have to match
  */
void remove_person(const int index)
{
}

/**
  * @brief  clears the list and removes all registered Persons.
  */
void clear_people()
{
  le.next = &le;
  // missing dealloc
}


ListElement le = {
  next: &le
};