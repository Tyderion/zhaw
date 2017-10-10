#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "person.h"

char *string_person(const Person *person)
{
    size_t size = strlen(person->name) + strlen(person->firstname) + 6;
    char* result = malloc(size * sizeof(char));
    sprintf(result, "%s %s, %d", person->name, person->firstname, person->age);
    return result;
}

int compare_person(const Person *p1, const Person *p2)
{
    int compare_name = strcmp(p1->name, p2->name);
    // printf("%s vs %s: %d", p1->name, p2->name, compare_name);
    if (compare_name == 0) {
        int compare_first = strcmp(p1->firstname, p2->firstname);
        if (compare_first == 0) {
            return p1->age > p2->age ? 1 : (p1->age < p2->age ? -1 : 0);
        }
        return compare_first;
    }
    return compare_name;
}