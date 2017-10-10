#include <stdio.h>
#include "person.h"

char *string_person(const Person *person)
{
    char* result = malloc(50 * sizeof(char));
    printf("Name: %s", person->name);
    return sprintf(result, "%s %s, %d", person->name, person->firstname, person->age);
}

int compare_person(const Person *p1, const Person *p2)
{
    return 0;
}