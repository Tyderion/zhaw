#include <stdio.h>
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
    return 0;
}