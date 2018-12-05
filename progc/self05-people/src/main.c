#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "list.h"
#include "person.h"

/**
 * @file
 * @brief Main Entry point with the main function which gets called when the program is executed.
 */

/**
 * @brief Flushes standard input so that no more characters are stored in the buffer
*/
static void flush_stdin()
{
	// Flushing stdin: https://stackoverflow.com/questions/7898215/how-to-clear-input-buffer-in-c
	char c;
	while ((c = getchar()) != '\n' && c != EOF)
	{
	}
}

static Person action_read_person()
{
	printf("Register new person.\n");
	printf("Firstname:\n");
	char first[20];
	scanf("%s", first);
	printf("Name:\n");

	char last[20];
	scanf("%s", last);
	printf("Age:\n");

	int age;
	int scanned = scanf("%d", &age);
	while (scanned != 1)
	{
		printf("Please enter a number for the age\n");
		flush_stdin();
		scanned = scanf("%d", &age);
	}
	Person p;
	strcpy(p.name, last);
	strcpy(p.firstname, first);
	p.age = age;
	return p;
}

static void action_remove_person()
{
	printf("Please enter the index of the person to remove\n");
	int index;
	scanf("%d", &index);
	remove_person(index);

	printf("Removed index %d.\n", index);
}

static void action_print_list()
{
	if (le.next == &le)
	{
		printf("The list is empty.\n");
	}
	else
	{
		printf("Currently the following people are stored:\n");
		ListElement *ele = &le;
		int index = 0;
		while (ele->next != &le)
		{
			ele = ele->next;
			index++;
			printf("%d: %s\n", index, string_person(&(ele->content)));
		}
	}
}

/**
 * @brief Main entry point.
 * @param[in] argc  The size of the argv array.
 * @param[in] argv  The command line arguments
 *                  with argv[0] being the command call
 *                  argv[1] the 1st argument, ...
 *                  argv[argc-1] the last argument.
 * @returns Returns EXIT_SUCCESS (=0) on success,
 *                  EXIT_FAILURE (=1) if more than one argument is given.
 */
int main(int argc, char *argv[])
{
	char *input = malloc(1 * sizeof(char));
	do
	{
		printf("Please choose your operation from I(nsert), R(emove), S(how), C(lear) and E(nd):\n");
		scanf("%s", input);
		if (strcmp(input, "I") == 0)
		{
			Person p = action_read_person();
			insert_person(&p);
			printf("%s %s inserted.\n", p.firstname, p.name);
		}
		else if (strcmp(input, "R") == 0)
		{
			action_remove_person();
		}
		else if (strcmp(input, "C") == 0)
		{
			clear_people();
			printf("The list was cleared.\n");
		}
		else if (strcmp(input, "S") == 0)
		{
			action_print_list();
		}
		else if (strcmp(input, "E") != 0)
		{
			printf("Command not recognized\n");
		}
	} while (strcmp(input, "E") != 0);
	printf("Bye.\n");
	clear_people();
}