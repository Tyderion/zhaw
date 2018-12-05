/* ----------------------------------------------------------------------------
 * --  _____       ______  _____                                              -
 * -- |_   _|     |  ____|/ ____|                                             -
 * --   | |  _ __ | |__  | (___    Institute of Embedded Systems              -
 * --   | | | '_ \|  __|  \___ \   Zuercher Hochschule Winterthur             -
 * --  _| |_| | | | |____ ____) |  (University of Applied Sciences)           -
 * -- |_____|_| |_|______|_____/   8401 Winterthur, Switzerland               -
 * ----------------------------------------------------------------------------
 */
/**
 * @file
 * @brief Lab implementation
 */
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

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
	char c;
	int words = 0, chars = 0;
	bool had_char = false;
	while ((c = getchar()) != '\n' && c != EOF)
	{
		chars++;
		if ((c == ' ' || c == '\t') && had_char)
		{
			words++;
		}
		else
		{
			had_char = true;
		}
	}
	words++;
	printf("Characters: %d\n", chars);
	printf("Words: %d\n", words);
	return EXIT_SUCCESS;
}
