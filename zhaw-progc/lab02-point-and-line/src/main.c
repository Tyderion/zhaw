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
#include <math.h>

/**
 * @brief  point structure of double coordinates
 */
// begin students to add code for task 4.2

// end students to add code

/**
 * line structure of two points
 */
// begin students to add code for task 4.2
struct Point {
	double x;
	double y;
};
typdef struct {
	struct Point a;
	struct Point b;
} Line;
// end students to add code

/**
 * @brief Main entry point.
 * @param[in] argc  The size of the argv array.
 * @param[in] argv  The command line arguments,
 *                  with argv[0] being the command call,
 *                  argv[1] the 1st argument,
 *                  argv[argc-1] the last argument.
 * @returns Returns EXIT_SUCCESS (=0) on success and
 *                  EXIT_FAILURE (=1) on error.
 */
int main(int argc, char* argv[])
{
	// begin students to add code for task 4.2


	// parse arguments with error handling
	if (argc != 5) {
		return EXIT_FAILURE;
	}
	double xs[4];
	for (int i = 0; i < 4; i++) {
		int res = sscanf(argv[i+1], "%lf", &xs[i]);
		if (res != 1) {
			return EXIT_FAILURE;
		}
	}
	// make one line variable an initialize from the parsed arguments
	Line line = {
		{ xs[0],  xs[1] },
		{  xs[2],  xs[3] }
	};§
	// print the line variable in the following format: 
	//	(void)printf("line %g/%g-%g/%g\n", ...);
	(void)printf("line %g/%g-%g/%g\n", line.a.x, line.a.y, line.b.x, line.b.y);

	// end students to add code
	
	return EXIT_SUCCESS;
}

