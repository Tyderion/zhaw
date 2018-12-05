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
// begin students to add code for task 4.1
struct Point {
	double x;
	double y;
};

// end students to add code


/**
 * @brief Main entry point.
 * @param[in] argc  The size of the argv array.
 * @param[in] argv  The command line arguments,
 *                  with argv[0] being the command call,
 *                  argv[1] the 1st argument,
 *                  argv[argc-1] the last argument.
 * @returns Returns EXIT_SUCCESS (=0) on success.
 */
int main(int argc, char* argv[])
{
	double distance = 0.0;

	// begin students to add code for task 4.1

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
	// make two point variable p1 and p2 from the parsed arguments
	struct Point a = { xs[0], xs[1] };
	struct Point b = { xs[2], xs[3] };
	// calcuate the dx and dy values
	double dx = a.x - b.x;
	double dy = a.y - b.y;
	// calculate distance form dx and dy values

	distance = sqrt(dx*dx+dy*dy);
	// end students to add code

	(void)printf("distance = %g\n", distance);
	
	return EXIT_SUCCESS;
}
