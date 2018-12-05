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

#define COINS_COUNT 7
const int COINS[COINS_COUNT] = { 500, 200, 100, 50, 20, 10, 5 };


int calculate(int* amount, int coin) {
	int counter = 0;
	while (*amount >= coin) {
	 	counter++;
	 	*amount = *amount - coin;
	}
	return counter;
}

/**
 * @brief Main entry point.
 * @param[in] argc  The size of the argv array.
 * @param[in] argv  The command line arguments
 *                  with argv[0] being the command call
 *                  argv[1] the 1st argument, ...
 *                  argv[argc-1] the last argument.
 * @returns Returns the rest of the calculation or
 *                  255 on failure
 */
int main(int argc, char* argv[])
{
	if (argc == 1) {
		return 255;
	}
	// begin students to add code for task 4.2
	int rappen = 0;
	int res = sscanf(argv[1], "%d", &rappen);
	if (res != 1 || rappen < 0) {
	 //(void)printf("Bitte ein Betrag als Argument mitgeben.");
	 return 255;
	}
	(void)printf("CHF %.2f:\n", rappen/100.0);
	for (int i = 0; i < COINS_COUNT; i++) {
		int amount = calculate(&rappen, COINS[i]);
		(void)printf("- %d x %.2f\n", amount, COINS[i]/100.0);
	}
	if (rappen == 0) {
		(void)printf("Kein Rest\n");
	} 
	else {
		(void)printf("Rest = %.2f\n", rappen/100.0);
	}
	// end students to add code
	return rappen; // rest = 0 = success
}

