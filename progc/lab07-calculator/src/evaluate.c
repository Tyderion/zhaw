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
#include <assert.h>
#include <stdio.h>
#include "evaluate.h"

// begin students to add code for task 4.1

//static double nop(double v)           { ... }
//static double neg(double v)           { ... }
//static double add(double a, double b) { ... }
//static double sub(double a, double b) { ... }
//static double mul(double a, double b) { ... }
//static double div(double a, double b) { ... }

// end students to add code


/// map for unary functions
struct unaryLookup {
	char op; ///< operato as key
	// begin students to add code for task 4.1
	//... ///< function pointer to operator function
	// end students to add code
};

/// map for binary functions
struct binaryLookup {
	char op; ///< operator as key
	// begin students to add code for task 4.1
	//... ///< function pointer to operator function
	// end students to add code
};

/// lookup table for unary operations
static struct unaryLookup unaryLookupTable[] = {
	// begin students to add code for task 4.1
	// ...init key-value pairs
	// end students to add code
};

/// lookup table for binary operations
static struct binaryLookup binaryLookupTable[] = {
	// begin students to add code for task 4.1
	// ...init key-value pairs
	// end students to add code
};

double evaluateUnaryOp(char op, double right)
{
	// begin students to add code for task 4.1

	// end students to add code
	assert(!"unexpected operator");
}

double evaluateBinaryOp(char op, double left, double right)
{
	// begin students to add code for task 4.1

	// end students to add code
	assert(!"unexpected operator");
}
