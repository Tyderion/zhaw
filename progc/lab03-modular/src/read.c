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
// begin students to add code for task 4.1
// end students to add code

#include <stdio.h>
#include "read.h"

#define EOF -1 // end of file
#define EOL 10 // end of line
// abnormal return values
#define PARSE_ERROR -1
// ASCII Codes
#define ASCII_SPACE 32 // ' '
#define ASCII_DIGIT_0 48 // '0'
#define ASCII_DIGIT_9 57 // '9'

// conversion buffer
#define NO_POS -1
#define BUFFERSIZE  10

int getInt(int maxResult) {
    // end of input
    char* buffer[BUFFERSIZE];

    int result = 0;

    // read line: up to EOL or EOF (i.e. error while reading)
    int bytes = 0;
    char input;
    scanf("%c", &input);
    while ((input != EOL) && (input != EOF)) { // read whole line
        if (bytes < BUFFERSIZE) { // only buffer first n characters
            buffer[bytes] = input;
            bytes++;
        } else {
            result = PARSE_ERROR; // exceed buffer size, continue read line
        }
        scanf("%c", &input);
    }
    if (input == EOF) {
        result = READ_ERROR;
    }
    // check for numbers: skip leading and trailing spaces
    // (i.e. this includes all control chars below the space ASCII code)
    int pos = 0;
    while((pos < bytes) && (buffer[pos] <= ASCII_SPACE)) pos++; // skip SP
    int posOfFirstDigit = pos;
    int posOfLastDigit = NO_POS;
    while ((pos < bytes)
        && (buffer[pos] >= ASCII_DIGIT_0)
        && (buffer[pos] <= ASCII_DIGIT_9))
    {
        posOfLastDigit = pos;
        pos++;
    }
    while((pos < bytes) && (buffer[pos] <= ASCII_SPACE)) pos++; // skip SP
    // produce return value
    if (result != 0) {
        // previously detected read or parse error given
    } else if ((pos != bytes) || (posOfLastDigit == NO_POS)) {
        result = PARSE_ERROR;
    } else { // convert number
        for(int i = posOfFirstDigit; i <= posOfLastDigit; i++) {
            result = result * 10 + (buffer[i] - ASCII_DIGIT_0);
            if (result > maxResult) {
                result = PARSE_ERROR;
                break;
            }
        }
    }
    return result;
}