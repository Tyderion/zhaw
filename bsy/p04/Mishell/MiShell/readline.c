/******************************************************************************
// File:    readline.c
// Fach:    BSy
// Autor:   M. Thaler
// Version: v.fs19
******************************************************************************/

#include <stdio.h>

/*****************************************************************************/
/* readline reads at most count-1 characters from stdin into the buffer buf  */
/* returns the number of characters (! must be less/equal : count-1          */

int readline(char *buf, int count) {
    char input;
    int size = 0;
    while(size < count - 1 && (input = getchar()) != '\n' && input != EOF) {
        buf[size] = input;
        size++;
    }

    buf[size] = '\0';

    return size;
}

/*****************************************************************************/
