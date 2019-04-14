/******************************************************************************
// Aufgabe:    Simple Shell   
// File:       sishell.c
// Fach:       Betriebssysteme
// Autor:      M. Thaler
// Version:    v.fs19
******************************************************************************/

#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <stdlib.h>
#include <sys/types.h>
#include <fcntl.h> 
#include <sys/wait.h>

#include "readline.h"

//*****************************************************************************

#define MAX_ARGV  16                    // max number of command line arguments
#define STR_LEN  255                    // max length of command line arguments

//*****************************************************************************
// split command line into tokens and store tokens -> argv[]

// !!!! only one token is implemented here 

void tokenizeCommand(char *cmdLine, char *argv[]) {
    const char *errMsg = "\n*** too many arguments ***\n";
    int idx = 0;
    char* token;
     
    argv[idx++] = strtok(cmdLine," \t\n");// get first word

    while((token = strtok(NULL, " \t\n"))) {
        argv[idx] = token;
        idx++;
    }

    if (idx < MAX_ARGV) {
        argv[idx] = NULL;  // terminate argument list by NULL
    } else {
        printf("%s", errMsg);
        argv[0] = NULL;
    }

}

//-----------------------------------------------------------------------------
// execute an external command, exit on failure of fork

void externalCommand(char *argv[]) {
    pid_t PID;                          // process identifier
    if ((PID = fork()) == 0) {          // fork child process
        execvp(argv[0],  &argv[0]);     // execute command
        printf("!!! panic !!!\n");      // should not come here
            exit(-1);                   // if we came here ... we have to exit
    }
    else if (PID < 0) {
        printf("fork failed\n");        // fork didn't succeed
        exit(-1);                       // terminate sishell
    }
    else  {                             // here we are parents
        wait(0);                        // wait for child process to terminate
    }
}

//-----------------------------------------------------------------------------
// execute command if not NULL pointer (invalid or "empty" command)

void executeCommand(char *argv[]) {
    if (argv[0] != NULL) {
        externalCommand(argv);
    }
}

//-----------------------------------------------------------------------------
// main program for shell

int main(void) {
    char  *argv[MAX_ARGV];              // pointer to command line arguments
    char  buf[STR_LEN];                 // buffer for command line and command

    while (1) {    
        printf("si ? ");                // print prompt
        readline(buf, STR_LEN);         // read one line from stdin 
        tokenizeCommand(buf, argv);     // split command line into tokens
        executeCommand(argv);           // execute command
    }      
    exit(0);
}

//*****************************************************************************
