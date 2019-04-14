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

void printArray(char *array[], int length) {
    for(int i = 0; i < length; i++) {
        printf("Element %d is %s\n", i, array[i]);
    }
}

void tokenizeCommand(char *cmdLine, char *argv[], char* redir[]) {
    const char *errMsg = "\n*** too many arguments ***\n";
    int idx = 0;
    char* token;

    char *argv_temp[15];
     
    argv[idx++] = strtok(cmdLine," \t\n");// get first word
    while((token = strtok(NULL, " \t\n"))) {
        argv_temp[idx-1] = token;
        idx++;
    }

    printArray(argv_temp, idx-1);

    int argvIdx = 1;
    redir[0] = NULL;
    redir[1] = NULL;
    for (int i = 0; i < idx-1; i++) {
        char* current = argv_temp[i];
        if (strcmp(current, ">") == 0) {
            char* file = argv_temp[i+1];
            redir[1] = malloc(strlen(file)+1);
            strcpy(redir[1], file);
            i++;
        }  else if (strcmp(current, "<") == 0) {
            char* file = argv_temp[i+1];
            redir[0] = malloc(strlen(file)+1);
            strcpy(redir[0], file);
            i++;
        } else {
            printf("Adding %s to argv\n", current);
            argv[argvIdx] = current;
            argvIdx++;
        }
    }

    if (argvIdx < MAX_ARGV) {
        argv[argvIdx] = NULL;  // terminate argument list by NULL
    } else {
        printf("%s", errMsg);
        argv[0] = NULL;
    }

}

//-----------------------------------------------------------------------------
// execute an external command, exit on failure of fork

void externalCommand(char *argv[], char *redir[]) {
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

int internalCommand(char *argv[]) {
    if (strcmp(argv[0], "logout") == 0 || strcmp(argv[0], "exit") == 0) {
        exit(0);
    }
    if (strcmp(argv[0], "cd") == 0) {
        char* path = argv[1] == NULL ? getenv("HOME") : argv[1];
        chdir(path);
        return 1;
    }
    if (strcmp(argv[0], "cwd") == 0) {
        printf("%s\n", getcwd(NULL, 0));
        return 1;
    }
    return 0;
}

//-----------------------------------------------------------------------------
// execute command if not NULL pointer (invalid or "empty" command)

void executeCommand(char *argv[], char* redir[]) {

    int stdin_copy = dup(0);
    int stdout_copy = dup(1);
    if (redir[0] != NULL) {
        dup2(open(redir[0], O_RDONLY), 0);
    }
    if(redir[1] != NULL) {
        dup2(open(redir[1], O_CREAT | O_TRUNC | O_WRONLY, 0644), 1);
    }

    if (argv[0] != NULL) {
        if (internalCommand(argv) == 0) {
            externalCommand(argv, redir);
        }
    }

    if (redir[0] != NULL) {
        dup2(stdin_copy, 0);
        redir[0] = NULL;
    }

    if (redir[1] != NULL) {
        dup2(stdout_copy, 1);
        redir[1] = NULL;
    }
}

//-----------------------------------------------------------------------------
// main program for shell

int main(void) {
    char  *argv[MAX_ARGV];              // pointer to command line arguments
    char  buf[STR_LEN];                 // buffer for command line and command
    char *redir[2];

    while (1) {    
        printf("si ? ");                // print prompt
        readline(buf, STR_LEN);         // read one line from stdin 
        tokenizeCommand(buf, argv, redir);     // split command line into tokens
        executeCommand(argv, redir);           // execute command
    }      
    exit(0);
}

//*****************************************************************************
