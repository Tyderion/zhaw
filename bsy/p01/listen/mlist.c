//******************************************************************************
// File:    mlist.c
// Purpose: implementation mlist (a single linked list with header dummy)
// Author:  M. Thaler, 2012, (based on former work by J. Zeman and M. Thaler)
// Version: v.fs19
//******************************************************************************

#include <stdlib.h>

#include "commondefs.h"
#include "mthread.h"
#include "mlist.h"

//******************************************************************************
// macro to allocate new tnode_t

#define mlNewTNode() (tnode_t *)malloc(sizeof(tnode_t))

//******************************************************************************
mlist_t *mlNewList()
{
    mlist_t *list = (mlist_t *)malloc(sizeof(mlist_t));
    list->head = mlNewTNode();
    list->head->next = NULL;
    list->tail = list->head;
    list->numNodes = 0;
    list->iter = NULL;
    return list;
}
void mlDelList(mlist_t *list)
{
    tnode_t *node = list->head;
    tnode_t *currentNode;
    do
    {
        currentNode = node;
        if (currentNode->tcb)
        {
            free(currentNode->tcb);
        }
        if (currentNode->next)
        {
            node = currentNode->next;
        }
        free(currentNode);
    } while (node->next);
    free(list);
} // delete list including data

void mlEnqueue(mlist_t *list, mthread_t *tcb)
{
    tnode_t *node = mlNewTNode();
    node->tcb = tcb;
    node->next = NULL;
    list->tail->next = node;
    list->numNodes++;
    list->tail = node;
} // append tcb to list
mthread_t *mlDequeue(mlist_t *list)
{
    mthread_t *thread;
    if (list->head->next)
    {
        thread = list->head->next->tcb;
        tnode_t *firstNode = list->head->next;
        list->head = firstNode;
        firstNode->tcb = NULL;
    }
    else
    {
        thread = NULL;
    }

    return thread;
} // take first element out of ist
  // return ptr to tcb or NULL

void mlSortIn(mlist_t *list, mthread_t *tcb) {} // insert tcb sorted
                                                // according ready time

mthread_t *mlReadFirst(mlist_t *list)
{
    return NULL;
} // return ptr to first tcb in list,
  // but do not dequeue

unsigned int mlGetNumNodes(mlist_t *list)
{
    return 0;
} // return number of elements in list
void mlSetPtrFirst(mlist_t *list)
{

} // set iter pointer to first element
void mlSetPtrNext(mlist_t *list) {} // move iter pointer to next element
mthread_t *mlReadCurrent(mlist_t *list)
{
    return NULL;
} // return tcb by iter pointer
//******************************************************************************
