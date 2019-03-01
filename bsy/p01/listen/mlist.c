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
    list->head->tcb = NULL;
    list->head->next = NULL;
    list->tail = list->head;
    list->iter = NULL;
    list->numNodes = 0;
    return list;
}

void mlDelList(mlist_t *list)
{
    tnode_t *last;
    tnode_t *iter = list->head;
    while (iter->next != NULL)
    {
        last = iter;
        iter = iter->next;
        free(last);
    }
    free(list->head);
}
void mlEnqueue(mlist_t *list, mthread_t *tcb)
{
    list->tail->next = mlNewTNode();
    list->tail->next->tcb = tcb;
    list->tail->next->next = NULL;
    list->tail = list->tail->next;
    list->numNodes++;
}

mthread_t *mlDequeue(mlist_t *list)
{
    tnode_t *dequeued;
    if (list->head->next == NULL)
    {
        return NULL; // no elements in the list
    }
    dequeued = list->head->next;
    list->head->next = dequeued->next;
    if (list->head->next == NULL)
    {
        list->tail = list->head;
    }
    mthread_t *tcb = dequeued->tcb;
    list->numNodes--;
    free(dequeued);
    return tcb;
}

void mlSortIn(mlist_t *list, mthread_t *tcb)
{
    tnode_t *iter;
    tnode_t *node;
    if (list->head->next == NULL)
    {
        mlEnqueue(list, tcb);
        return;
    }
    iter = list->head;
    while (iter->next != NULL)
    {
        if (iter->next->tcb->readyTime > tcb->readyTime)
        {
            break;
        }
        iter = iter->next;
    }

    node = mlNewTNode();
    node->next = iter->next;
    node->tcb = tcb;
    iter->next = node;
    list->numNodes++;
    if (list->tail->next != NULL)
    {
        list->tail = list->tail->next;
    }
}

mthread_t *mlReadFirst(mlist_t *list)
{
    if (list->iter == NULL)
    {
        list->iter = list->head;
    }
    if (list->head->next == NULL)
    {
        return NULL;
    }
    return list->head->next->tcb;
}

unsigned int mlGetNumNodes(mlist_t *list)
{
    return list->numNodes;
}

void mlSetPtrFirst(mlist_t *list)
{
    list->iter = list->head->next;
}

void mlSetPtrNext(mlist_t *list)
{
    if (list->iter != NULL)
    {
        list->iter = list->iter->next;
    }
}

mthread_t *mlReadCurrent(mlist_t *list)
{
    if (list->iter == NULL)
    {
        return NULL;
    }
    return list->iter->tcb;
}
//******************************************************************************
