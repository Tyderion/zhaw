import random
import game
import sys
import math
import numpy as np

# Author:				chrn (original by nneonneo)
# Date:				11.11.2016
# Description:			The logic of the AI to beat the game.

UP, DOWN, LEFT, RIGHT = 0, 1, 2, 3

def level(tile):
	if tile==0: return 0
	return math.log(tile)/math.log(2)

def entropy(board):
	result=0
	for x in range(0,4):
		last=None
		for y in range(0,4):
			current=level(board[x][y])
			if last!=None and last!=current:
				result=result+3+abs(current-last)+max(current,last)
			last=current
	for y in range(0,4):
		last=None
		for x in range(0,4):
			current=level(board[x][y])
			if last!=None and last!=current:
				result=result+3+abs(current-last)+max(current,last)
			last=current
	return result


w=np.array([
[ 0.135,  0.121,  0.1  ,  0.1  ],
[ 0.1  ,  0.08 ,  0.075,  0.072],
[ 0.06 ,  0.05 ,  0.04 ,  0.01 ],
[ 0.012,  0.01 ,  0.005,  0.003]])

def wScore(board):
	return (w * board).sum()

def cornerD(x,y):
    return x+y;

def getEmpty(board):
	return 16 - np.count_nonzero(board)


def getScore(board):
    s=10000
    for x in range(0,4):
        for y in range(0,4):
            if board[x][y] != 0:
               c=board[x][y]
               s=(level(c))*(1+cornerD(x,y))*5
    #            l=level(board[x][y])
    #            s=s-cornerD(x,y)*l
                #score=score-(level(board[x][y])-3)*distanceFromBigCorner(board,x,y)
    e=entropy(board)
    s=s-e
    #print("entropy", e, (10000-s), "final", (10000-s) )

    return s
def find_best_move(board):
    bestmove = -1

    bestmove = find_best_move_heur(board)
    return bestmove

def find_best_move_heur(board):
    bestmove=-1
    bestScore=-100000
    for move in range(0,4):
        newBoard=execute_move(move,board)
        score=getScore(newBoard)
        if score>bestScore and not board_equals(newBoard,board):
            bestScore=score
            bestmove=move

        print(move, newBoard, score)
    return bestmove

def execute_move(move, board):
    """
    move and return the grid without a new random tile
	It won't affect the state of the game in the browser.
    """

    if move == UP:
        return game.merge_up(board)
    elif move == DOWN:
        return game.merge_down(board)
    elif move == LEFT:
        return game.merge_left(board)
    elif move == RIGHT:
        return game.merge_right(board)
    else:
        sys.exit("No valid move")

def board_equals(board, newboard):
    """
    Check if two boards are equal
    """
    return  (newboard == board).all()