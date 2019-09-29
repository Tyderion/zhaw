import random
import game
import sys
from enum import Enum

# Author:				chrn (original by nneonneo)
# Date:				11.11.2016
# Description:			The logic of the AI to beat the game.

UP, DOWN, LEFT, RIGHT = 0, 1, 2, 3
NO_MOVE = -1

EMPTY = 0

flatten = lambda l: [item for sublist in l for item in sublist]

def find_best_move(board):
    bestmove = -1    
	
	# TODO:
	# Build a heuristic agent on your own that is much better than the random agent.
	# Your own agent don't have to beat the game.

    bestmove = merge_move(board)
    if (bestmove == NO_MOVE):
        # find a move that enables a move
        bestmove = find_best_move_random_agent()
    return bestmove

def find_best_move_random_agent():
    return random.choice([UP,DOWN,LEFT,RIGHT])


class Move:
    def __init__(self, horizontal, index, first, second, number):
        self.horizontal = horizontal
        self.index = index
        self.first = first
        self.second = second
        self.number = number

    def direction(self):
        if self.horizontal:
            if self.first == 0:
                return LEFT
            return RIGHT
        if self.first == 0:
            return UP
        return DOWN

    def __repr__(self):
        return "{{{}Move{}}}".format("Horizontal" if self.horizontal else "Vertical", self.__str__())
    def __str__(self):
        if self.horizontal:
            return "({}, {})-({}, {}): {}".format(self.index, self.first, self.index, self.second, self.number)
        return "({}, {})-({}, {}): {}".format(self.first, self.index, self.second, self.index, self.number)

def merge_move(board):
    rows = flatten([row_merge(board, row) for row in range(4)])
    cols = flatten([col_merge(board, col) for col in range(4)])
    if not rows and not cols:
        return NO_MOVE
    if not rows:
        return cols[0].direction()
    if not cols:
        return rows[0].direction()
    if len(rows) >= len(cols):
        return max(rows, key=lambda item: item.number).direction()
    return max(rows, key=lambda item: item.number).direction()

def row_merge(board, row):
    result = []
    current = -1
    for i in range(4):
        if board[row][i] == EMPTY:
            continue
        if current == -1:
            current = i
            continue
        if board[row][current] == board[row][i]:
            result.append(Move(True, row, current, i, board[row][i]))
        else:
            current = i
    return result

def col_merge(board, col):
    result = []
    current = -1
    for i in range(4):
        if board[i][col] == EMPTY:
            continue
        if current == -1:
            current = i
            continue
        if board[current][col] == board[i][col]:
            result.append(Move(False, col, current, i, board[i][col]))
        else:
            current = i
    return result

    
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