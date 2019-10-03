import random
import game
import sys
from enum import Enum
import numpy as np
import copy

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


    bestmove = heuristic_simple(board, 3, True)
    highest_merge = get_highest_merge(board)
    highest_merge = highest_merge.direction(board)
    if highest_merge != bestmove[0] and highest_merge != NO_MOVE:
        return highest_merge
    if board_equals(board, execute_move(bestmove[0], copy.deepcopy(board))):
        move = find_best_move_random_agent()
        while move == bestmove[0]:
            move = find_best_move_random_agent()
        return move
    return bestmove[0]

def find_best_move_random_agent():
    return random.choice([UP,DOWN,LEFT,RIGHT])

def get_next_merge_count(board, direction):
    b = copy.deepcopy(board)
    return 0

def get_empty_count(board):
    result = 0
    for row in range(4):
        for col in range(4):
            if board[row][col] == 0:
                result += 1
    return result

def heuristic_simple(board, depth, predict):
    if depth == 0:
        return max_merges(board)
    best_move = [LEFT, 0]
    for move in [UP, DOWN, LEFT, RIGHT]:
        new_board = execute_move(move, copy.deepcopy(board))
        if predict: #and get_empty_count(new_board) < 6:
            count = 0
            sum_count = 0
            for row in range(4):
                for col in range(4):
                    if new_board[row][col] == 0:
                        # TODO: Include 90% chance of new 2
                        count += 1
                        new_board[row][col] = 2
                        sum_count += heuristic_simple(new_board, depth - 1, predict)[1]
                        # new_board[row][col] = 4
                        # sum_count += heuristic_simple(new_board, depth - 1)[1]
                        new_board[row][col] = 0
            current = [move, 0 if count == 0 else sum_count / count]
        else:
            current = heuristic_simple(new_board, depth - 1, predict)
        if current[1] > best_move[1]:
            best_move = current
    return best_move

def max_merges(board):
    rows = len(flatten([count_row_merges(board, row) for row in range(4)]))
    cols = len(flatten([count_col_merges(board, col) for col in range(4)]))
    return [LEFT, rows] if rows > cols else [DOWN, cols]

class Move:
    def __init__(self, horizontal, index, first, second, number):
        self.horizontal = horizontal
        self.index = index
        self.first = first
        self.second = second
        self.number = number

    def direction(self, board):
        if self.number == -1:
            return NO_MOVE
        if self.horizontal:
            return LEFT
            # if self.first == 0:
            #     return LEFT
            # elif self.second == 3:
            #     return RIGHT
            # elif board[self.index][self.first] == EMPTY:
            #     return LEFT
            return RIGHT
        # if self.first == 0:
        #     return UP
        # elif self.second == 3:
        #     return DOWN
        # elif board[self.first][self.index] == EMPTY:
        #     return UP
        return DOWN

    def __repr__(self):
        return "{{{}Move{}}}".format("Horizontal" if self.horizontal else "Vertical", self.__str__())
    def __str__(self):
        if self.horizontal:
            return "({}, {})-({}, {}): {}".format(self.index, self.first, self.index, self.second, self.number)
        return "({}, {})-({}, {}): {}".format(self.first, self.index, self.second, self.index, self.number)


no_move = Move(True, 0, 0, 0, -1)
def get_highest_merge(board):
    rows = flatten([count_row_merges(board, row) for row in range(4)])
    cols = flatten([count_col_merges(board, col) for col in range(4)])

    maxrow = no_move if len(rows) == 0 else max(rows, key=lambda item: item.number)
    maxcol = no_move if len(cols) == 0 else max(cols, key=lambda item: item.number)
    if maxrow.number == maxcol.number:
        return no_move
    elif maxrow.number > maxcol.number:
        return maxrow
    else:
        return maxcol

def get_merge_moves(board):
    rows = flatten([count_row_merges(board, row) for row in range(4)])
    cols = flatten([count_col_merges(board, col) for col in range(4)])
    if not rows and not cols:
        return NO_MOVE
    if not rows:
        return cols[0].direction(board)
    if not cols:
        return rows[0].direction(board)
    if len(rows) >= len(cols):
        return max(rows, key=lambda item: item.number).direction(board)
    return max(rows, key=lambda item: item.number).direction(board)

def count_row_merges(board, row):
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

def count_col_merges(board, col):
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