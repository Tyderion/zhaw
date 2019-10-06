import random
import game
import sys
import numpy as np

# Author:				chrn (original by nneonneo)
# Date:				11.11.2016
# Description:			The logic of the AI to beat the game.

UP, DOWN, LEFT, RIGHT = 0, 1, 2, 3
NO_MOVE = -1
MIN_SCORE = -10 ** 12

EMPTY = 0


def find_best_move(board):
    best_move = NO_MOVE
    best_score = MIN_SCORE

    for move in range(4):
        new_board = execute_move(move, board)
        new_score = score(new_board)
        if new_score > best_score:
            best_score = new_score
            best_move = move

    return best_move


# weights = [[1, 0, 0, 1],
#            [0, -1, -1, 0],
#            [0, -1, -1, 0],
#            [1, 0, 0, 1]]
#
# weights = [[3, 2, 2, 3],
#            [2, 0, 0, 2],
#            [2, 0, 0, 2],
#            [3, 2, 2, 3]]
# weights = [[2, 1, 1, 1],
#            [2, -1, -1, 1],
#            [2, -1, -1, 1],
#            [2, 2, 2, 2]]
weights = np.array([
    [1, 1, 1, 1],
    [1, 0.1, 0.1, 1],
    [1, 0.1, 0.1, 1],
    [1, 1, 1, 1]
])


def score(board):
    free_spots = count_zeros(board) + 1
    result = (weights * board).sum()

    return result * free_spots


def count_zeros(board):
    return 16 - np.count_nonzero(board)


def find_best_move_random_agent():
    return random.choice([UP, DOWN, LEFT, RIGHT])


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
    return (newboard == board).all()
