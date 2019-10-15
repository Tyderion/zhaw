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
        new_score = getScore(new_board)
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
ourWeights = np.array([
    [1, 1, 1, 1],
    [1, -0.5, -0.5, 1],
    [1, -0.5, -0.5, 1],
    [1, 1, 1, 1]
])

snakeWeights = np.array([
    [4 ** 15, 4 ** 14, 4 ** 13, 4 ** 12],
    [4 ** 8, 4 ** 9, 4 ** 10, 4 ** 11],
    [4 ** 7, 4 ** 6, 4 ** 5, 4 ** 4],
    [4 ** 0, 4 ** 1, 4 ** 2, 4 ** 3]
])

diagonalWeights = np.array([
    [4 ** 6, 4 ** 5, 4 ** 4, 4 ** 3],
    [4 ** 5, 4 ** 4, 4 ** 3, 4 ** 2],
    [4 ** 4, 4 ** 3, 4 ** 2, 4 ** 1],
    [4 ** 3, 4 ** 2, 4 ** 1, 4 ** 0]
])
someOtherWeights = np.array([
    [0, 0, 1, 3],
    [0, 1, 3, 5],
    [1, 3, 5, 15],
    [3, 5, 15, 30]
])

weights = np.array([
    [6, 5, 4, 3],
    [5, 4, 3, 2],
    [4, 3, 2, 1],
    [3, 2, 1, 0]
])

weights2 = np.array([[6, 5, 4, 1],
                    [5, 4, 1, 0],
                    [4, 1, 0, -1],
                    [1, 0, -1, -2]])


def getScore2(board):
    score = 0
    for row in range(4):
        for col in range(4):
            score += weights2[row][col] * board[row][col]
    penalty = compute_penalty(board)
    return score - penalty

def getScore(board):
    # free_spots = count_zeros(board) + 1
    result = 0
    for row in range(4):
        for col in range(4):
            result += weights[row][col] * board[row][col]
    penalty = compute_penalty(board)

    return result - penalty  # * free_spots


def compute_penalty(board):
    penalty = 0
    for row in range(4):
        for col in range(4):
            current = board[row][col]
            if current == 0:
                pass
            else:
                for (nrow, ncol) in [(row-1, col), (row, col-1), (row+1, col), (row, col+1)]:
                    if nrow < 0 or nrow > 3 or ncol < 0 or ncol > 3:
                        pass
                    elif board[nrow][ncol] == 0:
                        pass
                    else:
                        penalty += abs(current - board[nrow][ncol])
    return penalty


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
