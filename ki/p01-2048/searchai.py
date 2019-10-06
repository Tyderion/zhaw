import random
import game
import sys
from multiprocessing import Pool
from heuristicai import score, count_zeros

# Author:      chrn (original by nneonneo)
# Date:        11.11.2016
# Copyright:   Algorithm from https://github.com/nneonneo/2048-ai
# Description: The logic to beat the game. Based on expectimax algorithm.

UP, DOWN, LEFT, RIGHT = 0, 1, 2, 3
MOVES = [UP, DOWN, LEFT, RIGHT]
PROBABILITY_2 = 0.9
PROBABILITY_4 = 0.1


def find_best_move(board):
    """
    find the best move for the next turn.
    """
    with Pool(processes=4) as pool:
        result = pool.starmap(score_toplevel_move, [(move, board) for move in MOVES])
    best_move = result.index(max(result))

    for m in MOVES:
        print("move: %d score: %.4f" % (m, result[m]))

    return best_move


def score_toplevel_move(move, board):
    """
    Entry Point to score the first move.
    """
    new_board = execute_move(move, board)

    if board_equals(board, new_board):
        return -10 * 2 ** 12
    # TODO:
    # Implement the Expectimax Algorithm.
    # 1.) Start the recursion until it reach a certain depth
    # 2.) When you don't reach the last depth, get all possible board states and
    #     calculate their scores dependence of the probability this will occur. (recursively)
    # 3.) When you reach the leaf calculate the board score with your heuristic.
    free_spots = count_zeros(new_board)
    if free_spots == 0:
        return expectimax(new_board, 4)
    if free_spots < 4:
        return expectimax(new_board, 3)
    else:
        return expectimax(new_board, 2)


def expectimax(board, depth):
    if depth == 0:
        return score(board)

    result = 0
    amount_of_moves = count_zeros(board)
    for move in MOVES:
        for row in range(4):
            for col in range(4):
                if board[row][col] == 0:
                    board[row][col] = 2
                    new_board = execute_move(move, board)
                    result += PROBABILITY_2 / amount_of_moves * expectimax(new_board, depth - 1)

                    board[row][col] = 4
                    new_board = execute_move(move, board)
                    result += PROBABILITY_4 / amount_of_moves * expectimax(new_board, depth - 1)
                    board[row][col] = 0

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


def board_equals(board, new_board):
    """
    Check if two boards are equal
    """
    return (new_board == board).all()
