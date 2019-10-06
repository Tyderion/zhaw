import random
import game
import sys
from multiprocessing import Pool
from heuristicai import getScore, count_zeros, getScore2

# Author:      chrn (original by nneonneo)
# Date:        11.11.2016
# Copyright:   Algorithm from https://github.com/nneonneo/2048-ai
# Description: The logic to beat the game. Based on expectimax algorithm.

UP, DOWN, LEFT, RIGHT = 0, 1, 2, 3
MOVES = [UP, DOWN, LEFT, RIGHT]
PROBABILITY_2 = 0.9
PROBABILITY_4 = 0.1
MIN_SCORE = -10 ** 14

BOARD = 0
PLAYER = 1


def find_best_move(board):
    """
    find the best move for the next turn.
    """
    with Pool(processes=4) as pool:
       result = pool.starmap(score_toplevel_move, [(move, board) for move in MOVES])
    best_move = result.index(max(result))
    #best_move = get_best_move(board, 6 if count_zeros(board) < 5 else 4)

    for m in MOVES:
        print("move: %d score: %.4f" % (m, result[m]))

    return best_move


def get_best_move(board, depth):
    score = MIN_SCORE
    best_move = -1
    for move in MOVES:
        new_board = execute_move(move, board)
        if board_equals(board, new_board):
            pass
        new_score = expect(board, depth, BOARD)

        if new_score > score:
            best_move = move
            score = new_score

    return best_move


def expect(board, depth, agent):
    score = MIN_SCORE
    if depth == 0:
        return getScore2(board)
    elif agent == PLAYER:
        for move in MOVES:
            new_board = execute_move(move, board)
            if board_equals(board, new_board):
                pass
            new_score = expect(board, depth - 1, BOARD)
            if new_score > score:
                score = new_score
        return score
    elif agent == BOARD:
        for row in range(4):
            for col in range(4):
                if board[row][col] == 0:
                    board[row][col] = 2
                    new_score = expect(board, depth - 1, PLAYER)
                    if new_score != MIN_SCORE:
                        score += PROBABILITY_2 * new_score

                    board[row][col] = 4
                    new_score = expect(board, depth - 1, PLAYER)
                    if new_score != MIN_SCORE:
                        score += PROBABILITY_2 * new_score
                    board[row][col] = 0
        zeros = count_zeros(board)
        if zeros == 0:
            return score
        return score / zeros


def score_toplevel_move(move, board):
    """
    Entry Point to score the first move.
    """
    new_board = execute_move(move, board)

    if board_equals(board, new_board):
        return MIN_SCORE
    # TODO:
    # Implement the Expectimax Algorithm.
    # 1.) Start the recursion until it reach a certain depth
    # 2.) When you don't reach the last depth, get all possible board states and
    #     calculate their scores dependence of the probability this will occur. (recursively)
    # 3.) When you reach the leaf calculate the board score with your heuristic.
    free_spots = count_zeros(new_board)
    if free_spots < 5:
        return expect(new_board, 6, BOARD)
    else:
        return expect(new_board, 4, BOARD)


def expectimax(board, depth):
    if depth == 0:
        return getScore(board)

    result = 0
    amount_of_moves = count_zeros(board)
    for move in MOVES:
        for row in range(4):
            for col in range(4):
                if board[row][col] == 0:
                    board[row][col] = 2
                    new_board = execute_move(move, board)
                    result += PROBABILITY_2 * expectimax(new_board, depth - 1)

                    board[row][col] = 4
                    new_board = execute_move(move, board)
                    result += PROBABILITY_4 * expectimax(new_board, depth - 1)
                    board[row][col] = 0

    return result / amount_of_moves


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
