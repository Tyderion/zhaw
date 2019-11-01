#!/usr/bin/env python2
# -*- coding: utf-8 -*-
"""
Created on Wed Feb 15 14:38:21 2017

@author: tugg
update: vissejul, bachmdo2, stdm (Nov 27, 2018)
"""
import pandas as pa
from pyDatalog import pyDatalog

# ---------------------------------------------------------------------------
# Social graph analysis:
# work through this code from top to bottom (in the way you would use a R or Jupyter notebook as well...) and write datalog clauses
# and python code in order to solve the respective tasks. Overall, there are 7 tasks.
# ---------------------------------------------------------------------------
calls = pa.read_csv('calls.csv', sep='\t', encoding='utf-8')
texts = pa.read_csv('texts.csv', sep='\t', encoding='utf-8')

suspect = 'Quandt Katarina'
company_Board = ['Soltau Kristine', 'Eder Eva', 'Michael Jill']

pyDatalog.create_terms('X', 'Y', 'Z', 'knows', 'has_link', 'many_more_needed', 'link', 'path', 'P', 'P2',
                       'path_with_count', 'Pcount', 'P2count', 'path_count', 'COUNT', 'COUNT2', 'has_link_with_date', 'D', 'D2', 'valid_date', 'valid_link', 'valid_path')
pyDatalog.clear()

# First, treat calls as simple social links (denoted as knows), that have no date
for i in range(0, 50):
    +knows(calls.iloc[i, 1], calls.iloc[i, 2])

# Task 1: Knowing someone is a bi-directional relationship -> define the predicate accordingly
knows(X, Y) <= knows(Y, X)

# Task 2: Define the predicate has_link in a way that it is true if a connection exists (path of people knowing the next link)

has_link(X, Y) <= knows(X, Y)
has_link(X, Y) <= knows(X, Z) & has_link(Z, Y) & (X != Y)
# Hints:
#   check if your predicate works: at least 1 of the following asserts should be true (2 if you read in all 150 communication records)
#   (be aware of the unusual behaviour that if an assert evaluates as true, an exception is thrown)
assert (has_link('Quandt Katarina', company_Board[0]) == ())
# assert (has_link('Quandt Katarina', company_Board[1]) == ())
# assert (has_link('Quandt Katarina', company_Board[2]) == ())

# Task 3: You already know that a connection exists; now find the concrete paths between the board members and the suspect
# Hints:
#   if a knows b, there is a path between a and b
#   (X._not_in(P2)) is used to check whether x is not in path P2
#   (P==P2+[Z]) declares P as a new path containing P2 and Z
path(Y, Y, P) <= (P == [Y])
path(X, Y, P) <= path(X, Z, P2) & knows(Z, Y) & (X != Y) & Y._not_in(P2) & (P == P2 + [Y])

# print(path(suspect, company_Board[1],P))
# print (path(suspect, company_Board[2],P))

# Task 4: There are too many paths. We are only interested in short paths.
# Find all the paths between the suspect and the company board that contain five people or less

path_with_count(Y, Y, P, Pcount) <= (P == [Y]) & (Pcount == 0)
path_with_count(X, Y, P, Pcount) <= path_with_count(X, Z, P2, P2count) & knows(Z, Y) & (Y._not_in(P2)) & (X != Y)  & (P == P2 + [Y]) & (Pcount == P2count + 1)

#print(path_with_count(suspect, company_Board[1], P, Pcount) & (Pcount <= 5))

# print(path_with_count(suspect, company_Board[1], P, Pcount) & (Pcount <= 5))
# print(path_with_count(suspect, company_Board[2], P, Pcount) & (Pcount <= 5))

# ---------------------------------------------------------------------------
# Call-Data analysis:
# Now we use the text and the calls data together with their corresponding dates
# ---------------------------------------------------------------------------
date_board_decision = '12.2.2017'
date_shares_bought = '23.2.2017'
pyDatalog.create_terms('called,texted', 'PD', 'PD2')
pyDatalog.clear()

for i in range(0, 150):  # calls
    +called(calls.iloc[i, 1], calls.iloc[i, 2], calls.iloc[i, 3])

for i in range(0, 150):  # texts
    +texted(texts.iloc[i, 1], texts.iloc[i, 2], texts.iloc[i, 3])

called(X,Y,Z) <= called(Y,X,Z) # calls are bi-directional

#https://github.zhaw.ch/kaufman3/KI1/blob/master/P03_CSP_Datalog/datalog_crimefighting_TASK.py
#https://github.zhaw.ch/affolraf/KI/blob/master/P03/datalog_crimefighting_TASK.py
# Task 5: Again we are interested in links, but this time a connection is only valid if the links are descending in date;
#         find out who could have actually sent the information by adding this new restriction
# Hints:
#   You are allowed to naively compare the dates lexicographically using ">" and "<";
#   it works in this example (but is evil in general)
link(X, Y, D) <= (called(X, Y, D)) or (texted(X, Y, D))

has_link_with_date(X, Y, D) <= link(X, Y, D)
has_link_with_date(X, Y, D) <= has_link_with_date(X, Z, D2) & link(Z, Y, D) & (X != Y) & (D <= D2)


#print(has_link_with_date(suspect, Y, D)) #'12.2.2017'


# Task 6: Find all the communication paths that lead to the suspect (with the restriction that the dates have to be ordered correctly)
valid_date(D) <= (D >= date_board_decision) & (D <= date_shares_bought)
valid_link(X, Y, D) <= link(X, Y, D) & valid_date(D)

valid_path(Y, Y, P, D, PD) <= (P==[Y]) & (D == '23.2.2017') & (PD==[D])
valid_path(X, Y, P, D, PD) <= valid_path(X, Z, P2, D2, PD2) & valid_link(Z, Y, D)  & (X != Y)  & (P == P2 + [Y]) & (PD == PD2 + [D]) & (D <= D2) & (Y._not_in(P2))


result = valid_path(suspect, company_Board[1], P, D, PD)
dates = PD.data
names = P.data
for i in range(len(dates)):
    for ele in range(len(dates[i])):
        if ele == 0:
            print(names[i][ele], end='')
        else: #if ele == len(dates[i])
            print('  <', dates[i][ele], '|', names[i][ele], end='', sep='')
    print('')

# Final task: after seeing this information, who, if anybody, do you think gave a tip to the suspect?
#print(valid_path(suspect, company_Board[1], P, Pcount, D) & (Pcount <= 5))

# General hint (only use on last resort!):
#   if nothing else helped, have a look at https://github.com/pcarbonn/pyDatalog/blob/master/pyDatalog/examples/graph.py
