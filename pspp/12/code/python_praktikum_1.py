# Aufgabe 4

a, b = 0, 1
while b < 200:
    print(b, end=' ')
    a, b = b, a+b

# Jede iteration wird a und b gewsitched und b wird zu a + b
# Folgendes passiert (a, b jeweils werte in iteration x, a+b wert von b nächste zeile)
# a    b    a+b
# 0    1  | 1 Iteration 0 (Zeile 3)
# 1    1  |  2 Iteration 1
# 1    2  |  3 Iteration 2
# 2    3  |  5 Iteration 3
# 3    5  |  8 Iteration 4



# Aufgabe 5
def strcodes1(str):
    ret = [] 
    for l in str:
        ret.append(ord(l))
    return ret

# Schöner:
def strcodes2(str):
    return [ord(l) for l in str]

# Mit Summe
def strsum(str): 
    return sum([ord(l) for l in str])

# Dicts
def printdict(dic): 
    keys=list(dic.keys())
    keys.sort()
    [print(key + "   " + str(dic[key])) for key in keys]

def addDict(dict1, dict2):
    d = {}
    for key in list(dict2.keys()):
        d[key] = dict2[key]
    for key in list(dict1.keys()):
        d[key] = dict1[key]
    return d

# Methodenverzeichnis
dir([])
dir({})
{}.values
{}.values.__doc__
# In python sind die methoden von objekten einfach in einer liste gespeichert, diese
# kann mit dir aufgelistet werden. 
# Funktionen sind objekte in python, deshlab ist {}.values eine valide instruktion.
# Die dokumentation von funktionen ist im attribut __doc__ der funktion gespeichert.

# Mengen
def union(lst1, lst2): 
    return lst1 + [ele for ele in lst2 if ele not in lst1]

def diff(lst1, lst2):
    return [ele for ele in lst1 if ele not in lst2]

def flatten(thing):
    if (type(thing) is list or type(thing) is tuple):
        return [item for ele in thing for item in flatten(ele)]
    return [thing]

# Aufgabe 6: Siehe adder.py
# Aufgabe 7
# Da die python standard libraries mehrheitlich gut verständliche funktionsnamen haben
# ist dieser code sehr einfach verständlich
# Er lädt eine url herunter, decoded sie zu einem string (utf bzw. iso im jeweiligen file)
# und entfernt mit regex dann alle tags der form <...> 
# ersetzte dann konsekutive leerzeichen mit einem
# und splittet alles in eine liste der wörter