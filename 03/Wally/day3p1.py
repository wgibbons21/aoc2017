input = 361527

matrix = [[0 for x in range(-350, 350)] for y in range(-350, 350)]

def fillright(num, moves, matrix, x, y):
    for k in range(1, moves+1):
        num += 1
        matrix[x+k][y] = num
        print("fillright matrix[", x+k, "][", y, "] =", num)
    retvalues = [num, matrix, x + moves, y]
    return retvalues

def fillleft(num, moves, matrix, x, y):
    for k in range(1, moves+1):
        num += 1
        matrix[y-k][x] = num
        print("fillleft  matrix[", y-k, "][", x, "] =", num)
    retvalues = [num, matrix, x - moves, y]
    return retvalues

def fillup(num, moves, matrix, x, y):
    for k in range(1, moves+1):
        num += 1
        matrix[y+k][x] = num
        print("fillup    matrix[", y+k, "][", x, "] =", num)
    retvalues = [num, matrix, x, y + moves]
    return retvalues

def filldown(num, moves, matrix, x, y):
    for k in range(1, moves+1):
        num += 1
        matrix[x][y-k] = num
        print("filldown  matrix[", x, "][", y-k, "] =", num)
    retvalues = [num, matrix, x, y-moves]
    return retvalues

matrix[0][0] = 0
initvalues = [1, matrix, 0, 0]

for i in range(1, 720, 2):
    print('i=', i)
    initvalues = fillright(initvalues[0], i, matrix, initvalues[2], initvalues[3])
    #print(initvalues[0], initvalues[2], initvalues[3])
    if initvalues[0] > input:
        break

    initvalues = fillup   (initvalues[0], i, matrix, initvalues[2], initvalues[3])
    #print(initvalues[0], initvalues[2], initvalues[3])
    if initvalues[0] > input:
        break

    initvalues = fillleft (initvalues[0], i+1, matrix, initvalues[2], initvalues[3])
    #print(initvalues[0], initvalues[2], initvalues[3])
    if initvalues[0] > input:
        break

    initvalues = filldown (initvalues[0], i+1, matrix, initvalues[2], initvalues[3])
    #print(initvalues[0], initvalues[2], initvalues[3])

    if initvalues[0] > input:
        break

"""
for l in range(5, -5, -1):
    for m in range(-5, 5):
        print(str(l) + "," + str(m), matrix[l][m], "\t\t", end='')
    print('\n')

"""