# --- Day 3: Spiral Memory ---
#
# You come across an experimental new kind of memory stored on an infinite two-dimensional grid.
#
# Each square on the grid is allocated in a spiral pattern starting at a location marked 1 and then counting up while
#  spiraling outward. For example, the first few squares are allocated like this:
#
#   17  16  15  14  13
#   18   5   4   3  12
#   19   6   1   2  11
#   20   7   8   9  10
#   21  22  23---> ...
# While this is very space-efficient (no squares are skipped), requested data must be carried back to square 1 (the
#  location of the only access port for this memory system) by programs that can only move up, down, left, or right.
#  They always take the shortest path: the Manhattan Distance between the location of the data and square 1.
#
# For example:
# Data from square 1 is carried 0 steps, since it's at the access port.
# Data from square 12 is carried 3 steps, such as: down, left, left.
# Data from square 23 is carried only 2 steps: up twice.
# Data from square 1024 must be carried 31 steps.
#
# How many steps are required to carry the data from the square identified in your puzzle input all the way to the
#  access port?
#
# Your puzzle input is 312051.

# --- Part Two ---
#
# As a stress test on the system, the programs here clear the grid and then store the value 1 in square 1. Then, in the
#  same allocation order as shown above, they store the sum of the values in all adjacent squares, including diagonals.
#
# So, the first few squares' values are chosen as follows:
#
# Square 1 starts with the value 1.
# Square 2 has only one adjacent filled square (with value 1), so it also stores 1.
# Square 3 has both of the above squares as neighbors and stores the sum of their values, 2.
# Square 4 has all three of the aforementioned squares as neighbors and stores the sum of their values, 4.
# Square 5 only has the first and fourth squares as neighbors, so it gets the value 5.
# Once a square is written, its value does not change. Therefore, the first few squares would receive the following
#  values:
#
# 147  142  133  122   59
# 304    5    4    2   57
# 330   10    1    1   54
# 351   11   23   25   26
# 362  747  806--->   ...
# What is the first value written that is larger than your puzzle input?
#
# Your puzzle input is still 312051.

def findNumber(goal):
    x = 0
    y = 0
    i = 1
    n = 1
    sign = 1

    while n < goal:
        for xx in range(0, i):
            x += sign * 1
            n += 1
            if n == goal:
                return x, y

        for yy in range(0, i):
            x += sign * 1
            n += 1
            if n > goal:
                return x, y

        sign = sign * -1    # flip the sign and increment the counter
        i += 1

#############################################################################
def findDistance(coords):
#  Manhattan distance formula: |x1 - x2| + |y1 - y2|
        return (abs(coords[0] - 0) + (abs(coords[1] - 0)))

#############################################################################
def buildMatrix(goal):
    x = 0
    y = 0
    i = 1
    n = 1
    sign = 1
    spiral = {(0,0): 1}

    while n < goal:
        for xx in range(0, i):
            x += sign * 1

            n = spiral.get((x+1,y), 0) + \
                spiral.get((x+1,y+1), 0) + \
                spiral.get((x,y+1), 0) +\
                spiral.get((x-1,y+1), 0) +\
                spiral.get((x-1,y), 0) + \
                spiral.get((x-1,y-1), 0) + \
                spiral.get((x,y-1), 0) + \
                spiral.get((x+1,y-1), 0)

            spiral[(x, y)] = n

            if n > goal:
                return n

        for yy in range(0, i):
            y += sign * 1

            n = spiral.get((x + 1, y), 0) + \
                spiral.get((x + 1, y + 1), 0) + \
                spiral.get((x, y + 1), 0) + \
                spiral.get((x - 1, y + 1), 0) + \
                spiral.get((x - 1, y), 0) + \
                spiral.get((x - 1, y - 1), 0) + \
                spiral.get((x, y - 1), 0) + \
                spiral.get((x + 1, y - 1), 0)

            spiral[(x, y)] = n

            if n > goal:
                return n

        sign = sign * -1    # flip the sign and increment the counter
        i += 1

#############################################################################

def main():
    n = 312051

    print("Stage 1 Result: {}".format(findDistance(findNumber(n))))
    print("Stage 2 Result: {}".format(buildMatrix(n)))

if __name__ == '__main__':
    main()