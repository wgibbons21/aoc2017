# --- Part Two ---
#
# "Great work; looks like we're on the right track after all. Here's a star for your effort." However, the program
#  seems a little worried. Can programs be worried?
#
# "Based on what we're seeing, it looks like all the User wanted is some information about the evenly divisible values
#  in the spreadsheet. Unfortunately, none of us are equipped for that kind of calculation - most of us specialize in
#  bitwise operations."
#
# It sounds like the goal is to find the only two numbers in each row where one evenly divides the other - that is,
#  where the result of the division operation is a whole number. They would like you to find those numbers on each
#  line, divide them, and add up each line's result.
#
# For example, given the following spreadsheet:
#
# 5 9 2 8
# 9 4 7 3
# 3 8 6 5

# In the first row, the only two numbers that evenly divide are 8 and 2; the result of this division is 4.
# In the second row, the two numbers are 9 and 3; the result is 3.
# In the third row, the result is 2.
# In this example, the sum of the results would be 4 + 3 + 2 = 9.
#
# What is the sum of each row's result in your puzzle input?
#
# result = 285
#############################################################################
def parseFile(fname):
    sum = 0

    f = open(fname, 'r')

    for line in f:
        lstLine = line.strip().split('\t')
        sum += evaluate(lstLine)
    f.close()
    return sum

#############################################################################
def evaluate(line):
    line = map(int, line) # type cast the elements of the input list 'line' from str to int

    line.sort(reverse = True) # sort the list to make division test faster

    for i in line:
        for j in line:
            if not(i == j) and (i % j == 0):
                return i / j

#############################################################################
def main():

    file = 'day2.txt'
    result = parseFile(file)
    print("result = {}").format(result)

if __name__ == '__main__':
    main()