# --- Day 8: I Heard You Like Registers ---
#
# You receive a signal directly from the CPU. Because of your recent assistance with jump instructions, it would like
#  you to compute the result of a series of unusual register instructions.
#
# Each instruction consists of several parts: the register to modify, whether to increase or decrease that register's
#  value, the amount by which to increase or decrease it, and a condition. If the condition fails, skip the
#  instruction without modifying the register. The registers all start at 0. The instructions look like this:
#
#    b inc 5 if a > 1
#    a inc 1 if b < 5
#    c dec -10 if a >= 1
#    c inc -20 if c == 10
#
# These instructions would be processed as follows:
#
# Because a starts at 0, it is not greater than 1, and so b is not modified.
# a is increased by 1 (to 1) because b is less than 5 (it is 0).
# c is decreased by -10 (to 10) because a is now greater than or equal to 1 (it is 1).
# c is increased by -20 (to -10) because c is equal to 10.
# After this process, the largest value in any register is 1.
#
# You might also encounter <= (less than or equal to) or != (not equal to). However, the CPU doesn't have the
#  bandwidth to tell you what all the registers are named, and leaves that to you to determine.
#
# What is the largest value in any register after completing the instructions in your puzzle input?
#
# --- Part Two ---
#
# To be safe, the CPU also needs to know the highest value held in any register during this process so that it can
#  decide how much memory to allocate to these operations. For example, in the above instructions, the highest value
#  ever held was 10 (in register c after the third instruction was evaluated).

#############################################################################
def main():
    fname = 'input.txt'
    i = 0
    dict = {}
    step = 0
    mx = 0
    mx2 = 0

    with open(fname, 'r') as f:
        lstLine = f.readlines()
    f.close()

    for l in lstLine:
        # Fields:   (i.e., vkg inc -257 if v >= -1921)
        #   0   reg:    the target register
        #   1   ins     the instruction (i.e., inc or dec)
        #   2   val     the value for the corresponding instruction
        #   3   cond    is always "IF" (can be skipped)
        #   4   tgt     the reference register for the conditional instruction
        #   5   tgtop   the operation on which to apply to condition (i.e., <, >, >=, <=, !=)
        #   6   tgtval  the value on which to evaluate the condition
        l = l.strip()
        l = l.split(' ')

        if l[4] not in dict:  # if the reference register exists, evaluate the statement
            dict[l[4]] = 0

        if l[5] == '<':
            result = (dict[l[4]] < int(l[6]))
        elif l[5] == '>':
            result = (dict[l[4]] > int(l[6]))
        elif l[5] == '<=':
            result = (dict[l[4]] <= int(l[6]))
        elif l[5] == '>=':
            result = (dict[l[4]] >= int(l[6]))
        elif l[5] == '==':
            result = (dict[l[4]] == int(l[6]))
        elif l[5] == '!=':
            result = dict[l[4]] != int(l[6])

        if result:
            if l[0] not in dict:
                dict[l[0]] = 0

            if l[1] == 'inc':
                before = dict[l[0]]
                dict[l[0]] = dict[l[0]] + int(l[2])

            elif l[1] == 'dec':
                before = dict[l[0]]
                dict[l[0]] = dict[l[0]] - int(l[2])

        if (dict[max(dict, key=dict.get)] > mx2):
            mx2 = dict[max(dict, key=dict.get)]

    for k, v in dict.iteritems():
        print k, v

    mx = max(dict, key = dict.get)
    print("Part 1 result: Register {}\tValue: {}").format(mx, dict[mx])

    print("Part 2 result: Highest All Time Value: {}").format(mx2)


if __name__ == '__main__':
    main()