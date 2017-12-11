# --- Day 9: Stream Processing ---
#
# A large stream blocks your path. According to the locals, it's not safe to cross the stream at the moment because
#  it's full of garbage. You look down at the stream; rather than water, you discover that it's a stream of characters.
#
# You sit for a while and record part of the stream (your puzzle input). The characters represent groups - sequences
#  that begin with { and end with }. Within a group, there are zero or more other things, separated by commas: either
#  another group or garbage. Since groups can contain other groups, a } only closes the most-recently-opened unclosed
#  group - that is, they are nestable. Your puzzle input represents a single, large group which itself contains many
#  smaller ones.
#
# Sometimes, instead of a group, you will find garbage. Garbage begins with < and ends with >. Between those angle
#  brackets, almost any character can appear, including { and }. Within garbage, < has no special meaning.
#
# In a futile attempt to clean up the garbage, some program has canceled some of the characters within it using !:
#  inside garbage, any character that comes after ! should be ignored, including <, >, and even another !.
#
# You don't see any characters that deviate from these rules. Outside garbage, you only find well-formed groups, and
# garbage always terminates according to the rules above.
#
# Here are some self-contained pieces of garbage:
#    <>, empty garbage.
#    <random characters>, garbage containing random characters.
#    <<<<>, because the extra < are ignored.
#    <{!>}>, because the first > is canceled.
#    <!!>, because the second ! is canceled, allowing the > to terminate the garbage.
#    <!!!>>, because the second ! and the first > are canceled.
#    <{o"i!a,<{i<a>, which ends at the first >.
#
# Here are some examples of whole streams and the number of groups they contain:
#
#    {}, 1 group.
#    {{{}}}, 3 groups.
#    {{},{}}, also 3 groups.
#    {{{},{},{{}}}}, 6 groups.
#    {<{},{},{{}}>}, 1 group (which itself contains garbage).
#    {<a>,<a>,<a>,<a>}, 1 group.
#    {{<a>},{<a>},{<a>},{<a>}}, 5 groups.
#    {{<!>},{<!>},{<!>},{<a>}}, 2 groups (since all but the last > are canceled).
#
# Your goal is to find the total score for all groups in your input. Each group is assigned a score which is one more
#  than the score of the group that immediately contains it. (The outermost group gets a score of 1.)
#
#   {}, score of 1.
#    {{{}}}, score of 1 + 2 + 3 = 6.
#    {{},{}}, score of 1 + 2 + 2 = 5.
#    {{{},{},{{}}}}, score of 1 + 2 + 3 + 3 + 3 + 4 = 16.
#    {<a>,<a>,<a>,<a>}, score of 1.
#    {{<ab>},{<ab>},{<ab>},{<ab>}}, score of 1 + 2 + 2 + 2 + 2 = 9.
#    {{<!!>},{<!!>},{<!!>},{<!!>}}, score of 1 + 2 + 2 + 2 + 2 = 9.
#    {{<a!>},{<a!>},{<a!>},{<ab>}}, score of 1 + 2 = 3.
#
# What is the total score for all groups in your input?
#
#
# --- Part Two ---
#
# Now, you're ready to remove the garbage.
#
# To prove you've removed it, you need to count all of the characters within the garbage. The leading and trailing
#  < and > don't count, nor do any canceled characters or the ! doing the canceling.
#
#    <>, 0 characters.
#    <random characters>, 17 characters.
#    <<<<>, 3 characters.
#    <{!>}>, 2 characters.
#    <!!>, 0 characters.
#    <!!!>>, 0 characters.
#    <{o"i!a,<{i<a>, 10 characters.
# How many non-canceled characters are within the garbage in your puzzle input?
#############################################################################
# APPROACH: (Stream processing using a stack data objects to track recursive groups)
#############################################################################
# Open the file and establish a loop that reads the first character at each iteration (stream)
# Establish a 'stack' class. One for groups and one for garbage
#   List as object
#   Methods:
#       push a new group onto the stack
#       add characters to the group on the top of the stack
#       pop a completed list off of the stack
#       track the group count (length of the stack, updated on push/pop)
# Establish an evaluation function
#   If the character is a '{', push it to the 'group' stack and begin 'adding' the next stream of characters.
#   If the character is a '<', push it to the 'garbage' stack and begin 'adding' the next stream of characters. Ignore
#       all characters including subsequent '<' characters. The exception is the '!' (see below) until exiting the
#       initial garbage loop.
#   If the character is a '>' pop it from the stack (internal class method increments the count).
#       If the length of the garbage stack is also zero, return to accumulating to the group stack.
#   If the character is a '}' pop it from the stack. An internal class method tracks the level of the stack so that
#       it can be added to a totalizer.
#   If the character is a '!' (in any condition) skip the next character.
#
#   NOTE: debugging print statements are commented to keep output brief, but left in final commit for future use.
#############################################################################
class Group:
    """A stream text processing class"""

    def __init__(self, name):
        self.name = name
        self.count = 0
        self.stack = []
        self.current = []

    def push(self):
        """ push a new group onto the stack """

        # print('\t[PUSH]\tPushing: {}; now {} on the {} stack.').format(self.current, len(self.stack), self.name)

        self.stack.append(self.current)     # First, push the working list onto the stack
        self.current = []                   # Then, clear out the current list. NOTE: the del function exposed logic
                                            # flaws due to soft copy
        self.count = (len(self.stack))

    def pop(self):
        """ pop a completed list off of the stack """

        if len(self.stack) > 0:
            self.current = self.stack.pop()
        result = self.current
        self.count = (len(self.stack))

        # print('\t[POP]\tCurrent list complete ({}): {}; now {} on the stack.').format(self.name, result, len(self.stack))

        return result               # returns the closed out list; for convenience

    def add(self, char):
        """ add characters to the group on the top of the stack """
        self.current.append(char)

        # print('\t[ADD]\tCurrent list ({}): {} with {} on the stack.').format(self.name, self.current, len(self.stack))

#############################################################################
def main():

    fname = 'input.txt'
    garbage = Group('garbage')
    loot = Group('loot')
    charcount = 0
    totalscore = 0
    garbcount = 0

    with open(fname) as f:
        char = f.read(1)
        while char:
            charcount += 1
            # print("[{}] processing char\t{}").format(charcount, char)

            # Assume that the first character is a '{' (it is in this case, but may not be true in all cases)

            if char == "{":
                loot.push()
                loot.add(char)

            elif char == '<':
                garbage.push()
                garbage.add(char)

                while (garbage.count > 0) and char:
                    char = f.read(1)
                    garbcount += 1

                    if char == '>':
                        # print("\t\tGarbage collected (Level {}): {}").format(garbage.count-1, garbage.pop())
                            # Subtract one from the level for the display because the inline pop happens after the print
                        garbage.pop()
                        garbcount -= 1      # terminating characters don't count in part 2

                    elif char == '!':
                        char = f.read(1)
                        charcount += 1
                        garbcount -= 1      # skipped characters don't count in part 2

                    else:
                        garbage.add(char)

            elif char == '}':
                totalscore += loot.count

                # print("Group #{} completed: {}\tTotal Score: {}").format(loot.count, loot.pop(), totalscore)
                loot.pop()

            else:
                loot.add(char)

            char = f.read(1)

        print('[*] Reached end of file. Totalscore: {} / totalgarbage {}').format(totalscore, garbcount)

if __name__ == '__main__':
    main()