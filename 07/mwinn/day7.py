# --- Day 7: Recursive Circus ---
#
# Wandering further through the circuits of the computer, you come upon a tower of programs that have gotten themselves
#  into a bit of trouble. A recursive algorithm has gotten out of hand, and now they're balanced precariously in a
#  large tower.
#
# One program at the bottom supports the entire tower. It's holding a large disc, and on the disc are balanced several
#  more sub-towers. At the bottom of these sub-towers, standing on the bottom disc, are other programs, each holding
#  their own disc, and so on. At the very tops of these sub-sub-sub-...-towers, many programs stand simply keeping the
#  disc below them balanced but with no disc of their own.
#
# You offer to help, but first you need to understand the structure of these towers. You ask each program to yell out
#  their name, their weight, and (if they're holding a disc) the names of the programs immediately above them balancing
#  on that disc. You write this information down (your puzzle input). Unfortunately, in their panic, they don't do
#  this in an orderly fashion; by the time you're done, you're not sure which program gave which information.
#
# For example, if your list is the following:
#
# pbga (66)
# xhth (57)
# ebii (61)
# havc (66)
# ktlj (57)
# fwft (72) -> ktlj, cntj, xhth
# qoyq (66)
# padx (45) -> pbga, havc, qoyq
# tknk (41) -> ugml, padx, fwft
# jptl (61)
# ugml (68) -> gyxo, ebii, jptl
# gyxo (61)
# cntj (57)
# ...then you would be able to recreate the structure of the towers that looks like this:
#
#                 gyxo
#               /
#          ugml - ebii
#        /      \
#       |         jptl
#       |
#       |         pbga
#      /        /
# tknk --- padx - havc
#      \        \
#       |         qoyq
#       |
#       |         ktlj
#        \      /
#          fwft - cntj
#               \
#                 xhth
# In this example, tknk is at the bottom of the tower (the bottom program), and is holding up ugml, padx, and fwft.
#  Those programs are, in turn, holding up other programs; in this example, none of those programs are holding up any
#  other programs, and are all the tops of their own towers. (The actual tower balancing in front of you is much
#  larger.)
#
# Before you're ready to help them, you need to make sure your information is correct. What is the name of the
#  bottom program?

#############################################################################
# APPROACH
# Read the input file into a dictionary, split on '->'
#   The key is a tuple of the (vertex ID, weight)
#   If the line is a leaf (i.e., no children), the value is None
#   If the line is a branch (i.e., denoted by the '->' string), the value is the list of children
# Build an instance of a tree class using the ANYTREE package
#   Iterate through the dictionary and create a (vertex,weight) for every key. Prune the leaves (value = None) in
#      each iteration.
#   Iterate through the now-pruned dictionary and create the edges for each vertex (key) and children (iterate through
#      the value list).

from anytree import AnyNode, RenderTree

def parsefile(fname):
    f = open(fname, 'r')

    dict = {}

    for line in f:
        l = line.strip
        l = line.split('->')

        key = l[0].split(' ')
        name = key[0]
        weight = key[1]
        weight = int(weight[weight.find("(") + 1:weight.find(")")])
        key = (name, weight)
        value = 'None'  # Assume the line does not have the '->' string and represents a leaf

        if len(l) == 2:  # The has the '->' string and represents a branch
            value = l[1].strip()
            value = value.split(', ')

        dict[key] = value

    f.close()
    return dict

#############################################################################

def main():
    fname = 'input.txt'
    dict = {}
    kdict = {}
    dict = parsefile(fname)             # Read the input file into a dictionary, where the id is a node and the key
                                        #   is either None for a leaf, or a list for a branch

    for k in dict.keys():               # Build the node list from all nodes in the dictionary; add properties (id, weight)
        kdict[k[0]] = AnyNode(id = k[0], weight = k[1])

    for i in dict.items():              # Add the children (branches) to the nodes in the node list
        if i[1] == "None":
            continue

        else:
            id = i[0][0]
            n = kdict[id]
            lst = [kdict[c] for c in i[1]]
            n.children = lst

    for i in kdict.items():             # Find the node without a parent in the node list
        if i[1].parent == None:
            print('Part 1 result: Node {} / Parent: {}').format(i, i[1].parent)

if __name__ == '__main__':
    main()