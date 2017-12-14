# --- Day 11: Hex Ed ---
#
# Crossing the bridge, you've barely reached the other side of the stream when a program comes up to you, clearly in
#  distress. "It's my child process," she says, "he's gotten lost in an infinite grid!"
#
# Fortunately for her, you have plenty of experience with infinite grids.
#
# Unfortunately for you, it's a hex grid.
#
# The hexagons ("hexes") in this grid are aligned such that adjacent hexes can be found to the north, northeast,
#  southeast, south, southwest, and northwest:
#
#   \ n  /
# nw +--+ ne
#   /    \
# -+      +-
#   \    /
# sw +--+ se
#   / s  \
# You have the path the child process took. Starting where he started, you need to determine the fewest number
#  of steps required to reach him. (A "step" means to move from the hex you are in to any adjacent hex.)
#
# For example:
#
# ne,ne,ne is 3 steps away.
# ne,ne,sw,sw is 0 steps away (back where you started).
# ne,ne,s,s is 2 steps away (se,se).
# se,sw,se,sw,sw is 3 steps away (s,s,sw).
#############################################################################

#   REFERENCE: https://www.redblobgames.com/grids/hexagons/#neighbors
class Hex:    
    """Implements a hexagonal infinite grid """

    def __init__(self):
        """Initializes the data."""
        self.pos = (0, 0, 0)
        self.max = 0

    def step(self, dir):
        if dir == 'n':        #   \ n  /
            mov = (0, 1, -1)  # nw +--+ ne
        elif dir == 'ne':     #   /    \
            mov = (1, 0, -1)  # -+      +-
        elif dir == 'se':     #   \    /
            mov = (1, -1 ,0)  # sw +--+ se
        elif dir == 's':      #   / s  \
            mov = (0, -1, 1)
        elif dir == 'sw':
            mov = (-1, 0, 1)
        elif dir == 'nw':
            mov = (-1, 1, 0)
        else:
            mov = (0, 0, 0)
            
        self.pos = tuple(map(sum, zip(self.pos, mov)))

        m = self.mdis()
        if m > self.max:
            self.max = m

    def mdis(self, start=(0,0,0)):
        return max(abs(self.pos[0] - start[0]), abs(self.pos[1] - start[1]), abs(self.pos[2] - start[2]))
#############################################################################
def main():

    fname = 'input.txt'
    grid = Hex()

    f = open(fname, 'r')
    lstLine = f.read()
    lstLine = lstLine.split(',')
    f.close()

    for l in lstLine:
        grid.step(l)

    print('Distance {} / max: {}'.format(grid.mdis(), grid.max))

if __name__ == '__main__':
    main()