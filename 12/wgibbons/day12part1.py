import numpy as np

contents = np.loadtxt("day12.txt", delimiter="\t")
contents = contents.astype(int)
i = 0       #index into array, current jump location
jumps = 1   #count the number of jumps made
#print(contents, contents.size)

def traverse(contents, i, jumps):
    #print(contents)
    print("i = ", i)
    print("contents[i] = ", contents[i])
    print("call->traverse(contents,", i, ",", jumps, ")\n")
    if (i + contents[i] < 0) or (i + contents[i] >= contents.size): #stopping condition - outside bounds of array
        print("Total jumps =", jumps)
        #return jumps
    else:
        temp = contents[i]                                          #save current index before we increment
        contents[i] += 1                                            #increment current jump instruction by 1
        #print("jumps =", jumps)
        traverse(contents, temp + i, jumps + 1)

#traverse(contents, i, jumps)

while not (i + contents[i] < 0 or (i + contents[i] >= contents.size)): # stopping condition - outside bounds of array
    temp = contents[i]                          # save current index before we increment
    contents[i] += 1                            # increment current jump instruction by 1
    # print("jumps =", jumps)
    i = temp + i
    jumps += 1

print("Total jumps =", jumps)