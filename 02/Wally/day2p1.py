import numpy as np

contents = np.loadtxt("day2.txt", delimiter="\t")
# print(contents)
checksum = 0

for i in range(0, contents.__len__()):
    # print("Min: ", min(contents[i]))
    # print("Max: ", max(contents[i]), "\n")
    checksum += max(contents[i]) - min(contents[i])

print(int(checksum))
