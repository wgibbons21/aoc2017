import numpy as np

contents = np.loadtxt("day2.txt", delimiter="\t")
contents.sort()

# print(contents)
checksum = 0

for i in range(0, contents.shape[0]):
    # print("i =", i)
    for j in range(0, contents.shape[1] - 1):
        # print("j = ", j)
        for k in range(j, contents.shape[1] - 1):
            # print("k =", k)
            if (contents[i][k + 1] / contents[i][j]).is_integer():
                # print("contents[", i, "][", k + 1, "] / ", "contents[", i, "][", j, "] = ", contents[i][k + 1] / contents[i][j])
                # print(contents[i][k + 1], "/ ", contents[i][j], "= ", contents[i][k + 1] / contents[i][j])
                checksum += contents[i][k + 1] / contents[i][j]

print(checksum)
