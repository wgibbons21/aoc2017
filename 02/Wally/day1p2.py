filehandle = open('day1.txt')

contents = filehandle.readline()

total = 0
length = int(len(contents))

for i in range(0, length):
    #print("Contents [", i, "] = ", contents[i]) #((x-1) + k) % k
    #print("Contents [", int((((i - (length / 2)) + length) % length)), "] = ", end="")
    #print(contents[int((((i - 1) + length) % length))], "\n")

    if contents[i] == contents[int((((i - (length / 2)) + length) % length))]:
        total += int(contents[i])

print("\nTotal is: ", total)
