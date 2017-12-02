filehandle = open('day1.txt')

contents = filehandle.readline()

total = 0

for i in range(0, contents.__len__() - 1):
    if contents[i] == contents[i+1]:
        total += int(contents[i])

if contents[contents.__len__() - 1] == contents[0]: #process list in a circular manner, check case where last value in list matches first
    total += int(contents[0])

print("\nSum is: " + total.__str__())


