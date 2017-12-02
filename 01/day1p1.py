filehandle = open('day1.txt')

contents = filehandle.readline()
print(contents.__len__())

#print(contents)

total = 0

for i in range(0, contents.__len__() - 1):
    if contents[i] == contents[i+1]:
        total += int(contents[i])

if contents[contents.__len__() - 1] == contents[0]:
    print('Last matches first')
    total += int(contents[0])

print("Sum is: " + total.__str__())


