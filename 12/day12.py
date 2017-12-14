filehandle = open("/home/ec2-user/environment/aoc2017/12/day12.txt")

contents = filehandle.readlines()

print(contents.__len__())

i = 0
for i in range(0, contents.__len__() - 1):
    contents[i] = contents[i].replace("<-> ", '')
    contents[i] = contents[i].replace(",", '').split(" ")
    for j in range(0, len(contents[i])):
        contents[i][j] = int(contents[i][j])

for i in range(0, 10):
    print(contents[i])
