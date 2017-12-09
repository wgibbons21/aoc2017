phrases = []
countInvalid = set()
valid = 0

with open('day4.txt') as fp:
    for line in fp:
        line = line.split()
        phrases.append(line)

for i in range(0, len(phrases)):
    for j in range(0, len(phrases[i]) - 1):
        for k in range(j, len(phrases[i]) - 1):
            if phrases[i][k + 1] == phrases[i][j]:
                # print("Line ", i, phrases[i][k+1], "==", phrases[i][j])
                countInvalid.add(i)

print("\nThere are only", 512 - len(countInvalid), "valid passphrases")
