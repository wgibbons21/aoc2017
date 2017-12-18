filehandle = open("day7.txt")

contents = filehandle.readlines()
b = [x for x in range(2000)]
b = set(b)
dictionary = {}

i = 0
for i in range(0, contents.__len__()): #contents.__len__() - 1
    contents[i] = contents[i].replace("<-> ", '')
    contents[i] = contents[i].replace(",", '').split(" ")
    for j in range(0, len(contents[i])):
        contents[i][j] = int(contents[i][j])

for i in range(0, contents.__len__()):
    dictionary[i] = contents[i][1::]

#http://code.activestate.com/recipes/576723-dfs-and-bfs-graph-traversal/
def recursive_dfs(graph, start, path=[]):
  '''recursive depth first search from start'''
  path=path+[start]
  for node in graph[start]:
    if not node in path:
      path=recursive_dfs(graph, node, path)
  return path

def iterative_dfs(graph, start, path=[]):
  '''iterative depth first search from start'''
  q=[start]
  while q:
    v=q.pop(0)
    if v not in path:
      path=path+[v]
      q=graph[v]+q
  return path

print(len(dictionary), dictionary)

#a = sorted(iterative_dfs(dictionary, 0))

i = 0

#b = b - set(a)
print(len(b))
print(list(b)[0])

while b:
    #print("i =", i)
    #print("Len of b=", len(b), "\nList b[0]=", list(b)[0], b)
    #print("len(iterative_dfs(dictionary, list(b)[0])) =", len(iterative_dfs(dictionary, list(b)[0])))
    temp = iterative_dfs(dictionary, list(b)[0])
    print("temp =", temp)
    print(b.__len__(), b)
    if len(temp) > 1:
        b = set(sorted(list(b - set(sorted(temp)))))
        i += 1
    else:
        i += 1
        print(iterative_dfs(dictionary, list(b)[0]), "b.remove = ", list(b)[0])
        b.remove(list(b)[0])


print("i =", i)