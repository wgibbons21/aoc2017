filehandle = open('day7.txt')

contents = filehandle.readlines()

dictionary = {}
i = 0
for i in range(0, contents.__len__()): #contents.__len__() - 1
    contents[i] = contents[i].replace("-> ", '')
    contents[i] = contents[i].replace("\n", '')
    contents[i] = contents[i].replace(",", '').split(" ")
    for j in range(0, len(contents[i])):
        contents[i][j] = contents[i][j]
        contents[i][j].split(' ')

#print(contents)
for i in range(0, contents.__len__()):
    temp = []
    #temp.append(contents[i][0])
    temp.extend(contents[i][2::])
    dictionary[contents[i][0]] = temp
    #print(tem)

#print(contents[i][0])# = contents + contents[i][1::]

#print(dictionary)

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

dictlist = list(dictionary)
#print(len(dictlist))

lengthofpath = [0, 0]
for d in range(0, dictlist.__len__()):
    if len(iterative_dfs(dictionary, dictlist[d])) > lengthofpath[0]:
        lengthofpath[0] = len(iterative_dfs(dictionary, dictlist[d]))
        lengthofpath[1] = dictlist[d]

print("\n", lengthofpath[1], lengthofpath[0])
#print()