filehandle = open("day7.txt")

contents = filehandle.readlines()

dictionary = {}
i = 0
for i in range(0,contents.__len__()): #contents.__len__() - 1
    contents[i] = contents[i].replace("<-> ", '')
    contents[i] = contents[i].replace(",", '').split(" ")
    for j in range(0, len(contents[i])):
        contents[i][j] = int(contents[i][j])

for i in range(0, contents.__len__() - 1):
    dictionary[i] = contents[i][1::]

#http://code.activestate.com/recipes/576723-dfs-and-bfs-graph-traversal/
def recursive_dfs(graph, start, path=[]):
  '''recursive depth first search from start'''
  path=path+[start]
  for node in graph[start]:
    if not node in path:
      path=recursive_dfs(graph, node, path)
  return path

print(len(recursive_dfs(dictionary, 0)))