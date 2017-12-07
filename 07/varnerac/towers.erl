#!/usr/bin/env escript

main(_) ->
    {ok, Data} = file:read_file("towers.txt"),
    Lines = binary:split(Data, <<"\n">>, [global]),
    OnlyArrows = lists:filter( fun(Line) -> [] =/= binary:matches(Line, <<"->">>) end, Lines ),
    LineTokens = [binary:split(OA, [<<", ">>, <<" ">>], [global]) || OA <- OnlyArrows],
    Nodes = [{Root, Children} || [Root, _, _ | Children] <- LineTokens],
    Graph = digraph:new([acyclic]),
    lists:foreach(fun({Root, Children}) ->
                          [digraph:add_vertex(Graph, Name) || Name <-  [Root | Children]], 
                          [digraph:add_edge(Graph, Root, Child) || Child <- Children]
                      end, Nodes),
    io:format("~p~n", [digraph_utils:arborescence_root(Graph)]).
