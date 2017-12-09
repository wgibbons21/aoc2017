#!/usr/bin/env escript

main(_) ->
    {ok, Data} = file:read_file("spreadsheet.txt"),
    Rows = binary:split(Data, <<"\n">>, [global]),
    TextColumns = [binary:split(Row, <<"\t">>, [global]) || Row <- Rows],
    NumColumns = lists:map(fun(Cols) -> [binary_to_integer(C) || C <- Cols] end, TextColumns),
    Checksums = lists:map(fun(Col) -> lists:max(Col) - lists:min(Col) end, NumColumns),
    Result = lists:sum(Checksums),
    io:format("~p~n", [Result]).
    