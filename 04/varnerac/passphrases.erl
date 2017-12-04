#!/usr/bin/env escript

main(_) ->
    {ok, Data} = file:read_file("passphrases.txt"),
    Lines = binary:split(Data, <<"\n">>, [global]),
    TokenList = [binary:split(Line, <<" ">>, [global]) || Line <- Lines],
    Result = lists:foldl(fun(Tokens, Count) ->
                                 case length(Tokens) =:= length(lists:usort(Tokens)) of
                                     true  -> Count + 1;
                                     false -> Count
                                 end
                             end, 0, TokenList),
    io:format("~p~n", [Result]).
    