#!/usr/bin/env escript

main(_) ->
    {ok, Data} = file:read_file("jumps.txt"),
    Split = binary:split(Data, <<"\n">>, [global]),
    JumpsList = [binary_to_integer(Token) || Token <- Split],
    Max = length(JumpsList),
    PropList = lists:zip(lists:seq(0, Max - 1), JumpsList),
    Jumps = maps:from_list(PropList),
    Count = loop(0, Jumps, 0, Max),
    io:format("~p~n", [Count]).

loop(Position, _Jumps, Count, Max) when Position >= Max orelse Position < 0 ->
    Count;
loop(Position, Jumps, Count, Max) ->
    CurrentValue = maps:get(Position, Jumps),
    NewJumps = maps:update(Position, CurrentValue + 1, Jumps),
    NewPosition = Position + CurrentValue,
    NewCount = Count + 1,
    loop(NewPosition, NewJumps, NewCount, Max).
