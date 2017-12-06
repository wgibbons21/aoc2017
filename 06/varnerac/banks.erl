#!/usr/bin/env escript

main(_) ->
    {ok, Data} = file:read_file("banks.txt"),
    Split = binary:split(Data, <<"\t">>, [global]),
    BanksList = [binary_to_integer(Token) || Token <- Split],
    Banks = maps:from_list(lists:zip(lists:seq(0, length(BanksList) - 1), BanksList)),
    Count = loop(Banks, 1, [Banks]),
    io:format("~p~n", [Count]).

find_starting_block(BanksList) ->
    string:str(BanksList, [lists:max(BanksList)]) - 1.

loop(Banks, Count, Acc) ->
    Position = find_starting_block(maps:values(Banks)),
    Value = maps:get(Position, Banks),
    NewBanks = reallocate(next(Position, Banks), maps:update(Position, 0, Banks), Value),
    case lists:member(NewBanks, Acc) of
        true  -> Count;
        false -> loop(NewBanks, Count + 1, [NewBanks | Acc])
    end.

next(Position, Banks) ->
    case Position =:= maps:size(Banks) - 1 of
        true  -> 0;
        false -> Position + 1
    end.

increment(Position, Banks) ->
    Value = maps:get(Position, Banks),
    maps:update(Position, Value + 1, Banks).

reallocate(_Position, Banks, 0) ->
    Banks;
reallocate(Position, Banks, Value) ->
    NewPosition = next(Position, Banks),
    NewBanks = increment(Position, Banks),
    NewValue = Value - 1,
    reallocate(NewPosition, NewBanks, NewValue).

show_banks(Maps) when is_list(Maps) -> [maps:values(Map) || Map <- Maps];
show_banks(Map) -> maps:values(Map).
