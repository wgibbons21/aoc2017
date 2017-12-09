#!/usr/bin/env escript

main(_) ->
    {ok, Data} = file:read_file("captcha.txt"),
    Numbers = [Char - 48 || Char <- binary_to_list(Data)],
    Result = case hd(Numbers) =:= lists:last(Numbers) of
        true  -> analyze(Numbers, hd(Numbers));
        false -> analyze(Numbers, 0)
    end,
    io:format("~p~n", [Result]).
 
analyze([First, Second], Acc) when First =:= Second-> Acc + First;
analyze([_, _], Acc) -> Acc;
analyze([First, Second | Rest], Acc) when First =:= Second ->  analyze([Second | Rest], Acc + First);
analyze([_, Second | Rest], Acc) -> analyze([Second | Rest], Acc).
