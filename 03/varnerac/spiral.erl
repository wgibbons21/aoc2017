#!/usr/bin/env escript

main(_) ->
    Target = 312051,
    Result = spiral({0, 0}, right, 1, #{}, Target),
    io:format("Result: ~p~n", [Result]).

-spec spiral({X, Y}, Direction, Value, Target, Accumulator) -> {X, Y} when
    X :: integer(),
    Y :: integer(),
    Direction :: up | down | left | right,
    Value :: pos_integer(),
    Target :: pos_integer(),
    Accumulator :: maps:map().
spiral(Coordinates, _Direction, Target, Accumulator, Target) ->
    Coordinates;
spiral(Coordinates, Direction, Value, Accumulator, Target) ->
    NewAccumulator = maps:put(Coordinates, Value, Accumulator),
    NewDirection = next_direction(Coordinates, Direction, NewAccumulator),
    NewCoordinates = move(NewDirection, Coordinates),
    spiral(NewCoordinates, NewDirection, Value + 1, NewAccumulator, Target).

move(right, {X, Y}) -> {X + 1, Y    };
move(left,  {X, Y}) -> {X - 1, Y    };
move(up,    {X, Y}) -> {X    , Y + 1};
move(down,  {X, Y}) -> {X    , Y - 1}.

next_direction({X, Y}, right, Accumulator) ->
    case maps:is_key({X, Y + 1}, Accumulator) of
        true  -> right;
        false -> up
    end;
next_direction({X, Y}, up, Accumulator) ->
    case maps:is_key({X - 1, Y}, Accumulator) of
        true  -> up;
        false -> left
    end;
next_direction({X, Y}, left, Accumulator) ->
    case maps:is_key({X, Y -1}, Accumulator) of
        true  -> left;
        false -> down
    end;
next_direction({X, Y}, down, Accumulator) ->
    case maps:is_key({X + 1, Y}, Accumulator) of
        true  -> down;
        false -> right
    end.
