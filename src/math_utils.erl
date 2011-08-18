-module(math_utils).
-export([floor/1, ceiling/1]).

-ifdef(TEST).
-export([xy_assert/2, swne_assert/2]).

-include_lib("eunit/include/eunit.hrl").

-define(PRECISION, 0.0000001).
xy_assert({X1, Y1}, {X2, Y2}) ->
    ?assert(abs(X1 - X2) < ?PRECISION),
    ?assert(abs(Y1 - Y2) < ?PRECISION).

swne_assert({S1, W1, N1, E1}, {S2, W2, N2, E2}) ->
    ?assert(abs(S1 - S2) < ?PRECISION),
    ?assert(abs(W1 - W2) < ?PRECISION),
    ?assert(abs(N1 - N2) < ?PRECISION),
    ?assert(abs(E1 - E2) < ?PRECISION).

-endif.

floor(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T - 1;
        Pos when Pos > 0 -> T;
        _ -> T
    end.

ceiling(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T;
        Pos when Pos > 0 -> T + 1;
        _ -> T
    end.


