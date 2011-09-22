%%% -------------------------------------------------------------------
%%% @author wulei <mjollnir.ray@gmail.com>
%%% @copyright 2011
%%% @end
%% -------------------------------------------------------------------
%%  Copyright (c) 2011
%%
%%   Permission is hereby granted, free of charge, to any person obtaining a
%%   copy of this software and associated documentation files (the "Software"),
%%   to deal in the Software without restriction, including without limitation
%%   the rights to use, copy, modify, merge, publish, distribute, sublicense,
%%   and/or sell copies of the Software, and to permit persons to whom the
%%   Software is furnished to do so, subject to the following conditions:
%%   
%%   The above copyright notice and this permission notice shall be included
%%   in all copies or substantial portions of the Software.
%%   
%%   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
%%   OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
%%   THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%%   FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
%%   DEALINGS IN THE SOFTWARE.
%% -------------------------------------------------------------------
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

-spec(floor(X::number()) -> integer()).
floor(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T - 1;
        Pos when Pos > 0 -> T;
        _ -> T
    end.

-spec(ceiling(X::number()) -> integer()).
ceiling(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T;
        Pos when Pos > 0 -> T + 1;
        _ -> T
    end.


