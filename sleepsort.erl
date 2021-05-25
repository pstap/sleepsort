%%% @author Peter <peter@beep>
%%% @copyright (C) 2021, Peter
%%% @doc
%%% please never use this
%%% @end
%%% Created : 24 May 2021 by Peter <peter@beep>

-module(sleepsort).

-include_lib("eunit/include/eunit.hrl").

-export([sort/1]). 

-define(ACCURACY_FACTOR, 10).

%% API
-spec sort(list(number())) -> list(number).
sort(L) when is_list(L) ->
    S = self(),

    %% Partition the positive and negative numbers 
    Neg = [X || X <- L, X < 0],
    Pos = [X || X <- L, X >= 0],

    F = fun(X, Ref) -> erlang:send_after(abs(X) * ?ACCURACY_FACTOR, S, {Ref, X}) end,
    
    %% spawn and collect positive first, then negative
    PosRef = make_ref(),
    NegRef = make_ref(),   

    [spawn(fun() -> F(X, PosRef) end) || X <- Pos],    
    PosSorted = [receive {PosRef, X} -> X end || _ <- Pos],
    
    [spawn(fun() -> F(X, NegRef) end) || X <- Neg],    
    NegSorted = [receive {NegRef, X} -> X end || _ <- Neg],
    
    %% negatives received in reverse order
    lists:reverse(NegSorted) ++ PosSorted.


%% EUNIT
-ifdef(TEST).
empty_test() ->
    ?assertEqual(sort([]), []).

asc_test() ->
    Input = [1,2,3,4,5,6,7,8,9,10],
    Expected = Input,
    Actual = sort(Input),
    ?assertEqual(Expected, Actual).

desc_test() ->
    Input = [10,9,8,7,6,5,4,3,2,1],
    Expected = lists:reverse(Input),
    Actual = sort(Input),
    ?assertEqual(Expected, Actual).

single_test() ->
    Input = [1],
    Expected = Input,
    Actual = sort(Input),
    ?assertEqual(Expected, Actual).

rand_test() ->
    %% decided by [random:uniform(20) || _ <- [1,2,3,4,5,6,7,8,9,10]],
    Input = [2,12,20,7,4,5,1,18,17,17], 
    Expected = lists:sort(Input),
    Actual = sort(Input),
    ?assertEqual(Expected, Actual).

neg_test() ->
    Input = [1,2,3,4,-4,-3,-2,-1,0],
    Expected = [-4,-3,-2,-1,0,1,2,3,4],
    Actual = sort(Input),
    ?assertEqual(Expected, Actual).
-endif.
