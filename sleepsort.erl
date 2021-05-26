%%% @author Peter
%%% @copyright (C) 2021, Peter
%%% @doc
%%% please never use this
%%% @end
%%% Created : 24 May 2021 by Peter

-module(sleepsort).

-include_lib("eunit/include/eunit.hrl").

-export([sort/1]). 

%% Hand tuned to pass all tests on my computer
-define(ACCURACY_FACTOR, 80).

%% API
-spec sort(list(number())) -> list(number()).
sort(L) when is_list(L) ->    
    %% Partition the list into negative and positive numbers
    %% this is to solve the problem of how long to wait for negative numbers
    Neg = [X || X <- L, X < 0],
    Pos = [X || X <- L, X >= 0],
    
    %% workers sleep for a time proportional to their magnitude, then send their own value to a pid (P)
    Worker = fun(P, X, Ref) -> erlang:send_after(trunc(abs(X) * ?ACCURACY_FACTOR), P, {Ref, X}) end,

    %% receivers send accumulated messages to the parent (S) after having received the correct # of messages
    S = self(),
    Receiver = fun(Ref, L1) -> S ! {self(), Ref, [receive {Ref, X} -> X end || _ <- L1]} end,
    
    %% spawn neg and pos workers in parallel
    PosRef = make_ref(),   
    PosReceiver = spawn(fun() -> Receiver(PosRef, Pos) end),
    [spawn(fun() -> Worker(PosReceiver, X, PosRef) end) || X <- Pos],    
    
    
    NegRef = make_ref(),
    NegReceiver = spawn(fun() -> Receiver(NegRef, Neg) end),
    [spawn(fun() -> Worker(NegReceiver, X, NegRef) end) || X <- Neg],
    
    
    %% await pos
    PosSorted = receive {PosReceiver, PosRef, LP} -> LP end,

    %% await neg
    NegSorted = receive {NegReceiver, NegRef, LN} -> LN end,
    
    %% negatives are received in reverse order
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

float_test() ->
    Input = [3.3,3.0,1.5,4.0,7,-1],
    Expected = [-1,1.5,3.0,3.3,4.0,7],
    Actual = sort(Input),
    ?assertEqual(Expected, Actual).    

close_float_test() ->
    %% had to adjust the ACCURACY FACTOR for this one
    Input = [1.05,1.04,1.03,1.02,1.01],
    Expected = [1.01,1.02,1.03,1.04,1.05],
    Actual = sort(Input),
    ?assertEqual(Expected, Actual).    
-endif.
