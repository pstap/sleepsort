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
sort([]) -> [];
sort(L) when is_list(L) ->
    Ref = make_ref(),
    N = length(L),
    S = self(),
    [spawn(fun() -> erlang:send_after(X * ?ACCURACY_FACTOR, S, {Ref, X}) end) || X <- L],
    sort_(Ref, [], N).

%% Private
-spec sort_(reference(), list(number()), number()) -> list(number()).
sort_(_Ref, Acc, 0) -> lists:reverse(Acc);
sort_(Ref, Acc, N) -> receive {Ref, X} -> sort_(Ref, [X|Acc], N - 1) end.


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
-endif.
