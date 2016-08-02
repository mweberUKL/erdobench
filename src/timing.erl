-module(timing).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type timing() :: fun(() -> {integer(), timing()} | done).
-export_type([timing/0]).

-export ([once/1, repeat/2, profile/1, repeat_timing/2]).

-spec once(integer()) -> timing().
once(T) ->
  fun() ->
    {T, fun() -> done end}
  end.

-spec repeat(integer(), integer()) -> timing().
repeat(N, T) when N > 0 ->
  fun() ->
    {T, repeat(N-1, T)}
  end;
repeat(_, _T) ->
  fun() ->
    done
  end.

-spec profile([integer()]) -> timing().
profile([]) ->
  fun() ->
    done
  end;
profile([H|T]) ->
  fun() ->
    {H, profile(T)}
  end.

-spec repeat_timing(integer(), timing()) -> timing().
repeat_timing(N, TM) when N > 0 ->
  repeat_timing(N-1, TM, TM);
repeat_timing(_, _) ->
  fun() ->
    done
  end.

repeat_timing(N, TM, TOrig)->
  fun() ->
    case TM() of
      {T, TMNew} ->
        {T, repeat_timing(N, TMNew, TOrig)};
      done ->
        case N of
          _ when N > 0 ->
            {T, TMNew} = TOrig(),
            {T, repeat_timing(N-1, TMNew, TOrig)};
          _ ->
            done
        end
    end
  end.


%% ===================================================================
%% EUnit tests
%% ===================================================================

-ifdef(TEST).
once_test() ->
  Timing1 = once(100),
  Res1 = Timing1(),
  ?assertMatch({100, _}, Res1),
  {_, Timing2} = Res1,
  Res2 = Timing2(),
  ?assertMatch(done, Res2).

repeat_test() ->
  Timing1 = repeat(2, 200),
  Res1 = Timing1(),
  ?assertMatch({200, _}, Res1),
  {_, Timing2} = Res1,
  Res2 = Timing2(),
  ?assertMatch({200, _}, Res2),
  {_, Timing3} = Res2,
  Res3 = Timing3(),
  ?assertMatch(done, Res3).

profile_test() ->
  Timing1 = profile([1, 2, 3]),
  Res1 = Timing1(),
  ?assertMatch({1, _}, Res1),
  {_, Timing2} = Res1,
  Res2 = Timing2(),
  ?assertMatch({2, _}, Res2),
  {_, Timing3} = Res2,
  Res3 = Timing3(),
  ?assertMatch({3, _}, Res3),
  {_, Timing4} = Res3,
  Res4 = Timing4(),
  ?assertMatch(done, Res4).

repeat_timing_test() ->
  Timing1 = repeat_timing(2, profile([1, 2, 3])),
  TimingEnd = lists:foldl(fun(Goal, TM) ->
      Res = TM(),
      ?assertMatch({Goal, _}, Res),
      {_, TMNew} = Res,
      TMNew
    end,
    Timing1,
    [1, 2, 3, 1, 2, 3]),
  ResEnd = TimingEnd(),
  ?assertMatch(done, ResEnd).
-endif.
