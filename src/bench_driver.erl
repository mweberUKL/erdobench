-module(bench_driver).

-callback timing() -> [integer()].
-callback work(Node :: atom(), UserData :: term()) -> term().

-export ([start/4, work/5]).

start(Mod, BenchRef, UserData, Nodes) ->
  [First|Timings] = Mod:timing(),
  timer:apply_after(First, ?MODULE, work, [Mod, BenchRef, UserData, Nodes, Timings]).

work(Mod, {BPid, BRef}, UserData, Nodes, Timings) ->
  io:format("doing work for ~p~n", [Mod]),
  NewUserData = lists:foldl(fun(Node, UD) ->
                              Mod:work(Node, UD)
                            end,
                            UserData,
                            Nodes),
  case Timings of
    [] ->
      io:format("Worker ~p done~n", [Mod]),
      BPid ! {BRef, done},
      done;
    [H|T] ->
      timer:apply_after(H, ?MODULE, work, [Mod, {BPid, BRef}, NewUserData, Nodes, T])
  end.
