-module(bench_driver).

-callback timing() -> timing:timing().
-callback work(Node :: atom(), UserData :: term()) -> term().

-export ([start/4, work/5]).

start(Mod, BenchRef, UserData, Nodes) ->
  TF = Mod:timing(),
  {First, TFNew} = TF(),
  timer:apply_after(First, ?MODULE, work, [Mod, BenchRef, UserData, Nodes, TFNew]).

work(Mod, {BPid, BRef}, UserData, Nodes, TF) ->
  io:format("doing work for ~p~n", [Mod]),
  NewUserData = lists:foldl(fun(Node, UD) ->
                              Mod:work(Node, UD)
                            end,
                            UserData,
                            Nodes),
  case TF() of
    done ->
      io:format("Worker ~p done~n", [Mod]),
      BPid ! {BRef, done},
      done;
    {T, TFNew} ->
      timer:apply_after(T, ?MODULE, work, [Mod, {BPid, BRef}, NewUserData, Nodes, TFNew])
  end.
