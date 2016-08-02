-module(bench_driver).

-callback timing() -> [integer()].
-callback work(Node :: atom(), UserData :: term()) -> term().

-export ([start/3, work/4]).

start(Mod, UserData, Nodes) ->
  [First|Timings] = Mod:timing(),
  timer:apply_after(First, ?MODULE, work, [Mod, UserData, Nodes, Timings]).

work(Mod, UserData, Nodes, Timings) ->
  NewUserData = lists:foldl(fun(Node, UD) ->
                              Mod:work(Node, UD)
                            end,
                            UserData,
                            Nodes),
  case Timings of
    [] ->
      done;
    [H|T] ->
      timer:apply_after(H, ?MODULE, work, [Mod, NewUserData, Nodes, T])
  end.
