-module(bench_logger).

-callback timing() -> timing:timing().
-callback commands(Node :: atom()) -> [fun(() -> term())].
-callback log_transform(atom(), [term()]) -> [term()].

-export ([start/3, read/6]).

start(Mod, BenchRef, Nodes) ->
  {ok, Fh} = file:open(atom_to_list(Mod)++".log", [write]),
  TF = Mod:timing(),
  {First, TFNew} = TF(),
  timer:apply_after(First, ?MODULE, read, [Mod, BenchRef, First, Fh, Nodes, TFNew]).

read(Mod, {BPid, BRef}, Time, Fh, Nodes, TF) ->
  Commands = lists:map(fun(Node) -> Mod:commands(Node) end, Nodes),
  Results = lists:map(fun({Node, Cmds}) ->
                        Reses = lists:map(fun(F) ->
                                            F()
                                          end, Cmds),
                        {Node, Mod:log_transform(Node, Reses)}
                      end,
                      lists:zip(Nodes, Commands)),
  log(Fh, Time, Results),
  case TF() of
    done ->
      file:close(Fh),
      io:format("Logger ~p done~n", [Mod]),
      BPid ! {BRef, done},
      done;
    {T, TFNew} ->
      timer:apply_after(T, ?MODULE, read, [Mod, {BPid, BRef}, Time+T, Fh, Nodes, TFNew]),
      continuing
  end.

log(Fh, Time, Results) ->
  lists:foreach(fun({Node, Reses}) ->
                  Format = atom_to_list(Node) ++ ";" ++ integer_to_list(Time) ++ ";" ++ string:join(lists:duplicate(length(Reses), "~w"), ";") ++ "~n",
                  io:fwrite(Fh, Format, Reses)
                end, Results).
