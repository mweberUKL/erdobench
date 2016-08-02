-module(bench_logger).

-callback timing() -> [integer()].
-callback commands(Node :: atom()) -> [{Module :: atom(), Function :: atom(), Params :: [term()]}].
-callback log_transform(atom(), [term()]) -> [term()].

-export ([start/3, read/5]).

start(Mod, BenchRef, Nodes) ->
  {ok, Fh} = file:open(atom_to_list(Mod)++".log", [write]),
  [First|Timings] = Mod:timing(),
  timer:apply_after(First, ?MODULE, read, [Mod, BenchRef, Fh, Nodes, Timings]).

read(Mod, {BPid, BRef}, Fh, Nodes, Timings) ->
  Commands = lists:map(fun(Node) -> Mod:commands(Node) end, Nodes),
  Results = lists:map(fun({Node, Cmds}) ->
                        Reses = lists:map(fun({Module, Function, Params}) ->
                                            rpc:call(Node, Module, Function, Params)
                                          end, Cmds),
                        {Node, Mod:log_transform(Node, Reses)}
                      end,
                      lists:zip(Nodes, Commands)),
  log(Fh, Results),
  case Timings of
    [] ->
      file:close(Fh),
      BPid ! {BRef, done},
      io:format("Logger ~p done~n", [Mod]),
      done;
    [H|T] ->
      timer:apply_after(H, ?MODULE, read, [Mod, {BPid, BRef}, Fh, Nodes, T]),
      continuing
  end.

log(Fh, Results) ->
  lists:foreach(fun({Node, Reses}) ->
                  Format = atom_to_list(Node) ++ ";" ++ string:join(lists:duplicate(length(Reses), "~w"), ";") ++ "~n",
                  io:fwrite(Fh, Format, Reses)
                end, Results).
