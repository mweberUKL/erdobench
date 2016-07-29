-module(bench_logger).

-callback timing() -> [integer()].
-callback commands(Node :: atom()) -> [{Module :: atom(), Function :: atom(), Params :: [term()]}].
-callback log_transform([{atom(), [term()]}]) -> [{atom(), [term()]}].

start(Mod, Nodes) ->
  Fh = file:open(atom_to_list(Mod)+".log"),
  [First|Timings] = Mod:timing(),
  timer:apply_after(First, ?MODULE, read, [Mod, Fh, Nodes, Timings]).

read(Mod, Fh, Nodes, Timings) ->
  Commands = lists:map(fun(Node) -> Mod:commands(Node) end, Nodes),
  Results = lists:map(fun({Node, Commands}) ->
                        Reses = lists:map(fun({Module, Function, Params}) ->
                                            rpc:call(Node, Module, Function, Params)
                                          end, Commands),
                        {Node, Reses}
                      end,
                      lists:zip(Nodes, Commands)),
  TransResults = Mod:log_transform(Results),
  log(Fh, TransResults),
  case Timings of
    [] ->
      file:close(Fh),
      done;
    [H|T] ->
      timer:apply_after(H, ?MODULE, read, [Mod, Nodes, T]),
      continuing
  end.

log(Fh, Results) ->
  lists:foreach(fun({Node, Reses}) ->
                  Format = atom_to_list(Node) + lists:flatten(lists:join(";", lists:duplicate(length(Reses), "~p"))),
                  io:fwrite(Fh, Format, Reses)
                end, Results).
