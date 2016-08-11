-module(erdobench).

%% API exports
-export([start/3]).

%%====================================================================
%% API functions
%%====================================================================

start(Loggers, Drivers, Nodes, {PID, Ref}) ->
  LoggerRefs = lists:map(fun(LMod) ->
                          Ref = make_ref(),
                          bench_logger:start(LMod, {self(), Ref}, Nodes),
                          Ref
                         end,
                         Loggers),
  DriverRefs = lists:map(fun({DMod, UserData}) ->
                          Ref = make_ref(),
                          bench_driver:start(DMod, {self(), Ref}, UserData, Nodes),
                          Ref
                         end,
                         Drivers),
  lists:foreach(fun(Ref) ->
                  receive
                    {Ref, done} ->
                      done
                  end
                end,
                LoggerRefs),
  lists:foreach(fun(Ref) ->
                  receive
                    {Ref, done} ->
                      done
                  end
                end,
                DriverRefs),
  PID ! {Ref, done}.

%%====================================================================
%% Internal functions
%%====================================================================
