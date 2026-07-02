-module(unsplit_ffi).
-export([start/0, unsplit_method_property/3]).

start() ->
    application:set_env(unsplit, reporter, unsplit_reporter),
    case unsplit:start(normal, []) of
        {ok, _Pid} ->
            {ok, nil};
        {error, {already_started, _Pid}} ->
            {ok, nil};
        {error, Reason} ->
            {error, Reason}
    end.

unsplit_method_property(Module, Function, Args) ->
    {unsplit_method, {Module, Function, Args}}.
