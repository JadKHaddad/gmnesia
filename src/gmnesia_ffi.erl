-module(gmnesia_ffi).

-export([info/0, system_info/1, start/0, stop/0, create_schema/1, delete_schema/1,
         create_table/2, delete_table/1, transaction_1/1, write_3/3, write_1/1, delete_3/3,
         read_3/3]).

info() ->
    mnesia:info().

system_info(SystemInfo) ->
    mnesia:system_info(SystemInfo).

start() ->
    case mnesia:start() of
        ok ->
            {ok, nil};
        {error, Reason} ->
            {error, Reason}
    end.

stop() ->
    case mnesia:stop() of
        stopped ->
            {ok, nil};
        {error, Reason} ->
            {error, Reason}
    end.

create_schema(Nodes) ->
    case mnesia:create_schema(Nodes) of
        ok ->
            {ok, nil};
        {error, Reason} ->
            {error, Reason}
    end.

delete_schema(Nodes) ->
    case mnesia:delete_schema(Nodes) of
        ok ->
            {ok, nil};
        {error, Reason} ->
            {error, Reason}
    end.

create_table(Table, Options) ->
    case mnesia:create_table(Table, Options) of
        {atomic, ok} ->
            {ok, nil};
        {aborted, Reason} ->
            {error, Reason}
    end.

delete_table(Table) ->
    case mnesia:delete_table(Table) of
        {atomic, ok} ->
            {ok, nil};
        {aborted, Reason} ->
            {error, Reason}
    end.

transaction_1(Fun) ->
    case mnesia:transaction(Fun) of
        {atomic, Result} ->
            {ok, Result};
        {aborted, Reason} ->
            {error, Reason}
    end.

write_3(Table, Record, Lock) ->
    mnesia:write(Table, Record, Lock),
    nil.

write_1(Record) ->
    mnesia:write(Record),
    nil.

delete_3(Table, Record, Lock) ->
    mnesia:delete(Table, Record, Lock),
    nil.

read_3(Tab, Key, Lock) ->
    mnesia:read(Tab, Key, Lock).
