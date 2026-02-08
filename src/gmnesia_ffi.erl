-module(gmnesia_ffi).

-export([info/0, system_info/1, start/0, stop/0, create_schema/1, delete_schema/1, all_keys/1, first/1, last/1,
         create_table/2,  delete_table/1, wait_for_tables/2, transaction/1, transaction/2, abort/1, write/3, write/1, delete/3,
         read/3, subscribe/1, unsubscribe/1]).

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

all_keys(Table) ->
    mnesia:all_keys(Table).

first(Table) ->
    mnesia:first(Table).

last(Table) ->
    mnesia:last(Table).

create_table(Table, Options) ->
    case mnesia:create_table(Table, Options) of
        {atomic, ok} ->
            {ok, nil};
        {aborted, Reason} ->
            {error, Reason}
    end.

wait_for_tables(Tabs, Timeout) ->
    TimeoutOpt =
        case Timeout of
            infinity ->
                infinity;
            {finite, N} ->
                N
        end,
    case mnesia:wait_for_tables(Tabs, TimeoutOpt) of
        ok ->
            {ok, nil};
        {error, Reason} ->
            {error, Reason};
        {timeout, Tables} ->
            {timeout, Tables}
    end.

delete_table(Table) ->
    case mnesia:delete_table(Table) of
        {atomic, ok} ->
            {ok, nil};
        {aborted, Reason} ->
            {error, Reason}
    end.

transaction(Fun) ->
    case mnesia:transaction(Fun) of
        {atomic, Result} ->
            {ok, Result};
        {aborted, Reason} ->
            {error, Reason}
    end.

transaction(Fun, Retries) ->
    RetriesOpt =
        case Retries of
            infinity ->
                infinity;
            {finite, N} ->
                N
        end,
    case mnesia:transaction(Fun, RetriesOpt) of
        {atomic, Result} ->
            {ok, Result};
        {aborted, Reason} ->
            {error, Reason}
    end.

abort(Reason) ->
    mnesia:abort(Reason).

write(Table, Record, Lock) ->
    mnesia:write(Table, Record, Lock).

write(Record) ->
    mnesia:write(Record).

delete(Table, Record, Lock) ->
    mnesia:delete(Table, Record, Lock).

read(Tab, Key, Lock) ->
    mnesia:read(Tab, Key, Lock).

subscribe(What) ->
    case mnesia:subscribe(What) of
        {ok, Node} ->
            {ok, Node};
        {error, Reason} ->
            {error, Reason}
    end.

unsubscribe(What) ->
    case mnesia:unsubscribe(What) of
        {ok, Node} ->
            {ok, Node};
        {error, Reason} ->
            {error, Reason}
    end.