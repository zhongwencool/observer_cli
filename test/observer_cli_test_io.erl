-module(observer_cli_test_io).

-include_lib("eunit/include/eunit.hrl").

-export([assert_ansi_boundaries/1, assert_stable_fragments/2]).
-export([column_widths/1, line_column_widths/1, line_widths/1, plain/1]).
-export([with_geometry/4, with_input/2]).

assert_stable_fragments(IoData, Fragments) ->
    Text = plain(IoData),
    lists:foreach(
        fun(Fragment) ->
            ?assertNotEqual(nomatch, string:find(Text, Fragment))
        end,
        Fragments
    ).

assert_ansi_boundaries(IoData) ->
    Bin = unicode:characters_to_binary(IoData),
    ?assertEqual(nomatch, binary:match(Bin, <<"\e[0m |">>)),
    ?assertEqual(nomatch, binary:match(strip_valid_ansi(Bin), <<"\e">>)),
    case has_sgr(Bin) of
        true -> ?assertNotEqual(nomatch, binary:match(Bin, <<"\e[0m">>));
        false -> ok
    end.

with_input(Inputs, Fun) when is_list(Inputs), is_function(Fun, 0) ->
    with_geometry(24, 80, Inputs, Fun).

with_geometry(Rows, Columns, Inputs, Fun) when is_list(Inputs), is_function(Fun, 0) ->
    Owner = self(),
    Pid = spawn(fun() -> io_server(Rows, Columns, Inputs, Owner, #{}) end),
    Old = group_leader(),
    group_leader(Pid, self()),
    try
        Fun()
    after
        group_leader(Old, self()),
        Pid ! stop_when_idle
    end.

io_server(Rows, Columns, Inputs, Owner, Clients) ->
    receive
        stop ->
            ok;
        stop_when_idle ->
            drain_io_server(Rows, Columns, Inputs, Owner, Clients);
        {'DOWN', Ref, process, Pid, _} ->
            io_server(Rows, Columns, Inputs, Owner, drop_client(Pid, Ref, Clients));
        {io_request, From, ReplyAs, Request} ->
            NextClients = track_client(From, Owner, Clients),
            {Reply, NextInputs} = handle_request(Request, Rows, Columns, Inputs),
            From ! {io_reply, ReplyAs, Reply},
            io_server(Rows, Columns, NextInputs, Owner, NextClients)
    end.

drain_io_server(Rows, Columns, Inputs, Owner, Clients) ->
    receive
        stop ->
            ok;
        {'DOWN', Ref, process, Pid, _} ->
            drain_io_server(Rows, Columns, Inputs, Owner, drop_client(Pid, Ref, Clients));
        {io_request, From, ReplyAs, Request} ->
            NextClients = track_client(From, Owner, Clients),
            {Reply, NextInputs} = handle_request(Request, Rows, Columns, Inputs),
            From ! {io_reply, ReplyAs, Reply},
            drain_io_server(Rows, Columns, NextInputs, Owner, NextClients)
    after 100 ->
        case maps:size(Clients) of
            0 -> ok;
            _ -> drain_io_server(Rows, Columns, Inputs, Owner, Clients)
        end
    end.

track_client(Owner, Owner, Clients) ->
    Clients;
track_client(From, _Owner, Clients) ->
    case maps:is_key(From, Clients) of
        true -> Clients;
        false -> maps:put(From, erlang:monitor(process, From), Clients)
    end.

drop_client(Pid, Ref, Clients) ->
    case maps:get(Pid, Clients, undefined) of
        Ref -> maps:remove(Pid, Clients);
        _ -> Clients
    end.

handle_request({get_line, _Enc, _Prompt}, _Rows, _Columns, Inputs) ->
    case Inputs of
        [Line | Rest] -> {Line, Rest};
        [] -> {eof, []}
    end;
handle_request({get_chars, _Enc, _Prompt, N}, _Rows, _Columns, Inputs) ->
    case Inputs of
        [Line | Rest] -> {lists:sublist(Line, N), Rest};
        [] -> {eof, []}
    end;
handle_request({put_chars, _Enc, _Chars}, _Rows, _Columns, Inputs) ->
    {ok, Inputs};
handle_request({put_chars, _Chars}, _Rows, _Columns, Inputs) ->
    {ok, Inputs};
handle_request({setopts, _Opts}, _Rows, _Columns, Inputs) ->
    {ok, Inputs};
handle_request({getopts, _Opts}, Rows, Columns, Inputs) ->
    {{ok, [{rows, Rows}, {columns, Columns}]}, Inputs};
handle_request({get_geometry, rows}, Rows, _Columns, Inputs) ->
    {Rows, Inputs};
handle_request({get_geometry, columns}, _Rows, Columns, Inputs) ->
    {Columns, Inputs};
handle_request(_Request, _Rows, _Columns, Inputs) ->
    {ok, Inputs}.

column_widths(IoData) ->
    [First | _] = line_column_widths(IoData),
    First.

line_column_widths(IoData) ->
    [
        [length(Column) || Column <- inner_columns(Line)]
     || Line <- non_empty_lines(IoData)
    ].

line_widths(IoData) ->
    [length(Line) || Line <- non_empty_lines(IoData)].

plain(IoData) ->
    binary_to_list(
        strip_valid_ansi(unicode:characters_to_binary(IoData))
    ).

strip_valid_ansi(Bin) ->
    re:replace(Bin, <<"\e\\[[0-9;]*[A-Za-z]">>, <<>>, [global, {return, binary}]).

has_sgr(Bin) ->
    re:run(Bin, <<"\e\\[[0-9;]*m">>, [{capture, none}]) =:= match.

non_empty_lines(IoData) ->
    [Line || Line <- string:split(plain(IoData), "\n", all), Line =/= ""].

inner_columns(Line) ->
    Parts = string:split(Line, "|", all),
    lists:sublist(Parts, 2, erlang:max(length(Parts) - 2, 0)).
