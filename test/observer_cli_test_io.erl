-module(observer_cli_test_io).

-include_lib("eunit/include/eunit.hrl").

-export([assert_ansi_boundaries/1, assert_stable_fragments/2]).
-export([capture_with_geometry/4]).
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
    {Result, _Output} = capture_with_geometry(Rows, Columns, Inputs, Fun),
    Result.

capture_with_geometry(Rows, Columns, Inputs, Fun) when is_list(Inputs), is_function(Fun, 0) ->
    Owner = self(),
    Pid = spawn(fun() -> io_server(Rows, Columns, Inputs, Owner, #{}, []) end),
    Old = group_leader(),
    group_leader(Pid, self()),
    try
        Result = Fun(),
        Ref = make_ref(),
        Pid ! {collect_output, self(), Ref},
        Output =
            receive
                {Ref, Captured} -> Captured
            after 1000 ->
                exit(capture_timeout)
            end,
        {Result, Output}
    after
        group_leader(Old, self()),
        Pid ! stop_when_idle
    end.

io_server(Rows, Columns, Inputs, Owner, Clients, Output) ->
    receive
        stop ->
            ok;
        stop_when_idle ->
            drain_io_server(Rows, Columns, Inputs, Owner, Clients, Output);
        {collect_output, From, Ref} ->
            From ! {Ref, lists:reverse(Output)},
            io_server(Rows, Columns, Inputs, Owner, Clients, Output);
        {delayed_io_reply, From, ReplyAs, Reply} ->
            From ! {io_reply, ReplyAs, Reply},
            io_server(Rows, Columns, Inputs, Owner, Clients, Output);
        {'DOWN', Ref, process, Pid, _} ->
            io_server(Rows, Columns, Inputs, Owner, drop_client(Pid, Ref, Clients), Output);
        {io_request, From, ReplyAs, Request} ->
            NextClients = track_client(From, Owner, Clients),
            case handle_request(Request, Rows, Columns, Inputs, Output) of
                {delay, Millis, Reply, NextInputs, NextOutput} ->
                    erlang:send_after(Millis, self(), {delayed_io_reply, From, ReplyAs, Reply}),
                    io_server(Rows, Columns, NextInputs, Owner, NextClients, NextOutput);
                {Reply, NextInputs, NextOutput} ->
                    From ! {io_reply, ReplyAs, Reply},
                    io_server(Rows, Columns, NextInputs, Owner, NextClients, NextOutput)
            end
    end.

drain_io_server(Rows, Columns, Inputs, Owner, Clients, Output) ->
    receive
        stop ->
            ok;
        {delayed_io_reply, From, ReplyAs, Reply} ->
            From ! {io_reply, ReplyAs, Reply},
            drain_io_server(Rows, Columns, Inputs, Owner, Clients, Output);
        {'DOWN', Ref, process, Pid, _} ->
            drain_io_server(Rows, Columns, Inputs, Owner, drop_client(Pid, Ref, Clients), Output);
        {io_request, From, ReplyAs, Request} ->
            NextClients = track_client(From, Owner, Clients),
            case handle_request(Request, Rows, Columns, Inputs, Output) of
                {delay, Millis, Reply, NextInputs, NextOutput} ->
                    erlang:send_after(Millis, self(), {delayed_io_reply, From, ReplyAs, Reply}),
                    drain_io_server(Rows, Columns, NextInputs, Owner, NextClients, NextOutput);
                {Reply, NextInputs, NextOutput} ->
                    From ! {io_reply, ReplyAs, Reply},
                    drain_io_server(Rows, Columns, NextInputs, Owner, NextClients, NextOutput)
            end
    after 100 ->
        case maps:size(Clients) of
            0 -> ok;
            _ -> drain_io_server(Rows, Columns, Inputs, Owner, Clients, Output)
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

handle_request({get_line, _Enc, _Prompt}, _Rows, _Columns, Inputs, Output) ->
    case Inputs of
        [{sleep, Millis, Line} | Rest] ->
            {delay, Millis, Line, Rest, Output};
        [Line | Rest] ->
            {Line, Rest, Output};
        [] ->
            {eof, [], Output}
    end;
handle_request({get_chars, _Enc, _Prompt, N}, _Rows, _Columns, Inputs, Output) ->
    case Inputs of
        [Line | Rest] -> {lists:sublist(Line, N), Rest, Output};
        [] -> {eof, [], Output}
    end;
handle_request({put_chars, _Enc, Chars}, _Rows, _Columns, Inputs, Output) ->
    {ok, Inputs, [Chars | Output]};
handle_request({put_chars, Chars}, _Rows, _Columns, Inputs, Output) ->
    {ok, Inputs, [Chars | Output]};
handle_request({setopts, _Opts}, _Rows, _Columns, Inputs, Output) ->
    {ok, Inputs, Output};
handle_request({getopts, _Opts}, Rows, Columns, Inputs, Output) ->
    {{ok, [{rows, Rows}, {columns, Columns}]}, Inputs, Output};
handle_request({get_geometry, rows}, Rows, _Columns, Inputs, Output) ->
    {Rows, Inputs, Output};
handle_request({get_geometry, columns}, _Rows, Columns, Inputs, Output) ->
    {Columns, Inputs, Output};
handle_request(_Request, _Rows, _Columns, Inputs, Output) ->
    {ok, Inputs, Output}.

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
