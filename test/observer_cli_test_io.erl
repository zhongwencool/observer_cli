-module(observer_cli_test_io).

-export([with_input/2]).

with_input(Inputs, Fun) when is_list(Inputs), is_function(Fun, 0) ->
    Pid = spawn(fun() -> io_server(Inputs) end),
    Old = group_leader(),
    group_leader(Pid, self()),
    try
        Fun()
    after
        group_leader(Old, self())
    end.

io_server(Inputs) ->
    receive
        stop ->
            ok;
        {io_request, From, ReplyAs, Request} ->
            {Reply, NextInputs} = handle_request(Request, Inputs),
            From ! {io_reply, ReplyAs, Reply},
            io_server(NextInputs)
    end.

handle_request({get_line, _Enc, _Prompt}, Inputs) ->
    case Inputs of
        [Line | Rest] -> {Line, Rest};
        [] -> {eof, []}
    end;
handle_request({get_chars, _Enc, _Prompt, N}, Inputs) ->
    case Inputs of
        [Line | Rest] -> {lists:sublist(Line, N), Rest};
        [] -> {eof, []}
    end;
handle_request({put_chars, _Enc, _Chars}, Inputs) ->
    {ok, Inputs};
handle_request({put_chars, _Chars}, Inputs) ->
    {ok, Inputs};
handle_request({setopts, _Opts}, Inputs) ->
    {ok, Inputs};
handle_request({getopts, _Opts}, Inputs) ->
    {{ok, [{rows, 24}, {columns, 80}]}, Inputs};
handle_request({get_geometry, rows}, Inputs) ->
    {24, Inputs};
handle_request({get_geometry, columns}, Inputs) ->
    {80, Inputs};
handle_request(_Request, Inputs) ->
    {ok, Inputs}.
