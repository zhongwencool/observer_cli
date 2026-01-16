-module(observer_cli_help_test).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-include("observer_cli.hrl").

start_quit_test() ->
    observer_cli_test_io:with_input(
        ["q\n"],
        fun() ->
            Opts = #view_opts{auto_row = false},
            ?assertEqual(quit, observer_cli_help:start(Opts))
        end
    ).

start_manager_unknown_test() ->
    observer_cli_test_io:with_input(
        ["x\n", "q\n"],
        fun() ->
            Opts = #view_opts{auto_row = false},
            ?assertEqual(quit, observer_cli_help:start(Opts))
        end
    ).

clean_test() ->
    Pid = spawn(fun() -> receive after infinity -> ok end end),
    Ref = erlang:monitor(process, Pid),
    observer_cli_help:clean([Pid]),
    receive
        {'DOWN', Ref, process, Pid, _} -> ok
    after 1000 ->
        ok
    end.

render_help_test() ->
    Text = lists:flatten(observer_cli_help:render_help()),
    ?assert(string:find(Text, "Start Mode") =/= nomatch).

render_doc_test() ->
    ok = observer_cli_help:render_doc("Interval: 1000ms").

render_worker_redraw_test() ->
    Pid = spawn(fun() -> observer_cli_help:render_worker(1) end),
    Ref = erlang:monitor(process, Pid),
    Pid ! redraw,
    Pid ! quit,
    receive
        {'DOWN', Ref, process, Pid, _} -> ok
    after 1000 ->
        ok
    end.

-endif.
