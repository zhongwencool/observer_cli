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

render_help_test() ->
    Text = lists:flatten(observer_cli_help:render_help()),
    ?assert(string:find(Text, "Start Mode") =/= nomatch).

render_help_grouping_test() ->
    Text = lists:flatten(observer_cli_help:render_help()),
    ?assert(string:find(Text, "Doc - Shortcuts and command examples") =/= nomatch),
    StartSection = section_text(Text, "1. Start Mode", "2. Global Commands"),
    GlobalSection = section_text(Text, "2. Global Commands", "3. HOME(H) Commands"),
    HomeSection = section_text(Text, "3. HOME(H) Commands", "4. Process Select Examples"),
    ProcessSection = section_text(Text, "4. Process Select Examples", "5. Reference"),
    ReferenceSection = after_text(Text, "5. Reference"),
    ?assert(string:find(StartSection, "observer_cli:start().") =/= nomatch),
    ?assert(string:find(GlobalSection, "pause/unpause") =/= nomatch),
    ?assertEqual(nomatch, string:find(HomeSection, "pause/unpause")),
    ?assert(string:find(HomeSection, "schedule usage") =/= nomatch),
    ?assert(string:find(ProcessSection, "<0.43.0>") =/= nomatch),
    ?assert(string:find(ReferenceSection, "github.com/ferd/recon") =/= nomatch).

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

after_text(Text, Pattern) ->
    [_, After] = string:split(Text, Pattern, leading),
    After.

section_text(Text, StartPattern, EndPattern) ->
    [Section, _AfterEnd] = string:split(after_text(Text, StartPattern), EndPattern, leading),
    Section.

-endif.
