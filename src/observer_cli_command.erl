%%% @author zhongwen <zhongwencool@gmail.com>
-module(observer_cli_command).

-include("observer_cli.hrl").

-export([parse_shared/1]).
-export([parse_integer/1]).

-spec parse_shared(term()) -> atom() | string() | tuple().
parse_shared(Key) ->
    case Key of
        "H\n" ->
            home_view;
        "S\n" ->
            system_view;
        "A\n" ->
            app_view;
        "N\n" ->
            inet_view;
        "M\n" ->
            mnesia_view;
        "E\n" ->
            ets_view;
        "D\n" ->
            help_view;
        "P\n" ->
            plugin_view;
        %% inet
        "ic\n" ->
            inet_count;
        "iw\n" ->
            inet_window;
        "rc\n" ->
            recv_cnt;
        "ro\n" ->
            recv_oct;
        "sc\n" ->
            send_cnt;
        "so\n" ->
            send_oct;
        "cnt\n" ->
            cnt;
        "oct\n" ->
            oct;
        "q\n" ->
            quit;
        "Q\n" ->
            quit;
        %% backward
        "pu\n" ->
            page_up_top_n;
        %% forward
        "pd\n" ->
            page_down_top_n;
        %% backward
        "PU\n" ->
            page_up_top_n;
        %% forward
        "PD\n" ->
            page_down_top_n;
        %% backward
        "B\n" ->
            page_up_top_n;
        %% forward
        "F\n" ->
            page_down_top_n;
        %% home
        "p\n" ->
            pause_or_resume;
        "r\n" ->
            {func, proc_count, reductions};
        "b\n" ->
            {func, proc_count, binary_memory};
        "t\n" ->
            {func, proc_count, total_heap_size};
        "m\n" ->
            {func, proc_count, memory};
        "mq\n" ->
            {func, proc_count, message_queue_len};
        "rr\n" ->
            {func, proc_window, reductions};
        "bb\n" ->
            {func, proc_window, binary_memory};
        "tt\n" ->
            {func, proc_window, total_heap_size};
        "mm\n" ->
            {func, proc_window, memory};
        "mmq\n" ->
            {func, proc_window, message_queue_len};
        "\n" ->
            jump;
        "s\n" ->
            size;
        "hide\n" ->
            hide;
        "`\n" ->
            scheduler_usage;
        [$< | PidStr] ->
            to_pid(PidStr);
        [$> | PidStr] ->
            to_pid(PidStr);
        %% {error, estale}|{error, terminated}
        {error, _Reason} ->
            quit;
        Number ->
            parse_integer(Number)
    end.

-spec parse_integer(string()) -> {term(), term()}.
parse_integer(Number) ->
    case string:to_integer(Number) of
        {error, _Reason} ->
            {input_str, Number -- "\n"};
        {Integer, _} ->
            if
                Integer >= ?MIN_INTERVAL ->
                    {new_interval, Integer};
                Integer > 0 ->
                    {jump, Integer};
                true ->
                    {input_str, Number -- "\n"}
            end
    end.

-spec to_pid(string()) -> {go_to_pid, pid()} | quit.
to_pid(Str) ->
    case string:tokens(Str, ".<>\n") of
        [X, Y, Z] ->
            {go_to_pid, list_to_pid("<" ++ X ++ "." ++ Y ++ "." ++ Z ++ ">")};
        [Y] ->
            {go_to_pid, list_to_pid("<0." ++ Y ++ ".0>")};
        _ ->
            quit
    end.
