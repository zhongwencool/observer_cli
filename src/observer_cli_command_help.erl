%% Human-readable command help. Descriptors remain the source of CLI semantics.
-module(observer_cli_command_help).

-export([render/1, family/2]).

-spec family(string(), [map()]) -> binary().
family(Family, Commands) ->
    Intro =
        case Family of
            "inspect" -> "Inspect runtime metrics or a selected resource.";
            "trace" -> "Trace function calls or stop node-global legacy tracing."
        end,
    iolist_to_binary([
        wrap(Intro, ""),
        "\nUsage:\n  observer_cli ",
        Family,
        " COMMAND [OPTIONS]\n\nCommands:\n",
        [
            ["  ", maps:get(<<"name">>, D), "\n", wrap(maps:get(<<"summary">>, D), "    ")]
         || D <- Commands
        ],
        "\nRun observer_cli ",
        Family,
        " COMMAND --help for options and examples.\n"
    ]).

-spec render(map()) -> binary().
render(D) ->
    Id = maps:get(<<"id">>, D),
    Options = maps:get(<<"options">>, D),
    Required = [O || O <- Options, required(Id, O)],
    Command = [O || O <- Options, not required(Id, O), group(O) =:= command],
    Target = [O || O <- Options, group(O) =:= target],
    Output = [O || O <- Options, group(O) =:= output],
    iolist_to_binary([
        wrap(maps:get(<<"summary">>, D), ""),
        "\nUsage:\n",
        usage_lines(D),
        arguments(maps:get(<<"positionals">>, D)),
        section("Required options", Required, Id),
        section("Command options", Command, Id),
        case lists:any(fun(O) -> maps:is_key(<<"minimum_ms">>, O) end, Options) of
            true ->
                wrap("Durations accept ms or s (1500ms, 5s); bare numbers are milliseconds.", "  ");
            false ->
                []
        end,
        section("Target options", Target, Id),
        case Target of
            [] ->
                [];
            _ ->
                wrap(
                    "Without --node, use OBSERVER_CLI_NODE and exactly one of OBSERVER_CLI_COOKIE or OBSERVER_CLI_COOKIE_FILE.",
                    "  "
                )
        end,
        section("Output options", Output, Id),
        "\nExamples:\n",
        [[example(Args), "\n"] || Args <- maps:get(<<"examples">>, D)],
        case Id of
            <<"describe">> ->
                [];
            _ ->
                wrap(
                    [
                        "Command metadata: observer_cli describe ",
                        maps:get(<<"name">>, D),
                        " --json"
                    ],
                    ""
                )
        end
    ]).

usage_lines(D) ->
    Words = string:lexemes(binary_to_list(iolist_to_binary(usage(D))), " "),
    [[Line, "\n"] || Line <- hanging_lines(Words, 80)].

usage(D) ->
    Positionals = [
        case maps:get(<<"required">>, P) of
            true -> maps:get(<<"name">>, P);
            false -> ["[", maps:get(<<"name">>, P), "]"]
        end
     || P <- maps:get(<<"positionals">>, D)
    ],
    Selector =
        case maps:get(<<"id">>, D) of
            <<"inspect_state">> -> ["(--pid PID | --name NAME)"];
            _ -> []
        end,
    Required = [
        option_label(O)
     || O <- maps:get(<<"options">>, D), maps:get(<<"required">>, O, false)
    ],
    lists:join(
        " ",
        ["observer_cli", maps:get(<<"name">>, D)] ++ Positionals ++ Selector ++ Required ++
            ["[OPTIONS]"]
    ).

arguments([]) ->
    [];
arguments(Positionals) ->
    [
        "\nArguments:\n",
        [
            [
                "  ",
                maps:get(<<"name">>, P),
                "\n",
                wrap(
                    maps:get(
                        <<"policy">>,
                        P,
                        <<"Command path, such as inspect process; omit to list entrypoints">>
                    ),
                    "    "
                )
            ]
         || P <- Positionals
        ]
    ].

required(<<"inspect_state">>, #{<<"name">> := Name}) when Name =:= <<"pid">>; Name =:= <<"name">> ->
    true;
required(_, O) ->
    maps:get(<<"required">>, O, false).

group(#{<<"name">> := Name}) ->
    case Name of
        <<"node">> -> target;
        <<"cookie-env">> -> target;
        <<"cookie-file">> -> target;
        <<"name-mode">> -> target;
        <<"format">> -> output;
        <<"json">> -> output;
        <<"verbose">> -> output;
        <<"redact">> -> output;
        _ -> command
    end.

section(_, [], _) ->
    [];
section(Title, Options, Id) ->
    %% Lead with resource selectors and keep timeout after observation options.
    Ordered = lists:sort(fun(A, B) -> option_order(A) =< option_order(B) end, Options),
    [
        "\n",
        Title,
        ":\n",
        [
            [
                "  ",
                option_label(O),
                "\n",
                wrap(maps:get(<<"summary">>, O), "    "),
                option_details(Id, O)
            ]
         || O <- Ordered
        ]
    ].

option_order(#{<<"name">> := Name}) when
    Name =:= <<"pid">>; Name =:= <<"name">>; Name =:= <<"id">>
->
    0;
option_order(#{<<"name">> := <<"timeout">>}) ->
    2;
option_order(_) ->
    1.

option_label(#{<<"kind">> := <<"flag">>, <<"name">> := Name}) ->
    ["--", Name];
option_label(#{<<"name">> := Name} = O) ->
    Placeholder =
        case maps:is_key(<<"minimum_ms">>, O) of
            true ->
                "DURATION";
            false ->
                case Name of
                    <<"node">> -> "NODE";
                    <<"cookie-env">> -> "NAME";
                    <<"cookie-file">> -> "PATH";
                    <<"name-mode">> -> "short|long";
                    <<"format">> -> "text|term|json";
                    <<"fail-on">> -> "warning|critical";
                    <<"app">> -> "APP";
                    <<"pid">> -> "PID";
                    <<"name">> -> "NAME";
                    <<"id">> -> "PORT_ID";
                    <<"behavior">> -> "BEHAVIOR";
                    <<"handler">> -> "HANDLER";
                    <<"sort">> -> "METRIC";
                    <<"rate">> -> "N/s";
                    _ -> "N"
                end
        end,
    ["--", Name, " ", Placeholder].

option_details(Id, #{<<"name">> := <<"timeout">>}) ->
    wrap(timeout_details(Id), "    ");
option_details(_, #{<<"kind">> := <<"flag">>}) ->
    [];
option_details(_, O) ->
    Bounds =
        case O of
            #{<<"minimum_ms">> := Min, <<"maximum_ms">> := Max} ->
                ["Range: ", duration(Min), "..", duration(Max), ". "];
            #{<<"minimum">> := Min, <<"maximum">> := Max} ->
                io_lib:format("Range: ~B..~B. ", [Min, Max]);
            _ ->
                []
        end,
    Default =
        case maps:find(<<"default">>, O) of
            {ok, V} ->
                [
                    "Default: ",
                    case maps:is_key(<<"minimum_ms">>, O) of
                        true -> duration(V);
                        false when is_integer(V) -> integer_to_list(V);
                        false -> V
                    end,
                    "."
                ];
            error ->
                []
        end,
    [
        case {Bounds, Default} of
            {[], []} -> [];
            _ -> wrap([Bounds, Default], "    ")
        end,
        case O of
            #{<<"name">> := Name, <<"enum">> := Values} when
                Name =:= <<"sort">>; Name =:= <<"behavior">>
            ->
                wrap(["Choices: ", lists:join(", ", Values), "."], "    ");
            _ ->
                []
        end
    ].

timeout_details(<<"check", _/binary>>) ->
    "Default and minimum: --window + 5s (20s with the default window). Maximum: 120s.";
timeout_details(<<"trace_call">>) ->
    "Default: max(10s, --duration + 7s), normally 17s. Minimum: --duration + 7s. Maximum: 120s.";
timeout_details(Id) when
    Id =:= <<"inspect_scheduler">>;
    Id =:= <<"inspect_process">>;
    Id =:= <<"inspect_network">>;
    Id =:= <<"inspect_socket">>
->
    "Default: 10s, or --window + 5s if longer. With sampling, must cover --window + 5s. Maximum: 120s.";
timeout_details(<<"inspect_state">>) ->
    "Range: 10s..120s. Default: 10s.";
timeout_details(<<"trace_stop_all">>) ->
    "Range: 5s..120s. Default: 10s.";
timeout_details(_) ->
    "Range: 1ms..120s. Default: 10s.".

duration(Ms) when Ms rem 1000 =:= 0 -> [integer_to_list(Ms div 1000), "s"];
duration(Ms) -> [integer_to_list(Ms), "ms"].

example(Args) ->
    Tokens = ["observer_cli" | [quote(Arg) || Arg <- Args]],
    %% Leave room for the shell continuation marker on wrapped lines.
    lists:join(" \\\n", hanging_lines(Tokens, 78)).

hanging_lines(Tokens, Width) ->
    [First | Rest] = wrap_tokens(Tokens, "    ", Width),
    [_, FirstWords] = First,
    [["  ", FirstWords] | Rest].

quote(Arg) ->
    case re:run(Arg, "^[A-Za-z0-9_@%+=:,./-]+$", [{capture, none}]) of
        match -> binary_to_list(Arg);
        nomatch -> ["'", binary_to_list(binary:replace(Arg, <<"'">>, <<"'\\''">>, [global])), "'"]
    end.

wrap(Text, Indent) ->
    Words = string:lexemes(binary_to_list(iolist_to_binary(Text)), " \n\t"),
    [[Line, "\n"] || Line <- wrap_tokens(Words, Indent, 80)].

wrap_tokens(Words, Indent, Width) ->
    wrap_tokens(Words, Indent, Width, [], length(Indent), []).
wrap_tokens([], _, _, [], _, Lines) ->
    lists:reverse(Lines);
wrap_tokens([], Indent, _, Current, _, Lines) ->
    lists:reverse([[Indent, lists:join(" ", lists:reverse(Current))] | Lines]);
wrap_tokens([Word | Rest], Indent, Width, Current, Length, Lines) ->
    Size = iolist_size(Word),
    Space =
        case Current of
            [] -> 0;
            _ -> 1
        end,
    case Current =/= [] andalso Length + Space + Size > Width of
        true ->
            wrap_tokens(
                [Word | Rest],
                Indent,
                Width,
                [],
                length(Indent),
                [[Indent, lists:join(" ", lists:reverse(Current))] | Lines]
            );
        false ->
            wrap_tokens(Rest, Indent, Width, [Word | Current], Length + Space + Size, Lines)
    end.
