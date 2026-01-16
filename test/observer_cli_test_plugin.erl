-module(observer_cli_test_plugin).

-export([attributes/1, sheet_header/0, sheet_body/1]).

attributes(State) ->
    Labels = [
        [
            #{content => "Name", width => 6, color => <<>>},
            #{content => {percent, 0.5}, width => 6}
        ]
    ],
    {Labels, State}.

sheet_header() ->
    [
        #{title => "Name", width => 6, shortcut => "N"},
        #{title => "Value", width => 5}
    ].

sheet_body(Prev) ->
    {
        [
            ["alpha", 1],
            ["beta", 2]
        ],
        Prev
    }.
