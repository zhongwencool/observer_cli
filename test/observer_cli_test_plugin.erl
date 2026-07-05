-module(observer_cli_test_plugin).

-export([attributes/1, sheet_header/0, sheet_body/1]).

attributes(State) ->
    Labels = [
        [
            #{content => "Name", width => 6, color => <<>>},
            #{content => {percent, 0.5}, width => 6}
        ]
    ],
    #{rows => Labels, state => State}.

sheet_header() ->
    #{
        columns => [
            #{id => name, title => "Name", width => 6, shortcut => "N"},
            #{id => value, title => "Value", width => 5}
        ],
        default_sort => value
    }.

sheet_body(Prev) ->
    #{
        rows => [
            #{cells => #{name => "alpha", value => 1}},
            #{cells => #{name => "beta", value => 2}}
        ],
        state => Prev
    }.
