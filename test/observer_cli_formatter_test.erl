-module(observer_cli_formatter_test).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

format_default_test() ->
    Text = observer_cli_formatter:format(observer_cli_formatter_default, self(), #{a => 1}),
    ?assert(lists:prefix("Process: ", Text)).

format_default_module_test() ->
    Text = observer_cli_formatter_default:format(self(), [1, 2, 3]),
    ?assert(lists:prefix("Process: ", Text)).

-endif.
