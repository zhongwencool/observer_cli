%% Public entrypoint: parse, preflight, resolve once, execute, project, print.
-module(observer_cli_entry).
-export([main/1]).

-spec main([string()]) -> ok | no_return().
main(Arguments) ->
    case observer_cli_input:parse(Arguments) of
        {ok, #{route := help, path := Path}} ->
            observer_cli_escriptize:write_stdout(observer_cli_catalog:help(Path));
        {ok, #{route := version}} ->
            version();
        {ok, Route} ->
            preflight(Route);
        {error, Error} ->
            Options = #{format => atom_to_list(observer_cli_input:requested_format(Arguments))},
            output(Options, observer_cli_result:argument_error(Error))
    end.

version() ->
    #{bundle_version := Bundle, protocol_version := Protocol} = observer_cli_snapshot:capabilities(),
    observer_cli_escriptize:write_stdout(
        io_lib:format(
            "observer_cli ~ts~nschema ~ts~nprotocol ~B~ncontroller OTP ~ts~n",
            [Bundle, observer_cli_result:schema(), Protocol, erlang:system_info(otp_release)]
        )
    ).

preflight(#{options := Options} = Route) ->
    Format = observer_cli_input:requested_format(Options),
    case observer_cli_capture:encode(Format, observer_cli_result:local(<<"describe">>, #{})) of
        {ok, _} -> execute(Route);
        {error, Error} -> observer_cli_escriptize:output_encode_error(Error)
    end.

execute(#{id := describe, arguments := Path, options := Options}) ->
    case maps:get(schema, Options, false) of
        true ->
            case observer_cli_catalog:schema() of
                {ok, Bytes} ->
                    observer_cli_escriptize:write_stdout(Bytes),
                    observer_cli_escriptize:exit_with_code(0);
                {error, Reason} ->
                    output(Options, observer_cli_result:error(<<"describe">>, internal, Reason))
            end;
        false ->
            {ok, Data} = observer_cli_catalog:describe(Path, maps:get(full, Options, false)),
            output(Options, observer_cli_result:local(<<"describe">>, Data))
    end;
execute(#{route := tui, options := Options}) ->
    case observer_cli_target:resolve(Options) of
        {ok, TargetOptions} ->
            case observer_cli_escriptize:run_tui_options(TargetOptions) of
                ok ->
                    ok;
                {error, Category, Reason} ->
                    output(Options, observer_cli_result:error(<<"tui">>, Category, Reason))
            end;
        {error, Reason} ->
            output(Options, observer_cli_result:error(<<"tui">>, argument, Reason))
    end;
execute(
    #{capture := Capture, capture_options := CaptureOptions, command := Command, options := Options} =
        Route
) ->
    case observer_cli_target:resolve(CaptureOptions) of
        {ok, TargetOptions} ->
            case observer_cli_escriptize:capture(Capture, TargetOptions) of
                {ok, Record, _PrivateExit} ->
                    output(Options, observer_cli_result:from_capture(Route, Record));
                {error, Category, Reason} ->
                    output(Options, observer_cli_result:error(Command, Category, Reason))
            end;
        {error, Reason} ->
            output(Options, observer_cli_result:error(Command, argument, Reason))
    end.

-spec output(map(), map()) -> no_return().
output(Options, Response) ->
    observer_cli_escriptize:command_output(
        Options, Response, observer_cli_result:exit_code(Response, Options)
    ).
