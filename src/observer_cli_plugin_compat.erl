%%% @author zhongwen <zhongwencool@gmail.com>
-module(observer_cli_plugin_compat).

-export([
    migrate_config/2,
    normalize_attributes/1,
    normalize_sheet_body/1,
    normalize_sheet_header/1,
    resolve_handler/2
]).

-type plugin_source() :: attributes | sheet_header | sheet_body | config | row_handler.

-spec normalize_attributes(term()) -> map() | no_return().
normalize_attributes(#{rows := Rows, state := _State} = Attrs) when is_list(Rows) ->
    ok = validate_attribute_rows(Rows),
    Attrs;
normalize_attributes({_Rows, _State}) ->
    api_error(attributes, legacy_tuple_result);
normalize_attributes(_Other) ->
    api_error(attributes, expected_rows_state_map).

-spec normalize_sheet_header(term()) -> map() | no_return().
normalize_sheet_header(#{columns := Columns, default_sort := DefaultSort} = Header) when
    is_list(Columns)
->
    Ids = column_ids(Columns),
    ok = ensure_unique_ids(Ids),
    ok = ensure_member(sheet_header, invalid_default_sort, DefaultSort, Ids),
    Header;
normalize_sheet_header(Header) when is_list(Header) ->
    api_error(sheet_header, legacy_header_list);
normalize_sheet_header(_Other) ->
    api_error(sheet_header, expected_columns_map).

-spec normalize_sheet_body(term()) -> map() | no_return().
normalize_sheet_body(#{rows := Rows, state := _State} = Body) when is_list(Rows) ->
    ok = validate_sheet_rows(Rows),
    Body;
normalize_sheet_body({_Rows, _State}) ->
    api_error(sheet_body, legacy_tuple_result);
normalize_sheet_body(_Other) ->
    api_error(sheet_body, expected_rows_state_map).

-spec migrate_config(map(), map()) -> map() | no_return().
migrate_config(Config, Header) when is_map(Config) ->
    #{columns := Columns, default_sort := DefaultSort} = normalize_sheet_header(Header),
    Ids = column_ids(Columns),
    migrate_sort(migrate_handler(Config), Ids, DefaultSort).

-spec resolve_handler(map(), map()) -> none | {module(), term()} | no_return().
resolve_handler(Config, #{handle := Handle}) when is_map(Config) ->
    case maps:find(handler, Config) of
        {ok, Handler} when is_atom(Handler) -> {Handler, Handle};
        {ok, _Handler} -> api_error(config, invalid_handler);
        error when is_pid(Handle) -> {observer_cli_process, Handle};
        error -> api_error(row_handler, non_pid_default_handle)
    end;
resolve_handler(_Config, Row) when is_map(Row) ->
    none;
resolve_handler(_Config, _Row) ->
    api_error(row_handler, invalid_row).

validate_attribute_rows(Rows) ->
    lists:foreach(fun validate_attribute_row/1, Rows).

validate_attribute_row(Row) when is_list(Row) ->
    lists:foreach(fun validate_attribute_cell/1, Row);
validate_attribute_row(_Row) ->
    api_error(attributes, invalid_attribute_row).

validate_attribute_cell(#{content := _Content, width := Width}) when
    is_integer(Width), Width > 0
->
    ok;
validate_attribute_cell(_Cell) ->
    api_error(attributes, invalid_attribute_cell).

validate_sheet_rows(Rows) ->
    lists:foreach(fun validate_sheet_row/1, Rows).

validate_sheet_row(#{cells := Cells}) when is_map(Cells) ->
    ok;
validate_sheet_row(Row) when is_list(Row) ->
    api_error(sheet_body, legacy_row_list);
validate_sheet_row(_Row) ->
    api_error(sheet_body, invalid_row).

column_ids(Columns) ->
    [column_id(Column) || Column <- Columns].

column_id(#{id := Id, title := _Title, width := Width}) when
    is_atom(Id), is_integer(Width), Width > 0
->
    Id;
column_id(_Column) ->
    api_error(sheet_header, invalid_column).

ensure_unique_ids(Ids) ->
    case length(Ids) =:= length(lists:usort(Ids)) of
        true -> ok;
        false -> api_error(sheet_header, duplicate_column_id)
    end.

ensure_member(Source, Reason, Id, Ids) ->
    case lists:member(Id, Ids) of
        true -> ok;
        false -> api_error(Source, Reason)
    end.

migrate_sort(Config, Ids, DefaultSort) ->
    case maps:find(sort, Config) of
        {ok, Sort} ->
            ok = ensure_member(config, invalid_sort, Sort, Ids),
            maps:remove(sort_column, Config);
        error ->
            migrate_sort_column(Config, Ids, DefaultSort)
    end.

migrate_sort_column(Config, Ids, DefaultSort) ->
    case maps:take(sort_column, Config) of
        {SortColumn, WithoutSortColumn} ->
            maps:put(sort, sort_column_id(SortColumn, Ids), WithoutSortColumn);
        error ->
            maps:put(sort, DefaultSort, Config)
    end.

sort_column_id(Index, Ids) when is_integer(Index), Index > 0, Index =< length(Ids) ->
    lists:nth(Index, Ids);
sort_column_id(_Index, _Ids) ->
    api_error(config, invalid_sort_column).

migrate_handler(Config) ->
    case maps:find(handler, Config) of
        {ok, {_Filter, Handler}} when is_atom(Handler) -> maps:put(handler, Handler, Config);
        {ok, Handler} when is_atom(Handler) -> Config;
        {ok, _Handler} -> api_error(config, invalid_handler);
        error -> Config
    end.

-spec api_error(plugin_source(), atom()) -> no_return().
api_error(Source, Reason) ->
    erlang:error({plugin_api_error, #{source => Source, reason => Reason}}).
