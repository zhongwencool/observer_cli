-module(less_server).
-behaviour(gen_server).

%% api
-export([
    start_link/1,
    start_link/2,
    stop/1,

    page/1,
    next/1,
    prev/1
]).

%% gen_server
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2
]).

-record(state, {
    lines :: pos_integer(),
    buf :: list(string()),
    position :: non_neg_integer()
}).

-type state() :: #state{}.

%%%===================================================================
%%% api
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
-spec start_link(String :: string()) ->
    {ok, pid()} | {error, {already_started, pid()}} | {error, Reason :: any()}.
%%--------------------------------------------------------------------
start_link(String) ->
    gen_server:start_link(?MODULE, {String}, []).
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
-spec start_link(String :: string(), Lines :: pos_integer()) ->
    {ok, pid()} | {error, {already_started, pid()}} | {error, Reason :: any()}.
%%--------------------------------------------------------------------
start_link(String, Lines) ->
    gen_server:start_link(?MODULE, {String, Lines}, []).
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
-spec stop(LessServer :: pid()) ->
    ok.
%%--------------------------------------------------------------------
stop(LessServer) ->
    gen_server:stop(LessServer).
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
-spec page(LessServer :: pid()) ->
    String :: string().
%%--------------------------------------------------------------------
page(LessServer) ->
    gen_server:call(LessServer, {page}, infinity).
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
-spec next(LessServer :: pid()) ->
    String :: string().
%%--------------------------------------------------------------------
next(LessServer) ->
    gen_server:call(LessServer, {next}, infinity).
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
-spec prev(LessServer :: pid()) ->
    String :: string().
%%--------------------------------------------------------------------
prev(LessServer) ->
    gen_server:call(LessServer, {prev}, infinity).
%%--------------------------------------------------------------------

%%%===================================================================
%%% gen_server
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
-spec init({String :: string()} | {String :: string(), Lines :: pos_integer()}) ->
    {ok, state()}.
%%--------------------------------------------------------------------
init({String}) ->
    {ok, state(String)};
init({String, Lines}) ->
    {ok, state(String, Lines)}.
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
-spec handle_call(Request :: term(), From :: {pid(), _Tag}, State :: state()) ->
    {reply, Result :: ok, State2 :: state()}.
%%--------------------------------------------------------------------
handle_call({page}, _From, State) ->
    {reply, page_2(State), State};
handle_call({next}, _From, State) ->
    State2 = next_2(State),
    {reply, page_2(State2), State2};
handle_call({prev}, _From, State) ->
    State2 = prev_2(State),
    {reply, page_2(State2), State2};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
-spec handle_cast(Request :: term(), State :: state()) ->
    {noreply, State2 :: state()}.
%%--------------------------------------------------------------------
handle_cast(_Request, State) ->
    {noreply, State}.
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
-spec handle_info(Info :: term(), State :: state()) ->
    {noreply, State :: state()}.
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
-spec terminate(Reason :: term(), State :: state()) ->
    ok.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.
%%--------------------------------------------------------------------

%%%===================================================================
%%% state api
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
-spec state(String :: string()) ->
    State :: state().
%%--------------------------------------------------------------------
state(String) ->
    state_2(string_to_buf(String), os:getenv("LINES", 43)).
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
-spec state(String :: string(), Lines :: pos_integer()) ->
    state().
%%--------------------------------------------------------------------
state(String, Lines) ->
    state_2(string_to_buf(String), Lines).
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
-spec state_2(Buf :: list(string()), Lines :: pos_integer()) ->
    state().
%%--------------------------------------------------------------------
state_2(Buf, Lines) ->
    #state{buf = Buf, lines = Lines, position = 0}.
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
-spec page_2(State :: state()) ->
    String :: string().
%%--------------------------------------------------------------------
page_2(State) ->
    observer_cli_compose:pipe(State#state.buf, [
        fun(X) -> lists:sublist(X, State#state.position + 1, State#state.lines) end,
        fun buf_to_string/1
    ]).
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
-spec next_2(State :: state()) ->
    State2 :: state().
%%--------------------------------------------------------------------
next_2(State) ->
    case State#state.position + 1 of
        Position when Position > length(State#state.buf) - State#state.lines ->
            State;
        Position ->
            State#state{position = Position}
    end.
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
-spec prev_2(State :: state()) ->
    State2 :: state().
%%--------------------------------------------------------------------
prev_2(State) ->
    case State#state.position of
        0 ->
            State;
        Position ->
            State#state{position = Position - 1}
    end.
%%--------------------------------------------------------------------

%%%===================================================================
%%% buf
%%%===================================================================

string_to_buf(String) ->
    string:split(String, "\n", all).

buf_to_string(Buf) ->
    observer_cli_compose:pipe(Buf, [
        fun(X) -> string:join(X, "\n") end,
        fun(X) -> string:concat(X, "\n") end
    ]).

