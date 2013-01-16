%%%-------------------------------------------------------------------
%%% @author Niclas Axelsson <burbas@Niclass-MacBook-Pro-2.local>
%%% @copyright (C) 2013, Niclas Axelsson
%%% @doc
%%%
%%% @end
%%% Created : 15 Jan 2013 by Niclas Axelsson <burbas@Niclass-MacBook-Pro-2.local>
%%%-------------------------------------------------------------------
-module(migraterl).
-author('Niclas Axelsson <niclas@burbas.se>').
-behaviour(gen_server).

%% API
-export([
         start_link/0,
         migrate_module/2,
         migrate_module/3
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% Migrates a module with Modulename from the current node to Dest.
%% Same as migrate_module(Modulename, Dest, true)
%%
%% @spec migrate_module(Modulename :: atom(), Dest :: atom()) -> ok
%% @end
%%--------------------------------------------------------------------
migrate_module(Modulename, Dest) ->
    migrate_module(Modulename, Dest, [mapcalls]).

%%--------------------------------------------------------------------
%% @doc
%% Migrates a module with Modulename from the current node to Dest. If
%% Options contains 'map_calls' the local function will be remapped to use
%% RPC calls to Dest.
%%
%% @spec migrate_module(Modulename :: atom(), Dest :: atom(), Options :: proplist()) -> ok
%% @end
%%--------------------------------------------------------------------
migrate_module(Modulename, Dest, Options) ->
    gen_server:call(?SERVER, {migrate_module, Modulename, Dest, Options}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({migrate_module, Modulename, Dest, Options}, _From, State) ->
    %% First we get the binary code from the module
    {Modulename, OrgBinary, _} = code:get_object_code(Modulename),

    %% Get all the exports
    ModuleInfo = Modulename:module_info(),
    Exports = proplists:get_value(exports, ModuleInfo),

    %% Purge potential old code on remote node
    rpc:call(Dest, code, purge, [Modulename]),
    %% Load the given module on the destination node
    {module, Modulename} = rpc:call(Dest, erlang, load_module, [Modulename, OrgBinary]),

    %% See if we can generate the code
    case generate_migration_code(Modulename, Exports, Dest) of
        {ok, Modulename, GenBinary} ->
            %% Mark the local module as old
            [ {code:purge(Modulename),
               {module, Modulename} = code:load_binary(Modulename, "migraterl", GenBinary)} || proplists:get_value(map_calls, Options) ];
        {ok, Modulename, GenBinary, _Warnings} ->
            %% Mark the local module as old
            [ {code:purge(Modulename),
              {module, Modulename} = code:load_binary(Modulename, "migraterl", GenBinary)} || proplists:get_value(map_calls, Options) ]
    end,
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
generate_migration_code(Modulename, Exports, Destination) ->
    ModuleAST = erl_syntax:attribute(erl_syntax:atom(module),
                         [erl_syntax:atom(Modulename)]),

    ExportAST = erl_syntax:attribute(erl_syntax:atom(export),
                                     [erl_syntax:list(
                                        [ erl_syntax:arity_qualifier(
                                            erl_syntax:atom(FunName),
                                            erl_syntax:integer(FunArity)) || {FunName, FunArity} <- Exports, FunName /= module_info ]
                                       )]),

    FunctionsAST = [ generate_function({FunName, FunArity}, Modulename, Destination) || {FunName, FunArity} <- Exports, FunName /= module_info ],

    Forms = [ erl_syntax:revert(AST) || AST <- [ModuleAST, ExportAST | FunctionsAST] ],

    compile:forms(Forms).

generate_function({FunctionName, Arity}, Modulename, Destination) ->
    Args = [ erl_syntax:variable(erlang:integer_to_list(X)) || X <- lists:seq(1, Arity) ],

    erl_syntax:function(erl_syntax:atom(FunctionName),
                        [erl_syntax:clause(
                           Args, none,
                           [erl_syntax:application(
                              erl_syntax:atom(rpc),
                              erl_syntax:atom(call),
                              [erl_syntax:atom(Destination),
                               erl_syntax:atom(Modulename),
                               erl_syntax:atom(FunctionName),
                               erl_syntax:list(Args, none)
                              ]
                             )])]).
