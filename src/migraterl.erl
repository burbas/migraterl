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
         migrate_module/3,
         migrate_application/2,
         migrate_application/3,
         migrate_back_module/2,
         migrate_back_module/3,
         migrate_back_application/2,
         migrate_back_application/3
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
%% Migrates an application from the current node to Dest.
%% Same as migrate_application(Application, Dest, [map_calls])
%%
%% @spec migrate_application(Application :: atom(), Dest :: atom()) -> ok | {error, Reason}
%% @end
%%--------------------------------------------------------------------
migrate_application(Application, Dest) ->
    migrate_application(Application, Dest, [map_calls]).

%%--------------------------------------------------------------------
%% @doc
%% Migrates an application from the current node to Dest with a set
%% of Options.
%%
%% @spec migrate_application(Application :: atom(), Dest :: atom(), Options :: proplist()) ->
%%                                                                        ok | {error, Reason}
%% @end
%%--------------------------------------------------------------------
migrate_application(Application, Dest, Options) ->
    case application:get_key(Application, modules) of
        undefined ->
            {error, application_not_found};
        {ok, Modules} ->
            [ migrate_module(Modulename, Dest, Options) || Modulename <- Modules ],
            Appfile = erlang:atom_to_list(Application) ++ ".app",
            {ok, Content} = file:read_file(code:where_is_file(Appfile)),
            rpc:call(Dest, file, write_file, [Appfile, Content]),
            rpc:call(Dest, application, start, [Application])
    end.

%%--------------------------------------------------------------------
%% @doc
%% Migrates a module with Modulename from the current node to Dest.
%% Same as migrate_module(Modulename, Dest, [map_calls])
%%
%% @spec migrate_module(Modulename :: atom(), Dest :: atom()) -> ok
%% @end
%%--------------------------------------------------------------------
migrate_module(Modulename, Dest) ->
    migrate_module(Modulename, Dest, [map_calls]).

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


%%--------------------------------------------------------------------
%% @doc
%% Moves an application from a remote node to the current one.
%% Same as migrate_back_application(Modulename, Source, [map_calls])
%%
%% @spec migrate_back_application(Modulename :: atom(), Source :: atom()) -> ok | {error, Reason}
%% @end
%%--------------------------------------------------------------------
migrate_back_application(Application, Source) ->
    migrate_back_application(Application, Source, [map_calls]).

%%--------------------------------------------------------------------
%% @doc
%% Moves an application from a remote node to the current one.
%% If 'map_calls' is present in the Options all functions in the modules
%% belonging to the application will be remapped with RPC calls to this node.
%%
%% @spec migrate_back_application(Modulename :: atom(), Source :: atom(), Options :: proplist()) -> ok | {error, Reason}
%% @end
%%--------------------------------------------------------------------
migrate_back_application(Application, Source, Options) ->
    case rpc:call(Source, application, get_key, [Application, modules]) of
        undefined ->
            {error, application_not_found};
        {ok, Modules} ->
            [ migrate_back_module(Modulename, Source, Options) || Modulename <- Modules ],
            Appfile = erlang:atom_to_list(Application) ++ ".app",
            Filename = rpc:call(Source, code, where_is_file, [Appfile]),
            {ok, Content} = rpc:call(Source, file, read_file, [Filename]),
            file:write_file(Appfile, Content),
            application:start(Application)
    end.

%%--------------------------------------------------------------------
%% @doc
%% Moves a module from a remote node to the current one.
%% Same as migrate_back_module(Modulename, Source, [map_calls])
%%
%% @spec migrate_back_module(Modulename :: atom(), Source :: atom()) -> ok | {error, Reason}
%% @end
%%--------------------------------------------------------------------
migrate_back_module(Modulename, Source) ->
    migrate_back_module(Modulename, Source, [map_calls]).

%%--------------------------------------------------------------------
%% @doc
%% Moves a module from a remote node to the current one.
%% If 'map_calls' is present in Options all functions of the remote module will be remapped
%% to use RPC:call to this node.
%%
%% @spec migrate_back_module(Modulename :: atom(), Source :: atom()) -> ok | {error, Reason}
%% @end
%%--------------------------------------------------------------------
migrate_back_module(Modulename, Source, Options) ->
    gen_server:call(?SERVER, {migrate_back_module, Modulename, Source, Options}).


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
    {ok, Modulename, GenBinary} = generate_migration_code(Modulename, Exports, Dest),
    case proplists:get_value(map_calls, Options) of
        true ->
            %% Mark the local module as old
            code:purge(Modulename),
            {module, Modulename} = code:load_binary(Modulename, "migraterl", GenBinary);
        _ ->
            ok
    end,

    {reply, ok, State};

handle_call({migrate_back_module, Modulename, Source, Options}, _From, State) ->
    %% Load the binary code from the source-node
    {Modulename, OrgBinary, _} = rpc:call(Source, code, get_object_code, [Modulename]),

    %% Get all exports
    ModuleInfo = rpc:call(Source, Modulename, module_info, []),
    Exports = proplists:get_value(exports, ModuleInfo),

    %% Purge the module on current node
    code:purge(Modulename),

    %% Load the module
    erlang:load_module(Modulename, OrgBinary),

    {ok, Modulename, GenBinary} = generate_migration_code(Modulename, Exports, node()),
    case proplists:get_value(map_calls, Options) of
        true ->
            rpc:call(Source, code, purge, [Modulename]),
            rpc:call(Source, erlang, load_binary, [Modulename, "migraterl", GenBinary]);
        _ ->
            ok
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

    FunctionsAST = [ generate_rpc_function({FunName, FunArity}, Modulename, Destination) || {FunName, FunArity} <- Exports, FunName /= module_info ],

    Forms = [ erl_syntax:revert(AST) || AST <- [ModuleAST, ExportAST | FunctionsAST] ],

    compile:forms(Forms, []).

generate_rpc_function({FunctionName, Arity}, Modulename, Destination) ->
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
