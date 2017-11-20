-module(db_server).
-behaviour(gen_server).

%% API
-export([start_link/0,
         get_state/0,
         connect/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {master}).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

get_state() ->
    gen_server:call(?SERVER, get_state).

connect(Node) ->
    gen_server:call(?SERVER, {connect, Node}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    process_flag(trap_exit, true),
    ok = db_lib:init(master),
    {ok, #state{master = node()}}.

handle_call(get_state, _From, State) ->
    Reply = State,
    {reply, Reply, State};

handle_call({connect, Node}, _From, State) ->
    NewState = handle_connect(Node, State),
    {reply, connected, NewState};

handle_call(whois_master, _From, State = #state{master = Master}) ->
    Reply = {master, Master},
    {reply, Reply, State};

handle_call({new_node, Client}, _From, State) ->
    ok = new_node_connect(Client),
    {reply, done, State};

handle_call({master_assign, NewMaster}, _From, _State) ->
    {noreply, #state{master = NewMaster}};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    case State#state.master == node() of
        true ->
            case nodes() of
                [] -> ok;
                Nodes ->
                    Msg = {master_assign, hd(Nodes)},
                    gen_server:multi_call(Nodes, ?SERVER, Msg)
            end;
        false ->
            ok
    end,
    ok = db_lib:cleanup(),
    stopped.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
handle_connect(Node, State) ->
    try
        pong = net_adm:ping(Node),
        {master, Master} = whois_master(Node),
        ok = db_lib:init(client),
        done = connect_to_master(Master),
        ok = db_lib:change_config(client, Master),
        #state{master = Master}
    catch _:_ ->
            State
    end.

whois_master(Node) ->
    gen_server:call({?SERVER, Node}, whois_master).

connect_to_master(Master) ->
    gen_server:call({?SERVER, Master}, {new_node, node()}).

new_node_connect(Client) ->
    lager:info("Master change config for ~p", [Client]),
    db_lib:change_config(master, Client).
