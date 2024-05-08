-module(var_server).
-behaviour(gen_server).

-export([ start_link/1, init/1, handle_call/3, handle_cast/2, getValue/0, incValue/0, terminate/2, stop/0]).

start_link(InitialValue) -> 
    % gen_server:start_link( {local, Name}, Module, Arguments, Options ).
    gen_server:start_link( {local, var_server}, ?MODULE, InitialValue, [] ).

init(InitialValue) ->
    {ok, InitialValue}.

handle_call( {getValue}, _From, State ) ->
    {reply, State, State}. 

handle_cast( {incValue}, State ) ->
    {noreply, State + 1};

handle_cast( stop, State ) -> 
    {stop, normal, State}.

terminate(Reason, State) ->
    io:format("Server: exit with the value ~p~n", [State]),
    Reason.
%% 
getValue() -> 
    gen_server:call(var_server, {getValue}).

incValue() ->
    gen_server:cast(var_server, {incValue}).

stop() -> 
    gen_server:cast(var_server, stop).
