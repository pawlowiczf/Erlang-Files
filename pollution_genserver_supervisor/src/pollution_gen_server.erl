-module(pollution_gen_server).
-behaviour(gen_server).
-export([start_link/0, handle_call/3, init/1, handle_cast/2]).
-export([add_station/2, add_value/4, remove_value/3, get_one_value/3, get_station_mean/2, get_daily_mean/2, get_minimum_pollution_station/1, get_monitor/0, crash/0]).

start_link() -> 
    gen_server:start_link( {local, ?MODULE}, ?MODULE, pollution:create_monitor(), []).

init(Monitor) -> 
    {ok, Monitor}.

%% Handle cast:

handle_cast( {add_station, Name, Coordinates}, Monitor ) ->
    NewMonitor = pollution:add_station(Name, Coordinates, Monitor),
    {noreply, NewMonitor};

handle_cast( {add_value, Whatever, Date, Type, Value}, Monitor ) ->
    NewMonitor = pollution:add_value(Whatever, Date, Type, Value, Monitor),
    {noreply, NewMonitor};

handle_cast( {remove_value, Whatever, Date, Type}, Monitor ) ->
    NewMonitor = pollution:remove_value(Whatever, Date, Type, Monitor),
    {noreply, NewMonitor}.

%% Handle call:

handle_call( {get_one_value, Whatever, Date, Type}, _, Monitor ) -> 
    Value = pollution:get_one_value(Whatever, Date, Type, Monitor),
    {reply, Value, Monitor};

handle_call( {get_station_mean, Whatever, Type}, _, Monitor ) ->
    Value = pollution:get_station_mean(Whatever, Type, Monitor),
    {reply, Value, Monitor};

handle_call( {get_daily_mean, Type, Date}, _, Monitor ) -> 
    Value = pollution:get_daily_mean(Type, Date, Monitor),
    {reply, Value, Monitor};

handle_call( {get_minimum_pollution_station, Type}, _, Monitor ) ->
    Value = pollution:get_minimum_pollution_station(Type, Monitor),
    {reply, Value, Monitor};

handle_call( {get_monitor}, _, Monitor ) -> 
    {reply, Monitor, Monitor}.


%% API:

%%
add_station (Name, Coordinates) ->  
    gen_server:cast( ?MODULE, {add_station, Name, Coordinates} ).

%%
add_value (Coordinates, Date, Type, Value) when is_tuple(Coordinates) ->
    gen_server:cast( ?MODULE, {add_value, Coordinates, Date, Type, Value} );
    
add_value (StationName, Date, Type, Value) when is_list(StationName) ->
    gen_server:cast( ?MODULE, {add_value, StationName, Date, Type, Value} ).
%%

%%
remove_value (Coordinates, Date, Type) when is_tuple(Coordinates) -> 
    gen_server:cast( ?MODULE, {remove_value, Coordinates, Date, Type} );

remove_value (StationName, Date, Type) when is_list(StationName) ->
    gen_server:cast( ?MODULE, {remove_value, StationName, Date, Type} ).
%%

%%
get_one_value (Coordinates, Date, Type) when is_tuple(Coordinates) -> 
    gen_server:call( ?MODULE, {get_one_value, Coordinates, Date, Type} );

get_one_value (StationName, Date, Type) when is_list(StationName) ->
    gen_server:call( ?MODULE, {get_one_value, StationName, Date, Type} ).

%%

%%
get_station_mean (Coordinates, Type) when is_tuple(Coordinates) ->
    gen_server:call( ?MODULE, {get_station_mean, Coordinates, Type} );

get_station_mean (StationName, Type) when is_list(StationName) ->
    gen_server:call( ?MODULE, {get_station_mean, StationName, Type} ).
%%

%%
get_daily_mean (Type, Date) ->
    gen_server:call( ?MODULE, {get_daily_mean, Type, Date} ).

get_minimum_pollution_station(Type) ->
    gen_server:call( ?MODULE, {get_minimum_pollution_station, Type} ).

%%

get_monitor() -> 
    gen_server:call( ?MODULE, {get_monitor} ).

crash() -> gen_server:call( ?MODULE, {no_match} ).