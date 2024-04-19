-module(pollution_server).

-export([start/0, init/0, stop/0, loop/1, handle_cast_call/2, handle_request_call/3]).
-export([add_station/2, add_value/4, remove_value/3, get_one_value/3, get_station_mean/2, get_daily_mean/2, get_minimum_pollution_station/1, get_monitor/0, handle_response_back/0 ]).


start() -> 
    register ( ?MODULE, spawn(?MODULE, init, []) ).

init() -> 
    ?MODULE:loop ( pollution:create_monitor() ).

stop () ->
    ?MODULE ! stop.

handle_cast_call(Monitor, Response) ->
    case Response of 
        {error, _} -> ?MODULE:loop(Monitor);
        _ -> ?MODULE:loop(Response)
    end.

handle_request_call(Monitor, Response, PID) -> 
    case Response of 
        {error, _} -> 
            PID ! {error, request},
            ?MODULE:loop(Monitor);
        _ -> 
            PID ! Response,
            ?MODULE:loop(Monitor)
    end. 

handle_response_back() -> 
    receive Value -> Value
    after 300 -> {error, no_value_returned_from_server}
    end.
%%

%% Whatever to albo Coordinates albo StationName
loop (Monitor) -> 
    receive 
        {add_station, Name, Coordinates} ->
            NewMonitor = pollution:add_station(Name, Coordinates, Monitor),
            io:format ("~p", [NewMonitor]),
            handle_cast_call(Monitor, NewMonitor);

        {add_value, Whatever, Date, Type, Value} ->
            NewMonitor = pollution:add_value(Whatever, Date, Type, Value, Monitor),
            handle_cast_call(Monitor, NewMonitor);

        {remove_value, Whatever, Date, Type} -> 
            NewMonitor = pollution:remove_value(Whatever, Date, Type, Monitor),
            handle_cast_call(Monitor, NewMonitor);

        {get_one_value, PID, Whatever, Date, Type} -> 
            Value = pollution:get_one_value(Whatever, Date, Type, Monitor),
            handle_request_call(Monitor, Value, PID);
        
        {get_station_mean, PID, Whatever, Type} ->
            Value = pollution:get_station_mean(Whatever, Type, Monitor),
            handle_request_call(Monitor, Value, PID);

        {get_daily_mean, PID, Type, Date} ->
            Value = pollution:get_daily_mean(Type, Date, Monitor),
            handle_request_call(Monitor, Value, PID);

        {get_minimum_pollution_station, PID, Type} ->
            Value = pollution:get_minimum_pollution_station(Type, Monitor),
            handle_request_call(Monitor, Value, PID);
        
        {get_monitor, PID} -> 
            PID ! Monitor,
            handle_request_call(Monitor, {error, no_error}, PID);

        stop -> ok

    end.
%%

%%
add_station (Name, Coordinates) -> 
    ?MODULE ! {add_station, Name, Coordinates}. 

%%
add_value (Coordinates, Date, Type, Value) when is_tuple(Coordinates) ->
    ?MODULE ! {add_value, Coordinates, Date, Type, Value};

add_value (StationName, Date, Type, Value) when is_list(StationName) ->
    ?MODULE ! {add_value, StationName, Date, Type, Value}.
%%

%%
remove_value (Coordinates, Date, Type) when is_tuple(Coordinates) -> 
    ?MODULE ! {remove_value, Coordinates, Date, Type};

remove_value (StationName, Date, Type) when is_list(StationName) ->
    ?MODULE ! {remove_value, StationName, Date, Type}.
%%

%%
get_one_value (Coordinates, Date, Type) when is_tuple(Coordinates) -> 
    ?MODULE ! {get_one_value, self(), Coordinates, Date, Type},
    handle_response_back();

get_one_value (StationName, Date, Type) when is_list(StationName) ->
    ?MODULE ! {get_one_value, self(), StationName, Date, Type},
    receive Value -> Value end.
    % handle_response_back().
%%

%%
get_station_mean (Coordinates, Type) when is_tuple(Coordinates) ->
    ?MODULE ! {get_station_mean, self(), Coordinates, Type},
    handle_response_back();

get_station_mean (StationName, Type) when is_list(StationName) ->
    ?MODULE ! {get_station_mean, self(), StationName, Type},
    handle_response_back().
%%

%%
get_daily_mean (Type, Date) ->
    ?MODULE ! {get_daily_mean, self(), Type, Date},
    handle_response_back().

get_minimum_pollution_station(Type) ->
    ?MODULE ! {get_minimum_pollution_staion, self(), Type},
    handle_response_back().
%%

get_monitor() -> 
    ?MODULE ! {get_monitor, self()},
    handle_response_back().
