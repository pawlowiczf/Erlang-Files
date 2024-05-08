-module(pollution_value_collector_gen_statem).
-export([init/1, start_link/0, callback_mode/0]).
-export([setStation/1, addValue/3, storeData/0]).
-export([add_measure/3, set_station/3]).

-behaviour(gen_statem).
-callback_mode(stateless).

start_link()  -> 
    gen_statem:start_link( {local, ?MODULE}, ?MODULE, [], [] ).

init(Data) -> {ok, set_station, Data}.

%% Api:
setStation(StationName) when is_list(StationName) ->
    gen_statem:cast( ?MODULE, {set_station, StationName} );

setStation(Coordinates) when is_tuple(Coordinates) -> 
    gen_statem:cast( ?MODULE, {set_station, Coordinates} ).

addValue( Date, Type, Value ) -> 
    gen_statem:cast( ?MODULE, {add_measure, Date, Type, Value} ).

storeData() -> 
    gen_statem:cast( ?MODULE, {store_data} ). 
%%

set_station( _Event, {set_station, Whatever}, _Data ) ->
    {next_state, add_measure, {Whatever, []} }.

add_measure( _Event, {add_measure, Date, Type, Value}, {Whatever, Measurements} ) -> 
    Measure = {Date, Type, Value},
    NewMeasurements = [ Measure | Measurements ],
    {next_state, add_measure, {Whatever, NewMeasurements}};
    
add_measure( _Event, {store_data}, {Whatever, Measurements} ) -> 
    [ pollution_gen_server:add_value(Whatever, Date, Type, Value) || {Date, Type, Value} <- Measurements ],
    {next_state, add_measure, {Whatever, Measurements}}.
    % gen_statem:stop(?MODULE).

callback_mode()-> state_functions.

