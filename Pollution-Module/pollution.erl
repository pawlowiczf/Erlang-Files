-module(pollution).

-export([create_monitor/0, add_station/3, add_value/5, remove_value/4, get_one_value/4, get_station_mean/3, get_daily_mean/3, traverse_keys/2, get_map_keys/1,
    only_one_Coordinates/2, only_one_Name/2, unique_measure/3]).

-record(station, {name, coordinates}).
-record(measure, {type, localTime}).
% Kluczami są rekordy station, a wartością są mapy, której kluczami jest measure, a wartością wartość pomiaru. 

create_monitor () -> #{}.

traverse_keys ( [], _ ) -> {error, "Nie znaleziono klucza w mapie"};
traverse_keys ( [H|T], Coordinates ) when is_tuple(Coordinates) -> 
    case H of 
        #station{ name = _, coordinates = Coordinates } -> H;
        _ -> traverse_keys(T, Coordinates)
    end;

traverse_keys ( [H|T], StationName ) when is_list(StationName) -> 
    case H of 
        #station{ name = StationName, coordinates = _ } -> H;
        _ -> traverse_keys(T, StationName)
    end.
%%

unique_measure ( Measurement, Date, Type ) -> 
    not( lists:any ( fun (Rec) -> #measure{ localTime = Date, type = Type} == Rec end, Measurement) ).


get_map_keys (Monitor) -> [ K || K := _ <- Monitor ].

only_one_Name ( [], _ ) -> true;
only_one_Name ( [H|T], StationName ) -> 
    case H of 
        #station{ name = StationName } -> false;
        _ -> only_one_Name(T, StationName)
    end.
%%

only_one_Coordinates ( [], _ ) -> true; 
only_one_Coordinates ( [H|T], Coordinates ) -> 
    case H of 
        #station{ coordinates = Coordinates } -> false; 
        _ -> only_one_Coordinates(T, Coordinates)
    end.
%%

add_station ( Name, Coordinates, Monitor ) ->
    StationsMaps = get_map_keys (Monitor),
    case only_one_Coordinates (StationsMaps, Coordinates) and only_one_Name (StationsMaps, Name) of 
        true -> Monitor#{ #station{name = Name, coordinates = Coordinates} => #{} };
        _ -> {error, "Taka stacja juz istnieje"}
    end.

add_value ( Coordinates, Date, Type, Value, Monitor ) when is_tuple(Coordinates) ->
    Station = traverse_keys( get_map_keys(Monitor), Coordinates ),

    case Station of 
        {error, _} -> {error, "Nie znaleziono klucza w mapie"};
        _ ->
            Measurement = maps:get( Station, Monitor ),
            case unique_measure ( [K || K := _ <- Measurement], Date, Type ) of 
                true -> 
                    UpdatedMeasurement = Measurement#{ #measure{type = Type, localTime = Date} => Value },
                    Monitor#{ Station := UpdatedMeasurement };
                _ -> {error, "Taki pomiar juz istnieje"}
            end
    end;

add_value ( StationName, Date, Type, Value, Monitor ) when is_list(StationName) ->
    Station = traverse_keys( get_map_keys(Monitor), StationName ),
    case Station of
        {error, _} -> {error, "Nie znaleziono klucza w mapie"};
        _ ->
            Measurement = maps:get( Station, Monitor ), 
            case unique_measure ( [ K || K := _ <- Measurement], Date, Type ) of 
                true ->           
                    UpdatedMeasurement = Measurement#{ #measure{type = Type, localTime = Date} => Value },
                    Monitor#{ Station := UpdatedMeasurement };
                _ -> {error, "Taki pomiar juz istnieje"}
            end
    end.
%%

remove_value (Coordinates, Date, Type, Monitor) when is_tuple(Coordinates) ->
    Station = traverse_keys( get_map_keys(Monitor), Coordinates ),
    if 
        Station == {error, "Nie znaleziono klucza w mapie"} -> {error, "Nie ma takiej stacji"};
        true ->
            Measurement = maps:get( Station, Monitor ),

            case maps:is_key( #measure{type = Type, localTime = Date}, Measurement ) of 
                false -> {error, "Brak klucza w pomiarach do usuniecia"};
                _ -> 
                    UpdatedMeasurement = maps:filter( fun (K, _) -> K /= #measure{type = Type, localTime = Date} end, Measurement),
                    Monitor#{ Station := UpdatedMeasurement }
            end
    end;

remove_value (StationName, Date, Type, Monitor) when is_list(StationName) ->
    Station = traverse_keys( get_map_keys(Monitor), StationName ),
    if 
        Station == {error, "Nie znaleziono klucza w mapie"} -> {error, "Nie ma takiej stacji"};
        true ->
            Measurement = maps:get( Station, Monitor ),

            case maps:is_key( #measure{type = Type, localTime = Date}, Measurement ) of 
                false -> {error, "Brak klucza w pomiarach do usuniecia"};
                _ ->
                    UpdatedMeasurement = maps:filter( fun (K, _) -> K /= #measure{type = Type, localTime = Date} end, Measurement),
                    Monitor#{ Station := UpdatedMeasurement }
            end
    end.
%%

get_one_value (Coordinates, Date, Type, Monitor) when is_tuple(Coordinates) -> 
    Station = traverse_keys( get_map_keys(Monitor), Coordinates ),

    if 
        Station == {error, "Nie znaleziono klucza w mapie"} -> {error, "Nie ma takiej stacji"};
        true ->  
            Measurement = maps:get( Station, Monitor ),
            
            case maps:is_key( #measure{type = Type, localTime = Date}, Measurement ) of
                false -> {error, "Brak klucza w pomiarach do wypisania wartosci"};
                _ ->
                    #{ #measure{ type = Type, localTime = Date } := Value } = Measurement,
                    Value
            end
    end;

get_one_value (StationName, Date, Type, Monitor) when is_list(StationName) -> 
    Station = traverse_keys( get_map_keys(Monitor), StationName ),

    if 
        Station == {error, "Nie znaleziono klucza w mapie"} -> {error, "Nie ma takiej stacji"};
        true ->  
            Measurement = maps:get( Station, Monitor ),

            case maps:is_key( #measure{type = Type, localTime = Date}, Measurement ) of
                false -> {error, "Brak klucza w pomiarach do wypisania wartosci"};
                _ ->
                    #{ #measure{ type = Type, localTime = Date } := Value } = Measurement,
                    Value
            end
    end.
%%

get_station_mean (Coordinates, Type, Monitor) when is_tuple(Coordinates) -> 
    Station = traverse_keys( get_map_keys(Monitor), Coordinates ),

    if 
        Station == {error, "Nie znaleziono klucza w mapie"} -> {error, "Nie ma takiej stacji"};
        true -> 
            Measurement = maps:get( Station, Monitor ),
            LocalF = fun (K) ->  K#measure.type == Type end,
            MeasurementType = [ V || K := V <- Measurement, LocalF(K) ],

            case length (MeasurementType) of 
                0 -> {error, "Zero pomiarow"};
                _ -> lists:sum( MeasurementType ) / length( MeasurementType )
            end
    end;

get_station_mean (StationName, Type, Monitor) when is_list(StationName) -> 
    Station = traverse_keys( get_map_keys(Monitor), StationName ),

    if 
        Station == {error, "Nie znaleziono klucza w mapie"} -> {error, "Nie ma takiej stacji"};    
        true -> 
            Measurement = maps:get( Station, Monitor ),
            LocalF = fun (K) ->  K#measure.type == Type end,
            MeasurementType = [ V || K := V <- Measurement, LocalF(K) ],

            case length (MeasurementType) of 
                0 -> {error, "Zero pomiarow"};
                _ -> lists:sum( MeasurementType ) / length( MeasurementType )
            end
    end.
%%

get_daily_mean (Type, Date, Monitor) -> 
    StationsMaps = [ V || _ := V <- Monitor ], % lista zawierajaca mapy kazdej stacji z pomiarami 

    LocalF = fun (Map) -> [ V || K := V <- Map, element(1, K#measure.localTime) == Date, K#measure.type == Type ] end, 
    MeasurementsStations = lists:map( LocalF, StationsMaps ),

    FlattenMeasurement = [ B || A <- MeasurementsStations, B <- A ],

    case length (FlattenMeasurement) of 
        0 -> {error, "Zero pomiarow"};
        _ -> lists:sum( FlattenMeasurement ) / length( FlattenMeasurement )
    end.
%%