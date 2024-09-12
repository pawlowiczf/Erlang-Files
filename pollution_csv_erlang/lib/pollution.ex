defmodule Loader do
	#
	Code.append_path("C:/Users/User/Documents/MEGA/ErlangElixir/erlang/pollutionOTP/_build/default/lib/pollutionOTP/ebin")
	# Code.append_path("C:/Users/Filip/Documents/MEGA/ErlangElixir/erlang/pollutionOTP/_build/default/lib/pollutionOTP/ebin")

	def identifyStations(stations) do
		#
		Enum.uniq_by(stations, fn station -> station.location end)
			|> Enum.map( fn station -> { "#{station.stationID}" <> " " <> station.stationName, station.location } end )
	end

	def addStations(stations) do
		identifyStations(stations)
			|> Enum.each( fn station -> :pollution_gen_server.add_station( elem(station, 0), elem(station, 1) ) end )
	end

	def addValues(stations) do
		Enum.each( stations, fn station -> :pollution_gen_server.add_value( station.location, station.dateTime, station.type, station.value ) end )
	end

	def getMonitor() do
		:pollution_gen_server.get_monitor()
	end

	def addStationValues do
		parsedCSV = ParseData.parseCSV()
		addStations(parsedCSV)

		{time, _} = :timer.tc( &addValues/1, [parsedCSV] )
		IO.puts(time)

	end

	def check() do
		# {time, _} = :timer.tc( &:pollution_gen_server.get_station_mean/2, ["9910 Polska, Kraków, Studencka", "PM10"] )
		val = :pollution_gen_server.get_station_mean( "9910 Polska, Kraków, Studencka", "PM10" )
		IO.puts(val)
		{time, _} = :timer.tc( &:pollution_gen_server.get_station_mean/2, ["9910 Polska, Kraków, Studencka", "PM10"] )
		IO.puts("Czas pomiary: #{time}")

	end
end

# Code.append_path("C:/Users/Filip/Documents/MEGA/ErlangElixir/erlang/pollutionOTP/_build/default/lib/pollutionOTP/ebin")
# Application.start(:pollutionOTP)
