defmodule Adder do
  #
	def identifyStations(stations) do
		#
		Enum.uniq_by(stations, fn station -> station.stationName end)
	end

	def addStations(stations) do
		identifyStations(stations)
			|> Enum.each( fn station -> Pollutiondb.Station.add(station.stationName, station.lon, station.lat) end )
			# mapa nazwa stacji -> id 
	end

	def addValues(stations) do

		Enum.each( stations, fn station -> Pollutiondb.Reading.add(Pollutiondb.Station.get_id_by_name(station.stationName), station.date, station.time, station.type, station.value) end )
	end

	def addStationValues do
		parsedCSV = ParseData.parseCSV()
		addStations(parsedCSV)
    addValues(parsedCSV)
	end

	def testAdd do

	end
end
