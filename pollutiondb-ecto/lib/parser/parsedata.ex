defmodule ParseData do

	def parseCSV() do
		lines = Reader.loadData("AirlyData-ALL-50k.csv")
		for row <- lines do entryFromRow(row) end
	end

	def parseDate(unixDate) do
		#
		firstPart = String.slice(unixDate, 0..9)
		secondPart = String.slice(unixDate, 11..18)

		{firstPart, secondPart}
	end

	def parseLocation(location) do
		String.split(location, ",")
			|> Enum.map( fn val -> String.to_float(val) end )
			|> List.to_tuple()
	end

	def entryFromRow(row) do
		#
		[unixDate, type, value, stationID, stationName, location] = String.split(row, ";")

		{date, time} = parseDate(unixDate)
		value = String.to_float(value)
		stationID = String.to_integer(stationID)
		{lon, lat} = parseLocation(location)
    newStationName = "#{stationID}" <> " " <> "#{stationName}"

		%{ :lon => lon, :lat => lat, :value => value, :date => date, :time => time, :stationName => newStationName, :type => type }
	end
end
