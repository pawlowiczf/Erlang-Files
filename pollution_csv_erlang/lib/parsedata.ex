defmodule ParseData do
	#
	# @unixDate "2024-02-10T13:00:00.000Z;PRESSURE;996.38;57570;Polska, Kraków, Floriana Straszewskiego;50.057224,19.933157"
	# Code.append_path("C:/Users/User/Documents/MEGA/ErlangElixir/erlang/pollutionOTP/_build/default/lib/pollutionOTP/ebin")

	def parseCSV() do
		lines = Reader.loadData("AirlyData-ALL-50k.csv")
		for row <- lines do entryFromRow(row) end
	end

	def parseDate(unixDate) do
		#
		firstPart = String.slice(unixDate, 0..9)
			|> String.split("-")
			|> Enum.map( fn val -> String.to_integer(val) end )
			|> List.to_tuple()

		secondPart = String.slice(unixDate, 11..18)
			|> String.split(":")
			|> Enum.map( fn val -> String.to_integer(val) end )
			|> List.to_tuple()

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

		date = parseDate(unixDate)
		value = String.to_float(value)
		stationID = String.to_integer(stationID)
		location = parseLocation(location)

		%{ :location => location, :value => value, :dateTime => date, :stationName => stationName, :stationID => stationID, :type => type }
	end

end
