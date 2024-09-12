defmodule Pollutiondb.Reading do
  #
  use Ecto.Schema
  require Ecto.Query

  schema "readings" do
    field :date, :date
    field :time, :time
    field :type, :string
    field :value, :float
    belongs_to :station, Pollutiondb.Station
  end

  def add_now(station, type, value) do
    # %Pollutiondb.Reading{type: type, value: value, date: Date.utc_today, time: Time.utc_now, station_id: station}

    %Pollutiondb.Reading{}
    |> changeset(%{station_id: station, type: type, value: value, date: Date.utc_today, time: Time.utc_now})
    |> Pollutiondb.Repo.insert
  end

  def changeset(reading, changesmap) do
    reading
    |> Ecto.Changeset.cast(changesmap, [:date, :time, :type, :value, :station_id])
    |> Ecto.Changeset.cast_assoc(:station)
    |> Ecto.Changeset.validate_required([:date, :time, :type, :value])
  end

  def find_by_date(date) do
    Ecto.Query.where(Pollutiondb.Reading, date: ^date)
    |> Pollutiondb.Repo.all
  end

  def find_by_station_id(id) do
    Ecto.Query.where(Pollutiondb.Reading, station_id: ^id)
    |> Pollutiondb.Repo.all
  end

  def add(station, date, time, type, value) do
    %Pollutiondb.Reading{}
    |> changeset( %{station_id: station, date: date, time: time, type: type, value: value} )
    |> Pollutiondb.Repo.insert
  end

  def get_latest_10_readings() do
    Ecto.Query.from(r in Pollutiondb.Reading,
    limit: 10, order_by: [desc: r.date, desc: r.time])
    |> Pollutiondb.Repo.all()
    |> Pollutiondb.Repo.preload(:station)
  end
end
