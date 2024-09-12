defmodule Pollutiondb.Station do
  use Ecto.Schema
  require Ecto.Query

  schema "stations" do
      field :name, :string
      field :lon, :float
      field :lat, :float
      has_many :readings, Pollutiondb.Reading
  end

  def add(station) do
    station
      |> changeset(%{})
      |> Pollutiondb.Repo.insert()
  end

  def get_id_by_name(name) do
    %Pollutiondb.Station{id: id} = Pollutiondb.Repo.get_by(Pollutiondb.Station, name: name)
    id 
  end

  def get_all() do
    Pollutiondb.Repo.all(Pollutiondb.Station)
  end

  def get_by_id(id) do
    Pollutiondb.Repo.get(Pollutiondb.Station, id)
  end

  def remove(station) do
    Pollutiondb.Repo.delete(station)
  end

  def find_by_name(name) do
    Pollutiondb.Repo.all( Ecto.Query.where(Pollutiondb.Station, name: ^name) ) # ^name - zmienna z zewnatrz (nie z tabeli)
  end

  def find_by_location(lon, lat) do
    Ecto.Query.from(s in Pollutiondb.Station,
    where: s.lon == ^lon,
    where: s.lat == ^lat)
    |> Pollutiondb.Repo.all
  end

  def find_by_location_range(lon_min, lon_max, lat_min, lat_max) do
    Ecto.Query.from(s in Pollutiondb.Station,
      where: s.lon >= ^lon_min and s.lon <= ^lon_max,
      where: s.lat >= ^lat_min and s.lat <= ^lat_max )
      |> Pollutiondb.Repo.all
  end

  def update_name(station, newname) do
    station
    |> changeset(%{name: newname})
    |> Pollutiondb.Repo.update

    # station
    # |> Ecto.Changeset.cast(%{name: newname}, [:name]) # (schema, map, allowedChanges) (map with new values to change)
    # |> Ecto.Changeset.validate_required([:name]) # checks whether name is not empty (is might not be present in a map)
    # |> Pollutiondb.Repo.update
  end

  def add(name, lon, lat) do
    %Pollutiondb.Station{}
    |> changeset(%{name: name, lon: lon, lat: lat})
    |> Pollutiondb.Repo.insert()
  end

  defp changeset(station, changesmap) do
    station
    |> Ecto.Changeset.cast( changesmap, [:name, :lon, :lat])
    |> Ecto.Changeset.validate_required([:name, :lon, :lat])
    |> Ecto.Changeset.validate_number(:lon, greater_than_or_equal_to: -180, less_than_or_equal_to: 180)
    |> Ecto.Changeset.validate_number(:lat, greater_than_or_equal_to: -90, less_than_or_equal_to: 90)
    |> Ecto.Changeset.validate_length(:name, min: 1)
  end

end
