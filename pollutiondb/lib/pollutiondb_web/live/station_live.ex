defmodule PollutiondbWeb.StationLive do
  use PollutiondbWeb, :live_view

  alias Pollutiondb.Station

  def mount(_params, _session, socket) do
    socket = assign(socket, stations: Station.get_all(), name: "", lat: "", lon: "", query: "", queryStations: Station.get_all())
    {:ok, socket}
  end

  def render(assigns) do
    ~H"""

    Create new station
    <form phx-submit="insert">
      Name: <input type="text" name="name" value={@name} /><br/>
      Lat: <input type="number" name="lat" step="0.1" value={@lat} /><br/>
      Lon: <input type="number" name="lon" step="0.1" value={@lon} /><br/>
      <input type="submit" />
    </form>

    <table>
      <tr>
        <th>Name</th><th>Longitude</th><th>Latitude</th>
      </tr>
      <%= for station <- @stations do %>
        <tr>
          <td><%= station.name %></td>
          <td><%= station.lon %></td>
          <td><%= station.lat %></td>
        </tr>
      <% end %>
    </table>

    Look for the station. Provide its name:
    <form phx-change="lookFor">
      Name: <input type="text" name="query" value={@query} /><br/>
      <input type="submit" />
    </form>

    <table>
      <tr>
        <th>Name</th><th>Longitude</th><th>Latitude</th>
      </tr>
      <%= for station <- @queryStations do %>
        <tr>
          <td><%= station.name %></td>
          <td><%= station.lon %></td>
          <td><%= station.lat %></td>
        </tr>
      <% end %>
    </table>
    """
  end

  def handle_event("insert", %{"name" => name, "lat" => lat, "lon" => lon}, socket) do
    Station.add(%Station{name: name, lat: to_float(lat, 0.0), lon: to_float(lon, 0.0)})
    socket = assign(socket, stations: Station.get_all(), name: name, lat: lat, lon: lon)
    {:noreply, socket}
  end

  def handle_event("lookFor", %{"query" => query}, socket) do
    #
    updatedSocket =
      if query == "" do
        assign(socket, queryStations: Station.get_all())
      else
        case Station.find_by_name(query) do
          nil -> assign(socket, queryStations: Station.get_all())
          station -> assign(socket, queryStations: station)
        end
      end

    {:noreply, updatedSocket}
  end

  def to_float(value, default) do
    if value == "" do
      default
    else
      String.to_float(value)
    end
  end
end
