defmodule PollutiondbWeb.StationRangeLive do
  use PollutiondbWeb, :live_view
  alias Pollutiondb.Station

  def mount(_params, _session, socket) do
    socket = assign(socket, stations: Station.get_all(), lat_min: 0, lat_max: 0, lon_min: 0, lon_max: 0)
    {:ok, socket}
  end

  def render(assigns) do
    ~H"""
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
      <br>

      <form phx-change="update">
      Lat min
      <input type="range" min="-90" max="90" name="lat_min" value={@lat_min}/><br/>
      Lat max
      <input type="range" min="-90" max="90" name="lat_max" value={@lat_max}/><br/>
      Lon min
      <input type="range" min="-180" max="180" name="lon_min" value={@lon_min}/><br/>
      Lon max
      <input type="range" min="-180" max="180" name="lon_max" value={@lon_max}/><br/>
      </form>

      <form phx-submit="reset">
        <input type="submit"/>
      </form>

    """
  end

  def handle_event("update", %{"lat_min" => lat_min, "lat_max" => lat_max, "lon_min" => lon_min, "lon_max" => lon_max}, socket) do
    lat_min2 = String.to_integer(lat_min)
    lat_max2 = String.to_integer(lat_max)
    lon_min2 = String.to_integer(lon_min)
    lon_max2 = String.to_integer(lon_max)

    newSocket =
      case Station.find_by_location_range(lon_min2, lon_max2, lat_min2, lat_max2) do
        nil -> assign(socket, stations: [], lat_min: lat_min, lat_max: lat_max, lon_min: lon_min, lon_max: lon_max)
        stations -> assign(socket, stations: stations, lat_min: lat_min, lat_max: lat_max, lon_min: lon_min, lon_max: lon_max)
      end

    {:noreply, newSocket}
  end

  def handle_event("reset", _params, socket) do
    newSocket = assign(socket, stations: Station.get_all())
    {:noreply, newSocket}
  end
end
