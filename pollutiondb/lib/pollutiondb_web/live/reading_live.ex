defmodule PollutiondbWeb.ReadingLive do
  use PollutiondbWeb, :live_view

  alias Pollutiondb.Reading
  alias Pollutiondb.Station

  def mount(_params, _session, socket) do
    socket = assign(socket, readings: Reading.get_latest_10_readings(), stations: Station.get_all(), station_id: 1)
    {:ok, socket}
  end

  def render(assigns) do
    ~H"""
      <table>
        <tr>
          <th>Name</th><th>Date</th><th>Time</th><th>Type</th><th>Value</th>
        </tr>
        <%= for reading <- @readings do %>
          <tr>
            <td><%= reading.station %></td>
            <td><%= reading.date %></td>
            <td><%= reading.time %></td>
            <td><%= reading.type %></td>
            <td><%= reading.value %></td>
          </tr>
        <% end %>
      </table>

      <form phx-submit="submit_station">
        <select name="station_id">
          <%= for station <- @stations do %>
            <option label={station.name} value={station.id} selected={station.id == @station_id}/>
          <% end %>
        </select>
        <input type="submit" value="Submit"/>
      </form>

    """
  end

  def handle_event("submit_station", %{"station_id" => station_id}, socket) do
    case Reading.find_by_station_id(String.to_integer(station_id)) do
      nil -> {:noreply, socket}
      readings -> {:noreply, assign(socket, readings: readings)}
    end
  end

end
