defmodule Pollutiondb.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    children = [
      Pollutiondb.Repo,
      PollutiondbWeb.Telemetry,
      {DNSCluster, query: Application.get_env(:pollutiondb, :dns_cluster_query) || :ignore},
      {Phoenix.PubSub, name: Pollutiondb.PubSub},
      # Start the Finch HTTP client for sending emails
      {Finch, name: Pollutiondb.Finch},
      # Start a worker by calling: Pollutiondb.Worker.start_link(arg)
      # {Pollutiondb.Worker, arg},
      # Start to serve requests, typically the last entry
      PollutiondbWeb.Endpoint
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: Pollutiondb.Supervisor]
    Supervisor.start_link(children, opts)
  end

  # Tell Phoenix to update the endpoint configuration
  # whenever the application is updated.
  @impl true
  def config_change(changed, _new, removed) do
    PollutiondbWeb.Endpoint.config_change(changed, removed)
    :ok
  end
end
