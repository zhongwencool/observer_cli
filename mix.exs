defmodule ObserverCli.MixProject do
  use Mix.Project

  def project do
    [
      app: :observer_cli,
      version: "0.4.4",
      elixir: ">= 1.6.6 and < 1.9.0",
      start_permanent: Mix.env == :prod,
      deps: []
    ]
  end

  def application do
    []
  end
end
