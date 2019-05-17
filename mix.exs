defmodule ObserverCli.MixProject do
  use Mix.Project

  def project do
    [
      app: :observer_cli,
      version: "1.4.5",
      language: :erlang,
      start_permanent: Mix.env == :prod,
      deps: [
        {:recon, "~> 2.5.0"},
      ]
    ]
  end

  def application do
    []
  end
end
