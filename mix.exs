defmodule ObserverCli.MixProject do
  use Mix.Project

  def project do
    [
      app: :observer_cli,
      version: "1.8.1",
      language: :erlang,
      description: "observer in shell",
      deps: [
        {:recon, "~> 2.5.6"}
      ]
    ]
  end
end
