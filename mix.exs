defmodule ObserverCli.MixProject do
  use Mix.Project

  def project do
    [
      app: :observer_cli,
      version: "1.7.2",
      language: :erlang,
      description: "observer in shell",
      deps: [
        {:recon, "~> 2.5.1"},
      ]
    ]
  end

end
