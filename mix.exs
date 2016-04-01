defmodule ObserverCli.Mixfile do
use Mix.Project

    def project do
      [
       app: :observer_cli,
       version: "1.0.4",
       description: "Visualize Erlang Nodes On The Command Line",
       deps: deps,
       package: package,
       language: :erlang
      ]
    end

    def application do
      [
        applications: [:kernel, :stdlib, :recon]
        ]
    end 
    def deps do
        [
            {:recon, "~> 2.2.1"},
        ]
    end

    defp package do
        [
            files: [
                "src",
                "include",
                "mix.exs",
                "mix.lock",
                "rebar.config",
                "rebar.lock",
                "README.md",
                "NEWS.md",
                "LICENSE"
            ],
            maintainers: ["zhongwencool"],
            licenses: ["MIT"],
            links: %{"Github" => "https://github.com/zhongwencool/observer_cli"}
        ]
    end
end

