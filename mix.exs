defmodule ObserverCli.MixProject do
  use Mix.Project

  def project do
    [
      app: :observer_cli,
      version: "1.8.4",
      language: :erlang,
      description: "observer in shell",
      deps: [
        {:recon, "~> 2.5.6"}
      ],
      env: [
        scheduler_usage: :disable,
        plugins: [
          # module       - Specific module implements plugin behavior. It's mandatory.
          # title        - Menu title. It's mandatory.
          # shortcut     - Switch plugin by shortcut. It's mandatory.
          # interval     - Refresh interval ms. It's options. default is 1500ms.
          # sort_column  - Sort the sheet by this index. It's options default is 2.

          # %{module: ObserverCli.Plug1, title: "Example-1", interval: 1500, shortcut: "S", sort_column: 3},
          # %{module: ObserverCli.Plug2, title: "Example-2", interval: 1600, shortcut: "D", sort_column: 2}
        ]
      ]
    ]
  end
end
