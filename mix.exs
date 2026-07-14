defmodule ObserverCli.MixProject do
  use Mix.Project

  def project do
    [
      app: :observer_cli,
      version: "2.0.0",
      language: :erlang,
      description: "Production-ready BEAM diagnostics for operators, automation, and AI agents.",
      escript: [
        main_module: :observer_cli_escriptize,
        app: nil,
        emu_args: "-hidden +sbtu +A0 -elixir ansi_enabled true",
        include_priv_for: [:observer_cli]
      ],
      deps: [
        {:recon, "2.5.6"}
      ]
    ]
  end

  def application do
    [
      extra_applications: [:crypto],
      env: [
        # {:formatter, #{
        # :application => atom(), - Formatter application. observer_cli loads all it's modules to remote node.
        # :mod => module() - observer_cli_formatter implementation.
        # }},
        # scheduler_usage: :enable, # default value is 'disable', uncomment to enable by default
        plugins: [
          # module       - Specific module implements plugin behavior. It's mandatory.
          # title        - Menu title. It's mandatory.
          # shortcut     - Switch plugin by shortcut. It's mandatory.
          # interval     - Refresh interval ms. It's options. default is 1500ms.
          # sort         - Sort by column ID. It's optional; defaults to sheet_header/0's default_sort.

          # %{module: ObserverCli.Plug1, title: "Example-1", interval: 1500, shortcut: "S", sort: :value},
          # %{module: ObserverCli.Plug2, title: "Example-2", interval: 1600, shortcut: "D", sort: :name}
        ]
      ]
    ]
  end
end
