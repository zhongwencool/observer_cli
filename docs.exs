[
  extras: [
    {"README.md", title: "Home"},
    {"./docs/tutorials/first-diagnosis.md", title: "Run your first diagnosis"},
    {"./docs/tutorials/first-tui-session.md", title: "Open your first TUI session"},
    {"./docs/how-to/install-and-build.md", title: "Install and build"},
    {"./docs/how-to/connect-to-a-node.md", title: "Connect to a node"},
    {"./docs/how-to/investigate-memory-and-processes.md",
     title: "Investigate memory and processes"},
    {"./docs/how-to/inspect-network-and-distribution.md",
     title: "Inspect network and distribution"},
    {"./docs/how-to/automate-diagnostics.md", title: "Automate diagnostics"},
    {"./docs/how-to/trace-calls.md", title: "Trace calls safely"},
    {"./docs/how-to/extend-the-tui.md", title: "Extend the TUI"},
    {"./docs/how-to/contribute.md", title: "Contribute a change"},
    {"./docs/reference/cli.md", title: "Command-line reference"},
    {"./docs/reference/tui.md", title: "TUI reference"},
    {"./docs/reference/configuration.md", title: "Configuration reference"},
    {"./docs/reference/output-contract.md", title: "Output and storage contract"},
    {"./docs/explanation/execution-model.md", title: "Execution model"},
    {"./docs/explanation/diagnostic-model.md", title: "Diagnostic model"},
    {"./docs/explanation/safety-and-observer-effect.md",
     title: "Safety and observer effect"},
    {"./docs/CHANGELOG.md", title: "Changelog"},
    {"LICENSE", title: "License"}
  ],
  logo: "./docs/favicon.png",
  authors: [
    "Zhongwen Deng <zhongwencool@gmail.com>"
  ],
  description:
    "Production-ready, LLM-friendly command diagnostics for live Erlang and Elixir systems, with bounded structured output and an interactive TUI.",
  proglang: :erlang,
  main: "readme",
  source_url: "https://github.com/zhongwencool/observer_cli",
  homepage_url: "https://github.com/zhongwencool/observer_cli",
  with_mermaid: true,
  before_closing_head_tag: fn
    :html ->
      """
      <link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/@highlightjs/cdn-assets@11.11.1/styles/github-dark-dimmed.min.css">
      <style>pre code.hljs { color: #adbac7; background: #22272e; }</style>
      """

    _ ->
      ""
  end,
  before_closing_body_tag: fn
    :html ->
      """
      <script src="https://cdn.jsdelivr.net/npm/mermaid/dist/mermaid.min.js"></script>
      <script>mermaid.initialize({startOnLoad: true})</script>
      <script src="https://cdn.jsdelivr.net/npm/@highlightjs/cdn-assets@11.11.1/highlight.min.js"></script>
      <script>
        if (window.hljs) {
          document.querySelectorAll("pre code.makeup.sh").forEach((block) => {
            block.textContent = block.textContent;
            window.hljs.highlightElement(block);
          });
        }
      </script>
      """

    _ ->
      ""
  end,
  api_reference: false,
  warnings_as_errors: true,
  groups_for_extras: [
    {"Tutorials", ~r"/tutorials/"},
    {"How-to guides", ~r"/how-to/"},
    {"Reference", ~r"/reference/"},
    {"Explanation", ~r"/explanation/"},
    {"Project", ~r"(CHANGELOG|LICENSE)"}
  ],
  filter_modules: fn _, _ -> false end,
  skip_code_autolink_to:
    &String.starts_with?(&1, ["observer_cli:", "observer_cli_snapshot:"])
]
