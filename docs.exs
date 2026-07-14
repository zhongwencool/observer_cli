[
  extras: [
    {"README.md", title: "Home"},
    {"./docs/reference/cli.md", title: "CLI"},
    {"./docs/reference/tui.md", title: "Reference"},
    {"./docs/reference/tui-plugins.md", title: "TUI plugins"},
    {"./docs/explanation/core-concepts.md", title: "Core concepts"},
    {"./docs/CHANGELOG.md", title: "Changelog"},
    {"LICENSE", title: "License"}
  ],
  logo: "./docs/favicon.png",
  assets: %{"./docs/assets" => "assets"},
  authors: [
    "Zhongwen Deng <zhongwencool@gmail.com>"
  ],
  description:
    "Production-ready, automation- and agent-friendly command diagnostics for live Erlang and Elixir systems, with bounded structured output and an interactive TUI.",
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
      <script src="https://cdn.jsdelivr.net/npm/mermaid@11.15.0/dist/mermaid.min.js"></script>
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
    {"CLI", ~r"/reference/cli\.md$"},
    {"TUI", ~r"/reference/tui(?:-plugins)?\.md$"},
    {"Core concepts", ~r"/explanation/core-concepts\.md$"},
    {"Project", ~r"(?:/CHANGELOG\.md|^LICENSE)$"}
  ],
  filter_modules: fn _, _ -> false end,
  skip_code_autolink_to:
    &String.starts_with?(&1, [
      "observer_cli:",
      "observer_cli_snapshot:",
      "proc_lib:get_label/1",
      "proc_lib:set_label/1"
    ])
]
