[
  extras: [
    {"README.md", title: "Home"},
    "./docs/plugin.md",
    "./docs/CHANGELOG.md",
    "LICENSE"
  ],
  logo: "./docs/favicon.png",
  authors: [
    "Zhongwen Deng <zhongwencool@gmail.com>"
  ],
  main: "readme",
  source_url: "https://github.com/zhongwencool/observer_cli",
  homepage_url: "https://github.com/zhongwencool/observer_cli",
  with_mermaid: true,
  before_closing_body_tag: fn
    :html ->
      """
      <script src="https://cdn.jsdelivr.net/npm/mermaid/dist/mermaid.min.js"></script>
      <script>mermaid.initialize({startOnLoad: true})</script>
      """

    _ ->
      ""
  end,
  api_reference: false,
  warnings_as_errors: true,
  groups_for_docs: [],
  filter_modules: fn _, _ -> false end
]
