#!/usr/bin/env escript
%% -*- erlang -*-
-module(docsite).
-mode(compile).
-export([main/1]).
-define(DOC_PATH, "doc/").
-define(PAGE, ["Home", "Process", "Network", "System", "ETS", "Mnesia", "Application"]).
-define(INC_FILE, [?DOC_PATH ++ "favicon.ico", ?DOC_PATH ++ "index.html",
        ?DOC_PATH ++ "screen.css", ?DOC_PATH ++ "overview.edoc",
        ?DOC_PATH ++ "stylesheet.css", ?DOC_PATH ++ "docsite.erl"]).

main(_) ->
    Overview = ?DOC_PATH ++ "overview-summary.html",
    [Pre, Post] = base(),
    [write(?DOC_PATH,
        case File of Overview -> "index.html"; _ -> File end,
        [Pre, content(File), Post])
        || File <- [Overview]],
    Files = filelib:wildcard(?DOC_PATH ++ "*"),
    [file:delete(F) || F<- Files -- ?INC_FILE],
    halt(0).

content(File) ->
    filter(
        lists:reverse(
            lists:foldl(fun("<h1>" ++ _ = Line, init) -> {keep, [Line]};
                ("<hr>" ++ _, {keep, Lines}) -> Lines;
                (Line, {keep, Lines}) -> {keep, [Line | Lines]};
                (_, Lines) -> Lines
                        end,
                init,
                lines(File))
        )
    ).

filter(Content) ->
    %% We filter using regexes. This is terrible but so easy.
    lists:foldl(fun({RE, Replacement}, Text) ->
        re:replace(Text, RE, Replacement, [global])
                end,
        unicode:characters_to_binary(Content, latin1, utf8),
        [{"h5>", "h6>"}, {"<h5", "<h6"},
            {"h4>", "h5>"}, {"<h4", "<h5"},
            {"h3>", "h4>"}, {"<h3", "<h4"},
            {"h2>", "h3>"}, {"<h2", "<h3"},
            {"h1>", "h2>"}, {"<h1", "<h2"},
            {"tt>", "code>"}, {"<b>", "<strong>"},
            {"</b>", "</strong>"}]).


lines(File) ->
    {ok, IoDevice} = file:open(File, [read, raw]),
    get_lines(IoDevice).

get_lines(Device) ->
    case file:read_line(Device) of
        {ok, Data} -> [Data | get_lines(Device)];
        eof -> []
    end.

base() ->
    [["
<!DOCTYPE html>
<html lang=\"en\">
  <head>
    <meta http-equiv=\"Content-Type\" content=\"text/html;charset=UTF-8\">
    <link href='https://fonts.googleapis.com/css?family=Titillium+Web:400,300italic,600italic,700,600,700italic' rel='stylesheet' type='text/css'>
    <link href='https://fonts.googleapis.com/css?family=Ubuntu+Mono:400,700,700italic,400italic' rel='stylesheet' type='text/css'>
    <link rel=\"stylesheet\" href=\"screen.css\" media=\"screen, projection\" />
    <link rel=\"Shortcut Icon\" type=\"image/x-icon\" href=\"favicon.ico\">
    <title>Observer_cli</title>
  </head>
  <body>
  <script async defer src=\"https://buttons.github.io/buttons.js\"></script>
    <header>
        <h1>
        <a class=\"github-button\" href=\"https://github.com/zhongwencool/observer_cli\" data-size=\"larger\" data-show-count=\"true\" aria-label=\"Star zhongwencool/observer_cli on GitHub\">observer_cli</a>
        <img src=\"https://travis-ci.org/zhongwencool/observer_cli.svg\?branch=master\" </img>
        <img src=\"https://img.shields.io/hexpm/dt/observer_cli.svg\?style=flat-square)\" </img>
        </h1>
        </br>
        <nav>
          <ul>
",
        [["<li><a href=\"#", Name, "\">", Name, "</a>"] || Name <- ?PAGE],
        "
        </ul>
          </nav>
            </header>
            <article>
            "],
        "
            </article>
          </body>
        </html>
        "].

write(DropPath, OriginalPath, Content) ->
    NewPath = filename:join(DropPath, filename:basename(OriginalPath)),
    file:write_file(NewPath, Content).
