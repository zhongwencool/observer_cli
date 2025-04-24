# Changelog

- 1.8.3
  - Fix mnesia crash by handling unknown storage types.
  - Fix OTP 27 warning.
  - Support ex_doc to generate documents.
  
- 1.8.2
  - Fix unit of `fullsweep_after`, The value of `fullsweep_after` is a number, not bytes.

- 1.8.1
  - Show node name in system pane.
 
- 1.8.0
  - Support `<Pid` to jump to specific pid.
  - Show process's label if it's set with [proc_lib:set_label(Label)](https://www.erlang.org/doc/apps/stdlib/proc_lib.html#set_label/1)
  - Show The number of bytes in the output distribution queue on System View. This queue sits between the Erlang code and the port driver, using undocumented function`erlang:dist_get_stat/1`.
  - Fix Doc View not show when otp version = 27
  
- 1.7.5
  - Fix crash when mnesia table with external copies.
    Which `mnesia:table_info(TabName, storage_type)` returns tuple `{ext, _, _}`
  - Correct the order of the application information; the items Memory and Reductions have been switched.

- 1.7.4
  - fix crash when ets:info/1 return undefined.
- 1.7.3
  - fix system pane exception by `ps` command.
- 1.7.2
  - Fix error when inspecting process that monitors via {RegName, Node}.
- 1.7.1
  - application view show starting/loading/startPfalse/loaded/started application.
  - fixed badarg when staring by rpc and stop by `ctrl+c`.
  - fixed mix.exe version error
- 1.7.0
  - application view support reductions/memory/process_count sort
  - plugin support `{byte, 1024}` to `10.0000 KB`
  - plugin support `{percent, 0.1234` to `12.34%`
  - plugin support dig deep process view.
- 1.6.2
  - fixed crash when ps command not found on windows.
- 1.6.1
  - remove precise opt version
- 1.6.0
  - hidden schedule usage default
  - format by erlformat
  - add `ps -o pcpu,pmem,rss,vsz` information
  - remove recon_alloc:memory/1 from `HOME`(too much cpu usage)
- 1.5.4
  - Bump Recon to 2.5.1 for otp23 alloc compat.
- 1.5.2
  - Use erlang:system_info(otp_release) when can't find `OTP_VERSION` file for the full version.
- 1.5.1
  - Hide mnesia tab when it's not started
  - Show specific erl version such as '22.0.5'
- 1.5.0
  - Bump Recon to 2.5.0
- 1.4.5
  - Include a minimal mix.exs build file
  - Make sure EXIT message has been clear
- 1.4.4
  - Make sure connection errors can be handled
- 1.4.3
  - Bump Recon to 2.4.0
- 1.4.2
  - Hidden schedule process bar when core > 100.
  - Allow to compile escript w/ inet6 based distribution.
  - Rewrite plugin callback, rename kv_label/0 to attributes/1.
- 1.4.1
  - Fixed ets view memory usage wrong.
  - mnesia view memory usage According to bytes.
- 1.4.0
  - Support write your own plugin.
- 1.3.4
  - View(ets mnesia) support page down/up; support sort by memory or size.
  - Fixed pause crash.
  - Make refresh interval configurable.
- 1.3.3
  - fixed io:format(Format,Args) Format not support iolist OTP R21
- 1.3.2
  - Make sure all observer_cli process exit when quit.
  - Upgrade recon to 2.3.6
- 1.3.1
  - Add atom limit/count in home.
  - Escript support short name and long name.
  - Fixed store process not exit.
  - [Upgrade recon to 2.3.5](https://github.com/ferd/recon/commit/e0c3614334589e375f8b1492f404e4b764fe35e7)
- 1.3.0
  - Rewrite Network/Process view.
  - Support PageDown/PageUp for top n list.
  - Escript auto load observer_cli when it's not load on target node.
- 1.2.2
  - fix schedule number >= 32 display wrong.
  - improve memory(byte/kilobyte/megabyte/gigabyte) unit.
- 1.2.1
  - fixed autosize not work.
  - try best to make color adjust all platform.
- 1.2.0

  - add application GUI.
  - Rearrange GUI and optimize render.
  - Always automatically adapt to the window size.

- 1.1.0

  - Support escript, `observer_cli <TARGETNODE> <COOKIE>`

- 1.0.9
  - Upgrade rebar3 to 3.3.3 for publish hex repo.
