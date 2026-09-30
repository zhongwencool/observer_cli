%% Closed, non-executing recommendations. Never derives argv from target text.
-module(observer_cli_actions).
-export([from_findings/1, valid/2]).

-spec from_findings([map()]) -> [map()].
from_findings(Findings) ->
    Ids = lists:usort(
        lists:append([
            rule_actions(maps:get(id, Finding, maps:get(<<"id">>, Finding, null)))
         || Finding <- Findings
        ])
    ),
    [action(Id) || Id <- Ids].

-spec valid(term(), [map()]) -> boolean().
valid(Actions, Findings) ->
    Actions =:= from_findings(Findings).

rule_actions(<<"vm.process_limit_pressure">>) ->
    [process_counts, process_memory, process_queues];
rule_actions(<<"vm.port_limit_pressure">>) ->
    [network_io, port_queues];
rule_actions(<<"vm.ets_limit_pressure">>) ->
    [ets_tables];
rule_actions(<<"vm.scheduler_pressure">>) ->
    [process_reductions];
rule_actions(_) ->
    [].

action(Id) ->
    {Purpose, Argv} = definition(Id),
    #{
        <<"id">> => atom_to_binary(Id),
        <<"purpose">> => Purpose,
        <<"argv">> => Argv,
        <<"target_binding">> => <<"same_explicit_target">>,
        <<"risk_level">> => <<"bounded_observation">>,
        <<"requires_confirmation">> => false,
        <<"requires_identifiers">> => false
    }.

definition(process_counts) ->
    {<<"Compare process counts by application before changing the VM limit.">>, [
        <<"applications">>, <<"--sort">>, <<"process_count">>, <<"--limit">>, <<"20">>
    ]};
definition(process_memory) ->
    {<<"Inspect current process memory; this is not proof of a memory leak.">>, [
        <<"processes">>, <<"--sort">>, <<"memory">>, <<"--limit">>, <<"20">>
    ]};
definition(process_queues) ->
    {<<"Inspect current mailbox lengths before attributing process pressure.">>, [
        <<"processes">>, <<"--sort">>, <<"message_queue_len">>, <<"--limit">>, <<"20">>
    ]};
definition(network_io) ->
    {<<"Inspect legacy inet port activity; this is not all host network traffic.">>, [
        <<"network">>, <<"--sort">>, <<"oct">>, <<"--limit">>, <<"20">>
    ]};
definition(port_queues) ->
    {<<"Inspect non-inet Erlang port queues and ownership.">>, [
        <<"ports">>, <<"--sort">>, <<"queue_size">>, <<"--limit">>, <<"20">>
    ]};
definition(ets_tables) ->
    {<<"Inspect table sizes without reading table contents.">>, [
        <<"ets">>, <<"--sort">>, <<"size">>, <<"--limit">>, <<"20">>
    ]};
definition(process_reductions) ->
    {<<"Compare stable-process reductions over a bounded window; reductions are not CPU time.">>, [
        <<"processes">>,
        <<"--sort">>,
        <<"reductions">>,
        <<"--duration">>,
        <<"1500ms">>,
        <<"--limit">>,
        <<"20">>
    ]}.
