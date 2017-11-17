-module(response).

-export([format_response/5, create/1]).

create(Value) ->
  %TODO get from logger
  Leader = self(), % kinda works :D
  Nodes = [],
  TraitorNodes = [],
  Messages = messenger:get_log(),
  format_response(Value, Leader, Nodes, TraitorNodes, Messages).

format_response(Value, Leader, Nodes, TraitorNodes, Messages) ->
  jsx:encode([
    {<<"value">>, Value},
    {<<"leader">>, format_pid(Leader)},
    {<<"acceptors">>, [format_pid(N) || N <- Nodes]},
    {<<"traitors">>, [format_pid(N) || N <- TraitorNodes]},
    {<<"messages">>, [format_message(M) || M <- Messages]}
  ]).

format_pid(Pid) -> list_to_binary(pid_to_list(Pid)).

format_message(Message) ->
  {From, To, Msg} = Message,
  [
    {<<"from">>, format_pid(From)},
    {<<"to">>, format_pid(To)},
    {<<"msg">>, list_to_binary(lists:flatten(io_lib:format("~p", [Msg])))}
  ].