fun(_Head, {Req}) ->
  {Query} = couch_util:get_value(<<"query">>, Req),
  ShowHeader = erlang:binary_to_atom(
    couch_util:get_value(<<"header">>, Query, <<"true">>), utf8),

  case ShowHeader of
    true -> Send(io_lib:format("Date,Contract~n", []));
    _ -> ok
  end,

  F = fun({Row}, _) ->
    null = couch_util:get_value(<<"key">>, Row),
    {L} = couch_util:get_value(<<"value">>, Row),
    D = dict:from_list(L),
    lists:map(
      fun(Date) ->
        Send(io_lib:format("~s,~s~n", [Date, dict:fetch(Date, D)]))
      end,
      lists:sort(dict:fetch_keys(D))
    ),

    {ok, null}
  end,

  FoldRows(F, init),
  ""
end.
