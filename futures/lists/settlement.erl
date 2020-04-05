fun(_Head, {Req}) ->
  Send(io_lib:format("Date,Contract~n", [])),

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
