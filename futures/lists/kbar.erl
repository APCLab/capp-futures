fun(_Head, {Req}) ->
  {Query} = couch_util:get_value(<<"query">>, Req),
  TimeFrame = erlang:binary_to_integer(couch_util:get_value(<<"tf">>, Query, <<"30">>)),

  Part_ = fun
    Part([], Acc) ->
      lists:reverse(Acc);

    Part(L, Acc) ->
      Sub = lists:sublist(L, TimeFrame),
      Tail = case Sub of
        L -> [];  % no tail
        _ -> lists:nthtail(TimeFrame, L)
      end,
      Part(Tail, [Sub | Acc])
  end,
  Part = fun (L) -> Part_(L, []) end,

  Send(io_lib:format("Date,Symbol,Contract,Time,Open,High,Low,Close,Volume~n", [])),

  F = fun({Row}, _) ->
    Id = couch_util:get_value(<<"id">>, Row),
    K = couch_util:get_value(<<"key">>, Row),
    V = couch_util:get_value(<<"value">>, Row),

    lists:foreach(
      fun(Kbars) ->
        Kbar = [
          hd(hd(Kbars)),  % time
          lists:nth(2, hd(Kbars)),  % o
          lists:max(lists:map(fun(L) -> lists:nth(2, L) end, Kbars)),
          lists:min(lists:map(fun(L) -> lists:nth(3, L) end, Kbars)),
          lists:nth(4, lists:last(Kbars)),
          lists:sum(lists:map(fun lists:last/1, Kbars))
        ],
        Send(io_lib:format("~s,~s,~s,~s,~.3f,~.3f,~.3f,~.3f,~b~n", K ++ Kbar))
      end,
      Part(V)
    ),

    {ok, Id}  % do not change this
  end,

  FoldRows(F, init),
  ""
end.
