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

  Send(io_lib:format("Date,Symbol,Contract,Time,PoC,VA1,VA2~n", [])),

  F = fun({Row}, _) ->
    Id = couch_util:get_value(<<"id">>, Row),
    K = couch_util:get_value(<<"key">>, Row),
    V = couch_util:get_value(<<"value">>, Row),

    lists:foreach(
      fun(Mps) ->
        Mp = [hd(hd(Mps)) | tl(lists:last(Mps))],
        Send(io_lib:format("~s,~s,~s,~s,~.3f,~.3f,~.3f~n", K ++ Mp))
      end,
      Part(V)
    ),

    {ok, Id}  % do not change this
  end,

  FoldRows(F, init),
  ""
end.
