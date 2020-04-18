fun(_Head, {Req}) ->
  {Query} = couch_util:get_value(<<"query">>, Req),
  TimeFrame = erlang:binary_to_integer(couch_util:get_value(<<"tf">>, Query, <<"30">>)),

  SecondsToTime = fun(S) ->
    string:join(
      lists:map(
        fun(X) -> io_lib:format("~2..0B", [X]) end,
        tuple_to_list(calendar:seconds_to_time(S))
      ),
    ":")
  end,

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

  StartTime     = 31500, %% 08:45:00 in seconds
  EndTime       = 49500, %% 13:45:00 in seconds
  AfterHourTime = 53100, %% 14:45:00 in seconds
  TimeIndex = lists:seq(StartTime, EndTime - 1, TimeFrame),

  Send(io_lib:format("Date,Symbol,Contract,Time,Price,Volume~n", [])),

  F = fun({Row}, _) ->
    Id = couch_util:get_value(<<"id">>, Row),
    K = couch_util:get_value(<<"key">>, Row),
    V = couch_util:get_value(<<"value">>, Row),

    lists:foreach(
      fun(VProfs) ->
        Time = hd(hd(VProfs)),

        MergedVProf = lists:foldl(  %% VProf under given timeframe
          fun([_Time, VProf], M) ->
            maps:fold(
              fun(P, Vol, Acc) -> Acc#{P => maps:get(P, Acc, 0) + Vol} end,
              M, VProf
            )
          end,
          #{},
          VProfs
        ),

        OutputPrefix = K ++ [SecondsToTime(Time)],
        lists:foreach(
          fun({P, V}) ->
            Send(io_lib:format("~s,~s,~s,~s,~s,~b~n", OutputPrefix ++ [P, V]))
          end,
          lists:keysort(1, maps:to_list(MergedVProf))
        ),

        ok
      end,
      Part(V)
    ),

    {ok, Id}
  end,

  FoldRows(F, init),
  ""
end.
