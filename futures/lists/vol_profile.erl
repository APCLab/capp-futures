fun(_Head, {Req}) ->
  {Query} = couch_util:get_value(<<"query">>, Req),
  TimeFrame = erlang:binary_to_integer(couch_util:get_value(<<"tf">>, Query, <<"30">>)),

  ParseTime = fun(X) ->
    calendar:time_to_seconds(
      erlang:list_to_tuple(
        lists:map(
          fun erlang:binary_to_integer/1,
          binary:split(X, <<":">>, [global])
        )
      )
    )
  end,
  SecondsToTime = fun(S) ->
    string:join(
      lists:map(
        fun(X) -> io_lib:format("~2..0B", [X]) end,
        tuple_to_list(calendar:seconds_to_time(S))
      ),
    ":")
  end,

  StartTime     = 31500, %% 08:45:00 in seconds
  EndTime       = 49500, %% 13:45:00 in seconds
  AfterHourTime = 53100, %% 14:45:00 in seconds
  TimeIndex = lists:seq(StartTime, EndTime - 1, TimeFrame),

  Send(io_lib:format("Date,Symbol,Contract,Time,Price,Volume~n", [])),

  F = fun({Row}, _) ->
    Id = couch_util:get_value(<<"id">>, Row),
    K = couch_util:get_value(<<"key">>, Row),
    V = couch_util:get_value(<<"value">>, Row),

    lists:foldl(
      fun(S, RawData) ->
        E = S + TimeFrame,

        TimeRange =
          fun([Time|_]) ->
            X = ParseTime(Time),
            (S =< X) and (X < E)
          end,

        {Ticks, Tail} = lists:splitwith(TimeRange, RawData),

        %% calculate volume profile
        VProf = lists:foldl(
          fun
            ([], _) -> skip;
            ([_, P, Vol | _], VProf) ->
              dict:update_counter(P, Vol, VProf)
          end,
          dict:new(),
          Ticks
        ),
        lists:foreach(
          fun(P) ->
            Send(
              io_lib:format(
                "~s,~s,~s,~s,~s,~b~n",
                K ++ [SecondsToTime(S), float_to_list(P, [{decimals,3}]), dict:fetch(P, VProf)]
              )
            )
          end,
          lists:sort(dict:fetch_keys(VProf))
        ),

        Tail
      end,
      lists:dropwhile(fun([Time|_]) -> ParseTime(Time) < StartTime end, V),
      TimeIndex
    ),

    {ok, Id}
  end,

  FoldRows(F, init),
  ""
end.
