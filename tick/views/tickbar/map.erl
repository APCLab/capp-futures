fun({Doc}) ->
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
  ToSortedList = fun(M) ->
    lists:map(fun erlang:tuple_to_list/1, lists:keysort(1, maps:to_list(M)))
  end,

  StartTime     = 31500, %% 08:45:00 in seconds
  EndTime       = 49500, %% 13:45:00 in seconds
  AfterHourTime = 53100, %% 14:45:00 in seconds
  TimeFrame     = 1,

  GenTickBar = fun(Row) ->
    TimeIndex = lists:seq(StartTime, EndTime - 1, TimeFrame),

    {_, TickBars} = lists:foldl(
      fun(S, {RawData, BarAcc}) ->
        E = S + TimeFrame,

        TimeRange =
          fun([Time|_]) ->
            X = ParseTime(Time),
            (S =< X) and (X < E)
          end,

        {Ticks, Tail} = lists:splitwith(TimeRange, RawData),

        %% calculate tick bar
        Bars = lists:foldl(
          fun([_, P| _], M) ->
            P_ = erlang:float_to_binary(P, [{decimals, 2}]),
            M#{P_ => maps:get(P_, M, 0) + 1}
          end,
          #{},
          Ticks
        ),

        {Tail, [[S, Bars] | BarAcc]}
      end,
      {
        lists:dropwhile(fun([Time|_]) -> ParseTime(Time) < StartTime end, Row),
        []
      },
      TimeIndex
    ),

    lists:reverse(TickBars)
  end,

  K = lists:map(
    fun(K) -> couch_util:get_value(K, Doc) end,
    [<<"symbol">>, <<"date">>, <<"contract">>]
  ),
  Rs = couch_util:get_value(<<"records">>, Doc, []),
  Emit(K, GenTickBar(Rs)),

  ok
end.

