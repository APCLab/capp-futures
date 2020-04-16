fun({Doc}) ->
  Sym = proplists:get_value(<<"symbol">>, Doc, null),

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
    list_to_binary(
      string:join(
        lists:map(
          fun(X) -> io_lib:format("~2..0B", [X]) end,
          tuple_to_list(calendar:seconds_to_time(S))
        ),
      ":"
      )
    )
  end,
  ToSortedList = fun(D) ->
    K = lists:sort(dict:fetch_keys(D)),
    V = lists:map(fun(X) -> dict:fetch(X, D) end, K),
    lists:zipwith(fun(X, Y) -> [X, Y] end, K, V)
  end,

  StartTime     = 31500, %% 08:45:00 in seconds
  EndTime       = 49500, %% 13:45:00 in seconds
  AfterHourTime = 53100, %% 14:45:00 in seconds
  TimeFrame     = 1,

  GenVolProfile = fun(Row) ->
    TimeIndex = lists:seq(StartTime, EndTime - 1, TimeFrame),

    {_, VolProfile} = lists:foldl(
      fun(S, {RawData, VolAcc}) ->
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

        {Tail, [[SecondsToTime(S), ToSortedList(VProf)] | VolAcc]}
      end,
      {
        lists:dropwhile(fun([Time|_]) -> ParseTime(Time) < StartTime end, Row),
        []
      },
      TimeIndex
    ),

    lists:reverse(VolProfile)
  end,

  case Sym of
    <<"TX">> ->
      K = lists:map(
        fun(K) -> couch_util:get_value(K, Doc) end,
        [<<"date">>, <<"symbol">>, <<"contract">>]
      ),
      Rs = couch_util:get_value(<<"records">>, Doc, []),
      Emit(K, GenVolProfile(Rs));
    _ -> skip
  end
end.

