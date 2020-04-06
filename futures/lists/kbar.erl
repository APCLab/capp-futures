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

  Send(io_lib:format("Date,Symbol,Contract,Time,Open,High,Low,Close,Volume~n", [])),

  F = fun({Row}, _) ->
    Id = couch_util:get_value(<<"id">>, Row),
    K = couch_util:get_value(<<"key">>, Row),
    V = couch_util:get_value(<<"value">>, Row),

    lists:foldl(
      fun(S, {RawData, PrevKbar}) ->
        E = S + TimeFrame,

        TimeRange =
          fun([Time|_]) ->
            X = ParseTime(Time),
            (S =< X) and (X < E)
          end,

        {Ticks, Tail} = lists:splitwith(TimeRange, RawData),

        %% calculate kbar
        Kbar = case Ticks of
          [] ->
            PrevC = proplists:get_value(c, PrevKbar, 0),
            [{o, PrevC}, {h, PrevC}, {l, PrevC}, {c, PrevC}, {v, 0}];

          _ -> lists:foldl(
            fun([_, P, Vol | _], Kbar) ->
              lists:map(
                fun({X, Y, F}) ->
                  {X, F([proplists:get_value(X, Kbar, Y), Y])}
                end,
                [
                  {o, P, fun erlang:hd/1},
                  {h, P, fun lists:max/1},
                  {l, P, fun lists:min/1},
                  {c, P, fun lists:last/1},
                  {v, Vol, fun lists:sum/1}
                ]
              )
            end,
            [],
            Ticks
          )
        end,  % Kbar

        KbarStr = string:join(
          lists:map(
            fun
              ({_K,V}) when is_float(V) ->
                float_to_list(V, [{decimals,3}]);

              ({_K,V}) when is_integer(V) ->
                integer_to_list(V)
            end,
            Kbar),
          ","
        ),

        Send(
          io_lib:format(
            "~s,~s,~s,~s,~s~n",
            K ++ [SecondsToTime(S) ,KbarStr]
          )
        ),

        {Tail, Kbar}
      end,
      {
        lists:dropwhile(fun([Time|_]) -> ParseTime(Time) < StartTime end, V),
        []
      },
      TimeIndex
    ),

    {ok, Id}
  end,

  FoldRows(F, init),
  ""
end.
