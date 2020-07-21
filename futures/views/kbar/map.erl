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

  StartTime     = 31500, %% 08:45:00 in seconds
  EndTime       = 49500, %% 13:45:00 in seconds
  AfterHourTime = 53100, %% 14:45:00 in seconds
  TimeFrame     = 1,

  GenKbar = fun(Row) ->
    TimeIndex = lists:seq(StartTime, EndTime - 1, TimeFrame),

    {_, _, Kbars} = lists:foldl(
      fun(S, {RawData, PrevKbar, Kbars}) ->
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
            PrevC = proplists:get_value(c, PrevKbar, 0.0),
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
            [{v, 0}],
            Ticks
          )
        end,  % Kbar

        OutputK = [SecondsToTime(S) | lists:map(fun({_K,V}) -> V end, Kbar)],

        {Tail, Kbar, [OutputK | Kbars]}
      end,
      {
        lists:dropwhile(fun([Time|_]) -> ParseTime(Time) < StartTime end, Row),
        [],
        []
      },
      TimeIndex
    ),

    lists:reverse(Kbars)
  end,

  K = lists:map(
    fun(K) -> couch_util:get_value(K, Doc) end,
    [<<"symbol">>, <<"date">>, <<"contract">>]
  ),
  Rs = couch_util:get_value(<<"records">>, Doc, []),

  Emit(K, GenKbar(Rs)),

  %% case length(Rs) of
  %%   X when X >= 5000 -> Emit(K, GenKbar(Rs));
  %%   _ -> skip
  %% end;
  ok
end.
