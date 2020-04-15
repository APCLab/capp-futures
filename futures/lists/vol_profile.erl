fun(_Head, {Req}) ->
  {Query} = couch_util:get_value(<<"query">>, Req),
  TimeFrame = erlang:binary_to_integer(couch_util:get_value(<<"tf">>, Query, <<"30">>)),
  OutputType = erlang:binary_to_atom(
    couch_util:get_value(<<"type">>, Query, <<"volume">>), latin1),

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

  case OutputType of
    volume ->
      Send(io_lib:format("Date,Symbol,Contract,Time,Price,Volume~n", []));

    poc ->
      Send(io_lib:format("Date,Symbol,Contract,Time,Price~n", []))
  end,

  F = fun({Row}, _) ->
    Id = couch_util:get_value(<<"id">>, Row),
    K = couch_util:get_value(<<"key">>, Row),
    V = couch_util:get_value(<<"value">>, Row),

    lists:foldl(
      fun(S, {RawData, PrevVolAcc}) ->
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

        VolAcc = case OutputType of
          volume -> ok;
          _ -> dict:merge(fun(_K, V1, V2) -> V1 + V2 end, PrevVolAcc, VProf)
        end,

        OutputTime = SecondsToTime(S),
        OutputPrefix = K ++ [OutputTime],
        FloatOpt = [{decimals, 3}],

        case OutputType of
          volume ->
            SortedKey = lists:sort(dict:fetch_keys(VProf)),
            SortedVols = lists:map(fun(X) -> dict:fetch(X, VProf) end, SortedKey),

            lists:foreach(
              fun({P, V}) ->
                Send(
                  io_lib:format(
                    "~s,~s,~s,~s,~s,~b~n",
                    OutputPrefix ++ [float_to_list(P, FloatOpt), V]
                  )
                )
              end,
              lists:zip(SortedKey, SortedVols)
            );

          poc ->
            SortedKey = lists:sort(dict:fetch_keys(VolAcc)),
            SortedVols = lists:map(fun(X) -> dict:fetch(X, VolAcc) end, SortedKey),
            {VolSum, RVolCumSum} =
              lists:foldl(
                fun(X, {Sum, Acc}) -> {Sum + X, [Sum + X | Acc]} end, {0, []}, SortedVols),
            VolCumSum = lists:reverse(RVolCumSum),
            VolumePerc = lists:map(fun(X) -> X / VolSum end, SortedVols),

            Poc_ = lists:dropwhile(
              fun({_, V}) -> V < 0.5 end,
              lists:zip(SortedKey, lists:map(fun(X) -> X / VolSum end, VolCumSum))
            ),
            Poc = case Poc_ of
              [] -> 0.0;
              [{X, _} | _] -> X
            end,
            Send(
              io_lib:format(
                "~s,~s,~s,~s,~s~n",
                OutputPrefix ++ [float_to_list(Poc, FloatOpt)]
              )
            )
        end,

        {Tail, VolAcc}
      end,
      {
        lists:dropwhile(fun([Time|_]) -> ParseTime(Time) < StartTime end, V),
        dict:new()
      },
      TimeIndex
    ),

    {ok, Id}
  end,

  FoldRows(F, init),
  ""
end.
