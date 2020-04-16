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

    {_, _, VolProfile} = lists:foldl(
      fun(S, {RawData, PrevVolProf, VolAcc}) ->
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
            ([_, P, Vol | _], D) ->
              dict:update_counter(P, Vol, D)
          end,
          PrevVolProf,
          Ticks
        ),

        SortedKey = lists:sort(dict:fetch_keys(VProf)),
        SortedVols = lists:map(fun(X) -> dict:fetch(X, VProf) end, SortedKey),
        {VolSum, RVolCumSum} =
          lists:foldl(
            fun(X, {Sum, Acc}) -> {Sum + X, [Sum + X | Acc]} end, {0, []}, SortedVols),
        VolCumSum = lists:reverse(RVolCumSum),

        % Point of control (POC)
        Poc_ = lists:dropwhile(
          fun({_, V}) -> V < 0.5 end,
          lists:zip(SortedKey, lists:map(fun(X) -> X / VolSum end, VolCumSum))
        ),
        Poc = case Poc_ of
          [] -> 0.0;
          [{X, _} | _] -> X
        end,

        % Value Area (VA)
        % Assume VA is in POC +- 35%
        VARange = 0.34,
        VA = case Poc of
          0.0 -> [0.0, 0.0];
          _ ->
            VolumePerc = lists:map(fun(X) -> X / VolSum end, SortedVols),
            {H, T} = lists:splitwith(fun({K, V}) -> K < Poc end, lists:zip(SortedKey, VolumePerc)),

            FindVA_ = fun
              F([], Sum) -> 0.0;

               F([{K, V}], _Sum) -> K;

              F([{K, V} | T], Sum) ->
                NewSum = Sum + V,
                case NewSum of
                  Y when Y >= VARange -> K;
                  _ -> F(T, NewSum)
                end
            end,

            [FindVA_(lists:reverse(H), 0.0), FindVA_(T, 0.0)]
        end,


        {Tail, VProf, [[SecondsToTime(S), Poc | VA] | VolAcc]}
      end,
      {
        lists:dropwhile(fun([Time|_]) -> ParseTime(Time) < StartTime end, Row),
        dict:new(),
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

