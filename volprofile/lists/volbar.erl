fun(_Head, {Req}) ->
  {Query} = couch_util:get_value(<<"query">>, Req),
  TimeFrame = erlang:binary_to_integer(couch_util:get_value(<<"tf">>, Query, <<"30">>)),
  ShowHeader = erlang:binary_to_atom(
    couch_util:get_value(<<"header">>, Query, <<"true">>), utf8),

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
  Part = fun(L) -> Part_(L, []) end,

  F = fun
    RowHandler({Row}, init) ->
      {Doc} = couch_util:get_value(<<"doc">>, Row),
      Send(io_lib:format("Date,Symbol,Contract,Time,Price,Vol~n", [])),
      RowHandler({Row}, inited);

    RowHandler({Row}, _) ->
      Id = couch_util:get_value(<<"id">>, Row),
      [Sym, Date, Contract] = couch_util:get_value(<<"key">>, Row),
      K = [Date, Sym, Contract],
      {Doc} = couch_util:get_value(<<"doc">>, Row),
      [AllTimes, AllBars] = couch_util:get_value(<<"records">>, Doc),

      MergedBars = lists:map(
        fun
          (Bars) ->
            lists:foldl(
              fun ({Bar}, Acc) ->
                  lists:foldl(
                    fun({Price, Vol}, D) ->
                      orddict:update_counter(Price, Vol, D)
                    end,
                    Acc,
                    Bar
                  );
                ([], Acc) ->
                  Acc
              end,
              orddict:new(),
              Bars
            )
        end,
        Part(AllBars)
      ),
            
      lists:map(
        fun
          ({[Time | _], []}) ->
            Send(io_lib:format("~s,~s,~s,~s,NaN,-1~n", K ++ [Time]));
          ({[Time | _], Bars}) ->
            orddict:map(
              fun (Price, Vol) ->
                Send(io_lib:format("~s,~s,~s,~s,~s,~b~n", K ++ [Time, Price, Vol]))
              end,
              Bars
            )
        end,
        lists:zip(Part(AllTimes), MergedBars)
      ),

      {ok, Id}  % do not change this
  end,

  case ShowHeader of
    true -> FoldRows(F, init);
    _    -> FoldRows(F, inited)
  end,
  ""
end.
