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

  DropZero = fun(L) ->
    X = lists:dropwhile(fun(X) -> X == 0 end, L),
    case X of
      [] -> [0.0];
      _  -> X
    end
  end,

  F = fun
    RowHandler({Row}, init) ->
      {Doc}= couch_util:get_value(<<"doc">>, Row),
      Sch = couch_util:get_value(<<"schema">>, Doc),

      Header = string:join(lists:map(fun([X, _]) -> binary_to_list(X) end, Sch), ","),
      Send("Date,Symbol,Contract,"),
      Send(io_lib:format("~s~n", [Header])),

      RowHandler({Row}, inited);

    RowHandler({Row}, _) ->
      Id = couch_util:get_value(<<"id">>, Row),
      [Sym, Date, Contract] = couch_util:get_value(<<"key">>, Row),
      K = [Date, Sym, Contract],
      {Doc}= couch_util:get_value(<<"doc">>, Row),
      V = couch_util:get_value(<<"records">>, Doc),

      % time o h l c v
      % 1    2 3 4 5 6
      [Time | OHLCV] = lists:map(
        fun({Op, X}) ->
            lists:map(Op, Part(X))
        end,
        lists:zip([
          fun erlang:hd/1,
          fun(X) -> hd(DropZero(X)) end,
          fun lists:max/1,
          fun(X) -> hd(DropZero(X)) end,
          fun lists:last/1,
          fun lists:sum/1
          ], V)
      ),

      lists:foldl(
        fun(T, OHLCV) ->
          R = lists:map(fun erlang:hd/1, OHLCV),
          Send(io_lib:format("~s,~s,~s,~s,~.3f,~.3f,~.3f,~.3f,~b~n", K ++ [T] ++ R)),

          lists:map(fun erlang:tl/1, OHLCV)
        end,
        OHLCV,
        Time
      ),

      {ok, Id}  % do not change this
  end,

  case ShowHeader of
    true -> FoldRows(F, init);
    _    -> FoldRows(F, inited)
  end,
  ""
end.
