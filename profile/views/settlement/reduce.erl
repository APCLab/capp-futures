%% single key is [ViewKey, DocId],
%% e.g. [[<<"TX">>,<<"2020-03-19">>,<<"202004">>], <<"20200319-TX-202004">>]

fun
  (Keys, Values, false) ->
    D = lists:foldl(
      fun([K, _], D) ->
          [Symbol, Date, Contract] = K,

          dict:update(Date, fun(Old) -> min(Old, Contract) end, Contract, D)
      end,
      dict:new(),
      Keys
    ),
    {dict:to_list(D)};  %% JSON object

  (_, Ds, true) ->  %% a list of JSON object

    D = lists:foldl(
      fun({L}, D2) ->
        D1 = dict:from_list(L),
        dict:merge(fun(_, V1, V2) -> erlang:min(V1, V2) end, D1, D2)
      end,
      dict:new(),
      Ds
    ),
    {dict:to_list(D)}  %% {"date": "nearby contract"}
end.
