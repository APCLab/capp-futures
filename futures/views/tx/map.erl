fun({Doc}) ->
  Sym = proplists:get_value(<<"symbol">>, Doc, null),

  case Sym of
    <<"TX">> ->
      K = lists:map(
        fun(K) -> couch_util:get_value(K, Doc) end,
        [<<"date">>, <<"symbol">>, <<"contract">>]
      ),
      Rs = couch_util:get_value(<<"records">>, Doc, []),
      Emit(K, Rs);
    _ -> skip
  end
end.
