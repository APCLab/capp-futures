fun({Doc}) ->
  Sym = proplists:get_value(<<"symbol">>, Doc, null),

  case Sym of
    <<"TE">> ->
      Emit(
        lists:map(
          fun(K) -> couch_util:get_value(K, Doc) end,
          [<<"date">>, <<"symbol">>, <<"contract">>]
        ),
        couch_util:get_value(<<"records">>, Doc, [])
      );
    _ -> skip
  end
end.
