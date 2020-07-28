fun({Doc}) ->
  K = lists:map(
    fun(K) -> couch_util:get_value(K, Doc) end,
    [<<"symbol">>, <<"date">>, <<"contract">>]
  ),

  Emit(K, null),
  ok
end.
