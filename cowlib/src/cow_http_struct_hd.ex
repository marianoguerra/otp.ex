defmodule :cow_http_struct_hd do
  use Bitwise
  def parse_dictionary(<<>>) do
    {%{}, []}
  end

  def parse_dictionary(<<c, r :: bits>>)
      when ((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z do
    {dict, order, <<>>} = parse_dict_key(r, %{}, [], <<c>>)
    {dict, order}
  end

  defp parse_dict_key(<<?=, ?(, r0 :: bits>>, acc, order, k) do
    false = :maps.is_key(k, acc)
    {item, r} = parse_inner_list(r0, [])
    parse_dict_before_sep(r, Map.put(acc, k, item),
                            [k | order])
  end

  defp parse_dict_key(<<?=, r0 :: bits>>, acc, order, k) do
    false = :maps.is_key(k, acc)
    {item, r} = parse_item1(r0)
    parse_dict_before_sep(r, Map.put(acc, k, item),
                            [k | order])
  end

  defp parse_dict_key(<<c, r :: bits>>, acc, order, k)
      when (((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9) or c === ?_) or c === ?-) or c === ?* do
    parse_dict_key(r, acc, order, <<k :: binary, c>>)
  end

  defp parse_dict_before_sep(<<c, r :: bits>>, acc, order)
      when c === ?\s or c === ?\t do
    parse_dict_before_sep(r, acc, order)
  end

  defp parse_dict_before_sep(<<c, r :: bits>>, acc, order) when c === ?, do
    parse_dict_before_member(r, acc, order)
  end

  defp parse_dict_before_sep(<<>>, acc, order) do
    {acc, :lists.reverse(order), <<>>}
  end

  defp parse_dict_before_member(<<c, r :: bits>>, acc, order)
      when c === ?\s or c === ?\t do
    parse_dict_before_member(r, acc, order)
  end

  defp parse_dict_before_member(<<c, r :: bits>>, acc, order)
      when ((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z do
    parse_dict_key(r, acc, order, <<c>>)
  end

  def parse_item(bin) do
    {item, <<>>} = parse_item1(bin)
    item
  end

  defp parse_item1(bin) do
    case (parse_bare_item(bin)) do
      {item, <<?;, r :: bits>>} ->
        {params, rest} = parse_before_param(r, %{})
        {{:with_params, item, params}, rest}
      {item, rest} ->
        {{:with_params, item, %{}}, rest}
    end
  end

  def parse_list(<<>>) do
    []
  end

  def parse_list(bin) do
    parse_list_before_member(bin, [])
  end

  defp parse_list_member(<<?(, r0 :: bits>>, acc) do
    {item, r} = parse_inner_list(r0, [])
    parse_list_before_sep(r, [item | acc])
  end

  defp parse_list_member(r0, acc) do
    {item, r} = parse_item1(r0)
    parse_list_before_sep(r, [item | acc])
  end

  defp parse_list_before_sep(<<c, r :: bits>>, acc)
      when c === ?\s or c === ?\t do
    parse_list_before_sep(r, acc)
  end

  defp parse_list_before_sep(<<?,, r :: bits>>, acc) do
    parse_list_before_member(r, acc)
  end

  defp parse_list_before_sep(<<>>, acc) do
    :lists.reverse(acc)
  end

  defp parse_list_before_member(<<c, r :: bits>>, acc)
      when c === ?\s or c === ?\t do
    parse_list_before_member(r, acc)
  end

  defp parse_list_before_member(r, acc) do
    parse_list_member(r, acc)
  end

  defp parse_inner_list(<<c, r :: bits>>, acc)
      when c === ?\s or c === ?\t do
    parse_inner_list(r, acc)
  end

  defp parse_inner_list(<<?), ?;, r0 :: bits>>, acc) do
    {params, r} = parse_before_param(r0, %{})
    {{:with_params, :lists.reverse(acc), params}, r}
  end

  defp parse_inner_list(<<?), r :: bits>>, acc) do
    {{:with_params, :lists.reverse(acc), %{}}, r}
  end

  defp parse_inner_list(r0, acc) do
    {item, r = <<c, _ :: bits>>} = parse_item1(r0)
    true = c === ?\s or c === ?)
    parse_inner_list(r, [item | acc])
  end

  defp parse_before_param(<<c, r :: bits>>, acc)
      when c === ?\s or c === ?\t do
    parse_before_param(r, acc)
  end

  defp parse_before_param(<<c, r :: bits>>, acc)
      when ((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z do
    parse_param(r, acc, <<c>>)
  end

  defp parse_param(<<?;, r :: bits>>, acc, k) do
    parse_before_param(r, Map.put(acc, k, :undefined))
  end

  defp parse_param(<<?=, r0 :: bits>>, acc, k) do
    case (parse_bare_item(r0)) do
      {item, <<?;, r :: bits>>} ->
        false = :maps.is_key(k, acc)
        parse_before_param(r, Map.put(acc, k, item))
      {item, r} ->
        false = :maps.is_key(k, acc)
        {Map.put(acc, k, item), r}
    end
  end

  defp parse_param(<<c, r :: bits>>, acc, k)
      when (((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9) or c === ?_) or c === ?-) or c === ?* do
    parse_param(r, acc, <<k :: binary, c>>)
  end

  defp parse_param(r, acc, k) do
    false = :maps.is_key(k, acc)
    {Map.put(acc, k, :undefined), r}
  end

  defp parse_bare_item(<<?-, r :: bits>>) do
    parse_number(r, 0, <<?->>)
  end

  defp parse_bare_item(<<c, r :: bits>>)
      when ((((((((c === ?0 or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9 do
    parse_number(r, 1, <<c>>)
  end

  defp parse_bare_item(<<?", r :: bits>>) do
    parse_string(r, <<>>)
  end

  defp parse_bare_item(<<c, r :: bits>>)
      when ((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z do
    parse_token(r, <<c>>)
  end

  defp parse_bare_item(<<?*, r :: bits>>) do
    parse_binary(r, <<>>)
  end

  defp parse_bare_item(<<"?0", r :: bits>>) do
    {false, r}
  end

  defp parse_bare_item(<<"?1", r :: bits>>) do
    {true, r}
  end

  defp parse_number(<<c, r :: bits>>, l, acc)
      when ((((((((c === ?0 or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9 do
    parse_number(r, l + 1, <<acc :: binary, c>>)
  end

  defp parse_number(<<c, r :: bits>>, l, acc) when c === ?. do
    parse_float(r, l, 0, <<acc :: binary, c>>)
  end

  defp parse_number(r, l, acc) when l <= 15 do
    {:erlang.binary_to_integer(acc), r}
  end

  defp parse_float(<<c, r :: bits>>, l1, l2, acc)
      when ((((((((c === ?0 or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9 do
    parse_float(r, l1, l2 + 1, <<acc :: binary, c>>)
  end

  defp parse_float(r, l1, l2, acc) when (l1 <= 9 and l2 <= 6) or
                                 (l1 <= 10 and l2 <= 5) or
                                 (l1 <= 11 and l2 <= 4) or
                                 (l1 <= 12 and l2 <= 3) or
                                 (l1 <= 13 and l2 <= 2) or
                                 (l1 <= 14 and l2 <= 1) do
    {:erlang.binary_to_float(acc), r}
  end

  defp parse_string(<<?\\, ?", r :: bits>>, acc) do
    parse_string(r, <<acc :: binary, ?">>)
  end

  defp parse_string(<<?\\, ?\\, r :: bits>>, acc) do
    parse_string(r, <<acc :: binary, ?\\>>)
  end

  defp parse_string(<<?", r :: bits>>, acc) do
    {{:string, acc}, r}
  end

  defp parse_string(<<c, r :: bits>>, acc) when (c >= 32 and
                                         c <= 33) or
                                        (c >= 35 and c <= 91) or
                                        (c >= 93 and c <= 126) do
    parse_string(r, <<acc :: binary, c>>)
  end

  defp parse_token(<<c, r :: bits>>, acc)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9) or c === ?!) or c === ?#) or c === ?$) or c === ?%) or c === ?&) or c === ?') or c === ?*) or c === ?+) or c === ?-) or c === ?.) or c === ?^) or c === ?_) or c === ?`) or c === ?|) or c === ?~) or c === ?:) or c === ?/ do
    parse_token(r, <<acc :: binary, c>>)
  end

  defp parse_token(r, acc) do
    {{:token, acc}, r}
  end

  defp parse_binary(<<?*, r :: bits>>, acc) do
    {{:binary, :base64.decode(acc)}, r}
  end

  defp parse_binary(<<c, r :: bits>>, acc)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9) or c === ?+) or c === ?/) or c === ?= do
    parse_binary(r, <<acc :: binary, c>>)
  end

  def dictionary(map) when is_map(map) do
    dictionary(:maps.to_list(map))
  end

  def dictionary(kVList) when is_list(kVList) do
    :lists.join(", ",
                  for {key, value} <- kVList do
                    [key, ?=, item_or_inner_list(value)]
                  end)
  end

  def item({:with_params, bareItem, params}) do
    [bare_item(bareItem), params(params)]
  end

  def list(list) do
    :lists.join(", ",
                  for value <- list do
                    item_or_inner_list(value)
                  end)
  end

  defp item_or_inner_list(value = {:with_params, list, _})
      when is_list(list) do
    inner_list(value)
  end

  defp item_or_inner_list(value) do
    item(value)
  end

  defp inner_list({:with_params, list, params}) do
    [?(, :lists.join(?\s,
                       for value <- list do
                         item(value)
                       end),
             ?), params(params)]
  end

  defp bare_item({:string, string}) do
    [?", escape_string(string, <<>>), ?"]
  end

  defp bare_item({:token, token}) do
    token
  end

  defp bare_item({:binary, binary}) do
    [?*, :base64.encode(binary), ?*]
  end

  defp bare_item(integer) when is_integer(integer) do
    :erlang.integer_to_binary(integer)
  end

  defp bare_item(float) when is_float(float) do
    decimals = (case (trunc(float)) do
                  i when i >= 10000000000000 ->
                    1
                  i when i >= 1000000000000 ->
                    2
                  i when i >= 100000000000 ->
                    3
                  i when i >= 10000000000 ->
                    4
                  i when i >= 1000000000 ->
                    5
                  _ ->
                    6
                end)
    :erlang.float_to_binary(float,
                              [{:decimals, decimals}, :compact])
  end

  defp bare_item(true) do
    "?1"
  end

  defp bare_item(false) do
    "?0"
  end

  defp escape_string(<<>>, acc) do
    acc
  end

  defp escape_string(<<?\\, r :: bits>>, acc) do
    escape_string(r, <<acc :: binary, ?\\, ?\\>>)
  end

  defp escape_string(<<?", r :: bits>>, acc) do
    escape_string(r, <<acc :: binary, ?\\, ?">>)
  end

  defp escape_string(<<c, r :: bits>>, acc) do
    escape_string(r, <<acc :: binary, c>>)
  end

  defp params(params) do
    :maps.fold(fn key, :undefined, acc ->
                    [[?;, key] | acc]
                  key, value, acc ->
                    [[?;, key, ?=, bare_item(value)] | acc]
               end,
                 [], params)
  end

end