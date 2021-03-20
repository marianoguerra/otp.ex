defmodule :cow_uri_template do
  use Bitwise
  def parse(uRITemplate) do
    parse(uRITemplate, <<>>)
  end

  defp parse(<<>>, <<>>) do
    []
  end

  defp parse(<<>>, acc) do
    [acc]
  end

  defp parse(<<?{, r :: bits>>, <<>>) do
    parse_expr(r)
  end

  defp parse(<<?{, r :: bits>>, acc) do
    [acc | parse_expr(r)]
  end

  defp parse(<<c, r :: bits>>, acc) when c !== ?} do
    parse(r, <<acc :: binary, c>>)
  end

  defp parse_expr(<<?+, r :: bits>>) do
    parse_var_list(r, :reserved_expansion, [])
  end

  defp parse_expr(<<?#, r :: bits>>) do
    parse_var_list(r, :fragment_expansion, [])
  end

  defp parse_expr(<<?., r :: bits>>) do
    parse_var_list(r, :label_expansion_with_dot_prefix, [])
  end

  defp parse_expr(<<?/, r :: bits>>) do
    parse_var_list(r, :path_segment_expansion, [])
  end

  defp parse_expr(<<?;, r :: bits>>) do
    parse_var_list(r, :path_style_parameter_expansion, [])
  end

  defp parse_expr(<<??, r :: bits>>) do
    parse_var_list(r, :form_style_query_expansion, [])
  end

  defp parse_expr(<<?&, r :: bits>>) do
    parse_var_list(r, :form_style_query_continuation, [])
  end

  defp parse_expr(r) do
    parse_var_list(r, :simple_string_expansion, [])
  end

  defp parse_var_list(<<c, r :: bits>>, op, list)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9) or c === ?_ do
    parse_varname(r, op, list, <<c>>)
  end

  defp parse_varname(<<c, r :: bits>>, op, list, name)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9) or c === ?_) or c === ?.) or c === ?% do
    parse_varname(r, op, list, <<name :: binary, c>>)
  end

  defp parse_varname(<<?:, c, r :: bits>>, op, list, name)
      when (((((((c === ?1 or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9 do
    parse_prefix_modifier(r, op, list, name, <<c>>)
  end

  defp parse_varname(<<?*, ?,, r :: bits>>, op, list, name) do
    parse_var_list(r, op,
                     [{:explode_modifier, name} | list])
  end

  defp parse_varname(<<?*, ?}, r :: bits>>, op, list, name) do
    [{:expr, op,
        :lists.reverse([{:explode_modifier, name} | list])} |
         parse(r, <<>>)]
  end

  defp parse_varname(<<?,, r :: bits>>, op, list, name) do
    parse_var_list(r, op, [{:no_modifier, name} | list])
  end

  defp parse_varname(<<?}, r :: bits>>, op, list, name) do
    [{:expr, op,
        :lists.reverse([{:no_modifier, name} | list])} |
         parse(r, <<>>)]
  end

  defp parse_prefix_modifier(<<c, r :: bits>>, op, list, name, acc)
      when (((((((((c === ?0 or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9 and
              byte_size(acc) < 4) do
    parse_prefix_modifier(r, op, list, name,
                            <<acc :: binary, c>>)
  end

  defp parse_prefix_modifier(<<?,, r :: bits>>, op, list, name, acc) do
    parse_var_list(r, op,
                     [{{:prefix_modifier, :erlang.binary_to_integer(acc)},
                         name} |
                          list])
  end

  defp parse_prefix_modifier(<<?}, r :: bits>>, op, list, name, acc) do
    [{:expr, op,
        :lists.reverse([{{:prefix_modifier,
                            :erlang.binary_to_integer(acc)},
                           name} |
                            list])} |
         parse(r, <<>>)]
  end

  def expand(uRITemplate, vars) when is_binary(uRITemplate) do
    expand(parse(uRITemplate), vars)
  end

  def expand(uRITemplate, vars) do
    expand1(uRITemplate, vars)
  end

  defp expand1([], _) do
    []
  end

  defp expand1([literal | tail], vars)
      when is_binary(literal) do
    [literal | expand1(tail, vars)]
  end

  defp expand1([{:expr, :simple_string_expansion, varList} |
               tail],
            vars) do
    [simple_string_expansion(varList, vars) | expand1(tail,
                                                        vars)]
  end

  defp expand1([{:expr, :reserved_expansion, varList} | tail],
            vars) do
    [reserved_expansion(varList, vars) | expand1(tail,
                                                   vars)]
  end

  defp expand1([{:expr, :fragment_expansion, varList} | tail],
            vars) do
    [fragment_expansion(varList, vars) | expand1(tail,
                                                   vars)]
  end

  defp expand1([{:expr, :label_expansion_with_dot_prefix,
              varList} |
               tail],
            vars) do
    [label_expansion_with_dot_prefix(varList, vars) |
         expand1(tail, vars)]
  end

  defp expand1([{:expr, :path_segment_expansion, varList} |
               tail],
            vars) do
    [path_segment_expansion(varList, vars) | expand1(tail,
                                                       vars)]
  end

  defp expand1([{:expr, :path_style_parameter_expansion,
              varList} |
               tail],
            vars) do
    [path_style_parameter_expansion(varList, vars) |
         expand1(tail, vars)]
  end

  defp expand1([{:expr, :form_style_query_expansion, varList} |
               tail],
            vars) do
    [form_style_query_expansion(varList, vars) |
         expand1(tail, vars)]
  end

  defp expand1([{:expr, :form_style_query_continuation,
              varList} |
               tail],
            vars) do
    [form_style_query_continuation(varList, vars) |
         expand1(tail, vars)]
  end

  defp simple_string_expansion(varList, vars) do
    :lists.join(?,,
                  for {modifier, _Name,
                         value} <- lookup_variables(varList, vars) do
                    apply_modifier(modifier, :unreserved, ?,, value)
                  end)
  end

  defp reserved_expansion(varList, vars) do
    :lists.join(?,,
                  for {modifier, _Name,
                         value} <- lookup_variables(varList, vars) do
                    apply_modifier(modifier, :reserved, ?,, value)
                  end)
  end

  defp fragment_expansion(varList, vars) do
    case (reserved_expansion(varList, vars)) do
      [] ->
        []
      expanded ->
        [?#, expanded]
    end
  end

  defp label_expansion_with_dot_prefix(varList, vars) do
    segment_expansion(varList, vars, ?.)
  end

  defp path_segment_expansion(varList, vars) do
    segment_expansion(varList, vars, ?/)
  end

  defp segment_expansion(varList, vars, sep) do
    expanded = :lists.join(sep,
                             for {modifier, _Name,
                                    value} <- lookup_variables(varList, vars) do
                               apply_modifier(modifier, :unreserved, sep, value)
                             end)
    case (expanded) do
      [] ->
        []
      [[]] ->
        []
      _ ->
        [sep, expanded]
    end
  end

  defp path_style_parameter_expansion(varList, vars) do
    parameter_expansion(varList, vars, ?;, ?;, :trim)
  end

  defp form_style_query_expansion(varList, vars) do
    parameter_expansion(varList, vars, ??, ?&, :no_trim)
  end

  defp form_style_query_continuation(varList, vars) do
    parameter_expansion(varList, vars, ?&, ?&, :no_trim)
  end

  defp parameter_expansion(varList, vars, leadingSep, sep, trim) do
    expanded = :lists.join(sep,
                             for {modifier, name,
                                    value} <- lookup_variables(varList, vars) do
                               apply_parameter_modifier(modifier, :unreserved,
                                                          sep, trim, name,
                                                          value)
                             end)
    case (expanded) do
      [] ->
        []
      [[]] ->
        []
      _ ->
        [leadingSep, expanded]
    end
  end

  defp lookup_variables([], _) do
    []
  end

  defp lookup_variables([{modifier, name} | tail], vars) do
    case (vars) do
      %{^name => value} ->
        [{modifier, name, value} | lookup_variables(tail, vars)]
      _ ->
        lookup_variables(tail, vars)
    end
  end

  defp apply_modifier(:no_modifier, allowedChars, _, list)
      when is_list(list) do
    :lists.join(?,,
                  for value <- list do
                    urlencode(value, allowedChars)
                  end)
  end

  defp apply_modifier(:explode_modifier, allowedChars, explodeSep,
            list)
      when is_list(list) do
    :lists.join(explodeSep,
                  for value <- list do
                    urlencode(value, allowedChars)
                  end)
  end

  defp apply_modifier(modifier, allowedChars, explodeSep, map)
      when is_map(map) do
    {joinSep, kVSep} = (case (modifier) do
                          :no_modifier ->
                            {?,, ?,}
                          :explode_modifier ->
                            {explodeSep, ?=}
                        end)
    :lists.reverse(:lists.join(joinSep,
                                 :maps.fold(fn key, value, acc ->
                                                 [[urlencode(key, allowedChars),
                                                       kVSep, urlencode(value,
                                                                          allowedChars)] |
                                                      acc]
                                            end,
                                              [], map)))
  end

  defp apply_modifier({:prefix_modifier, maxLen}, allowedChars, _,
            value) do
    urlencode(:string.slice(binarize(value), 0, maxLen),
                allowedChars)
  end

  defp apply_modifier(_, allowedChars, _, value) do
    urlencode(binarize(value), allowedChars)
  end

  defp apply_parameter_modifier(_, _, _, _, _, []) do
    []
  end

  defp apply_parameter_modifier(_, _, _, _, _, map) when map === %{} do
    []
  end

  defp apply_parameter_modifier(:no_modifier, allowedChars, _, _, name, list)
      when is_list(list) do
    [name, ?=, :lists.join(?,,
                             for value <- list do
                               urlencode(value, allowedChars)
                             end)]
  end

  defp apply_parameter_modifier(:explode_modifier, allowedChars, explodeSep, _,
            name, list)
      when is_list(list) do
    :lists.join(explodeSep,
                  for value <- list do
                    [name, ?=, urlencode(value, allowedChars)]
                  end)
  end

  defp apply_parameter_modifier(modifier, allowedChars, explodeSep, _, name,
            map)
      when is_map(map) do
    {joinSep, kVSep} = (case (modifier) do
                          :no_modifier ->
                            {?,, ?,}
                          :explode_modifier ->
                            {explodeSep, ?=}
                        end)
    [case (modifier) do
       :no_modifier ->
         [name, ?=]
       :explode_modifier ->
         []
     end,
         :lists.reverse(:lists.join(joinSep,
                                      :maps.fold(fn key, value, acc ->
                                                      [[urlencode(key,
                                                                    allowedChars),
                                                            kVSep,
                                                                urlencode(value,
                                                                            allowedChars)] |
                                                           acc]
                                                 end,
                                                   [], map)))]
  end

  defp apply_parameter_modifier(modifier, allowedChars, _, trim, name,
            value0) do
    value1 = binarize(value0)
    value = (case (modifier) do
               {:prefix_modifier, maxLen} ->
                 :string.slice(value1, 0, maxLen)
               :no_modifier ->
                 value1
             end)
    [name, case (value) do
             <<>> when trim === :trim ->
               []
             <<>> when trim === :no_trim ->
               ?=
             _ ->
               [?=, urlencode(value, allowedChars)]
           end]
  end

  defp binarize(value) when is_integer(value) do
    :erlang.integer_to_binary(value)
  end

  defp binarize(value) when is_float(value) do
    :erlang.float_to_binary(value,
                              [{:decimals, 10}, :compact])
  end

  defp binarize(value) do
    value
  end

  defp urlencode(value, :unreserved) do
    urlencode_unreserved(value, <<>>)
  end

  defp urlencode(value, :reserved) do
    urlencode_reserved(value, <<>>)
  end

  defp urlencode_unreserved(<<c, r :: bits>>, acc)
      when ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9) or c === ?-) or c === ?.) or c === ?_) or c === ?~ do
    urlencode_unreserved(r, <<acc :: binary, c>>)
  end

  defp urlencode_unreserved(<<c, r :: bits>>, acc) do
    urlencode_unreserved(r,
                           <<acc :: binary, ?%,
                               case (c >>> 4) do
                                 0 ->
                                   ?0
                                 1 ->
                                   ?1
                                 2 ->
                                   ?2
                                 3 ->
                                   ?3
                                 4 ->
                                   ?4
                                 5 ->
                                   ?5
                                 6 ->
                                   ?6
                                 7 ->
                                   ?7
                                 8 ->
                                   ?8
                                 9 ->
                                   ?9
                                 10 ->
                                   ?A
                                 11 ->
                                   ?B
                                 12 ->
                                   ?C
                                 13 ->
                                   ?D
                                 14 ->
                                   ?E
                                 15 ->
                                   ?F
                               end,
                               case (c &&& 15) do
                                 0 ->
                                   ?0
                                 1 ->
                                   ?1
                                 2 ->
                                   ?2
                                 3 ->
                                   ?3
                                 4 ->
                                   ?4
                                 5 ->
                                   ?5
                                 6 ->
                                   ?6
                                 7 ->
                                   ?7
                                 8 ->
                                   ?8
                                 9 ->
                                   ?9
                                 10 ->
                                   ?A
                                 11 ->
                                   ?B
                                 12 ->
                                   ?C
                                 13 ->
                                   ?D
                                 14 ->
                                   ?E
                                 15 ->
                                   ?F
                               end>>)
  end

  defp urlencode_unreserved(<<>>, acc) do
    acc
  end

  defp urlencode_reserved(<<c, r :: bits>>, acc)
      when ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9) or c === ?-) or c === ?.) or c === ?_) or c === ?~) or c === ?:) or c === ?/) or c === ??) or c === ?#) or c === ?[) or c === ?]) or c === ?@) or c === ?!) or c === ?$) or c === ?&) or c === ?') or c === ?() or c === ?)) or c === ?*) or c === ?+) or c === ?,) or c === ?;) or c === ?= do
    urlencode_reserved(r, <<acc :: binary, c>>)
  end

  defp urlencode_reserved(<<c, r :: bits>>, acc) do
    urlencode_reserved(r,
                         <<acc :: binary, ?%,
                             case (c >>> 4) do
                               0 ->
                                 ?0
                               1 ->
                                 ?1
                               2 ->
                                 ?2
                               3 ->
                                 ?3
                               4 ->
                                 ?4
                               5 ->
                                 ?5
                               6 ->
                                 ?6
                               7 ->
                                 ?7
                               8 ->
                                 ?8
                               9 ->
                                 ?9
                               10 ->
                                 ?A
                               11 ->
                                 ?B
                               12 ->
                                 ?C
                               13 ->
                                 ?D
                               14 ->
                                 ?E
                               15 ->
                                 ?F
                             end,
                             case (c &&& 15) do
                               0 ->
                                 ?0
                               1 ->
                                 ?1
                               2 ->
                                 ?2
                               3 ->
                                 ?3
                               4 ->
                                 ?4
                               5 ->
                                 ?5
                               6 ->
                                 ?6
                               7 ->
                                 ?7
                               8 ->
                                 ?8
                               9 ->
                                 ?9
                               10 ->
                                 ?A
                               11 ->
                                 ?B
                               12 ->
                                 ?C
                               13 ->
                                 ?D
                               14 ->
                                 ?E
                               15 ->
                                 ?F
                             end>>)
  end

  defp urlencode_reserved(<<>>, acc) do
    acc
  end

end