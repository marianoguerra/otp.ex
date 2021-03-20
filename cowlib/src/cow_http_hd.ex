defmodule :cow_http_hd do
  use Bitwise
  def parse_accept("*/*") do
    [{{"*", "*", []}, 1000, []}]
  end

  def parse_accept(accept) do
    media_range_list(accept, [])
  end

  defp media_range_list(<<c, r :: bits>>, acc)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9) or c === ?!) or c === ?#) or c === ?$) or c === ?%) or c === ?&) or c === ?') or c === ?*) or c === ?+) or c === ?-) or c === ?.) or c === ?^) or c === ?_) or c === ?`) or c === ?|) or c === ?~ do
    case (c) do
      ?A ->
        media_range_type(r, acc, <<<<>> :: binary, ?a>>)
      ?B ->
        media_range_type(r, acc, <<<<>> :: binary, ?b>>)
      ?C ->
        media_range_type(r, acc, <<<<>> :: binary, ?c>>)
      ?D ->
        media_range_type(r, acc, <<<<>> :: binary, ?d>>)
      ?E ->
        media_range_type(r, acc, <<<<>> :: binary, ?e>>)
      ?F ->
        media_range_type(r, acc, <<<<>> :: binary, ?f>>)
      ?G ->
        media_range_type(r, acc, <<<<>> :: binary, ?g>>)
      ?H ->
        media_range_type(r, acc, <<<<>> :: binary, ?h>>)
      ?I ->
        media_range_type(r, acc, <<<<>> :: binary, ?i>>)
      ?J ->
        media_range_type(r, acc, <<<<>> :: binary, ?j>>)
      ?K ->
        media_range_type(r, acc, <<<<>> :: binary, ?k>>)
      ?L ->
        media_range_type(r, acc, <<<<>> :: binary, ?l>>)
      ?M ->
        media_range_type(r, acc, <<<<>> :: binary, ?m>>)
      ?N ->
        media_range_type(r, acc, <<<<>> :: binary, ?n>>)
      ?O ->
        media_range_type(r, acc, <<<<>> :: binary, ?o>>)
      ?P ->
        media_range_type(r, acc, <<<<>> :: binary, ?p>>)
      ?Q ->
        media_range_type(r, acc, <<<<>> :: binary, ?q>>)
      ?R ->
        media_range_type(r, acc, <<<<>> :: binary, ?r>>)
      ?S ->
        media_range_type(r, acc, <<<<>> :: binary, ?s>>)
      ?T ->
        media_range_type(r, acc, <<<<>> :: binary, ?t>>)
      ?U ->
        media_range_type(r, acc, <<<<>> :: binary, ?u>>)
      ?V ->
        media_range_type(r, acc, <<<<>> :: binary, ?v>>)
      ?W ->
        media_range_type(r, acc, <<<<>> :: binary, ?w>>)
      ?X ->
        media_range_type(r, acc, <<<<>> :: binary, ?x>>)
      ?Y ->
        media_range_type(r, acc, <<<<>> :: binary, ?y>>)
      ?Z ->
        media_range_type(r, acc, <<<<>> :: binary, ?z>>)
      ^c ->
        media_range_type(r, acc, <<<<>> :: binary, c>>)
    end
  end

  defp media_range_list(<<c, r :: bits>>, acc)
      when (c === ?\s or c === ?\t) or c === ?, do
    media_range_list(r, acc)
  end

  defp media_range_list(<<>>, acc) do
    :lists.reverse(acc)
  end

  defp media_range_type(<<c, r :: bits>>, acc, t)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9) or c === ?!) or c === ?#) or c === ?$) or c === ?%) or c === ?&) or c === ?') or c === ?*) or c === ?+) or c === ?-) or c === ?.) or c === ?^) or c === ?_) or c === ?`) or c === ?|) or c === ?~ do
    case (c) do
      ?A ->
        media_range_type(r, acc, <<t :: binary, ?a>>)
      ?B ->
        media_range_type(r, acc, <<t :: binary, ?b>>)
      ?C ->
        media_range_type(r, acc, <<t :: binary, ?c>>)
      ?D ->
        media_range_type(r, acc, <<t :: binary, ?d>>)
      ?E ->
        media_range_type(r, acc, <<t :: binary, ?e>>)
      ?F ->
        media_range_type(r, acc, <<t :: binary, ?f>>)
      ?G ->
        media_range_type(r, acc, <<t :: binary, ?g>>)
      ?H ->
        media_range_type(r, acc, <<t :: binary, ?h>>)
      ?I ->
        media_range_type(r, acc, <<t :: binary, ?i>>)
      ?J ->
        media_range_type(r, acc, <<t :: binary, ?j>>)
      ?K ->
        media_range_type(r, acc, <<t :: binary, ?k>>)
      ?L ->
        media_range_type(r, acc, <<t :: binary, ?l>>)
      ?M ->
        media_range_type(r, acc, <<t :: binary, ?m>>)
      ?N ->
        media_range_type(r, acc, <<t :: binary, ?n>>)
      ?O ->
        media_range_type(r, acc, <<t :: binary, ?o>>)
      ?P ->
        media_range_type(r, acc, <<t :: binary, ?p>>)
      ?Q ->
        media_range_type(r, acc, <<t :: binary, ?q>>)
      ?R ->
        media_range_type(r, acc, <<t :: binary, ?r>>)
      ?S ->
        media_range_type(r, acc, <<t :: binary, ?s>>)
      ?T ->
        media_range_type(r, acc, <<t :: binary, ?t>>)
      ?U ->
        media_range_type(r, acc, <<t :: binary, ?u>>)
      ?V ->
        media_range_type(r, acc, <<t :: binary, ?v>>)
      ?W ->
        media_range_type(r, acc, <<t :: binary, ?w>>)
      ?X ->
        media_range_type(r, acc, <<t :: binary, ?x>>)
      ?Y ->
        media_range_type(r, acc, <<t :: binary, ?y>>)
      ?Z ->
        media_range_type(r, acc, <<t :: binary, ?z>>)
      ^c ->
        media_range_type(r, acc, <<t :: binary, c>>)
    end
  end

  defp media_range_type(<<?/, c, r :: bits>>, acc, t)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9) or c === ?!) or c === ?#) or c === ?$) or c === ?%) or c === ?&) or c === ?') or c === ?*) or c === ?+) or c === ?-) or c === ?.) or c === ?^) or c === ?_) or c === ?`) or c === ?|) or c === ?~ do
    case (c) do
      ?A ->
        media_range_subtype(r, acc, t, <<<<>> :: binary, ?a>>)
      ?B ->
        media_range_subtype(r, acc, t, <<<<>> :: binary, ?b>>)
      ?C ->
        media_range_subtype(r, acc, t, <<<<>> :: binary, ?c>>)
      ?D ->
        media_range_subtype(r, acc, t, <<<<>> :: binary, ?d>>)
      ?E ->
        media_range_subtype(r, acc, t, <<<<>> :: binary, ?e>>)
      ?F ->
        media_range_subtype(r, acc, t, <<<<>> :: binary, ?f>>)
      ?G ->
        media_range_subtype(r, acc, t, <<<<>> :: binary, ?g>>)
      ?H ->
        media_range_subtype(r, acc, t, <<<<>> :: binary, ?h>>)
      ?I ->
        media_range_subtype(r, acc, t, <<<<>> :: binary, ?i>>)
      ?J ->
        media_range_subtype(r, acc, t, <<<<>> :: binary, ?j>>)
      ?K ->
        media_range_subtype(r, acc, t, <<<<>> :: binary, ?k>>)
      ?L ->
        media_range_subtype(r, acc, t, <<<<>> :: binary, ?l>>)
      ?M ->
        media_range_subtype(r, acc, t, <<<<>> :: binary, ?m>>)
      ?N ->
        media_range_subtype(r, acc, t, <<<<>> :: binary, ?n>>)
      ?O ->
        media_range_subtype(r, acc, t, <<<<>> :: binary, ?o>>)
      ?P ->
        media_range_subtype(r, acc, t, <<<<>> :: binary, ?p>>)
      ?Q ->
        media_range_subtype(r, acc, t, <<<<>> :: binary, ?q>>)
      ?R ->
        media_range_subtype(r, acc, t, <<<<>> :: binary, ?r>>)
      ?S ->
        media_range_subtype(r, acc, t, <<<<>> :: binary, ?s>>)
      ?T ->
        media_range_subtype(r, acc, t, <<<<>> :: binary, ?t>>)
      ?U ->
        media_range_subtype(r, acc, t, <<<<>> :: binary, ?u>>)
      ?V ->
        media_range_subtype(r, acc, t, <<<<>> :: binary, ?v>>)
      ?W ->
        media_range_subtype(r, acc, t, <<<<>> :: binary, ?w>>)
      ?X ->
        media_range_subtype(r, acc, t, <<<<>> :: binary, ?x>>)
      ?Y ->
        media_range_subtype(r, acc, t, <<<<>> :: binary, ?y>>)
      ?Z ->
        media_range_subtype(r, acc, t, <<<<>> :: binary, ?z>>)
      ^c ->
        media_range_subtype(r, acc, t, <<<<>> :: binary, c>>)
    end
  end

  defp media_range_type(<<?;, r :: bits>>, acc, "*") do
    media_range_before_param(r, acc, "*", "*", [])
  end

  defp media_range_subtype(<<c, r :: bits>>, acc, t, s)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9) or c === ?!) or c === ?#) or c === ?$) or c === ?%) or c === ?&) or c === ?') or c === ?*) or c === ?+) or c === ?-) or c === ?.) or c === ?^) or c === ?_) or c === ?`) or c === ?|) or c === ?~ do
    case (c) do
      ?A ->
        media_range_subtype(r, acc, t, <<s :: binary, ?a>>)
      ?B ->
        media_range_subtype(r, acc, t, <<s :: binary, ?b>>)
      ?C ->
        media_range_subtype(r, acc, t, <<s :: binary, ?c>>)
      ?D ->
        media_range_subtype(r, acc, t, <<s :: binary, ?d>>)
      ?E ->
        media_range_subtype(r, acc, t, <<s :: binary, ?e>>)
      ?F ->
        media_range_subtype(r, acc, t, <<s :: binary, ?f>>)
      ?G ->
        media_range_subtype(r, acc, t, <<s :: binary, ?g>>)
      ?H ->
        media_range_subtype(r, acc, t, <<s :: binary, ?h>>)
      ?I ->
        media_range_subtype(r, acc, t, <<s :: binary, ?i>>)
      ?J ->
        media_range_subtype(r, acc, t, <<s :: binary, ?j>>)
      ?K ->
        media_range_subtype(r, acc, t, <<s :: binary, ?k>>)
      ?L ->
        media_range_subtype(r, acc, t, <<s :: binary, ?l>>)
      ?M ->
        media_range_subtype(r, acc, t, <<s :: binary, ?m>>)
      ?N ->
        media_range_subtype(r, acc, t, <<s :: binary, ?n>>)
      ?O ->
        media_range_subtype(r, acc, t, <<s :: binary, ?o>>)
      ?P ->
        media_range_subtype(r, acc, t, <<s :: binary, ?p>>)
      ?Q ->
        media_range_subtype(r, acc, t, <<s :: binary, ?q>>)
      ?R ->
        media_range_subtype(r, acc, t, <<s :: binary, ?r>>)
      ?S ->
        media_range_subtype(r, acc, t, <<s :: binary, ?s>>)
      ?T ->
        media_range_subtype(r, acc, t, <<s :: binary, ?t>>)
      ?U ->
        media_range_subtype(r, acc, t, <<s :: binary, ?u>>)
      ?V ->
        media_range_subtype(r, acc, t, <<s :: binary, ?v>>)
      ?W ->
        media_range_subtype(r, acc, t, <<s :: binary, ?w>>)
      ?X ->
        media_range_subtype(r, acc, t, <<s :: binary, ?x>>)
      ?Y ->
        media_range_subtype(r, acc, t, <<s :: binary, ?y>>)
      ?Z ->
        media_range_subtype(r, acc, t, <<s :: binary, ?z>>)
      ^c ->
        media_range_subtype(r, acc, t, <<s :: binary, c>>)
    end
  end

  defp media_range_subtype(r, acc, t, s) do
    media_range_param_sep(r, acc, t, s, [])
  end

  defp media_range_param_sep(<<>>, acc, t, s, p) do
    :lists.reverse([{{t, s, :lists.reverse(p)}, 1000, []} |
                        acc])
  end

  defp media_range_param_sep(<<?,, r :: bits>>, acc, t, s, p) do
    media_range_list(r,
                       [{{t, s, :lists.reverse(p)}, 1000, []} | acc])
  end

  defp media_range_param_sep(<<?;, r :: bits>>, acc, t, s, p) do
    media_range_before_param(r, acc, t, s, p)
  end

  defp media_range_param_sep(<<c, r :: bits>>, acc, t, s, p)
      when c === ?\s or c === ?\t do
    media_range_param_sep(r, acc, t, s, p)
  end

  defp media_range_before_param(<<c, r :: bits>>, acc, t, s, p)
      when c === ?\s or c === ?\t do
    media_range_before_param(r, acc, t, s, p)
  end

  defp media_range_before_param(<<?q, ?=, r :: bits>>, acc, t, s, p) do
    media_range_weight(r, acc, t, s, p)
  end

  defp media_range_before_param(<<"charset=", ?", r :: bits>>, acc, t, s, p) do
    media_range_charset_quoted(r, acc, t, s, p, <<>>)
  end

  defp media_range_before_param(<<"charset=", r :: bits>>, acc, t, s, p) do
    media_range_charset(r, acc, t, s, p, <<>>)
  end

  defp media_range_before_param(<<c, r :: bits>>, acc, t, s, p)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9) or c === ?!) or c === ?#) or c === ?$) or c === ?%) or c === ?&) or c === ?') or c === ?*) or c === ?+) or c === ?-) or c === ?.) or c === ?^) or c === ?_) or c === ?`) or c === ?|) or c === ?~ do
    case (c) do
      ?A ->
        media_range_param(r, acc, t, s, p,
                            <<<<>> :: binary, ?a>>)
      ?B ->
        media_range_param(r, acc, t, s, p,
                            <<<<>> :: binary, ?b>>)
      ?C ->
        media_range_param(r, acc, t, s, p,
                            <<<<>> :: binary, ?c>>)
      ?D ->
        media_range_param(r, acc, t, s, p,
                            <<<<>> :: binary, ?d>>)
      ?E ->
        media_range_param(r, acc, t, s, p,
                            <<<<>> :: binary, ?e>>)
      ?F ->
        media_range_param(r, acc, t, s, p,
                            <<<<>> :: binary, ?f>>)
      ?G ->
        media_range_param(r, acc, t, s, p,
                            <<<<>> :: binary, ?g>>)
      ?H ->
        media_range_param(r, acc, t, s, p,
                            <<<<>> :: binary, ?h>>)
      ?I ->
        media_range_param(r, acc, t, s, p,
                            <<<<>> :: binary, ?i>>)
      ?J ->
        media_range_param(r, acc, t, s, p,
                            <<<<>> :: binary, ?j>>)
      ?K ->
        media_range_param(r, acc, t, s, p,
                            <<<<>> :: binary, ?k>>)
      ?L ->
        media_range_param(r, acc, t, s, p,
                            <<<<>> :: binary, ?l>>)
      ?M ->
        media_range_param(r, acc, t, s, p,
                            <<<<>> :: binary, ?m>>)
      ?N ->
        media_range_param(r, acc, t, s, p,
                            <<<<>> :: binary, ?n>>)
      ?O ->
        media_range_param(r, acc, t, s, p,
                            <<<<>> :: binary, ?o>>)
      ?P ->
        media_range_param(r, acc, t, s, p,
                            <<<<>> :: binary, ?p>>)
      ?Q ->
        media_range_param(r, acc, t, s, p,
                            <<<<>> :: binary, ?q>>)
      ?R ->
        media_range_param(r, acc, t, s, p,
                            <<<<>> :: binary, ?r>>)
      ?S ->
        media_range_param(r, acc, t, s, p,
                            <<<<>> :: binary, ?s>>)
      ?T ->
        media_range_param(r, acc, t, s, p,
                            <<<<>> :: binary, ?t>>)
      ?U ->
        media_range_param(r, acc, t, s, p,
                            <<<<>> :: binary, ?u>>)
      ?V ->
        media_range_param(r, acc, t, s, p,
                            <<<<>> :: binary, ?v>>)
      ?W ->
        media_range_param(r, acc, t, s, p,
                            <<<<>> :: binary, ?w>>)
      ?X ->
        media_range_param(r, acc, t, s, p,
                            <<<<>> :: binary, ?x>>)
      ?Y ->
        media_range_param(r, acc, t, s, p,
                            <<<<>> :: binary, ?y>>)
      ?Z ->
        media_range_param(r, acc, t, s, p,
                            <<<<>> :: binary, ?z>>)
      ^c ->
        media_range_param(r, acc, t, s, p,
                            <<<<>> :: binary, c>>)
    end
  end

  defp media_range_charset_quoted(<<?", r :: bits>>, acc, t, s, p, v) do
    media_range_param_sep(r, acc, t, s, [{"charset", v} | p])
  end

  defp media_range_charset_quoted(<<?\\, c, r :: bits>>, acc, t, s, p, v)
      when c === ?\t or (c > 31 and c !== 127) do
    case (c) do
      ?A ->
        media_range_charset_quoted(r, acc, t, s, p,
                                     <<v :: binary, ?a>>)
      ?B ->
        media_range_charset_quoted(r, acc, t, s, p,
                                     <<v :: binary, ?b>>)
      ?C ->
        media_range_charset_quoted(r, acc, t, s, p,
                                     <<v :: binary, ?c>>)
      ?D ->
        media_range_charset_quoted(r, acc, t, s, p,
                                     <<v :: binary, ?d>>)
      ?E ->
        media_range_charset_quoted(r, acc, t, s, p,
                                     <<v :: binary, ?e>>)
      ?F ->
        media_range_charset_quoted(r, acc, t, s, p,
                                     <<v :: binary, ?f>>)
      ?G ->
        media_range_charset_quoted(r, acc, t, s, p,
                                     <<v :: binary, ?g>>)
      ?H ->
        media_range_charset_quoted(r, acc, t, s, p,
                                     <<v :: binary, ?h>>)
      ?I ->
        media_range_charset_quoted(r, acc, t, s, p,
                                     <<v :: binary, ?i>>)
      ?J ->
        media_range_charset_quoted(r, acc, t, s, p,
                                     <<v :: binary, ?j>>)
      ?K ->
        media_range_charset_quoted(r, acc, t, s, p,
                                     <<v :: binary, ?k>>)
      ?L ->
        media_range_charset_quoted(r, acc, t, s, p,
                                     <<v :: binary, ?l>>)
      ?M ->
        media_range_charset_quoted(r, acc, t, s, p,
                                     <<v :: binary, ?m>>)
      ?N ->
        media_range_charset_quoted(r, acc, t, s, p,
                                     <<v :: binary, ?n>>)
      ?O ->
        media_range_charset_quoted(r, acc, t, s, p,
                                     <<v :: binary, ?o>>)
      ?P ->
        media_range_charset_quoted(r, acc, t, s, p,
                                     <<v :: binary, ?p>>)
      ?Q ->
        media_range_charset_quoted(r, acc, t, s, p,
                                     <<v :: binary, ?q>>)
      ?R ->
        media_range_charset_quoted(r, acc, t, s, p,
                                     <<v :: binary, ?r>>)
      ?S ->
        media_range_charset_quoted(r, acc, t, s, p,
                                     <<v :: binary, ?s>>)
      ?T ->
        media_range_charset_quoted(r, acc, t, s, p,
                                     <<v :: binary, ?t>>)
      ?U ->
        media_range_charset_quoted(r, acc, t, s, p,
                                     <<v :: binary, ?u>>)
      ?V ->
        media_range_charset_quoted(r, acc, t, s, p,
                                     <<v :: binary, ?v>>)
      ?W ->
        media_range_charset_quoted(r, acc, t, s, p,
                                     <<v :: binary, ?w>>)
      ?X ->
        media_range_charset_quoted(r, acc, t, s, p,
                                     <<v :: binary, ?x>>)
      ?Y ->
        media_range_charset_quoted(r, acc, t, s, p,
                                     <<v :: binary, ?y>>)
      ?Z ->
        media_range_charset_quoted(r, acc, t, s, p,
                                     <<v :: binary, ?z>>)
      ^c ->
        media_range_charset_quoted(r, acc, t, s, p,
                                     <<v :: binary, c>>)
    end
  end

  defp media_range_charset_quoted(<<c, r :: bits>>, acc, t, s, p, v)
      when c === ?\t or (c > 31 and c !== 127) do
    case (c) do
      ?A ->
        media_range_charset_quoted(r, acc, t, s, p,
                                     <<v :: binary, ?a>>)
      ?B ->
        media_range_charset_quoted(r, acc, t, s, p,
                                     <<v :: binary, ?b>>)
      ?C ->
        media_range_charset_quoted(r, acc, t, s, p,
                                     <<v :: binary, ?c>>)
      ?D ->
        media_range_charset_quoted(r, acc, t, s, p,
                                     <<v :: binary, ?d>>)
      ?E ->
        media_range_charset_quoted(r, acc, t, s, p,
                                     <<v :: binary, ?e>>)
      ?F ->
        media_range_charset_quoted(r, acc, t, s, p,
                                     <<v :: binary, ?f>>)
      ?G ->
        media_range_charset_quoted(r, acc, t, s, p,
                                     <<v :: binary, ?g>>)
      ?H ->
        media_range_charset_quoted(r, acc, t, s, p,
                                     <<v :: binary, ?h>>)
      ?I ->
        media_range_charset_quoted(r, acc, t, s, p,
                                     <<v :: binary, ?i>>)
      ?J ->
        media_range_charset_quoted(r, acc, t, s, p,
                                     <<v :: binary, ?j>>)
      ?K ->
        media_range_charset_quoted(r, acc, t, s, p,
                                     <<v :: binary, ?k>>)
      ?L ->
        media_range_charset_quoted(r, acc, t, s, p,
                                     <<v :: binary, ?l>>)
      ?M ->
        media_range_charset_quoted(r, acc, t, s, p,
                                     <<v :: binary, ?m>>)
      ?N ->
        media_range_charset_quoted(r, acc, t, s, p,
                                     <<v :: binary, ?n>>)
      ?O ->
        media_range_charset_quoted(r, acc, t, s, p,
                                     <<v :: binary, ?o>>)
      ?P ->
        media_range_charset_quoted(r, acc, t, s, p,
                                     <<v :: binary, ?p>>)
      ?Q ->
        media_range_charset_quoted(r, acc, t, s, p,
                                     <<v :: binary, ?q>>)
      ?R ->
        media_range_charset_quoted(r, acc, t, s, p,
                                     <<v :: binary, ?r>>)
      ?S ->
        media_range_charset_quoted(r, acc, t, s, p,
                                     <<v :: binary, ?s>>)
      ?T ->
        media_range_charset_quoted(r, acc, t, s, p,
                                     <<v :: binary, ?t>>)
      ?U ->
        media_range_charset_quoted(r, acc, t, s, p,
                                     <<v :: binary, ?u>>)
      ?V ->
        media_range_charset_quoted(r, acc, t, s, p,
                                     <<v :: binary, ?v>>)
      ?W ->
        media_range_charset_quoted(r, acc, t, s, p,
                                     <<v :: binary, ?w>>)
      ?X ->
        media_range_charset_quoted(r, acc, t, s, p,
                                     <<v :: binary, ?x>>)
      ?Y ->
        media_range_charset_quoted(r, acc, t, s, p,
                                     <<v :: binary, ?y>>)
      ?Z ->
        media_range_charset_quoted(r, acc, t, s, p,
                                     <<v :: binary, ?z>>)
      ^c ->
        media_range_charset_quoted(r, acc, t, s, p,
                                     <<v :: binary, c>>)
    end
  end

  defp media_range_charset(<<c, r :: bits>>, acc, t, s, p, v)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9) or c === ?!) or c === ?#) or c === ?$) or c === ?%) or c === ?&) or c === ?') or c === ?*) or c === ?+) or c === ?-) or c === ?.) or c === ?^) or c === ?_) or c === ?`) or c === ?|) or c === ?~ do
    case (c) do
      ?A ->
        media_range_charset(r, acc, t, s, p,
                              <<v :: binary, ?a>>)
      ?B ->
        media_range_charset(r, acc, t, s, p,
                              <<v :: binary, ?b>>)
      ?C ->
        media_range_charset(r, acc, t, s, p,
                              <<v :: binary, ?c>>)
      ?D ->
        media_range_charset(r, acc, t, s, p,
                              <<v :: binary, ?d>>)
      ?E ->
        media_range_charset(r, acc, t, s, p,
                              <<v :: binary, ?e>>)
      ?F ->
        media_range_charset(r, acc, t, s, p,
                              <<v :: binary, ?f>>)
      ?G ->
        media_range_charset(r, acc, t, s, p,
                              <<v :: binary, ?g>>)
      ?H ->
        media_range_charset(r, acc, t, s, p,
                              <<v :: binary, ?h>>)
      ?I ->
        media_range_charset(r, acc, t, s, p,
                              <<v :: binary, ?i>>)
      ?J ->
        media_range_charset(r, acc, t, s, p,
                              <<v :: binary, ?j>>)
      ?K ->
        media_range_charset(r, acc, t, s, p,
                              <<v :: binary, ?k>>)
      ?L ->
        media_range_charset(r, acc, t, s, p,
                              <<v :: binary, ?l>>)
      ?M ->
        media_range_charset(r, acc, t, s, p,
                              <<v :: binary, ?m>>)
      ?N ->
        media_range_charset(r, acc, t, s, p,
                              <<v :: binary, ?n>>)
      ?O ->
        media_range_charset(r, acc, t, s, p,
                              <<v :: binary, ?o>>)
      ?P ->
        media_range_charset(r, acc, t, s, p,
                              <<v :: binary, ?p>>)
      ?Q ->
        media_range_charset(r, acc, t, s, p,
                              <<v :: binary, ?q>>)
      ?R ->
        media_range_charset(r, acc, t, s, p,
                              <<v :: binary, ?r>>)
      ?S ->
        media_range_charset(r, acc, t, s, p,
                              <<v :: binary, ?s>>)
      ?T ->
        media_range_charset(r, acc, t, s, p,
                              <<v :: binary, ?t>>)
      ?U ->
        media_range_charset(r, acc, t, s, p,
                              <<v :: binary, ?u>>)
      ?V ->
        media_range_charset(r, acc, t, s, p,
                              <<v :: binary, ?v>>)
      ?W ->
        media_range_charset(r, acc, t, s, p,
                              <<v :: binary, ?w>>)
      ?X ->
        media_range_charset(r, acc, t, s, p,
                              <<v :: binary, ?x>>)
      ?Y ->
        media_range_charset(r, acc, t, s, p,
                              <<v :: binary, ?y>>)
      ?Z ->
        media_range_charset(r, acc, t, s, p,
                              <<v :: binary, ?z>>)
      ^c ->
        media_range_charset(r, acc, t, s, p, <<v :: binary, c>>)
    end
  end

  defp media_range_charset(r, acc, t, s, p, v) do
    media_range_param_sep(r, acc, t, s, [{"charset", v} | p])
  end

  defp media_range_param(<<?=, ?", r :: bits>>, acc, t, s, p, k) do
    media_range_quoted(r, acc, t, s, p, k, <<>>)
  end

  defp media_range_param(<<?=, c, r :: bits>>, acc, t, s, p, k)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9) or c === ?!) or c === ?#) or c === ?$) or c === ?%) or c === ?&) or c === ?') or c === ?*) or c === ?+) or c === ?-) or c === ?.) or c === ?^) or c === ?_) or c === ?`) or c === ?|) or c === ?~ do
    media_range_value(r, acc, t, s, p, k, <<c>>)
  end

  defp media_range_param(<<c, r :: bits>>, acc, t, s, p, k)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9) or c === ?!) or c === ?#) or c === ?$) or c === ?%) or c === ?&) or c === ?') or c === ?*) or c === ?+) or c === ?-) or c === ?.) or c === ?^) or c === ?_) or c === ?`) or c === ?|) or c === ?~ do
    case (c) do
      ?A ->
        media_range_param(r, acc, t, s, p, <<k :: binary, ?a>>)
      ?B ->
        media_range_param(r, acc, t, s, p, <<k :: binary, ?b>>)
      ?C ->
        media_range_param(r, acc, t, s, p, <<k :: binary, ?c>>)
      ?D ->
        media_range_param(r, acc, t, s, p, <<k :: binary, ?d>>)
      ?E ->
        media_range_param(r, acc, t, s, p, <<k :: binary, ?e>>)
      ?F ->
        media_range_param(r, acc, t, s, p, <<k :: binary, ?f>>)
      ?G ->
        media_range_param(r, acc, t, s, p, <<k :: binary, ?g>>)
      ?H ->
        media_range_param(r, acc, t, s, p, <<k :: binary, ?h>>)
      ?I ->
        media_range_param(r, acc, t, s, p, <<k :: binary, ?i>>)
      ?J ->
        media_range_param(r, acc, t, s, p, <<k :: binary, ?j>>)
      ?K ->
        media_range_param(r, acc, t, s, p, <<k :: binary, ?k>>)
      ?L ->
        media_range_param(r, acc, t, s, p, <<k :: binary, ?l>>)
      ?M ->
        media_range_param(r, acc, t, s, p, <<k :: binary, ?m>>)
      ?N ->
        media_range_param(r, acc, t, s, p, <<k :: binary, ?n>>)
      ?O ->
        media_range_param(r, acc, t, s, p, <<k :: binary, ?o>>)
      ?P ->
        media_range_param(r, acc, t, s, p, <<k :: binary, ?p>>)
      ?Q ->
        media_range_param(r, acc, t, s, p, <<k :: binary, ?q>>)
      ?R ->
        media_range_param(r, acc, t, s, p, <<k :: binary, ?r>>)
      ?S ->
        media_range_param(r, acc, t, s, p, <<k :: binary, ?s>>)
      ?T ->
        media_range_param(r, acc, t, s, p, <<k :: binary, ?t>>)
      ?U ->
        media_range_param(r, acc, t, s, p, <<k :: binary, ?u>>)
      ?V ->
        media_range_param(r, acc, t, s, p, <<k :: binary, ?v>>)
      ?W ->
        media_range_param(r, acc, t, s, p, <<k :: binary, ?w>>)
      ?X ->
        media_range_param(r, acc, t, s, p, <<k :: binary, ?x>>)
      ?Y ->
        media_range_param(r, acc, t, s, p, <<k :: binary, ?y>>)
      ?Z ->
        media_range_param(r, acc, t, s, p, <<k :: binary, ?z>>)
      ^c ->
        media_range_param(r, acc, t, s, p, <<k :: binary, c>>)
    end
  end

  defp media_range_quoted(<<?", r :: bits>>, acc, t, s, p, k, v) do
    media_range_param_sep(r, acc, t, s, [{k, v} | p])
  end

  defp media_range_quoted(<<?\\, c, r :: bits>>, acc, t, s, p, k, v)
      when c === ?\t or (c > 31 and c !== 127) do
    media_range_quoted(r, acc, t, s, p, k,
                         <<v :: binary, c>>)
  end

  defp media_range_quoted(<<c, r :: bits>>, acc, t, s, p, k, v)
      when c === ?\t or (c > 31 and c !== 127) do
    media_range_quoted(r, acc, t, s, p, k,
                         <<v :: binary, c>>)
  end

  defp media_range_value(<<c, r :: bits>>, acc, t, s, p, k, v)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9) or c === ?!) or c === ?#) or c === ?$) or c === ?%) or c === ?&) or c === ?') or c === ?*) or c === ?+) or c === ?-) or c === ?.) or c === ?^) or c === ?_) or c === ?`) or c === ?|) or c === ?~ do
    media_range_value(r, acc, t, s, p, k,
                        <<v :: binary, c>>)
  end

  defp media_range_value(r, acc, t, s, p, k, v) do
    media_range_param_sep(r, acc, t, s, [{k, v} | p])
  end

  defp media_range_weight(<<"1.000", r :: bits>>, acc, t, s, p) do
    accept_ext_sep(r, acc, t, s, p, 1000, [])
  end

  defp media_range_weight(<<"1.00", r :: bits>>, acc, t, s, p) do
    accept_ext_sep(r, acc, t, s, p, 1000, [])
  end

  defp media_range_weight(<<"1.0", r :: bits>>, acc, t, s, p) do
    accept_ext_sep(r, acc, t, s, p, 1000, [])
  end

  defp media_range_weight(<<"1.", r :: bits>>, acc, t, s, p) do
    accept_ext_sep(r, acc, t, s, p, 1000, [])
  end

  defp media_range_weight(<<"1", r :: bits>>, acc, t, s, p) do
    accept_ext_sep(r, acc, t, s, p, 1000, [])
  end

  defp media_range_weight(<<"0.", a, b, c, r :: bits>>, acc, t, s, p)
      when (((((((((a === ?0 or a === ?1) or a === ?2) or a === ?3) or a === ?4) or a === ?5) or a === ?6) or a === ?7) or a === ?8) or a === ?9 and
              ((((((((b === ?0 or b === ?1) or b === ?2) or b === ?3) or b === ?4) or b === ?5) or b === ?6) or b === ?7) or b === ?8) or b === ?9 and
              ((((((((c === ?0 or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9) do
    accept_ext_sep(r, acc, t, s, p,
                     (a - ?0) * 100 + (b - ?0) * 10 + (c - ?0), [])
  end

  defp media_range_weight(<<"0.", a, b, r :: bits>>, acc, t, s, p)
      when (((((((((a === ?0 or a === ?1) or a === ?2) or a === ?3) or a === ?4) or a === ?5) or a === ?6) or a === ?7) or a === ?8) or a === ?9 and
              ((((((((b === ?0 or b === ?1) or b === ?2) or b === ?3) or b === ?4) or b === ?5) or b === ?6) or b === ?7) or b === ?8) or b === ?9) do
    accept_ext_sep(r, acc, t, s, p,
                     (a - ?0) * 100 + (b - ?0) * 10, [])
  end

  defp media_range_weight(<<"0.", a, r :: bits>>, acc, t, s, p)
      when ((((((((a === ?0 or a === ?1) or a === ?2) or a === ?3) or a === ?4) or a === ?5) or a === ?6) or a === ?7) or a === ?8) or a === ?9 do
    accept_ext_sep(r, acc, t, s, p, (a - ?0) * 100, [])
  end

  defp media_range_weight(<<"0.", r :: bits>>, acc, t, s, p) do
    accept_ext_sep(r, acc, t, s, p, 0, [])
  end

  defp media_range_weight(<<"0", r :: bits>>, acc, t, s, p) do
    accept_ext_sep(r, acc, t, s, p, 0, [])
  end

  defp media_range_weight(<<".", a, b, c, r :: bits>>, acc, t, s, p)
      when (((((((((a === ?0 or a === ?1) or a === ?2) or a === ?3) or a === ?4) or a === ?5) or a === ?6) or a === ?7) or a === ?8) or a === ?9 and
              ((((((((b === ?0 or b === ?1) or b === ?2) or b === ?3) or b === ?4) or b === ?5) or b === ?6) or b === ?7) or b === ?8) or b === ?9 and
              ((((((((c === ?0 or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9) do
    accept_ext_sep(r, acc, t, s, p,
                     (a - ?0) * 100 + (b - ?0) * 10 + (c - ?0), [])
  end

  defp media_range_weight(<<".", a, b, r :: bits>>, acc, t, s, p)
      when (((((((((a === ?0 or a === ?1) or a === ?2) or a === ?3) or a === ?4) or a === ?5) or a === ?6) or a === ?7) or a === ?8) or a === ?9 and
              ((((((((b === ?0 or b === ?1) or b === ?2) or b === ?3) or b === ?4) or b === ?5) or b === ?6) or b === ?7) or b === ?8) or b === ?9) do
    accept_ext_sep(r, acc, t, s, p,
                     (a - ?0) * 100 + (b - ?0) * 10, [])
  end

  defp media_range_weight(<<".", a, r :: bits>>, acc, t, s, p)
      when ((((((((a === ?0 or a === ?1) or a === ?2) or a === ?3) or a === ?4) or a === ?5) or a === ?6) or a === ?7) or a === ?8) or a === ?9 do
    accept_ext_sep(r, acc, t, s, p, (a - ?0) * 100, [])
  end

  defp accept_ext_sep(<<>>, acc, t, s, p, q, e) do
    :lists.reverse([{{t, s, :lists.reverse(p)}, q,
                       :lists.reverse(e)} |
                        acc])
  end

  defp accept_ext_sep(<<?,, r :: bits>>, acc, t, s, p, q, e) do
    media_range_list(r,
                       [{{t, s, :lists.reverse(p)}, q, :lists.reverse(e)} |
                            acc])
  end

  defp accept_ext_sep(<<?;, r :: bits>>, acc, t, s, p, q, e) do
    accept_before_ext(r, acc, t, s, p, q, e)
  end

  defp accept_ext_sep(<<c, r :: bits>>, acc, t, s, p, q, e)
      when c === ?\s or c === ?\t do
    accept_ext_sep(r, acc, t, s, p, q, e)
  end

  defp accept_before_ext(<<c, r :: bits>>, acc, t, s, p, q, e)
      when c === ?\s or c === ?\t do
    accept_before_ext(r, acc, t, s, p, q, e)
  end

  defp accept_before_ext(<<c, r :: bits>>, acc, t, s, p, q, e)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9) or c === ?!) or c === ?#) or c === ?$) or c === ?%) or c === ?&) or c === ?') or c === ?*) or c === ?+) or c === ?-) or c === ?.) or c === ?^) or c === ?_) or c === ?`) or c === ?|) or c === ?~ do
    case (c) do
      ?A ->
        accept_ext(r, acc, t, s, p, q, e,
                     <<<<>> :: binary, ?a>>)
      ?B ->
        accept_ext(r, acc, t, s, p, q, e,
                     <<<<>> :: binary, ?b>>)
      ?C ->
        accept_ext(r, acc, t, s, p, q, e,
                     <<<<>> :: binary, ?c>>)
      ?D ->
        accept_ext(r, acc, t, s, p, q, e,
                     <<<<>> :: binary, ?d>>)
      ?E ->
        accept_ext(r, acc, t, s, p, q, e,
                     <<<<>> :: binary, ?e>>)
      ?F ->
        accept_ext(r, acc, t, s, p, q, e,
                     <<<<>> :: binary, ?f>>)
      ?G ->
        accept_ext(r, acc, t, s, p, q, e,
                     <<<<>> :: binary, ?g>>)
      ?H ->
        accept_ext(r, acc, t, s, p, q, e,
                     <<<<>> :: binary, ?h>>)
      ?I ->
        accept_ext(r, acc, t, s, p, q, e,
                     <<<<>> :: binary, ?i>>)
      ?J ->
        accept_ext(r, acc, t, s, p, q, e,
                     <<<<>> :: binary, ?j>>)
      ?K ->
        accept_ext(r, acc, t, s, p, q, e,
                     <<<<>> :: binary, ?k>>)
      ?L ->
        accept_ext(r, acc, t, s, p, q, e,
                     <<<<>> :: binary, ?l>>)
      ?M ->
        accept_ext(r, acc, t, s, p, q, e,
                     <<<<>> :: binary, ?m>>)
      ?N ->
        accept_ext(r, acc, t, s, p, q, e,
                     <<<<>> :: binary, ?n>>)
      ?O ->
        accept_ext(r, acc, t, s, p, q, e,
                     <<<<>> :: binary, ?o>>)
      ?P ->
        accept_ext(r, acc, t, s, p, q, e,
                     <<<<>> :: binary, ?p>>)
      ?Q ->
        accept_ext(r, acc, t, s, p, q, e,
                     <<<<>> :: binary, ?q>>)
      ?R ->
        accept_ext(r, acc, t, s, p, q, e,
                     <<<<>> :: binary, ?r>>)
      ?S ->
        accept_ext(r, acc, t, s, p, q, e,
                     <<<<>> :: binary, ?s>>)
      ?T ->
        accept_ext(r, acc, t, s, p, q, e,
                     <<<<>> :: binary, ?t>>)
      ?U ->
        accept_ext(r, acc, t, s, p, q, e,
                     <<<<>> :: binary, ?u>>)
      ?V ->
        accept_ext(r, acc, t, s, p, q, e,
                     <<<<>> :: binary, ?v>>)
      ?W ->
        accept_ext(r, acc, t, s, p, q, e,
                     <<<<>> :: binary, ?w>>)
      ?X ->
        accept_ext(r, acc, t, s, p, q, e,
                     <<<<>> :: binary, ?x>>)
      ?Y ->
        accept_ext(r, acc, t, s, p, q, e,
                     <<<<>> :: binary, ?y>>)
      ?Z ->
        accept_ext(r, acc, t, s, p, q, e,
                     <<<<>> :: binary, ?z>>)
      ^c ->
        accept_ext(r, acc, t, s, p, q, e, <<<<>> :: binary, c>>)
    end
  end

  defp accept_ext(<<?=, ?", r :: bits>>, acc, t, s, p, q, e, k) do
    accept_quoted(r, acc, t, s, p, q, e, k, <<>>)
  end

  defp accept_ext(<<?=, c, r :: bits>>, acc, t, s, p, q, e, k)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9) or c === ?!) or c === ?#) or c === ?$) or c === ?%) or c === ?&) or c === ?') or c === ?*) or c === ?+) or c === ?-) or c === ?.) or c === ?^) or c === ?_) or c === ?`) or c === ?|) or c === ?~ do
    accept_value(r, acc, t, s, p, q, e, k, <<c>>)
  end

  defp accept_ext(<<c, r :: bits>>, acc, t, s, p, q, e, k)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9) or c === ?!) or c === ?#) or c === ?$) or c === ?%) or c === ?&) or c === ?') or c === ?*) or c === ?+) or c === ?-) or c === ?.) or c === ?^) or c === ?_) or c === ?`) or c === ?|) or c === ?~ do
    case (c) do
      ?A ->
        accept_ext(r, acc, t, s, p, q, e, <<k :: binary, ?a>>)
      ?B ->
        accept_ext(r, acc, t, s, p, q, e, <<k :: binary, ?b>>)
      ?C ->
        accept_ext(r, acc, t, s, p, q, e, <<k :: binary, ?c>>)
      ?D ->
        accept_ext(r, acc, t, s, p, q, e, <<k :: binary, ?d>>)
      ?E ->
        accept_ext(r, acc, t, s, p, q, e, <<k :: binary, ?e>>)
      ?F ->
        accept_ext(r, acc, t, s, p, q, e, <<k :: binary, ?f>>)
      ?G ->
        accept_ext(r, acc, t, s, p, q, e, <<k :: binary, ?g>>)
      ?H ->
        accept_ext(r, acc, t, s, p, q, e, <<k :: binary, ?h>>)
      ?I ->
        accept_ext(r, acc, t, s, p, q, e, <<k :: binary, ?i>>)
      ?J ->
        accept_ext(r, acc, t, s, p, q, e, <<k :: binary, ?j>>)
      ?K ->
        accept_ext(r, acc, t, s, p, q, e, <<k :: binary, ?k>>)
      ?L ->
        accept_ext(r, acc, t, s, p, q, e, <<k :: binary, ?l>>)
      ?M ->
        accept_ext(r, acc, t, s, p, q, e, <<k :: binary, ?m>>)
      ?N ->
        accept_ext(r, acc, t, s, p, q, e, <<k :: binary, ?n>>)
      ?O ->
        accept_ext(r, acc, t, s, p, q, e, <<k :: binary, ?o>>)
      ?P ->
        accept_ext(r, acc, t, s, p, q, e, <<k :: binary, ?p>>)
      ?Q ->
        accept_ext(r, acc, t, s, p, q, e, <<k :: binary, ?q>>)
      ?R ->
        accept_ext(r, acc, t, s, p, q, e, <<k :: binary, ?r>>)
      ?S ->
        accept_ext(r, acc, t, s, p, q, e, <<k :: binary, ?s>>)
      ?T ->
        accept_ext(r, acc, t, s, p, q, e, <<k :: binary, ?t>>)
      ?U ->
        accept_ext(r, acc, t, s, p, q, e, <<k :: binary, ?u>>)
      ?V ->
        accept_ext(r, acc, t, s, p, q, e, <<k :: binary, ?v>>)
      ?W ->
        accept_ext(r, acc, t, s, p, q, e, <<k :: binary, ?w>>)
      ?X ->
        accept_ext(r, acc, t, s, p, q, e, <<k :: binary, ?x>>)
      ?Y ->
        accept_ext(r, acc, t, s, p, q, e, <<k :: binary, ?y>>)
      ?Z ->
        accept_ext(r, acc, t, s, p, q, e, <<k :: binary, ?z>>)
      ^c ->
        accept_ext(r, acc, t, s, p, q, e, <<k :: binary, c>>)
    end
  end

  defp accept_ext(r, acc, t, s, p, q, e, k) do
    accept_ext_sep(r, acc, t, s, p, q, [k | e])
  end

  defp accept_quoted(<<?", r :: bits>>, acc, t, s, p, q, e, k, v) do
    accept_ext_sep(r, acc, t, s, p, q, [{k, v} | e])
  end

  defp accept_quoted(<<?\\, c, r :: bits>>, acc, t, s, p, q, e, k, v)
      when c === ?\t or (c > 31 and c !== 127) do
    accept_quoted(r, acc, t, s, p, q, e, k,
                    <<v :: binary, c>>)
  end

  defp accept_quoted(<<c, r :: bits>>, acc, t, s, p, q, e, k, v)
      when c === ?\t or (c > 31 and c !== 127) do
    accept_quoted(r, acc, t, s, p, q, e, k,
                    <<v :: binary, c>>)
  end

  defp accept_value(<<c, r :: bits>>, acc, t, s, p, q, e, k, v)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9) or c === ?!) or c === ?#) or c === ?$) or c === ?%) or c === ?&) or c === ?') or c === ?*) or c === ?+) or c === ?-) or c === ?.) or c === ?^) or c === ?_) or c === ?`) or c === ?|) or c === ?~ do
    accept_value(r, acc, t, s, p, q, e, k,
                   <<v :: binary, c>>)
  end

  defp accept_value(r, acc, t, s, p, q, e, k, v) do
    accept_ext_sep(r, acc, t, s, p, q, [{k, v} | e])
  end

  def parse_accept_charset(charset) do
    nonempty(conneg_list(charset, []))
  end

  defp conneg_list(<<>>, acc) do
    :lists.reverse(acc)
  end

  defp conneg_list(<<c, r :: bits>>, acc)
      when (c === ?\s or c === ?\t) or c === ?, do
    conneg_list(r, acc)
  end

  defp conneg_list(<<c, r :: bits>>, acc)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9) or c === ?!) or c === ?#) or c === ?$) or c === ?%) or c === ?&) or c === ?') or c === ?*) or c === ?+) or c === ?-) or c === ?.) or c === ?^) or c === ?_) or c === ?`) or c === ?|) or c === ?~ do
    case (c) do
      ?A ->
        conneg(r, acc, <<<<>> :: binary, ?a>>)
      ?B ->
        conneg(r, acc, <<<<>> :: binary, ?b>>)
      ?C ->
        conneg(r, acc, <<<<>> :: binary, ?c>>)
      ?D ->
        conneg(r, acc, <<<<>> :: binary, ?d>>)
      ?E ->
        conneg(r, acc, <<<<>> :: binary, ?e>>)
      ?F ->
        conneg(r, acc, <<<<>> :: binary, ?f>>)
      ?G ->
        conneg(r, acc, <<<<>> :: binary, ?g>>)
      ?H ->
        conneg(r, acc, <<<<>> :: binary, ?h>>)
      ?I ->
        conneg(r, acc, <<<<>> :: binary, ?i>>)
      ?J ->
        conneg(r, acc, <<<<>> :: binary, ?j>>)
      ?K ->
        conneg(r, acc, <<<<>> :: binary, ?k>>)
      ?L ->
        conneg(r, acc, <<<<>> :: binary, ?l>>)
      ?M ->
        conneg(r, acc, <<<<>> :: binary, ?m>>)
      ?N ->
        conneg(r, acc, <<<<>> :: binary, ?n>>)
      ?O ->
        conneg(r, acc, <<<<>> :: binary, ?o>>)
      ?P ->
        conneg(r, acc, <<<<>> :: binary, ?p>>)
      ?Q ->
        conneg(r, acc, <<<<>> :: binary, ?q>>)
      ?R ->
        conneg(r, acc, <<<<>> :: binary, ?r>>)
      ?S ->
        conneg(r, acc, <<<<>> :: binary, ?s>>)
      ?T ->
        conneg(r, acc, <<<<>> :: binary, ?t>>)
      ?U ->
        conneg(r, acc, <<<<>> :: binary, ?u>>)
      ?V ->
        conneg(r, acc, <<<<>> :: binary, ?v>>)
      ?W ->
        conneg(r, acc, <<<<>> :: binary, ?w>>)
      ?X ->
        conneg(r, acc, <<<<>> :: binary, ?x>>)
      ?Y ->
        conneg(r, acc, <<<<>> :: binary, ?y>>)
      ?Z ->
        conneg(r, acc, <<<<>> :: binary, ?z>>)
      ^c ->
        conneg(r, acc, <<<<>> :: binary, c>>)
    end
  end

  defp conneg(<<c, r :: bits>>, acc, t)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9) or c === ?!) or c === ?#) or c === ?$) or c === ?%) or c === ?&) or c === ?') or c === ?*) or c === ?+) or c === ?-) or c === ?.) or c === ?^) or c === ?_) or c === ?`) or c === ?|) or c === ?~ do
    case (c) do
      ?A ->
        conneg(r, acc, <<t :: binary, ?a>>)
      ?B ->
        conneg(r, acc, <<t :: binary, ?b>>)
      ?C ->
        conneg(r, acc, <<t :: binary, ?c>>)
      ?D ->
        conneg(r, acc, <<t :: binary, ?d>>)
      ?E ->
        conneg(r, acc, <<t :: binary, ?e>>)
      ?F ->
        conneg(r, acc, <<t :: binary, ?f>>)
      ?G ->
        conneg(r, acc, <<t :: binary, ?g>>)
      ?H ->
        conneg(r, acc, <<t :: binary, ?h>>)
      ?I ->
        conneg(r, acc, <<t :: binary, ?i>>)
      ?J ->
        conneg(r, acc, <<t :: binary, ?j>>)
      ?K ->
        conneg(r, acc, <<t :: binary, ?k>>)
      ?L ->
        conneg(r, acc, <<t :: binary, ?l>>)
      ?M ->
        conneg(r, acc, <<t :: binary, ?m>>)
      ?N ->
        conneg(r, acc, <<t :: binary, ?n>>)
      ?O ->
        conneg(r, acc, <<t :: binary, ?o>>)
      ?P ->
        conneg(r, acc, <<t :: binary, ?p>>)
      ?Q ->
        conneg(r, acc, <<t :: binary, ?q>>)
      ?R ->
        conneg(r, acc, <<t :: binary, ?r>>)
      ?S ->
        conneg(r, acc, <<t :: binary, ?s>>)
      ?T ->
        conneg(r, acc, <<t :: binary, ?t>>)
      ?U ->
        conneg(r, acc, <<t :: binary, ?u>>)
      ?V ->
        conneg(r, acc, <<t :: binary, ?v>>)
      ?W ->
        conneg(r, acc, <<t :: binary, ?w>>)
      ?X ->
        conneg(r, acc, <<t :: binary, ?x>>)
      ?Y ->
        conneg(r, acc, <<t :: binary, ?y>>)
      ?Z ->
        conneg(r, acc, <<t :: binary, ?z>>)
      ^c ->
        conneg(r, acc, <<t :: binary, c>>)
    end
  end

  defp conneg(r, acc, t) do
    conneg_param_sep(r, acc, t)
  end

  defp conneg_param_sep(<<>>, acc, t) do
    :lists.reverse([{t, 1000} | acc])
  end

  defp conneg_param_sep(<<?,, r :: bits>>, acc, t) do
    conneg_list(r, [{t, 1000} | acc])
  end

  defp conneg_param_sep(<<?;, r :: bits>>, acc, t) do
    conneg_before_weight(r, acc, t)
  end

  defp conneg_param_sep(<<c, r :: bits>>, acc, t)
      when c === ?\s or c === ?\t do
    conneg_param_sep(r, acc, t)
  end

  defp conneg_before_weight(<<c, r :: bits>>, acc, t)
      when c === ?\s or c === ?\t do
    conneg_before_weight(r, acc, t)
  end

  defp conneg_before_weight(<<?q, ?=, r :: bits>>, acc, t) do
    conneg_weight(r, acc, t)
  end

  defp conneg_before_weight(<<c, r :: bits>>, acc, t)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9) or c === ?!) or c === ?#) or c === ?$) or c === ?%) or c === ?&) or c === ?') or c === ?*) or c === ?+) or c === ?-) or c === ?.) or c === ?^) or c === ?_) or c === ?`) or c === ?|) or c === ?~ do
    case (c) do
      ?A ->
        conneg(r, [{t, 1000} | acc], <<<<>> :: binary, ?a>>)
      ?B ->
        conneg(r, [{t, 1000} | acc], <<<<>> :: binary, ?b>>)
      ?C ->
        conneg(r, [{t, 1000} | acc], <<<<>> :: binary, ?c>>)
      ?D ->
        conneg(r, [{t, 1000} | acc], <<<<>> :: binary, ?d>>)
      ?E ->
        conneg(r, [{t, 1000} | acc], <<<<>> :: binary, ?e>>)
      ?F ->
        conneg(r, [{t, 1000} | acc], <<<<>> :: binary, ?f>>)
      ?G ->
        conneg(r, [{t, 1000} | acc], <<<<>> :: binary, ?g>>)
      ?H ->
        conneg(r, [{t, 1000} | acc], <<<<>> :: binary, ?h>>)
      ?I ->
        conneg(r, [{t, 1000} | acc], <<<<>> :: binary, ?i>>)
      ?J ->
        conneg(r, [{t, 1000} | acc], <<<<>> :: binary, ?j>>)
      ?K ->
        conneg(r, [{t, 1000} | acc], <<<<>> :: binary, ?k>>)
      ?L ->
        conneg(r, [{t, 1000} | acc], <<<<>> :: binary, ?l>>)
      ?M ->
        conneg(r, [{t, 1000} | acc], <<<<>> :: binary, ?m>>)
      ?N ->
        conneg(r, [{t, 1000} | acc], <<<<>> :: binary, ?n>>)
      ?O ->
        conneg(r, [{t, 1000} | acc], <<<<>> :: binary, ?o>>)
      ?P ->
        conneg(r, [{t, 1000} | acc], <<<<>> :: binary, ?p>>)
      ?Q ->
        conneg(r, [{t, 1000} | acc], <<<<>> :: binary, ?q>>)
      ?R ->
        conneg(r, [{t, 1000} | acc], <<<<>> :: binary, ?r>>)
      ?S ->
        conneg(r, [{t, 1000} | acc], <<<<>> :: binary, ?s>>)
      ?T ->
        conneg(r, [{t, 1000} | acc], <<<<>> :: binary, ?t>>)
      ?U ->
        conneg(r, [{t, 1000} | acc], <<<<>> :: binary, ?u>>)
      ?V ->
        conneg(r, [{t, 1000} | acc], <<<<>> :: binary, ?v>>)
      ?W ->
        conneg(r, [{t, 1000} | acc], <<<<>> :: binary, ?w>>)
      ?X ->
        conneg(r, [{t, 1000} | acc], <<<<>> :: binary, ?x>>)
      ?Y ->
        conneg(r, [{t, 1000} | acc], <<<<>> :: binary, ?y>>)
      ?Z ->
        conneg(r, [{t, 1000} | acc], <<<<>> :: binary, ?z>>)
      ^c ->
        conneg(r, [{t, 1000} | acc], <<<<>> :: binary, c>>)
    end
  end

  defp conneg_weight(<<"1.000", r :: bits>>, acc, t) do
    conneg_list_sep(r, [{t, 1000} | acc])
  end

  defp conneg_weight(<<"1.00", r :: bits>>, acc, t) do
    conneg_list_sep(r, [{t, 1000} | acc])
  end

  defp conneg_weight(<<"1.0", r :: bits>>, acc, t) do
    conneg_list_sep(r, [{t, 1000} | acc])
  end

  defp conneg_weight(<<"1.", r :: bits>>, acc, t) do
    conneg_list_sep(r, [{t, 1000} | acc])
  end

  defp conneg_weight(<<"1", r :: bits>>, acc, t) do
    conneg_list_sep(r, [{t, 1000} | acc])
  end

  defp conneg_weight(<<"0.", a, b, c, r :: bits>>, acc, t)
      when (((((((((a === ?0 or a === ?1) or a === ?2) or a === ?3) or a === ?4) or a === ?5) or a === ?6) or a === ?7) or a === ?8) or a === ?9 and
              ((((((((b === ?0 or b === ?1) or b === ?2) or b === ?3) or b === ?4) or b === ?5) or b === ?6) or b === ?7) or b === ?8) or b === ?9 and
              ((((((((c === ?0 or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9) do
    conneg_list_sep(r,
                      [{t, (a - ?0) * 100 + (b - ?0) * 10 + (c - ?0)} | acc])
  end

  defp conneg_weight(<<"0.", a, b, r :: bits>>, acc, t)
      when (((((((((a === ?0 or a === ?1) or a === ?2) or a === ?3) or a === ?4) or a === ?5) or a === ?6) or a === ?7) or a === ?8) or a === ?9 and
              ((((((((b === ?0 or b === ?1) or b === ?2) or b === ?3) or b === ?4) or b === ?5) or b === ?6) or b === ?7) or b === ?8) or b === ?9) do
    conneg_list_sep(r,
                      [{t, (a - ?0) * 100 + (b - ?0) * 10} | acc])
  end

  defp conneg_weight(<<"0.", a, r :: bits>>, acc, t)
      when ((((((((a === ?0 or a === ?1) or a === ?2) or a === ?3) or a === ?4) or a === ?5) or a === ?6) or a === ?7) or a === ?8) or a === ?9 do
    conneg_list_sep(r, [{t, (a - ?0) * 100} | acc])
  end

  defp conneg_weight(<<"0.", r :: bits>>, acc, t) do
    conneg_list_sep(r, [{t, 0} | acc])
  end

  defp conneg_weight(<<"0", r :: bits>>, acc, t) do
    conneg_list_sep(r, [{t, 0} | acc])
  end

  defp conneg_list_sep(<<>>, acc) do
    :lists.reverse(acc)
  end

  defp conneg_list_sep(<<c, r :: bits>>, acc)
      when c === ?\s or c === ?\t do
    conneg_list_sep(r, acc)
  end

  defp conneg_list_sep(<<?,, r :: bits>>, acc) do
    conneg_list(r, acc)
  end

  def parse_accept_encoding(encoding) do
    conneg_list(encoding, [])
  end

  def parse_accept_language(languageRange) do
    nonempty(language_range_list(languageRange, []))
  end

  defp language_range_list(<<>>, acc) do
    :lists.reverse(acc)
  end

  defp language_range_list(<<c, r :: bits>>, acc)
      when (c === ?\s or c === ?\t) or c === ?, do
    language_range_list(r, acc)
  end

  defp language_range_list(<<?*, r :: bits>>, acc) do
    language_range_param_sep(r, acc, "*")
  end

  defp language_range_list(<<c, r :: bits>>, acc)
      when ((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z do
    case (c) do
      ?A ->
        language_range(r, acc, 1, <<<<>> :: binary, ?a>>)
      ?B ->
        language_range(r, acc, 1, <<<<>> :: binary, ?b>>)
      ?C ->
        language_range(r, acc, 1, <<<<>> :: binary, ?c>>)
      ?D ->
        language_range(r, acc, 1, <<<<>> :: binary, ?d>>)
      ?E ->
        language_range(r, acc, 1, <<<<>> :: binary, ?e>>)
      ?F ->
        language_range(r, acc, 1, <<<<>> :: binary, ?f>>)
      ?G ->
        language_range(r, acc, 1, <<<<>> :: binary, ?g>>)
      ?H ->
        language_range(r, acc, 1, <<<<>> :: binary, ?h>>)
      ?I ->
        language_range(r, acc, 1, <<<<>> :: binary, ?i>>)
      ?J ->
        language_range(r, acc, 1, <<<<>> :: binary, ?j>>)
      ?K ->
        language_range(r, acc, 1, <<<<>> :: binary, ?k>>)
      ?L ->
        language_range(r, acc, 1, <<<<>> :: binary, ?l>>)
      ?M ->
        language_range(r, acc, 1, <<<<>> :: binary, ?m>>)
      ?N ->
        language_range(r, acc, 1, <<<<>> :: binary, ?n>>)
      ?O ->
        language_range(r, acc, 1, <<<<>> :: binary, ?o>>)
      ?P ->
        language_range(r, acc, 1, <<<<>> :: binary, ?p>>)
      ?Q ->
        language_range(r, acc, 1, <<<<>> :: binary, ?q>>)
      ?R ->
        language_range(r, acc, 1, <<<<>> :: binary, ?r>>)
      ?S ->
        language_range(r, acc, 1, <<<<>> :: binary, ?s>>)
      ?T ->
        language_range(r, acc, 1, <<<<>> :: binary, ?t>>)
      ?U ->
        language_range(r, acc, 1, <<<<>> :: binary, ?u>>)
      ?V ->
        language_range(r, acc, 1, <<<<>> :: binary, ?v>>)
      ?W ->
        language_range(r, acc, 1, <<<<>> :: binary, ?w>>)
      ?X ->
        language_range(r, acc, 1, <<<<>> :: binary, ?x>>)
      ?Y ->
        language_range(r, acc, 1, <<<<>> :: binary, ?y>>)
      ?Z ->
        language_range(r, acc, 1, <<<<>> :: binary, ?z>>)
      ^c ->
        language_range(r, acc, 1, <<<<>> :: binary, c>>)
    end
  end

  defp language_range(<<?-, c, r :: bits>>, acc, _, t)
      when ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9 do
    case (c) do
      ?A ->
        language_range_sub(r, acc, 1,
                             <<<<t :: binary, ?->> :: binary, ?a>>)
      ?B ->
        language_range_sub(r, acc, 1,
                             <<<<t :: binary, ?->> :: binary, ?b>>)
      ?C ->
        language_range_sub(r, acc, 1,
                             <<<<t :: binary, ?->> :: binary, ?c>>)
      ?D ->
        language_range_sub(r, acc, 1,
                             <<<<t :: binary, ?->> :: binary, ?d>>)
      ?E ->
        language_range_sub(r, acc, 1,
                             <<<<t :: binary, ?->> :: binary, ?e>>)
      ?F ->
        language_range_sub(r, acc, 1,
                             <<<<t :: binary, ?->> :: binary, ?f>>)
      ?G ->
        language_range_sub(r, acc, 1,
                             <<<<t :: binary, ?->> :: binary, ?g>>)
      ?H ->
        language_range_sub(r, acc, 1,
                             <<<<t :: binary, ?->> :: binary, ?h>>)
      ?I ->
        language_range_sub(r, acc, 1,
                             <<<<t :: binary, ?->> :: binary, ?i>>)
      ?J ->
        language_range_sub(r, acc, 1,
                             <<<<t :: binary, ?->> :: binary, ?j>>)
      ?K ->
        language_range_sub(r, acc, 1,
                             <<<<t :: binary, ?->> :: binary, ?k>>)
      ?L ->
        language_range_sub(r, acc, 1,
                             <<<<t :: binary, ?->> :: binary, ?l>>)
      ?M ->
        language_range_sub(r, acc, 1,
                             <<<<t :: binary, ?->> :: binary, ?m>>)
      ?N ->
        language_range_sub(r, acc, 1,
                             <<<<t :: binary, ?->> :: binary, ?n>>)
      ?O ->
        language_range_sub(r, acc, 1,
                             <<<<t :: binary, ?->> :: binary, ?o>>)
      ?P ->
        language_range_sub(r, acc, 1,
                             <<<<t :: binary, ?->> :: binary, ?p>>)
      ?Q ->
        language_range_sub(r, acc, 1,
                             <<<<t :: binary, ?->> :: binary, ?q>>)
      ?R ->
        language_range_sub(r, acc, 1,
                             <<<<t :: binary, ?->> :: binary, ?r>>)
      ?S ->
        language_range_sub(r, acc, 1,
                             <<<<t :: binary, ?->> :: binary, ?s>>)
      ?T ->
        language_range_sub(r, acc, 1,
                             <<<<t :: binary, ?->> :: binary, ?t>>)
      ?U ->
        language_range_sub(r, acc, 1,
                             <<<<t :: binary, ?->> :: binary, ?u>>)
      ?V ->
        language_range_sub(r, acc, 1,
                             <<<<t :: binary, ?->> :: binary, ?v>>)
      ?W ->
        language_range_sub(r, acc, 1,
                             <<<<t :: binary, ?->> :: binary, ?w>>)
      ?X ->
        language_range_sub(r, acc, 1,
                             <<<<t :: binary, ?->> :: binary, ?x>>)
      ?Y ->
        language_range_sub(r, acc, 1,
                             <<<<t :: binary, ?->> :: binary, ?y>>)
      ?Z ->
        language_range_sub(r, acc, 1,
                             <<<<t :: binary, ?->> :: binary, ?z>>)
      ^c ->
        language_range_sub(r, acc, 1,
                             <<<<t :: binary, ?->> :: binary, c>>)
    end
  end

  defp language_range(<<c, r :: bits>>, acc, n, t)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z and
              n < 8) do
    case (c) do
      ?A ->
        language_range(r, acc, n + 1, <<t :: binary, ?a>>)
      ?B ->
        language_range(r, acc, n + 1, <<t :: binary, ?b>>)
      ?C ->
        language_range(r, acc, n + 1, <<t :: binary, ?c>>)
      ?D ->
        language_range(r, acc, n + 1, <<t :: binary, ?d>>)
      ?E ->
        language_range(r, acc, n + 1, <<t :: binary, ?e>>)
      ?F ->
        language_range(r, acc, n + 1, <<t :: binary, ?f>>)
      ?G ->
        language_range(r, acc, n + 1, <<t :: binary, ?g>>)
      ?H ->
        language_range(r, acc, n + 1, <<t :: binary, ?h>>)
      ?I ->
        language_range(r, acc, n + 1, <<t :: binary, ?i>>)
      ?J ->
        language_range(r, acc, n + 1, <<t :: binary, ?j>>)
      ?K ->
        language_range(r, acc, n + 1, <<t :: binary, ?k>>)
      ?L ->
        language_range(r, acc, n + 1, <<t :: binary, ?l>>)
      ?M ->
        language_range(r, acc, n + 1, <<t :: binary, ?m>>)
      ?N ->
        language_range(r, acc, n + 1, <<t :: binary, ?n>>)
      ?O ->
        language_range(r, acc, n + 1, <<t :: binary, ?o>>)
      ?P ->
        language_range(r, acc, n + 1, <<t :: binary, ?p>>)
      ?Q ->
        language_range(r, acc, n + 1, <<t :: binary, ?q>>)
      ?R ->
        language_range(r, acc, n + 1, <<t :: binary, ?r>>)
      ?S ->
        language_range(r, acc, n + 1, <<t :: binary, ?s>>)
      ?T ->
        language_range(r, acc, n + 1, <<t :: binary, ?t>>)
      ?U ->
        language_range(r, acc, n + 1, <<t :: binary, ?u>>)
      ?V ->
        language_range(r, acc, n + 1, <<t :: binary, ?v>>)
      ?W ->
        language_range(r, acc, n + 1, <<t :: binary, ?w>>)
      ?X ->
        language_range(r, acc, n + 1, <<t :: binary, ?x>>)
      ?Y ->
        language_range(r, acc, n + 1, <<t :: binary, ?y>>)
      ?Z ->
        language_range(r, acc, n + 1, <<t :: binary, ?z>>)
      ^c ->
        language_range(r, acc, n + 1, <<t :: binary, c>>)
    end
  end

  defp language_range(r, acc, _, t) do
    language_range_param_sep(r, acc, t)
  end

  defp language_range_sub(<<?-, r :: bits>>, acc, _, t) do
    language_range_sub(r, acc, 0, <<t :: binary, ?->>)
  end

  defp language_range_sub(<<c, r :: bits>>, acc, n, t)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9 and
              n < 8) do
    case (c) do
      ?A ->
        language_range_sub(r, acc, n + 1, <<t :: binary, ?a>>)
      ?B ->
        language_range_sub(r, acc, n + 1, <<t :: binary, ?b>>)
      ?C ->
        language_range_sub(r, acc, n + 1, <<t :: binary, ?c>>)
      ?D ->
        language_range_sub(r, acc, n + 1, <<t :: binary, ?d>>)
      ?E ->
        language_range_sub(r, acc, n + 1, <<t :: binary, ?e>>)
      ?F ->
        language_range_sub(r, acc, n + 1, <<t :: binary, ?f>>)
      ?G ->
        language_range_sub(r, acc, n + 1, <<t :: binary, ?g>>)
      ?H ->
        language_range_sub(r, acc, n + 1, <<t :: binary, ?h>>)
      ?I ->
        language_range_sub(r, acc, n + 1, <<t :: binary, ?i>>)
      ?J ->
        language_range_sub(r, acc, n + 1, <<t :: binary, ?j>>)
      ?K ->
        language_range_sub(r, acc, n + 1, <<t :: binary, ?k>>)
      ?L ->
        language_range_sub(r, acc, n + 1, <<t :: binary, ?l>>)
      ?M ->
        language_range_sub(r, acc, n + 1, <<t :: binary, ?m>>)
      ?N ->
        language_range_sub(r, acc, n + 1, <<t :: binary, ?n>>)
      ?O ->
        language_range_sub(r, acc, n + 1, <<t :: binary, ?o>>)
      ?P ->
        language_range_sub(r, acc, n + 1, <<t :: binary, ?p>>)
      ?Q ->
        language_range_sub(r, acc, n + 1, <<t :: binary, ?q>>)
      ?R ->
        language_range_sub(r, acc, n + 1, <<t :: binary, ?r>>)
      ?S ->
        language_range_sub(r, acc, n + 1, <<t :: binary, ?s>>)
      ?T ->
        language_range_sub(r, acc, n + 1, <<t :: binary, ?t>>)
      ?U ->
        language_range_sub(r, acc, n + 1, <<t :: binary, ?u>>)
      ?V ->
        language_range_sub(r, acc, n + 1, <<t :: binary, ?v>>)
      ?W ->
        language_range_sub(r, acc, n + 1, <<t :: binary, ?w>>)
      ?X ->
        language_range_sub(r, acc, n + 1, <<t :: binary, ?x>>)
      ?Y ->
        language_range_sub(r, acc, n + 1, <<t :: binary, ?y>>)
      ?Z ->
        language_range_sub(r, acc, n + 1, <<t :: binary, ?z>>)
      ^c ->
        language_range_sub(r, acc, n + 1, <<t :: binary, c>>)
    end
  end

  defp language_range_sub(r, acc, _, t) do
    language_range_param_sep(r, acc, t)
  end

  defp language_range_param_sep(<<>>, acc, t) do
    :lists.reverse([{t, 1000} | acc])
  end

  defp language_range_param_sep(<<?,, r :: bits>>, acc, t) do
    language_range_list(r, [{t, 1000} | acc])
  end

  defp language_range_param_sep(<<?;, r :: bits>>, acc, t) do
    language_range_before_weight(r, acc, t)
  end

  defp language_range_param_sep(<<c, r :: bits>>, acc, t)
      when c === ?\s or c === ?\t do
    language_range_param_sep(r, acc, t)
  end

  defp language_range_before_weight(<<c, r :: bits>>, acc, t)
      when c === ?\s or c === ?\t do
    language_range_before_weight(r, acc, t)
  end

  defp language_range_before_weight(<<?q, ?=, r :: bits>>, acc, t) do
    language_range_weight(r, acc, t)
  end

  defp language_range_before_weight(<<c, r :: bits>>, acc, t)
      when ((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z do
    case (c) do
      ?A ->
        language_range(r, [{t, 1000} | acc], 1,
                         <<<<>> :: binary, ?a>>)
      ?B ->
        language_range(r, [{t, 1000} | acc], 1,
                         <<<<>> :: binary, ?b>>)
      ?C ->
        language_range(r, [{t, 1000} | acc], 1,
                         <<<<>> :: binary, ?c>>)
      ?D ->
        language_range(r, [{t, 1000} | acc], 1,
                         <<<<>> :: binary, ?d>>)
      ?E ->
        language_range(r, [{t, 1000} | acc], 1,
                         <<<<>> :: binary, ?e>>)
      ?F ->
        language_range(r, [{t, 1000} | acc], 1,
                         <<<<>> :: binary, ?f>>)
      ?G ->
        language_range(r, [{t, 1000} | acc], 1,
                         <<<<>> :: binary, ?g>>)
      ?H ->
        language_range(r, [{t, 1000} | acc], 1,
                         <<<<>> :: binary, ?h>>)
      ?I ->
        language_range(r, [{t, 1000} | acc], 1,
                         <<<<>> :: binary, ?i>>)
      ?J ->
        language_range(r, [{t, 1000} | acc], 1,
                         <<<<>> :: binary, ?j>>)
      ?K ->
        language_range(r, [{t, 1000} | acc], 1,
                         <<<<>> :: binary, ?k>>)
      ?L ->
        language_range(r, [{t, 1000} | acc], 1,
                         <<<<>> :: binary, ?l>>)
      ?M ->
        language_range(r, [{t, 1000} | acc], 1,
                         <<<<>> :: binary, ?m>>)
      ?N ->
        language_range(r, [{t, 1000} | acc], 1,
                         <<<<>> :: binary, ?n>>)
      ?O ->
        language_range(r, [{t, 1000} | acc], 1,
                         <<<<>> :: binary, ?o>>)
      ?P ->
        language_range(r, [{t, 1000} | acc], 1,
                         <<<<>> :: binary, ?p>>)
      ?Q ->
        language_range(r, [{t, 1000} | acc], 1,
                         <<<<>> :: binary, ?q>>)
      ?R ->
        language_range(r, [{t, 1000} | acc], 1,
                         <<<<>> :: binary, ?r>>)
      ?S ->
        language_range(r, [{t, 1000} | acc], 1,
                         <<<<>> :: binary, ?s>>)
      ?T ->
        language_range(r, [{t, 1000} | acc], 1,
                         <<<<>> :: binary, ?t>>)
      ?U ->
        language_range(r, [{t, 1000} | acc], 1,
                         <<<<>> :: binary, ?u>>)
      ?V ->
        language_range(r, [{t, 1000} | acc], 1,
                         <<<<>> :: binary, ?v>>)
      ?W ->
        language_range(r, [{t, 1000} | acc], 1,
                         <<<<>> :: binary, ?w>>)
      ?X ->
        language_range(r, [{t, 1000} | acc], 1,
                         <<<<>> :: binary, ?x>>)
      ?Y ->
        language_range(r, [{t, 1000} | acc], 1,
                         <<<<>> :: binary, ?y>>)
      ?Z ->
        language_range(r, [{t, 1000} | acc], 1,
                         <<<<>> :: binary, ?z>>)
      ^c ->
        language_range(r, [{t, 1000} | acc], 1,
                         <<<<>> :: binary, c>>)
    end
  end

  defp language_range_weight(<<"1.000", r :: bits>>, acc, t) do
    language_range_list_sep(r, [{t, 1000} | acc])
  end

  defp language_range_weight(<<"1.00", r :: bits>>, acc, t) do
    language_range_list_sep(r, [{t, 1000} | acc])
  end

  defp language_range_weight(<<"1.0", r :: bits>>, acc, t) do
    language_range_list_sep(r, [{t, 1000} | acc])
  end

  defp language_range_weight(<<"1.", r :: bits>>, acc, t) do
    language_range_list_sep(r, [{t, 1000} | acc])
  end

  defp language_range_weight(<<"1", r :: bits>>, acc, t) do
    language_range_list_sep(r, [{t, 1000} | acc])
  end

  defp language_range_weight(<<"0.", a, b, c, r :: bits>>, acc, t)
      when (((((((((a === ?0 or a === ?1) or a === ?2) or a === ?3) or a === ?4) or a === ?5) or a === ?6) or a === ?7) or a === ?8) or a === ?9 and
              ((((((((b === ?0 or b === ?1) or b === ?2) or b === ?3) or b === ?4) or b === ?5) or b === ?6) or b === ?7) or b === ?8) or b === ?9 and
              ((((((((c === ?0 or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9) do
    language_range_list_sep(r,
                              [{t, (a - ?0) * 100 + (b - ?0) * 10 + (c - ?0)} |
                                   acc])
  end

  defp language_range_weight(<<"0.", a, b, r :: bits>>, acc, t)
      when (((((((((a === ?0 or a === ?1) or a === ?2) or a === ?3) or a === ?4) or a === ?5) or a === ?6) or a === ?7) or a === ?8) or a === ?9 and
              ((((((((b === ?0 or b === ?1) or b === ?2) or b === ?3) or b === ?4) or b === ?5) or b === ?6) or b === ?7) or b === ?8) or b === ?9) do
    language_range_list_sep(r,
                              [{t, (a - ?0) * 100 + (b - ?0) * 10} | acc])
  end

  defp language_range_weight(<<"0.", a, r :: bits>>, acc, t)
      when ((((((((a === ?0 or a === ?1) or a === ?2) or a === ?3) or a === ?4) or a === ?5) or a === ?6) or a === ?7) or a === ?8) or a === ?9 do
    language_range_list_sep(r, [{t, (a - ?0) * 100} | acc])
  end

  defp language_range_weight(<<"0.", r :: bits>>, acc, t) do
    language_range_list_sep(r, [{t, 0} | acc])
  end

  defp language_range_weight(<<"0", r :: bits>>, acc, t) do
    language_range_list_sep(r, [{t, 0} | acc])
  end

  defp language_range_list_sep(<<>>, acc) do
    :lists.reverse(acc)
  end

  defp language_range_list_sep(<<c, r :: bits>>, acc)
      when c === ?\s or c === ?\t do
    language_range_list_sep(r, acc)
  end

  defp language_range_list_sep(<<?,, r :: bits>>, acc) do
    language_range_list(r, acc)
  end

  def parse_accept_ranges("none") do
    []
  end

  def parse_accept_ranges("bytes") do
    ["bytes"]
  end

  def parse_accept_ranges(acceptRanges) do
    nonempty(token_ci_list(acceptRanges, []))
  end

  def access_control_allow_credentials() do
    "true"
  end

  def access_control_allow_headers(headers) do
    join_token_list(nonempty(headers))
  end

  def access_control_allow_methods(methods) do
    join_token_list(nonempty(methods))
  end

  def access_control_allow_origin({scheme, host, port}) do
    case (default_port(scheme)) do
      ^port ->
        [scheme, "://", host]
      _ ->
        [scheme, "://", host, ":", :erlang.integer_to_binary(port)]
    end
  end

  def access_control_allow_origin(:"*") do
    <<?*>>
  end

  def access_control_allow_origin(ref) when is_reference(ref) do
    "null"
  end

  def access_control_expose_headers(headers) do
    join_token_list(nonempty(headers))
  end

  def access_control_max_age(maxAge) do
    :erlang.integer_to_binary(maxAge)
  end

  def parse_access_control_request_headers(headers) do
    token_ci_list(headers, [])
  end

  def parse_access_control_request_method(method) do
    true = <<>> !== method
    :ok = validate_token(method)
    method
  end

  defp validate_token(<<c, r :: bits>>)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9) or c === ?!) or c === ?#) or c === ?$) or c === ?%) or c === ?&) or c === ?') or c === ?*) or c === ?+) or c === ?-) or c === ?.) or c === ?^) or c === ?_) or c === ?`) or c === ?|) or c === ?~ do
    validate_token(r)
  end

  defp validate_token(<<>>) do
    :ok
  end

  def parse_age(age) do
    i = :erlang.binary_to_integer(age)
    true = i >= 0
    i
  end

  def parse_allow(allow) do
    token_list(allow, [])
  end

  def parse_authorization(<<b, a, s, i, c, " ", r :: bits>>)
      when (b === ?B or b === ?b and a === ?A or a === ?a and
              s === ?S or s === ?s and i === ?I or i === ?i and
              c === ?C or c === ?c) do
    auth_basic(:base64.decode(r), <<>>)
  end

  def parse_authorization(<<b, e1, a, r1, e2, r2, " ", r :: bits>>)
      when (r !== <<>> and b === ?B or b === ?b and
              e1 === ?E or e1 === ?e and a === ?A or a === ?a and
              r1 === ?R or r1 === ?r and e2 === ?E or e2 === ?e and
              r2 === ?R or r2 === ?r) do
    validate_auth_bearer(r)
    {:bearer, r}
  end

  def parse_authorization(<<d, i, g, e, s, t, " ", r :: bits>>)
      when (d === ?D or d === ?d and i === ?I or i === ?i and
              g === ?G or g === ?g and e === ?E or e === ?e and
              s === ?S or s === ?s and t === ?T or t === ?t) do
    {:digest, nonempty(auth_digest_list(r, []))}
  end

  defp auth_basic(<<?:, password :: bits>>, userID) do
    {:basic, userID, password}
  end

  defp auth_basic(<<c, r :: bits>>, userID) do
    auth_basic(r, <<userID :: binary, c>>)
  end

  defp validate_auth_bearer(<<c, r :: bits>>)
      when ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9) or c === ?-) or c === ?.) or c === ?_) or c === ?~) or c === ?+) or c === ?/ do
    validate_auth_bearer(r)
  end

  defp validate_auth_bearer(<<?=, r :: bits>>) do
    validate_auth_bearer_eq(r)
  end

  defp validate_auth_bearer(<<>>) do
    :ok
  end

  defp validate_auth_bearer_eq(<<?=, r :: bits>>) do
    validate_auth_bearer_eq(r)
  end

  defp validate_auth_bearer_eq(<<>>) do
    :ok
  end

  defp auth_digest_list(<<>>, acc) do
    :lists.reverse(acc)
  end

  defp auth_digest_list(<<c, r :: bits>>, acc)
      when (c === ?\s or c === ?\t) or c === ?, do
    auth_digest_list(r, acc)
  end

  defp auth_digest_list(<<"algorithm=", c, r :: bits>>, acc)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9) or c === ?!) or c === ?#) or c === ?$) or c === ?%) or c === ?&) or c === ?') or c === ?*) or c === ?+) or c === ?-) or c === ?.) or c === ?^) or c === ?_) or c === ?`) or c === ?|) or c === ?~ do
    auth_digest_token(r, acc, "algorithm", <<c>>)
  end

  defp auth_digest_list(<<"cnonce=\"", r :: bits>>, acc) do
    auth_digest_quoted(r, acc, "cnonce", <<>>)
  end

  defp auth_digest_list(<<"nc=", a, b, c, d, e, f, g, h, r :: bits>>, acc)
      when (((((((((((((((a === ?0 or a === ?1) or a === ?2) or a === ?3) or a === ?4) or a === ?5) or a === ?6) or a === ?7) or a === ?8) or a === ?9) or a === ?a) or a === ?b) or a === ?c) or a === ?d) or a === ?e) or a === ?f and
              ((((((((((((((b === ?0 or b === ?1) or b === ?2) or b === ?3) or b === ?4) or b === ?5) or b === ?6) or b === ?7) or b === ?8) or b === ?9) or b === ?a) or b === ?b) or b === ?c) or b === ?d) or b === ?e) or b === ?f and
              ((((((((((((((c === ?0 or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9) or c === ?a) or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f and
              ((((((((((((((d === ?0 or d === ?1) or d === ?2) or d === ?3) or d === ?4) or d === ?5) or d === ?6) or d === ?7) or d === ?8) or d === ?9) or d === ?a) or d === ?b) or d === ?c) or d === ?d) or d === ?e) or d === ?f and
              ((((((((((((((e === ?0 or e === ?1) or e === ?2) or e === ?3) or e === ?4) or e === ?5) or e === ?6) or e === ?7) or e === ?8) or e === ?9) or e === ?a) or e === ?b) or e === ?c) or e === ?d) or e === ?e) or e === ?f and
              ((((((((((((((f === ?0 or f === ?1) or f === ?2) or f === ?3) or f === ?4) or f === ?5) or f === ?6) or f === ?7) or f === ?8) or f === ?9) or f === ?a) or f === ?b) or f === ?c) or f === ?d) or f === ?e) or f === ?f and
              ((((((((((((((g === ?0 or g === ?1) or g === ?2) or g === ?3) or g === ?4) or g === ?5) or g === ?6) or g === ?7) or g === ?8) or g === ?9) or g === ?a) or g === ?b) or g === ?c) or g === ?d) or g === ?e) or g === ?f and
              ((((((((((((((h === ?0 or h === ?1) or h === ?2) or h === ?3) or h === ?4) or h === ?5) or h === ?6) or h === ?7) or h === ?8) or h === ?9) or h === ?a) or h === ?b) or h === ?c) or h === ?d) or h === ?e) or h === ?f) do
    auth_digest_list_sep(r,
                           [{"nc", <<a, b, c, d, e, f, g, h>>} | acc])
  end

  defp auth_digest_list(<<"nonce=\"", r :: bits>>, acc) do
    auth_digest_quoted(r, acc, "nonce", <<>>)
  end

  defp auth_digest_list(<<"opaque=\"", r :: bits>>, acc) do
    auth_digest_quoted(r, acc, "opaque", <<>>)
  end

  defp auth_digest_list(<<"qop=", c, r :: bits>>, acc)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9) or c === ?!) or c === ?#) or c === ?$) or c === ?%) or c === ?&) or c === ?') or c === ?*) or c === ?+) or c === ?-) or c === ?.) or c === ?^) or c === ?_) or c === ?`) or c === ?|) or c === ?~ do
    auth_digest_token(r, acc, "qop", <<c>>)
  end

  defp auth_digest_list(<<"realm=\"", r :: bits>>, acc) do
    auth_digest_quoted(r, acc, "realm", <<>>)
  end

  defp auth_digest_list(<<"response=\"", r :: bits>>, acc) do
    auth_digest_quoted(r, acc, "response", <<>>)
  end

  defp auth_digest_list(<<"uri=\"", r :: bits>>, acc) do
    auth_digest_quoted(r, acc, "uri", <<>>)
  end

  defp auth_digest_list(<<"username=\"", r :: bits>>, acc) do
    auth_digest_quoted(r, acc, "username", <<>>)
  end

  defp auth_digest_list(<<c, r :: bits>>, acc)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9) or c === ?!) or c === ?#) or c === ?$) or c === ?%) or c === ?&) or c === ?') or c === ?*) or c === ?+) or c === ?-) or c === ?.) or c === ?^) or c === ?_) or c === ?`) or c === ?|) or c === ?~ do
    case (c) do
      ?A ->
        auth_digest_param(r, acc, <<<<>> :: binary, ?a>>)
      ?B ->
        auth_digest_param(r, acc, <<<<>> :: binary, ?b>>)
      ?C ->
        auth_digest_param(r, acc, <<<<>> :: binary, ?c>>)
      ?D ->
        auth_digest_param(r, acc, <<<<>> :: binary, ?d>>)
      ?E ->
        auth_digest_param(r, acc, <<<<>> :: binary, ?e>>)
      ?F ->
        auth_digest_param(r, acc, <<<<>> :: binary, ?f>>)
      ?G ->
        auth_digest_param(r, acc, <<<<>> :: binary, ?g>>)
      ?H ->
        auth_digest_param(r, acc, <<<<>> :: binary, ?h>>)
      ?I ->
        auth_digest_param(r, acc, <<<<>> :: binary, ?i>>)
      ?J ->
        auth_digest_param(r, acc, <<<<>> :: binary, ?j>>)
      ?K ->
        auth_digest_param(r, acc, <<<<>> :: binary, ?k>>)
      ?L ->
        auth_digest_param(r, acc, <<<<>> :: binary, ?l>>)
      ?M ->
        auth_digest_param(r, acc, <<<<>> :: binary, ?m>>)
      ?N ->
        auth_digest_param(r, acc, <<<<>> :: binary, ?n>>)
      ?O ->
        auth_digest_param(r, acc, <<<<>> :: binary, ?o>>)
      ?P ->
        auth_digest_param(r, acc, <<<<>> :: binary, ?p>>)
      ?Q ->
        auth_digest_param(r, acc, <<<<>> :: binary, ?q>>)
      ?R ->
        auth_digest_param(r, acc, <<<<>> :: binary, ?r>>)
      ?S ->
        auth_digest_param(r, acc, <<<<>> :: binary, ?s>>)
      ?T ->
        auth_digest_param(r, acc, <<<<>> :: binary, ?t>>)
      ?U ->
        auth_digest_param(r, acc, <<<<>> :: binary, ?u>>)
      ?V ->
        auth_digest_param(r, acc, <<<<>> :: binary, ?v>>)
      ?W ->
        auth_digest_param(r, acc, <<<<>> :: binary, ?w>>)
      ?X ->
        auth_digest_param(r, acc, <<<<>> :: binary, ?x>>)
      ?Y ->
        auth_digest_param(r, acc, <<<<>> :: binary, ?y>>)
      ?Z ->
        auth_digest_param(r, acc, <<<<>> :: binary, ?z>>)
      ^c ->
        auth_digest_param(r, acc, <<<<>> :: binary, c>>)
    end
  end

  defp auth_digest_param(<<?=, ?", r :: bits>>, acc, k) do
    auth_digest_quoted(r, acc, k, <<>>)
  end

  defp auth_digest_param(<<?=, c, r :: bits>>, acc, k)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9) or c === ?!) or c === ?#) or c === ?$) or c === ?%) or c === ?&) or c === ?') or c === ?*) or c === ?+) or c === ?-) or c === ?.) or c === ?^) or c === ?_) or c === ?`) or c === ?|) or c === ?~ do
    auth_digest_token(r, acc, k, <<c>>)
  end

  defp auth_digest_param(<<c, r :: bits>>, acc, k)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9) or c === ?!) or c === ?#) or c === ?$) or c === ?%) or c === ?&) or c === ?') or c === ?*) or c === ?+) or c === ?-) or c === ?.) or c === ?^) or c === ?_) or c === ?`) or c === ?|) or c === ?~ do
    case (c) do
      ?A ->
        auth_digest_param(r, acc, <<k :: binary, ?a>>)
      ?B ->
        auth_digest_param(r, acc, <<k :: binary, ?b>>)
      ?C ->
        auth_digest_param(r, acc, <<k :: binary, ?c>>)
      ?D ->
        auth_digest_param(r, acc, <<k :: binary, ?d>>)
      ?E ->
        auth_digest_param(r, acc, <<k :: binary, ?e>>)
      ?F ->
        auth_digest_param(r, acc, <<k :: binary, ?f>>)
      ?G ->
        auth_digest_param(r, acc, <<k :: binary, ?g>>)
      ?H ->
        auth_digest_param(r, acc, <<k :: binary, ?h>>)
      ?I ->
        auth_digest_param(r, acc, <<k :: binary, ?i>>)
      ?J ->
        auth_digest_param(r, acc, <<k :: binary, ?j>>)
      ?K ->
        auth_digest_param(r, acc, <<k :: binary, ?k>>)
      ?L ->
        auth_digest_param(r, acc, <<k :: binary, ?l>>)
      ?M ->
        auth_digest_param(r, acc, <<k :: binary, ?m>>)
      ?N ->
        auth_digest_param(r, acc, <<k :: binary, ?n>>)
      ?O ->
        auth_digest_param(r, acc, <<k :: binary, ?o>>)
      ?P ->
        auth_digest_param(r, acc, <<k :: binary, ?p>>)
      ?Q ->
        auth_digest_param(r, acc, <<k :: binary, ?q>>)
      ?R ->
        auth_digest_param(r, acc, <<k :: binary, ?r>>)
      ?S ->
        auth_digest_param(r, acc, <<k :: binary, ?s>>)
      ?T ->
        auth_digest_param(r, acc, <<k :: binary, ?t>>)
      ?U ->
        auth_digest_param(r, acc, <<k :: binary, ?u>>)
      ?V ->
        auth_digest_param(r, acc, <<k :: binary, ?v>>)
      ?W ->
        auth_digest_param(r, acc, <<k :: binary, ?w>>)
      ?X ->
        auth_digest_param(r, acc, <<k :: binary, ?x>>)
      ?Y ->
        auth_digest_param(r, acc, <<k :: binary, ?y>>)
      ?Z ->
        auth_digest_param(r, acc, <<k :: binary, ?z>>)
      ^c ->
        auth_digest_param(r, acc, <<k :: binary, c>>)
    end
  end

  defp auth_digest_token(<<c, r :: bits>>, acc, k, v)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9) or c === ?!) or c === ?#) or c === ?$) or c === ?%) or c === ?&) or c === ?') or c === ?*) or c === ?+) or c === ?-) or c === ?.) or c === ?^) or c === ?_) or c === ?`) or c === ?|) or c === ?~ do
    auth_digest_token(r, acc, k, <<v :: binary, c>>)
  end

  defp auth_digest_token(r, acc, k, v) do
    auth_digest_list_sep(r, [{k, v} | acc])
  end

  defp auth_digest_quoted(<<?", r :: bits>>, acc, k, v) do
    auth_digest_list_sep(r, [{k, v} | acc])
  end

  defp auth_digest_quoted(<<?\\, c, r :: bits>>, acc, k, v)
      when c === ?\t or (c > 31 and c !== 127) do
    auth_digest_quoted(r, acc, k, <<v :: binary, c>>)
  end

  defp auth_digest_quoted(<<c, r :: bits>>, acc, k, v) when c === ?\t or
                                              (c > 31 and c !== 127) do
    auth_digest_quoted(r, acc, k, <<v :: binary, c>>)
  end

  defp auth_digest_list_sep(<<>>, acc) do
    :lists.reverse(acc)
  end

  defp auth_digest_list_sep(<<?,, r :: bits>>, acc) do
    auth_digest_list(r, acc)
  end

  defp auth_digest_list_sep(<<c, r :: bits>>, acc)
      when c === ?\s or c === ?\t do
    auth_digest_list_sep(r, acc)
  end

  def parse_cache_control("no-cache") do
    ["no-cache"]
  end

  def parse_cache_control("max-age=0") do
    [{"max-age", 0}]
  end

  def parse_cache_control(cacheControl) do
    nonempty(cache_directive_list(cacheControl, []))
  end

  defp cache_directive_list(<<>>, acc) do
    :lists.reverse(acc)
  end

  defp cache_directive_list(<<c, r :: bits>>, acc)
      when (c === ?\s or c === ?\t) or c === ?, do
    cache_directive_list(r, acc)
  end

  defp cache_directive_list(<<c, r :: bits>>, acc)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9) or c === ?!) or c === ?#) or c === ?$) or c === ?%) or c === ?&) or c === ?') or c === ?*) or c === ?+) or c === ?-) or c === ?.) or c === ?^) or c === ?_) or c === ?`) or c === ?|) or c === ?~ do
    case (c) do
      ?A ->
        cache_directive(r, acc, <<<<>> :: binary, ?a>>)
      ?B ->
        cache_directive(r, acc, <<<<>> :: binary, ?b>>)
      ?C ->
        cache_directive(r, acc, <<<<>> :: binary, ?c>>)
      ?D ->
        cache_directive(r, acc, <<<<>> :: binary, ?d>>)
      ?E ->
        cache_directive(r, acc, <<<<>> :: binary, ?e>>)
      ?F ->
        cache_directive(r, acc, <<<<>> :: binary, ?f>>)
      ?G ->
        cache_directive(r, acc, <<<<>> :: binary, ?g>>)
      ?H ->
        cache_directive(r, acc, <<<<>> :: binary, ?h>>)
      ?I ->
        cache_directive(r, acc, <<<<>> :: binary, ?i>>)
      ?J ->
        cache_directive(r, acc, <<<<>> :: binary, ?j>>)
      ?K ->
        cache_directive(r, acc, <<<<>> :: binary, ?k>>)
      ?L ->
        cache_directive(r, acc, <<<<>> :: binary, ?l>>)
      ?M ->
        cache_directive(r, acc, <<<<>> :: binary, ?m>>)
      ?N ->
        cache_directive(r, acc, <<<<>> :: binary, ?n>>)
      ?O ->
        cache_directive(r, acc, <<<<>> :: binary, ?o>>)
      ?P ->
        cache_directive(r, acc, <<<<>> :: binary, ?p>>)
      ?Q ->
        cache_directive(r, acc, <<<<>> :: binary, ?q>>)
      ?R ->
        cache_directive(r, acc, <<<<>> :: binary, ?r>>)
      ?S ->
        cache_directive(r, acc, <<<<>> :: binary, ?s>>)
      ?T ->
        cache_directive(r, acc, <<<<>> :: binary, ?t>>)
      ?U ->
        cache_directive(r, acc, <<<<>> :: binary, ?u>>)
      ?V ->
        cache_directive(r, acc, <<<<>> :: binary, ?v>>)
      ?W ->
        cache_directive(r, acc, <<<<>> :: binary, ?w>>)
      ?X ->
        cache_directive(r, acc, <<<<>> :: binary, ?x>>)
      ?Y ->
        cache_directive(r, acc, <<<<>> :: binary, ?y>>)
      ?Z ->
        cache_directive(r, acc, <<<<>> :: binary, ?z>>)
      ^c ->
        cache_directive(r, acc, <<<<>> :: binary, c>>)
    end
  end

  defp cache_directive(<<?=, ?", r :: bits>>, acc, t)
      when t === "no-cache" or t === "private" do
    cache_directive_fields_list(r, acc, t, [])
  end

  defp cache_directive(<<?=, c, r :: bits>>, acc, t)
      when (((((((((c === ?0 or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9 and
              ((t === "max-age" or t === "max-stale") or t === "min-fresh") or t === "s-maxage") do
    cache_directive_delta(r, acc, t, c - ?0)
  end

  defp cache_directive(<<?=, ?", r :: bits>>, acc, t) do
    cache_directive_quoted_string(r, acc, t, <<>>)
  end

  defp cache_directive(<<?=, c, r :: bits>>, acc, t)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9) or c === ?!) or c === ?#) or c === ?$) or c === ?%) or c === ?&) or c === ?') or c === ?*) or c === ?+) or c === ?-) or c === ?.) or c === ?^) or c === ?_) or c === ?`) or c === ?|) or c === ?~ do
    cache_directive_token(r, acc, t, <<c>>)
  end

  defp cache_directive(<<c, r :: bits>>, acc, t)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9) or c === ?!) or c === ?#) or c === ?$) or c === ?%) or c === ?&) or c === ?') or c === ?*) or c === ?+) or c === ?-) or c === ?.) or c === ?^) or c === ?_) or c === ?`) or c === ?|) or c === ?~ do
    case (c) do
      ?A ->
        cache_directive(r, acc, <<t :: binary, ?a>>)
      ?B ->
        cache_directive(r, acc, <<t :: binary, ?b>>)
      ?C ->
        cache_directive(r, acc, <<t :: binary, ?c>>)
      ?D ->
        cache_directive(r, acc, <<t :: binary, ?d>>)
      ?E ->
        cache_directive(r, acc, <<t :: binary, ?e>>)
      ?F ->
        cache_directive(r, acc, <<t :: binary, ?f>>)
      ?G ->
        cache_directive(r, acc, <<t :: binary, ?g>>)
      ?H ->
        cache_directive(r, acc, <<t :: binary, ?h>>)
      ?I ->
        cache_directive(r, acc, <<t :: binary, ?i>>)
      ?J ->
        cache_directive(r, acc, <<t :: binary, ?j>>)
      ?K ->
        cache_directive(r, acc, <<t :: binary, ?k>>)
      ?L ->
        cache_directive(r, acc, <<t :: binary, ?l>>)
      ?M ->
        cache_directive(r, acc, <<t :: binary, ?m>>)
      ?N ->
        cache_directive(r, acc, <<t :: binary, ?n>>)
      ?O ->
        cache_directive(r, acc, <<t :: binary, ?o>>)
      ?P ->
        cache_directive(r, acc, <<t :: binary, ?p>>)
      ?Q ->
        cache_directive(r, acc, <<t :: binary, ?q>>)
      ?R ->
        cache_directive(r, acc, <<t :: binary, ?r>>)
      ?S ->
        cache_directive(r, acc, <<t :: binary, ?s>>)
      ?T ->
        cache_directive(r, acc, <<t :: binary, ?t>>)
      ?U ->
        cache_directive(r, acc, <<t :: binary, ?u>>)
      ?V ->
        cache_directive(r, acc, <<t :: binary, ?v>>)
      ?W ->
        cache_directive(r, acc, <<t :: binary, ?w>>)
      ?X ->
        cache_directive(r, acc, <<t :: binary, ?x>>)
      ?Y ->
        cache_directive(r, acc, <<t :: binary, ?y>>)
      ?Z ->
        cache_directive(r, acc, <<t :: binary, ?z>>)
      ^c ->
        cache_directive(r, acc, <<t :: binary, c>>)
    end
  end

  defp cache_directive(r, acc, t) do
    cache_directive_list_sep(r, [t | acc])
  end

  defp cache_directive_delta(<<c, r :: bits>>, acc, k, v)
      when ((((((((c === ?0 or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9 do
    cache_directive_delta(r, acc, k, v * 10 + (c - ?0))
  end

  defp cache_directive_delta(r, acc, k, v) do
    cache_directive_list_sep(r, [{k, v} | acc])
  end

  defp cache_directive_fields_list(<<c, r :: bits>>, acc, k, l)
      when (c === ?\s or c === ?\t) or c === ?, do
    cache_directive_fields_list(r, acc, k, l)
  end

  defp cache_directive_fields_list(<<?", r :: bits>>, acc, k, l) do
    cache_directive_list_sep(r,
                               [{k, :lists.reverse(l)} | acc])
  end

  defp cache_directive_fields_list(<<c, r :: bits>>, acc, k, l)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9) or c === ?!) or c === ?#) or c === ?$) or c === ?%) or c === ?&) or c === ?') or c === ?*) or c === ?+) or c === ?-) or c === ?.) or c === ?^) or c === ?_) or c === ?`) or c === ?|) or c === ?~ do
    case (c) do
      ?A ->
        cache_directive_field(r, acc, k, l,
                                <<<<>> :: binary, ?a>>)
      ?B ->
        cache_directive_field(r, acc, k, l,
                                <<<<>> :: binary, ?b>>)
      ?C ->
        cache_directive_field(r, acc, k, l,
                                <<<<>> :: binary, ?c>>)
      ?D ->
        cache_directive_field(r, acc, k, l,
                                <<<<>> :: binary, ?d>>)
      ?E ->
        cache_directive_field(r, acc, k, l,
                                <<<<>> :: binary, ?e>>)
      ?F ->
        cache_directive_field(r, acc, k, l,
                                <<<<>> :: binary, ?f>>)
      ?G ->
        cache_directive_field(r, acc, k, l,
                                <<<<>> :: binary, ?g>>)
      ?H ->
        cache_directive_field(r, acc, k, l,
                                <<<<>> :: binary, ?h>>)
      ?I ->
        cache_directive_field(r, acc, k, l,
                                <<<<>> :: binary, ?i>>)
      ?J ->
        cache_directive_field(r, acc, k, l,
                                <<<<>> :: binary, ?j>>)
      ?K ->
        cache_directive_field(r, acc, k, l,
                                <<<<>> :: binary, ?k>>)
      ?L ->
        cache_directive_field(r, acc, k, l,
                                <<<<>> :: binary, ?l>>)
      ?M ->
        cache_directive_field(r, acc, k, l,
                                <<<<>> :: binary, ?m>>)
      ?N ->
        cache_directive_field(r, acc, k, l,
                                <<<<>> :: binary, ?n>>)
      ?O ->
        cache_directive_field(r, acc, k, l,
                                <<<<>> :: binary, ?o>>)
      ?P ->
        cache_directive_field(r, acc, k, l,
                                <<<<>> :: binary, ?p>>)
      ?Q ->
        cache_directive_field(r, acc, k, l,
                                <<<<>> :: binary, ?q>>)
      ?R ->
        cache_directive_field(r, acc, k, l,
                                <<<<>> :: binary, ?r>>)
      ?S ->
        cache_directive_field(r, acc, k, l,
                                <<<<>> :: binary, ?s>>)
      ?T ->
        cache_directive_field(r, acc, k, l,
                                <<<<>> :: binary, ?t>>)
      ?U ->
        cache_directive_field(r, acc, k, l,
                                <<<<>> :: binary, ?u>>)
      ?V ->
        cache_directive_field(r, acc, k, l,
                                <<<<>> :: binary, ?v>>)
      ?W ->
        cache_directive_field(r, acc, k, l,
                                <<<<>> :: binary, ?w>>)
      ?X ->
        cache_directive_field(r, acc, k, l,
                                <<<<>> :: binary, ?x>>)
      ?Y ->
        cache_directive_field(r, acc, k, l,
                                <<<<>> :: binary, ?y>>)
      ?Z ->
        cache_directive_field(r, acc, k, l,
                                <<<<>> :: binary, ?z>>)
      ^c ->
        cache_directive_field(r, acc, k, l,
                                <<<<>> :: binary, c>>)
    end
  end

  defp cache_directive_field(<<c, r :: bits>>, acc, k, l, f)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9) or c === ?!) or c === ?#) or c === ?$) or c === ?%) or c === ?&) or c === ?') or c === ?*) or c === ?+) or c === ?-) or c === ?.) or c === ?^) or c === ?_) or c === ?`) or c === ?|) or c === ?~ do
    case (c) do
      ?A ->
        cache_directive_field(r, acc, k, l, <<f :: binary, ?a>>)
      ?B ->
        cache_directive_field(r, acc, k, l, <<f :: binary, ?b>>)
      ?C ->
        cache_directive_field(r, acc, k, l, <<f :: binary, ?c>>)
      ?D ->
        cache_directive_field(r, acc, k, l, <<f :: binary, ?d>>)
      ?E ->
        cache_directive_field(r, acc, k, l, <<f :: binary, ?e>>)
      ?F ->
        cache_directive_field(r, acc, k, l, <<f :: binary, ?f>>)
      ?G ->
        cache_directive_field(r, acc, k, l, <<f :: binary, ?g>>)
      ?H ->
        cache_directive_field(r, acc, k, l, <<f :: binary, ?h>>)
      ?I ->
        cache_directive_field(r, acc, k, l, <<f :: binary, ?i>>)
      ?J ->
        cache_directive_field(r, acc, k, l, <<f :: binary, ?j>>)
      ?K ->
        cache_directive_field(r, acc, k, l, <<f :: binary, ?k>>)
      ?L ->
        cache_directive_field(r, acc, k, l, <<f :: binary, ?l>>)
      ?M ->
        cache_directive_field(r, acc, k, l, <<f :: binary, ?m>>)
      ?N ->
        cache_directive_field(r, acc, k, l, <<f :: binary, ?n>>)
      ?O ->
        cache_directive_field(r, acc, k, l, <<f :: binary, ?o>>)
      ?P ->
        cache_directive_field(r, acc, k, l, <<f :: binary, ?p>>)
      ?Q ->
        cache_directive_field(r, acc, k, l, <<f :: binary, ?q>>)
      ?R ->
        cache_directive_field(r, acc, k, l, <<f :: binary, ?r>>)
      ?S ->
        cache_directive_field(r, acc, k, l, <<f :: binary, ?s>>)
      ?T ->
        cache_directive_field(r, acc, k, l, <<f :: binary, ?t>>)
      ?U ->
        cache_directive_field(r, acc, k, l, <<f :: binary, ?u>>)
      ?V ->
        cache_directive_field(r, acc, k, l, <<f :: binary, ?v>>)
      ?W ->
        cache_directive_field(r, acc, k, l, <<f :: binary, ?w>>)
      ?X ->
        cache_directive_field(r, acc, k, l, <<f :: binary, ?x>>)
      ?Y ->
        cache_directive_field(r, acc, k, l, <<f :: binary, ?y>>)
      ?Z ->
        cache_directive_field(r, acc, k, l, <<f :: binary, ?z>>)
      ^c ->
        cache_directive_field(r, acc, k, l, <<f :: binary, c>>)
    end
  end

  defp cache_directive_field(r, acc, k, l, f) do
    cache_directive_fields_list_sep(r, acc, k, [f | l])
  end

  defp cache_directive_fields_list_sep(<<c, r :: bits>>, acc, k, l)
      when c === ?\s or c === ?\t do
    cache_directive_fields_list_sep(r, acc, k, l)
  end

  defp cache_directive_fields_list_sep(<<?,, r :: bits>>, acc, k, l) do
    cache_directive_fields_list(r, acc, k, l)
  end

  defp cache_directive_fields_list_sep(<<?", r :: bits>>, acc, k, l) do
    cache_directive_list_sep(r,
                               [{k, :lists.reverse(l)} | acc])
  end

  defp cache_directive_token(<<c, r :: bits>>, acc, k, v)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9) or c === ?!) or c === ?#) or c === ?$) or c === ?%) or c === ?&) or c === ?') or c === ?*) or c === ?+) or c === ?-) or c === ?.) or c === ?^) or c === ?_) or c === ?`) or c === ?|) or c === ?~ do
    cache_directive_token(r, acc, k, <<v :: binary, c>>)
  end

  defp cache_directive_token(r, acc, k, v) do
    cache_directive_list_sep(r, [{k, v} | acc])
  end

  defp cache_directive_quoted_string(<<?", r :: bits>>, acc, k, v) do
    cache_directive_list_sep(r, [{k, v} | acc])
  end

  defp cache_directive_quoted_string(<<?\\, c, r :: bits>>, acc, k, v)
      when c === ?\t or (c > 31 and c !== 127) do
    cache_directive_quoted_string(r, acc, k,
                                    <<v :: binary, c>>)
  end

  defp cache_directive_quoted_string(<<c, r :: bits>>, acc, k, v) when c === ?\t or
                                              (c > 31 and c !== 127) do
    cache_directive_quoted_string(r, acc, k,
                                    <<v :: binary, c>>)
  end

  defp cache_directive_list_sep(<<>>, acc) do
    :lists.reverse(acc)
  end

  defp cache_directive_list_sep(<<c, r :: bits>>, acc)
      when c === ?\s or c === ?\t do
    cache_directive_list_sep(r, acc)
  end

  defp cache_directive_list_sep(<<?,, r :: bits>>, acc) do
    cache_directive_list(r, acc)
  end

  def parse_connection("close") do
    ["close"]
  end

  def parse_connection("keep-alive") do
    ["keep-alive"]
  end

  def parse_connection(connection) do
    nonempty(token_ci_list(connection, []))
  end

  def parse_content_encoding(contentEncoding) do
    nonempty(token_ci_list(contentEncoding, []))
  end

  def parse_content_language(contentLanguage) do
    nonempty(langtag_list(contentLanguage, []))
  end

  defp langtag_list(<<>>, acc) do
    :lists.reverse(acc)
  end

  defp langtag_list(<<c, r :: bits>>, acc)
      when (c === ?\s or c === ?\t) or c === ?, do
    langtag_list(r, acc)
  end

  defp langtag_list(<<a, b, c, r :: bits>>, acc)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((a === ?a or a === ?b) or a === ?c) or a === ?d) or a === ?e) or a === ?f) or a === ?g) or a === ?h) or a === ?i) or a === ?j) or a === ?k) or a === ?l) or a === ?m) or a === ?n) or a === ?o) or a === ?p) or a === ?q) or a === ?r) or a === ?s) or a === ?t) or a === ?u) or a === ?v) or a === ?w) or a === ?x) or a === ?y) or a === ?z) or a === ?A) or a === ?B) or a === ?C) or a === ?D) or a === ?E) or a === ?F) or a === ?G) or a === ?H) or a === ?I) or a === ?J) or a === ?K) or a === ?L) or a === ?M) or a === ?N) or a === ?O) or a === ?P) or a === ?Q) or a === ?R) or a === ?S) or a === ?T) or a === ?U) or a === ?V) or a === ?W) or a === ?X) or a === ?Y) or a === ?Z and
              ((((((((((((((((((((((((((((((((((((((((((((((((((b === ?a or b === ?b) or b === ?c) or b === ?d) or b === ?e) or b === ?f) or b === ?g) or b === ?h) or b === ?i) or b === ?j) or b === ?k) or b === ?l) or b === ?m) or b === ?n) or b === ?o) or b === ?p) or b === ?q) or b === ?r) or b === ?s) or b === ?t) or b === ?u) or b === ?v) or b === ?w) or b === ?x) or b === ?y) or b === ?z) or b === ?A) or b === ?B) or b === ?C) or b === ?D) or b === ?E) or b === ?F) or b === ?G) or b === ?H) or b === ?I) or b === ?J) or b === ?K) or b === ?L) or b === ?M) or b === ?N) or b === ?O) or b === ?P) or b === ?Q) or b === ?R) or b === ?S) or b === ?T) or b === ?U) or b === ?V) or b === ?W) or b === ?X) or b === ?Y) or b === ?Z and
              ((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) do
    langtag_extlang(r, acc,
                      <<case (a) do
                          ?A ->
                            ?a
                          ?B ->
                            ?b
                          ?C ->
                            ?c
                          ?D ->
                            ?d
                          ?E ->
                            ?e
                          ?F ->
                            ?f
                          ?G ->
                            ?g
                          ?H ->
                            ?h
                          ?I ->
                            ?i
                          ?J ->
                            ?j
                          ?K ->
                            ?k
                          ?L ->
                            ?l
                          ?M ->
                            ?m
                          ?N ->
                            ?n
                          ?O ->
                            ?o
                          ?P ->
                            ?p
                          ?Q ->
                            ?q
                          ?R ->
                            ?r
                          ?S ->
                            ?s
                          ?T ->
                            ?t
                          ?U ->
                            ?u
                          ?V ->
                            ?v
                          ?W ->
                            ?w
                          ?X ->
                            ?x
                          ?Y ->
                            ?y
                          ?Z ->
                            ?z
                          _ ->
                            a
                        end,
                          case (b) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              b
                          end,
                          case (c) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              c
                          end>>,
                      0)
  end

  defp langtag_list(<<a, b, r :: bits>>, acc)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((a === ?a or a === ?b) or a === ?c) or a === ?d) or a === ?e) or a === ?f) or a === ?g) or a === ?h) or a === ?i) or a === ?j) or a === ?k) or a === ?l) or a === ?m) or a === ?n) or a === ?o) or a === ?p) or a === ?q) or a === ?r) or a === ?s) or a === ?t) or a === ?u) or a === ?v) or a === ?w) or a === ?x) or a === ?y) or a === ?z) or a === ?A) or a === ?B) or a === ?C) or a === ?D) or a === ?E) or a === ?F) or a === ?G) or a === ?H) or a === ?I) or a === ?J) or a === ?K) or a === ?L) or a === ?M) or a === ?N) or a === ?O) or a === ?P) or a === ?Q) or a === ?R) or a === ?S) or a === ?T) or a === ?U) or a === ?V) or a === ?W) or a === ?X) or a === ?Y) or a === ?Z and
              ((((((((((((((((((((((((((((((((((((((((((((((((((b === ?a or b === ?b) or b === ?c) or b === ?d) or b === ?e) or b === ?f) or b === ?g) or b === ?h) or b === ?i) or b === ?j) or b === ?k) or b === ?l) or b === ?m) or b === ?n) or b === ?o) or b === ?p) or b === ?q) or b === ?r) or b === ?s) or b === ?t) or b === ?u) or b === ?v) or b === ?w) or b === ?x) or b === ?y) or b === ?z) or b === ?A) or b === ?B) or b === ?C) or b === ?D) or b === ?E) or b === ?F) or b === ?G) or b === ?H) or b === ?I) or b === ?J) or b === ?K) or b === ?L) or b === ?M) or b === ?N) or b === ?O) or b === ?P) or b === ?Q) or b === ?R) or b === ?S) or b === ?T) or b === ?U) or b === ?V) or b === ?W) or b === ?X) or b === ?Y) or b === ?Z) do
    langtag_extlang(r, acc,
                      <<case (a) do
                          ?A ->
                            ?a
                          ?B ->
                            ?b
                          ?C ->
                            ?c
                          ?D ->
                            ?d
                          ?E ->
                            ?e
                          ?F ->
                            ?f
                          ?G ->
                            ?g
                          ?H ->
                            ?h
                          ?I ->
                            ?i
                          ?J ->
                            ?j
                          ?K ->
                            ?k
                          ?L ->
                            ?l
                          ?M ->
                            ?m
                          ?N ->
                            ?n
                          ?O ->
                            ?o
                          ?P ->
                            ?p
                          ?Q ->
                            ?q
                          ?R ->
                            ?r
                          ?S ->
                            ?s
                          ?T ->
                            ?t
                          ?U ->
                            ?u
                          ?V ->
                            ?v
                          ?W ->
                            ?w
                          ?X ->
                            ?x
                          ?Y ->
                            ?y
                          ?Z ->
                            ?z
                          _ ->
                            a
                        end,
                          case (b) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              b
                          end>>,
                      0)
  end

  defp langtag_list(<<x, r :: bits>>, acc) when x === ?x or
                                        x === ?X do
    langtag_privateuse_sub(r, acc, <<?x>>, 0)
  end

  defp langtag_extlang(<<?-, a, b, c, d, e, f, g, h, r :: bits>>, acc,
            t, _)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((a === ?a or a === ?b) or a === ?c) or a === ?d) or a === ?e) or a === ?f) or a === ?g) or a === ?h) or a === ?i) or a === ?j) or a === ?k) or a === ?l) or a === ?m) or a === ?n) or a === ?o) or a === ?p) or a === ?q) or a === ?r) or a === ?s) or a === ?t) or a === ?u) or a === ?v) or a === ?w) or a === ?x) or a === ?y) or a === ?z) or a === ?A) or a === ?B) or a === ?C) or a === ?D) or a === ?E) or a === ?F) or a === ?G) or a === ?H) or a === ?I) or a === ?J) or a === ?K) or a === ?L) or a === ?M) or a === ?N) or a === ?O) or a === ?P) or a === ?Q) or a === ?R) or a === ?S) or a === ?T) or a === ?U) or a === ?V) or a === ?W) or a === ?X) or a === ?Y) or a === ?Z) or a === ?0) or a === ?1) or a === ?2) or a === ?3) or a === ?4) or a === ?5) or a === ?6) or a === ?7) or a === ?8) or a === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((b === ?a or b === ?b) or b === ?c) or b === ?d) or b === ?e) or b === ?f) or b === ?g) or b === ?h) or b === ?i) or b === ?j) or b === ?k) or b === ?l) or b === ?m) or b === ?n) or b === ?o) or b === ?p) or b === ?q) or b === ?r) or b === ?s) or b === ?t) or b === ?u) or b === ?v) or b === ?w) or b === ?x) or b === ?y) or b === ?z) or b === ?A) or b === ?B) or b === ?C) or b === ?D) or b === ?E) or b === ?F) or b === ?G) or b === ?H) or b === ?I) or b === ?J) or b === ?K) or b === ?L) or b === ?M) or b === ?N) or b === ?O) or b === ?P) or b === ?Q) or b === ?R) or b === ?S) or b === ?T) or b === ?U) or b === ?V) or b === ?W) or b === ?X) or b === ?Y) or b === ?Z) or b === ?0) or b === ?1) or b === ?2) or b === ?3) or b === ?4) or b === ?5) or b === ?6) or b === ?7) or b === ?8) or b === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((d === ?a or d === ?b) or d === ?c) or d === ?d) or d === ?e) or d === ?f) or d === ?g) or d === ?h) or d === ?i) or d === ?j) or d === ?k) or d === ?l) or d === ?m) or d === ?n) or d === ?o) or d === ?p) or d === ?q) or d === ?r) or d === ?s) or d === ?t) or d === ?u) or d === ?v) or d === ?w) or d === ?x) or d === ?y) or d === ?z) or d === ?A) or d === ?B) or d === ?C) or d === ?D) or d === ?E) or d === ?F) or d === ?G) or d === ?H) or d === ?I) or d === ?J) or d === ?K) or d === ?L) or d === ?M) or d === ?N) or d === ?O) or d === ?P) or d === ?Q) or d === ?R) or d === ?S) or d === ?T) or d === ?U) or d === ?V) or d === ?W) or d === ?X) or d === ?Y) or d === ?Z) or d === ?0) or d === ?1) or d === ?2) or d === ?3) or d === ?4) or d === ?5) or d === ?6) or d === ?7) or d === ?8) or d === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((e === ?a or e === ?b) or e === ?c) or e === ?d) or e === ?e) or e === ?f) or e === ?g) or e === ?h) or e === ?i) or e === ?j) or e === ?k) or e === ?l) or e === ?m) or e === ?n) or e === ?o) or e === ?p) or e === ?q) or e === ?r) or e === ?s) or e === ?t) or e === ?u) or e === ?v) or e === ?w) or e === ?x) or e === ?y) or e === ?z) or e === ?A) or e === ?B) or e === ?C) or e === ?D) or e === ?E) or e === ?F) or e === ?G) or e === ?H) or e === ?I) or e === ?J) or e === ?K) or e === ?L) or e === ?M) or e === ?N) or e === ?O) or e === ?P) or e === ?Q) or e === ?R) or e === ?S) or e === ?T) or e === ?U) or e === ?V) or e === ?W) or e === ?X) or e === ?Y) or e === ?Z) or e === ?0) or e === ?1) or e === ?2) or e === ?3) or e === ?4) or e === ?5) or e === ?6) or e === ?7) or e === ?8) or e === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((f === ?a or f === ?b) or f === ?c) or f === ?d) or f === ?e) or f === ?f) or f === ?g) or f === ?h) or f === ?i) or f === ?j) or f === ?k) or f === ?l) or f === ?m) or f === ?n) or f === ?o) or f === ?p) or f === ?q) or f === ?r) or f === ?s) or f === ?t) or f === ?u) or f === ?v) or f === ?w) or f === ?x) or f === ?y) or f === ?z) or f === ?A) or f === ?B) or f === ?C) or f === ?D) or f === ?E) or f === ?F) or f === ?G) or f === ?H) or f === ?I) or f === ?J) or f === ?K) or f === ?L) or f === ?M) or f === ?N) or f === ?O) or f === ?P) or f === ?Q) or f === ?R) or f === ?S) or f === ?T) or f === ?U) or f === ?V) or f === ?W) or f === ?X) or f === ?Y) or f === ?Z) or f === ?0) or f === ?1) or f === ?2) or f === ?3) or f === ?4) or f === ?5) or f === ?6) or f === ?7) or f === ?8) or f === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((g === ?a or g === ?b) or g === ?c) or g === ?d) or g === ?e) or g === ?f) or g === ?g) or g === ?h) or g === ?i) or g === ?j) or g === ?k) or g === ?l) or g === ?m) or g === ?n) or g === ?o) or g === ?p) or g === ?q) or g === ?r) or g === ?s) or g === ?t) or g === ?u) or g === ?v) or g === ?w) or g === ?x) or g === ?y) or g === ?z) or g === ?A) or g === ?B) or g === ?C) or g === ?D) or g === ?E) or g === ?F) or g === ?G) or g === ?H) or g === ?I) or g === ?J) or g === ?K) or g === ?L) or g === ?M) or g === ?N) or g === ?O) or g === ?P) or g === ?Q) or g === ?R) or g === ?S) or g === ?T) or g === ?U) or g === ?V) or g === ?W) or g === ?X) or g === ?Y) or g === ?Z) or g === ?0) or g === ?1) or g === ?2) or g === ?3) or g === ?4) or g === ?5) or g === ?6) or g === ?7) or g === ?8) or g === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((h === ?a or h === ?b) or h === ?c) or h === ?d) or h === ?e) or h === ?f) or h === ?g) or h === ?h) or h === ?i) or h === ?j) or h === ?k) or h === ?l) or h === ?m) or h === ?n) or h === ?o) or h === ?p) or h === ?q) or h === ?r) or h === ?s) or h === ?t) or h === ?u) or h === ?v) or h === ?w) or h === ?x) or h === ?y) or h === ?z) or h === ?A) or h === ?B) or h === ?C) or h === ?D) or h === ?E) or h === ?F) or h === ?G) or h === ?H) or h === ?I) or h === ?J) or h === ?K) or h === ?L) or h === ?M) or h === ?N) or h === ?O) or h === ?P) or h === ?Q) or h === ?R) or h === ?S) or h === ?T) or h === ?U) or h === ?V) or h === ?W) or h === ?X) or h === ?Y) or h === ?Z) or h === ?0) or h === ?1) or h === ?2) or h === ?3) or h === ?4) or h === ?5) or h === ?6) or h === ?7) or h === ?8) or h === ?9) do
    langtag_variant(r, acc,
                      <<t :: binary, ?-,
                          case (a) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              a
                          end,
                          case (b) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              b
                          end,
                          case (c) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              c
                          end,
                          case (d) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              d
                          end,
                          case (e) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              e
                          end,
                          case (f) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              f
                          end,
                          case (g) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              g
                          end,
                          case (h) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              h
                          end>>)
  end

  defp langtag_extlang(<<?-, a, b, c, d, e, f, g, r :: bits>>, acc, t,
            _)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((a === ?a or a === ?b) or a === ?c) or a === ?d) or a === ?e) or a === ?f) or a === ?g) or a === ?h) or a === ?i) or a === ?j) or a === ?k) or a === ?l) or a === ?m) or a === ?n) or a === ?o) or a === ?p) or a === ?q) or a === ?r) or a === ?s) or a === ?t) or a === ?u) or a === ?v) or a === ?w) or a === ?x) or a === ?y) or a === ?z) or a === ?A) or a === ?B) or a === ?C) or a === ?D) or a === ?E) or a === ?F) or a === ?G) or a === ?H) or a === ?I) or a === ?J) or a === ?K) or a === ?L) or a === ?M) or a === ?N) or a === ?O) or a === ?P) or a === ?Q) or a === ?R) or a === ?S) or a === ?T) or a === ?U) or a === ?V) or a === ?W) or a === ?X) or a === ?Y) or a === ?Z) or a === ?0) or a === ?1) or a === ?2) or a === ?3) or a === ?4) or a === ?5) or a === ?6) or a === ?7) or a === ?8) or a === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((b === ?a or b === ?b) or b === ?c) or b === ?d) or b === ?e) or b === ?f) or b === ?g) or b === ?h) or b === ?i) or b === ?j) or b === ?k) or b === ?l) or b === ?m) or b === ?n) or b === ?o) or b === ?p) or b === ?q) or b === ?r) or b === ?s) or b === ?t) or b === ?u) or b === ?v) or b === ?w) or b === ?x) or b === ?y) or b === ?z) or b === ?A) or b === ?B) or b === ?C) or b === ?D) or b === ?E) or b === ?F) or b === ?G) or b === ?H) or b === ?I) or b === ?J) or b === ?K) or b === ?L) or b === ?M) or b === ?N) or b === ?O) or b === ?P) or b === ?Q) or b === ?R) or b === ?S) or b === ?T) or b === ?U) or b === ?V) or b === ?W) or b === ?X) or b === ?Y) or b === ?Z) or b === ?0) or b === ?1) or b === ?2) or b === ?3) or b === ?4) or b === ?5) or b === ?6) or b === ?7) or b === ?8) or b === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((d === ?a or d === ?b) or d === ?c) or d === ?d) or d === ?e) or d === ?f) or d === ?g) or d === ?h) or d === ?i) or d === ?j) or d === ?k) or d === ?l) or d === ?m) or d === ?n) or d === ?o) or d === ?p) or d === ?q) or d === ?r) or d === ?s) or d === ?t) or d === ?u) or d === ?v) or d === ?w) or d === ?x) or d === ?y) or d === ?z) or d === ?A) or d === ?B) or d === ?C) or d === ?D) or d === ?E) or d === ?F) or d === ?G) or d === ?H) or d === ?I) or d === ?J) or d === ?K) or d === ?L) or d === ?M) or d === ?N) or d === ?O) or d === ?P) or d === ?Q) or d === ?R) or d === ?S) or d === ?T) or d === ?U) or d === ?V) or d === ?W) or d === ?X) or d === ?Y) or d === ?Z) or d === ?0) or d === ?1) or d === ?2) or d === ?3) or d === ?4) or d === ?5) or d === ?6) or d === ?7) or d === ?8) or d === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((e === ?a or e === ?b) or e === ?c) or e === ?d) or e === ?e) or e === ?f) or e === ?g) or e === ?h) or e === ?i) or e === ?j) or e === ?k) or e === ?l) or e === ?m) or e === ?n) or e === ?o) or e === ?p) or e === ?q) or e === ?r) or e === ?s) or e === ?t) or e === ?u) or e === ?v) or e === ?w) or e === ?x) or e === ?y) or e === ?z) or e === ?A) or e === ?B) or e === ?C) or e === ?D) or e === ?E) or e === ?F) or e === ?G) or e === ?H) or e === ?I) or e === ?J) or e === ?K) or e === ?L) or e === ?M) or e === ?N) or e === ?O) or e === ?P) or e === ?Q) or e === ?R) or e === ?S) or e === ?T) or e === ?U) or e === ?V) or e === ?W) or e === ?X) or e === ?Y) or e === ?Z) or e === ?0) or e === ?1) or e === ?2) or e === ?3) or e === ?4) or e === ?5) or e === ?6) or e === ?7) or e === ?8) or e === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((f === ?a or f === ?b) or f === ?c) or f === ?d) or f === ?e) or f === ?f) or f === ?g) or f === ?h) or f === ?i) or f === ?j) or f === ?k) or f === ?l) or f === ?m) or f === ?n) or f === ?o) or f === ?p) or f === ?q) or f === ?r) or f === ?s) or f === ?t) or f === ?u) or f === ?v) or f === ?w) or f === ?x) or f === ?y) or f === ?z) or f === ?A) or f === ?B) or f === ?C) or f === ?D) or f === ?E) or f === ?F) or f === ?G) or f === ?H) or f === ?I) or f === ?J) or f === ?K) or f === ?L) or f === ?M) or f === ?N) or f === ?O) or f === ?P) or f === ?Q) or f === ?R) or f === ?S) or f === ?T) or f === ?U) or f === ?V) or f === ?W) or f === ?X) or f === ?Y) or f === ?Z) or f === ?0) or f === ?1) or f === ?2) or f === ?3) or f === ?4) or f === ?5) or f === ?6) or f === ?7) or f === ?8) or f === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((g === ?a or g === ?b) or g === ?c) or g === ?d) or g === ?e) or g === ?f) or g === ?g) or g === ?h) or g === ?i) or g === ?j) or g === ?k) or g === ?l) or g === ?m) or g === ?n) or g === ?o) or g === ?p) or g === ?q) or g === ?r) or g === ?s) or g === ?t) or g === ?u) or g === ?v) or g === ?w) or g === ?x) or g === ?y) or g === ?z) or g === ?A) or g === ?B) or g === ?C) or g === ?D) or g === ?E) or g === ?F) or g === ?G) or g === ?H) or g === ?I) or g === ?J) or g === ?K) or g === ?L) or g === ?M) or g === ?N) or g === ?O) or g === ?P) or g === ?Q) or g === ?R) or g === ?S) or g === ?T) or g === ?U) or g === ?V) or g === ?W) or g === ?X) or g === ?Y) or g === ?Z) or g === ?0) or g === ?1) or g === ?2) or g === ?3) or g === ?4) or g === ?5) or g === ?6) or g === ?7) or g === ?8) or g === ?9) do
    langtag_variant(r, acc,
                      <<t :: binary, ?-,
                          case (a) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              a
                          end,
                          case (b) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              b
                          end,
                          case (c) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              c
                          end,
                          case (d) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              d
                          end,
                          case (e) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              e
                          end,
                          case (f) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              f
                          end,
                          case (g) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              g
                          end>>)
  end

  defp langtag_extlang(<<?-, a, b, c, d, e, f, r :: bits>>, acc, t, _)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((a === ?a or a === ?b) or a === ?c) or a === ?d) or a === ?e) or a === ?f) or a === ?g) or a === ?h) or a === ?i) or a === ?j) or a === ?k) or a === ?l) or a === ?m) or a === ?n) or a === ?o) or a === ?p) or a === ?q) or a === ?r) or a === ?s) or a === ?t) or a === ?u) or a === ?v) or a === ?w) or a === ?x) or a === ?y) or a === ?z) or a === ?A) or a === ?B) or a === ?C) or a === ?D) or a === ?E) or a === ?F) or a === ?G) or a === ?H) or a === ?I) or a === ?J) or a === ?K) or a === ?L) or a === ?M) or a === ?N) or a === ?O) or a === ?P) or a === ?Q) or a === ?R) or a === ?S) or a === ?T) or a === ?U) or a === ?V) or a === ?W) or a === ?X) or a === ?Y) or a === ?Z) or a === ?0) or a === ?1) or a === ?2) or a === ?3) or a === ?4) or a === ?5) or a === ?6) or a === ?7) or a === ?8) or a === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((b === ?a or b === ?b) or b === ?c) or b === ?d) or b === ?e) or b === ?f) or b === ?g) or b === ?h) or b === ?i) or b === ?j) or b === ?k) or b === ?l) or b === ?m) or b === ?n) or b === ?o) or b === ?p) or b === ?q) or b === ?r) or b === ?s) or b === ?t) or b === ?u) or b === ?v) or b === ?w) or b === ?x) or b === ?y) or b === ?z) or b === ?A) or b === ?B) or b === ?C) or b === ?D) or b === ?E) or b === ?F) or b === ?G) or b === ?H) or b === ?I) or b === ?J) or b === ?K) or b === ?L) or b === ?M) or b === ?N) or b === ?O) or b === ?P) or b === ?Q) or b === ?R) or b === ?S) or b === ?T) or b === ?U) or b === ?V) or b === ?W) or b === ?X) or b === ?Y) or b === ?Z) or b === ?0) or b === ?1) or b === ?2) or b === ?3) or b === ?4) or b === ?5) or b === ?6) or b === ?7) or b === ?8) or b === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((d === ?a or d === ?b) or d === ?c) or d === ?d) or d === ?e) or d === ?f) or d === ?g) or d === ?h) or d === ?i) or d === ?j) or d === ?k) or d === ?l) or d === ?m) or d === ?n) or d === ?o) or d === ?p) or d === ?q) or d === ?r) or d === ?s) or d === ?t) or d === ?u) or d === ?v) or d === ?w) or d === ?x) or d === ?y) or d === ?z) or d === ?A) or d === ?B) or d === ?C) or d === ?D) or d === ?E) or d === ?F) or d === ?G) or d === ?H) or d === ?I) or d === ?J) or d === ?K) or d === ?L) or d === ?M) or d === ?N) or d === ?O) or d === ?P) or d === ?Q) or d === ?R) or d === ?S) or d === ?T) or d === ?U) or d === ?V) or d === ?W) or d === ?X) or d === ?Y) or d === ?Z) or d === ?0) or d === ?1) or d === ?2) or d === ?3) or d === ?4) or d === ?5) or d === ?6) or d === ?7) or d === ?8) or d === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((e === ?a or e === ?b) or e === ?c) or e === ?d) or e === ?e) or e === ?f) or e === ?g) or e === ?h) or e === ?i) or e === ?j) or e === ?k) or e === ?l) or e === ?m) or e === ?n) or e === ?o) or e === ?p) or e === ?q) or e === ?r) or e === ?s) or e === ?t) or e === ?u) or e === ?v) or e === ?w) or e === ?x) or e === ?y) or e === ?z) or e === ?A) or e === ?B) or e === ?C) or e === ?D) or e === ?E) or e === ?F) or e === ?G) or e === ?H) or e === ?I) or e === ?J) or e === ?K) or e === ?L) or e === ?M) or e === ?N) or e === ?O) or e === ?P) or e === ?Q) or e === ?R) or e === ?S) or e === ?T) or e === ?U) or e === ?V) or e === ?W) or e === ?X) or e === ?Y) or e === ?Z) or e === ?0) or e === ?1) or e === ?2) or e === ?3) or e === ?4) or e === ?5) or e === ?6) or e === ?7) or e === ?8) or e === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((f === ?a or f === ?b) or f === ?c) or f === ?d) or f === ?e) or f === ?f) or f === ?g) or f === ?h) or f === ?i) or f === ?j) or f === ?k) or f === ?l) or f === ?m) or f === ?n) or f === ?o) or f === ?p) or f === ?q) or f === ?r) or f === ?s) or f === ?t) or f === ?u) or f === ?v) or f === ?w) or f === ?x) or f === ?y) or f === ?z) or f === ?A) or f === ?B) or f === ?C) or f === ?D) or f === ?E) or f === ?F) or f === ?G) or f === ?H) or f === ?I) or f === ?J) or f === ?K) or f === ?L) or f === ?M) or f === ?N) or f === ?O) or f === ?P) or f === ?Q) or f === ?R) or f === ?S) or f === ?T) or f === ?U) or f === ?V) or f === ?W) or f === ?X) or f === ?Y) or f === ?Z) or f === ?0) or f === ?1) or f === ?2) or f === ?3) or f === ?4) or f === ?5) or f === ?6) or f === ?7) or f === ?8) or f === ?9) do
    langtag_variant(r, acc,
                      <<t :: binary, ?-,
                          case (a) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              a
                          end,
                          case (b) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              b
                          end,
                          case (c) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              c
                          end,
                          case (d) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              d
                          end,
                          case (e) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              e
                          end,
                          case (f) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              f
                          end>>)
  end

  defp langtag_extlang(<<?-, a, b, c, d, e, r :: bits>>, acc, t, _)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((a === ?a or a === ?b) or a === ?c) or a === ?d) or a === ?e) or a === ?f) or a === ?g) or a === ?h) or a === ?i) or a === ?j) or a === ?k) or a === ?l) or a === ?m) or a === ?n) or a === ?o) or a === ?p) or a === ?q) or a === ?r) or a === ?s) or a === ?t) or a === ?u) or a === ?v) or a === ?w) or a === ?x) or a === ?y) or a === ?z) or a === ?A) or a === ?B) or a === ?C) or a === ?D) or a === ?E) or a === ?F) or a === ?G) or a === ?H) or a === ?I) or a === ?J) or a === ?K) or a === ?L) or a === ?M) or a === ?N) or a === ?O) or a === ?P) or a === ?Q) or a === ?R) or a === ?S) or a === ?T) or a === ?U) or a === ?V) or a === ?W) or a === ?X) or a === ?Y) or a === ?Z) or a === ?0) or a === ?1) or a === ?2) or a === ?3) or a === ?4) or a === ?5) or a === ?6) or a === ?7) or a === ?8) or a === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((b === ?a or b === ?b) or b === ?c) or b === ?d) or b === ?e) or b === ?f) or b === ?g) or b === ?h) or b === ?i) or b === ?j) or b === ?k) or b === ?l) or b === ?m) or b === ?n) or b === ?o) or b === ?p) or b === ?q) or b === ?r) or b === ?s) or b === ?t) or b === ?u) or b === ?v) or b === ?w) or b === ?x) or b === ?y) or b === ?z) or b === ?A) or b === ?B) or b === ?C) or b === ?D) or b === ?E) or b === ?F) or b === ?G) or b === ?H) or b === ?I) or b === ?J) or b === ?K) or b === ?L) or b === ?M) or b === ?N) or b === ?O) or b === ?P) or b === ?Q) or b === ?R) or b === ?S) or b === ?T) or b === ?U) or b === ?V) or b === ?W) or b === ?X) or b === ?Y) or b === ?Z) or b === ?0) or b === ?1) or b === ?2) or b === ?3) or b === ?4) or b === ?5) or b === ?6) or b === ?7) or b === ?8) or b === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((d === ?a or d === ?b) or d === ?c) or d === ?d) or d === ?e) or d === ?f) or d === ?g) or d === ?h) or d === ?i) or d === ?j) or d === ?k) or d === ?l) or d === ?m) or d === ?n) or d === ?o) or d === ?p) or d === ?q) or d === ?r) or d === ?s) or d === ?t) or d === ?u) or d === ?v) or d === ?w) or d === ?x) or d === ?y) or d === ?z) or d === ?A) or d === ?B) or d === ?C) or d === ?D) or d === ?E) or d === ?F) or d === ?G) or d === ?H) or d === ?I) or d === ?J) or d === ?K) or d === ?L) or d === ?M) or d === ?N) or d === ?O) or d === ?P) or d === ?Q) or d === ?R) or d === ?S) or d === ?T) or d === ?U) or d === ?V) or d === ?W) or d === ?X) or d === ?Y) or d === ?Z) or d === ?0) or d === ?1) or d === ?2) or d === ?3) or d === ?4) or d === ?5) or d === ?6) or d === ?7) or d === ?8) or d === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((e === ?a or e === ?b) or e === ?c) or e === ?d) or e === ?e) or e === ?f) or e === ?g) or e === ?h) or e === ?i) or e === ?j) or e === ?k) or e === ?l) or e === ?m) or e === ?n) or e === ?o) or e === ?p) or e === ?q) or e === ?r) or e === ?s) or e === ?t) or e === ?u) or e === ?v) or e === ?w) or e === ?x) or e === ?y) or e === ?z) or e === ?A) or e === ?B) or e === ?C) or e === ?D) or e === ?E) or e === ?F) or e === ?G) or e === ?H) or e === ?I) or e === ?J) or e === ?K) or e === ?L) or e === ?M) or e === ?N) or e === ?O) or e === ?P) or e === ?Q) or e === ?R) or e === ?S) or e === ?T) or e === ?U) or e === ?V) or e === ?W) or e === ?X) or e === ?Y) or e === ?Z) or e === ?0) or e === ?1) or e === ?2) or e === ?3) or e === ?4) or e === ?5) or e === ?6) or e === ?7) or e === ?8) or e === ?9) do
    langtag_variant(r, acc,
                      <<t :: binary, ?-,
                          case (a) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              a
                          end,
                          case (b) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              b
                          end,
                          case (c) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              c
                          end,
                          case (d) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              d
                          end,
                          case (e) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              e
                          end>>)
  end

  defp langtag_extlang(<<?-, a, b, c, d, r :: bits>>, acc, t, _)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((a === ?a or a === ?b) or a === ?c) or a === ?d) or a === ?e) or a === ?f) or a === ?g) or a === ?h) or a === ?i) or a === ?j) or a === ?k) or a === ?l) or a === ?m) or a === ?n) or a === ?o) or a === ?p) or a === ?q) or a === ?r) or a === ?s) or a === ?t) or a === ?u) or a === ?v) or a === ?w) or a === ?x) or a === ?y) or a === ?z) or a === ?A) or a === ?B) or a === ?C) or a === ?D) or a === ?E) or a === ?F) or a === ?G) or a === ?H) or a === ?I) or a === ?J) or a === ?K) or a === ?L) or a === ?M) or a === ?N) or a === ?O) or a === ?P) or a === ?Q) or a === ?R) or a === ?S) or a === ?T) or a === ?U) or a === ?V) or a === ?W) or a === ?X) or a === ?Y) or a === ?Z and
              ((((((((((((((((((((((((((((((((((((((((((((((((((b === ?a or b === ?b) or b === ?c) or b === ?d) or b === ?e) or b === ?f) or b === ?g) or b === ?h) or b === ?i) or b === ?j) or b === ?k) or b === ?l) or b === ?m) or b === ?n) or b === ?o) or b === ?p) or b === ?q) or b === ?r) or b === ?s) or b === ?t) or b === ?u) or b === ?v) or b === ?w) or b === ?x) or b === ?y) or b === ?z) or b === ?A) or b === ?B) or b === ?C) or b === ?D) or b === ?E) or b === ?F) or b === ?G) or b === ?H) or b === ?I) or b === ?J) or b === ?K) or b === ?L) or b === ?M) or b === ?N) or b === ?O) or b === ?P) or b === ?Q) or b === ?R) or b === ?S) or b === ?T) or b === ?U) or b === ?V) or b === ?W) or b === ?X) or b === ?Y) or b === ?Z and
              ((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z and
              ((((((((((((((((((((((((((((((((((((((((((((((((((d === ?a or d === ?b) or d === ?c) or d === ?d) or d === ?e) or d === ?f) or d === ?g) or d === ?h) or d === ?i) or d === ?j) or d === ?k) or d === ?l) or d === ?m) or d === ?n) or d === ?o) or d === ?p) or d === ?q) or d === ?r) or d === ?s) or d === ?t) or d === ?u) or d === ?v) or d === ?w) or d === ?x) or d === ?y) or d === ?z) or d === ?A) or d === ?B) or d === ?C) or d === ?D) or d === ?E) or d === ?F) or d === ?G) or d === ?H) or d === ?I) or d === ?J) or d === ?K) or d === ?L) or d === ?M) or d === ?N) or d === ?O) or d === ?P) or d === ?Q) or d === ?R) or d === ?S) or d === ?T) or d === ?U) or d === ?V) or d === ?W) or d === ?X) or d === ?Y) or d === ?Z) do
    langtag_region(r, acc,
                     <<t :: binary, ?-,
                         case (a) do
                           ?A ->
                             ?a
                           ?B ->
                             ?b
                           ?C ->
                             ?c
                           ?D ->
                             ?d
                           ?E ->
                             ?e
                           ?F ->
                             ?f
                           ?G ->
                             ?g
                           ?H ->
                             ?h
                           ?I ->
                             ?i
                           ?J ->
                             ?j
                           ?K ->
                             ?k
                           ?L ->
                             ?l
                           ?M ->
                             ?m
                           ?N ->
                             ?n
                           ?O ->
                             ?o
                           ?P ->
                             ?p
                           ?Q ->
                             ?q
                           ?R ->
                             ?r
                           ?S ->
                             ?s
                           ?T ->
                             ?t
                           ?U ->
                             ?u
                           ?V ->
                             ?v
                           ?W ->
                             ?w
                           ?X ->
                             ?x
                           ?Y ->
                             ?y
                           ?Z ->
                             ?z
                           _ ->
                             a
                         end,
                         case (b) do
                           ?A ->
                             ?a
                           ?B ->
                             ?b
                           ?C ->
                             ?c
                           ?D ->
                             ?d
                           ?E ->
                             ?e
                           ?F ->
                             ?f
                           ?G ->
                             ?g
                           ?H ->
                             ?h
                           ?I ->
                             ?i
                           ?J ->
                             ?j
                           ?K ->
                             ?k
                           ?L ->
                             ?l
                           ?M ->
                             ?m
                           ?N ->
                             ?n
                           ?O ->
                             ?o
                           ?P ->
                             ?p
                           ?Q ->
                             ?q
                           ?R ->
                             ?r
                           ?S ->
                             ?s
                           ?T ->
                             ?t
                           ?U ->
                             ?u
                           ?V ->
                             ?v
                           ?W ->
                             ?w
                           ?X ->
                             ?x
                           ?Y ->
                             ?y
                           ?Z ->
                             ?z
                           _ ->
                             b
                         end,
                         case (c) do
                           ?A ->
                             ?a
                           ?B ->
                             ?b
                           ?C ->
                             ?c
                           ?D ->
                             ?d
                           ?E ->
                             ?e
                           ?F ->
                             ?f
                           ?G ->
                             ?g
                           ?H ->
                             ?h
                           ?I ->
                             ?i
                           ?J ->
                             ?j
                           ?K ->
                             ?k
                           ?L ->
                             ?l
                           ?M ->
                             ?m
                           ?N ->
                             ?n
                           ?O ->
                             ?o
                           ?P ->
                             ?p
                           ?Q ->
                             ?q
                           ?R ->
                             ?r
                           ?S ->
                             ?s
                           ?T ->
                             ?t
                           ?U ->
                             ?u
                           ?V ->
                             ?v
                           ?W ->
                             ?w
                           ?X ->
                             ?x
                           ?Y ->
                             ?y
                           ?Z ->
                             ?z
                           _ ->
                             c
                         end,
                         case (d) do
                           ?A ->
                             ?a
                           ?B ->
                             ?b
                           ?C ->
                             ?c
                           ?D ->
                             ?d
                           ?E ->
                             ?e
                           ?F ->
                             ?f
                           ?G ->
                             ?g
                           ?H ->
                             ?h
                           ?I ->
                             ?i
                           ?J ->
                             ?j
                           ?K ->
                             ?k
                           ?L ->
                             ?l
                           ?M ->
                             ?m
                           ?N ->
                             ?n
                           ?O ->
                             ?o
                           ?P ->
                             ?p
                           ?Q ->
                             ?q
                           ?R ->
                             ?r
                           ?S ->
                             ?s
                           ?T ->
                             ?t
                           ?U ->
                             ?u
                           ?V ->
                             ?v
                           ?W ->
                             ?w
                           ?X ->
                             ?x
                           ?Y ->
                             ?y
                           ?Z ->
                             ?z
                           _ ->
                             d
                         end>>)
  end

  defp langtag_extlang(<<?-, a, b, c, r :: bits>>, acc, t, n)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((a === ?a or a === ?b) or a === ?c) or a === ?d) or a === ?e) or a === ?f) or a === ?g) or a === ?h) or a === ?i) or a === ?j) or a === ?k) or a === ?l) or a === ?m) or a === ?n) or a === ?o) or a === ?p) or a === ?q) or a === ?r) or a === ?s) or a === ?t) or a === ?u) or a === ?v) or a === ?w) or a === ?x) or a === ?y) or a === ?z) or a === ?A) or a === ?B) or a === ?C) or a === ?D) or a === ?E) or a === ?F) or a === ?G) or a === ?H) or a === ?I) or a === ?J) or a === ?K) or a === ?L) or a === ?M) or a === ?N) or a === ?O) or a === ?P) or a === ?Q) or a === ?R) or a === ?S) or a === ?T) or a === ?U) or a === ?V) or a === ?W) or a === ?X) or a === ?Y) or a === ?Z and
              ((((((((((((((((((((((((((((((((((((((((((((((((((b === ?a or b === ?b) or b === ?c) or b === ?d) or b === ?e) or b === ?f) or b === ?g) or b === ?h) or b === ?i) or b === ?j) or b === ?k) or b === ?l) or b === ?m) or b === ?n) or b === ?o) or b === ?p) or b === ?q) or b === ?r) or b === ?s) or b === ?t) or b === ?u) or b === ?v) or b === ?w) or b === ?x) or b === ?y) or b === ?z) or b === ?A) or b === ?B) or b === ?C) or b === ?D) or b === ?E) or b === ?F) or b === ?G) or b === ?H) or b === ?I) or b === ?J) or b === ?K) or b === ?L) or b === ?M) or b === ?N) or b === ?O) or b === ?P) or b === ?Q) or b === ?R) or b === ?S) or b === ?T) or b === ?U) or b === ?V) or b === ?W) or b === ?X) or b === ?Y) or b === ?Z and
              ((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) do
    case (n) do
      2 ->
        langtag_script(r, acc,
                         <<t :: binary, ?-,
                             case (a) do
                               ?A ->
                                 ?a
                               ?B ->
                                 ?b
                               ?C ->
                                 ?c
                               ?D ->
                                 ?d
                               ?E ->
                                 ?e
                               ?F ->
                                 ?f
                               ?G ->
                                 ?g
                               ?H ->
                                 ?h
                               ?I ->
                                 ?i
                               ?J ->
                                 ?j
                               ?K ->
                                 ?k
                               ?L ->
                                 ?l
                               ?M ->
                                 ?m
                               ?N ->
                                 ?n
                               ?O ->
                                 ?o
                               ?P ->
                                 ?p
                               ?Q ->
                                 ?q
                               ?R ->
                                 ?r
                               ?S ->
                                 ?s
                               ?T ->
                                 ?t
                               ?U ->
                                 ?u
                               ?V ->
                                 ?v
                               ?W ->
                                 ?w
                               ?X ->
                                 ?x
                               ?Y ->
                                 ?y
                               ?Z ->
                                 ?z
                               _ ->
                                 a
                             end,
                             case (b) do
                               ?A ->
                                 ?a
                               ?B ->
                                 ?b
                               ?C ->
                                 ?c
                               ?D ->
                                 ?d
                               ?E ->
                                 ?e
                               ?F ->
                                 ?f
                               ?G ->
                                 ?g
                               ?H ->
                                 ?h
                               ?I ->
                                 ?i
                               ?J ->
                                 ?j
                               ?K ->
                                 ?k
                               ?L ->
                                 ?l
                               ?M ->
                                 ?m
                               ?N ->
                                 ?n
                               ?O ->
                                 ?o
                               ?P ->
                                 ?p
                               ?Q ->
                                 ?q
                               ?R ->
                                 ?r
                               ?S ->
                                 ?s
                               ?T ->
                                 ?t
                               ?U ->
                                 ?u
                               ?V ->
                                 ?v
                               ?W ->
                                 ?w
                               ?X ->
                                 ?x
                               ?Y ->
                                 ?y
                               ?Z ->
                                 ?z
                               _ ->
                                 b
                             end,
                             case (c) do
                               ?A ->
                                 ?a
                               ?B ->
                                 ?b
                               ?C ->
                                 ?c
                               ?D ->
                                 ?d
                               ?E ->
                                 ?e
                               ?F ->
                                 ?f
                               ?G ->
                                 ?g
                               ?H ->
                                 ?h
                               ?I ->
                                 ?i
                               ?J ->
                                 ?j
                               ?K ->
                                 ?k
                               ?L ->
                                 ?l
                               ?M ->
                                 ?m
                               ?N ->
                                 ?n
                               ?O ->
                                 ?o
                               ?P ->
                                 ?p
                               ?Q ->
                                 ?q
                               ?R ->
                                 ?r
                               ?S ->
                                 ?s
                               ?T ->
                                 ?t
                               ?U ->
                                 ?u
                               ?V ->
                                 ?v
                               ?W ->
                                 ?w
                               ?X ->
                                 ?x
                               ?Y ->
                                 ?y
                               ?Z ->
                                 ?z
                               _ ->
                                 c
                             end>>)
      _ ->
        langtag_extlang(r, acc,
                          <<t :: binary, ?-,
                              case (a) do
                                ?A ->
                                  ?a
                                ?B ->
                                  ?b
                                ?C ->
                                  ?c
                                ?D ->
                                  ?d
                                ?E ->
                                  ?e
                                ?F ->
                                  ?f
                                ?G ->
                                  ?g
                                ?H ->
                                  ?h
                                ?I ->
                                  ?i
                                ?J ->
                                  ?j
                                ?K ->
                                  ?k
                                ?L ->
                                  ?l
                                ?M ->
                                  ?m
                                ?N ->
                                  ?n
                                ?O ->
                                  ?o
                                ?P ->
                                  ?p
                                ?Q ->
                                  ?q
                                ?R ->
                                  ?r
                                ?S ->
                                  ?s
                                ?T ->
                                  ?t
                                ?U ->
                                  ?u
                                ?V ->
                                  ?v
                                ?W ->
                                  ?w
                                ?X ->
                                  ?x
                                ?Y ->
                                  ?y
                                ?Z ->
                                  ?z
                                _ ->
                                  a
                              end,
                              case (b) do
                                ?A ->
                                  ?a
                                ?B ->
                                  ?b
                                ?C ->
                                  ?c
                                ?D ->
                                  ?d
                                ?E ->
                                  ?e
                                ?F ->
                                  ?f
                                ?G ->
                                  ?g
                                ?H ->
                                  ?h
                                ?I ->
                                  ?i
                                ?J ->
                                  ?j
                                ?K ->
                                  ?k
                                ?L ->
                                  ?l
                                ?M ->
                                  ?m
                                ?N ->
                                  ?n
                                ?O ->
                                  ?o
                                ?P ->
                                  ?p
                                ?Q ->
                                  ?q
                                ?R ->
                                  ?r
                                ?S ->
                                  ?s
                                ?T ->
                                  ?t
                                ?U ->
                                  ?u
                                ?V ->
                                  ?v
                                ?W ->
                                  ?w
                                ?X ->
                                  ?x
                                ?Y ->
                                  ?y
                                ?Z ->
                                  ?z
                                _ ->
                                  b
                              end,
                              case (c) do
                                ?A ->
                                  ?a
                                ?B ->
                                  ?b
                                ?C ->
                                  ?c
                                ?D ->
                                  ?d
                                ?E ->
                                  ?e
                                ?F ->
                                  ?f
                                ?G ->
                                  ?g
                                ?H ->
                                  ?h
                                ?I ->
                                  ?i
                                ?J ->
                                  ?j
                                ?K ->
                                  ?k
                                ?L ->
                                  ?l
                                ?M ->
                                  ?m
                                ?N ->
                                  ?n
                                ?O ->
                                  ?o
                                ?P ->
                                  ?p
                                ?Q ->
                                  ?q
                                ?R ->
                                  ?r
                                ?S ->
                                  ?s
                                ?T ->
                                  ?t
                                ?U ->
                                  ?u
                                ?V ->
                                  ?v
                                ?W ->
                                  ?w
                                ?X ->
                                  ?x
                                ?Y ->
                                  ?y
                                ?Z ->
                                  ?z
                                _ ->
                                  c
                              end>>,
                          n + 1)
    end
  end

  defp langtag_extlang(r, acc, t, _) do
    langtag_region(r, acc, t)
  end

  defp langtag_script(<<?-, a, b, c, d, e, f, g, h, r :: bits>>, acc,
            t)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((a === ?a or a === ?b) or a === ?c) or a === ?d) or a === ?e) or a === ?f) or a === ?g) or a === ?h) or a === ?i) or a === ?j) or a === ?k) or a === ?l) or a === ?m) or a === ?n) or a === ?o) or a === ?p) or a === ?q) or a === ?r) or a === ?s) or a === ?t) or a === ?u) or a === ?v) or a === ?w) or a === ?x) or a === ?y) or a === ?z) or a === ?A) or a === ?B) or a === ?C) or a === ?D) or a === ?E) or a === ?F) or a === ?G) or a === ?H) or a === ?I) or a === ?J) or a === ?K) or a === ?L) or a === ?M) or a === ?N) or a === ?O) or a === ?P) or a === ?Q) or a === ?R) or a === ?S) or a === ?T) or a === ?U) or a === ?V) or a === ?W) or a === ?X) or a === ?Y) or a === ?Z) or a === ?0) or a === ?1) or a === ?2) or a === ?3) or a === ?4) or a === ?5) or a === ?6) or a === ?7) or a === ?8) or a === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((b === ?a or b === ?b) or b === ?c) or b === ?d) or b === ?e) or b === ?f) or b === ?g) or b === ?h) or b === ?i) or b === ?j) or b === ?k) or b === ?l) or b === ?m) or b === ?n) or b === ?o) or b === ?p) or b === ?q) or b === ?r) or b === ?s) or b === ?t) or b === ?u) or b === ?v) or b === ?w) or b === ?x) or b === ?y) or b === ?z) or b === ?A) or b === ?B) or b === ?C) or b === ?D) or b === ?E) or b === ?F) or b === ?G) or b === ?H) or b === ?I) or b === ?J) or b === ?K) or b === ?L) or b === ?M) or b === ?N) or b === ?O) or b === ?P) or b === ?Q) or b === ?R) or b === ?S) or b === ?T) or b === ?U) or b === ?V) or b === ?W) or b === ?X) or b === ?Y) or b === ?Z) or b === ?0) or b === ?1) or b === ?2) or b === ?3) or b === ?4) or b === ?5) or b === ?6) or b === ?7) or b === ?8) or b === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((d === ?a or d === ?b) or d === ?c) or d === ?d) or d === ?e) or d === ?f) or d === ?g) or d === ?h) or d === ?i) or d === ?j) or d === ?k) or d === ?l) or d === ?m) or d === ?n) or d === ?o) or d === ?p) or d === ?q) or d === ?r) or d === ?s) or d === ?t) or d === ?u) or d === ?v) or d === ?w) or d === ?x) or d === ?y) or d === ?z) or d === ?A) or d === ?B) or d === ?C) or d === ?D) or d === ?E) or d === ?F) or d === ?G) or d === ?H) or d === ?I) or d === ?J) or d === ?K) or d === ?L) or d === ?M) or d === ?N) or d === ?O) or d === ?P) or d === ?Q) or d === ?R) or d === ?S) or d === ?T) or d === ?U) or d === ?V) or d === ?W) or d === ?X) or d === ?Y) or d === ?Z) or d === ?0) or d === ?1) or d === ?2) or d === ?3) or d === ?4) or d === ?5) or d === ?6) or d === ?7) or d === ?8) or d === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((e === ?a or e === ?b) or e === ?c) or e === ?d) or e === ?e) or e === ?f) or e === ?g) or e === ?h) or e === ?i) or e === ?j) or e === ?k) or e === ?l) or e === ?m) or e === ?n) or e === ?o) or e === ?p) or e === ?q) or e === ?r) or e === ?s) or e === ?t) or e === ?u) or e === ?v) or e === ?w) or e === ?x) or e === ?y) or e === ?z) or e === ?A) or e === ?B) or e === ?C) or e === ?D) or e === ?E) or e === ?F) or e === ?G) or e === ?H) or e === ?I) or e === ?J) or e === ?K) or e === ?L) or e === ?M) or e === ?N) or e === ?O) or e === ?P) or e === ?Q) or e === ?R) or e === ?S) or e === ?T) or e === ?U) or e === ?V) or e === ?W) or e === ?X) or e === ?Y) or e === ?Z) or e === ?0) or e === ?1) or e === ?2) or e === ?3) or e === ?4) or e === ?5) or e === ?6) or e === ?7) or e === ?8) or e === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((f === ?a or f === ?b) or f === ?c) or f === ?d) or f === ?e) or f === ?f) or f === ?g) or f === ?h) or f === ?i) or f === ?j) or f === ?k) or f === ?l) or f === ?m) or f === ?n) or f === ?o) or f === ?p) or f === ?q) or f === ?r) or f === ?s) or f === ?t) or f === ?u) or f === ?v) or f === ?w) or f === ?x) or f === ?y) or f === ?z) or f === ?A) or f === ?B) or f === ?C) or f === ?D) or f === ?E) or f === ?F) or f === ?G) or f === ?H) or f === ?I) or f === ?J) or f === ?K) or f === ?L) or f === ?M) or f === ?N) or f === ?O) or f === ?P) or f === ?Q) or f === ?R) or f === ?S) or f === ?T) or f === ?U) or f === ?V) or f === ?W) or f === ?X) or f === ?Y) or f === ?Z) or f === ?0) or f === ?1) or f === ?2) or f === ?3) or f === ?4) or f === ?5) or f === ?6) or f === ?7) or f === ?8) or f === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((g === ?a or g === ?b) or g === ?c) or g === ?d) or g === ?e) or g === ?f) or g === ?g) or g === ?h) or g === ?i) or g === ?j) or g === ?k) or g === ?l) or g === ?m) or g === ?n) or g === ?o) or g === ?p) or g === ?q) or g === ?r) or g === ?s) or g === ?t) or g === ?u) or g === ?v) or g === ?w) or g === ?x) or g === ?y) or g === ?z) or g === ?A) or g === ?B) or g === ?C) or g === ?D) or g === ?E) or g === ?F) or g === ?G) or g === ?H) or g === ?I) or g === ?J) or g === ?K) or g === ?L) or g === ?M) or g === ?N) or g === ?O) or g === ?P) or g === ?Q) or g === ?R) or g === ?S) or g === ?T) or g === ?U) or g === ?V) or g === ?W) or g === ?X) or g === ?Y) or g === ?Z) or g === ?0) or g === ?1) or g === ?2) or g === ?3) or g === ?4) or g === ?5) or g === ?6) or g === ?7) or g === ?8) or g === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((h === ?a or h === ?b) or h === ?c) or h === ?d) or h === ?e) or h === ?f) or h === ?g) or h === ?h) or h === ?i) or h === ?j) or h === ?k) or h === ?l) or h === ?m) or h === ?n) or h === ?o) or h === ?p) or h === ?q) or h === ?r) or h === ?s) or h === ?t) or h === ?u) or h === ?v) or h === ?w) or h === ?x) or h === ?y) or h === ?z) or h === ?A) or h === ?B) or h === ?C) or h === ?D) or h === ?E) or h === ?F) or h === ?G) or h === ?H) or h === ?I) or h === ?J) or h === ?K) or h === ?L) or h === ?M) or h === ?N) or h === ?O) or h === ?P) or h === ?Q) or h === ?R) or h === ?S) or h === ?T) or h === ?U) or h === ?V) or h === ?W) or h === ?X) or h === ?Y) or h === ?Z) or h === ?0) or h === ?1) or h === ?2) or h === ?3) or h === ?4) or h === ?5) or h === ?6) or h === ?7) or h === ?8) or h === ?9) do
    langtag_variant(r, acc,
                      <<t :: binary, ?-,
                          case (a) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              a
                          end,
                          case (b) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              b
                          end,
                          case (c) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              c
                          end,
                          case (d) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              d
                          end,
                          case (e) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              e
                          end,
                          case (f) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              f
                          end,
                          case (g) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              g
                          end,
                          case (h) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              h
                          end>>)
  end

  defp langtag_script(<<?-, a, b, c, d, e, f, g, r :: bits>>, acc, t)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((a === ?a or a === ?b) or a === ?c) or a === ?d) or a === ?e) or a === ?f) or a === ?g) or a === ?h) or a === ?i) or a === ?j) or a === ?k) or a === ?l) or a === ?m) or a === ?n) or a === ?o) or a === ?p) or a === ?q) or a === ?r) or a === ?s) or a === ?t) or a === ?u) or a === ?v) or a === ?w) or a === ?x) or a === ?y) or a === ?z) or a === ?A) or a === ?B) or a === ?C) or a === ?D) or a === ?E) or a === ?F) or a === ?G) or a === ?H) or a === ?I) or a === ?J) or a === ?K) or a === ?L) or a === ?M) or a === ?N) or a === ?O) or a === ?P) or a === ?Q) or a === ?R) or a === ?S) or a === ?T) or a === ?U) or a === ?V) or a === ?W) or a === ?X) or a === ?Y) or a === ?Z) or a === ?0) or a === ?1) or a === ?2) or a === ?3) or a === ?4) or a === ?5) or a === ?6) or a === ?7) or a === ?8) or a === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((b === ?a or b === ?b) or b === ?c) or b === ?d) or b === ?e) or b === ?f) or b === ?g) or b === ?h) or b === ?i) or b === ?j) or b === ?k) or b === ?l) or b === ?m) or b === ?n) or b === ?o) or b === ?p) or b === ?q) or b === ?r) or b === ?s) or b === ?t) or b === ?u) or b === ?v) or b === ?w) or b === ?x) or b === ?y) or b === ?z) or b === ?A) or b === ?B) or b === ?C) or b === ?D) or b === ?E) or b === ?F) or b === ?G) or b === ?H) or b === ?I) or b === ?J) or b === ?K) or b === ?L) or b === ?M) or b === ?N) or b === ?O) or b === ?P) or b === ?Q) or b === ?R) or b === ?S) or b === ?T) or b === ?U) or b === ?V) or b === ?W) or b === ?X) or b === ?Y) or b === ?Z) or b === ?0) or b === ?1) or b === ?2) or b === ?3) or b === ?4) or b === ?5) or b === ?6) or b === ?7) or b === ?8) or b === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((d === ?a or d === ?b) or d === ?c) or d === ?d) or d === ?e) or d === ?f) or d === ?g) or d === ?h) or d === ?i) or d === ?j) or d === ?k) or d === ?l) or d === ?m) or d === ?n) or d === ?o) or d === ?p) or d === ?q) or d === ?r) or d === ?s) or d === ?t) or d === ?u) or d === ?v) or d === ?w) or d === ?x) or d === ?y) or d === ?z) or d === ?A) or d === ?B) or d === ?C) or d === ?D) or d === ?E) or d === ?F) or d === ?G) or d === ?H) or d === ?I) or d === ?J) or d === ?K) or d === ?L) or d === ?M) or d === ?N) or d === ?O) or d === ?P) or d === ?Q) or d === ?R) or d === ?S) or d === ?T) or d === ?U) or d === ?V) or d === ?W) or d === ?X) or d === ?Y) or d === ?Z) or d === ?0) or d === ?1) or d === ?2) or d === ?3) or d === ?4) or d === ?5) or d === ?6) or d === ?7) or d === ?8) or d === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((e === ?a or e === ?b) or e === ?c) or e === ?d) or e === ?e) or e === ?f) or e === ?g) or e === ?h) or e === ?i) or e === ?j) or e === ?k) or e === ?l) or e === ?m) or e === ?n) or e === ?o) or e === ?p) or e === ?q) or e === ?r) or e === ?s) or e === ?t) or e === ?u) or e === ?v) or e === ?w) or e === ?x) or e === ?y) or e === ?z) or e === ?A) or e === ?B) or e === ?C) or e === ?D) or e === ?E) or e === ?F) or e === ?G) or e === ?H) or e === ?I) or e === ?J) or e === ?K) or e === ?L) or e === ?M) or e === ?N) or e === ?O) or e === ?P) or e === ?Q) or e === ?R) or e === ?S) or e === ?T) or e === ?U) or e === ?V) or e === ?W) or e === ?X) or e === ?Y) or e === ?Z) or e === ?0) or e === ?1) or e === ?2) or e === ?3) or e === ?4) or e === ?5) or e === ?6) or e === ?7) or e === ?8) or e === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((f === ?a or f === ?b) or f === ?c) or f === ?d) or f === ?e) or f === ?f) or f === ?g) or f === ?h) or f === ?i) or f === ?j) or f === ?k) or f === ?l) or f === ?m) or f === ?n) or f === ?o) or f === ?p) or f === ?q) or f === ?r) or f === ?s) or f === ?t) or f === ?u) or f === ?v) or f === ?w) or f === ?x) or f === ?y) or f === ?z) or f === ?A) or f === ?B) or f === ?C) or f === ?D) or f === ?E) or f === ?F) or f === ?G) or f === ?H) or f === ?I) or f === ?J) or f === ?K) or f === ?L) or f === ?M) or f === ?N) or f === ?O) or f === ?P) or f === ?Q) or f === ?R) or f === ?S) or f === ?T) or f === ?U) or f === ?V) or f === ?W) or f === ?X) or f === ?Y) or f === ?Z) or f === ?0) or f === ?1) or f === ?2) or f === ?3) or f === ?4) or f === ?5) or f === ?6) or f === ?7) or f === ?8) or f === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((g === ?a or g === ?b) or g === ?c) or g === ?d) or g === ?e) or g === ?f) or g === ?g) or g === ?h) or g === ?i) or g === ?j) or g === ?k) or g === ?l) or g === ?m) or g === ?n) or g === ?o) or g === ?p) or g === ?q) or g === ?r) or g === ?s) or g === ?t) or g === ?u) or g === ?v) or g === ?w) or g === ?x) or g === ?y) or g === ?z) or g === ?A) or g === ?B) or g === ?C) or g === ?D) or g === ?E) or g === ?F) or g === ?G) or g === ?H) or g === ?I) or g === ?J) or g === ?K) or g === ?L) or g === ?M) or g === ?N) or g === ?O) or g === ?P) or g === ?Q) or g === ?R) or g === ?S) or g === ?T) or g === ?U) or g === ?V) or g === ?W) or g === ?X) or g === ?Y) or g === ?Z) or g === ?0) or g === ?1) or g === ?2) or g === ?3) or g === ?4) or g === ?5) or g === ?6) or g === ?7) or g === ?8) or g === ?9) do
    langtag_variant(r, acc,
                      <<t :: binary, ?-,
                          case (a) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              a
                          end,
                          case (b) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              b
                          end,
                          case (c) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              c
                          end,
                          case (d) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              d
                          end,
                          case (e) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              e
                          end,
                          case (f) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              f
                          end,
                          case (g) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              g
                          end>>)
  end

  defp langtag_script(<<?-, a, b, c, d, e, f, r :: bits>>, acc, t)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((a === ?a or a === ?b) or a === ?c) or a === ?d) or a === ?e) or a === ?f) or a === ?g) or a === ?h) or a === ?i) or a === ?j) or a === ?k) or a === ?l) or a === ?m) or a === ?n) or a === ?o) or a === ?p) or a === ?q) or a === ?r) or a === ?s) or a === ?t) or a === ?u) or a === ?v) or a === ?w) or a === ?x) or a === ?y) or a === ?z) or a === ?A) or a === ?B) or a === ?C) or a === ?D) or a === ?E) or a === ?F) or a === ?G) or a === ?H) or a === ?I) or a === ?J) or a === ?K) or a === ?L) or a === ?M) or a === ?N) or a === ?O) or a === ?P) or a === ?Q) or a === ?R) or a === ?S) or a === ?T) or a === ?U) or a === ?V) or a === ?W) or a === ?X) or a === ?Y) or a === ?Z) or a === ?0) or a === ?1) or a === ?2) or a === ?3) or a === ?4) or a === ?5) or a === ?6) or a === ?7) or a === ?8) or a === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((b === ?a or b === ?b) or b === ?c) or b === ?d) or b === ?e) or b === ?f) or b === ?g) or b === ?h) or b === ?i) or b === ?j) or b === ?k) or b === ?l) or b === ?m) or b === ?n) or b === ?o) or b === ?p) or b === ?q) or b === ?r) or b === ?s) or b === ?t) or b === ?u) or b === ?v) or b === ?w) or b === ?x) or b === ?y) or b === ?z) or b === ?A) or b === ?B) or b === ?C) or b === ?D) or b === ?E) or b === ?F) or b === ?G) or b === ?H) or b === ?I) or b === ?J) or b === ?K) or b === ?L) or b === ?M) or b === ?N) or b === ?O) or b === ?P) or b === ?Q) or b === ?R) or b === ?S) or b === ?T) or b === ?U) or b === ?V) or b === ?W) or b === ?X) or b === ?Y) or b === ?Z) or b === ?0) or b === ?1) or b === ?2) or b === ?3) or b === ?4) or b === ?5) or b === ?6) or b === ?7) or b === ?8) or b === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((d === ?a or d === ?b) or d === ?c) or d === ?d) or d === ?e) or d === ?f) or d === ?g) or d === ?h) or d === ?i) or d === ?j) or d === ?k) or d === ?l) or d === ?m) or d === ?n) or d === ?o) or d === ?p) or d === ?q) or d === ?r) or d === ?s) or d === ?t) or d === ?u) or d === ?v) or d === ?w) or d === ?x) or d === ?y) or d === ?z) or d === ?A) or d === ?B) or d === ?C) or d === ?D) or d === ?E) or d === ?F) or d === ?G) or d === ?H) or d === ?I) or d === ?J) or d === ?K) or d === ?L) or d === ?M) or d === ?N) or d === ?O) or d === ?P) or d === ?Q) or d === ?R) or d === ?S) or d === ?T) or d === ?U) or d === ?V) or d === ?W) or d === ?X) or d === ?Y) or d === ?Z) or d === ?0) or d === ?1) or d === ?2) or d === ?3) or d === ?4) or d === ?5) or d === ?6) or d === ?7) or d === ?8) or d === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((e === ?a or e === ?b) or e === ?c) or e === ?d) or e === ?e) or e === ?f) or e === ?g) or e === ?h) or e === ?i) or e === ?j) or e === ?k) or e === ?l) or e === ?m) or e === ?n) or e === ?o) or e === ?p) or e === ?q) or e === ?r) or e === ?s) or e === ?t) or e === ?u) or e === ?v) or e === ?w) or e === ?x) or e === ?y) or e === ?z) or e === ?A) or e === ?B) or e === ?C) or e === ?D) or e === ?E) or e === ?F) or e === ?G) or e === ?H) or e === ?I) or e === ?J) or e === ?K) or e === ?L) or e === ?M) or e === ?N) or e === ?O) or e === ?P) or e === ?Q) or e === ?R) or e === ?S) or e === ?T) or e === ?U) or e === ?V) or e === ?W) or e === ?X) or e === ?Y) or e === ?Z) or e === ?0) or e === ?1) or e === ?2) or e === ?3) or e === ?4) or e === ?5) or e === ?6) or e === ?7) or e === ?8) or e === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((f === ?a or f === ?b) or f === ?c) or f === ?d) or f === ?e) or f === ?f) or f === ?g) or f === ?h) or f === ?i) or f === ?j) or f === ?k) or f === ?l) or f === ?m) or f === ?n) or f === ?o) or f === ?p) or f === ?q) or f === ?r) or f === ?s) or f === ?t) or f === ?u) or f === ?v) or f === ?w) or f === ?x) or f === ?y) or f === ?z) or f === ?A) or f === ?B) or f === ?C) or f === ?D) or f === ?E) or f === ?F) or f === ?G) or f === ?H) or f === ?I) or f === ?J) or f === ?K) or f === ?L) or f === ?M) or f === ?N) or f === ?O) or f === ?P) or f === ?Q) or f === ?R) or f === ?S) or f === ?T) or f === ?U) or f === ?V) or f === ?W) or f === ?X) or f === ?Y) or f === ?Z) or f === ?0) or f === ?1) or f === ?2) or f === ?3) or f === ?4) or f === ?5) or f === ?6) or f === ?7) or f === ?8) or f === ?9) do
    langtag_variant(r, acc,
                      <<t :: binary, ?-,
                          case (a) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              a
                          end,
                          case (b) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              b
                          end,
                          case (c) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              c
                          end,
                          case (d) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              d
                          end,
                          case (e) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              e
                          end,
                          case (f) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              f
                          end>>)
  end

  defp langtag_script(<<?-, a, b, c, d, e, r :: bits>>, acc, t)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((a === ?a or a === ?b) or a === ?c) or a === ?d) or a === ?e) or a === ?f) or a === ?g) or a === ?h) or a === ?i) or a === ?j) or a === ?k) or a === ?l) or a === ?m) or a === ?n) or a === ?o) or a === ?p) or a === ?q) or a === ?r) or a === ?s) or a === ?t) or a === ?u) or a === ?v) or a === ?w) or a === ?x) or a === ?y) or a === ?z) or a === ?A) or a === ?B) or a === ?C) or a === ?D) or a === ?E) or a === ?F) or a === ?G) or a === ?H) or a === ?I) or a === ?J) or a === ?K) or a === ?L) or a === ?M) or a === ?N) or a === ?O) or a === ?P) or a === ?Q) or a === ?R) or a === ?S) or a === ?T) or a === ?U) or a === ?V) or a === ?W) or a === ?X) or a === ?Y) or a === ?Z) or a === ?0) or a === ?1) or a === ?2) or a === ?3) or a === ?4) or a === ?5) or a === ?6) or a === ?7) or a === ?8) or a === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((b === ?a or b === ?b) or b === ?c) or b === ?d) or b === ?e) or b === ?f) or b === ?g) or b === ?h) or b === ?i) or b === ?j) or b === ?k) or b === ?l) or b === ?m) or b === ?n) or b === ?o) or b === ?p) or b === ?q) or b === ?r) or b === ?s) or b === ?t) or b === ?u) or b === ?v) or b === ?w) or b === ?x) or b === ?y) or b === ?z) or b === ?A) or b === ?B) or b === ?C) or b === ?D) or b === ?E) or b === ?F) or b === ?G) or b === ?H) or b === ?I) or b === ?J) or b === ?K) or b === ?L) or b === ?M) or b === ?N) or b === ?O) or b === ?P) or b === ?Q) or b === ?R) or b === ?S) or b === ?T) or b === ?U) or b === ?V) or b === ?W) or b === ?X) or b === ?Y) or b === ?Z) or b === ?0) or b === ?1) or b === ?2) or b === ?3) or b === ?4) or b === ?5) or b === ?6) or b === ?7) or b === ?8) or b === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((d === ?a or d === ?b) or d === ?c) or d === ?d) or d === ?e) or d === ?f) or d === ?g) or d === ?h) or d === ?i) or d === ?j) or d === ?k) or d === ?l) or d === ?m) or d === ?n) or d === ?o) or d === ?p) or d === ?q) or d === ?r) or d === ?s) or d === ?t) or d === ?u) or d === ?v) or d === ?w) or d === ?x) or d === ?y) or d === ?z) or d === ?A) or d === ?B) or d === ?C) or d === ?D) or d === ?E) or d === ?F) or d === ?G) or d === ?H) or d === ?I) or d === ?J) or d === ?K) or d === ?L) or d === ?M) or d === ?N) or d === ?O) or d === ?P) or d === ?Q) or d === ?R) or d === ?S) or d === ?T) or d === ?U) or d === ?V) or d === ?W) or d === ?X) or d === ?Y) or d === ?Z) or d === ?0) or d === ?1) or d === ?2) or d === ?3) or d === ?4) or d === ?5) or d === ?6) or d === ?7) or d === ?8) or d === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((e === ?a or e === ?b) or e === ?c) or e === ?d) or e === ?e) or e === ?f) or e === ?g) or e === ?h) or e === ?i) or e === ?j) or e === ?k) or e === ?l) or e === ?m) or e === ?n) or e === ?o) or e === ?p) or e === ?q) or e === ?r) or e === ?s) or e === ?t) or e === ?u) or e === ?v) or e === ?w) or e === ?x) or e === ?y) or e === ?z) or e === ?A) or e === ?B) or e === ?C) or e === ?D) or e === ?E) or e === ?F) or e === ?G) or e === ?H) or e === ?I) or e === ?J) or e === ?K) or e === ?L) or e === ?M) or e === ?N) or e === ?O) or e === ?P) or e === ?Q) or e === ?R) or e === ?S) or e === ?T) or e === ?U) or e === ?V) or e === ?W) or e === ?X) or e === ?Y) or e === ?Z) or e === ?0) or e === ?1) or e === ?2) or e === ?3) or e === ?4) or e === ?5) or e === ?6) or e === ?7) or e === ?8) or e === ?9) do
    langtag_variant(r, acc,
                      <<t :: binary, ?-,
                          case (a) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              a
                          end,
                          case (b) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              b
                          end,
                          case (c) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              c
                          end,
                          case (d) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              d
                          end,
                          case (e) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              e
                          end>>)
  end

  defp langtag_script(<<?-, a, b, c, d, r :: bits>>, acc, t)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((a === ?a or a === ?b) or a === ?c) or a === ?d) or a === ?e) or a === ?f) or a === ?g) or a === ?h) or a === ?i) or a === ?j) or a === ?k) or a === ?l) or a === ?m) or a === ?n) or a === ?o) or a === ?p) or a === ?q) or a === ?r) or a === ?s) or a === ?t) or a === ?u) or a === ?v) or a === ?w) or a === ?x) or a === ?y) or a === ?z) or a === ?A) or a === ?B) or a === ?C) or a === ?D) or a === ?E) or a === ?F) or a === ?G) or a === ?H) or a === ?I) or a === ?J) or a === ?K) or a === ?L) or a === ?M) or a === ?N) or a === ?O) or a === ?P) or a === ?Q) or a === ?R) or a === ?S) or a === ?T) or a === ?U) or a === ?V) or a === ?W) or a === ?X) or a === ?Y) or a === ?Z and
              ((((((((((((((((((((((((((((((((((((((((((((((((((b === ?a or b === ?b) or b === ?c) or b === ?d) or b === ?e) or b === ?f) or b === ?g) or b === ?h) or b === ?i) or b === ?j) or b === ?k) or b === ?l) or b === ?m) or b === ?n) or b === ?o) or b === ?p) or b === ?q) or b === ?r) or b === ?s) or b === ?t) or b === ?u) or b === ?v) or b === ?w) or b === ?x) or b === ?y) or b === ?z) or b === ?A) or b === ?B) or b === ?C) or b === ?D) or b === ?E) or b === ?F) or b === ?G) or b === ?H) or b === ?I) or b === ?J) or b === ?K) or b === ?L) or b === ?M) or b === ?N) or b === ?O) or b === ?P) or b === ?Q) or b === ?R) or b === ?S) or b === ?T) or b === ?U) or b === ?V) or b === ?W) or b === ?X) or b === ?Y) or b === ?Z and
              ((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z and
              ((((((((((((((((((((((((((((((((((((((((((((((((((d === ?a or d === ?b) or d === ?c) or d === ?d) or d === ?e) or d === ?f) or d === ?g) or d === ?h) or d === ?i) or d === ?j) or d === ?k) or d === ?l) or d === ?m) or d === ?n) or d === ?o) or d === ?p) or d === ?q) or d === ?r) or d === ?s) or d === ?t) or d === ?u) or d === ?v) or d === ?w) or d === ?x) or d === ?y) or d === ?z) or d === ?A) or d === ?B) or d === ?C) or d === ?D) or d === ?E) or d === ?F) or d === ?G) or d === ?H) or d === ?I) or d === ?J) or d === ?K) or d === ?L) or d === ?M) or d === ?N) or d === ?O) or d === ?P) or d === ?Q) or d === ?R) or d === ?S) or d === ?T) or d === ?U) or d === ?V) or d === ?W) or d === ?X) or d === ?Y) or d === ?Z) do
    langtag_region(r, acc,
                     <<t :: binary, ?-,
                         case (a) do
                           ?A ->
                             ?a
                           ?B ->
                             ?b
                           ?C ->
                             ?c
                           ?D ->
                             ?d
                           ?E ->
                             ?e
                           ?F ->
                             ?f
                           ?G ->
                             ?g
                           ?H ->
                             ?h
                           ?I ->
                             ?i
                           ?J ->
                             ?j
                           ?K ->
                             ?k
                           ?L ->
                             ?l
                           ?M ->
                             ?m
                           ?N ->
                             ?n
                           ?O ->
                             ?o
                           ?P ->
                             ?p
                           ?Q ->
                             ?q
                           ?R ->
                             ?r
                           ?S ->
                             ?s
                           ?T ->
                             ?t
                           ?U ->
                             ?u
                           ?V ->
                             ?v
                           ?W ->
                             ?w
                           ?X ->
                             ?x
                           ?Y ->
                             ?y
                           ?Z ->
                             ?z
                           _ ->
                             a
                         end,
                         case (b) do
                           ?A ->
                             ?a
                           ?B ->
                             ?b
                           ?C ->
                             ?c
                           ?D ->
                             ?d
                           ?E ->
                             ?e
                           ?F ->
                             ?f
                           ?G ->
                             ?g
                           ?H ->
                             ?h
                           ?I ->
                             ?i
                           ?J ->
                             ?j
                           ?K ->
                             ?k
                           ?L ->
                             ?l
                           ?M ->
                             ?m
                           ?N ->
                             ?n
                           ?O ->
                             ?o
                           ?P ->
                             ?p
                           ?Q ->
                             ?q
                           ?R ->
                             ?r
                           ?S ->
                             ?s
                           ?T ->
                             ?t
                           ?U ->
                             ?u
                           ?V ->
                             ?v
                           ?W ->
                             ?w
                           ?X ->
                             ?x
                           ?Y ->
                             ?y
                           ?Z ->
                             ?z
                           _ ->
                             b
                         end,
                         case (c) do
                           ?A ->
                             ?a
                           ?B ->
                             ?b
                           ?C ->
                             ?c
                           ?D ->
                             ?d
                           ?E ->
                             ?e
                           ?F ->
                             ?f
                           ?G ->
                             ?g
                           ?H ->
                             ?h
                           ?I ->
                             ?i
                           ?J ->
                             ?j
                           ?K ->
                             ?k
                           ?L ->
                             ?l
                           ?M ->
                             ?m
                           ?N ->
                             ?n
                           ?O ->
                             ?o
                           ?P ->
                             ?p
                           ?Q ->
                             ?q
                           ?R ->
                             ?r
                           ?S ->
                             ?s
                           ?T ->
                             ?t
                           ?U ->
                             ?u
                           ?V ->
                             ?v
                           ?W ->
                             ?w
                           ?X ->
                             ?x
                           ?Y ->
                             ?y
                           ?Z ->
                             ?z
                           _ ->
                             c
                         end,
                         case (d) do
                           ?A ->
                             ?a
                           ?B ->
                             ?b
                           ?C ->
                             ?c
                           ?D ->
                             ?d
                           ?E ->
                             ?e
                           ?F ->
                             ?f
                           ?G ->
                             ?g
                           ?H ->
                             ?h
                           ?I ->
                             ?i
                           ?J ->
                             ?j
                           ?K ->
                             ?k
                           ?L ->
                             ?l
                           ?M ->
                             ?m
                           ?N ->
                             ?n
                           ?O ->
                             ?o
                           ?P ->
                             ?p
                           ?Q ->
                             ?q
                           ?R ->
                             ?r
                           ?S ->
                             ?s
                           ?T ->
                             ?t
                           ?U ->
                             ?u
                           ?V ->
                             ?v
                           ?W ->
                             ?w
                           ?X ->
                             ?x
                           ?Y ->
                             ?y
                           ?Z ->
                             ?z
                           _ ->
                             d
                         end>>)
  end

  defp langtag_script(r, acc, t) do
    langtag_region(r, acc, t)
  end

  defp langtag_region(<<?-, a, b, c, d, e, f, g, h, r :: bits>>, acc,
            t)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((a === ?a or a === ?b) or a === ?c) or a === ?d) or a === ?e) or a === ?f) or a === ?g) or a === ?h) or a === ?i) or a === ?j) or a === ?k) or a === ?l) or a === ?m) or a === ?n) or a === ?o) or a === ?p) or a === ?q) or a === ?r) or a === ?s) or a === ?t) or a === ?u) or a === ?v) or a === ?w) or a === ?x) or a === ?y) or a === ?z) or a === ?A) or a === ?B) or a === ?C) or a === ?D) or a === ?E) or a === ?F) or a === ?G) or a === ?H) or a === ?I) or a === ?J) or a === ?K) or a === ?L) or a === ?M) or a === ?N) or a === ?O) or a === ?P) or a === ?Q) or a === ?R) or a === ?S) or a === ?T) or a === ?U) or a === ?V) or a === ?W) or a === ?X) or a === ?Y) or a === ?Z) or a === ?0) or a === ?1) or a === ?2) or a === ?3) or a === ?4) or a === ?5) or a === ?6) or a === ?7) or a === ?8) or a === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((b === ?a or b === ?b) or b === ?c) or b === ?d) or b === ?e) or b === ?f) or b === ?g) or b === ?h) or b === ?i) or b === ?j) or b === ?k) or b === ?l) or b === ?m) or b === ?n) or b === ?o) or b === ?p) or b === ?q) or b === ?r) or b === ?s) or b === ?t) or b === ?u) or b === ?v) or b === ?w) or b === ?x) or b === ?y) or b === ?z) or b === ?A) or b === ?B) or b === ?C) or b === ?D) or b === ?E) or b === ?F) or b === ?G) or b === ?H) or b === ?I) or b === ?J) or b === ?K) or b === ?L) or b === ?M) or b === ?N) or b === ?O) or b === ?P) or b === ?Q) or b === ?R) or b === ?S) or b === ?T) or b === ?U) or b === ?V) or b === ?W) or b === ?X) or b === ?Y) or b === ?Z) or b === ?0) or b === ?1) or b === ?2) or b === ?3) or b === ?4) or b === ?5) or b === ?6) or b === ?7) or b === ?8) or b === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((d === ?a or d === ?b) or d === ?c) or d === ?d) or d === ?e) or d === ?f) or d === ?g) or d === ?h) or d === ?i) or d === ?j) or d === ?k) or d === ?l) or d === ?m) or d === ?n) or d === ?o) or d === ?p) or d === ?q) or d === ?r) or d === ?s) or d === ?t) or d === ?u) or d === ?v) or d === ?w) or d === ?x) or d === ?y) or d === ?z) or d === ?A) or d === ?B) or d === ?C) or d === ?D) or d === ?E) or d === ?F) or d === ?G) or d === ?H) or d === ?I) or d === ?J) or d === ?K) or d === ?L) or d === ?M) or d === ?N) or d === ?O) or d === ?P) or d === ?Q) or d === ?R) or d === ?S) or d === ?T) or d === ?U) or d === ?V) or d === ?W) or d === ?X) or d === ?Y) or d === ?Z) or d === ?0) or d === ?1) or d === ?2) or d === ?3) or d === ?4) or d === ?5) or d === ?6) or d === ?7) or d === ?8) or d === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((e === ?a or e === ?b) or e === ?c) or e === ?d) or e === ?e) or e === ?f) or e === ?g) or e === ?h) or e === ?i) or e === ?j) or e === ?k) or e === ?l) or e === ?m) or e === ?n) or e === ?o) or e === ?p) or e === ?q) or e === ?r) or e === ?s) or e === ?t) or e === ?u) or e === ?v) or e === ?w) or e === ?x) or e === ?y) or e === ?z) or e === ?A) or e === ?B) or e === ?C) or e === ?D) or e === ?E) or e === ?F) or e === ?G) or e === ?H) or e === ?I) or e === ?J) or e === ?K) or e === ?L) or e === ?M) or e === ?N) or e === ?O) or e === ?P) or e === ?Q) or e === ?R) or e === ?S) or e === ?T) or e === ?U) or e === ?V) or e === ?W) or e === ?X) or e === ?Y) or e === ?Z) or e === ?0) or e === ?1) or e === ?2) or e === ?3) or e === ?4) or e === ?5) or e === ?6) or e === ?7) or e === ?8) or e === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((f === ?a or f === ?b) or f === ?c) or f === ?d) or f === ?e) or f === ?f) or f === ?g) or f === ?h) or f === ?i) or f === ?j) or f === ?k) or f === ?l) or f === ?m) or f === ?n) or f === ?o) or f === ?p) or f === ?q) or f === ?r) or f === ?s) or f === ?t) or f === ?u) or f === ?v) or f === ?w) or f === ?x) or f === ?y) or f === ?z) or f === ?A) or f === ?B) or f === ?C) or f === ?D) or f === ?E) or f === ?F) or f === ?G) or f === ?H) or f === ?I) or f === ?J) or f === ?K) or f === ?L) or f === ?M) or f === ?N) or f === ?O) or f === ?P) or f === ?Q) or f === ?R) or f === ?S) or f === ?T) or f === ?U) or f === ?V) or f === ?W) or f === ?X) or f === ?Y) or f === ?Z) or f === ?0) or f === ?1) or f === ?2) or f === ?3) or f === ?4) or f === ?5) or f === ?6) or f === ?7) or f === ?8) or f === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((g === ?a or g === ?b) or g === ?c) or g === ?d) or g === ?e) or g === ?f) or g === ?g) or g === ?h) or g === ?i) or g === ?j) or g === ?k) or g === ?l) or g === ?m) or g === ?n) or g === ?o) or g === ?p) or g === ?q) or g === ?r) or g === ?s) or g === ?t) or g === ?u) or g === ?v) or g === ?w) or g === ?x) or g === ?y) or g === ?z) or g === ?A) or g === ?B) or g === ?C) or g === ?D) or g === ?E) or g === ?F) or g === ?G) or g === ?H) or g === ?I) or g === ?J) or g === ?K) or g === ?L) or g === ?M) or g === ?N) or g === ?O) or g === ?P) or g === ?Q) or g === ?R) or g === ?S) or g === ?T) or g === ?U) or g === ?V) or g === ?W) or g === ?X) or g === ?Y) or g === ?Z) or g === ?0) or g === ?1) or g === ?2) or g === ?3) or g === ?4) or g === ?5) or g === ?6) or g === ?7) or g === ?8) or g === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((h === ?a or h === ?b) or h === ?c) or h === ?d) or h === ?e) or h === ?f) or h === ?g) or h === ?h) or h === ?i) or h === ?j) or h === ?k) or h === ?l) or h === ?m) or h === ?n) or h === ?o) or h === ?p) or h === ?q) or h === ?r) or h === ?s) or h === ?t) or h === ?u) or h === ?v) or h === ?w) or h === ?x) or h === ?y) or h === ?z) or h === ?A) or h === ?B) or h === ?C) or h === ?D) or h === ?E) or h === ?F) or h === ?G) or h === ?H) or h === ?I) or h === ?J) or h === ?K) or h === ?L) or h === ?M) or h === ?N) or h === ?O) or h === ?P) or h === ?Q) or h === ?R) or h === ?S) or h === ?T) or h === ?U) or h === ?V) or h === ?W) or h === ?X) or h === ?Y) or h === ?Z) or h === ?0) or h === ?1) or h === ?2) or h === ?3) or h === ?4) or h === ?5) or h === ?6) or h === ?7) or h === ?8) or h === ?9) do
    langtag_variant(r, acc,
                      <<t :: binary, ?-,
                          case (a) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              a
                          end,
                          case (b) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              b
                          end,
                          case (c) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              c
                          end,
                          case (d) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              d
                          end,
                          case (e) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              e
                          end,
                          case (f) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              f
                          end,
                          case (g) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              g
                          end,
                          case (h) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              h
                          end>>)
  end

  defp langtag_region(<<?-, a, b, c, d, e, f, g, r :: bits>>, acc, t)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((a === ?a or a === ?b) or a === ?c) or a === ?d) or a === ?e) or a === ?f) or a === ?g) or a === ?h) or a === ?i) or a === ?j) or a === ?k) or a === ?l) or a === ?m) or a === ?n) or a === ?o) or a === ?p) or a === ?q) or a === ?r) or a === ?s) or a === ?t) or a === ?u) or a === ?v) or a === ?w) or a === ?x) or a === ?y) or a === ?z) or a === ?A) or a === ?B) or a === ?C) or a === ?D) or a === ?E) or a === ?F) or a === ?G) or a === ?H) or a === ?I) or a === ?J) or a === ?K) or a === ?L) or a === ?M) or a === ?N) or a === ?O) or a === ?P) or a === ?Q) or a === ?R) or a === ?S) or a === ?T) or a === ?U) or a === ?V) or a === ?W) or a === ?X) or a === ?Y) or a === ?Z) or a === ?0) or a === ?1) or a === ?2) or a === ?3) or a === ?4) or a === ?5) or a === ?6) or a === ?7) or a === ?8) or a === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((b === ?a or b === ?b) or b === ?c) or b === ?d) or b === ?e) or b === ?f) or b === ?g) or b === ?h) or b === ?i) or b === ?j) or b === ?k) or b === ?l) or b === ?m) or b === ?n) or b === ?o) or b === ?p) or b === ?q) or b === ?r) or b === ?s) or b === ?t) or b === ?u) or b === ?v) or b === ?w) or b === ?x) or b === ?y) or b === ?z) or b === ?A) or b === ?B) or b === ?C) or b === ?D) or b === ?E) or b === ?F) or b === ?G) or b === ?H) or b === ?I) or b === ?J) or b === ?K) or b === ?L) or b === ?M) or b === ?N) or b === ?O) or b === ?P) or b === ?Q) or b === ?R) or b === ?S) or b === ?T) or b === ?U) or b === ?V) or b === ?W) or b === ?X) or b === ?Y) or b === ?Z) or b === ?0) or b === ?1) or b === ?2) or b === ?3) or b === ?4) or b === ?5) or b === ?6) or b === ?7) or b === ?8) or b === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((d === ?a or d === ?b) or d === ?c) or d === ?d) or d === ?e) or d === ?f) or d === ?g) or d === ?h) or d === ?i) or d === ?j) or d === ?k) or d === ?l) or d === ?m) or d === ?n) or d === ?o) or d === ?p) or d === ?q) or d === ?r) or d === ?s) or d === ?t) or d === ?u) or d === ?v) or d === ?w) or d === ?x) or d === ?y) or d === ?z) or d === ?A) or d === ?B) or d === ?C) or d === ?D) or d === ?E) or d === ?F) or d === ?G) or d === ?H) or d === ?I) or d === ?J) or d === ?K) or d === ?L) or d === ?M) or d === ?N) or d === ?O) or d === ?P) or d === ?Q) or d === ?R) or d === ?S) or d === ?T) or d === ?U) or d === ?V) or d === ?W) or d === ?X) or d === ?Y) or d === ?Z) or d === ?0) or d === ?1) or d === ?2) or d === ?3) or d === ?4) or d === ?5) or d === ?6) or d === ?7) or d === ?8) or d === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((e === ?a or e === ?b) or e === ?c) or e === ?d) or e === ?e) or e === ?f) or e === ?g) or e === ?h) or e === ?i) or e === ?j) or e === ?k) or e === ?l) or e === ?m) or e === ?n) or e === ?o) or e === ?p) or e === ?q) or e === ?r) or e === ?s) or e === ?t) or e === ?u) or e === ?v) or e === ?w) or e === ?x) or e === ?y) or e === ?z) or e === ?A) or e === ?B) or e === ?C) or e === ?D) or e === ?E) or e === ?F) or e === ?G) or e === ?H) or e === ?I) or e === ?J) or e === ?K) or e === ?L) or e === ?M) or e === ?N) or e === ?O) or e === ?P) or e === ?Q) or e === ?R) or e === ?S) or e === ?T) or e === ?U) or e === ?V) or e === ?W) or e === ?X) or e === ?Y) or e === ?Z) or e === ?0) or e === ?1) or e === ?2) or e === ?3) or e === ?4) or e === ?5) or e === ?6) or e === ?7) or e === ?8) or e === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((f === ?a or f === ?b) or f === ?c) or f === ?d) or f === ?e) or f === ?f) or f === ?g) or f === ?h) or f === ?i) or f === ?j) or f === ?k) or f === ?l) or f === ?m) or f === ?n) or f === ?o) or f === ?p) or f === ?q) or f === ?r) or f === ?s) or f === ?t) or f === ?u) or f === ?v) or f === ?w) or f === ?x) or f === ?y) or f === ?z) or f === ?A) or f === ?B) or f === ?C) or f === ?D) or f === ?E) or f === ?F) or f === ?G) or f === ?H) or f === ?I) or f === ?J) or f === ?K) or f === ?L) or f === ?M) or f === ?N) or f === ?O) or f === ?P) or f === ?Q) or f === ?R) or f === ?S) or f === ?T) or f === ?U) or f === ?V) or f === ?W) or f === ?X) or f === ?Y) or f === ?Z) or f === ?0) or f === ?1) or f === ?2) or f === ?3) or f === ?4) or f === ?5) or f === ?6) or f === ?7) or f === ?8) or f === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((g === ?a or g === ?b) or g === ?c) or g === ?d) or g === ?e) or g === ?f) or g === ?g) or g === ?h) or g === ?i) or g === ?j) or g === ?k) or g === ?l) or g === ?m) or g === ?n) or g === ?o) or g === ?p) or g === ?q) or g === ?r) or g === ?s) or g === ?t) or g === ?u) or g === ?v) or g === ?w) or g === ?x) or g === ?y) or g === ?z) or g === ?A) or g === ?B) or g === ?C) or g === ?D) or g === ?E) or g === ?F) or g === ?G) or g === ?H) or g === ?I) or g === ?J) or g === ?K) or g === ?L) or g === ?M) or g === ?N) or g === ?O) or g === ?P) or g === ?Q) or g === ?R) or g === ?S) or g === ?T) or g === ?U) or g === ?V) or g === ?W) or g === ?X) or g === ?Y) or g === ?Z) or g === ?0) or g === ?1) or g === ?2) or g === ?3) or g === ?4) or g === ?5) or g === ?6) or g === ?7) or g === ?8) or g === ?9) do
    langtag_variant(r, acc,
                      <<t :: binary, ?-,
                          case (a) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              a
                          end,
                          case (b) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              b
                          end,
                          case (c) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              c
                          end,
                          case (d) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              d
                          end,
                          case (e) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              e
                          end,
                          case (f) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              f
                          end,
                          case (g) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              g
                          end>>)
  end

  defp langtag_region(<<?-, a, b, c, d, e, f, r :: bits>>, acc, t)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((a === ?a or a === ?b) or a === ?c) or a === ?d) or a === ?e) or a === ?f) or a === ?g) or a === ?h) or a === ?i) or a === ?j) or a === ?k) or a === ?l) or a === ?m) or a === ?n) or a === ?o) or a === ?p) or a === ?q) or a === ?r) or a === ?s) or a === ?t) or a === ?u) or a === ?v) or a === ?w) or a === ?x) or a === ?y) or a === ?z) or a === ?A) or a === ?B) or a === ?C) or a === ?D) or a === ?E) or a === ?F) or a === ?G) or a === ?H) or a === ?I) or a === ?J) or a === ?K) or a === ?L) or a === ?M) or a === ?N) or a === ?O) or a === ?P) or a === ?Q) or a === ?R) or a === ?S) or a === ?T) or a === ?U) or a === ?V) or a === ?W) or a === ?X) or a === ?Y) or a === ?Z) or a === ?0) or a === ?1) or a === ?2) or a === ?3) or a === ?4) or a === ?5) or a === ?6) or a === ?7) or a === ?8) or a === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((b === ?a or b === ?b) or b === ?c) or b === ?d) or b === ?e) or b === ?f) or b === ?g) or b === ?h) or b === ?i) or b === ?j) or b === ?k) or b === ?l) or b === ?m) or b === ?n) or b === ?o) or b === ?p) or b === ?q) or b === ?r) or b === ?s) or b === ?t) or b === ?u) or b === ?v) or b === ?w) or b === ?x) or b === ?y) or b === ?z) or b === ?A) or b === ?B) or b === ?C) or b === ?D) or b === ?E) or b === ?F) or b === ?G) or b === ?H) or b === ?I) or b === ?J) or b === ?K) or b === ?L) or b === ?M) or b === ?N) or b === ?O) or b === ?P) or b === ?Q) or b === ?R) or b === ?S) or b === ?T) or b === ?U) or b === ?V) or b === ?W) or b === ?X) or b === ?Y) or b === ?Z) or b === ?0) or b === ?1) or b === ?2) or b === ?3) or b === ?4) or b === ?5) or b === ?6) or b === ?7) or b === ?8) or b === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((d === ?a or d === ?b) or d === ?c) or d === ?d) or d === ?e) or d === ?f) or d === ?g) or d === ?h) or d === ?i) or d === ?j) or d === ?k) or d === ?l) or d === ?m) or d === ?n) or d === ?o) or d === ?p) or d === ?q) or d === ?r) or d === ?s) or d === ?t) or d === ?u) or d === ?v) or d === ?w) or d === ?x) or d === ?y) or d === ?z) or d === ?A) or d === ?B) or d === ?C) or d === ?D) or d === ?E) or d === ?F) or d === ?G) or d === ?H) or d === ?I) or d === ?J) or d === ?K) or d === ?L) or d === ?M) or d === ?N) or d === ?O) or d === ?P) or d === ?Q) or d === ?R) or d === ?S) or d === ?T) or d === ?U) or d === ?V) or d === ?W) or d === ?X) or d === ?Y) or d === ?Z) or d === ?0) or d === ?1) or d === ?2) or d === ?3) or d === ?4) or d === ?5) or d === ?6) or d === ?7) or d === ?8) or d === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((e === ?a or e === ?b) or e === ?c) or e === ?d) or e === ?e) or e === ?f) or e === ?g) or e === ?h) or e === ?i) or e === ?j) or e === ?k) or e === ?l) or e === ?m) or e === ?n) or e === ?o) or e === ?p) or e === ?q) or e === ?r) or e === ?s) or e === ?t) or e === ?u) or e === ?v) or e === ?w) or e === ?x) or e === ?y) or e === ?z) or e === ?A) or e === ?B) or e === ?C) or e === ?D) or e === ?E) or e === ?F) or e === ?G) or e === ?H) or e === ?I) or e === ?J) or e === ?K) or e === ?L) or e === ?M) or e === ?N) or e === ?O) or e === ?P) or e === ?Q) or e === ?R) or e === ?S) or e === ?T) or e === ?U) or e === ?V) or e === ?W) or e === ?X) or e === ?Y) or e === ?Z) or e === ?0) or e === ?1) or e === ?2) or e === ?3) or e === ?4) or e === ?5) or e === ?6) or e === ?7) or e === ?8) or e === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((f === ?a or f === ?b) or f === ?c) or f === ?d) or f === ?e) or f === ?f) or f === ?g) or f === ?h) or f === ?i) or f === ?j) or f === ?k) or f === ?l) or f === ?m) or f === ?n) or f === ?o) or f === ?p) or f === ?q) or f === ?r) or f === ?s) or f === ?t) or f === ?u) or f === ?v) or f === ?w) or f === ?x) or f === ?y) or f === ?z) or f === ?A) or f === ?B) or f === ?C) or f === ?D) or f === ?E) or f === ?F) or f === ?G) or f === ?H) or f === ?I) or f === ?J) or f === ?K) or f === ?L) or f === ?M) or f === ?N) or f === ?O) or f === ?P) or f === ?Q) or f === ?R) or f === ?S) or f === ?T) or f === ?U) or f === ?V) or f === ?W) or f === ?X) or f === ?Y) or f === ?Z) or f === ?0) or f === ?1) or f === ?2) or f === ?3) or f === ?4) or f === ?5) or f === ?6) or f === ?7) or f === ?8) or f === ?9) do
    langtag_variant(r, acc,
                      <<t :: binary, ?-,
                          case (a) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              a
                          end,
                          case (b) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              b
                          end,
                          case (c) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              c
                          end,
                          case (d) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              d
                          end,
                          case (e) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              e
                          end,
                          case (f) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              f
                          end>>)
  end

  defp langtag_region(<<?-, a, b, c, d, e, r :: bits>>, acc, t)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((a === ?a or a === ?b) or a === ?c) or a === ?d) or a === ?e) or a === ?f) or a === ?g) or a === ?h) or a === ?i) or a === ?j) or a === ?k) or a === ?l) or a === ?m) or a === ?n) or a === ?o) or a === ?p) or a === ?q) or a === ?r) or a === ?s) or a === ?t) or a === ?u) or a === ?v) or a === ?w) or a === ?x) or a === ?y) or a === ?z) or a === ?A) or a === ?B) or a === ?C) or a === ?D) or a === ?E) or a === ?F) or a === ?G) or a === ?H) or a === ?I) or a === ?J) or a === ?K) or a === ?L) or a === ?M) or a === ?N) or a === ?O) or a === ?P) or a === ?Q) or a === ?R) or a === ?S) or a === ?T) or a === ?U) or a === ?V) or a === ?W) or a === ?X) or a === ?Y) or a === ?Z) or a === ?0) or a === ?1) or a === ?2) or a === ?3) or a === ?4) or a === ?5) or a === ?6) or a === ?7) or a === ?8) or a === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((b === ?a or b === ?b) or b === ?c) or b === ?d) or b === ?e) or b === ?f) or b === ?g) or b === ?h) or b === ?i) or b === ?j) or b === ?k) or b === ?l) or b === ?m) or b === ?n) or b === ?o) or b === ?p) or b === ?q) or b === ?r) or b === ?s) or b === ?t) or b === ?u) or b === ?v) or b === ?w) or b === ?x) or b === ?y) or b === ?z) or b === ?A) or b === ?B) or b === ?C) or b === ?D) or b === ?E) or b === ?F) or b === ?G) or b === ?H) or b === ?I) or b === ?J) or b === ?K) or b === ?L) or b === ?M) or b === ?N) or b === ?O) or b === ?P) or b === ?Q) or b === ?R) or b === ?S) or b === ?T) or b === ?U) or b === ?V) or b === ?W) or b === ?X) or b === ?Y) or b === ?Z) or b === ?0) or b === ?1) or b === ?2) or b === ?3) or b === ?4) or b === ?5) or b === ?6) or b === ?7) or b === ?8) or b === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((d === ?a or d === ?b) or d === ?c) or d === ?d) or d === ?e) or d === ?f) or d === ?g) or d === ?h) or d === ?i) or d === ?j) or d === ?k) or d === ?l) or d === ?m) or d === ?n) or d === ?o) or d === ?p) or d === ?q) or d === ?r) or d === ?s) or d === ?t) or d === ?u) or d === ?v) or d === ?w) or d === ?x) or d === ?y) or d === ?z) or d === ?A) or d === ?B) or d === ?C) or d === ?D) or d === ?E) or d === ?F) or d === ?G) or d === ?H) or d === ?I) or d === ?J) or d === ?K) or d === ?L) or d === ?M) or d === ?N) or d === ?O) or d === ?P) or d === ?Q) or d === ?R) or d === ?S) or d === ?T) or d === ?U) or d === ?V) or d === ?W) or d === ?X) or d === ?Y) or d === ?Z) or d === ?0) or d === ?1) or d === ?2) or d === ?3) or d === ?4) or d === ?5) or d === ?6) or d === ?7) or d === ?8) or d === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((e === ?a or e === ?b) or e === ?c) or e === ?d) or e === ?e) or e === ?f) or e === ?g) or e === ?h) or e === ?i) or e === ?j) or e === ?k) or e === ?l) or e === ?m) or e === ?n) or e === ?o) or e === ?p) or e === ?q) or e === ?r) or e === ?s) or e === ?t) or e === ?u) or e === ?v) or e === ?w) or e === ?x) or e === ?y) or e === ?z) or e === ?A) or e === ?B) or e === ?C) or e === ?D) or e === ?E) or e === ?F) or e === ?G) or e === ?H) or e === ?I) or e === ?J) or e === ?K) or e === ?L) or e === ?M) or e === ?N) or e === ?O) or e === ?P) or e === ?Q) or e === ?R) or e === ?S) or e === ?T) or e === ?U) or e === ?V) or e === ?W) or e === ?X) or e === ?Y) or e === ?Z) or e === ?0) or e === ?1) or e === ?2) or e === ?3) or e === ?4) or e === ?5) or e === ?6) or e === ?7) or e === ?8) or e === ?9) do
    langtag_variant(r, acc,
                      <<t :: binary, ?-,
                          case (a) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              a
                          end,
                          case (b) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              b
                          end,
                          case (c) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              c
                          end,
                          case (d) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              d
                          end,
                          case (e) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              e
                          end>>)
  end

  defp langtag_region(<<?-, a, b, c, d, r :: bits>>, acc, t)
      when (((((((((a === ?0 or a === ?1) or a === ?2) or a === ?3) or a === ?4) or a === ?5) or a === ?6) or a === ?7) or a === ?8) or a === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((b === ?a or b === ?b) or b === ?c) or b === ?d) or b === ?e) or b === ?f) or b === ?g) or b === ?h) or b === ?i) or b === ?j) or b === ?k) or b === ?l) or b === ?m) or b === ?n) or b === ?o) or b === ?p) or b === ?q) or b === ?r) or b === ?s) or b === ?t) or b === ?u) or b === ?v) or b === ?w) or b === ?x) or b === ?y) or b === ?z) or b === ?A) or b === ?B) or b === ?C) or b === ?D) or b === ?E) or b === ?F) or b === ?G) or b === ?H) or b === ?I) or b === ?J) or b === ?K) or b === ?L) or b === ?M) or b === ?N) or b === ?O) or b === ?P) or b === ?Q) or b === ?R) or b === ?S) or b === ?T) or b === ?U) or b === ?V) or b === ?W) or b === ?X) or b === ?Y) or b === ?Z) or b === ?0) or b === ?1) or b === ?2) or b === ?3) or b === ?4) or b === ?5) or b === ?6) or b === ?7) or b === ?8) or b === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((d === ?a or d === ?b) or d === ?c) or d === ?d) or d === ?e) or d === ?f) or d === ?g) or d === ?h) or d === ?i) or d === ?j) or d === ?k) or d === ?l) or d === ?m) or d === ?n) or d === ?o) or d === ?p) or d === ?q) or d === ?r) or d === ?s) or d === ?t) or d === ?u) or d === ?v) or d === ?w) or d === ?x) or d === ?y) or d === ?z) or d === ?A) or d === ?B) or d === ?C) or d === ?D) or d === ?E) or d === ?F) or d === ?G) or d === ?H) or d === ?I) or d === ?J) or d === ?K) or d === ?L) or d === ?M) or d === ?N) or d === ?O) or d === ?P) or d === ?Q) or d === ?R) or d === ?S) or d === ?T) or d === ?U) or d === ?V) or d === ?W) or d === ?X) or d === ?Y) or d === ?Z) or d === ?0) or d === ?1) or d === ?2) or d === ?3) or d === ?4) or d === ?5) or d === ?6) or d === ?7) or d === ?8) or d === ?9) do
    langtag_variant(r, acc,
                      <<t :: binary, ?-, a,
                          case (b) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              b
                          end,
                          case (c) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              c
                          end,
                          case (d) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              d
                          end>>)
  end

  defp langtag_region(<<?-, a, b, r :: bits>>, acc, t)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((a === ?a or a === ?b) or a === ?c) or a === ?d) or a === ?e) or a === ?f) or a === ?g) or a === ?h) or a === ?i) or a === ?j) or a === ?k) or a === ?l) or a === ?m) or a === ?n) or a === ?o) or a === ?p) or a === ?q) or a === ?r) or a === ?s) or a === ?t) or a === ?u) or a === ?v) or a === ?w) or a === ?x) or a === ?y) or a === ?z) or a === ?A) or a === ?B) or a === ?C) or a === ?D) or a === ?E) or a === ?F) or a === ?G) or a === ?H) or a === ?I) or a === ?J) or a === ?K) or a === ?L) or a === ?M) or a === ?N) or a === ?O) or a === ?P) or a === ?Q) or a === ?R) or a === ?S) or a === ?T) or a === ?U) or a === ?V) or a === ?W) or a === ?X) or a === ?Y) or a === ?Z and
              ((((((((((((((((((((((((((((((((((((((((((((((((((b === ?a or b === ?b) or b === ?c) or b === ?d) or b === ?e) or b === ?f) or b === ?g) or b === ?h) or b === ?i) or b === ?j) or b === ?k) or b === ?l) or b === ?m) or b === ?n) or b === ?o) or b === ?p) or b === ?q) or b === ?r) or b === ?s) or b === ?t) or b === ?u) or b === ?v) or b === ?w) or b === ?x) or b === ?y) or b === ?z) or b === ?A) or b === ?B) or b === ?C) or b === ?D) or b === ?E) or b === ?F) or b === ?G) or b === ?H) or b === ?I) or b === ?J) or b === ?K) or b === ?L) or b === ?M) or b === ?N) or b === ?O) or b === ?P) or b === ?Q) or b === ?R) or b === ?S) or b === ?T) or b === ?U) or b === ?V) or b === ?W) or b === ?X) or b === ?Y) or b === ?Z) do
    langtag_variant(r, acc,
                      <<t :: binary, ?-,
                          case (a) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              a
                          end,
                          case (b) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              b
                          end>>)
  end

  defp langtag_region(<<?-, a, b, c, r :: bits>>, acc, t)
      when (((((((((a === ?0 or a === ?1) or a === ?2) or a === ?3) or a === ?4) or a === ?5) or a === ?6) or a === ?7) or a === ?8) or a === ?9 and
              ((((((((b === ?0 or b === ?1) or b === ?2) or b === ?3) or b === ?4) or b === ?5) or b === ?6) or b === ?7) or b === ?8) or b === ?9 and
              ((((((((c === ?0 or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9) do
    langtag_variant(r, acc, <<t :: binary, ?-, a, b, c>>)
  end

  defp langtag_region(r, acc, t) do
    langtag_variant(r, acc, t)
  end

  defp langtag_variant(<<?-, a, b, c, d, e, f, g, h, r :: bits>>, acc,
            t)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((a === ?a or a === ?b) or a === ?c) or a === ?d) or a === ?e) or a === ?f) or a === ?g) or a === ?h) or a === ?i) or a === ?j) or a === ?k) or a === ?l) or a === ?m) or a === ?n) or a === ?o) or a === ?p) or a === ?q) or a === ?r) or a === ?s) or a === ?t) or a === ?u) or a === ?v) or a === ?w) or a === ?x) or a === ?y) or a === ?z) or a === ?A) or a === ?B) or a === ?C) or a === ?D) or a === ?E) or a === ?F) or a === ?G) or a === ?H) or a === ?I) or a === ?J) or a === ?K) or a === ?L) or a === ?M) or a === ?N) or a === ?O) or a === ?P) or a === ?Q) or a === ?R) or a === ?S) or a === ?T) or a === ?U) or a === ?V) or a === ?W) or a === ?X) or a === ?Y) or a === ?Z) or a === ?0) or a === ?1) or a === ?2) or a === ?3) or a === ?4) or a === ?5) or a === ?6) or a === ?7) or a === ?8) or a === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((b === ?a or b === ?b) or b === ?c) or b === ?d) or b === ?e) or b === ?f) or b === ?g) or b === ?h) or b === ?i) or b === ?j) or b === ?k) or b === ?l) or b === ?m) or b === ?n) or b === ?o) or b === ?p) or b === ?q) or b === ?r) or b === ?s) or b === ?t) or b === ?u) or b === ?v) or b === ?w) or b === ?x) or b === ?y) or b === ?z) or b === ?A) or b === ?B) or b === ?C) or b === ?D) or b === ?E) or b === ?F) or b === ?G) or b === ?H) or b === ?I) or b === ?J) or b === ?K) or b === ?L) or b === ?M) or b === ?N) or b === ?O) or b === ?P) or b === ?Q) or b === ?R) or b === ?S) or b === ?T) or b === ?U) or b === ?V) or b === ?W) or b === ?X) or b === ?Y) or b === ?Z) or b === ?0) or b === ?1) or b === ?2) or b === ?3) or b === ?4) or b === ?5) or b === ?6) or b === ?7) or b === ?8) or b === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((d === ?a or d === ?b) or d === ?c) or d === ?d) or d === ?e) or d === ?f) or d === ?g) or d === ?h) or d === ?i) or d === ?j) or d === ?k) or d === ?l) or d === ?m) or d === ?n) or d === ?o) or d === ?p) or d === ?q) or d === ?r) or d === ?s) or d === ?t) or d === ?u) or d === ?v) or d === ?w) or d === ?x) or d === ?y) or d === ?z) or d === ?A) or d === ?B) or d === ?C) or d === ?D) or d === ?E) or d === ?F) or d === ?G) or d === ?H) or d === ?I) or d === ?J) or d === ?K) or d === ?L) or d === ?M) or d === ?N) or d === ?O) or d === ?P) or d === ?Q) or d === ?R) or d === ?S) or d === ?T) or d === ?U) or d === ?V) or d === ?W) or d === ?X) or d === ?Y) or d === ?Z) or d === ?0) or d === ?1) or d === ?2) or d === ?3) or d === ?4) or d === ?5) or d === ?6) or d === ?7) or d === ?8) or d === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((e === ?a or e === ?b) or e === ?c) or e === ?d) or e === ?e) or e === ?f) or e === ?g) or e === ?h) or e === ?i) or e === ?j) or e === ?k) or e === ?l) or e === ?m) or e === ?n) or e === ?o) or e === ?p) or e === ?q) or e === ?r) or e === ?s) or e === ?t) or e === ?u) or e === ?v) or e === ?w) or e === ?x) or e === ?y) or e === ?z) or e === ?A) or e === ?B) or e === ?C) or e === ?D) or e === ?E) or e === ?F) or e === ?G) or e === ?H) or e === ?I) or e === ?J) or e === ?K) or e === ?L) or e === ?M) or e === ?N) or e === ?O) or e === ?P) or e === ?Q) or e === ?R) or e === ?S) or e === ?T) or e === ?U) or e === ?V) or e === ?W) or e === ?X) or e === ?Y) or e === ?Z) or e === ?0) or e === ?1) or e === ?2) or e === ?3) or e === ?4) or e === ?5) or e === ?6) or e === ?7) or e === ?8) or e === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((f === ?a or f === ?b) or f === ?c) or f === ?d) or f === ?e) or f === ?f) or f === ?g) or f === ?h) or f === ?i) or f === ?j) or f === ?k) or f === ?l) or f === ?m) or f === ?n) or f === ?o) or f === ?p) or f === ?q) or f === ?r) or f === ?s) or f === ?t) or f === ?u) or f === ?v) or f === ?w) or f === ?x) or f === ?y) or f === ?z) or f === ?A) or f === ?B) or f === ?C) or f === ?D) or f === ?E) or f === ?F) or f === ?G) or f === ?H) or f === ?I) or f === ?J) or f === ?K) or f === ?L) or f === ?M) or f === ?N) or f === ?O) or f === ?P) or f === ?Q) or f === ?R) or f === ?S) or f === ?T) or f === ?U) or f === ?V) or f === ?W) or f === ?X) or f === ?Y) or f === ?Z) or f === ?0) or f === ?1) or f === ?2) or f === ?3) or f === ?4) or f === ?5) or f === ?6) or f === ?7) or f === ?8) or f === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((g === ?a or g === ?b) or g === ?c) or g === ?d) or g === ?e) or g === ?f) or g === ?g) or g === ?h) or g === ?i) or g === ?j) or g === ?k) or g === ?l) or g === ?m) or g === ?n) or g === ?o) or g === ?p) or g === ?q) or g === ?r) or g === ?s) or g === ?t) or g === ?u) or g === ?v) or g === ?w) or g === ?x) or g === ?y) or g === ?z) or g === ?A) or g === ?B) or g === ?C) or g === ?D) or g === ?E) or g === ?F) or g === ?G) or g === ?H) or g === ?I) or g === ?J) or g === ?K) or g === ?L) or g === ?M) or g === ?N) or g === ?O) or g === ?P) or g === ?Q) or g === ?R) or g === ?S) or g === ?T) or g === ?U) or g === ?V) or g === ?W) or g === ?X) or g === ?Y) or g === ?Z) or g === ?0) or g === ?1) or g === ?2) or g === ?3) or g === ?4) or g === ?5) or g === ?6) or g === ?7) or g === ?8) or g === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((h === ?a or h === ?b) or h === ?c) or h === ?d) or h === ?e) or h === ?f) or h === ?g) or h === ?h) or h === ?i) or h === ?j) or h === ?k) or h === ?l) or h === ?m) or h === ?n) or h === ?o) or h === ?p) or h === ?q) or h === ?r) or h === ?s) or h === ?t) or h === ?u) or h === ?v) or h === ?w) or h === ?x) or h === ?y) or h === ?z) or h === ?A) or h === ?B) or h === ?C) or h === ?D) or h === ?E) or h === ?F) or h === ?G) or h === ?H) or h === ?I) or h === ?J) or h === ?K) or h === ?L) or h === ?M) or h === ?N) or h === ?O) or h === ?P) or h === ?Q) or h === ?R) or h === ?S) or h === ?T) or h === ?U) or h === ?V) or h === ?W) or h === ?X) or h === ?Y) or h === ?Z) or h === ?0) or h === ?1) or h === ?2) or h === ?3) or h === ?4) or h === ?5) or h === ?6) or h === ?7) or h === ?8) or h === ?9) do
    langtag_variant(r, acc,
                      <<t :: binary, ?-,
                          case (a) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              a
                          end,
                          case (b) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              b
                          end,
                          case (c) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              c
                          end,
                          case (d) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              d
                          end,
                          case (e) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              e
                          end,
                          case (f) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              f
                          end,
                          case (g) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              g
                          end,
                          case (h) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              h
                          end>>)
  end

  defp langtag_variant(<<?-, a, b, c, d, e, f, g, r :: bits>>, acc, t)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((a === ?a or a === ?b) or a === ?c) or a === ?d) or a === ?e) or a === ?f) or a === ?g) or a === ?h) or a === ?i) or a === ?j) or a === ?k) or a === ?l) or a === ?m) or a === ?n) or a === ?o) or a === ?p) or a === ?q) or a === ?r) or a === ?s) or a === ?t) or a === ?u) or a === ?v) or a === ?w) or a === ?x) or a === ?y) or a === ?z) or a === ?A) or a === ?B) or a === ?C) or a === ?D) or a === ?E) or a === ?F) or a === ?G) or a === ?H) or a === ?I) or a === ?J) or a === ?K) or a === ?L) or a === ?M) or a === ?N) or a === ?O) or a === ?P) or a === ?Q) or a === ?R) or a === ?S) or a === ?T) or a === ?U) or a === ?V) or a === ?W) or a === ?X) or a === ?Y) or a === ?Z) or a === ?0) or a === ?1) or a === ?2) or a === ?3) or a === ?4) or a === ?5) or a === ?6) or a === ?7) or a === ?8) or a === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((b === ?a or b === ?b) or b === ?c) or b === ?d) or b === ?e) or b === ?f) or b === ?g) or b === ?h) or b === ?i) or b === ?j) or b === ?k) or b === ?l) or b === ?m) or b === ?n) or b === ?o) or b === ?p) or b === ?q) or b === ?r) or b === ?s) or b === ?t) or b === ?u) or b === ?v) or b === ?w) or b === ?x) or b === ?y) or b === ?z) or b === ?A) or b === ?B) or b === ?C) or b === ?D) or b === ?E) or b === ?F) or b === ?G) or b === ?H) or b === ?I) or b === ?J) or b === ?K) or b === ?L) or b === ?M) or b === ?N) or b === ?O) or b === ?P) or b === ?Q) or b === ?R) or b === ?S) or b === ?T) or b === ?U) or b === ?V) or b === ?W) or b === ?X) or b === ?Y) or b === ?Z) or b === ?0) or b === ?1) or b === ?2) or b === ?3) or b === ?4) or b === ?5) or b === ?6) or b === ?7) or b === ?8) or b === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((d === ?a or d === ?b) or d === ?c) or d === ?d) or d === ?e) or d === ?f) or d === ?g) or d === ?h) or d === ?i) or d === ?j) or d === ?k) or d === ?l) or d === ?m) or d === ?n) or d === ?o) or d === ?p) or d === ?q) or d === ?r) or d === ?s) or d === ?t) or d === ?u) or d === ?v) or d === ?w) or d === ?x) or d === ?y) or d === ?z) or d === ?A) or d === ?B) or d === ?C) or d === ?D) or d === ?E) or d === ?F) or d === ?G) or d === ?H) or d === ?I) or d === ?J) or d === ?K) or d === ?L) or d === ?M) or d === ?N) or d === ?O) or d === ?P) or d === ?Q) or d === ?R) or d === ?S) or d === ?T) or d === ?U) or d === ?V) or d === ?W) or d === ?X) or d === ?Y) or d === ?Z) or d === ?0) or d === ?1) or d === ?2) or d === ?3) or d === ?4) or d === ?5) or d === ?6) or d === ?7) or d === ?8) or d === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((e === ?a or e === ?b) or e === ?c) or e === ?d) or e === ?e) or e === ?f) or e === ?g) or e === ?h) or e === ?i) or e === ?j) or e === ?k) or e === ?l) or e === ?m) or e === ?n) or e === ?o) or e === ?p) or e === ?q) or e === ?r) or e === ?s) or e === ?t) or e === ?u) or e === ?v) or e === ?w) or e === ?x) or e === ?y) or e === ?z) or e === ?A) or e === ?B) or e === ?C) or e === ?D) or e === ?E) or e === ?F) or e === ?G) or e === ?H) or e === ?I) or e === ?J) or e === ?K) or e === ?L) or e === ?M) or e === ?N) or e === ?O) or e === ?P) or e === ?Q) or e === ?R) or e === ?S) or e === ?T) or e === ?U) or e === ?V) or e === ?W) or e === ?X) or e === ?Y) or e === ?Z) or e === ?0) or e === ?1) or e === ?2) or e === ?3) or e === ?4) or e === ?5) or e === ?6) or e === ?7) or e === ?8) or e === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((f === ?a or f === ?b) or f === ?c) or f === ?d) or f === ?e) or f === ?f) or f === ?g) or f === ?h) or f === ?i) or f === ?j) or f === ?k) or f === ?l) or f === ?m) or f === ?n) or f === ?o) or f === ?p) or f === ?q) or f === ?r) or f === ?s) or f === ?t) or f === ?u) or f === ?v) or f === ?w) or f === ?x) or f === ?y) or f === ?z) or f === ?A) or f === ?B) or f === ?C) or f === ?D) or f === ?E) or f === ?F) or f === ?G) or f === ?H) or f === ?I) or f === ?J) or f === ?K) or f === ?L) or f === ?M) or f === ?N) or f === ?O) or f === ?P) or f === ?Q) or f === ?R) or f === ?S) or f === ?T) or f === ?U) or f === ?V) or f === ?W) or f === ?X) or f === ?Y) or f === ?Z) or f === ?0) or f === ?1) or f === ?2) or f === ?3) or f === ?4) or f === ?5) or f === ?6) or f === ?7) or f === ?8) or f === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((g === ?a or g === ?b) or g === ?c) or g === ?d) or g === ?e) or g === ?f) or g === ?g) or g === ?h) or g === ?i) or g === ?j) or g === ?k) or g === ?l) or g === ?m) or g === ?n) or g === ?o) or g === ?p) or g === ?q) or g === ?r) or g === ?s) or g === ?t) or g === ?u) or g === ?v) or g === ?w) or g === ?x) or g === ?y) or g === ?z) or g === ?A) or g === ?B) or g === ?C) or g === ?D) or g === ?E) or g === ?F) or g === ?G) or g === ?H) or g === ?I) or g === ?J) or g === ?K) or g === ?L) or g === ?M) or g === ?N) or g === ?O) or g === ?P) or g === ?Q) or g === ?R) or g === ?S) or g === ?T) or g === ?U) or g === ?V) or g === ?W) or g === ?X) or g === ?Y) or g === ?Z) or g === ?0) or g === ?1) or g === ?2) or g === ?3) or g === ?4) or g === ?5) or g === ?6) or g === ?7) or g === ?8) or g === ?9) do
    langtag_variant(r, acc,
                      <<t :: binary, ?-,
                          case (a) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              a
                          end,
                          case (b) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              b
                          end,
                          case (c) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              c
                          end,
                          case (d) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              d
                          end,
                          case (e) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              e
                          end,
                          case (f) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              f
                          end,
                          case (g) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              g
                          end>>)
  end

  defp langtag_variant(<<?-, a, b, c, d, e, f, r :: bits>>, acc, t)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((a === ?a or a === ?b) or a === ?c) or a === ?d) or a === ?e) or a === ?f) or a === ?g) or a === ?h) or a === ?i) or a === ?j) or a === ?k) or a === ?l) or a === ?m) or a === ?n) or a === ?o) or a === ?p) or a === ?q) or a === ?r) or a === ?s) or a === ?t) or a === ?u) or a === ?v) or a === ?w) or a === ?x) or a === ?y) or a === ?z) or a === ?A) or a === ?B) or a === ?C) or a === ?D) or a === ?E) or a === ?F) or a === ?G) or a === ?H) or a === ?I) or a === ?J) or a === ?K) or a === ?L) or a === ?M) or a === ?N) or a === ?O) or a === ?P) or a === ?Q) or a === ?R) or a === ?S) or a === ?T) or a === ?U) or a === ?V) or a === ?W) or a === ?X) or a === ?Y) or a === ?Z) or a === ?0) or a === ?1) or a === ?2) or a === ?3) or a === ?4) or a === ?5) or a === ?6) or a === ?7) or a === ?8) or a === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((b === ?a or b === ?b) or b === ?c) or b === ?d) or b === ?e) or b === ?f) or b === ?g) or b === ?h) or b === ?i) or b === ?j) or b === ?k) or b === ?l) or b === ?m) or b === ?n) or b === ?o) or b === ?p) or b === ?q) or b === ?r) or b === ?s) or b === ?t) or b === ?u) or b === ?v) or b === ?w) or b === ?x) or b === ?y) or b === ?z) or b === ?A) or b === ?B) or b === ?C) or b === ?D) or b === ?E) or b === ?F) or b === ?G) or b === ?H) or b === ?I) or b === ?J) or b === ?K) or b === ?L) or b === ?M) or b === ?N) or b === ?O) or b === ?P) or b === ?Q) or b === ?R) or b === ?S) or b === ?T) or b === ?U) or b === ?V) or b === ?W) or b === ?X) or b === ?Y) or b === ?Z) or b === ?0) or b === ?1) or b === ?2) or b === ?3) or b === ?4) or b === ?5) or b === ?6) or b === ?7) or b === ?8) or b === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((d === ?a or d === ?b) or d === ?c) or d === ?d) or d === ?e) or d === ?f) or d === ?g) or d === ?h) or d === ?i) or d === ?j) or d === ?k) or d === ?l) or d === ?m) or d === ?n) or d === ?o) or d === ?p) or d === ?q) or d === ?r) or d === ?s) or d === ?t) or d === ?u) or d === ?v) or d === ?w) or d === ?x) or d === ?y) or d === ?z) or d === ?A) or d === ?B) or d === ?C) or d === ?D) or d === ?E) or d === ?F) or d === ?G) or d === ?H) or d === ?I) or d === ?J) or d === ?K) or d === ?L) or d === ?M) or d === ?N) or d === ?O) or d === ?P) or d === ?Q) or d === ?R) or d === ?S) or d === ?T) or d === ?U) or d === ?V) or d === ?W) or d === ?X) or d === ?Y) or d === ?Z) or d === ?0) or d === ?1) or d === ?2) or d === ?3) or d === ?4) or d === ?5) or d === ?6) or d === ?7) or d === ?8) or d === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((e === ?a or e === ?b) or e === ?c) or e === ?d) or e === ?e) or e === ?f) or e === ?g) or e === ?h) or e === ?i) or e === ?j) or e === ?k) or e === ?l) or e === ?m) or e === ?n) or e === ?o) or e === ?p) or e === ?q) or e === ?r) or e === ?s) or e === ?t) or e === ?u) or e === ?v) or e === ?w) or e === ?x) or e === ?y) or e === ?z) or e === ?A) or e === ?B) or e === ?C) or e === ?D) or e === ?E) or e === ?F) or e === ?G) or e === ?H) or e === ?I) or e === ?J) or e === ?K) or e === ?L) or e === ?M) or e === ?N) or e === ?O) or e === ?P) or e === ?Q) or e === ?R) or e === ?S) or e === ?T) or e === ?U) or e === ?V) or e === ?W) or e === ?X) or e === ?Y) or e === ?Z) or e === ?0) or e === ?1) or e === ?2) or e === ?3) or e === ?4) or e === ?5) or e === ?6) or e === ?7) or e === ?8) or e === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((f === ?a or f === ?b) or f === ?c) or f === ?d) or f === ?e) or f === ?f) or f === ?g) or f === ?h) or f === ?i) or f === ?j) or f === ?k) or f === ?l) or f === ?m) or f === ?n) or f === ?o) or f === ?p) or f === ?q) or f === ?r) or f === ?s) or f === ?t) or f === ?u) or f === ?v) or f === ?w) or f === ?x) or f === ?y) or f === ?z) or f === ?A) or f === ?B) or f === ?C) or f === ?D) or f === ?E) or f === ?F) or f === ?G) or f === ?H) or f === ?I) or f === ?J) or f === ?K) or f === ?L) or f === ?M) or f === ?N) or f === ?O) or f === ?P) or f === ?Q) or f === ?R) or f === ?S) or f === ?T) or f === ?U) or f === ?V) or f === ?W) or f === ?X) or f === ?Y) or f === ?Z) or f === ?0) or f === ?1) or f === ?2) or f === ?3) or f === ?4) or f === ?5) or f === ?6) or f === ?7) or f === ?8) or f === ?9) do
    langtag_variant(r, acc,
                      <<t :: binary, ?-,
                          case (a) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              a
                          end,
                          case (b) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              b
                          end,
                          case (c) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              c
                          end,
                          case (d) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              d
                          end,
                          case (e) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              e
                          end,
                          case (f) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              f
                          end>>)
  end

  defp langtag_variant(<<?-, a, b, c, d, e, r :: bits>>, acc, t)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((a === ?a or a === ?b) or a === ?c) or a === ?d) or a === ?e) or a === ?f) or a === ?g) or a === ?h) or a === ?i) or a === ?j) or a === ?k) or a === ?l) or a === ?m) or a === ?n) or a === ?o) or a === ?p) or a === ?q) or a === ?r) or a === ?s) or a === ?t) or a === ?u) or a === ?v) or a === ?w) or a === ?x) or a === ?y) or a === ?z) or a === ?A) or a === ?B) or a === ?C) or a === ?D) or a === ?E) or a === ?F) or a === ?G) or a === ?H) or a === ?I) or a === ?J) or a === ?K) or a === ?L) or a === ?M) or a === ?N) or a === ?O) or a === ?P) or a === ?Q) or a === ?R) or a === ?S) or a === ?T) or a === ?U) or a === ?V) or a === ?W) or a === ?X) or a === ?Y) or a === ?Z) or a === ?0) or a === ?1) or a === ?2) or a === ?3) or a === ?4) or a === ?5) or a === ?6) or a === ?7) or a === ?8) or a === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((b === ?a or b === ?b) or b === ?c) or b === ?d) or b === ?e) or b === ?f) or b === ?g) or b === ?h) or b === ?i) or b === ?j) or b === ?k) or b === ?l) or b === ?m) or b === ?n) or b === ?o) or b === ?p) or b === ?q) or b === ?r) or b === ?s) or b === ?t) or b === ?u) or b === ?v) or b === ?w) or b === ?x) or b === ?y) or b === ?z) or b === ?A) or b === ?B) or b === ?C) or b === ?D) or b === ?E) or b === ?F) or b === ?G) or b === ?H) or b === ?I) or b === ?J) or b === ?K) or b === ?L) or b === ?M) or b === ?N) or b === ?O) or b === ?P) or b === ?Q) or b === ?R) or b === ?S) or b === ?T) or b === ?U) or b === ?V) or b === ?W) or b === ?X) or b === ?Y) or b === ?Z) or b === ?0) or b === ?1) or b === ?2) or b === ?3) or b === ?4) or b === ?5) or b === ?6) or b === ?7) or b === ?8) or b === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((d === ?a or d === ?b) or d === ?c) or d === ?d) or d === ?e) or d === ?f) or d === ?g) or d === ?h) or d === ?i) or d === ?j) or d === ?k) or d === ?l) or d === ?m) or d === ?n) or d === ?o) or d === ?p) or d === ?q) or d === ?r) or d === ?s) or d === ?t) or d === ?u) or d === ?v) or d === ?w) or d === ?x) or d === ?y) or d === ?z) or d === ?A) or d === ?B) or d === ?C) or d === ?D) or d === ?E) or d === ?F) or d === ?G) or d === ?H) or d === ?I) or d === ?J) or d === ?K) or d === ?L) or d === ?M) or d === ?N) or d === ?O) or d === ?P) or d === ?Q) or d === ?R) or d === ?S) or d === ?T) or d === ?U) or d === ?V) or d === ?W) or d === ?X) or d === ?Y) or d === ?Z) or d === ?0) or d === ?1) or d === ?2) or d === ?3) or d === ?4) or d === ?5) or d === ?6) or d === ?7) or d === ?8) or d === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((e === ?a or e === ?b) or e === ?c) or e === ?d) or e === ?e) or e === ?f) or e === ?g) or e === ?h) or e === ?i) or e === ?j) or e === ?k) or e === ?l) or e === ?m) or e === ?n) or e === ?o) or e === ?p) or e === ?q) or e === ?r) or e === ?s) or e === ?t) or e === ?u) or e === ?v) or e === ?w) or e === ?x) or e === ?y) or e === ?z) or e === ?A) or e === ?B) or e === ?C) or e === ?D) or e === ?E) or e === ?F) or e === ?G) or e === ?H) or e === ?I) or e === ?J) or e === ?K) or e === ?L) or e === ?M) or e === ?N) or e === ?O) or e === ?P) or e === ?Q) or e === ?R) or e === ?S) or e === ?T) or e === ?U) or e === ?V) or e === ?W) or e === ?X) or e === ?Y) or e === ?Z) or e === ?0) or e === ?1) or e === ?2) or e === ?3) or e === ?4) or e === ?5) or e === ?6) or e === ?7) or e === ?8) or e === ?9) do
    langtag_variant(r, acc,
                      <<t :: binary, ?-,
                          case (a) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              a
                          end,
                          case (b) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              b
                          end,
                          case (c) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              c
                          end,
                          case (d) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              d
                          end,
                          case (e) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              e
                          end>>)
  end

  defp langtag_variant(<<?-, a, b, c, d, r :: bits>>, acc, t)
      when (((((((((a === ?0 or a === ?1) or a === ?2) or a === ?3) or a === ?4) or a === ?5) or a === ?6) or a === ?7) or a === ?8) or a === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((b === ?a or b === ?b) or b === ?c) or b === ?d) or b === ?e) or b === ?f) or b === ?g) or b === ?h) or b === ?i) or b === ?j) or b === ?k) or b === ?l) or b === ?m) or b === ?n) or b === ?o) or b === ?p) or b === ?q) or b === ?r) or b === ?s) or b === ?t) or b === ?u) or b === ?v) or b === ?w) or b === ?x) or b === ?y) or b === ?z) or b === ?A) or b === ?B) or b === ?C) or b === ?D) or b === ?E) or b === ?F) or b === ?G) or b === ?H) or b === ?I) or b === ?J) or b === ?K) or b === ?L) or b === ?M) or b === ?N) or b === ?O) or b === ?P) or b === ?Q) or b === ?R) or b === ?S) or b === ?T) or b === ?U) or b === ?V) or b === ?W) or b === ?X) or b === ?Y) or b === ?Z) or b === ?0) or b === ?1) or b === ?2) or b === ?3) or b === ?4) or b === ?5) or b === ?6) or b === ?7) or b === ?8) or b === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((d === ?a or d === ?b) or d === ?c) or d === ?d) or d === ?e) or d === ?f) or d === ?g) or d === ?h) or d === ?i) or d === ?j) or d === ?k) or d === ?l) or d === ?m) or d === ?n) or d === ?o) or d === ?p) or d === ?q) or d === ?r) or d === ?s) or d === ?t) or d === ?u) or d === ?v) or d === ?w) or d === ?x) or d === ?y) or d === ?z) or d === ?A) or d === ?B) or d === ?C) or d === ?D) or d === ?E) or d === ?F) or d === ?G) or d === ?H) or d === ?I) or d === ?J) or d === ?K) or d === ?L) or d === ?M) or d === ?N) or d === ?O) or d === ?P) or d === ?Q) or d === ?R) or d === ?S) or d === ?T) or d === ?U) or d === ?V) or d === ?W) or d === ?X) or d === ?Y) or d === ?Z) or d === ?0) or d === ?1) or d === ?2) or d === ?3) or d === ?4) or d === ?5) or d === ?6) or d === ?7) or d === ?8) or d === ?9) do
    langtag_variant(r, acc,
                      <<t :: binary, ?-, a,
                          case (b) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              b
                          end,
                          case (c) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              c
                          end,
                          case (d) do
                            ?A ->
                              ?a
                            ?B ->
                              ?b
                            ?C ->
                              ?c
                            ?D ->
                              ?d
                            ?E ->
                              ?e
                            ?F ->
                              ?f
                            ?G ->
                              ?g
                            ?H ->
                              ?h
                            ?I ->
                              ?i
                            ?J ->
                              ?j
                            ?K ->
                              ?k
                            ?L ->
                              ?l
                            ?M ->
                              ?m
                            ?N ->
                              ?n
                            ?O ->
                              ?o
                            ?P ->
                              ?p
                            ?Q ->
                              ?q
                            ?R ->
                              ?r
                            ?S ->
                              ?s
                            ?T ->
                              ?t
                            ?U ->
                              ?u
                            ?V ->
                              ?v
                            ?W ->
                              ?w
                            ?X ->
                              ?x
                            ?Y ->
                              ?y
                            ?Z ->
                              ?z
                            _ ->
                              d
                          end>>)
  end

  defp langtag_variant(r, acc, t) do
    langtag_extension(r, acc, t)
  end

  defp langtag_extension(<<?-, x, r :: bits>>, acc, t) when x === ?x or
                                               x === ?X do
    langtag_privateuse_sub(r, acc, <<t :: binary, ?-, ?x>>,
                             0)
  end

  defp langtag_extension(<<?-, s, r :: bits>>, acc, t)
      when ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((s === ?a or s === ?b) or s === ?c) or s === ?d) or s === ?e) or s === ?f) or s === ?g) or s === ?h) or s === ?i) or s === ?j) or s === ?k) or s === ?l) or s === ?m) or s === ?n) or s === ?o) or s === ?p) or s === ?q) or s === ?r) or s === ?s) or s === ?t) or s === ?u) or s === ?v) or s === ?w) or s === ?x) or s === ?y) or s === ?z) or s === ?A) or s === ?B) or s === ?C) or s === ?D) or s === ?E) or s === ?F) or s === ?G) or s === ?H) or s === ?I) or s === ?J) or s === ?K) or s === ?L) or s === ?M) or s === ?N) or s === ?O) or s === ?P) or s === ?Q) or s === ?R) or s === ?S) or s === ?T) or s === ?U) or s === ?V) or s === ?W) or s === ?X) or s === ?Y) or s === ?Z) or s === ?0) or s === ?1) or s === ?2) or s === ?3) or s === ?4) or s === ?5) or s === ?6) or s === ?7) or s === ?8) or s === ?9 do
    langtag_extension_sub(r, acc,
                            <<t :: binary, ?-,
                                case (s) do
                                  ?A ->
                                    ?a
                                  ?B ->
                                    ?b
                                  ?C ->
                                    ?c
                                  ?D ->
                                    ?d
                                  ?E ->
                                    ?e
                                  ?F ->
                                    ?f
                                  ?G ->
                                    ?g
                                  ?H ->
                                    ?h
                                  ?I ->
                                    ?i
                                  ?J ->
                                    ?j
                                  ?K ->
                                    ?k
                                  ?L ->
                                    ?l
                                  ?M ->
                                    ?m
                                  ?N ->
                                    ?n
                                  ?O ->
                                    ?o
                                  ?P ->
                                    ?p
                                  ?Q ->
                                    ?q
                                  ?R ->
                                    ?r
                                  ?S ->
                                    ?s
                                  ?T ->
                                    ?t
                                  ?U ->
                                    ?u
                                  ?V ->
                                    ?v
                                  ?W ->
                                    ?w
                                  ?X ->
                                    ?x
                                  ?Y ->
                                    ?y
                                  ?Z ->
                                    ?z
                                  _ ->
                                    s
                                end>>,
                            0)
  end

  defp langtag_extension(r, acc, t) do
    langtag_list_sep(r, [t | acc])
  end

  defp langtag_extension_sub(<<?-, a, b, c, d, e, f, g, h, r :: bits>>, acc,
            t, n)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((a === ?a or a === ?b) or a === ?c) or a === ?d) or a === ?e) or a === ?f) or a === ?g) or a === ?h) or a === ?i) or a === ?j) or a === ?k) or a === ?l) or a === ?m) or a === ?n) or a === ?o) or a === ?p) or a === ?q) or a === ?r) or a === ?s) or a === ?t) or a === ?u) or a === ?v) or a === ?w) or a === ?x) or a === ?y) or a === ?z) or a === ?A) or a === ?B) or a === ?C) or a === ?D) or a === ?E) or a === ?F) or a === ?G) or a === ?H) or a === ?I) or a === ?J) or a === ?K) or a === ?L) or a === ?M) or a === ?N) or a === ?O) or a === ?P) or a === ?Q) or a === ?R) or a === ?S) or a === ?T) or a === ?U) or a === ?V) or a === ?W) or a === ?X) or a === ?Y) or a === ?Z) or a === ?0) or a === ?1) or a === ?2) or a === ?3) or a === ?4) or a === ?5) or a === ?6) or a === ?7) or a === ?8) or a === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((b === ?a or b === ?b) or b === ?c) or b === ?d) or b === ?e) or b === ?f) or b === ?g) or b === ?h) or b === ?i) or b === ?j) or b === ?k) or b === ?l) or b === ?m) or b === ?n) or b === ?o) or b === ?p) or b === ?q) or b === ?r) or b === ?s) or b === ?t) or b === ?u) or b === ?v) or b === ?w) or b === ?x) or b === ?y) or b === ?z) or b === ?A) or b === ?B) or b === ?C) or b === ?D) or b === ?E) or b === ?F) or b === ?G) or b === ?H) or b === ?I) or b === ?J) or b === ?K) or b === ?L) or b === ?M) or b === ?N) or b === ?O) or b === ?P) or b === ?Q) or b === ?R) or b === ?S) or b === ?T) or b === ?U) or b === ?V) or b === ?W) or b === ?X) or b === ?Y) or b === ?Z) or b === ?0) or b === ?1) or b === ?2) or b === ?3) or b === ?4) or b === ?5) or b === ?6) or b === ?7) or b === ?8) or b === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((d === ?a or d === ?b) or d === ?c) or d === ?d) or d === ?e) or d === ?f) or d === ?g) or d === ?h) or d === ?i) or d === ?j) or d === ?k) or d === ?l) or d === ?m) or d === ?n) or d === ?o) or d === ?p) or d === ?q) or d === ?r) or d === ?s) or d === ?t) or d === ?u) or d === ?v) or d === ?w) or d === ?x) or d === ?y) or d === ?z) or d === ?A) or d === ?B) or d === ?C) or d === ?D) or d === ?E) or d === ?F) or d === ?G) or d === ?H) or d === ?I) or d === ?J) or d === ?K) or d === ?L) or d === ?M) or d === ?N) or d === ?O) or d === ?P) or d === ?Q) or d === ?R) or d === ?S) or d === ?T) or d === ?U) or d === ?V) or d === ?W) or d === ?X) or d === ?Y) or d === ?Z) or d === ?0) or d === ?1) or d === ?2) or d === ?3) or d === ?4) or d === ?5) or d === ?6) or d === ?7) or d === ?8) or d === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((e === ?a or e === ?b) or e === ?c) or e === ?d) or e === ?e) or e === ?f) or e === ?g) or e === ?h) or e === ?i) or e === ?j) or e === ?k) or e === ?l) or e === ?m) or e === ?n) or e === ?o) or e === ?p) or e === ?q) or e === ?r) or e === ?s) or e === ?t) or e === ?u) or e === ?v) or e === ?w) or e === ?x) or e === ?y) or e === ?z) or e === ?A) or e === ?B) or e === ?C) or e === ?D) or e === ?E) or e === ?F) or e === ?G) or e === ?H) or e === ?I) or e === ?J) or e === ?K) or e === ?L) or e === ?M) or e === ?N) or e === ?O) or e === ?P) or e === ?Q) or e === ?R) or e === ?S) or e === ?T) or e === ?U) or e === ?V) or e === ?W) or e === ?X) or e === ?Y) or e === ?Z) or e === ?0) or e === ?1) or e === ?2) or e === ?3) or e === ?4) or e === ?5) or e === ?6) or e === ?7) or e === ?8) or e === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((f === ?a or f === ?b) or f === ?c) or f === ?d) or f === ?e) or f === ?f) or f === ?g) or f === ?h) or f === ?i) or f === ?j) or f === ?k) or f === ?l) or f === ?m) or f === ?n) or f === ?o) or f === ?p) or f === ?q) or f === ?r) or f === ?s) or f === ?t) or f === ?u) or f === ?v) or f === ?w) or f === ?x) or f === ?y) or f === ?z) or f === ?A) or f === ?B) or f === ?C) or f === ?D) or f === ?E) or f === ?F) or f === ?G) or f === ?H) or f === ?I) or f === ?J) or f === ?K) or f === ?L) or f === ?M) or f === ?N) or f === ?O) or f === ?P) or f === ?Q) or f === ?R) or f === ?S) or f === ?T) or f === ?U) or f === ?V) or f === ?W) or f === ?X) or f === ?Y) or f === ?Z) or f === ?0) or f === ?1) or f === ?2) or f === ?3) or f === ?4) or f === ?5) or f === ?6) or f === ?7) or f === ?8) or f === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((g === ?a or g === ?b) or g === ?c) or g === ?d) or g === ?e) or g === ?f) or g === ?g) or g === ?h) or g === ?i) or g === ?j) or g === ?k) or g === ?l) or g === ?m) or g === ?n) or g === ?o) or g === ?p) or g === ?q) or g === ?r) or g === ?s) or g === ?t) or g === ?u) or g === ?v) or g === ?w) or g === ?x) or g === ?y) or g === ?z) or g === ?A) or g === ?B) or g === ?C) or g === ?D) or g === ?E) or g === ?F) or g === ?G) or g === ?H) or g === ?I) or g === ?J) or g === ?K) or g === ?L) or g === ?M) or g === ?N) or g === ?O) or g === ?P) or g === ?Q) or g === ?R) or g === ?S) or g === ?T) or g === ?U) or g === ?V) or g === ?W) or g === ?X) or g === ?Y) or g === ?Z) or g === ?0) or g === ?1) or g === ?2) or g === ?3) or g === ?4) or g === ?5) or g === ?6) or g === ?7) or g === ?8) or g === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((h === ?a or h === ?b) or h === ?c) or h === ?d) or h === ?e) or h === ?f) or h === ?g) or h === ?h) or h === ?i) or h === ?j) or h === ?k) or h === ?l) or h === ?m) or h === ?n) or h === ?o) or h === ?p) or h === ?q) or h === ?r) or h === ?s) or h === ?t) or h === ?u) or h === ?v) or h === ?w) or h === ?x) or h === ?y) or h === ?z) or h === ?A) or h === ?B) or h === ?C) or h === ?D) or h === ?E) or h === ?F) or h === ?G) or h === ?H) or h === ?I) or h === ?J) or h === ?K) or h === ?L) or h === ?M) or h === ?N) or h === ?O) or h === ?P) or h === ?Q) or h === ?R) or h === ?S) or h === ?T) or h === ?U) or h === ?V) or h === ?W) or h === ?X) or h === ?Y) or h === ?Z) or h === ?0) or h === ?1) or h === ?2) or h === ?3) or h === ?4) or h === ?5) or h === ?6) or h === ?7) or h === ?8) or h === ?9) do
    langtag_extension_sub(r, acc,
                            <<t :: binary, ?-,
                                case (a) do
                                  ?A ->
                                    ?a
                                  ?B ->
                                    ?b
                                  ?C ->
                                    ?c
                                  ?D ->
                                    ?d
                                  ?E ->
                                    ?e
                                  ?F ->
                                    ?f
                                  ?G ->
                                    ?g
                                  ?H ->
                                    ?h
                                  ?I ->
                                    ?i
                                  ?J ->
                                    ?j
                                  ?K ->
                                    ?k
                                  ?L ->
                                    ?l
                                  ?M ->
                                    ?m
                                  ?N ->
                                    ?n
                                  ?O ->
                                    ?o
                                  ?P ->
                                    ?p
                                  ?Q ->
                                    ?q
                                  ?R ->
                                    ?r
                                  ?S ->
                                    ?s
                                  ?T ->
                                    ?t
                                  ?U ->
                                    ?u
                                  ?V ->
                                    ?v
                                  ?W ->
                                    ?w
                                  ?X ->
                                    ?x
                                  ?Y ->
                                    ?y
                                  ?Z ->
                                    ?z
                                  _ ->
                                    a
                                end,
                                case (b) do
                                  ?A ->
                                    ?a
                                  ?B ->
                                    ?b
                                  ?C ->
                                    ?c
                                  ?D ->
                                    ?d
                                  ?E ->
                                    ?e
                                  ?F ->
                                    ?f
                                  ?G ->
                                    ?g
                                  ?H ->
                                    ?h
                                  ?I ->
                                    ?i
                                  ?J ->
                                    ?j
                                  ?K ->
                                    ?k
                                  ?L ->
                                    ?l
                                  ?M ->
                                    ?m
                                  ?N ->
                                    ?n
                                  ?O ->
                                    ?o
                                  ?P ->
                                    ?p
                                  ?Q ->
                                    ?q
                                  ?R ->
                                    ?r
                                  ?S ->
                                    ?s
                                  ?T ->
                                    ?t
                                  ?U ->
                                    ?u
                                  ?V ->
                                    ?v
                                  ?W ->
                                    ?w
                                  ?X ->
                                    ?x
                                  ?Y ->
                                    ?y
                                  ?Z ->
                                    ?z
                                  _ ->
                                    b
                                end,
                                case (c) do
                                  ?A ->
                                    ?a
                                  ?B ->
                                    ?b
                                  ?C ->
                                    ?c
                                  ?D ->
                                    ?d
                                  ?E ->
                                    ?e
                                  ?F ->
                                    ?f
                                  ?G ->
                                    ?g
                                  ?H ->
                                    ?h
                                  ?I ->
                                    ?i
                                  ?J ->
                                    ?j
                                  ?K ->
                                    ?k
                                  ?L ->
                                    ?l
                                  ?M ->
                                    ?m
                                  ?N ->
                                    ?n
                                  ?O ->
                                    ?o
                                  ?P ->
                                    ?p
                                  ?Q ->
                                    ?q
                                  ?R ->
                                    ?r
                                  ?S ->
                                    ?s
                                  ?T ->
                                    ?t
                                  ?U ->
                                    ?u
                                  ?V ->
                                    ?v
                                  ?W ->
                                    ?w
                                  ?X ->
                                    ?x
                                  ?Y ->
                                    ?y
                                  ?Z ->
                                    ?z
                                  _ ->
                                    c
                                end,
                                case (d) do
                                  ?A ->
                                    ?a
                                  ?B ->
                                    ?b
                                  ?C ->
                                    ?c
                                  ?D ->
                                    ?d
                                  ?E ->
                                    ?e
                                  ?F ->
                                    ?f
                                  ?G ->
                                    ?g
                                  ?H ->
                                    ?h
                                  ?I ->
                                    ?i
                                  ?J ->
                                    ?j
                                  ?K ->
                                    ?k
                                  ?L ->
                                    ?l
                                  ?M ->
                                    ?m
                                  ?N ->
                                    ?n
                                  ?O ->
                                    ?o
                                  ?P ->
                                    ?p
                                  ?Q ->
                                    ?q
                                  ?R ->
                                    ?r
                                  ?S ->
                                    ?s
                                  ?T ->
                                    ?t
                                  ?U ->
                                    ?u
                                  ?V ->
                                    ?v
                                  ?W ->
                                    ?w
                                  ?X ->
                                    ?x
                                  ?Y ->
                                    ?y
                                  ?Z ->
                                    ?z
                                  _ ->
                                    d
                                end,
                                case (e) do
                                  ?A ->
                                    ?a
                                  ?B ->
                                    ?b
                                  ?C ->
                                    ?c
                                  ?D ->
                                    ?d
                                  ?E ->
                                    ?e
                                  ?F ->
                                    ?f
                                  ?G ->
                                    ?g
                                  ?H ->
                                    ?h
                                  ?I ->
                                    ?i
                                  ?J ->
                                    ?j
                                  ?K ->
                                    ?k
                                  ?L ->
                                    ?l
                                  ?M ->
                                    ?m
                                  ?N ->
                                    ?n
                                  ?O ->
                                    ?o
                                  ?P ->
                                    ?p
                                  ?Q ->
                                    ?q
                                  ?R ->
                                    ?r
                                  ?S ->
                                    ?s
                                  ?T ->
                                    ?t
                                  ?U ->
                                    ?u
                                  ?V ->
                                    ?v
                                  ?W ->
                                    ?w
                                  ?X ->
                                    ?x
                                  ?Y ->
                                    ?y
                                  ?Z ->
                                    ?z
                                  _ ->
                                    e
                                end,
                                case (f) do
                                  ?A ->
                                    ?a
                                  ?B ->
                                    ?b
                                  ?C ->
                                    ?c
                                  ?D ->
                                    ?d
                                  ?E ->
                                    ?e
                                  ?F ->
                                    ?f
                                  ?G ->
                                    ?g
                                  ?H ->
                                    ?h
                                  ?I ->
                                    ?i
                                  ?J ->
                                    ?j
                                  ?K ->
                                    ?k
                                  ?L ->
                                    ?l
                                  ?M ->
                                    ?m
                                  ?N ->
                                    ?n
                                  ?O ->
                                    ?o
                                  ?P ->
                                    ?p
                                  ?Q ->
                                    ?q
                                  ?R ->
                                    ?r
                                  ?S ->
                                    ?s
                                  ?T ->
                                    ?t
                                  ?U ->
                                    ?u
                                  ?V ->
                                    ?v
                                  ?W ->
                                    ?w
                                  ?X ->
                                    ?x
                                  ?Y ->
                                    ?y
                                  ?Z ->
                                    ?z
                                  _ ->
                                    f
                                end,
                                case (g) do
                                  ?A ->
                                    ?a
                                  ?B ->
                                    ?b
                                  ?C ->
                                    ?c
                                  ?D ->
                                    ?d
                                  ?E ->
                                    ?e
                                  ?F ->
                                    ?f
                                  ?G ->
                                    ?g
                                  ?H ->
                                    ?h
                                  ?I ->
                                    ?i
                                  ?J ->
                                    ?j
                                  ?K ->
                                    ?k
                                  ?L ->
                                    ?l
                                  ?M ->
                                    ?m
                                  ?N ->
                                    ?n
                                  ?O ->
                                    ?o
                                  ?P ->
                                    ?p
                                  ?Q ->
                                    ?q
                                  ?R ->
                                    ?r
                                  ?S ->
                                    ?s
                                  ?T ->
                                    ?t
                                  ?U ->
                                    ?u
                                  ?V ->
                                    ?v
                                  ?W ->
                                    ?w
                                  ?X ->
                                    ?x
                                  ?Y ->
                                    ?y
                                  ?Z ->
                                    ?z
                                  _ ->
                                    g
                                end,
                                case (h) do
                                  ?A ->
                                    ?a
                                  ?B ->
                                    ?b
                                  ?C ->
                                    ?c
                                  ?D ->
                                    ?d
                                  ?E ->
                                    ?e
                                  ?F ->
                                    ?f
                                  ?G ->
                                    ?g
                                  ?H ->
                                    ?h
                                  ?I ->
                                    ?i
                                  ?J ->
                                    ?j
                                  ?K ->
                                    ?k
                                  ?L ->
                                    ?l
                                  ?M ->
                                    ?m
                                  ?N ->
                                    ?n
                                  ?O ->
                                    ?o
                                  ?P ->
                                    ?p
                                  ?Q ->
                                    ?q
                                  ?R ->
                                    ?r
                                  ?S ->
                                    ?s
                                  ?T ->
                                    ?t
                                  ?U ->
                                    ?u
                                  ?V ->
                                    ?v
                                  ?W ->
                                    ?w
                                  ?X ->
                                    ?x
                                  ?Y ->
                                    ?y
                                  ?Z ->
                                    ?z
                                  _ ->
                                    h
                                end>>,
                            n + 1)
  end

  defp langtag_extension_sub(<<?-, a, b, c, d, e, f, g, r :: bits>>, acc, t,
            n)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((a === ?a or a === ?b) or a === ?c) or a === ?d) or a === ?e) or a === ?f) or a === ?g) or a === ?h) or a === ?i) or a === ?j) or a === ?k) or a === ?l) or a === ?m) or a === ?n) or a === ?o) or a === ?p) or a === ?q) or a === ?r) or a === ?s) or a === ?t) or a === ?u) or a === ?v) or a === ?w) or a === ?x) or a === ?y) or a === ?z) or a === ?A) or a === ?B) or a === ?C) or a === ?D) or a === ?E) or a === ?F) or a === ?G) or a === ?H) or a === ?I) or a === ?J) or a === ?K) or a === ?L) or a === ?M) or a === ?N) or a === ?O) or a === ?P) or a === ?Q) or a === ?R) or a === ?S) or a === ?T) or a === ?U) or a === ?V) or a === ?W) or a === ?X) or a === ?Y) or a === ?Z) or a === ?0) or a === ?1) or a === ?2) or a === ?3) or a === ?4) or a === ?5) or a === ?6) or a === ?7) or a === ?8) or a === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((b === ?a or b === ?b) or b === ?c) or b === ?d) or b === ?e) or b === ?f) or b === ?g) or b === ?h) or b === ?i) or b === ?j) or b === ?k) or b === ?l) or b === ?m) or b === ?n) or b === ?o) or b === ?p) or b === ?q) or b === ?r) or b === ?s) or b === ?t) or b === ?u) or b === ?v) or b === ?w) or b === ?x) or b === ?y) or b === ?z) or b === ?A) or b === ?B) or b === ?C) or b === ?D) or b === ?E) or b === ?F) or b === ?G) or b === ?H) or b === ?I) or b === ?J) or b === ?K) or b === ?L) or b === ?M) or b === ?N) or b === ?O) or b === ?P) or b === ?Q) or b === ?R) or b === ?S) or b === ?T) or b === ?U) or b === ?V) or b === ?W) or b === ?X) or b === ?Y) or b === ?Z) or b === ?0) or b === ?1) or b === ?2) or b === ?3) or b === ?4) or b === ?5) or b === ?6) or b === ?7) or b === ?8) or b === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((d === ?a or d === ?b) or d === ?c) or d === ?d) or d === ?e) or d === ?f) or d === ?g) or d === ?h) or d === ?i) or d === ?j) or d === ?k) or d === ?l) or d === ?m) or d === ?n) or d === ?o) or d === ?p) or d === ?q) or d === ?r) or d === ?s) or d === ?t) or d === ?u) or d === ?v) or d === ?w) or d === ?x) or d === ?y) or d === ?z) or d === ?A) or d === ?B) or d === ?C) or d === ?D) or d === ?E) or d === ?F) or d === ?G) or d === ?H) or d === ?I) or d === ?J) or d === ?K) or d === ?L) or d === ?M) or d === ?N) or d === ?O) or d === ?P) or d === ?Q) or d === ?R) or d === ?S) or d === ?T) or d === ?U) or d === ?V) or d === ?W) or d === ?X) or d === ?Y) or d === ?Z) or d === ?0) or d === ?1) or d === ?2) or d === ?3) or d === ?4) or d === ?5) or d === ?6) or d === ?7) or d === ?8) or d === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((e === ?a or e === ?b) or e === ?c) or e === ?d) or e === ?e) or e === ?f) or e === ?g) or e === ?h) or e === ?i) or e === ?j) or e === ?k) or e === ?l) or e === ?m) or e === ?n) or e === ?o) or e === ?p) or e === ?q) or e === ?r) or e === ?s) or e === ?t) or e === ?u) or e === ?v) or e === ?w) or e === ?x) or e === ?y) or e === ?z) or e === ?A) or e === ?B) or e === ?C) or e === ?D) or e === ?E) or e === ?F) or e === ?G) or e === ?H) or e === ?I) or e === ?J) or e === ?K) or e === ?L) or e === ?M) or e === ?N) or e === ?O) or e === ?P) or e === ?Q) or e === ?R) or e === ?S) or e === ?T) or e === ?U) or e === ?V) or e === ?W) or e === ?X) or e === ?Y) or e === ?Z) or e === ?0) or e === ?1) or e === ?2) or e === ?3) or e === ?4) or e === ?5) or e === ?6) or e === ?7) or e === ?8) or e === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((f === ?a or f === ?b) or f === ?c) or f === ?d) or f === ?e) or f === ?f) or f === ?g) or f === ?h) or f === ?i) or f === ?j) or f === ?k) or f === ?l) or f === ?m) or f === ?n) or f === ?o) or f === ?p) or f === ?q) or f === ?r) or f === ?s) or f === ?t) or f === ?u) or f === ?v) or f === ?w) or f === ?x) or f === ?y) or f === ?z) or f === ?A) or f === ?B) or f === ?C) or f === ?D) or f === ?E) or f === ?F) or f === ?G) or f === ?H) or f === ?I) or f === ?J) or f === ?K) or f === ?L) or f === ?M) or f === ?N) or f === ?O) or f === ?P) or f === ?Q) or f === ?R) or f === ?S) or f === ?T) or f === ?U) or f === ?V) or f === ?W) or f === ?X) or f === ?Y) or f === ?Z) or f === ?0) or f === ?1) or f === ?2) or f === ?3) or f === ?4) or f === ?5) or f === ?6) or f === ?7) or f === ?8) or f === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((g === ?a or g === ?b) or g === ?c) or g === ?d) or g === ?e) or g === ?f) or g === ?g) or g === ?h) or g === ?i) or g === ?j) or g === ?k) or g === ?l) or g === ?m) or g === ?n) or g === ?o) or g === ?p) or g === ?q) or g === ?r) or g === ?s) or g === ?t) or g === ?u) or g === ?v) or g === ?w) or g === ?x) or g === ?y) or g === ?z) or g === ?A) or g === ?B) or g === ?C) or g === ?D) or g === ?E) or g === ?F) or g === ?G) or g === ?H) or g === ?I) or g === ?J) or g === ?K) or g === ?L) or g === ?M) or g === ?N) or g === ?O) or g === ?P) or g === ?Q) or g === ?R) or g === ?S) or g === ?T) or g === ?U) or g === ?V) or g === ?W) or g === ?X) or g === ?Y) or g === ?Z) or g === ?0) or g === ?1) or g === ?2) or g === ?3) or g === ?4) or g === ?5) or g === ?6) or g === ?7) or g === ?8) or g === ?9) do
    langtag_extension_sub(r, acc,
                            <<t :: binary, ?-,
                                case (a) do
                                  ?A ->
                                    ?a
                                  ?B ->
                                    ?b
                                  ?C ->
                                    ?c
                                  ?D ->
                                    ?d
                                  ?E ->
                                    ?e
                                  ?F ->
                                    ?f
                                  ?G ->
                                    ?g
                                  ?H ->
                                    ?h
                                  ?I ->
                                    ?i
                                  ?J ->
                                    ?j
                                  ?K ->
                                    ?k
                                  ?L ->
                                    ?l
                                  ?M ->
                                    ?m
                                  ?N ->
                                    ?n
                                  ?O ->
                                    ?o
                                  ?P ->
                                    ?p
                                  ?Q ->
                                    ?q
                                  ?R ->
                                    ?r
                                  ?S ->
                                    ?s
                                  ?T ->
                                    ?t
                                  ?U ->
                                    ?u
                                  ?V ->
                                    ?v
                                  ?W ->
                                    ?w
                                  ?X ->
                                    ?x
                                  ?Y ->
                                    ?y
                                  ?Z ->
                                    ?z
                                  _ ->
                                    a
                                end,
                                case (b) do
                                  ?A ->
                                    ?a
                                  ?B ->
                                    ?b
                                  ?C ->
                                    ?c
                                  ?D ->
                                    ?d
                                  ?E ->
                                    ?e
                                  ?F ->
                                    ?f
                                  ?G ->
                                    ?g
                                  ?H ->
                                    ?h
                                  ?I ->
                                    ?i
                                  ?J ->
                                    ?j
                                  ?K ->
                                    ?k
                                  ?L ->
                                    ?l
                                  ?M ->
                                    ?m
                                  ?N ->
                                    ?n
                                  ?O ->
                                    ?o
                                  ?P ->
                                    ?p
                                  ?Q ->
                                    ?q
                                  ?R ->
                                    ?r
                                  ?S ->
                                    ?s
                                  ?T ->
                                    ?t
                                  ?U ->
                                    ?u
                                  ?V ->
                                    ?v
                                  ?W ->
                                    ?w
                                  ?X ->
                                    ?x
                                  ?Y ->
                                    ?y
                                  ?Z ->
                                    ?z
                                  _ ->
                                    b
                                end,
                                case (c) do
                                  ?A ->
                                    ?a
                                  ?B ->
                                    ?b
                                  ?C ->
                                    ?c
                                  ?D ->
                                    ?d
                                  ?E ->
                                    ?e
                                  ?F ->
                                    ?f
                                  ?G ->
                                    ?g
                                  ?H ->
                                    ?h
                                  ?I ->
                                    ?i
                                  ?J ->
                                    ?j
                                  ?K ->
                                    ?k
                                  ?L ->
                                    ?l
                                  ?M ->
                                    ?m
                                  ?N ->
                                    ?n
                                  ?O ->
                                    ?o
                                  ?P ->
                                    ?p
                                  ?Q ->
                                    ?q
                                  ?R ->
                                    ?r
                                  ?S ->
                                    ?s
                                  ?T ->
                                    ?t
                                  ?U ->
                                    ?u
                                  ?V ->
                                    ?v
                                  ?W ->
                                    ?w
                                  ?X ->
                                    ?x
                                  ?Y ->
                                    ?y
                                  ?Z ->
                                    ?z
                                  _ ->
                                    c
                                end,
                                case (d) do
                                  ?A ->
                                    ?a
                                  ?B ->
                                    ?b
                                  ?C ->
                                    ?c
                                  ?D ->
                                    ?d
                                  ?E ->
                                    ?e
                                  ?F ->
                                    ?f
                                  ?G ->
                                    ?g
                                  ?H ->
                                    ?h
                                  ?I ->
                                    ?i
                                  ?J ->
                                    ?j
                                  ?K ->
                                    ?k
                                  ?L ->
                                    ?l
                                  ?M ->
                                    ?m
                                  ?N ->
                                    ?n
                                  ?O ->
                                    ?o
                                  ?P ->
                                    ?p
                                  ?Q ->
                                    ?q
                                  ?R ->
                                    ?r
                                  ?S ->
                                    ?s
                                  ?T ->
                                    ?t
                                  ?U ->
                                    ?u
                                  ?V ->
                                    ?v
                                  ?W ->
                                    ?w
                                  ?X ->
                                    ?x
                                  ?Y ->
                                    ?y
                                  ?Z ->
                                    ?z
                                  _ ->
                                    d
                                end,
                                case (e) do
                                  ?A ->
                                    ?a
                                  ?B ->
                                    ?b
                                  ?C ->
                                    ?c
                                  ?D ->
                                    ?d
                                  ?E ->
                                    ?e
                                  ?F ->
                                    ?f
                                  ?G ->
                                    ?g
                                  ?H ->
                                    ?h
                                  ?I ->
                                    ?i
                                  ?J ->
                                    ?j
                                  ?K ->
                                    ?k
                                  ?L ->
                                    ?l
                                  ?M ->
                                    ?m
                                  ?N ->
                                    ?n
                                  ?O ->
                                    ?o
                                  ?P ->
                                    ?p
                                  ?Q ->
                                    ?q
                                  ?R ->
                                    ?r
                                  ?S ->
                                    ?s
                                  ?T ->
                                    ?t
                                  ?U ->
                                    ?u
                                  ?V ->
                                    ?v
                                  ?W ->
                                    ?w
                                  ?X ->
                                    ?x
                                  ?Y ->
                                    ?y
                                  ?Z ->
                                    ?z
                                  _ ->
                                    e
                                end,
                                case (f) do
                                  ?A ->
                                    ?a
                                  ?B ->
                                    ?b
                                  ?C ->
                                    ?c
                                  ?D ->
                                    ?d
                                  ?E ->
                                    ?e
                                  ?F ->
                                    ?f
                                  ?G ->
                                    ?g
                                  ?H ->
                                    ?h
                                  ?I ->
                                    ?i
                                  ?J ->
                                    ?j
                                  ?K ->
                                    ?k
                                  ?L ->
                                    ?l
                                  ?M ->
                                    ?m
                                  ?N ->
                                    ?n
                                  ?O ->
                                    ?o
                                  ?P ->
                                    ?p
                                  ?Q ->
                                    ?q
                                  ?R ->
                                    ?r
                                  ?S ->
                                    ?s
                                  ?T ->
                                    ?t
                                  ?U ->
                                    ?u
                                  ?V ->
                                    ?v
                                  ?W ->
                                    ?w
                                  ?X ->
                                    ?x
                                  ?Y ->
                                    ?y
                                  ?Z ->
                                    ?z
                                  _ ->
                                    f
                                end,
                                case (g) do
                                  ?A ->
                                    ?a
                                  ?B ->
                                    ?b
                                  ?C ->
                                    ?c
                                  ?D ->
                                    ?d
                                  ?E ->
                                    ?e
                                  ?F ->
                                    ?f
                                  ?G ->
                                    ?g
                                  ?H ->
                                    ?h
                                  ?I ->
                                    ?i
                                  ?J ->
                                    ?j
                                  ?K ->
                                    ?k
                                  ?L ->
                                    ?l
                                  ?M ->
                                    ?m
                                  ?N ->
                                    ?n
                                  ?O ->
                                    ?o
                                  ?P ->
                                    ?p
                                  ?Q ->
                                    ?q
                                  ?R ->
                                    ?r
                                  ?S ->
                                    ?s
                                  ?T ->
                                    ?t
                                  ?U ->
                                    ?u
                                  ?V ->
                                    ?v
                                  ?W ->
                                    ?w
                                  ?X ->
                                    ?x
                                  ?Y ->
                                    ?y
                                  ?Z ->
                                    ?z
                                  _ ->
                                    g
                                end>>,
                            n + 1)
  end

  defp langtag_extension_sub(<<?-, a, b, c, d, e, f, r :: bits>>, acc, t, n)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((a === ?a or a === ?b) or a === ?c) or a === ?d) or a === ?e) or a === ?f) or a === ?g) or a === ?h) or a === ?i) or a === ?j) or a === ?k) or a === ?l) or a === ?m) or a === ?n) or a === ?o) or a === ?p) or a === ?q) or a === ?r) or a === ?s) or a === ?t) or a === ?u) or a === ?v) or a === ?w) or a === ?x) or a === ?y) or a === ?z) or a === ?A) or a === ?B) or a === ?C) or a === ?D) or a === ?E) or a === ?F) or a === ?G) or a === ?H) or a === ?I) or a === ?J) or a === ?K) or a === ?L) or a === ?M) or a === ?N) or a === ?O) or a === ?P) or a === ?Q) or a === ?R) or a === ?S) or a === ?T) or a === ?U) or a === ?V) or a === ?W) or a === ?X) or a === ?Y) or a === ?Z) or a === ?0) or a === ?1) or a === ?2) or a === ?3) or a === ?4) or a === ?5) or a === ?6) or a === ?7) or a === ?8) or a === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((b === ?a or b === ?b) or b === ?c) or b === ?d) or b === ?e) or b === ?f) or b === ?g) or b === ?h) or b === ?i) or b === ?j) or b === ?k) or b === ?l) or b === ?m) or b === ?n) or b === ?o) or b === ?p) or b === ?q) or b === ?r) or b === ?s) or b === ?t) or b === ?u) or b === ?v) or b === ?w) or b === ?x) or b === ?y) or b === ?z) or b === ?A) or b === ?B) or b === ?C) or b === ?D) or b === ?E) or b === ?F) or b === ?G) or b === ?H) or b === ?I) or b === ?J) or b === ?K) or b === ?L) or b === ?M) or b === ?N) or b === ?O) or b === ?P) or b === ?Q) or b === ?R) or b === ?S) or b === ?T) or b === ?U) or b === ?V) or b === ?W) or b === ?X) or b === ?Y) or b === ?Z) or b === ?0) or b === ?1) or b === ?2) or b === ?3) or b === ?4) or b === ?5) or b === ?6) or b === ?7) or b === ?8) or b === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((d === ?a or d === ?b) or d === ?c) or d === ?d) or d === ?e) or d === ?f) or d === ?g) or d === ?h) or d === ?i) or d === ?j) or d === ?k) or d === ?l) or d === ?m) or d === ?n) or d === ?o) or d === ?p) or d === ?q) or d === ?r) or d === ?s) or d === ?t) or d === ?u) or d === ?v) or d === ?w) or d === ?x) or d === ?y) or d === ?z) or d === ?A) or d === ?B) or d === ?C) or d === ?D) or d === ?E) or d === ?F) or d === ?G) or d === ?H) or d === ?I) or d === ?J) or d === ?K) or d === ?L) or d === ?M) or d === ?N) or d === ?O) or d === ?P) or d === ?Q) or d === ?R) or d === ?S) or d === ?T) or d === ?U) or d === ?V) or d === ?W) or d === ?X) or d === ?Y) or d === ?Z) or d === ?0) or d === ?1) or d === ?2) or d === ?3) or d === ?4) or d === ?5) or d === ?6) or d === ?7) or d === ?8) or d === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((e === ?a or e === ?b) or e === ?c) or e === ?d) or e === ?e) or e === ?f) or e === ?g) or e === ?h) or e === ?i) or e === ?j) or e === ?k) or e === ?l) or e === ?m) or e === ?n) or e === ?o) or e === ?p) or e === ?q) or e === ?r) or e === ?s) or e === ?t) or e === ?u) or e === ?v) or e === ?w) or e === ?x) or e === ?y) or e === ?z) or e === ?A) or e === ?B) or e === ?C) or e === ?D) or e === ?E) or e === ?F) or e === ?G) or e === ?H) or e === ?I) or e === ?J) or e === ?K) or e === ?L) or e === ?M) or e === ?N) or e === ?O) or e === ?P) or e === ?Q) or e === ?R) or e === ?S) or e === ?T) or e === ?U) or e === ?V) or e === ?W) or e === ?X) or e === ?Y) or e === ?Z) or e === ?0) or e === ?1) or e === ?2) or e === ?3) or e === ?4) or e === ?5) or e === ?6) or e === ?7) or e === ?8) or e === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((f === ?a or f === ?b) or f === ?c) or f === ?d) or f === ?e) or f === ?f) or f === ?g) or f === ?h) or f === ?i) or f === ?j) or f === ?k) or f === ?l) or f === ?m) or f === ?n) or f === ?o) or f === ?p) or f === ?q) or f === ?r) or f === ?s) or f === ?t) or f === ?u) or f === ?v) or f === ?w) or f === ?x) or f === ?y) or f === ?z) or f === ?A) or f === ?B) or f === ?C) or f === ?D) or f === ?E) or f === ?F) or f === ?G) or f === ?H) or f === ?I) or f === ?J) or f === ?K) or f === ?L) or f === ?M) or f === ?N) or f === ?O) or f === ?P) or f === ?Q) or f === ?R) or f === ?S) or f === ?T) or f === ?U) or f === ?V) or f === ?W) or f === ?X) or f === ?Y) or f === ?Z) or f === ?0) or f === ?1) or f === ?2) or f === ?3) or f === ?4) or f === ?5) or f === ?6) or f === ?7) or f === ?8) or f === ?9) do
    langtag_extension_sub(r, acc,
                            <<t :: binary, ?-,
                                case (a) do
                                  ?A ->
                                    ?a
                                  ?B ->
                                    ?b
                                  ?C ->
                                    ?c
                                  ?D ->
                                    ?d
                                  ?E ->
                                    ?e
                                  ?F ->
                                    ?f
                                  ?G ->
                                    ?g
                                  ?H ->
                                    ?h
                                  ?I ->
                                    ?i
                                  ?J ->
                                    ?j
                                  ?K ->
                                    ?k
                                  ?L ->
                                    ?l
                                  ?M ->
                                    ?m
                                  ?N ->
                                    ?n
                                  ?O ->
                                    ?o
                                  ?P ->
                                    ?p
                                  ?Q ->
                                    ?q
                                  ?R ->
                                    ?r
                                  ?S ->
                                    ?s
                                  ?T ->
                                    ?t
                                  ?U ->
                                    ?u
                                  ?V ->
                                    ?v
                                  ?W ->
                                    ?w
                                  ?X ->
                                    ?x
                                  ?Y ->
                                    ?y
                                  ?Z ->
                                    ?z
                                  _ ->
                                    a
                                end,
                                case (b) do
                                  ?A ->
                                    ?a
                                  ?B ->
                                    ?b
                                  ?C ->
                                    ?c
                                  ?D ->
                                    ?d
                                  ?E ->
                                    ?e
                                  ?F ->
                                    ?f
                                  ?G ->
                                    ?g
                                  ?H ->
                                    ?h
                                  ?I ->
                                    ?i
                                  ?J ->
                                    ?j
                                  ?K ->
                                    ?k
                                  ?L ->
                                    ?l
                                  ?M ->
                                    ?m
                                  ?N ->
                                    ?n
                                  ?O ->
                                    ?o
                                  ?P ->
                                    ?p
                                  ?Q ->
                                    ?q
                                  ?R ->
                                    ?r
                                  ?S ->
                                    ?s
                                  ?T ->
                                    ?t
                                  ?U ->
                                    ?u
                                  ?V ->
                                    ?v
                                  ?W ->
                                    ?w
                                  ?X ->
                                    ?x
                                  ?Y ->
                                    ?y
                                  ?Z ->
                                    ?z
                                  _ ->
                                    b
                                end,
                                case (c) do
                                  ?A ->
                                    ?a
                                  ?B ->
                                    ?b
                                  ?C ->
                                    ?c
                                  ?D ->
                                    ?d
                                  ?E ->
                                    ?e
                                  ?F ->
                                    ?f
                                  ?G ->
                                    ?g
                                  ?H ->
                                    ?h
                                  ?I ->
                                    ?i
                                  ?J ->
                                    ?j
                                  ?K ->
                                    ?k
                                  ?L ->
                                    ?l
                                  ?M ->
                                    ?m
                                  ?N ->
                                    ?n
                                  ?O ->
                                    ?o
                                  ?P ->
                                    ?p
                                  ?Q ->
                                    ?q
                                  ?R ->
                                    ?r
                                  ?S ->
                                    ?s
                                  ?T ->
                                    ?t
                                  ?U ->
                                    ?u
                                  ?V ->
                                    ?v
                                  ?W ->
                                    ?w
                                  ?X ->
                                    ?x
                                  ?Y ->
                                    ?y
                                  ?Z ->
                                    ?z
                                  _ ->
                                    c
                                end,
                                case (d) do
                                  ?A ->
                                    ?a
                                  ?B ->
                                    ?b
                                  ?C ->
                                    ?c
                                  ?D ->
                                    ?d
                                  ?E ->
                                    ?e
                                  ?F ->
                                    ?f
                                  ?G ->
                                    ?g
                                  ?H ->
                                    ?h
                                  ?I ->
                                    ?i
                                  ?J ->
                                    ?j
                                  ?K ->
                                    ?k
                                  ?L ->
                                    ?l
                                  ?M ->
                                    ?m
                                  ?N ->
                                    ?n
                                  ?O ->
                                    ?o
                                  ?P ->
                                    ?p
                                  ?Q ->
                                    ?q
                                  ?R ->
                                    ?r
                                  ?S ->
                                    ?s
                                  ?T ->
                                    ?t
                                  ?U ->
                                    ?u
                                  ?V ->
                                    ?v
                                  ?W ->
                                    ?w
                                  ?X ->
                                    ?x
                                  ?Y ->
                                    ?y
                                  ?Z ->
                                    ?z
                                  _ ->
                                    d
                                end,
                                case (e) do
                                  ?A ->
                                    ?a
                                  ?B ->
                                    ?b
                                  ?C ->
                                    ?c
                                  ?D ->
                                    ?d
                                  ?E ->
                                    ?e
                                  ?F ->
                                    ?f
                                  ?G ->
                                    ?g
                                  ?H ->
                                    ?h
                                  ?I ->
                                    ?i
                                  ?J ->
                                    ?j
                                  ?K ->
                                    ?k
                                  ?L ->
                                    ?l
                                  ?M ->
                                    ?m
                                  ?N ->
                                    ?n
                                  ?O ->
                                    ?o
                                  ?P ->
                                    ?p
                                  ?Q ->
                                    ?q
                                  ?R ->
                                    ?r
                                  ?S ->
                                    ?s
                                  ?T ->
                                    ?t
                                  ?U ->
                                    ?u
                                  ?V ->
                                    ?v
                                  ?W ->
                                    ?w
                                  ?X ->
                                    ?x
                                  ?Y ->
                                    ?y
                                  ?Z ->
                                    ?z
                                  _ ->
                                    e
                                end,
                                case (f) do
                                  ?A ->
                                    ?a
                                  ?B ->
                                    ?b
                                  ?C ->
                                    ?c
                                  ?D ->
                                    ?d
                                  ?E ->
                                    ?e
                                  ?F ->
                                    ?f
                                  ?G ->
                                    ?g
                                  ?H ->
                                    ?h
                                  ?I ->
                                    ?i
                                  ?J ->
                                    ?j
                                  ?K ->
                                    ?k
                                  ?L ->
                                    ?l
                                  ?M ->
                                    ?m
                                  ?N ->
                                    ?n
                                  ?O ->
                                    ?o
                                  ?P ->
                                    ?p
                                  ?Q ->
                                    ?q
                                  ?R ->
                                    ?r
                                  ?S ->
                                    ?s
                                  ?T ->
                                    ?t
                                  ?U ->
                                    ?u
                                  ?V ->
                                    ?v
                                  ?W ->
                                    ?w
                                  ?X ->
                                    ?x
                                  ?Y ->
                                    ?y
                                  ?Z ->
                                    ?z
                                  _ ->
                                    f
                                end>>,
                            n + 1)
  end

  defp langtag_extension_sub(<<?-, a, b, c, d, e, r :: bits>>, acc, t, n)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((a === ?a or a === ?b) or a === ?c) or a === ?d) or a === ?e) or a === ?f) or a === ?g) or a === ?h) or a === ?i) or a === ?j) or a === ?k) or a === ?l) or a === ?m) or a === ?n) or a === ?o) or a === ?p) or a === ?q) or a === ?r) or a === ?s) or a === ?t) or a === ?u) or a === ?v) or a === ?w) or a === ?x) or a === ?y) or a === ?z) or a === ?A) or a === ?B) or a === ?C) or a === ?D) or a === ?E) or a === ?F) or a === ?G) or a === ?H) or a === ?I) or a === ?J) or a === ?K) or a === ?L) or a === ?M) or a === ?N) or a === ?O) or a === ?P) or a === ?Q) or a === ?R) or a === ?S) or a === ?T) or a === ?U) or a === ?V) or a === ?W) or a === ?X) or a === ?Y) or a === ?Z) or a === ?0) or a === ?1) or a === ?2) or a === ?3) or a === ?4) or a === ?5) or a === ?6) or a === ?7) or a === ?8) or a === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((b === ?a or b === ?b) or b === ?c) or b === ?d) or b === ?e) or b === ?f) or b === ?g) or b === ?h) or b === ?i) or b === ?j) or b === ?k) or b === ?l) or b === ?m) or b === ?n) or b === ?o) or b === ?p) or b === ?q) or b === ?r) or b === ?s) or b === ?t) or b === ?u) or b === ?v) or b === ?w) or b === ?x) or b === ?y) or b === ?z) or b === ?A) or b === ?B) or b === ?C) or b === ?D) or b === ?E) or b === ?F) or b === ?G) or b === ?H) or b === ?I) or b === ?J) or b === ?K) or b === ?L) or b === ?M) or b === ?N) or b === ?O) or b === ?P) or b === ?Q) or b === ?R) or b === ?S) or b === ?T) or b === ?U) or b === ?V) or b === ?W) or b === ?X) or b === ?Y) or b === ?Z) or b === ?0) or b === ?1) or b === ?2) or b === ?3) or b === ?4) or b === ?5) or b === ?6) or b === ?7) or b === ?8) or b === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((d === ?a or d === ?b) or d === ?c) or d === ?d) or d === ?e) or d === ?f) or d === ?g) or d === ?h) or d === ?i) or d === ?j) or d === ?k) or d === ?l) or d === ?m) or d === ?n) or d === ?o) or d === ?p) or d === ?q) or d === ?r) or d === ?s) or d === ?t) or d === ?u) or d === ?v) or d === ?w) or d === ?x) or d === ?y) or d === ?z) or d === ?A) or d === ?B) or d === ?C) or d === ?D) or d === ?E) or d === ?F) or d === ?G) or d === ?H) or d === ?I) or d === ?J) or d === ?K) or d === ?L) or d === ?M) or d === ?N) or d === ?O) or d === ?P) or d === ?Q) or d === ?R) or d === ?S) or d === ?T) or d === ?U) or d === ?V) or d === ?W) or d === ?X) or d === ?Y) or d === ?Z) or d === ?0) or d === ?1) or d === ?2) or d === ?3) or d === ?4) or d === ?5) or d === ?6) or d === ?7) or d === ?8) or d === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((e === ?a or e === ?b) or e === ?c) or e === ?d) or e === ?e) or e === ?f) or e === ?g) or e === ?h) or e === ?i) or e === ?j) or e === ?k) or e === ?l) or e === ?m) or e === ?n) or e === ?o) or e === ?p) or e === ?q) or e === ?r) or e === ?s) or e === ?t) or e === ?u) or e === ?v) or e === ?w) or e === ?x) or e === ?y) or e === ?z) or e === ?A) or e === ?B) or e === ?C) or e === ?D) or e === ?E) or e === ?F) or e === ?G) or e === ?H) or e === ?I) or e === ?J) or e === ?K) or e === ?L) or e === ?M) or e === ?N) or e === ?O) or e === ?P) or e === ?Q) or e === ?R) or e === ?S) or e === ?T) or e === ?U) or e === ?V) or e === ?W) or e === ?X) or e === ?Y) or e === ?Z) or e === ?0) or e === ?1) or e === ?2) or e === ?3) or e === ?4) or e === ?5) or e === ?6) or e === ?7) or e === ?8) or e === ?9) do
    langtag_extension_sub(r, acc,
                            <<t :: binary, ?-,
                                case (a) do
                                  ?A ->
                                    ?a
                                  ?B ->
                                    ?b
                                  ?C ->
                                    ?c
                                  ?D ->
                                    ?d
                                  ?E ->
                                    ?e
                                  ?F ->
                                    ?f
                                  ?G ->
                                    ?g
                                  ?H ->
                                    ?h
                                  ?I ->
                                    ?i
                                  ?J ->
                                    ?j
                                  ?K ->
                                    ?k
                                  ?L ->
                                    ?l
                                  ?M ->
                                    ?m
                                  ?N ->
                                    ?n
                                  ?O ->
                                    ?o
                                  ?P ->
                                    ?p
                                  ?Q ->
                                    ?q
                                  ?R ->
                                    ?r
                                  ?S ->
                                    ?s
                                  ?T ->
                                    ?t
                                  ?U ->
                                    ?u
                                  ?V ->
                                    ?v
                                  ?W ->
                                    ?w
                                  ?X ->
                                    ?x
                                  ?Y ->
                                    ?y
                                  ?Z ->
                                    ?z
                                  _ ->
                                    a
                                end,
                                case (b) do
                                  ?A ->
                                    ?a
                                  ?B ->
                                    ?b
                                  ?C ->
                                    ?c
                                  ?D ->
                                    ?d
                                  ?E ->
                                    ?e
                                  ?F ->
                                    ?f
                                  ?G ->
                                    ?g
                                  ?H ->
                                    ?h
                                  ?I ->
                                    ?i
                                  ?J ->
                                    ?j
                                  ?K ->
                                    ?k
                                  ?L ->
                                    ?l
                                  ?M ->
                                    ?m
                                  ?N ->
                                    ?n
                                  ?O ->
                                    ?o
                                  ?P ->
                                    ?p
                                  ?Q ->
                                    ?q
                                  ?R ->
                                    ?r
                                  ?S ->
                                    ?s
                                  ?T ->
                                    ?t
                                  ?U ->
                                    ?u
                                  ?V ->
                                    ?v
                                  ?W ->
                                    ?w
                                  ?X ->
                                    ?x
                                  ?Y ->
                                    ?y
                                  ?Z ->
                                    ?z
                                  _ ->
                                    b
                                end,
                                case (c) do
                                  ?A ->
                                    ?a
                                  ?B ->
                                    ?b
                                  ?C ->
                                    ?c
                                  ?D ->
                                    ?d
                                  ?E ->
                                    ?e
                                  ?F ->
                                    ?f
                                  ?G ->
                                    ?g
                                  ?H ->
                                    ?h
                                  ?I ->
                                    ?i
                                  ?J ->
                                    ?j
                                  ?K ->
                                    ?k
                                  ?L ->
                                    ?l
                                  ?M ->
                                    ?m
                                  ?N ->
                                    ?n
                                  ?O ->
                                    ?o
                                  ?P ->
                                    ?p
                                  ?Q ->
                                    ?q
                                  ?R ->
                                    ?r
                                  ?S ->
                                    ?s
                                  ?T ->
                                    ?t
                                  ?U ->
                                    ?u
                                  ?V ->
                                    ?v
                                  ?W ->
                                    ?w
                                  ?X ->
                                    ?x
                                  ?Y ->
                                    ?y
                                  ?Z ->
                                    ?z
                                  _ ->
                                    c
                                end,
                                case (d) do
                                  ?A ->
                                    ?a
                                  ?B ->
                                    ?b
                                  ?C ->
                                    ?c
                                  ?D ->
                                    ?d
                                  ?E ->
                                    ?e
                                  ?F ->
                                    ?f
                                  ?G ->
                                    ?g
                                  ?H ->
                                    ?h
                                  ?I ->
                                    ?i
                                  ?J ->
                                    ?j
                                  ?K ->
                                    ?k
                                  ?L ->
                                    ?l
                                  ?M ->
                                    ?m
                                  ?N ->
                                    ?n
                                  ?O ->
                                    ?o
                                  ?P ->
                                    ?p
                                  ?Q ->
                                    ?q
                                  ?R ->
                                    ?r
                                  ?S ->
                                    ?s
                                  ?T ->
                                    ?t
                                  ?U ->
                                    ?u
                                  ?V ->
                                    ?v
                                  ?W ->
                                    ?w
                                  ?X ->
                                    ?x
                                  ?Y ->
                                    ?y
                                  ?Z ->
                                    ?z
                                  _ ->
                                    d
                                end,
                                case (e) do
                                  ?A ->
                                    ?a
                                  ?B ->
                                    ?b
                                  ?C ->
                                    ?c
                                  ?D ->
                                    ?d
                                  ?E ->
                                    ?e
                                  ?F ->
                                    ?f
                                  ?G ->
                                    ?g
                                  ?H ->
                                    ?h
                                  ?I ->
                                    ?i
                                  ?J ->
                                    ?j
                                  ?K ->
                                    ?k
                                  ?L ->
                                    ?l
                                  ?M ->
                                    ?m
                                  ?N ->
                                    ?n
                                  ?O ->
                                    ?o
                                  ?P ->
                                    ?p
                                  ?Q ->
                                    ?q
                                  ?R ->
                                    ?r
                                  ?S ->
                                    ?s
                                  ?T ->
                                    ?t
                                  ?U ->
                                    ?u
                                  ?V ->
                                    ?v
                                  ?W ->
                                    ?w
                                  ?X ->
                                    ?x
                                  ?Y ->
                                    ?y
                                  ?Z ->
                                    ?z
                                  _ ->
                                    e
                                end>>,
                            n + 1)
  end

  defp langtag_extension_sub(<<?-, a, b, c, d, r :: bits>>, acc, t, n)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((a === ?a or a === ?b) or a === ?c) or a === ?d) or a === ?e) or a === ?f) or a === ?g) or a === ?h) or a === ?i) or a === ?j) or a === ?k) or a === ?l) or a === ?m) or a === ?n) or a === ?o) or a === ?p) or a === ?q) or a === ?r) or a === ?s) or a === ?t) or a === ?u) or a === ?v) or a === ?w) or a === ?x) or a === ?y) or a === ?z) or a === ?A) or a === ?B) or a === ?C) or a === ?D) or a === ?E) or a === ?F) or a === ?G) or a === ?H) or a === ?I) or a === ?J) or a === ?K) or a === ?L) or a === ?M) or a === ?N) or a === ?O) or a === ?P) or a === ?Q) or a === ?R) or a === ?S) or a === ?T) or a === ?U) or a === ?V) or a === ?W) or a === ?X) or a === ?Y) or a === ?Z) or a === ?0) or a === ?1) or a === ?2) or a === ?3) or a === ?4) or a === ?5) or a === ?6) or a === ?7) or a === ?8) or a === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((b === ?a or b === ?b) or b === ?c) or b === ?d) or b === ?e) or b === ?f) or b === ?g) or b === ?h) or b === ?i) or b === ?j) or b === ?k) or b === ?l) or b === ?m) or b === ?n) or b === ?o) or b === ?p) or b === ?q) or b === ?r) or b === ?s) or b === ?t) or b === ?u) or b === ?v) or b === ?w) or b === ?x) or b === ?y) or b === ?z) or b === ?A) or b === ?B) or b === ?C) or b === ?D) or b === ?E) or b === ?F) or b === ?G) or b === ?H) or b === ?I) or b === ?J) or b === ?K) or b === ?L) or b === ?M) or b === ?N) or b === ?O) or b === ?P) or b === ?Q) or b === ?R) or b === ?S) or b === ?T) or b === ?U) or b === ?V) or b === ?W) or b === ?X) or b === ?Y) or b === ?Z) or b === ?0) or b === ?1) or b === ?2) or b === ?3) or b === ?4) or b === ?5) or b === ?6) or b === ?7) or b === ?8) or b === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((d === ?a or d === ?b) or d === ?c) or d === ?d) or d === ?e) or d === ?f) or d === ?g) or d === ?h) or d === ?i) or d === ?j) or d === ?k) or d === ?l) or d === ?m) or d === ?n) or d === ?o) or d === ?p) or d === ?q) or d === ?r) or d === ?s) or d === ?t) or d === ?u) or d === ?v) or d === ?w) or d === ?x) or d === ?y) or d === ?z) or d === ?A) or d === ?B) or d === ?C) or d === ?D) or d === ?E) or d === ?F) or d === ?G) or d === ?H) or d === ?I) or d === ?J) or d === ?K) or d === ?L) or d === ?M) or d === ?N) or d === ?O) or d === ?P) or d === ?Q) or d === ?R) or d === ?S) or d === ?T) or d === ?U) or d === ?V) or d === ?W) or d === ?X) or d === ?Y) or d === ?Z) or d === ?0) or d === ?1) or d === ?2) or d === ?3) or d === ?4) or d === ?5) or d === ?6) or d === ?7) or d === ?8) or d === ?9) do
    langtag_extension_sub(r, acc,
                            <<t :: binary, ?-,
                                case (a) do
                                  ?A ->
                                    ?a
                                  ?B ->
                                    ?b
                                  ?C ->
                                    ?c
                                  ?D ->
                                    ?d
                                  ?E ->
                                    ?e
                                  ?F ->
                                    ?f
                                  ?G ->
                                    ?g
                                  ?H ->
                                    ?h
                                  ?I ->
                                    ?i
                                  ?J ->
                                    ?j
                                  ?K ->
                                    ?k
                                  ?L ->
                                    ?l
                                  ?M ->
                                    ?m
                                  ?N ->
                                    ?n
                                  ?O ->
                                    ?o
                                  ?P ->
                                    ?p
                                  ?Q ->
                                    ?q
                                  ?R ->
                                    ?r
                                  ?S ->
                                    ?s
                                  ?T ->
                                    ?t
                                  ?U ->
                                    ?u
                                  ?V ->
                                    ?v
                                  ?W ->
                                    ?w
                                  ?X ->
                                    ?x
                                  ?Y ->
                                    ?y
                                  ?Z ->
                                    ?z
                                  _ ->
                                    a
                                end,
                                case (b) do
                                  ?A ->
                                    ?a
                                  ?B ->
                                    ?b
                                  ?C ->
                                    ?c
                                  ?D ->
                                    ?d
                                  ?E ->
                                    ?e
                                  ?F ->
                                    ?f
                                  ?G ->
                                    ?g
                                  ?H ->
                                    ?h
                                  ?I ->
                                    ?i
                                  ?J ->
                                    ?j
                                  ?K ->
                                    ?k
                                  ?L ->
                                    ?l
                                  ?M ->
                                    ?m
                                  ?N ->
                                    ?n
                                  ?O ->
                                    ?o
                                  ?P ->
                                    ?p
                                  ?Q ->
                                    ?q
                                  ?R ->
                                    ?r
                                  ?S ->
                                    ?s
                                  ?T ->
                                    ?t
                                  ?U ->
                                    ?u
                                  ?V ->
                                    ?v
                                  ?W ->
                                    ?w
                                  ?X ->
                                    ?x
                                  ?Y ->
                                    ?y
                                  ?Z ->
                                    ?z
                                  _ ->
                                    b
                                end,
                                case (c) do
                                  ?A ->
                                    ?a
                                  ?B ->
                                    ?b
                                  ?C ->
                                    ?c
                                  ?D ->
                                    ?d
                                  ?E ->
                                    ?e
                                  ?F ->
                                    ?f
                                  ?G ->
                                    ?g
                                  ?H ->
                                    ?h
                                  ?I ->
                                    ?i
                                  ?J ->
                                    ?j
                                  ?K ->
                                    ?k
                                  ?L ->
                                    ?l
                                  ?M ->
                                    ?m
                                  ?N ->
                                    ?n
                                  ?O ->
                                    ?o
                                  ?P ->
                                    ?p
                                  ?Q ->
                                    ?q
                                  ?R ->
                                    ?r
                                  ?S ->
                                    ?s
                                  ?T ->
                                    ?t
                                  ?U ->
                                    ?u
                                  ?V ->
                                    ?v
                                  ?W ->
                                    ?w
                                  ?X ->
                                    ?x
                                  ?Y ->
                                    ?y
                                  ?Z ->
                                    ?z
                                  _ ->
                                    c
                                end,
                                case (d) do
                                  ?A ->
                                    ?a
                                  ?B ->
                                    ?b
                                  ?C ->
                                    ?c
                                  ?D ->
                                    ?d
                                  ?E ->
                                    ?e
                                  ?F ->
                                    ?f
                                  ?G ->
                                    ?g
                                  ?H ->
                                    ?h
                                  ?I ->
                                    ?i
                                  ?J ->
                                    ?j
                                  ?K ->
                                    ?k
                                  ?L ->
                                    ?l
                                  ?M ->
                                    ?m
                                  ?N ->
                                    ?n
                                  ?O ->
                                    ?o
                                  ?P ->
                                    ?p
                                  ?Q ->
                                    ?q
                                  ?R ->
                                    ?r
                                  ?S ->
                                    ?s
                                  ?T ->
                                    ?t
                                  ?U ->
                                    ?u
                                  ?V ->
                                    ?v
                                  ?W ->
                                    ?w
                                  ?X ->
                                    ?x
                                  ?Y ->
                                    ?y
                                  ?Z ->
                                    ?z
                                  _ ->
                                    d
                                end>>,
                            n + 1)
  end

  defp langtag_extension_sub(<<?-, a, b, c, r :: bits>>, acc, t, n)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((a === ?a or a === ?b) or a === ?c) or a === ?d) or a === ?e) or a === ?f) or a === ?g) or a === ?h) or a === ?i) or a === ?j) or a === ?k) or a === ?l) or a === ?m) or a === ?n) or a === ?o) or a === ?p) or a === ?q) or a === ?r) or a === ?s) or a === ?t) or a === ?u) or a === ?v) or a === ?w) or a === ?x) or a === ?y) or a === ?z) or a === ?A) or a === ?B) or a === ?C) or a === ?D) or a === ?E) or a === ?F) or a === ?G) or a === ?H) or a === ?I) or a === ?J) or a === ?K) or a === ?L) or a === ?M) or a === ?N) or a === ?O) or a === ?P) or a === ?Q) or a === ?R) or a === ?S) or a === ?T) or a === ?U) or a === ?V) or a === ?W) or a === ?X) or a === ?Y) or a === ?Z) or a === ?0) or a === ?1) or a === ?2) or a === ?3) or a === ?4) or a === ?5) or a === ?6) or a === ?7) or a === ?8) or a === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((b === ?a or b === ?b) or b === ?c) or b === ?d) or b === ?e) or b === ?f) or b === ?g) or b === ?h) or b === ?i) or b === ?j) or b === ?k) or b === ?l) or b === ?m) or b === ?n) or b === ?o) or b === ?p) or b === ?q) or b === ?r) or b === ?s) or b === ?t) or b === ?u) or b === ?v) or b === ?w) or b === ?x) or b === ?y) or b === ?z) or b === ?A) or b === ?B) or b === ?C) or b === ?D) or b === ?E) or b === ?F) or b === ?G) or b === ?H) or b === ?I) or b === ?J) or b === ?K) or b === ?L) or b === ?M) or b === ?N) or b === ?O) or b === ?P) or b === ?Q) or b === ?R) or b === ?S) or b === ?T) or b === ?U) or b === ?V) or b === ?W) or b === ?X) or b === ?Y) or b === ?Z) or b === ?0) or b === ?1) or b === ?2) or b === ?3) or b === ?4) or b === ?5) or b === ?6) or b === ?7) or b === ?8) or b === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9) do
    langtag_extension_sub(r, acc,
                            <<t :: binary, ?-,
                                case (a) do
                                  ?A ->
                                    ?a
                                  ?B ->
                                    ?b
                                  ?C ->
                                    ?c
                                  ?D ->
                                    ?d
                                  ?E ->
                                    ?e
                                  ?F ->
                                    ?f
                                  ?G ->
                                    ?g
                                  ?H ->
                                    ?h
                                  ?I ->
                                    ?i
                                  ?J ->
                                    ?j
                                  ?K ->
                                    ?k
                                  ?L ->
                                    ?l
                                  ?M ->
                                    ?m
                                  ?N ->
                                    ?n
                                  ?O ->
                                    ?o
                                  ?P ->
                                    ?p
                                  ?Q ->
                                    ?q
                                  ?R ->
                                    ?r
                                  ?S ->
                                    ?s
                                  ?T ->
                                    ?t
                                  ?U ->
                                    ?u
                                  ?V ->
                                    ?v
                                  ?W ->
                                    ?w
                                  ?X ->
                                    ?x
                                  ?Y ->
                                    ?y
                                  ?Z ->
                                    ?z
                                  _ ->
                                    a
                                end,
                                case (b) do
                                  ?A ->
                                    ?a
                                  ?B ->
                                    ?b
                                  ?C ->
                                    ?c
                                  ?D ->
                                    ?d
                                  ?E ->
                                    ?e
                                  ?F ->
                                    ?f
                                  ?G ->
                                    ?g
                                  ?H ->
                                    ?h
                                  ?I ->
                                    ?i
                                  ?J ->
                                    ?j
                                  ?K ->
                                    ?k
                                  ?L ->
                                    ?l
                                  ?M ->
                                    ?m
                                  ?N ->
                                    ?n
                                  ?O ->
                                    ?o
                                  ?P ->
                                    ?p
                                  ?Q ->
                                    ?q
                                  ?R ->
                                    ?r
                                  ?S ->
                                    ?s
                                  ?T ->
                                    ?t
                                  ?U ->
                                    ?u
                                  ?V ->
                                    ?v
                                  ?W ->
                                    ?w
                                  ?X ->
                                    ?x
                                  ?Y ->
                                    ?y
                                  ?Z ->
                                    ?z
                                  _ ->
                                    b
                                end,
                                case (c) do
                                  ?A ->
                                    ?a
                                  ?B ->
                                    ?b
                                  ?C ->
                                    ?c
                                  ?D ->
                                    ?d
                                  ?E ->
                                    ?e
                                  ?F ->
                                    ?f
                                  ?G ->
                                    ?g
                                  ?H ->
                                    ?h
                                  ?I ->
                                    ?i
                                  ?J ->
                                    ?j
                                  ?K ->
                                    ?k
                                  ?L ->
                                    ?l
                                  ?M ->
                                    ?m
                                  ?N ->
                                    ?n
                                  ?O ->
                                    ?o
                                  ?P ->
                                    ?p
                                  ?Q ->
                                    ?q
                                  ?R ->
                                    ?r
                                  ?S ->
                                    ?s
                                  ?T ->
                                    ?t
                                  ?U ->
                                    ?u
                                  ?V ->
                                    ?v
                                  ?W ->
                                    ?w
                                  ?X ->
                                    ?x
                                  ?Y ->
                                    ?y
                                  ?Z ->
                                    ?z
                                  _ ->
                                    c
                                end>>,
                            n + 1)
  end

  defp langtag_extension_sub(<<?-, a, b, r :: bits>>, acc, t, n)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((a === ?a or a === ?b) or a === ?c) or a === ?d) or a === ?e) or a === ?f) or a === ?g) or a === ?h) or a === ?i) or a === ?j) or a === ?k) or a === ?l) or a === ?m) or a === ?n) or a === ?o) or a === ?p) or a === ?q) or a === ?r) or a === ?s) or a === ?t) or a === ?u) or a === ?v) or a === ?w) or a === ?x) or a === ?y) or a === ?z) or a === ?A) or a === ?B) or a === ?C) or a === ?D) or a === ?E) or a === ?F) or a === ?G) or a === ?H) or a === ?I) or a === ?J) or a === ?K) or a === ?L) or a === ?M) or a === ?N) or a === ?O) or a === ?P) or a === ?Q) or a === ?R) or a === ?S) or a === ?T) or a === ?U) or a === ?V) or a === ?W) or a === ?X) or a === ?Y) or a === ?Z) or a === ?0) or a === ?1) or a === ?2) or a === ?3) or a === ?4) or a === ?5) or a === ?6) or a === ?7) or a === ?8) or a === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((b === ?a or b === ?b) or b === ?c) or b === ?d) or b === ?e) or b === ?f) or b === ?g) or b === ?h) or b === ?i) or b === ?j) or b === ?k) or b === ?l) or b === ?m) or b === ?n) or b === ?o) or b === ?p) or b === ?q) or b === ?r) or b === ?s) or b === ?t) or b === ?u) or b === ?v) or b === ?w) or b === ?x) or b === ?y) or b === ?z) or b === ?A) or b === ?B) or b === ?C) or b === ?D) or b === ?E) or b === ?F) or b === ?G) or b === ?H) or b === ?I) or b === ?J) or b === ?K) or b === ?L) or b === ?M) or b === ?N) or b === ?O) or b === ?P) or b === ?Q) or b === ?R) or b === ?S) or b === ?T) or b === ?U) or b === ?V) or b === ?W) or b === ?X) or b === ?Y) or b === ?Z) or b === ?0) or b === ?1) or b === ?2) or b === ?3) or b === ?4) or b === ?5) or b === ?6) or b === ?7) or b === ?8) or b === ?9) do
    langtag_extension_sub(r, acc,
                            <<t :: binary, ?-,
                                case (a) do
                                  ?A ->
                                    ?a
                                  ?B ->
                                    ?b
                                  ?C ->
                                    ?c
                                  ?D ->
                                    ?d
                                  ?E ->
                                    ?e
                                  ?F ->
                                    ?f
                                  ?G ->
                                    ?g
                                  ?H ->
                                    ?h
                                  ?I ->
                                    ?i
                                  ?J ->
                                    ?j
                                  ?K ->
                                    ?k
                                  ?L ->
                                    ?l
                                  ?M ->
                                    ?m
                                  ?N ->
                                    ?n
                                  ?O ->
                                    ?o
                                  ?P ->
                                    ?p
                                  ?Q ->
                                    ?q
                                  ?R ->
                                    ?r
                                  ?S ->
                                    ?s
                                  ?T ->
                                    ?t
                                  ?U ->
                                    ?u
                                  ?V ->
                                    ?v
                                  ?W ->
                                    ?w
                                  ?X ->
                                    ?x
                                  ?Y ->
                                    ?y
                                  ?Z ->
                                    ?z
                                  _ ->
                                    a
                                end,
                                case (b) do
                                  ?A ->
                                    ?a
                                  ?B ->
                                    ?b
                                  ?C ->
                                    ?c
                                  ?D ->
                                    ?d
                                  ?E ->
                                    ?e
                                  ?F ->
                                    ?f
                                  ?G ->
                                    ?g
                                  ?H ->
                                    ?h
                                  ?I ->
                                    ?i
                                  ?J ->
                                    ?j
                                  ?K ->
                                    ?k
                                  ?L ->
                                    ?l
                                  ?M ->
                                    ?m
                                  ?N ->
                                    ?n
                                  ?O ->
                                    ?o
                                  ?P ->
                                    ?p
                                  ?Q ->
                                    ?q
                                  ?R ->
                                    ?r
                                  ?S ->
                                    ?s
                                  ?T ->
                                    ?t
                                  ?U ->
                                    ?u
                                  ?V ->
                                    ?v
                                  ?W ->
                                    ?w
                                  ?X ->
                                    ?x
                                  ?Y ->
                                    ?y
                                  ?Z ->
                                    ?z
                                  _ ->
                                    b
                                end>>,
                            n + 1)
  end

  defp langtag_extension_sub(r, acc, t, n) when n > 0 do
    langtag_extension(r, acc, t)
  end

  defp langtag_privateuse_sub(<<?-, a, b, c, d, e, f, g, h, r :: bits>>, acc,
            t, n)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((a === ?a or a === ?b) or a === ?c) or a === ?d) or a === ?e) or a === ?f) or a === ?g) or a === ?h) or a === ?i) or a === ?j) or a === ?k) or a === ?l) or a === ?m) or a === ?n) or a === ?o) or a === ?p) or a === ?q) or a === ?r) or a === ?s) or a === ?t) or a === ?u) or a === ?v) or a === ?w) or a === ?x) or a === ?y) or a === ?z) or a === ?A) or a === ?B) or a === ?C) or a === ?D) or a === ?E) or a === ?F) or a === ?G) or a === ?H) or a === ?I) or a === ?J) or a === ?K) or a === ?L) or a === ?M) or a === ?N) or a === ?O) or a === ?P) or a === ?Q) or a === ?R) or a === ?S) or a === ?T) or a === ?U) or a === ?V) or a === ?W) or a === ?X) or a === ?Y) or a === ?Z) or a === ?0) or a === ?1) or a === ?2) or a === ?3) or a === ?4) or a === ?5) or a === ?6) or a === ?7) or a === ?8) or a === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((b === ?a or b === ?b) or b === ?c) or b === ?d) or b === ?e) or b === ?f) or b === ?g) or b === ?h) or b === ?i) or b === ?j) or b === ?k) or b === ?l) or b === ?m) or b === ?n) or b === ?o) or b === ?p) or b === ?q) or b === ?r) or b === ?s) or b === ?t) or b === ?u) or b === ?v) or b === ?w) or b === ?x) or b === ?y) or b === ?z) or b === ?A) or b === ?B) or b === ?C) or b === ?D) or b === ?E) or b === ?F) or b === ?G) or b === ?H) or b === ?I) or b === ?J) or b === ?K) or b === ?L) or b === ?M) or b === ?N) or b === ?O) or b === ?P) or b === ?Q) or b === ?R) or b === ?S) or b === ?T) or b === ?U) or b === ?V) or b === ?W) or b === ?X) or b === ?Y) or b === ?Z) or b === ?0) or b === ?1) or b === ?2) or b === ?3) or b === ?4) or b === ?5) or b === ?6) or b === ?7) or b === ?8) or b === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((d === ?a or d === ?b) or d === ?c) or d === ?d) or d === ?e) or d === ?f) or d === ?g) or d === ?h) or d === ?i) or d === ?j) or d === ?k) or d === ?l) or d === ?m) or d === ?n) or d === ?o) or d === ?p) or d === ?q) or d === ?r) or d === ?s) or d === ?t) or d === ?u) or d === ?v) or d === ?w) or d === ?x) or d === ?y) or d === ?z) or d === ?A) or d === ?B) or d === ?C) or d === ?D) or d === ?E) or d === ?F) or d === ?G) or d === ?H) or d === ?I) or d === ?J) or d === ?K) or d === ?L) or d === ?M) or d === ?N) or d === ?O) or d === ?P) or d === ?Q) or d === ?R) or d === ?S) or d === ?T) or d === ?U) or d === ?V) or d === ?W) or d === ?X) or d === ?Y) or d === ?Z) or d === ?0) or d === ?1) or d === ?2) or d === ?3) or d === ?4) or d === ?5) or d === ?6) or d === ?7) or d === ?8) or d === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((e === ?a or e === ?b) or e === ?c) or e === ?d) or e === ?e) or e === ?f) or e === ?g) or e === ?h) or e === ?i) or e === ?j) or e === ?k) or e === ?l) or e === ?m) or e === ?n) or e === ?o) or e === ?p) or e === ?q) or e === ?r) or e === ?s) or e === ?t) or e === ?u) or e === ?v) or e === ?w) or e === ?x) or e === ?y) or e === ?z) or e === ?A) or e === ?B) or e === ?C) or e === ?D) or e === ?E) or e === ?F) or e === ?G) or e === ?H) or e === ?I) or e === ?J) or e === ?K) or e === ?L) or e === ?M) or e === ?N) or e === ?O) or e === ?P) or e === ?Q) or e === ?R) or e === ?S) or e === ?T) or e === ?U) or e === ?V) or e === ?W) or e === ?X) or e === ?Y) or e === ?Z) or e === ?0) or e === ?1) or e === ?2) or e === ?3) or e === ?4) or e === ?5) or e === ?6) or e === ?7) or e === ?8) or e === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((f === ?a or f === ?b) or f === ?c) or f === ?d) or f === ?e) or f === ?f) or f === ?g) or f === ?h) or f === ?i) or f === ?j) or f === ?k) or f === ?l) or f === ?m) or f === ?n) or f === ?o) or f === ?p) or f === ?q) or f === ?r) or f === ?s) or f === ?t) or f === ?u) or f === ?v) or f === ?w) or f === ?x) or f === ?y) or f === ?z) or f === ?A) or f === ?B) or f === ?C) or f === ?D) or f === ?E) or f === ?F) or f === ?G) or f === ?H) or f === ?I) or f === ?J) or f === ?K) or f === ?L) or f === ?M) or f === ?N) or f === ?O) or f === ?P) or f === ?Q) or f === ?R) or f === ?S) or f === ?T) or f === ?U) or f === ?V) or f === ?W) or f === ?X) or f === ?Y) or f === ?Z) or f === ?0) or f === ?1) or f === ?2) or f === ?3) or f === ?4) or f === ?5) or f === ?6) or f === ?7) or f === ?8) or f === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((g === ?a or g === ?b) or g === ?c) or g === ?d) or g === ?e) or g === ?f) or g === ?g) or g === ?h) or g === ?i) or g === ?j) or g === ?k) or g === ?l) or g === ?m) or g === ?n) or g === ?o) or g === ?p) or g === ?q) or g === ?r) or g === ?s) or g === ?t) or g === ?u) or g === ?v) or g === ?w) or g === ?x) or g === ?y) or g === ?z) or g === ?A) or g === ?B) or g === ?C) or g === ?D) or g === ?E) or g === ?F) or g === ?G) or g === ?H) or g === ?I) or g === ?J) or g === ?K) or g === ?L) or g === ?M) or g === ?N) or g === ?O) or g === ?P) or g === ?Q) or g === ?R) or g === ?S) or g === ?T) or g === ?U) or g === ?V) or g === ?W) or g === ?X) or g === ?Y) or g === ?Z) or g === ?0) or g === ?1) or g === ?2) or g === ?3) or g === ?4) or g === ?5) or g === ?6) or g === ?7) or g === ?8) or g === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((h === ?a or h === ?b) or h === ?c) or h === ?d) or h === ?e) or h === ?f) or h === ?g) or h === ?h) or h === ?i) or h === ?j) or h === ?k) or h === ?l) or h === ?m) or h === ?n) or h === ?o) or h === ?p) or h === ?q) or h === ?r) or h === ?s) or h === ?t) or h === ?u) or h === ?v) or h === ?w) or h === ?x) or h === ?y) or h === ?z) or h === ?A) or h === ?B) or h === ?C) or h === ?D) or h === ?E) or h === ?F) or h === ?G) or h === ?H) or h === ?I) or h === ?J) or h === ?K) or h === ?L) or h === ?M) or h === ?N) or h === ?O) or h === ?P) or h === ?Q) or h === ?R) or h === ?S) or h === ?T) or h === ?U) or h === ?V) or h === ?W) or h === ?X) or h === ?Y) or h === ?Z) or h === ?0) or h === ?1) or h === ?2) or h === ?3) or h === ?4) or h === ?5) or h === ?6) or h === ?7) or h === ?8) or h === ?9) do
    langtag_privateuse_sub(r, acc,
                             <<t :: binary, ?-,
                                 case (a) do
                                   ?A ->
                                     ?a
                                   ?B ->
                                     ?b
                                   ?C ->
                                     ?c
                                   ?D ->
                                     ?d
                                   ?E ->
                                     ?e
                                   ?F ->
                                     ?f
                                   ?G ->
                                     ?g
                                   ?H ->
                                     ?h
                                   ?I ->
                                     ?i
                                   ?J ->
                                     ?j
                                   ?K ->
                                     ?k
                                   ?L ->
                                     ?l
                                   ?M ->
                                     ?m
                                   ?N ->
                                     ?n
                                   ?O ->
                                     ?o
                                   ?P ->
                                     ?p
                                   ?Q ->
                                     ?q
                                   ?R ->
                                     ?r
                                   ?S ->
                                     ?s
                                   ?T ->
                                     ?t
                                   ?U ->
                                     ?u
                                   ?V ->
                                     ?v
                                   ?W ->
                                     ?w
                                   ?X ->
                                     ?x
                                   ?Y ->
                                     ?y
                                   ?Z ->
                                     ?z
                                   _ ->
                                     a
                                 end,
                                 case (b) do
                                   ?A ->
                                     ?a
                                   ?B ->
                                     ?b
                                   ?C ->
                                     ?c
                                   ?D ->
                                     ?d
                                   ?E ->
                                     ?e
                                   ?F ->
                                     ?f
                                   ?G ->
                                     ?g
                                   ?H ->
                                     ?h
                                   ?I ->
                                     ?i
                                   ?J ->
                                     ?j
                                   ?K ->
                                     ?k
                                   ?L ->
                                     ?l
                                   ?M ->
                                     ?m
                                   ?N ->
                                     ?n
                                   ?O ->
                                     ?o
                                   ?P ->
                                     ?p
                                   ?Q ->
                                     ?q
                                   ?R ->
                                     ?r
                                   ?S ->
                                     ?s
                                   ?T ->
                                     ?t
                                   ?U ->
                                     ?u
                                   ?V ->
                                     ?v
                                   ?W ->
                                     ?w
                                   ?X ->
                                     ?x
                                   ?Y ->
                                     ?y
                                   ?Z ->
                                     ?z
                                   _ ->
                                     b
                                 end,
                                 case (c) do
                                   ?A ->
                                     ?a
                                   ?B ->
                                     ?b
                                   ?C ->
                                     ?c
                                   ?D ->
                                     ?d
                                   ?E ->
                                     ?e
                                   ?F ->
                                     ?f
                                   ?G ->
                                     ?g
                                   ?H ->
                                     ?h
                                   ?I ->
                                     ?i
                                   ?J ->
                                     ?j
                                   ?K ->
                                     ?k
                                   ?L ->
                                     ?l
                                   ?M ->
                                     ?m
                                   ?N ->
                                     ?n
                                   ?O ->
                                     ?o
                                   ?P ->
                                     ?p
                                   ?Q ->
                                     ?q
                                   ?R ->
                                     ?r
                                   ?S ->
                                     ?s
                                   ?T ->
                                     ?t
                                   ?U ->
                                     ?u
                                   ?V ->
                                     ?v
                                   ?W ->
                                     ?w
                                   ?X ->
                                     ?x
                                   ?Y ->
                                     ?y
                                   ?Z ->
                                     ?z
                                   _ ->
                                     c
                                 end,
                                 case (d) do
                                   ?A ->
                                     ?a
                                   ?B ->
                                     ?b
                                   ?C ->
                                     ?c
                                   ?D ->
                                     ?d
                                   ?E ->
                                     ?e
                                   ?F ->
                                     ?f
                                   ?G ->
                                     ?g
                                   ?H ->
                                     ?h
                                   ?I ->
                                     ?i
                                   ?J ->
                                     ?j
                                   ?K ->
                                     ?k
                                   ?L ->
                                     ?l
                                   ?M ->
                                     ?m
                                   ?N ->
                                     ?n
                                   ?O ->
                                     ?o
                                   ?P ->
                                     ?p
                                   ?Q ->
                                     ?q
                                   ?R ->
                                     ?r
                                   ?S ->
                                     ?s
                                   ?T ->
                                     ?t
                                   ?U ->
                                     ?u
                                   ?V ->
                                     ?v
                                   ?W ->
                                     ?w
                                   ?X ->
                                     ?x
                                   ?Y ->
                                     ?y
                                   ?Z ->
                                     ?z
                                   _ ->
                                     d
                                 end,
                                 case (e) do
                                   ?A ->
                                     ?a
                                   ?B ->
                                     ?b
                                   ?C ->
                                     ?c
                                   ?D ->
                                     ?d
                                   ?E ->
                                     ?e
                                   ?F ->
                                     ?f
                                   ?G ->
                                     ?g
                                   ?H ->
                                     ?h
                                   ?I ->
                                     ?i
                                   ?J ->
                                     ?j
                                   ?K ->
                                     ?k
                                   ?L ->
                                     ?l
                                   ?M ->
                                     ?m
                                   ?N ->
                                     ?n
                                   ?O ->
                                     ?o
                                   ?P ->
                                     ?p
                                   ?Q ->
                                     ?q
                                   ?R ->
                                     ?r
                                   ?S ->
                                     ?s
                                   ?T ->
                                     ?t
                                   ?U ->
                                     ?u
                                   ?V ->
                                     ?v
                                   ?W ->
                                     ?w
                                   ?X ->
                                     ?x
                                   ?Y ->
                                     ?y
                                   ?Z ->
                                     ?z
                                   _ ->
                                     e
                                 end,
                                 case (f) do
                                   ?A ->
                                     ?a
                                   ?B ->
                                     ?b
                                   ?C ->
                                     ?c
                                   ?D ->
                                     ?d
                                   ?E ->
                                     ?e
                                   ?F ->
                                     ?f
                                   ?G ->
                                     ?g
                                   ?H ->
                                     ?h
                                   ?I ->
                                     ?i
                                   ?J ->
                                     ?j
                                   ?K ->
                                     ?k
                                   ?L ->
                                     ?l
                                   ?M ->
                                     ?m
                                   ?N ->
                                     ?n
                                   ?O ->
                                     ?o
                                   ?P ->
                                     ?p
                                   ?Q ->
                                     ?q
                                   ?R ->
                                     ?r
                                   ?S ->
                                     ?s
                                   ?T ->
                                     ?t
                                   ?U ->
                                     ?u
                                   ?V ->
                                     ?v
                                   ?W ->
                                     ?w
                                   ?X ->
                                     ?x
                                   ?Y ->
                                     ?y
                                   ?Z ->
                                     ?z
                                   _ ->
                                     f
                                 end,
                                 case (g) do
                                   ?A ->
                                     ?a
                                   ?B ->
                                     ?b
                                   ?C ->
                                     ?c
                                   ?D ->
                                     ?d
                                   ?E ->
                                     ?e
                                   ?F ->
                                     ?f
                                   ?G ->
                                     ?g
                                   ?H ->
                                     ?h
                                   ?I ->
                                     ?i
                                   ?J ->
                                     ?j
                                   ?K ->
                                     ?k
                                   ?L ->
                                     ?l
                                   ?M ->
                                     ?m
                                   ?N ->
                                     ?n
                                   ?O ->
                                     ?o
                                   ?P ->
                                     ?p
                                   ?Q ->
                                     ?q
                                   ?R ->
                                     ?r
                                   ?S ->
                                     ?s
                                   ?T ->
                                     ?t
                                   ?U ->
                                     ?u
                                   ?V ->
                                     ?v
                                   ?W ->
                                     ?w
                                   ?X ->
                                     ?x
                                   ?Y ->
                                     ?y
                                   ?Z ->
                                     ?z
                                   _ ->
                                     g
                                 end,
                                 case (h) do
                                   ?A ->
                                     ?a
                                   ?B ->
                                     ?b
                                   ?C ->
                                     ?c
                                   ?D ->
                                     ?d
                                   ?E ->
                                     ?e
                                   ?F ->
                                     ?f
                                   ?G ->
                                     ?g
                                   ?H ->
                                     ?h
                                   ?I ->
                                     ?i
                                   ?J ->
                                     ?j
                                   ?K ->
                                     ?k
                                   ?L ->
                                     ?l
                                   ?M ->
                                     ?m
                                   ?N ->
                                     ?n
                                   ?O ->
                                     ?o
                                   ?P ->
                                     ?p
                                   ?Q ->
                                     ?q
                                   ?R ->
                                     ?r
                                   ?S ->
                                     ?s
                                   ?T ->
                                     ?t
                                   ?U ->
                                     ?u
                                   ?V ->
                                     ?v
                                   ?W ->
                                     ?w
                                   ?X ->
                                     ?x
                                   ?Y ->
                                     ?y
                                   ?Z ->
                                     ?z
                                   _ ->
                                     h
                                 end>>,
                             n + 1)
  end

  defp langtag_privateuse_sub(<<?-, a, b, c, d, e, f, g, r :: bits>>, acc, t,
            n)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((a === ?a or a === ?b) or a === ?c) or a === ?d) or a === ?e) or a === ?f) or a === ?g) or a === ?h) or a === ?i) or a === ?j) or a === ?k) or a === ?l) or a === ?m) or a === ?n) or a === ?o) or a === ?p) or a === ?q) or a === ?r) or a === ?s) or a === ?t) or a === ?u) or a === ?v) or a === ?w) or a === ?x) or a === ?y) or a === ?z) or a === ?A) or a === ?B) or a === ?C) or a === ?D) or a === ?E) or a === ?F) or a === ?G) or a === ?H) or a === ?I) or a === ?J) or a === ?K) or a === ?L) or a === ?M) or a === ?N) or a === ?O) or a === ?P) or a === ?Q) or a === ?R) or a === ?S) or a === ?T) or a === ?U) or a === ?V) or a === ?W) or a === ?X) or a === ?Y) or a === ?Z) or a === ?0) or a === ?1) or a === ?2) or a === ?3) or a === ?4) or a === ?5) or a === ?6) or a === ?7) or a === ?8) or a === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((b === ?a or b === ?b) or b === ?c) or b === ?d) or b === ?e) or b === ?f) or b === ?g) or b === ?h) or b === ?i) or b === ?j) or b === ?k) or b === ?l) or b === ?m) or b === ?n) or b === ?o) or b === ?p) or b === ?q) or b === ?r) or b === ?s) or b === ?t) or b === ?u) or b === ?v) or b === ?w) or b === ?x) or b === ?y) or b === ?z) or b === ?A) or b === ?B) or b === ?C) or b === ?D) or b === ?E) or b === ?F) or b === ?G) or b === ?H) or b === ?I) or b === ?J) or b === ?K) or b === ?L) or b === ?M) or b === ?N) or b === ?O) or b === ?P) or b === ?Q) or b === ?R) or b === ?S) or b === ?T) or b === ?U) or b === ?V) or b === ?W) or b === ?X) or b === ?Y) or b === ?Z) or b === ?0) or b === ?1) or b === ?2) or b === ?3) or b === ?4) or b === ?5) or b === ?6) or b === ?7) or b === ?8) or b === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((d === ?a or d === ?b) or d === ?c) or d === ?d) or d === ?e) or d === ?f) or d === ?g) or d === ?h) or d === ?i) or d === ?j) or d === ?k) or d === ?l) or d === ?m) or d === ?n) or d === ?o) or d === ?p) or d === ?q) or d === ?r) or d === ?s) or d === ?t) or d === ?u) or d === ?v) or d === ?w) or d === ?x) or d === ?y) or d === ?z) or d === ?A) or d === ?B) or d === ?C) or d === ?D) or d === ?E) or d === ?F) or d === ?G) or d === ?H) or d === ?I) or d === ?J) or d === ?K) or d === ?L) or d === ?M) or d === ?N) or d === ?O) or d === ?P) or d === ?Q) or d === ?R) or d === ?S) or d === ?T) or d === ?U) or d === ?V) or d === ?W) or d === ?X) or d === ?Y) or d === ?Z) or d === ?0) or d === ?1) or d === ?2) or d === ?3) or d === ?4) or d === ?5) or d === ?6) or d === ?7) or d === ?8) or d === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((e === ?a or e === ?b) or e === ?c) or e === ?d) or e === ?e) or e === ?f) or e === ?g) or e === ?h) or e === ?i) or e === ?j) or e === ?k) or e === ?l) or e === ?m) or e === ?n) or e === ?o) or e === ?p) or e === ?q) or e === ?r) or e === ?s) or e === ?t) or e === ?u) or e === ?v) or e === ?w) or e === ?x) or e === ?y) or e === ?z) or e === ?A) or e === ?B) or e === ?C) or e === ?D) or e === ?E) or e === ?F) or e === ?G) or e === ?H) or e === ?I) or e === ?J) or e === ?K) or e === ?L) or e === ?M) or e === ?N) or e === ?O) or e === ?P) or e === ?Q) or e === ?R) or e === ?S) or e === ?T) or e === ?U) or e === ?V) or e === ?W) or e === ?X) or e === ?Y) or e === ?Z) or e === ?0) or e === ?1) or e === ?2) or e === ?3) or e === ?4) or e === ?5) or e === ?6) or e === ?7) or e === ?8) or e === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((f === ?a or f === ?b) or f === ?c) or f === ?d) or f === ?e) or f === ?f) or f === ?g) or f === ?h) or f === ?i) or f === ?j) or f === ?k) or f === ?l) or f === ?m) or f === ?n) or f === ?o) or f === ?p) or f === ?q) or f === ?r) or f === ?s) or f === ?t) or f === ?u) or f === ?v) or f === ?w) or f === ?x) or f === ?y) or f === ?z) or f === ?A) or f === ?B) or f === ?C) or f === ?D) or f === ?E) or f === ?F) or f === ?G) or f === ?H) or f === ?I) or f === ?J) or f === ?K) or f === ?L) or f === ?M) or f === ?N) or f === ?O) or f === ?P) or f === ?Q) or f === ?R) or f === ?S) or f === ?T) or f === ?U) or f === ?V) or f === ?W) or f === ?X) or f === ?Y) or f === ?Z) or f === ?0) or f === ?1) or f === ?2) or f === ?3) or f === ?4) or f === ?5) or f === ?6) or f === ?7) or f === ?8) or f === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((g === ?a or g === ?b) or g === ?c) or g === ?d) or g === ?e) or g === ?f) or g === ?g) or g === ?h) or g === ?i) or g === ?j) or g === ?k) or g === ?l) or g === ?m) or g === ?n) or g === ?o) or g === ?p) or g === ?q) or g === ?r) or g === ?s) or g === ?t) or g === ?u) or g === ?v) or g === ?w) or g === ?x) or g === ?y) or g === ?z) or g === ?A) or g === ?B) or g === ?C) or g === ?D) or g === ?E) or g === ?F) or g === ?G) or g === ?H) or g === ?I) or g === ?J) or g === ?K) or g === ?L) or g === ?M) or g === ?N) or g === ?O) or g === ?P) or g === ?Q) or g === ?R) or g === ?S) or g === ?T) or g === ?U) or g === ?V) or g === ?W) or g === ?X) or g === ?Y) or g === ?Z) or g === ?0) or g === ?1) or g === ?2) or g === ?3) or g === ?4) or g === ?5) or g === ?6) or g === ?7) or g === ?8) or g === ?9) do
    langtag_privateuse_sub(r, acc,
                             <<t :: binary, ?-,
                                 case (a) do
                                   ?A ->
                                     ?a
                                   ?B ->
                                     ?b
                                   ?C ->
                                     ?c
                                   ?D ->
                                     ?d
                                   ?E ->
                                     ?e
                                   ?F ->
                                     ?f
                                   ?G ->
                                     ?g
                                   ?H ->
                                     ?h
                                   ?I ->
                                     ?i
                                   ?J ->
                                     ?j
                                   ?K ->
                                     ?k
                                   ?L ->
                                     ?l
                                   ?M ->
                                     ?m
                                   ?N ->
                                     ?n
                                   ?O ->
                                     ?o
                                   ?P ->
                                     ?p
                                   ?Q ->
                                     ?q
                                   ?R ->
                                     ?r
                                   ?S ->
                                     ?s
                                   ?T ->
                                     ?t
                                   ?U ->
                                     ?u
                                   ?V ->
                                     ?v
                                   ?W ->
                                     ?w
                                   ?X ->
                                     ?x
                                   ?Y ->
                                     ?y
                                   ?Z ->
                                     ?z
                                   _ ->
                                     a
                                 end,
                                 case (b) do
                                   ?A ->
                                     ?a
                                   ?B ->
                                     ?b
                                   ?C ->
                                     ?c
                                   ?D ->
                                     ?d
                                   ?E ->
                                     ?e
                                   ?F ->
                                     ?f
                                   ?G ->
                                     ?g
                                   ?H ->
                                     ?h
                                   ?I ->
                                     ?i
                                   ?J ->
                                     ?j
                                   ?K ->
                                     ?k
                                   ?L ->
                                     ?l
                                   ?M ->
                                     ?m
                                   ?N ->
                                     ?n
                                   ?O ->
                                     ?o
                                   ?P ->
                                     ?p
                                   ?Q ->
                                     ?q
                                   ?R ->
                                     ?r
                                   ?S ->
                                     ?s
                                   ?T ->
                                     ?t
                                   ?U ->
                                     ?u
                                   ?V ->
                                     ?v
                                   ?W ->
                                     ?w
                                   ?X ->
                                     ?x
                                   ?Y ->
                                     ?y
                                   ?Z ->
                                     ?z
                                   _ ->
                                     b
                                 end,
                                 case (c) do
                                   ?A ->
                                     ?a
                                   ?B ->
                                     ?b
                                   ?C ->
                                     ?c
                                   ?D ->
                                     ?d
                                   ?E ->
                                     ?e
                                   ?F ->
                                     ?f
                                   ?G ->
                                     ?g
                                   ?H ->
                                     ?h
                                   ?I ->
                                     ?i
                                   ?J ->
                                     ?j
                                   ?K ->
                                     ?k
                                   ?L ->
                                     ?l
                                   ?M ->
                                     ?m
                                   ?N ->
                                     ?n
                                   ?O ->
                                     ?o
                                   ?P ->
                                     ?p
                                   ?Q ->
                                     ?q
                                   ?R ->
                                     ?r
                                   ?S ->
                                     ?s
                                   ?T ->
                                     ?t
                                   ?U ->
                                     ?u
                                   ?V ->
                                     ?v
                                   ?W ->
                                     ?w
                                   ?X ->
                                     ?x
                                   ?Y ->
                                     ?y
                                   ?Z ->
                                     ?z
                                   _ ->
                                     c
                                 end,
                                 case (d) do
                                   ?A ->
                                     ?a
                                   ?B ->
                                     ?b
                                   ?C ->
                                     ?c
                                   ?D ->
                                     ?d
                                   ?E ->
                                     ?e
                                   ?F ->
                                     ?f
                                   ?G ->
                                     ?g
                                   ?H ->
                                     ?h
                                   ?I ->
                                     ?i
                                   ?J ->
                                     ?j
                                   ?K ->
                                     ?k
                                   ?L ->
                                     ?l
                                   ?M ->
                                     ?m
                                   ?N ->
                                     ?n
                                   ?O ->
                                     ?o
                                   ?P ->
                                     ?p
                                   ?Q ->
                                     ?q
                                   ?R ->
                                     ?r
                                   ?S ->
                                     ?s
                                   ?T ->
                                     ?t
                                   ?U ->
                                     ?u
                                   ?V ->
                                     ?v
                                   ?W ->
                                     ?w
                                   ?X ->
                                     ?x
                                   ?Y ->
                                     ?y
                                   ?Z ->
                                     ?z
                                   _ ->
                                     d
                                 end,
                                 case (e) do
                                   ?A ->
                                     ?a
                                   ?B ->
                                     ?b
                                   ?C ->
                                     ?c
                                   ?D ->
                                     ?d
                                   ?E ->
                                     ?e
                                   ?F ->
                                     ?f
                                   ?G ->
                                     ?g
                                   ?H ->
                                     ?h
                                   ?I ->
                                     ?i
                                   ?J ->
                                     ?j
                                   ?K ->
                                     ?k
                                   ?L ->
                                     ?l
                                   ?M ->
                                     ?m
                                   ?N ->
                                     ?n
                                   ?O ->
                                     ?o
                                   ?P ->
                                     ?p
                                   ?Q ->
                                     ?q
                                   ?R ->
                                     ?r
                                   ?S ->
                                     ?s
                                   ?T ->
                                     ?t
                                   ?U ->
                                     ?u
                                   ?V ->
                                     ?v
                                   ?W ->
                                     ?w
                                   ?X ->
                                     ?x
                                   ?Y ->
                                     ?y
                                   ?Z ->
                                     ?z
                                   _ ->
                                     e
                                 end,
                                 case (f) do
                                   ?A ->
                                     ?a
                                   ?B ->
                                     ?b
                                   ?C ->
                                     ?c
                                   ?D ->
                                     ?d
                                   ?E ->
                                     ?e
                                   ?F ->
                                     ?f
                                   ?G ->
                                     ?g
                                   ?H ->
                                     ?h
                                   ?I ->
                                     ?i
                                   ?J ->
                                     ?j
                                   ?K ->
                                     ?k
                                   ?L ->
                                     ?l
                                   ?M ->
                                     ?m
                                   ?N ->
                                     ?n
                                   ?O ->
                                     ?o
                                   ?P ->
                                     ?p
                                   ?Q ->
                                     ?q
                                   ?R ->
                                     ?r
                                   ?S ->
                                     ?s
                                   ?T ->
                                     ?t
                                   ?U ->
                                     ?u
                                   ?V ->
                                     ?v
                                   ?W ->
                                     ?w
                                   ?X ->
                                     ?x
                                   ?Y ->
                                     ?y
                                   ?Z ->
                                     ?z
                                   _ ->
                                     f
                                 end,
                                 case (g) do
                                   ?A ->
                                     ?a
                                   ?B ->
                                     ?b
                                   ?C ->
                                     ?c
                                   ?D ->
                                     ?d
                                   ?E ->
                                     ?e
                                   ?F ->
                                     ?f
                                   ?G ->
                                     ?g
                                   ?H ->
                                     ?h
                                   ?I ->
                                     ?i
                                   ?J ->
                                     ?j
                                   ?K ->
                                     ?k
                                   ?L ->
                                     ?l
                                   ?M ->
                                     ?m
                                   ?N ->
                                     ?n
                                   ?O ->
                                     ?o
                                   ?P ->
                                     ?p
                                   ?Q ->
                                     ?q
                                   ?R ->
                                     ?r
                                   ?S ->
                                     ?s
                                   ?T ->
                                     ?t
                                   ?U ->
                                     ?u
                                   ?V ->
                                     ?v
                                   ?W ->
                                     ?w
                                   ?X ->
                                     ?x
                                   ?Y ->
                                     ?y
                                   ?Z ->
                                     ?z
                                   _ ->
                                     g
                                 end>>,
                             n + 1)
  end

  defp langtag_privateuse_sub(<<?-, a, b, c, d, e, f, r :: bits>>, acc, t, n)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((a === ?a or a === ?b) or a === ?c) or a === ?d) or a === ?e) or a === ?f) or a === ?g) or a === ?h) or a === ?i) or a === ?j) or a === ?k) or a === ?l) or a === ?m) or a === ?n) or a === ?o) or a === ?p) or a === ?q) or a === ?r) or a === ?s) or a === ?t) or a === ?u) or a === ?v) or a === ?w) or a === ?x) or a === ?y) or a === ?z) or a === ?A) or a === ?B) or a === ?C) or a === ?D) or a === ?E) or a === ?F) or a === ?G) or a === ?H) or a === ?I) or a === ?J) or a === ?K) or a === ?L) or a === ?M) or a === ?N) or a === ?O) or a === ?P) or a === ?Q) or a === ?R) or a === ?S) or a === ?T) or a === ?U) or a === ?V) or a === ?W) or a === ?X) or a === ?Y) or a === ?Z) or a === ?0) or a === ?1) or a === ?2) or a === ?3) or a === ?4) or a === ?5) or a === ?6) or a === ?7) or a === ?8) or a === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((b === ?a or b === ?b) or b === ?c) or b === ?d) or b === ?e) or b === ?f) or b === ?g) or b === ?h) or b === ?i) or b === ?j) or b === ?k) or b === ?l) or b === ?m) or b === ?n) or b === ?o) or b === ?p) or b === ?q) or b === ?r) or b === ?s) or b === ?t) or b === ?u) or b === ?v) or b === ?w) or b === ?x) or b === ?y) or b === ?z) or b === ?A) or b === ?B) or b === ?C) or b === ?D) or b === ?E) or b === ?F) or b === ?G) or b === ?H) or b === ?I) or b === ?J) or b === ?K) or b === ?L) or b === ?M) or b === ?N) or b === ?O) or b === ?P) or b === ?Q) or b === ?R) or b === ?S) or b === ?T) or b === ?U) or b === ?V) or b === ?W) or b === ?X) or b === ?Y) or b === ?Z) or b === ?0) or b === ?1) or b === ?2) or b === ?3) or b === ?4) or b === ?5) or b === ?6) or b === ?7) or b === ?8) or b === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((d === ?a or d === ?b) or d === ?c) or d === ?d) or d === ?e) or d === ?f) or d === ?g) or d === ?h) or d === ?i) or d === ?j) or d === ?k) or d === ?l) or d === ?m) or d === ?n) or d === ?o) or d === ?p) or d === ?q) or d === ?r) or d === ?s) or d === ?t) or d === ?u) or d === ?v) or d === ?w) or d === ?x) or d === ?y) or d === ?z) or d === ?A) or d === ?B) or d === ?C) or d === ?D) or d === ?E) or d === ?F) or d === ?G) or d === ?H) or d === ?I) or d === ?J) or d === ?K) or d === ?L) or d === ?M) or d === ?N) or d === ?O) or d === ?P) or d === ?Q) or d === ?R) or d === ?S) or d === ?T) or d === ?U) or d === ?V) or d === ?W) or d === ?X) or d === ?Y) or d === ?Z) or d === ?0) or d === ?1) or d === ?2) or d === ?3) or d === ?4) or d === ?5) or d === ?6) or d === ?7) or d === ?8) or d === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((e === ?a or e === ?b) or e === ?c) or e === ?d) or e === ?e) or e === ?f) or e === ?g) or e === ?h) or e === ?i) or e === ?j) or e === ?k) or e === ?l) or e === ?m) or e === ?n) or e === ?o) or e === ?p) or e === ?q) or e === ?r) or e === ?s) or e === ?t) or e === ?u) or e === ?v) or e === ?w) or e === ?x) or e === ?y) or e === ?z) or e === ?A) or e === ?B) or e === ?C) or e === ?D) or e === ?E) or e === ?F) or e === ?G) or e === ?H) or e === ?I) or e === ?J) or e === ?K) or e === ?L) or e === ?M) or e === ?N) or e === ?O) or e === ?P) or e === ?Q) or e === ?R) or e === ?S) or e === ?T) or e === ?U) or e === ?V) or e === ?W) or e === ?X) or e === ?Y) or e === ?Z) or e === ?0) or e === ?1) or e === ?2) or e === ?3) or e === ?4) or e === ?5) or e === ?6) or e === ?7) or e === ?8) or e === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((f === ?a or f === ?b) or f === ?c) or f === ?d) or f === ?e) or f === ?f) or f === ?g) or f === ?h) or f === ?i) or f === ?j) or f === ?k) or f === ?l) or f === ?m) or f === ?n) or f === ?o) or f === ?p) or f === ?q) or f === ?r) or f === ?s) or f === ?t) or f === ?u) or f === ?v) or f === ?w) or f === ?x) or f === ?y) or f === ?z) or f === ?A) or f === ?B) or f === ?C) or f === ?D) or f === ?E) or f === ?F) or f === ?G) or f === ?H) or f === ?I) or f === ?J) or f === ?K) or f === ?L) or f === ?M) or f === ?N) or f === ?O) or f === ?P) or f === ?Q) or f === ?R) or f === ?S) or f === ?T) or f === ?U) or f === ?V) or f === ?W) or f === ?X) or f === ?Y) or f === ?Z) or f === ?0) or f === ?1) or f === ?2) or f === ?3) or f === ?4) or f === ?5) or f === ?6) or f === ?7) or f === ?8) or f === ?9) do
    langtag_privateuse_sub(r, acc,
                             <<t :: binary, ?-,
                                 case (a) do
                                   ?A ->
                                     ?a
                                   ?B ->
                                     ?b
                                   ?C ->
                                     ?c
                                   ?D ->
                                     ?d
                                   ?E ->
                                     ?e
                                   ?F ->
                                     ?f
                                   ?G ->
                                     ?g
                                   ?H ->
                                     ?h
                                   ?I ->
                                     ?i
                                   ?J ->
                                     ?j
                                   ?K ->
                                     ?k
                                   ?L ->
                                     ?l
                                   ?M ->
                                     ?m
                                   ?N ->
                                     ?n
                                   ?O ->
                                     ?o
                                   ?P ->
                                     ?p
                                   ?Q ->
                                     ?q
                                   ?R ->
                                     ?r
                                   ?S ->
                                     ?s
                                   ?T ->
                                     ?t
                                   ?U ->
                                     ?u
                                   ?V ->
                                     ?v
                                   ?W ->
                                     ?w
                                   ?X ->
                                     ?x
                                   ?Y ->
                                     ?y
                                   ?Z ->
                                     ?z
                                   _ ->
                                     a
                                 end,
                                 case (b) do
                                   ?A ->
                                     ?a
                                   ?B ->
                                     ?b
                                   ?C ->
                                     ?c
                                   ?D ->
                                     ?d
                                   ?E ->
                                     ?e
                                   ?F ->
                                     ?f
                                   ?G ->
                                     ?g
                                   ?H ->
                                     ?h
                                   ?I ->
                                     ?i
                                   ?J ->
                                     ?j
                                   ?K ->
                                     ?k
                                   ?L ->
                                     ?l
                                   ?M ->
                                     ?m
                                   ?N ->
                                     ?n
                                   ?O ->
                                     ?o
                                   ?P ->
                                     ?p
                                   ?Q ->
                                     ?q
                                   ?R ->
                                     ?r
                                   ?S ->
                                     ?s
                                   ?T ->
                                     ?t
                                   ?U ->
                                     ?u
                                   ?V ->
                                     ?v
                                   ?W ->
                                     ?w
                                   ?X ->
                                     ?x
                                   ?Y ->
                                     ?y
                                   ?Z ->
                                     ?z
                                   _ ->
                                     b
                                 end,
                                 case (c) do
                                   ?A ->
                                     ?a
                                   ?B ->
                                     ?b
                                   ?C ->
                                     ?c
                                   ?D ->
                                     ?d
                                   ?E ->
                                     ?e
                                   ?F ->
                                     ?f
                                   ?G ->
                                     ?g
                                   ?H ->
                                     ?h
                                   ?I ->
                                     ?i
                                   ?J ->
                                     ?j
                                   ?K ->
                                     ?k
                                   ?L ->
                                     ?l
                                   ?M ->
                                     ?m
                                   ?N ->
                                     ?n
                                   ?O ->
                                     ?o
                                   ?P ->
                                     ?p
                                   ?Q ->
                                     ?q
                                   ?R ->
                                     ?r
                                   ?S ->
                                     ?s
                                   ?T ->
                                     ?t
                                   ?U ->
                                     ?u
                                   ?V ->
                                     ?v
                                   ?W ->
                                     ?w
                                   ?X ->
                                     ?x
                                   ?Y ->
                                     ?y
                                   ?Z ->
                                     ?z
                                   _ ->
                                     c
                                 end,
                                 case (d) do
                                   ?A ->
                                     ?a
                                   ?B ->
                                     ?b
                                   ?C ->
                                     ?c
                                   ?D ->
                                     ?d
                                   ?E ->
                                     ?e
                                   ?F ->
                                     ?f
                                   ?G ->
                                     ?g
                                   ?H ->
                                     ?h
                                   ?I ->
                                     ?i
                                   ?J ->
                                     ?j
                                   ?K ->
                                     ?k
                                   ?L ->
                                     ?l
                                   ?M ->
                                     ?m
                                   ?N ->
                                     ?n
                                   ?O ->
                                     ?o
                                   ?P ->
                                     ?p
                                   ?Q ->
                                     ?q
                                   ?R ->
                                     ?r
                                   ?S ->
                                     ?s
                                   ?T ->
                                     ?t
                                   ?U ->
                                     ?u
                                   ?V ->
                                     ?v
                                   ?W ->
                                     ?w
                                   ?X ->
                                     ?x
                                   ?Y ->
                                     ?y
                                   ?Z ->
                                     ?z
                                   _ ->
                                     d
                                 end,
                                 case (e) do
                                   ?A ->
                                     ?a
                                   ?B ->
                                     ?b
                                   ?C ->
                                     ?c
                                   ?D ->
                                     ?d
                                   ?E ->
                                     ?e
                                   ?F ->
                                     ?f
                                   ?G ->
                                     ?g
                                   ?H ->
                                     ?h
                                   ?I ->
                                     ?i
                                   ?J ->
                                     ?j
                                   ?K ->
                                     ?k
                                   ?L ->
                                     ?l
                                   ?M ->
                                     ?m
                                   ?N ->
                                     ?n
                                   ?O ->
                                     ?o
                                   ?P ->
                                     ?p
                                   ?Q ->
                                     ?q
                                   ?R ->
                                     ?r
                                   ?S ->
                                     ?s
                                   ?T ->
                                     ?t
                                   ?U ->
                                     ?u
                                   ?V ->
                                     ?v
                                   ?W ->
                                     ?w
                                   ?X ->
                                     ?x
                                   ?Y ->
                                     ?y
                                   ?Z ->
                                     ?z
                                   _ ->
                                     e
                                 end,
                                 case (f) do
                                   ?A ->
                                     ?a
                                   ?B ->
                                     ?b
                                   ?C ->
                                     ?c
                                   ?D ->
                                     ?d
                                   ?E ->
                                     ?e
                                   ?F ->
                                     ?f
                                   ?G ->
                                     ?g
                                   ?H ->
                                     ?h
                                   ?I ->
                                     ?i
                                   ?J ->
                                     ?j
                                   ?K ->
                                     ?k
                                   ?L ->
                                     ?l
                                   ?M ->
                                     ?m
                                   ?N ->
                                     ?n
                                   ?O ->
                                     ?o
                                   ?P ->
                                     ?p
                                   ?Q ->
                                     ?q
                                   ?R ->
                                     ?r
                                   ?S ->
                                     ?s
                                   ?T ->
                                     ?t
                                   ?U ->
                                     ?u
                                   ?V ->
                                     ?v
                                   ?W ->
                                     ?w
                                   ?X ->
                                     ?x
                                   ?Y ->
                                     ?y
                                   ?Z ->
                                     ?z
                                   _ ->
                                     f
                                 end>>,
                             n + 1)
  end

  defp langtag_privateuse_sub(<<?-, a, b, c, d, e, r :: bits>>, acc, t, n)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((a === ?a or a === ?b) or a === ?c) or a === ?d) or a === ?e) or a === ?f) or a === ?g) or a === ?h) or a === ?i) or a === ?j) or a === ?k) or a === ?l) or a === ?m) or a === ?n) or a === ?o) or a === ?p) or a === ?q) or a === ?r) or a === ?s) or a === ?t) or a === ?u) or a === ?v) or a === ?w) or a === ?x) or a === ?y) or a === ?z) or a === ?A) or a === ?B) or a === ?C) or a === ?D) or a === ?E) or a === ?F) or a === ?G) or a === ?H) or a === ?I) or a === ?J) or a === ?K) or a === ?L) or a === ?M) or a === ?N) or a === ?O) or a === ?P) or a === ?Q) or a === ?R) or a === ?S) or a === ?T) or a === ?U) or a === ?V) or a === ?W) or a === ?X) or a === ?Y) or a === ?Z) or a === ?0) or a === ?1) or a === ?2) or a === ?3) or a === ?4) or a === ?5) or a === ?6) or a === ?7) or a === ?8) or a === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((b === ?a or b === ?b) or b === ?c) or b === ?d) or b === ?e) or b === ?f) or b === ?g) or b === ?h) or b === ?i) or b === ?j) or b === ?k) or b === ?l) or b === ?m) or b === ?n) or b === ?o) or b === ?p) or b === ?q) or b === ?r) or b === ?s) or b === ?t) or b === ?u) or b === ?v) or b === ?w) or b === ?x) or b === ?y) or b === ?z) or b === ?A) or b === ?B) or b === ?C) or b === ?D) or b === ?E) or b === ?F) or b === ?G) or b === ?H) or b === ?I) or b === ?J) or b === ?K) or b === ?L) or b === ?M) or b === ?N) or b === ?O) or b === ?P) or b === ?Q) or b === ?R) or b === ?S) or b === ?T) or b === ?U) or b === ?V) or b === ?W) or b === ?X) or b === ?Y) or b === ?Z) or b === ?0) or b === ?1) or b === ?2) or b === ?3) or b === ?4) or b === ?5) or b === ?6) or b === ?7) or b === ?8) or b === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((d === ?a or d === ?b) or d === ?c) or d === ?d) or d === ?e) or d === ?f) or d === ?g) or d === ?h) or d === ?i) or d === ?j) or d === ?k) or d === ?l) or d === ?m) or d === ?n) or d === ?o) or d === ?p) or d === ?q) or d === ?r) or d === ?s) or d === ?t) or d === ?u) or d === ?v) or d === ?w) or d === ?x) or d === ?y) or d === ?z) or d === ?A) or d === ?B) or d === ?C) or d === ?D) or d === ?E) or d === ?F) or d === ?G) or d === ?H) or d === ?I) or d === ?J) or d === ?K) or d === ?L) or d === ?M) or d === ?N) or d === ?O) or d === ?P) or d === ?Q) or d === ?R) or d === ?S) or d === ?T) or d === ?U) or d === ?V) or d === ?W) or d === ?X) or d === ?Y) or d === ?Z) or d === ?0) or d === ?1) or d === ?2) or d === ?3) or d === ?4) or d === ?5) or d === ?6) or d === ?7) or d === ?8) or d === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((e === ?a or e === ?b) or e === ?c) or e === ?d) or e === ?e) or e === ?f) or e === ?g) or e === ?h) or e === ?i) or e === ?j) or e === ?k) or e === ?l) or e === ?m) or e === ?n) or e === ?o) or e === ?p) or e === ?q) or e === ?r) or e === ?s) or e === ?t) or e === ?u) or e === ?v) or e === ?w) or e === ?x) or e === ?y) or e === ?z) or e === ?A) or e === ?B) or e === ?C) or e === ?D) or e === ?E) or e === ?F) or e === ?G) or e === ?H) or e === ?I) or e === ?J) or e === ?K) or e === ?L) or e === ?M) or e === ?N) or e === ?O) or e === ?P) or e === ?Q) or e === ?R) or e === ?S) or e === ?T) or e === ?U) or e === ?V) or e === ?W) or e === ?X) or e === ?Y) or e === ?Z) or e === ?0) or e === ?1) or e === ?2) or e === ?3) or e === ?4) or e === ?5) or e === ?6) or e === ?7) or e === ?8) or e === ?9) do
    langtag_privateuse_sub(r, acc,
                             <<t :: binary, ?-,
                                 case (a) do
                                   ?A ->
                                     ?a
                                   ?B ->
                                     ?b
                                   ?C ->
                                     ?c
                                   ?D ->
                                     ?d
                                   ?E ->
                                     ?e
                                   ?F ->
                                     ?f
                                   ?G ->
                                     ?g
                                   ?H ->
                                     ?h
                                   ?I ->
                                     ?i
                                   ?J ->
                                     ?j
                                   ?K ->
                                     ?k
                                   ?L ->
                                     ?l
                                   ?M ->
                                     ?m
                                   ?N ->
                                     ?n
                                   ?O ->
                                     ?o
                                   ?P ->
                                     ?p
                                   ?Q ->
                                     ?q
                                   ?R ->
                                     ?r
                                   ?S ->
                                     ?s
                                   ?T ->
                                     ?t
                                   ?U ->
                                     ?u
                                   ?V ->
                                     ?v
                                   ?W ->
                                     ?w
                                   ?X ->
                                     ?x
                                   ?Y ->
                                     ?y
                                   ?Z ->
                                     ?z
                                   _ ->
                                     a
                                 end,
                                 case (b) do
                                   ?A ->
                                     ?a
                                   ?B ->
                                     ?b
                                   ?C ->
                                     ?c
                                   ?D ->
                                     ?d
                                   ?E ->
                                     ?e
                                   ?F ->
                                     ?f
                                   ?G ->
                                     ?g
                                   ?H ->
                                     ?h
                                   ?I ->
                                     ?i
                                   ?J ->
                                     ?j
                                   ?K ->
                                     ?k
                                   ?L ->
                                     ?l
                                   ?M ->
                                     ?m
                                   ?N ->
                                     ?n
                                   ?O ->
                                     ?o
                                   ?P ->
                                     ?p
                                   ?Q ->
                                     ?q
                                   ?R ->
                                     ?r
                                   ?S ->
                                     ?s
                                   ?T ->
                                     ?t
                                   ?U ->
                                     ?u
                                   ?V ->
                                     ?v
                                   ?W ->
                                     ?w
                                   ?X ->
                                     ?x
                                   ?Y ->
                                     ?y
                                   ?Z ->
                                     ?z
                                   _ ->
                                     b
                                 end,
                                 case (c) do
                                   ?A ->
                                     ?a
                                   ?B ->
                                     ?b
                                   ?C ->
                                     ?c
                                   ?D ->
                                     ?d
                                   ?E ->
                                     ?e
                                   ?F ->
                                     ?f
                                   ?G ->
                                     ?g
                                   ?H ->
                                     ?h
                                   ?I ->
                                     ?i
                                   ?J ->
                                     ?j
                                   ?K ->
                                     ?k
                                   ?L ->
                                     ?l
                                   ?M ->
                                     ?m
                                   ?N ->
                                     ?n
                                   ?O ->
                                     ?o
                                   ?P ->
                                     ?p
                                   ?Q ->
                                     ?q
                                   ?R ->
                                     ?r
                                   ?S ->
                                     ?s
                                   ?T ->
                                     ?t
                                   ?U ->
                                     ?u
                                   ?V ->
                                     ?v
                                   ?W ->
                                     ?w
                                   ?X ->
                                     ?x
                                   ?Y ->
                                     ?y
                                   ?Z ->
                                     ?z
                                   _ ->
                                     c
                                 end,
                                 case (d) do
                                   ?A ->
                                     ?a
                                   ?B ->
                                     ?b
                                   ?C ->
                                     ?c
                                   ?D ->
                                     ?d
                                   ?E ->
                                     ?e
                                   ?F ->
                                     ?f
                                   ?G ->
                                     ?g
                                   ?H ->
                                     ?h
                                   ?I ->
                                     ?i
                                   ?J ->
                                     ?j
                                   ?K ->
                                     ?k
                                   ?L ->
                                     ?l
                                   ?M ->
                                     ?m
                                   ?N ->
                                     ?n
                                   ?O ->
                                     ?o
                                   ?P ->
                                     ?p
                                   ?Q ->
                                     ?q
                                   ?R ->
                                     ?r
                                   ?S ->
                                     ?s
                                   ?T ->
                                     ?t
                                   ?U ->
                                     ?u
                                   ?V ->
                                     ?v
                                   ?W ->
                                     ?w
                                   ?X ->
                                     ?x
                                   ?Y ->
                                     ?y
                                   ?Z ->
                                     ?z
                                   _ ->
                                     d
                                 end,
                                 case (e) do
                                   ?A ->
                                     ?a
                                   ?B ->
                                     ?b
                                   ?C ->
                                     ?c
                                   ?D ->
                                     ?d
                                   ?E ->
                                     ?e
                                   ?F ->
                                     ?f
                                   ?G ->
                                     ?g
                                   ?H ->
                                     ?h
                                   ?I ->
                                     ?i
                                   ?J ->
                                     ?j
                                   ?K ->
                                     ?k
                                   ?L ->
                                     ?l
                                   ?M ->
                                     ?m
                                   ?N ->
                                     ?n
                                   ?O ->
                                     ?o
                                   ?P ->
                                     ?p
                                   ?Q ->
                                     ?q
                                   ?R ->
                                     ?r
                                   ?S ->
                                     ?s
                                   ?T ->
                                     ?t
                                   ?U ->
                                     ?u
                                   ?V ->
                                     ?v
                                   ?W ->
                                     ?w
                                   ?X ->
                                     ?x
                                   ?Y ->
                                     ?y
                                   ?Z ->
                                     ?z
                                   _ ->
                                     e
                                 end>>,
                             n + 1)
  end

  defp langtag_privateuse_sub(<<?-, a, b, c, d, r :: bits>>, acc, t, n)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((a === ?a or a === ?b) or a === ?c) or a === ?d) or a === ?e) or a === ?f) or a === ?g) or a === ?h) or a === ?i) or a === ?j) or a === ?k) or a === ?l) or a === ?m) or a === ?n) or a === ?o) or a === ?p) or a === ?q) or a === ?r) or a === ?s) or a === ?t) or a === ?u) or a === ?v) or a === ?w) or a === ?x) or a === ?y) or a === ?z) or a === ?A) or a === ?B) or a === ?C) or a === ?D) or a === ?E) or a === ?F) or a === ?G) or a === ?H) or a === ?I) or a === ?J) or a === ?K) or a === ?L) or a === ?M) or a === ?N) or a === ?O) or a === ?P) or a === ?Q) or a === ?R) or a === ?S) or a === ?T) or a === ?U) or a === ?V) or a === ?W) or a === ?X) or a === ?Y) or a === ?Z) or a === ?0) or a === ?1) or a === ?2) or a === ?3) or a === ?4) or a === ?5) or a === ?6) or a === ?7) or a === ?8) or a === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((b === ?a or b === ?b) or b === ?c) or b === ?d) or b === ?e) or b === ?f) or b === ?g) or b === ?h) or b === ?i) or b === ?j) or b === ?k) or b === ?l) or b === ?m) or b === ?n) or b === ?o) or b === ?p) or b === ?q) or b === ?r) or b === ?s) or b === ?t) or b === ?u) or b === ?v) or b === ?w) or b === ?x) or b === ?y) or b === ?z) or b === ?A) or b === ?B) or b === ?C) or b === ?D) or b === ?E) or b === ?F) or b === ?G) or b === ?H) or b === ?I) or b === ?J) or b === ?K) or b === ?L) or b === ?M) or b === ?N) or b === ?O) or b === ?P) or b === ?Q) or b === ?R) or b === ?S) or b === ?T) or b === ?U) or b === ?V) or b === ?W) or b === ?X) or b === ?Y) or b === ?Z) or b === ?0) or b === ?1) or b === ?2) or b === ?3) or b === ?4) or b === ?5) or b === ?6) or b === ?7) or b === ?8) or b === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((d === ?a or d === ?b) or d === ?c) or d === ?d) or d === ?e) or d === ?f) or d === ?g) or d === ?h) or d === ?i) or d === ?j) or d === ?k) or d === ?l) or d === ?m) or d === ?n) or d === ?o) or d === ?p) or d === ?q) or d === ?r) or d === ?s) or d === ?t) or d === ?u) or d === ?v) or d === ?w) or d === ?x) or d === ?y) or d === ?z) or d === ?A) or d === ?B) or d === ?C) or d === ?D) or d === ?E) or d === ?F) or d === ?G) or d === ?H) or d === ?I) or d === ?J) or d === ?K) or d === ?L) or d === ?M) or d === ?N) or d === ?O) or d === ?P) or d === ?Q) or d === ?R) or d === ?S) or d === ?T) or d === ?U) or d === ?V) or d === ?W) or d === ?X) or d === ?Y) or d === ?Z) or d === ?0) or d === ?1) or d === ?2) or d === ?3) or d === ?4) or d === ?5) or d === ?6) or d === ?7) or d === ?8) or d === ?9) do
    langtag_privateuse_sub(r, acc,
                             <<t :: binary, ?-,
                                 case (a) do
                                   ?A ->
                                     ?a
                                   ?B ->
                                     ?b
                                   ?C ->
                                     ?c
                                   ?D ->
                                     ?d
                                   ?E ->
                                     ?e
                                   ?F ->
                                     ?f
                                   ?G ->
                                     ?g
                                   ?H ->
                                     ?h
                                   ?I ->
                                     ?i
                                   ?J ->
                                     ?j
                                   ?K ->
                                     ?k
                                   ?L ->
                                     ?l
                                   ?M ->
                                     ?m
                                   ?N ->
                                     ?n
                                   ?O ->
                                     ?o
                                   ?P ->
                                     ?p
                                   ?Q ->
                                     ?q
                                   ?R ->
                                     ?r
                                   ?S ->
                                     ?s
                                   ?T ->
                                     ?t
                                   ?U ->
                                     ?u
                                   ?V ->
                                     ?v
                                   ?W ->
                                     ?w
                                   ?X ->
                                     ?x
                                   ?Y ->
                                     ?y
                                   ?Z ->
                                     ?z
                                   _ ->
                                     a
                                 end,
                                 case (b) do
                                   ?A ->
                                     ?a
                                   ?B ->
                                     ?b
                                   ?C ->
                                     ?c
                                   ?D ->
                                     ?d
                                   ?E ->
                                     ?e
                                   ?F ->
                                     ?f
                                   ?G ->
                                     ?g
                                   ?H ->
                                     ?h
                                   ?I ->
                                     ?i
                                   ?J ->
                                     ?j
                                   ?K ->
                                     ?k
                                   ?L ->
                                     ?l
                                   ?M ->
                                     ?m
                                   ?N ->
                                     ?n
                                   ?O ->
                                     ?o
                                   ?P ->
                                     ?p
                                   ?Q ->
                                     ?q
                                   ?R ->
                                     ?r
                                   ?S ->
                                     ?s
                                   ?T ->
                                     ?t
                                   ?U ->
                                     ?u
                                   ?V ->
                                     ?v
                                   ?W ->
                                     ?w
                                   ?X ->
                                     ?x
                                   ?Y ->
                                     ?y
                                   ?Z ->
                                     ?z
                                   _ ->
                                     b
                                 end,
                                 case (c) do
                                   ?A ->
                                     ?a
                                   ?B ->
                                     ?b
                                   ?C ->
                                     ?c
                                   ?D ->
                                     ?d
                                   ?E ->
                                     ?e
                                   ?F ->
                                     ?f
                                   ?G ->
                                     ?g
                                   ?H ->
                                     ?h
                                   ?I ->
                                     ?i
                                   ?J ->
                                     ?j
                                   ?K ->
                                     ?k
                                   ?L ->
                                     ?l
                                   ?M ->
                                     ?m
                                   ?N ->
                                     ?n
                                   ?O ->
                                     ?o
                                   ?P ->
                                     ?p
                                   ?Q ->
                                     ?q
                                   ?R ->
                                     ?r
                                   ?S ->
                                     ?s
                                   ?T ->
                                     ?t
                                   ?U ->
                                     ?u
                                   ?V ->
                                     ?v
                                   ?W ->
                                     ?w
                                   ?X ->
                                     ?x
                                   ?Y ->
                                     ?y
                                   ?Z ->
                                     ?z
                                   _ ->
                                     c
                                 end,
                                 case (d) do
                                   ?A ->
                                     ?a
                                   ?B ->
                                     ?b
                                   ?C ->
                                     ?c
                                   ?D ->
                                     ?d
                                   ?E ->
                                     ?e
                                   ?F ->
                                     ?f
                                   ?G ->
                                     ?g
                                   ?H ->
                                     ?h
                                   ?I ->
                                     ?i
                                   ?J ->
                                     ?j
                                   ?K ->
                                     ?k
                                   ?L ->
                                     ?l
                                   ?M ->
                                     ?m
                                   ?N ->
                                     ?n
                                   ?O ->
                                     ?o
                                   ?P ->
                                     ?p
                                   ?Q ->
                                     ?q
                                   ?R ->
                                     ?r
                                   ?S ->
                                     ?s
                                   ?T ->
                                     ?t
                                   ?U ->
                                     ?u
                                   ?V ->
                                     ?v
                                   ?W ->
                                     ?w
                                   ?X ->
                                     ?x
                                   ?Y ->
                                     ?y
                                   ?Z ->
                                     ?z
                                   _ ->
                                     d
                                 end>>,
                             n + 1)
  end

  defp langtag_privateuse_sub(<<?-, a, b, c, r :: bits>>, acc, t, n)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((a === ?a or a === ?b) or a === ?c) or a === ?d) or a === ?e) or a === ?f) or a === ?g) or a === ?h) or a === ?i) or a === ?j) or a === ?k) or a === ?l) or a === ?m) or a === ?n) or a === ?o) or a === ?p) or a === ?q) or a === ?r) or a === ?s) or a === ?t) or a === ?u) or a === ?v) or a === ?w) or a === ?x) or a === ?y) or a === ?z) or a === ?A) or a === ?B) or a === ?C) or a === ?D) or a === ?E) or a === ?F) or a === ?G) or a === ?H) or a === ?I) or a === ?J) or a === ?K) or a === ?L) or a === ?M) or a === ?N) or a === ?O) or a === ?P) or a === ?Q) or a === ?R) or a === ?S) or a === ?T) or a === ?U) or a === ?V) or a === ?W) or a === ?X) or a === ?Y) or a === ?Z) or a === ?0) or a === ?1) or a === ?2) or a === ?3) or a === ?4) or a === ?5) or a === ?6) or a === ?7) or a === ?8) or a === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((b === ?a or b === ?b) or b === ?c) or b === ?d) or b === ?e) or b === ?f) or b === ?g) or b === ?h) or b === ?i) or b === ?j) or b === ?k) or b === ?l) or b === ?m) or b === ?n) or b === ?o) or b === ?p) or b === ?q) or b === ?r) or b === ?s) or b === ?t) or b === ?u) or b === ?v) or b === ?w) or b === ?x) or b === ?y) or b === ?z) or b === ?A) or b === ?B) or b === ?C) or b === ?D) or b === ?E) or b === ?F) or b === ?G) or b === ?H) or b === ?I) or b === ?J) or b === ?K) or b === ?L) or b === ?M) or b === ?N) or b === ?O) or b === ?P) or b === ?Q) or b === ?R) or b === ?S) or b === ?T) or b === ?U) or b === ?V) or b === ?W) or b === ?X) or b === ?Y) or b === ?Z) or b === ?0) or b === ?1) or b === ?2) or b === ?3) or b === ?4) or b === ?5) or b === ?6) or b === ?7) or b === ?8) or b === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9) do
    langtag_privateuse_sub(r, acc,
                             <<t :: binary, ?-,
                                 case (a) do
                                   ?A ->
                                     ?a
                                   ?B ->
                                     ?b
                                   ?C ->
                                     ?c
                                   ?D ->
                                     ?d
                                   ?E ->
                                     ?e
                                   ?F ->
                                     ?f
                                   ?G ->
                                     ?g
                                   ?H ->
                                     ?h
                                   ?I ->
                                     ?i
                                   ?J ->
                                     ?j
                                   ?K ->
                                     ?k
                                   ?L ->
                                     ?l
                                   ?M ->
                                     ?m
                                   ?N ->
                                     ?n
                                   ?O ->
                                     ?o
                                   ?P ->
                                     ?p
                                   ?Q ->
                                     ?q
                                   ?R ->
                                     ?r
                                   ?S ->
                                     ?s
                                   ?T ->
                                     ?t
                                   ?U ->
                                     ?u
                                   ?V ->
                                     ?v
                                   ?W ->
                                     ?w
                                   ?X ->
                                     ?x
                                   ?Y ->
                                     ?y
                                   ?Z ->
                                     ?z
                                   _ ->
                                     a
                                 end,
                                 case (b) do
                                   ?A ->
                                     ?a
                                   ?B ->
                                     ?b
                                   ?C ->
                                     ?c
                                   ?D ->
                                     ?d
                                   ?E ->
                                     ?e
                                   ?F ->
                                     ?f
                                   ?G ->
                                     ?g
                                   ?H ->
                                     ?h
                                   ?I ->
                                     ?i
                                   ?J ->
                                     ?j
                                   ?K ->
                                     ?k
                                   ?L ->
                                     ?l
                                   ?M ->
                                     ?m
                                   ?N ->
                                     ?n
                                   ?O ->
                                     ?o
                                   ?P ->
                                     ?p
                                   ?Q ->
                                     ?q
                                   ?R ->
                                     ?r
                                   ?S ->
                                     ?s
                                   ?T ->
                                     ?t
                                   ?U ->
                                     ?u
                                   ?V ->
                                     ?v
                                   ?W ->
                                     ?w
                                   ?X ->
                                     ?x
                                   ?Y ->
                                     ?y
                                   ?Z ->
                                     ?z
                                   _ ->
                                     b
                                 end,
                                 case (c) do
                                   ?A ->
                                     ?a
                                   ?B ->
                                     ?b
                                   ?C ->
                                     ?c
                                   ?D ->
                                     ?d
                                   ?E ->
                                     ?e
                                   ?F ->
                                     ?f
                                   ?G ->
                                     ?g
                                   ?H ->
                                     ?h
                                   ?I ->
                                     ?i
                                   ?J ->
                                     ?j
                                   ?K ->
                                     ?k
                                   ?L ->
                                     ?l
                                   ?M ->
                                     ?m
                                   ?N ->
                                     ?n
                                   ?O ->
                                     ?o
                                   ?P ->
                                     ?p
                                   ?Q ->
                                     ?q
                                   ?R ->
                                     ?r
                                   ?S ->
                                     ?s
                                   ?T ->
                                     ?t
                                   ?U ->
                                     ?u
                                   ?V ->
                                     ?v
                                   ?W ->
                                     ?w
                                   ?X ->
                                     ?x
                                   ?Y ->
                                     ?y
                                   ?Z ->
                                     ?z
                                   _ ->
                                     c
                                 end>>,
                             n + 1)
  end

  defp langtag_privateuse_sub(<<?-, a, b, r :: bits>>, acc, t, n)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((a === ?a or a === ?b) or a === ?c) or a === ?d) or a === ?e) or a === ?f) or a === ?g) or a === ?h) or a === ?i) or a === ?j) or a === ?k) or a === ?l) or a === ?m) or a === ?n) or a === ?o) or a === ?p) or a === ?q) or a === ?r) or a === ?s) or a === ?t) or a === ?u) or a === ?v) or a === ?w) or a === ?x) or a === ?y) or a === ?z) or a === ?A) or a === ?B) or a === ?C) or a === ?D) or a === ?E) or a === ?F) or a === ?G) or a === ?H) or a === ?I) or a === ?J) or a === ?K) or a === ?L) or a === ?M) or a === ?N) or a === ?O) or a === ?P) or a === ?Q) or a === ?R) or a === ?S) or a === ?T) or a === ?U) or a === ?V) or a === ?W) or a === ?X) or a === ?Y) or a === ?Z) or a === ?0) or a === ?1) or a === ?2) or a === ?3) or a === ?4) or a === ?5) or a === ?6) or a === ?7) or a === ?8) or a === ?9 and
              ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((b === ?a or b === ?b) or b === ?c) or b === ?d) or b === ?e) or b === ?f) or b === ?g) or b === ?h) or b === ?i) or b === ?j) or b === ?k) or b === ?l) or b === ?m) or b === ?n) or b === ?o) or b === ?p) or b === ?q) or b === ?r) or b === ?s) or b === ?t) or b === ?u) or b === ?v) or b === ?w) or b === ?x) or b === ?y) or b === ?z) or b === ?A) or b === ?B) or b === ?C) or b === ?D) or b === ?E) or b === ?F) or b === ?G) or b === ?H) or b === ?I) or b === ?J) or b === ?K) or b === ?L) or b === ?M) or b === ?N) or b === ?O) or b === ?P) or b === ?Q) or b === ?R) or b === ?S) or b === ?T) or b === ?U) or b === ?V) or b === ?W) or b === ?X) or b === ?Y) or b === ?Z) or b === ?0) or b === ?1) or b === ?2) or b === ?3) or b === ?4) or b === ?5) or b === ?6) or b === ?7) or b === ?8) or b === ?9) do
    langtag_privateuse_sub(r, acc,
                             <<t :: binary, ?-,
                                 case (a) do
                                   ?A ->
                                     ?a
                                   ?B ->
                                     ?b
                                   ?C ->
                                     ?c
                                   ?D ->
                                     ?d
                                   ?E ->
                                     ?e
                                   ?F ->
                                     ?f
                                   ?G ->
                                     ?g
                                   ?H ->
                                     ?h
                                   ?I ->
                                     ?i
                                   ?J ->
                                     ?j
                                   ?K ->
                                     ?k
                                   ?L ->
                                     ?l
                                   ?M ->
                                     ?m
                                   ?N ->
                                     ?n
                                   ?O ->
                                     ?o
                                   ?P ->
                                     ?p
                                   ?Q ->
                                     ?q
                                   ?R ->
                                     ?r
                                   ?S ->
                                     ?s
                                   ?T ->
                                     ?t
                                   ?U ->
                                     ?u
                                   ?V ->
                                     ?v
                                   ?W ->
                                     ?w
                                   ?X ->
                                     ?x
                                   ?Y ->
                                     ?y
                                   ?Z ->
                                     ?z
                                   _ ->
                                     a
                                 end,
                                 case (b) do
                                   ?A ->
                                     ?a
                                   ?B ->
                                     ?b
                                   ?C ->
                                     ?c
                                   ?D ->
                                     ?d
                                   ?E ->
                                     ?e
                                   ?F ->
                                     ?f
                                   ?G ->
                                     ?g
                                   ?H ->
                                     ?h
                                   ?I ->
                                     ?i
                                   ?J ->
                                     ?j
                                   ?K ->
                                     ?k
                                   ?L ->
                                     ?l
                                   ?M ->
                                     ?m
                                   ?N ->
                                     ?n
                                   ?O ->
                                     ?o
                                   ?P ->
                                     ?p
                                   ?Q ->
                                     ?q
                                   ?R ->
                                     ?r
                                   ?S ->
                                     ?s
                                   ?T ->
                                     ?t
                                   ?U ->
                                     ?u
                                   ?V ->
                                     ?v
                                   ?W ->
                                     ?w
                                   ?X ->
                                     ?x
                                   ?Y ->
                                     ?y
                                   ?Z ->
                                     ?z
                                   _ ->
                                     b
                                 end>>,
                             n + 1)
  end

  defp langtag_privateuse_sub(<<?-, a, r :: bits>>, acc, t, n)
      when ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((a === ?a or a === ?b) or a === ?c) or a === ?d) or a === ?e) or a === ?f) or a === ?g) or a === ?h) or a === ?i) or a === ?j) or a === ?k) or a === ?l) or a === ?m) or a === ?n) or a === ?o) or a === ?p) or a === ?q) or a === ?r) or a === ?s) or a === ?t) or a === ?u) or a === ?v) or a === ?w) or a === ?x) or a === ?y) or a === ?z) or a === ?A) or a === ?B) or a === ?C) or a === ?D) or a === ?E) or a === ?F) or a === ?G) or a === ?H) or a === ?I) or a === ?J) or a === ?K) or a === ?L) or a === ?M) or a === ?N) or a === ?O) or a === ?P) or a === ?Q) or a === ?R) or a === ?S) or a === ?T) or a === ?U) or a === ?V) or a === ?W) or a === ?X) or a === ?Y) or a === ?Z) or a === ?0) or a === ?1) or a === ?2) or a === ?3) or a === ?4) or a === ?5) or a === ?6) or a === ?7) or a === ?8) or a === ?9 do
    langtag_privateuse_sub(r, acc,
                             <<t :: binary, ?-,
                                 case (a) do
                                   ?A ->
                                     ?a
                                   ?B ->
                                     ?b
                                   ?C ->
                                     ?c
                                   ?D ->
                                     ?d
                                   ?E ->
                                     ?e
                                   ?F ->
                                     ?f
                                   ?G ->
                                     ?g
                                   ?H ->
                                     ?h
                                   ?I ->
                                     ?i
                                   ?J ->
                                     ?j
                                   ?K ->
                                     ?k
                                   ?L ->
                                     ?l
                                   ?M ->
                                     ?m
                                   ?N ->
                                     ?n
                                   ?O ->
                                     ?o
                                   ?P ->
                                     ?p
                                   ?Q ->
                                     ?q
                                   ?R ->
                                     ?r
                                   ?S ->
                                     ?s
                                   ?T ->
                                     ?t
                                   ?U ->
                                     ?u
                                   ?V ->
                                     ?v
                                   ?W ->
                                     ?w
                                   ?X ->
                                     ?x
                                   ?Y ->
                                     ?y
                                   ?Z ->
                                     ?z
                                   _ ->
                                     a
                                 end>>,
                             n + 1)
  end

  defp langtag_privateuse_sub(r, acc, t, n) when n > 0 do
    langtag_list_sep(r, [t | acc])
  end

  defp langtag_list_sep(<<>>, acc) do
    :lists.reverse(acc)
  end

  defp langtag_list_sep(<<?,, r :: bits>>, acc) do
    langtag_list(r, acc)
  end

  defp langtag_list_sep(<<c, r :: bits>>, acc)
      when c === ?\s or c === ?\t do
    langtag_list_sep(r, acc)
  end

  def parse_content_length(contentLength) do
    i = :erlang.binary_to_integer(contentLength)
    true = i >= 0
    i
  end

  def parse_content_range(<<"bytes */", c, r :: bits>>)
      when ((((((((c === ?0 or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9 do
    unsatisfied_range(r, c - ?0)
  end

  def parse_content_range(<<"bytes ", c, r :: bits>>)
      when ((((((((c === ?0 or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9 do
    byte_range_first(r, c - ?0)
  end

  def parse_content_range(<<c, r :: bits>>)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9) or c === ?!) or c === ?#) or c === ?$) or c === ?%) or c === ?&) or c === ?') or c === ?*) or c === ?+) or c === ?-) or c === ?.) or c === ?^) or c === ?_) or c === ?`) or c === ?|) or c === ?~ do
    case (c) do
      ?A ->
        other_content_range_unit(r, <<<<>> :: binary, ?a>>)
      ?B ->
        other_content_range_unit(r, <<<<>> :: binary, ?b>>)
      ?C ->
        other_content_range_unit(r, <<<<>> :: binary, ?c>>)
      ?D ->
        other_content_range_unit(r, <<<<>> :: binary, ?d>>)
      ?E ->
        other_content_range_unit(r, <<<<>> :: binary, ?e>>)
      ?F ->
        other_content_range_unit(r, <<<<>> :: binary, ?f>>)
      ?G ->
        other_content_range_unit(r, <<<<>> :: binary, ?g>>)
      ?H ->
        other_content_range_unit(r, <<<<>> :: binary, ?h>>)
      ?I ->
        other_content_range_unit(r, <<<<>> :: binary, ?i>>)
      ?J ->
        other_content_range_unit(r, <<<<>> :: binary, ?j>>)
      ?K ->
        other_content_range_unit(r, <<<<>> :: binary, ?k>>)
      ?L ->
        other_content_range_unit(r, <<<<>> :: binary, ?l>>)
      ?M ->
        other_content_range_unit(r, <<<<>> :: binary, ?m>>)
      ?N ->
        other_content_range_unit(r, <<<<>> :: binary, ?n>>)
      ?O ->
        other_content_range_unit(r, <<<<>> :: binary, ?o>>)
      ?P ->
        other_content_range_unit(r, <<<<>> :: binary, ?p>>)
      ?Q ->
        other_content_range_unit(r, <<<<>> :: binary, ?q>>)
      ?R ->
        other_content_range_unit(r, <<<<>> :: binary, ?r>>)
      ?S ->
        other_content_range_unit(r, <<<<>> :: binary, ?s>>)
      ?T ->
        other_content_range_unit(r, <<<<>> :: binary, ?t>>)
      ?U ->
        other_content_range_unit(r, <<<<>> :: binary, ?u>>)
      ?V ->
        other_content_range_unit(r, <<<<>> :: binary, ?v>>)
      ?W ->
        other_content_range_unit(r, <<<<>> :: binary, ?w>>)
      ?X ->
        other_content_range_unit(r, <<<<>> :: binary, ?x>>)
      ?Y ->
        other_content_range_unit(r, <<<<>> :: binary, ?y>>)
      ?Z ->
        other_content_range_unit(r, <<<<>> :: binary, ?z>>)
      ^c ->
        other_content_range_unit(r, <<<<>> :: binary, c>>)
    end
  end

  defp byte_range_first(<<?-, c, r :: bits>>, first)
      when ((((((((c === ?0 or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9 do
    byte_range_last(r, first, c - ?0)
  end

  defp byte_range_first(<<c, r :: bits>>, first)
      when ((((((((c === ?0 or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9 do
    byte_range_first(r, first * 10 + c - ?0)
  end

  defp byte_range_last("/*", first, last) do
    {:bytes, first, last, :"*"}
  end

  defp byte_range_last(<<?/, c, r :: bits>>, first, last)
      when ((((((((c === ?0 or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9 do
    byte_range_complete(r, first, last, c - ?0)
  end

  defp byte_range_last(<<c, r :: bits>>, first, last)
      when ((((((((c === ?0 or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9 do
    byte_range_last(r, first, last * 10 + c - ?0)
  end

  defp byte_range_complete(<<>>, first, last, complete) do
    {:bytes, first, last, complete}
  end

  defp byte_range_complete(<<c, r :: bits>>, first, last, complete)
      when ((((((((c === ?0 or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9 do
    byte_range_complete(r, first, last,
                          complete * 10 + c - ?0)
  end

  defp unsatisfied_range(<<>>, complete) do
    {:bytes, :"*", complete}
  end

  defp unsatisfied_range(<<c, r :: bits>>, complete)
      when ((((((((c === ?0 or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9 do
    unsatisfied_range(r, complete * 10 + c - ?0)
  end

  defp other_content_range_unit(<<?\s, r :: bits>>, unit) do
    other_content_range_resp(r, unit, <<>>)
  end

  defp other_content_range_unit(<<c, r :: bits>>, unit)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9) or c === ?!) or c === ?#) or c === ?$) or c === ?%) or c === ?&) or c === ?') or c === ?*) or c === ?+) or c === ?-) or c === ?.) or c === ?^) or c === ?_) or c === ?`) or c === ?|) or c === ?~ do
    case (c) do
      ?A ->
        other_content_range_unit(r, <<unit :: binary, ?a>>)
      ?B ->
        other_content_range_unit(r, <<unit :: binary, ?b>>)
      ?C ->
        other_content_range_unit(r, <<unit :: binary, ?c>>)
      ?D ->
        other_content_range_unit(r, <<unit :: binary, ?d>>)
      ?E ->
        other_content_range_unit(r, <<unit :: binary, ?e>>)
      ?F ->
        other_content_range_unit(r, <<unit :: binary, ?f>>)
      ?G ->
        other_content_range_unit(r, <<unit :: binary, ?g>>)
      ?H ->
        other_content_range_unit(r, <<unit :: binary, ?h>>)
      ?I ->
        other_content_range_unit(r, <<unit :: binary, ?i>>)
      ?J ->
        other_content_range_unit(r, <<unit :: binary, ?j>>)
      ?K ->
        other_content_range_unit(r, <<unit :: binary, ?k>>)
      ?L ->
        other_content_range_unit(r, <<unit :: binary, ?l>>)
      ?M ->
        other_content_range_unit(r, <<unit :: binary, ?m>>)
      ?N ->
        other_content_range_unit(r, <<unit :: binary, ?n>>)
      ?O ->
        other_content_range_unit(r, <<unit :: binary, ?o>>)
      ?P ->
        other_content_range_unit(r, <<unit :: binary, ?p>>)
      ?Q ->
        other_content_range_unit(r, <<unit :: binary, ?q>>)
      ?R ->
        other_content_range_unit(r, <<unit :: binary, ?r>>)
      ?S ->
        other_content_range_unit(r, <<unit :: binary, ?s>>)
      ?T ->
        other_content_range_unit(r, <<unit :: binary, ?t>>)
      ?U ->
        other_content_range_unit(r, <<unit :: binary, ?u>>)
      ?V ->
        other_content_range_unit(r, <<unit :: binary, ?v>>)
      ?W ->
        other_content_range_unit(r, <<unit :: binary, ?w>>)
      ?X ->
        other_content_range_unit(r, <<unit :: binary, ?x>>)
      ?Y ->
        other_content_range_unit(r, <<unit :: binary, ?y>>)
      ?Z ->
        other_content_range_unit(r, <<unit :: binary, ?z>>)
      ^c ->
        other_content_range_unit(r, <<unit :: binary, c>>)
    end
  end

  defp other_content_range_resp(<<>>, unit, resp) do
    {unit, resp}
  end

  defp other_content_range_resp(<<c, r :: bits>>, unit, resp) when (c > 0 and
                                                c < 128) do
    other_content_range_resp(r, unit, <<resp :: binary, c>>)
  end

  def parse_content_type(<<c, r :: bits>>)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9) or c === ?!) or c === ?#) or c === ?$) or c === ?%) or c === ?&) or c === ?') or c === ?*) or c === ?+) or c === ?-) or c === ?.) or c === ?^) or c === ?_) or c === ?`) or c === ?|) or c === ?~ do
    case (c) do
      ?A ->
        media_type(r, <<<<>> :: binary, ?a>>)
      ?B ->
        media_type(r, <<<<>> :: binary, ?b>>)
      ?C ->
        media_type(r, <<<<>> :: binary, ?c>>)
      ?D ->
        media_type(r, <<<<>> :: binary, ?d>>)
      ?E ->
        media_type(r, <<<<>> :: binary, ?e>>)
      ?F ->
        media_type(r, <<<<>> :: binary, ?f>>)
      ?G ->
        media_type(r, <<<<>> :: binary, ?g>>)
      ?H ->
        media_type(r, <<<<>> :: binary, ?h>>)
      ?I ->
        media_type(r, <<<<>> :: binary, ?i>>)
      ?J ->
        media_type(r, <<<<>> :: binary, ?j>>)
      ?K ->
        media_type(r, <<<<>> :: binary, ?k>>)
      ?L ->
        media_type(r, <<<<>> :: binary, ?l>>)
      ?M ->
        media_type(r, <<<<>> :: binary, ?m>>)
      ?N ->
        media_type(r, <<<<>> :: binary, ?n>>)
      ?O ->
        media_type(r, <<<<>> :: binary, ?o>>)
      ?P ->
        media_type(r, <<<<>> :: binary, ?p>>)
      ?Q ->
        media_type(r, <<<<>> :: binary, ?q>>)
      ?R ->
        media_type(r, <<<<>> :: binary, ?r>>)
      ?S ->
        media_type(r, <<<<>> :: binary, ?s>>)
      ?T ->
        media_type(r, <<<<>> :: binary, ?t>>)
      ?U ->
        media_type(r, <<<<>> :: binary, ?u>>)
      ?V ->
        media_type(r, <<<<>> :: binary, ?v>>)
      ?W ->
        media_type(r, <<<<>> :: binary, ?w>>)
      ?X ->
        media_type(r, <<<<>> :: binary, ?x>>)
      ?Y ->
        media_type(r, <<<<>> :: binary, ?y>>)
      ?Z ->
        media_type(r, <<<<>> :: binary, ?z>>)
      ^c ->
        media_type(r, <<<<>> :: binary, c>>)
    end
  end

  defp media_type(<<?/, c, r :: bits>>, t)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9) or c === ?!) or c === ?#) or c === ?$) or c === ?%) or c === ?&) or c === ?') or c === ?*) or c === ?+) or c === ?-) or c === ?.) or c === ?^) or c === ?_) or c === ?`) or c === ?|) or c === ?~ do
    case (c) do
      ?A ->
        media_subtype(r, t, <<<<>> :: binary, ?a>>)
      ?B ->
        media_subtype(r, t, <<<<>> :: binary, ?b>>)
      ?C ->
        media_subtype(r, t, <<<<>> :: binary, ?c>>)
      ?D ->
        media_subtype(r, t, <<<<>> :: binary, ?d>>)
      ?E ->
        media_subtype(r, t, <<<<>> :: binary, ?e>>)
      ?F ->
        media_subtype(r, t, <<<<>> :: binary, ?f>>)
      ?G ->
        media_subtype(r, t, <<<<>> :: binary, ?g>>)
      ?H ->
        media_subtype(r, t, <<<<>> :: binary, ?h>>)
      ?I ->
        media_subtype(r, t, <<<<>> :: binary, ?i>>)
      ?J ->
        media_subtype(r, t, <<<<>> :: binary, ?j>>)
      ?K ->
        media_subtype(r, t, <<<<>> :: binary, ?k>>)
      ?L ->
        media_subtype(r, t, <<<<>> :: binary, ?l>>)
      ?M ->
        media_subtype(r, t, <<<<>> :: binary, ?m>>)
      ?N ->
        media_subtype(r, t, <<<<>> :: binary, ?n>>)
      ?O ->
        media_subtype(r, t, <<<<>> :: binary, ?o>>)
      ?P ->
        media_subtype(r, t, <<<<>> :: binary, ?p>>)
      ?Q ->
        media_subtype(r, t, <<<<>> :: binary, ?q>>)
      ?R ->
        media_subtype(r, t, <<<<>> :: binary, ?r>>)
      ?S ->
        media_subtype(r, t, <<<<>> :: binary, ?s>>)
      ?T ->
        media_subtype(r, t, <<<<>> :: binary, ?t>>)
      ?U ->
        media_subtype(r, t, <<<<>> :: binary, ?u>>)
      ?V ->
        media_subtype(r, t, <<<<>> :: binary, ?v>>)
      ?W ->
        media_subtype(r, t, <<<<>> :: binary, ?w>>)
      ?X ->
        media_subtype(r, t, <<<<>> :: binary, ?x>>)
      ?Y ->
        media_subtype(r, t, <<<<>> :: binary, ?y>>)
      ?Z ->
        media_subtype(r, t, <<<<>> :: binary, ?z>>)
      ^c ->
        media_subtype(r, t, <<<<>> :: binary, c>>)
    end
  end

  defp media_type(<<c, r :: bits>>, t)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9) or c === ?!) or c === ?#) or c === ?$) or c === ?%) or c === ?&) or c === ?') or c === ?*) or c === ?+) or c === ?-) or c === ?.) or c === ?^) or c === ?_) or c === ?`) or c === ?|) or c === ?~ do
    case (c) do
      ?A ->
        media_type(r, <<t :: binary, ?a>>)
      ?B ->
        media_type(r, <<t :: binary, ?b>>)
      ?C ->
        media_type(r, <<t :: binary, ?c>>)
      ?D ->
        media_type(r, <<t :: binary, ?d>>)
      ?E ->
        media_type(r, <<t :: binary, ?e>>)
      ?F ->
        media_type(r, <<t :: binary, ?f>>)
      ?G ->
        media_type(r, <<t :: binary, ?g>>)
      ?H ->
        media_type(r, <<t :: binary, ?h>>)
      ?I ->
        media_type(r, <<t :: binary, ?i>>)
      ?J ->
        media_type(r, <<t :: binary, ?j>>)
      ?K ->
        media_type(r, <<t :: binary, ?k>>)
      ?L ->
        media_type(r, <<t :: binary, ?l>>)
      ?M ->
        media_type(r, <<t :: binary, ?m>>)
      ?N ->
        media_type(r, <<t :: binary, ?n>>)
      ?O ->
        media_type(r, <<t :: binary, ?o>>)
      ?P ->
        media_type(r, <<t :: binary, ?p>>)
      ?Q ->
        media_type(r, <<t :: binary, ?q>>)
      ?R ->
        media_type(r, <<t :: binary, ?r>>)
      ?S ->
        media_type(r, <<t :: binary, ?s>>)
      ?T ->
        media_type(r, <<t :: binary, ?t>>)
      ?U ->
        media_type(r, <<t :: binary, ?u>>)
      ?V ->
        media_type(r, <<t :: binary, ?v>>)
      ?W ->
        media_type(r, <<t :: binary, ?w>>)
      ?X ->
        media_type(r, <<t :: binary, ?x>>)
      ?Y ->
        media_type(r, <<t :: binary, ?y>>)
      ?Z ->
        media_type(r, <<t :: binary, ?z>>)
      ^c ->
        media_type(r, <<t :: binary, c>>)
    end
  end

  defp media_subtype(<<c, r :: bits>>, t, s)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9) or c === ?!) or c === ?#) or c === ?$) or c === ?%) or c === ?&) or c === ?') or c === ?*) or c === ?+) or c === ?-) or c === ?.) or c === ?^) or c === ?_) or c === ?`) or c === ?|) or c === ?~ do
    case (c) do
      ?A ->
        media_subtype(r, t, <<s :: binary, ?a>>)
      ?B ->
        media_subtype(r, t, <<s :: binary, ?b>>)
      ?C ->
        media_subtype(r, t, <<s :: binary, ?c>>)
      ?D ->
        media_subtype(r, t, <<s :: binary, ?d>>)
      ?E ->
        media_subtype(r, t, <<s :: binary, ?e>>)
      ?F ->
        media_subtype(r, t, <<s :: binary, ?f>>)
      ?G ->
        media_subtype(r, t, <<s :: binary, ?g>>)
      ?H ->
        media_subtype(r, t, <<s :: binary, ?h>>)
      ?I ->
        media_subtype(r, t, <<s :: binary, ?i>>)
      ?J ->
        media_subtype(r, t, <<s :: binary, ?j>>)
      ?K ->
        media_subtype(r, t, <<s :: binary, ?k>>)
      ?L ->
        media_subtype(r, t, <<s :: binary, ?l>>)
      ?M ->
        media_subtype(r, t, <<s :: binary, ?m>>)
      ?N ->
        media_subtype(r, t, <<s :: binary, ?n>>)
      ?O ->
        media_subtype(r, t, <<s :: binary, ?o>>)
      ?P ->
        media_subtype(r, t, <<s :: binary, ?p>>)
      ?Q ->
        media_subtype(r, t, <<s :: binary, ?q>>)
      ?R ->
        media_subtype(r, t, <<s :: binary, ?r>>)
      ?S ->
        media_subtype(r, t, <<s :: binary, ?s>>)
      ?T ->
        media_subtype(r, t, <<s :: binary, ?t>>)
      ?U ->
        media_subtype(r, t, <<s :: binary, ?u>>)
      ?V ->
        media_subtype(r, t, <<s :: binary, ?v>>)
      ?W ->
        media_subtype(r, t, <<s :: binary, ?w>>)
      ?X ->
        media_subtype(r, t, <<s :: binary, ?x>>)
      ?Y ->
        media_subtype(r, t, <<s :: binary, ?y>>)
      ?Z ->
        media_subtype(r, t, <<s :: binary, ?z>>)
      ^c ->
        media_subtype(r, t, <<s :: binary, c>>)
    end
  end

  defp media_subtype(r, t, s) do
    media_param_sep(r, t, s, [])
  end

  defp media_param_sep(<<>>, t, s, p) do
    {t, s, :lists.reverse(p)}
  end

  defp media_param_sep(<<?;, r :: bits>>, t, s, p) do
    media_before_param(r, t, s, p)
  end

  defp media_param_sep(<<c, r :: bits>>, t, s, p)
      when c === ?\s or c === ?\t do
    media_param_sep(r, t, s, p)
  end

  defp media_before_param(<<c, r :: bits>>, t, s, p)
      when c === ?\s or c === ?\t do
    media_before_param(r, t, s, p)
  end

  defp media_before_param(<<"charset=", ?", r :: bits>>, t, s, p) do
    media_charset_quoted(r, t, s, p, <<>>)
  end

  defp media_before_param(<<"charset=", r :: bits>>, t, s, p) do
    media_charset(r, t, s, p, <<>>)
  end

  defp media_before_param(<<c, r :: bits>>, t, s, p)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9) or c === ?!) or c === ?#) or c === ?$) or c === ?%) or c === ?&) or c === ?') or c === ?*) or c === ?+) or c === ?-) or c === ?.) or c === ?^) or c === ?_) or c === ?`) or c === ?|) or c === ?~ do
    case (c) do
      ?A ->
        media_param(r, t, s, p, <<<<>> :: binary, ?a>>)
      ?B ->
        media_param(r, t, s, p, <<<<>> :: binary, ?b>>)
      ?C ->
        media_param(r, t, s, p, <<<<>> :: binary, ?c>>)
      ?D ->
        media_param(r, t, s, p, <<<<>> :: binary, ?d>>)
      ?E ->
        media_param(r, t, s, p, <<<<>> :: binary, ?e>>)
      ?F ->
        media_param(r, t, s, p, <<<<>> :: binary, ?f>>)
      ?G ->
        media_param(r, t, s, p, <<<<>> :: binary, ?g>>)
      ?H ->
        media_param(r, t, s, p, <<<<>> :: binary, ?h>>)
      ?I ->
        media_param(r, t, s, p, <<<<>> :: binary, ?i>>)
      ?J ->
        media_param(r, t, s, p, <<<<>> :: binary, ?j>>)
      ?K ->
        media_param(r, t, s, p, <<<<>> :: binary, ?k>>)
      ?L ->
        media_param(r, t, s, p, <<<<>> :: binary, ?l>>)
      ?M ->
        media_param(r, t, s, p, <<<<>> :: binary, ?m>>)
      ?N ->
        media_param(r, t, s, p, <<<<>> :: binary, ?n>>)
      ?O ->
        media_param(r, t, s, p, <<<<>> :: binary, ?o>>)
      ?P ->
        media_param(r, t, s, p, <<<<>> :: binary, ?p>>)
      ?Q ->
        media_param(r, t, s, p, <<<<>> :: binary, ?q>>)
      ?R ->
        media_param(r, t, s, p, <<<<>> :: binary, ?r>>)
      ?S ->
        media_param(r, t, s, p, <<<<>> :: binary, ?s>>)
      ?T ->
        media_param(r, t, s, p, <<<<>> :: binary, ?t>>)
      ?U ->
        media_param(r, t, s, p, <<<<>> :: binary, ?u>>)
      ?V ->
        media_param(r, t, s, p, <<<<>> :: binary, ?v>>)
      ?W ->
        media_param(r, t, s, p, <<<<>> :: binary, ?w>>)
      ?X ->
        media_param(r, t, s, p, <<<<>> :: binary, ?x>>)
      ?Y ->
        media_param(r, t, s, p, <<<<>> :: binary, ?y>>)
      ?Z ->
        media_param(r, t, s, p, <<<<>> :: binary, ?z>>)
      ^c ->
        media_param(r, t, s, p, <<<<>> :: binary, c>>)
    end
  end

  defp media_charset_quoted(<<?", r :: bits>>, t, s, p, v) do
    media_param_sep(r, t, s, [{"charset", v} | p])
  end

  defp media_charset_quoted(<<?\\, c, r :: bits>>, t, s, p, v)
      when c === ?\t or (c > 31 and c !== 127) do
    case (c) do
      ?A ->
        media_charset_quoted(r, t, s, p, <<v :: binary, ?a>>)
      ?B ->
        media_charset_quoted(r, t, s, p, <<v :: binary, ?b>>)
      ?C ->
        media_charset_quoted(r, t, s, p, <<v :: binary, ?c>>)
      ?D ->
        media_charset_quoted(r, t, s, p, <<v :: binary, ?d>>)
      ?E ->
        media_charset_quoted(r, t, s, p, <<v :: binary, ?e>>)
      ?F ->
        media_charset_quoted(r, t, s, p, <<v :: binary, ?f>>)
      ?G ->
        media_charset_quoted(r, t, s, p, <<v :: binary, ?g>>)
      ?H ->
        media_charset_quoted(r, t, s, p, <<v :: binary, ?h>>)
      ?I ->
        media_charset_quoted(r, t, s, p, <<v :: binary, ?i>>)
      ?J ->
        media_charset_quoted(r, t, s, p, <<v :: binary, ?j>>)
      ?K ->
        media_charset_quoted(r, t, s, p, <<v :: binary, ?k>>)
      ?L ->
        media_charset_quoted(r, t, s, p, <<v :: binary, ?l>>)
      ?M ->
        media_charset_quoted(r, t, s, p, <<v :: binary, ?m>>)
      ?N ->
        media_charset_quoted(r, t, s, p, <<v :: binary, ?n>>)
      ?O ->
        media_charset_quoted(r, t, s, p, <<v :: binary, ?o>>)
      ?P ->
        media_charset_quoted(r, t, s, p, <<v :: binary, ?p>>)
      ?Q ->
        media_charset_quoted(r, t, s, p, <<v :: binary, ?q>>)
      ?R ->
        media_charset_quoted(r, t, s, p, <<v :: binary, ?r>>)
      ?S ->
        media_charset_quoted(r, t, s, p, <<v :: binary, ?s>>)
      ?T ->
        media_charset_quoted(r, t, s, p, <<v :: binary, ?t>>)
      ?U ->
        media_charset_quoted(r, t, s, p, <<v :: binary, ?u>>)
      ?V ->
        media_charset_quoted(r, t, s, p, <<v :: binary, ?v>>)
      ?W ->
        media_charset_quoted(r, t, s, p, <<v :: binary, ?w>>)
      ?X ->
        media_charset_quoted(r, t, s, p, <<v :: binary, ?x>>)
      ?Y ->
        media_charset_quoted(r, t, s, p, <<v :: binary, ?y>>)
      ?Z ->
        media_charset_quoted(r, t, s, p, <<v :: binary, ?z>>)
      ^c ->
        media_charset_quoted(r, t, s, p, <<v :: binary, c>>)
    end
  end

  defp media_charset_quoted(<<c, r :: bits>>, t, s, p, v) when c === ?\t or
                                               (c > 31 and c !== 127) do
    case (c) do
      ?A ->
        media_charset_quoted(r, t, s, p, <<v :: binary, ?a>>)
      ?B ->
        media_charset_quoted(r, t, s, p, <<v :: binary, ?b>>)
      ?C ->
        media_charset_quoted(r, t, s, p, <<v :: binary, ?c>>)
      ?D ->
        media_charset_quoted(r, t, s, p, <<v :: binary, ?d>>)
      ?E ->
        media_charset_quoted(r, t, s, p, <<v :: binary, ?e>>)
      ?F ->
        media_charset_quoted(r, t, s, p, <<v :: binary, ?f>>)
      ?G ->
        media_charset_quoted(r, t, s, p, <<v :: binary, ?g>>)
      ?H ->
        media_charset_quoted(r, t, s, p, <<v :: binary, ?h>>)
      ?I ->
        media_charset_quoted(r, t, s, p, <<v :: binary, ?i>>)
      ?J ->
        media_charset_quoted(r, t, s, p, <<v :: binary, ?j>>)
      ?K ->
        media_charset_quoted(r, t, s, p, <<v :: binary, ?k>>)
      ?L ->
        media_charset_quoted(r, t, s, p, <<v :: binary, ?l>>)
      ?M ->
        media_charset_quoted(r, t, s, p, <<v :: binary, ?m>>)
      ?N ->
        media_charset_quoted(r, t, s, p, <<v :: binary, ?n>>)
      ?O ->
        media_charset_quoted(r, t, s, p, <<v :: binary, ?o>>)
      ?P ->
        media_charset_quoted(r, t, s, p, <<v :: binary, ?p>>)
      ?Q ->
        media_charset_quoted(r, t, s, p, <<v :: binary, ?q>>)
      ?R ->
        media_charset_quoted(r, t, s, p, <<v :: binary, ?r>>)
      ?S ->
        media_charset_quoted(r, t, s, p, <<v :: binary, ?s>>)
      ?T ->
        media_charset_quoted(r, t, s, p, <<v :: binary, ?t>>)
      ?U ->
        media_charset_quoted(r, t, s, p, <<v :: binary, ?u>>)
      ?V ->
        media_charset_quoted(r, t, s, p, <<v :: binary, ?v>>)
      ?W ->
        media_charset_quoted(r, t, s, p, <<v :: binary, ?w>>)
      ?X ->
        media_charset_quoted(r, t, s, p, <<v :: binary, ?x>>)
      ?Y ->
        media_charset_quoted(r, t, s, p, <<v :: binary, ?y>>)
      ?Z ->
        media_charset_quoted(r, t, s, p, <<v :: binary, ?z>>)
      ^c ->
        media_charset_quoted(r, t, s, p, <<v :: binary, c>>)
    end
  end

  defp media_charset(<<c, r :: bits>>, t, s, p, v)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9) or c === ?!) or c === ?#) or c === ?$) or c === ?%) or c === ?&) or c === ?') or c === ?*) or c === ?+) or c === ?-) or c === ?.) or c === ?^) or c === ?_) or c === ?`) or c === ?|) or c === ?~ do
    case (c) do
      ?A ->
        media_charset(r, t, s, p, <<v :: binary, ?a>>)
      ?B ->
        media_charset(r, t, s, p, <<v :: binary, ?b>>)
      ?C ->
        media_charset(r, t, s, p, <<v :: binary, ?c>>)
      ?D ->
        media_charset(r, t, s, p, <<v :: binary, ?d>>)
      ?E ->
        media_charset(r, t, s, p, <<v :: binary, ?e>>)
      ?F ->
        media_charset(r, t, s, p, <<v :: binary, ?f>>)
      ?G ->
        media_charset(r, t, s, p, <<v :: binary, ?g>>)
      ?H ->
        media_charset(r, t, s, p, <<v :: binary, ?h>>)
      ?I ->
        media_charset(r, t, s, p, <<v :: binary, ?i>>)
      ?J ->
        media_charset(r, t, s, p, <<v :: binary, ?j>>)
      ?K ->
        media_charset(r, t, s, p, <<v :: binary, ?k>>)
      ?L ->
        media_charset(r, t, s, p, <<v :: binary, ?l>>)
      ?M ->
        media_charset(r, t, s, p, <<v :: binary, ?m>>)
      ?N ->
        media_charset(r, t, s, p, <<v :: binary, ?n>>)
      ?O ->
        media_charset(r, t, s, p, <<v :: binary, ?o>>)
      ?P ->
        media_charset(r, t, s, p, <<v :: binary, ?p>>)
      ?Q ->
        media_charset(r, t, s, p, <<v :: binary, ?q>>)
      ?R ->
        media_charset(r, t, s, p, <<v :: binary, ?r>>)
      ?S ->
        media_charset(r, t, s, p, <<v :: binary, ?s>>)
      ?T ->
        media_charset(r, t, s, p, <<v :: binary, ?t>>)
      ?U ->
        media_charset(r, t, s, p, <<v :: binary, ?u>>)
      ?V ->
        media_charset(r, t, s, p, <<v :: binary, ?v>>)
      ?W ->
        media_charset(r, t, s, p, <<v :: binary, ?w>>)
      ?X ->
        media_charset(r, t, s, p, <<v :: binary, ?x>>)
      ?Y ->
        media_charset(r, t, s, p, <<v :: binary, ?y>>)
      ?Z ->
        media_charset(r, t, s, p, <<v :: binary, ?z>>)
      ^c ->
        media_charset(r, t, s, p, <<v :: binary, c>>)
    end
  end

  defp media_charset(r, t, s, p, v) do
    media_param_sep(r, t, s, [{"charset", v} | p])
  end

  defp media_param(<<?=, ?", r :: bits>>, t, s, p, k) do
    media_quoted(r, t, s, p, k, <<>>)
  end

  defp media_param(<<?=, c, r :: bits>>, t, s, p, k)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9) or c === ?!) or c === ?#) or c === ?$) or c === ?%) or c === ?&) or c === ?') or c === ?*) or c === ?+) or c === ?-) or c === ?.) or c === ?^) or c === ?_) or c === ?`) or c === ?|) or c === ?~ do
    media_value(r, t, s, p, k, <<c>>)
  end

  defp media_param(<<c, r :: bits>>, t, s, p, k)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9) or c === ?!) or c === ?#) or c === ?$) or c === ?%) or c === ?&) or c === ?') or c === ?*) or c === ?+) or c === ?-) or c === ?.) or c === ?^) or c === ?_) or c === ?`) or c === ?|) or c === ?~ do
    case (c) do
      ?A ->
        media_param(r, t, s, p, <<k :: binary, ?a>>)
      ?B ->
        media_param(r, t, s, p, <<k :: binary, ?b>>)
      ?C ->
        media_param(r, t, s, p, <<k :: binary, ?c>>)
      ?D ->
        media_param(r, t, s, p, <<k :: binary, ?d>>)
      ?E ->
        media_param(r, t, s, p, <<k :: binary, ?e>>)
      ?F ->
        media_param(r, t, s, p, <<k :: binary, ?f>>)
      ?G ->
        media_param(r, t, s, p, <<k :: binary, ?g>>)
      ?H ->
        media_param(r, t, s, p, <<k :: binary, ?h>>)
      ?I ->
        media_param(r, t, s, p, <<k :: binary, ?i>>)
      ?J ->
        media_param(r, t, s, p, <<k :: binary, ?j>>)
      ?K ->
        media_param(r, t, s, p, <<k :: binary, ?k>>)
      ?L ->
        media_param(r, t, s, p, <<k :: binary, ?l>>)
      ?M ->
        media_param(r, t, s, p, <<k :: binary, ?m>>)
      ?N ->
        media_param(r, t, s, p, <<k :: binary, ?n>>)
      ?O ->
        media_param(r, t, s, p, <<k :: binary, ?o>>)
      ?P ->
        media_param(r, t, s, p, <<k :: binary, ?p>>)
      ?Q ->
        media_param(r, t, s, p, <<k :: binary, ?q>>)
      ?R ->
        media_param(r, t, s, p, <<k :: binary, ?r>>)
      ?S ->
        media_param(r, t, s, p, <<k :: binary, ?s>>)
      ?T ->
        media_param(r, t, s, p, <<k :: binary, ?t>>)
      ?U ->
        media_param(r, t, s, p, <<k :: binary, ?u>>)
      ?V ->
        media_param(r, t, s, p, <<k :: binary, ?v>>)
      ?W ->
        media_param(r, t, s, p, <<k :: binary, ?w>>)
      ?X ->
        media_param(r, t, s, p, <<k :: binary, ?x>>)
      ?Y ->
        media_param(r, t, s, p, <<k :: binary, ?y>>)
      ?Z ->
        media_param(r, t, s, p, <<k :: binary, ?z>>)
      ^c ->
        media_param(r, t, s, p, <<k :: binary, c>>)
    end
  end

  defp media_quoted(<<?", r :: bits>>, t, s, p, k, v) do
    media_param_sep(r, t, s, [{k, v} | p])
  end

  defp media_quoted(<<?\\, c, r :: bits>>, t, s, p, k, v)
      when c === ?\t or (c > 31 and c !== 127) do
    media_quoted(r, t, s, p, k, <<v :: binary, c>>)
  end

  defp media_quoted(<<c, r :: bits>>, t, s, p, k, v)
      when c === ?\t or (c > 31 and c !== 127) do
    media_quoted(r, t, s, p, k, <<v :: binary, c>>)
  end

  defp media_value(<<c, r :: bits>>, t, s, p, k, v)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9) or c === ?!) or c === ?#) or c === ?$) or c === ?%) or c === ?&) or c === ?') or c === ?*) or c === ?+) or c === ?-) or c === ?.) or c === ?^) or c === ?_) or c === ?`) or c === ?|) or c === ?~ do
    media_value(r, t, s, p, k, <<v :: binary, c>>)
  end

  defp media_value(r, t, s, p, k, v) do
    media_param_sep(r, t, s, [{k, v} | p])
  end

  def parse_cookie(cookie) do
    :cow_cookie.parse_cookie(cookie)
  end

  def parse_date(date) do
    :cow_date.parse_date(date)
  end

  def parse_etag(<<?W, ?/, ?", r :: bits>>) do
    etag(r, :weak, <<>>)
  end

  def parse_etag(<<?", r :: bits>>) do
    etag(r, :strong, <<>>)
  end

  defp etag(<<?">>, strength, tag) do
    {strength, tag}
  end

  defp etag(<<c, r :: bits>>, strength, tag)
      when c === 33 or (c >= 35 and c !== 127) do
    etag(r, strength, <<tag :: binary, c>>)
  end

  def parse_expect("100-continue") do
    :continue
  end

  def parse_expect(<<"100-", c, o, n, t, i, m, u, e>>)
      when (c === ?C or c === ?c and o === ?O or o === ?o and
              n === ?N or n === ?n and t === ?T or t === ?t and
              i === ?I or i === ?i and m === ?N or m === ?n and
              u === ?U or u === ?u and e === ?E or e === ?e) do
    :continue
  end

  def parse_expires("0") do
    {{1, 1, 1}, {0, 0, 0}}
  end

  def parse_expires(expires) do
    try do
      :cow_date.parse_date(expires)
    catch
      _, _ ->
        {{1, 1, 1}, {0, 0, 0}}
    end
  end

  def parse_host(<<?[, r :: bits>>) do
    ipv6_address(r, <<?[>>)
  end

  def parse_host(host) do
    reg_name(host, <<>>)
  end

  defp ipv6_address(<<?]>>, iP) do
    {<<iP :: binary, ?]>>, :undefined}
  end

  defp ipv6_address(<<?], ?:, port :: bits>>, iP) do
    {<<iP :: binary, ?]>>, :erlang.binary_to_integer(port)}
  end

  defp ipv6_address(<<c, r :: bits>>, iP)
      when ((((((((((((((((((((((c === ?0 or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9) or c === ?a) or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?:) or c === ?. do
    case (c) do
      ?A ->
        ipv6_address(r, <<iP :: binary, ?a>>)
      ?B ->
        ipv6_address(r, <<iP :: binary, ?b>>)
      ?C ->
        ipv6_address(r, <<iP :: binary, ?c>>)
      ?D ->
        ipv6_address(r, <<iP :: binary, ?d>>)
      ?E ->
        ipv6_address(r, <<iP :: binary, ?e>>)
      ?F ->
        ipv6_address(r, <<iP :: binary, ?f>>)
      ?G ->
        ipv6_address(r, <<iP :: binary, ?g>>)
      ?H ->
        ipv6_address(r, <<iP :: binary, ?h>>)
      ?I ->
        ipv6_address(r, <<iP :: binary, ?i>>)
      ?J ->
        ipv6_address(r, <<iP :: binary, ?j>>)
      ?K ->
        ipv6_address(r, <<iP :: binary, ?k>>)
      ?L ->
        ipv6_address(r, <<iP :: binary, ?l>>)
      ?M ->
        ipv6_address(r, <<iP :: binary, ?m>>)
      ?N ->
        ipv6_address(r, <<iP :: binary, ?n>>)
      ?O ->
        ipv6_address(r, <<iP :: binary, ?o>>)
      ?P ->
        ipv6_address(r, <<iP :: binary, ?p>>)
      ?Q ->
        ipv6_address(r, <<iP :: binary, ?q>>)
      ?R ->
        ipv6_address(r, <<iP :: binary, ?r>>)
      ?S ->
        ipv6_address(r, <<iP :: binary, ?s>>)
      ?T ->
        ipv6_address(r, <<iP :: binary, ?t>>)
      ?U ->
        ipv6_address(r, <<iP :: binary, ?u>>)
      ?V ->
        ipv6_address(r, <<iP :: binary, ?v>>)
      ?W ->
        ipv6_address(r, <<iP :: binary, ?w>>)
      ?X ->
        ipv6_address(r, <<iP :: binary, ?x>>)
      ?Y ->
        ipv6_address(r, <<iP :: binary, ?y>>)
      ?Z ->
        ipv6_address(r, <<iP :: binary, ?z>>)
      ^c ->
        ipv6_address(r, <<iP :: binary, c>>)
    end
  end

  defp reg_name(<<>>, name) do
    {name, :undefined}
  end

  defp reg_name(<<?:, port :: bits>>, name) do
    {name, :erlang.binary_to_integer(port)}
  end

  defp reg_name(<<c, r :: bits>>, name)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9) or c === ?-) or c === ?.) or c === ?_) or c === ?~) or c === ?!) or c === ?$) or c === ?&) or c === ?') or c === ?() or c === ?)) or c === ?*) or c === ?+) or c === ?,) or c === ?;) or c === ?= do
    case (c) do
      ?A ->
        reg_name(r, <<name :: binary, ?a>>)
      ?B ->
        reg_name(r, <<name :: binary, ?b>>)
      ?C ->
        reg_name(r, <<name :: binary, ?c>>)
      ?D ->
        reg_name(r, <<name :: binary, ?d>>)
      ?E ->
        reg_name(r, <<name :: binary, ?e>>)
      ?F ->
        reg_name(r, <<name :: binary, ?f>>)
      ?G ->
        reg_name(r, <<name :: binary, ?g>>)
      ?H ->
        reg_name(r, <<name :: binary, ?h>>)
      ?I ->
        reg_name(r, <<name :: binary, ?i>>)
      ?J ->
        reg_name(r, <<name :: binary, ?j>>)
      ?K ->
        reg_name(r, <<name :: binary, ?k>>)
      ?L ->
        reg_name(r, <<name :: binary, ?l>>)
      ?M ->
        reg_name(r, <<name :: binary, ?m>>)
      ?N ->
        reg_name(r, <<name :: binary, ?n>>)
      ?O ->
        reg_name(r, <<name :: binary, ?o>>)
      ?P ->
        reg_name(r, <<name :: binary, ?p>>)
      ?Q ->
        reg_name(r, <<name :: binary, ?q>>)
      ?R ->
        reg_name(r, <<name :: binary, ?r>>)
      ?S ->
        reg_name(r, <<name :: binary, ?s>>)
      ?T ->
        reg_name(r, <<name :: binary, ?t>>)
      ?U ->
        reg_name(r, <<name :: binary, ?u>>)
      ?V ->
        reg_name(r, <<name :: binary, ?v>>)
      ?W ->
        reg_name(r, <<name :: binary, ?w>>)
      ?X ->
        reg_name(r, <<name :: binary, ?x>>)
      ?Y ->
        reg_name(r, <<name :: binary, ?y>>)
      ?Z ->
        reg_name(r, <<name :: binary, ?z>>)
      ^c ->
        reg_name(r, <<name :: binary, c>>)
    end
  end

  def parse_http2_settings(hTTP2Settings) do
    :cow_http2.parse_settings_payload(:base64.decode(hTTP2Settings))
  end

  def parse_if_match("*") do
    :"*"
  end

  def parse_if_match(ifMatch) do
    nonempty(etag_list(ifMatch, []))
  end

  defp etag_list(<<>>, acc) do
    :lists.reverse(acc)
  end

  defp etag_list(<<c, r :: bits>>, acc)
      when (c === ?\s or c === ?\t) or c === ?, do
    etag_list(r, acc)
  end

  defp etag_list(<<?W, ?/, ?", r :: bits>>, acc) do
    etag(r, acc, :weak, <<>>)
  end

  defp etag_list(<<?", r :: bits>>, acc) do
    etag(r, acc, :strong, <<>>)
  end

  defp etag(<<?", r :: bits>>, acc, strength, tag) do
    etag_list_sep(r, [{strength, tag} | acc])
  end

  defp etag(<<c, r :: bits>>, acc, strength, tag)
      when c === 33 or (c >= 35 and c !== 127) do
    etag(r, acc, strength, <<tag :: binary, c>>)
  end

  defp etag_list_sep(<<>>, acc) do
    :lists.reverse(acc)
  end

  defp etag_list_sep(<<c, r :: bits>>, acc)
      when c === ?\s or c === ?\t do
    etag_list_sep(r, acc)
  end

  defp etag_list_sep(<<?,, r :: bits>>, acc) do
    etag_list(r, acc)
  end

  def parse_if_modified_since(ifModifiedSince) do
    :cow_date.parse_date(ifModifiedSince)
  end

  def parse_if_none_match("*") do
    :"*"
  end

  def parse_if_none_match(ifNoneMatch) do
    nonempty(etag_list(ifNoneMatch, []))
  end

  def parse_if_range(<<?W, ?/, ?", r :: bits>>) do
    etag(r, :weak, <<>>)
  end

  def parse_if_range(<<?", r :: bits>>) do
    etag(r, :strong, <<>>)
  end

  def parse_if_range(ifRange) do
    :cow_date.parse_date(ifRange)
  end

  def parse_if_unmodified_since(ifModifiedSince) do
    :cow_date.parse_date(ifModifiedSince)
  end

  def parse_last_modified(lastModified) do
    :cow_date.parse_date(lastModified)
  end

  def parse_link(link) do
    :cow_link.parse_link(link)
  end

  def parse_max_forwards(maxForwards) do
    i = :erlang.binary_to_integer(maxForwards)
    true = i >= 0
    i
  end

  def parse_origin(origins) do
    nonempty(origin_scheme(origins, []))
  end

  defp origin_scheme(<<>>, acc) do
    acc
  end

  defp origin_scheme(<<"http://", r :: bits>>, acc) do
    origin_host(r, acc, "http")
  end

  defp origin_scheme(<<"https://", r :: bits>>, acc) do
    origin_host(r, acc, "https")
  end

  defp origin_scheme(<<c, r :: bits>>, acc)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9) or c === ?!) or c === ?#) or c === ?$) or c === ?%) or c === ?&) or c === ?') or c === ?*) or c === ?+) or c === ?-) or c === ?.) or c === ?^) or c === ?_) or c === ?`) or c === ?|) or c === ?~ do
    origin_scheme(next_origin(r), [make_ref() | acc])
  end

  defp origin_host(<<?[, r :: bits>>, acc, scheme) do
    origin_ipv6_address(r, acc, scheme, <<?[>>)
  end

  defp origin_host(host, acc, scheme) do
    origin_reg_name(host, acc, scheme, <<>>)
  end

  defp origin_ipv6_address(<<?]>>, acc, scheme, iP) do
    :lists.reverse([{scheme, <<iP :: binary, ?]>>,
                       default_port(scheme)} |
                        acc])
  end

  defp origin_ipv6_address(<<?], ?\s, r :: bits>>, acc, scheme, iP) do
    origin_scheme(r,
                    [{scheme, <<iP :: binary, ?]>>, default_port(scheme)} |
                         acc])
  end

  defp origin_ipv6_address(<<?], ?:, port :: bits>>, acc, scheme, iP) do
    origin_port(port, acc, scheme, <<iP :: binary, ?]>>,
                  <<>>)
  end

  defp origin_ipv6_address(<<c, r :: bits>>, acc, scheme, iP)
      when ((((((((((((((((((((((c === ?0 or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9) or c === ?a) or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?:) or c === ?. do
    case (c) do
      ?A ->
        origin_ipv6_address(r, acc, scheme,
                              <<iP :: binary, ?a>>)
      ?B ->
        origin_ipv6_address(r, acc, scheme,
                              <<iP :: binary, ?b>>)
      ?C ->
        origin_ipv6_address(r, acc, scheme,
                              <<iP :: binary, ?c>>)
      ?D ->
        origin_ipv6_address(r, acc, scheme,
                              <<iP :: binary, ?d>>)
      ?E ->
        origin_ipv6_address(r, acc, scheme,
                              <<iP :: binary, ?e>>)
      ?F ->
        origin_ipv6_address(r, acc, scheme,
                              <<iP :: binary, ?f>>)
      ?G ->
        origin_ipv6_address(r, acc, scheme,
                              <<iP :: binary, ?g>>)
      ?H ->
        origin_ipv6_address(r, acc, scheme,
                              <<iP :: binary, ?h>>)
      ?I ->
        origin_ipv6_address(r, acc, scheme,
                              <<iP :: binary, ?i>>)
      ?J ->
        origin_ipv6_address(r, acc, scheme,
                              <<iP :: binary, ?j>>)
      ?K ->
        origin_ipv6_address(r, acc, scheme,
                              <<iP :: binary, ?k>>)
      ?L ->
        origin_ipv6_address(r, acc, scheme,
                              <<iP :: binary, ?l>>)
      ?M ->
        origin_ipv6_address(r, acc, scheme,
                              <<iP :: binary, ?m>>)
      ?N ->
        origin_ipv6_address(r, acc, scheme,
                              <<iP :: binary, ?n>>)
      ?O ->
        origin_ipv6_address(r, acc, scheme,
                              <<iP :: binary, ?o>>)
      ?P ->
        origin_ipv6_address(r, acc, scheme,
                              <<iP :: binary, ?p>>)
      ?Q ->
        origin_ipv6_address(r, acc, scheme,
                              <<iP :: binary, ?q>>)
      ?R ->
        origin_ipv6_address(r, acc, scheme,
                              <<iP :: binary, ?r>>)
      ?S ->
        origin_ipv6_address(r, acc, scheme,
                              <<iP :: binary, ?s>>)
      ?T ->
        origin_ipv6_address(r, acc, scheme,
                              <<iP :: binary, ?t>>)
      ?U ->
        origin_ipv6_address(r, acc, scheme,
                              <<iP :: binary, ?u>>)
      ?V ->
        origin_ipv6_address(r, acc, scheme,
                              <<iP :: binary, ?v>>)
      ?W ->
        origin_ipv6_address(r, acc, scheme,
                              <<iP :: binary, ?w>>)
      ?X ->
        origin_ipv6_address(r, acc, scheme,
                              <<iP :: binary, ?x>>)
      ?Y ->
        origin_ipv6_address(r, acc, scheme,
                              <<iP :: binary, ?y>>)
      ?Z ->
        origin_ipv6_address(r, acc, scheme,
                              <<iP :: binary, ?z>>)
      ^c ->
        origin_ipv6_address(r, acc, scheme, <<iP :: binary, c>>)
    end
  end

  defp origin_reg_name(<<>>, acc, scheme, name) do
    :lists.reverse([{scheme, name, default_port(scheme)} |
                        acc])
  end

  defp origin_reg_name(<<?\s, r :: bits>>, acc, scheme, name) do
    origin_scheme(r,
                    [{scheme, name, default_port(scheme)} | acc])
  end

  defp origin_reg_name(<<?:, port :: bits>>, acc, scheme, name) do
    origin_port(port, acc, scheme, name, <<>>)
  end

  defp origin_reg_name(<<c, r :: bits>>, acc, scheme, name)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9) or c === ?-) or c === ?.) or c === ?_) or c === ?~) or c === ?!) or c === ?$) or c === ?&) or c === ?') or c === ?() or c === ?)) or c === ?*) or c === ?+) or c === ?,) or c === ?;) or c === ?= do
    case (c) do
      ?A ->
        origin_reg_name(r, acc, scheme, <<name :: binary, ?a>>)
      ?B ->
        origin_reg_name(r, acc, scheme, <<name :: binary, ?b>>)
      ?C ->
        origin_reg_name(r, acc, scheme, <<name :: binary, ?c>>)
      ?D ->
        origin_reg_name(r, acc, scheme, <<name :: binary, ?d>>)
      ?E ->
        origin_reg_name(r, acc, scheme, <<name :: binary, ?e>>)
      ?F ->
        origin_reg_name(r, acc, scheme, <<name :: binary, ?f>>)
      ?G ->
        origin_reg_name(r, acc, scheme, <<name :: binary, ?g>>)
      ?H ->
        origin_reg_name(r, acc, scheme, <<name :: binary, ?h>>)
      ?I ->
        origin_reg_name(r, acc, scheme, <<name :: binary, ?i>>)
      ?J ->
        origin_reg_name(r, acc, scheme, <<name :: binary, ?j>>)
      ?K ->
        origin_reg_name(r, acc, scheme, <<name :: binary, ?k>>)
      ?L ->
        origin_reg_name(r, acc, scheme, <<name :: binary, ?l>>)
      ?M ->
        origin_reg_name(r, acc, scheme, <<name :: binary, ?m>>)
      ?N ->
        origin_reg_name(r, acc, scheme, <<name :: binary, ?n>>)
      ?O ->
        origin_reg_name(r, acc, scheme, <<name :: binary, ?o>>)
      ?P ->
        origin_reg_name(r, acc, scheme, <<name :: binary, ?p>>)
      ?Q ->
        origin_reg_name(r, acc, scheme, <<name :: binary, ?q>>)
      ?R ->
        origin_reg_name(r, acc, scheme, <<name :: binary, ?r>>)
      ?S ->
        origin_reg_name(r, acc, scheme, <<name :: binary, ?s>>)
      ?T ->
        origin_reg_name(r, acc, scheme, <<name :: binary, ?t>>)
      ?U ->
        origin_reg_name(r, acc, scheme, <<name :: binary, ?u>>)
      ?V ->
        origin_reg_name(r, acc, scheme, <<name :: binary, ?v>>)
      ?W ->
        origin_reg_name(r, acc, scheme, <<name :: binary, ?w>>)
      ?X ->
        origin_reg_name(r, acc, scheme, <<name :: binary, ?x>>)
      ?Y ->
        origin_reg_name(r, acc, scheme, <<name :: binary, ?y>>)
      ?Z ->
        origin_reg_name(r, acc, scheme, <<name :: binary, ?z>>)
      ^c ->
        origin_reg_name(r, acc, scheme, <<name :: binary, c>>)
    end
  end

  defp origin_port(<<>>, acc, scheme, host, port) do
    :lists.reverse([{scheme, host,
                       :erlang.binary_to_integer(port)} |
                        acc])
  end

  defp origin_port(<<?\s, r :: bits>>, acc, scheme, host, port) do
    origin_scheme(r,
                    [{scheme, host, :erlang.binary_to_integer(port)} | acc])
  end

  defp origin_port(<<c, r :: bits>>, acc, scheme, host, port)
      when ((((((((c === ?0 or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9 do
    origin_port(r, acc, scheme, host, <<port :: binary, c>>)
  end

  defp next_origin(<<>>) do
    <<>>
  end

  defp next_origin(<<?\s, c, r :: bits>>)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9) or c === ?!) or c === ?#) or c === ?$) or c === ?%) or c === ?&) or c === ?') or c === ?*) or c === ?+) or c === ?-) or c === ?.) or c === ?^) or c === ?_) or c === ?`) or c === ?|) or c === ?~ do
    <<c, r :: bits>>
  end

  defp next_origin(<<c, r :: bits>>)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9) or c === ?!) or c === ?#) or c === ?$) or c === ?%) or c === ?&) or c === ?') or c === ?*) or c === ?+) or c === ?-) or c === ?.) or c === ?^) or c === ?_) or c === ?`) or c === ?|) or c === ?~) or c === ?:) or c === ?/ do
    next_origin(r)
  end

  defp default_port("http") do
    80
  end

  defp default_port("https") do
    443
  end

  def parse_pragma("no-cache") do
    :no_cache
  end

  def parse_pragma(_) do
    :cache
  end

  def parse_proxy_authenticate(proxyAuthenticate) do
    parse_www_authenticate(proxyAuthenticate)
  end

  def parse_proxy_authorization(proxyAuthorization) do
    parse_authorization(proxyAuthorization)
  end

  def parse_range(<<"bytes=", r :: bits>>) do
    bytes_range_set(r, [])
  end

  def parse_range(<<c, r :: bits>>)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9) or c === ?!) or c === ?#) or c === ?$) or c === ?%) or c === ?&) or c === ?') or c === ?*) or c === ?+) or c === ?-) or c === ?.) or c === ?^) or c === ?_) or c === ?`) or c === ?|) or c === ?~ do
    case (c) do
      ?A ->
        other_range_unit(r, <<<<>> :: binary, ?a>>)
      ?B ->
        other_range_unit(r, <<<<>> :: binary, ?b>>)
      ?C ->
        other_range_unit(r, <<<<>> :: binary, ?c>>)
      ?D ->
        other_range_unit(r, <<<<>> :: binary, ?d>>)
      ?E ->
        other_range_unit(r, <<<<>> :: binary, ?e>>)
      ?F ->
        other_range_unit(r, <<<<>> :: binary, ?f>>)
      ?G ->
        other_range_unit(r, <<<<>> :: binary, ?g>>)
      ?H ->
        other_range_unit(r, <<<<>> :: binary, ?h>>)
      ?I ->
        other_range_unit(r, <<<<>> :: binary, ?i>>)
      ?J ->
        other_range_unit(r, <<<<>> :: binary, ?j>>)
      ?K ->
        other_range_unit(r, <<<<>> :: binary, ?k>>)
      ?L ->
        other_range_unit(r, <<<<>> :: binary, ?l>>)
      ?M ->
        other_range_unit(r, <<<<>> :: binary, ?m>>)
      ?N ->
        other_range_unit(r, <<<<>> :: binary, ?n>>)
      ?O ->
        other_range_unit(r, <<<<>> :: binary, ?o>>)
      ?P ->
        other_range_unit(r, <<<<>> :: binary, ?p>>)
      ?Q ->
        other_range_unit(r, <<<<>> :: binary, ?q>>)
      ?R ->
        other_range_unit(r, <<<<>> :: binary, ?r>>)
      ?S ->
        other_range_unit(r, <<<<>> :: binary, ?s>>)
      ?T ->
        other_range_unit(r, <<<<>> :: binary, ?t>>)
      ?U ->
        other_range_unit(r, <<<<>> :: binary, ?u>>)
      ?V ->
        other_range_unit(r, <<<<>> :: binary, ?v>>)
      ?W ->
        other_range_unit(r, <<<<>> :: binary, ?w>>)
      ?X ->
        other_range_unit(r, <<<<>> :: binary, ?x>>)
      ?Y ->
        other_range_unit(r, <<<<>> :: binary, ?y>>)
      ?Z ->
        other_range_unit(r, <<<<>> :: binary, ?z>>)
      ^c ->
        other_range_unit(r, <<<<>> :: binary, c>>)
    end
  end

  defp bytes_range_set(<<>>, acc) do
    {:bytes, :lists.reverse(acc)}
  end

  defp bytes_range_set(<<c, r :: bits>>, acc)
      when (c === ?\s or c === ?\t) or c === ?, do
    bytes_range_set(r, acc)
  end

  defp bytes_range_set(<<?-, c, r :: bits>>, acc)
      when ((((((((c === ?0 or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9 do
    bytes_range_suffix_spec(r, acc, c - ?0)
  end

  defp bytes_range_set(<<c, r :: bits>>, acc)
      when ((((((((c === ?0 or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9 do
    bytes_range_spec(r, acc, c - ?0)
  end

  defp bytes_range_spec(<<?-, c, r :: bits>>, acc, first)
      when ((((((((c === ?0 or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9 do
    bytes_range_spec_last(r, acc, first, c - ?0)
  end

  defp bytes_range_spec(<<?-, r :: bits>>, acc, first) do
    bytes_range_set_sep(r, [{first, :infinity} | acc])
  end

  defp bytes_range_spec(<<c, r :: bits>>, acc, first)
      when ((((((((c === ?0 or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9 do
    bytes_range_spec(r, acc, first * 10 + c - ?0)
  end

  defp bytes_range_spec_last(<<c, r :: bits>>, acc, first, last)
      when ((((((((c === ?0 or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9 do
    bytes_range_spec_last(r, acc, first, last * 10 + c - ?0)
  end

  defp bytes_range_spec_last(r, acc, first, last) do
    bytes_range_set_sep(r, [{first, last} | acc])
  end

  defp bytes_range_suffix_spec(<<c, r :: bits>>, acc, suffix)
      when ((((((((c === ?0 or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9 do
    bytes_range_suffix_spec(r, acc, suffix * 10 + c - ?0)
  end

  defp bytes_range_suffix_spec(r, acc, suffix) do
    bytes_range_set_sep(r, [- suffix | acc])
  end

  defp bytes_range_set_sep(<<>>, acc) do
    {:bytes, :lists.reverse(acc)}
  end

  defp bytes_range_set_sep(<<c, r :: bits>>, acc)
      when c === ?\s or c === ?\t do
    bytes_range_set_sep(r, acc)
  end

  defp bytes_range_set_sep(<<?,, r :: bits>>, acc) do
    bytes_range_set(r, acc)
  end

  defp other_range_unit(<<?=, c, r :: bits>>, u) when c === ?\t or
                                          (c > 31 and c < 127) do
    other_range_set(r, u, <<c>>)
  end

  defp other_range_unit(<<c, r :: bits>>, u)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9) or c === ?!) or c === ?#) or c === ?$) or c === ?%) or c === ?&) or c === ?') or c === ?*) or c === ?+) or c === ?-) or c === ?.) or c === ?^) or c === ?_) or c === ?`) or c === ?|) or c === ?~ do
    case (c) do
      ?A ->
        other_range_unit(r, <<u :: binary, ?a>>)
      ?B ->
        other_range_unit(r, <<u :: binary, ?b>>)
      ?C ->
        other_range_unit(r, <<u :: binary, ?c>>)
      ?D ->
        other_range_unit(r, <<u :: binary, ?d>>)
      ?E ->
        other_range_unit(r, <<u :: binary, ?e>>)
      ?F ->
        other_range_unit(r, <<u :: binary, ?f>>)
      ?G ->
        other_range_unit(r, <<u :: binary, ?g>>)
      ?H ->
        other_range_unit(r, <<u :: binary, ?h>>)
      ?I ->
        other_range_unit(r, <<u :: binary, ?i>>)
      ?J ->
        other_range_unit(r, <<u :: binary, ?j>>)
      ?K ->
        other_range_unit(r, <<u :: binary, ?k>>)
      ?L ->
        other_range_unit(r, <<u :: binary, ?l>>)
      ?M ->
        other_range_unit(r, <<u :: binary, ?m>>)
      ?N ->
        other_range_unit(r, <<u :: binary, ?n>>)
      ?O ->
        other_range_unit(r, <<u :: binary, ?o>>)
      ?P ->
        other_range_unit(r, <<u :: binary, ?p>>)
      ?Q ->
        other_range_unit(r, <<u :: binary, ?q>>)
      ?R ->
        other_range_unit(r, <<u :: binary, ?r>>)
      ?S ->
        other_range_unit(r, <<u :: binary, ?s>>)
      ?T ->
        other_range_unit(r, <<u :: binary, ?t>>)
      ?U ->
        other_range_unit(r, <<u :: binary, ?u>>)
      ?V ->
        other_range_unit(r, <<u :: binary, ?v>>)
      ?W ->
        other_range_unit(r, <<u :: binary, ?w>>)
      ?X ->
        other_range_unit(r, <<u :: binary, ?x>>)
      ?Y ->
        other_range_unit(r, <<u :: binary, ?y>>)
      ?Z ->
        other_range_unit(r, <<u :: binary, ?z>>)
      ^c ->
        other_range_unit(r, <<u :: binary, c>>)
    end
  end

  defp other_range_set(<<>>, u, s) do
    {u, s}
  end

  defp other_range_set(<<c, r :: bits>>, u, s) when c === ?\t or
                                         (c > 31 and c < 127) do
    other_range_set(r, u, <<s :: binary, c>>)
  end

  def parse_retry_after(retryAfter = <<d, _ :: bits>>)
      when ((((((((d === ?0 or d === ?1) or d === ?2) or d === ?3) or d === ?4) or d === ?5) or d === ?6) or d === ?7) or d === ?8) or d === ?9 do
    i = :erlang.binary_to_integer(retryAfter)
    true = i >= 0
    i
  end

  def parse_retry_after(retryAfter) do
    :cow_date.parse_date(retryAfter)
  end

  def parse_sec_websocket_accept(secWebSocketAccept) do
    secWebSocketAccept
  end

  def parse_sec_websocket_extensions(secWebSocketExtensions) do
    nonempty(ws_extension_list(secWebSocketExtensions, []))
  end

  defp ws_extension_list(<<>>, acc) do
    :lists.reverse(acc)
  end

  defp ws_extension_list(<<c, r :: bits>>, acc)
      when (c === ?\s or c === ?\t) or c === ?, do
    ws_extension_list(r, acc)
  end

  defp ws_extension_list(<<c, r :: bits>>, acc)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9) or c === ?!) or c === ?#) or c === ?$) or c === ?%) or c === ?&) or c === ?') or c === ?*) or c === ?+) or c === ?-) or c === ?.) or c === ?^) or c === ?_) or c === ?`) or c === ?|) or c === ?~ do
    ws_extension(r, acc, <<c>>)
  end

  defp ws_extension(<<c, r :: bits>>, acc, e)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9) or c === ?!) or c === ?#) or c === ?$) or c === ?%) or c === ?&) or c === ?') or c === ?*) or c === ?+) or c === ?-) or c === ?.) or c === ?^) or c === ?_) or c === ?`) or c === ?|) or c === ?~ do
    ws_extension(r, acc, <<e :: binary, c>>)
  end

  defp ws_extension(r, acc, e) do
    ws_extension_param_sep(r, acc, e, [])
  end

  defp ws_extension_param_sep(<<>>, acc, e, p) do
    :lists.reverse([{e, :lists.reverse(p)} | acc])
  end

  defp ws_extension_param_sep(<<?,, r :: bits>>, acc, e, p) do
    ws_extension_list(r, [{e, :lists.reverse(p)} | acc])
  end

  defp ws_extension_param_sep(<<?;, r :: bits>>, acc, e, p) do
    ws_extension_before_param(r, acc, e, p)
  end

  defp ws_extension_param_sep(<<c, r :: bits>>, acc, e, p)
      when c === ?\s or c === ?\t do
    ws_extension_param_sep(r, acc, e, p)
  end

  defp ws_extension_before_param(<<c, r :: bits>>, acc, e, p)
      when c === ?\s or c === ?\t do
    ws_extension_before_param(r, acc, e, p)
  end

  defp ws_extension_before_param(<<c, r :: bits>>, acc, e, p)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9) or c === ?!) or c === ?#) or c === ?$) or c === ?%) or c === ?&) or c === ?') or c === ?*) or c === ?+) or c === ?-) or c === ?.) or c === ?^) or c === ?_) or c === ?`) or c === ?|) or c === ?~ do
    ws_extension_param(r, acc, e, p, <<c>>)
  end

  defp ws_extension_param(<<?=, ?", r :: bits>>, acc, e, p, k) do
    ws_extension_quoted(r, acc, e, p, k, <<>>)
  end

  defp ws_extension_param(<<?=, c, r :: bits>>, acc, e, p, k)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9) or c === ?!) or c === ?#) or c === ?$) or c === ?%) or c === ?&) or c === ?') or c === ?*) or c === ?+) or c === ?-) or c === ?.) or c === ?^) or c === ?_) or c === ?`) or c === ?|) or c === ?~ do
    ws_extension_value(r, acc, e, p, k, <<c>>)
  end

  defp ws_extension_param(<<c, r :: bits>>, acc, e, p, k)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9) or c === ?!) or c === ?#) or c === ?$) or c === ?%) or c === ?&) or c === ?') or c === ?*) or c === ?+) or c === ?-) or c === ?.) or c === ?^) or c === ?_) or c === ?`) or c === ?|) or c === ?~ do
    ws_extension_param(r, acc, e, p, <<k :: binary, c>>)
  end

  defp ws_extension_param(r, acc, e, p, k) do
    ws_extension_param_sep(r, acc, e, [k | p])
  end

  defp ws_extension_quoted(<<?", r :: bits>>, acc, e, p, k, v) do
    ws_extension_param_sep(r, acc, e, [{k, v} | p])
  end

  defp ws_extension_quoted(<<?\\, c, r :: bits>>, acc, e, p, k, v)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9) or c === ?!) or c === ?#) or c === ?$) or c === ?%) or c === ?&) or c === ?') or c === ?*) or c === ?+) or c === ?-) or c === ?.) or c === ?^) or c === ?_) or c === ?`) or c === ?|) or c === ?~ do
    ws_extension_quoted(r, acc, e, p, k, <<v :: binary, c>>)
  end

  defp ws_extension_quoted(<<c, r :: bits>>, acc, e, p, k, v)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9) or c === ?!) or c === ?#) or c === ?$) or c === ?%) or c === ?&) or c === ?') or c === ?*) or c === ?+) or c === ?-) or c === ?.) or c === ?^) or c === ?_) or c === ?`) or c === ?|) or c === ?~ do
    ws_extension_quoted(r, acc, e, p, k, <<v :: binary, c>>)
  end

  defp ws_extension_value(<<c, r :: bits>>, acc, e, p, k, v)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9) or c === ?!) or c === ?#) or c === ?$) or c === ?%) or c === ?&) or c === ?') or c === ?*) or c === ?+) or c === ?-) or c === ?.) or c === ?^) or c === ?_) or c === ?`) or c === ?|) or c === ?~ do
    ws_extension_value(r, acc, e, p, k, <<v :: binary, c>>)
  end

  defp ws_extension_value(r, acc, e, p, k, v) do
    ws_extension_param_sep(r, acc, e, [{k, v} | p])
  end

  def parse_sec_websocket_key(secWebSocketKey) do
    secWebSocketKey
  end

  def parse_sec_websocket_protocol_req(secWebSocketProtocol) do
    nonempty(token_list(secWebSocketProtocol, []))
  end

  def parse_sec_websocket_protocol_resp(protocol) do
    true = <<>> !== protocol
    :ok = validate_token(protocol)
    protocol
  end

  def parse_sec_websocket_version_req(secWebSocketVersion)
      when byte_size(secWebSocketVersion) < 4 do
    version = :erlang.binary_to_integer(secWebSocketVersion)
    true = version >= 0 and version <= 255
    version
  end

  def parse_sec_websocket_version_resp(secWebSocketVersion) do
    nonempty(ws_version_list(secWebSocketVersion, []))
  end

  defp ws_version_list(<<>>, acc) do
    :lists.reverse(acc)
  end

  defp ws_version_list(<<c, r :: bits>>, acc)
      when (c === ?\s or c === ?\t) or c === ?, do
    ws_version_list(r, acc)
  end

  defp ws_version_list(<<c, r :: bits>>, acc)
      when ((((((((c === ?0 or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9 do
    ws_version(r, acc, c - ?0)
  end

  defp ws_version(<<c, r :: bits>>, acc, v)
      when ((((((((c === ?0 or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9 do
    ws_version(r, acc, v * 10 + c - ?0)
  end

  defp ws_version(r, acc, v) do
    ws_version_list_sep(r, [v | acc])
  end

  defp ws_version_list_sep(<<>>, acc) do
    :lists.reverse(acc)
  end

  defp ws_version_list_sep(<<c, r :: bits>>, acc)
      when c === ?\s or c === ?\t do
    ws_version_list_sep(r, acc)
  end

  defp ws_version_list_sep(<<?,, r :: bits>>, acc) do
    ws_version_list(r, acc)
  end

  def parse_set_cookie(setCookie) do
    :cow_cookie.parse_set_cookie(setCookie)
  end

  def parse_te(tE) do
    te_list(tE, :no_trailers, [])
  end

  defp te_list(<<>>, trail, acc) do
    {trail, :lists.reverse(acc)}
  end

  defp te_list(<<c, r :: bits>>, trail, acc)
      when (c === ?\s or c === ?\t) or c === ?, do
    te_list(r, trail, acc)
  end

  defp te_list(<<"trailers", r :: bits>>, trail, acc) do
    te(r, trail, acc, "trailers")
  end

  defp te_list(<<"compress", r :: bits>>, trail, acc) do
    te(r, trail, acc, "compress")
  end

  defp te_list(<<"deflate", r :: bits>>, trail, acc) do
    te(r, trail, acc, "deflate")
  end

  defp te_list(<<"gzip", r :: bits>>, trail, acc) do
    te(r, trail, acc, "gzip")
  end

  defp te_list(<<c, r :: bits>>, trail, acc)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9) or c === ?!) or c === ?#) or c === ?$) or c === ?%) or c === ?&) or c === ?') or c === ?*) or c === ?+) or c === ?-) or c === ?.) or c === ?^) or c === ?_) or c === ?`) or c === ?|) or c === ?~ do
    case (c) do
      ?A ->
        te(r, trail, acc, <<<<>> :: binary, ?a>>)
      ?B ->
        te(r, trail, acc, <<<<>> :: binary, ?b>>)
      ?C ->
        te(r, trail, acc, <<<<>> :: binary, ?c>>)
      ?D ->
        te(r, trail, acc, <<<<>> :: binary, ?d>>)
      ?E ->
        te(r, trail, acc, <<<<>> :: binary, ?e>>)
      ?F ->
        te(r, trail, acc, <<<<>> :: binary, ?f>>)
      ?G ->
        te(r, trail, acc, <<<<>> :: binary, ?g>>)
      ?H ->
        te(r, trail, acc, <<<<>> :: binary, ?h>>)
      ?I ->
        te(r, trail, acc, <<<<>> :: binary, ?i>>)
      ?J ->
        te(r, trail, acc, <<<<>> :: binary, ?j>>)
      ?K ->
        te(r, trail, acc, <<<<>> :: binary, ?k>>)
      ?L ->
        te(r, trail, acc, <<<<>> :: binary, ?l>>)
      ?M ->
        te(r, trail, acc, <<<<>> :: binary, ?m>>)
      ?N ->
        te(r, trail, acc, <<<<>> :: binary, ?n>>)
      ?O ->
        te(r, trail, acc, <<<<>> :: binary, ?o>>)
      ?P ->
        te(r, trail, acc, <<<<>> :: binary, ?p>>)
      ?Q ->
        te(r, trail, acc, <<<<>> :: binary, ?q>>)
      ?R ->
        te(r, trail, acc, <<<<>> :: binary, ?r>>)
      ?S ->
        te(r, trail, acc, <<<<>> :: binary, ?s>>)
      ?T ->
        te(r, trail, acc, <<<<>> :: binary, ?t>>)
      ?U ->
        te(r, trail, acc, <<<<>> :: binary, ?u>>)
      ?V ->
        te(r, trail, acc, <<<<>> :: binary, ?v>>)
      ?W ->
        te(r, trail, acc, <<<<>> :: binary, ?w>>)
      ?X ->
        te(r, trail, acc, <<<<>> :: binary, ?x>>)
      ?Y ->
        te(r, trail, acc, <<<<>> :: binary, ?y>>)
      ?Z ->
        te(r, trail, acc, <<<<>> :: binary, ?z>>)
      ^c ->
        te(r, trail, acc, <<<<>> :: binary, c>>)
    end
  end

  defp te(<<>>, _, acc, "trailers") do
    {:trailers, :lists.reverse(acc)}
  end

  defp te(<<?,, r :: bits>>, _, acc, "trailers") do
    te_list(r, :trailers, acc)
  end

  defp te(<<?;, r :: bits>>, trail, acc, t)
      when t !== "trailers" do
    te_before_weight(r, trail, acc, t)
  end

  defp te(<<c, r :: bits>>, _, acc, "trailers")
      when c === ?\s or c === ?\t do
    te_list_sep(r, :trailers, acc)
  end

  defp te(<<c, r :: bits>>, trail, acc, t)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9) or c === ?!) or c === ?#) or c === ?$) or c === ?%) or c === ?&) or c === ?') or c === ?*) or c === ?+) or c === ?-) or c === ?.) or c === ?^) or c === ?_) or c === ?`) or c === ?|) or c === ?~ do
    case (c) do
      ?A ->
        te(r, trail, acc, <<t :: binary, ?a>>)
      ?B ->
        te(r, trail, acc, <<t :: binary, ?b>>)
      ?C ->
        te(r, trail, acc, <<t :: binary, ?c>>)
      ?D ->
        te(r, trail, acc, <<t :: binary, ?d>>)
      ?E ->
        te(r, trail, acc, <<t :: binary, ?e>>)
      ?F ->
        te(r, trail, acc, <<t :: binary, ?f>>)
      ?G ->
        te(r, trail, acc, <<t :: binary, ?g>>)
      ?H ->
        te(r, trail, acc, <<t :: binary, ?h>>)
      ?I ->
        te(r, trail, acc, <<t :: binary, ?i>>)
      ?J ->
        te(r, trail, acc, <<t :: binary, ?j>>)
      ?K ->
        te(r, trail, acc, <<t :: binary, ?k>>)
      ?L ->
        te(r, trail, acc, <<t :: binary, ?l>>)
      ?M ->
        te(r, trail, acc, <<t :: binary, ?m>>)
      ?N ->
        te(r, trail, acc, <<t :: binary, ?n>>)
      ?O ->
        te(r, trail, acc, <<t :: binary, ?o>>)
      ?P ->
        te(r, trail, acc, <<t :: binary, ?p>>)
      ?Q ->
        te(r, trail, acc, <<t :: binary, ?q>>)
      ?R ->
        te(r, trail, acc, <<t :: binary, ?r>>)
      ?S ->
        te(r, trail, acc, <<t :: binary, ?s>>)
      ?T ->
        te(r, trail, acc, <<t :: binary, ?t>>)
      ?U ->
        te(r, trail, acc, <<t :: binary, ?u>>)
      ?V ->
        te(r, trail, acc, <<t :: binary, ?v>>)
      ?W ->
        te(r, trail, acc, <<t :: binary, ?w>>)
      ?X ->
        te(r, trail, acc, <<t :: binary, ?x>>)
      ?Y ->
        te(r, trail, acc, <<t :: binary, ?y>>)
      ?Z ->
        te(r, trail, acc, <<t :: binary, ?z>>)
      ^c ->
        te(r, trail, acc, <<t :: binary, c>>)
    end
  end

  defp te(r, trail, acc, t) do
    te_param_sep(r, trail, acc, t)
  end

  defp te_param_sep(<<>>, trail, acc, t) do
    {trail, :lists.reverse([{t, 1000} | acc])}
  end

  defp te_param_sep(<<?,, r :: bits>>, trail, acc, t) do
    te_list(r, trail, [{t, 1000} | acc])
  end

  defp te_param_sep(<<c, r :: bits>>, trail, acc, t)
      when c === ?\s or c === ?\t do
    te_param_sep(r, trail, acc, t)
  end

  defp te_before_weight(<<c, r :: bits>>, trail, acc, t)
      when c === ?\s or c === ?\t do
    te_before_weight(r, trail, acc, t)
  end

  defp te_before_weight(<<?q, ?=, r :: bits>>, trail, acc, t) do
    te_weight(r, trail, acc, t)
  end

  defp te_weight(<<"1.000", r :: bits>>, trail, acc, t) do
    te_list_sep(r, trail, [{t, 1000} | acc])
  end

  defp te_weight(<<"1.00", r :: bits>>, trail, acc, t) do
    te_list_sep(r, trail, [{t, 1000} | acc])
  end

  defp te_weight(<<"1.0", r :: bits>>, trail, acc, t) do
    te_list_sep(r, trail, [{t, 1000} | acc])
  end

  defp te_weight(<<"1.", r :: bits>>, trail, acc, t) do
    te_list_sep(r, trail, [{t, 1000} | acc])
  end

  defp te_weight(<<"1", r :: bits>>, trail, acc, t) do
    te_list_sep(r, trail, [{t, 1000} | acc])
  end

  defp te_weight(<<"0.", a, b, c, r :: bits>>, trail, acc, t)
      when (((((((((a === ?0 or a === ?1) or a === ?2) or a === ?3) or a === ?4) or a === ?5) or a === ?6) or a === ?7) or a === ?8) or a === ?9 and
              ((((((((b === ?0 or b === ?1) or b === ?2) or b === ?3) or b === ?4) or b === ?5) or b === ?6) or b === ?7) or b === ?8) or b === ?9 and
              ((((((((c === ?0 or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9) do
    te_list_sep(r, trail,
                  [{t, (a - ?0) * 100 + (b - ?0) * 10 + (c - ?0)} | acc])
  end

  defp te_weight(<<"0.", a, b, r :: bits>>, trail, acc, t)
      when (((((((((a === ?0 or a === ?1) or a === ?2) or a === ?3) or a === ?4) or a === ?5) or a === ?6) or a === ?7) or a === ?8) or a === ?9 and
              ((((((((b === ?0 or b === ?1) or b === ?2) or b === ?3) or b === ?4) or b === ?5) or b === ?6) or b === ?7) or b === ?8) or b === ?9) do
    te_list_sep(r, trail,
                  [{t, (a - ?0) * 100 + (b - ?0) * 10} | acc])
  end

  defp te_weight(<<"0.", a, r :: bits>>, trail, acc, t)
      when ((((((((a === ?0 or a === ?1) or a === ?2) or a === ?3) or a === ?4) or a === ?5) or a === ?6) or a === ?7) or a === ?8) or a === ?9 do
    te_list_sep(r, trail, [{t, (a - ?0) * 100} | acc])
  end

  defp te_weight(<<"0.", r :: bits>>, trail, acc, t) do
    te_list_sep(r, trail, [{t, 0} | acc])
  end

  defp te_weight(<<"0", r :: bits>>, trail, acc, t) do
    te_list_sep(r, trail, [{t, 0} | acc])
  end

  defp te_list_sep(<<>>, trail, acc) do
    {trail, :lists.reverse(acc)}
  end

  defp te_list_sep(<<c, r :: bits>>, trail, acc)
      when c === ?\s or c === ?\t do
    te_list_sep(r, trail, acc)
  end

  defp te_list_sep(<<?,, r :: bits>>, trail, acc) do
    te_list(r, trail, acc)
  end

  def parse_trailer(trailer) do
    nonempty(token_ci_list(trailer, []))
  end

  def parse_transfer_encoding("chunked") do
    ["chunked"]
  end

  def parse_transfer_encoding(transferEncoding) do
    nonempty(token_ci_list(transferEncoding, []))
  end

  def parse_upgrade(upgrade) do
    nonempty(protocol_list(upgrade, []))
  end

  defp protocol_list(<<>>, acc) do
    :lists.reverse(acc)
  end

  defp protocol_list(<<c, r :: bits>>, acc)
      when (c === ?\s or c === ?\t) or c === ?, do
    protocol_list(r, acc)
  end

  defp protocol_list(<<c, r :: bits>>, acc)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9) or c === ?!) or c === ?#) or c === ?$) or c === ?%) or c === ?&) or c === ?') or c === ?*) or c === ?+) or c === ?-) or c === ?.) or c === ?^) or c === ?_) or c === ?`) or c === ?|) or c === ?~ do
    case (c) do
      ?A ->
        protocol_name(r, acc, <<<<>> :: binary, ?a>>)
      ?B ->
        protocol_name(r, acc, <<<<>> :: binary, ?b>>)
      ?C ->
        protocol_name(r, acc, <<<<>> :: binary, ?c>>)
      ?D ->
        protocol_name(r, acc, <<<<>> :: binary, ?d>>)
      ?E ->
        protocol_name(r, acc, <<<<>> :: binary, ?e>>)
      ?F ->
        protocol_name(r, acc, <<<<>> :: binary, ?f>>)
      ?G ->
        protocol_name(r, acc, <<<<>> :: binary, ?g>>)
      ?H ->
        protocol_name(r, acc, <<<<>> :: binary, ?h>>)
      ?I ->
        protocol_name(r, acc, <<<<>> :: binary, ?i>>)
      ?J ->
        protocol_name(r, acc, <<<<>> :: binary, ?j>>)
      ?K ->
        protocol_name(r, acc, <<<<>> :: binary, ?k>>)
      ?L ->
        protocol_name(r, acc, <<<<>> :: binary, ?l>>)
      ?M ->
        protocol_name(r, acc, <<<<>> :: binary, ?m>>)
      ?N ->
        protocol_name(r, acc, <<<<>> :: binary, ?n>>)
      ?O ->
        protocol_name(r, acc, <<<<>> :: binary, ?o>>)
      ?P ->
        protocol_name(r, acc, <<<<>> :: binary, ?p>>)
      ?Q ->
        protocol_name(r, acc, <<<<>> :: binary, ?q>>)
      ?R ->
        protocol_name(r, acc, <<<<>> :: binary, ?r>>)
      ?S ->
        protocol_name(r, acc, <<<<>> :: binary, ?s>>)
      ?T ->
        protocol_name(r, acc, <<<<>> :: binary, ?t>>)
      ?U ->
        protocol_name(r, acc, <<<<>> :: binary, ?u>>)
      ?V ->
        protocol_name(r, acc, <<<<>> :: binary, ?v>>)
      ?W ->
        protocol_name(r, acc, <<<<>> :: binary, ?w>>)
      ?X ->
        protocol_name(r, acc, <<<<>> :: binary, ?x>>)
      ?Y ->
        protocol_name(r, acc, <<<<>> :: binary, ?y>>)
      ?Z ->
        protocol_name(r, acc, <<<<>> :: binary, ?z>>)
      ^c ->
        protocol_name(r, acc, <<<<>> :: binary, c>>)
    end
  end

  defp protocol_name(<<?/, c, r :: bits>>, acc, p) do
    case (c) do
      ?A ->
        protocol_version(r, acc,
                           <<<<p :: binary, ?/>> :: binary, ?a>>)
      ?B ->
        protocol_version(r, acc,
                           <<<<p :: binary, ?/>> :: binary, ?b>>)
      ?C ->
        protocol_version(r, acc,
                           <<<<p :: binary, ?/>> :: binary, ?c>>)
      ?D ->
        protocol_version(r, acc,
                           <<<<p :: binary, ?/>> :: binary, ?d>>)
      ?E ->
        protocol_version(r, acc,
                           <<<<p :: binary, ?/>> :: binary, ?e>>)
      ?F ->
        protocol_version(r, acc,
                           <<<<p :: binary, ?/>> :: binary, ?f>>)
      ?G ->
        protocol_version(r, acc,
                           <<<<p :: binary, ?/>> :: binary, ?g>>)
      ?H ->
        protocol_version(r, acc,
                           <<<<p :: binary, ?/>> :: binary, ?h>>)
      ?I ->
        protocol_version(r, acc,
                           <<<<p :: binary, ?/>> :: binary, ?i>>)
      ?J ->
        protocol_version(r, acc,
                           <<<<p :: binary, ?/>> :: binary, ?j>>)
      ?K ->
        protocol_version(r, acc,
                           <<<<p :: binary, ?/>> :: binary, ?k>>)
      ?L ->
        protocol_version(r, acc,
                           <<<<p :: binary, ?/>> :: binary, ?l>>)
      ?M ->
        protocol_version(r, acc,
                           <<<<p :: binary, ?/>> :: binary, ?m>>)
      ?N ->
        protocol_version(r, acc,
                           <<<<p :: binary, ?/>> :: binary, ?n>>)
      ?O ->
        protocol_version(r, acc,
                           <<<<p :: binary, ?/>> :: binary, ?o>>)
      ?P ->
        protocol_version(r, acc,
                           <<<<p :: binary, ?/>> :: binary, ?p>>)
      ?Q ->
        protocol_version(r, acc,
                           <<<<p :: binary, ?/>> :: binary, ?q>>)
      ?R ->
        protocol_version(r, acc,
                           <<<<p :: binary, ?/>> :: binary, ?r>>)
      ?S ->
        protocol_version(r, acc,
                           <<<<p :: binary, ?/>> :: binary, ?s>>)
      ?T ->
        protocol_version(r, acc,
                           <<<<p :: binary, ?/>> :: binary, ?t>>)
      ?U ->
        protocol_version(r, acc,
                           <<<<p :: binary, ?/>> :: binary, ?u>>)
      ?V ->
        protocol_version(r, acc,
                           <<<<p :: binary, ?/>> :: binary, ?v>>)
      ?W ->
        protocol_version(r, acc,
                           <<<<p :: binary, ?/>> :: binary, ?w>>)
      ?X ->
        protocol_version(r, acc,
                           <<<<p :: binary, ?/>> :: binary, ?x>>)
      ?Y ->
        protocol_version(r, acc,
                           <<<<p :: binary, ?/>> :: binary, ?y>>)
      ?Z ->
        protocol_version(r, acc,
                           <<<<p :: binary, ?/>> :: binary, ?z>>)
      ^c ->
        protocol_version(r, acc,
                           <<<<p :: binary, ?/>> :: binary, c>>)
    end
  end

  defp protocol_name(<<c, r :: bits>>, acc, p)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9) or c === ?!) or c === ?#) or c === ?$) or c === ?%) or c === ?&) or c === ?') or c === ?*) or c === ?+) or c === ?-) or c === ?.) or c === ?^) or c === ?_) or c === ?`) or c === ?|) or c === ?~ do
    case (c) do
      ?A ->
        protocol_name(r, acc, <<p :: binary, ?a>>)
      ?B ->
        protocol_name(r, acc, <<p :: binary, ?b>>)
      ?C ->
        protocol_name(r, acc, <<p :: binary, ?c>>)
      ?D ->
        protocol_name(r, acc, <<p :: binary, ?d>>)
      ?E ->
        protocol_name(r, acc, <<p :: binary, ?e>>)
      ?F ->
        protocol_name(r, acc, <<p :: binary, ?f>>)
      ?G ->
        protocol_name(r, acc, <<p :: binary, ?g>>)
      ?H ->
        protocol_name(r, acc, <<p :: binary, ?h>>)
      ?I ->
        protocol_name(r, acc, <<p :: binary, ?i>>)
      ?J ->
        protocol_name(r, acc, <<p :: binary, ?j>>)
      ?K ->
        protocol_name(r, acc, <<p :: binary, ?k>>)
      ?L ->
        protocol_name(r, acc, <<p :: binary, ?l>>)
      ?M ->
        protocol_name(r, acc, <<p :: binary, ?m>>)
      ?N ->
        protocol_name(r, acc, <<p :: binary, ?n>>)
      ?O ->
        protocol_name(r, acc, <<p :: binary, ?o>>)
      ?P ->
        protocol_name(r, acc, <<p :: binary, ?p>>)
      ?Q ->
        protocol_name(r, acc, <<p :: binary, ?q>>)
      ?R ->
        protocol_name(r, acc, <<p :: binary, ?r>>)
      ?S ->
        protocol_name(r, acc, <<p :: binary, ?s>>)
      ?T ->
        protocol_name(r, acc, <<p :: binary, ?t>>)
      ?U ->
        protocol_name(r, acc, <<p :: binary, ?u>>)
      ?V ->
        protocol_name(r, acc, <<p :: binary, ?v>>)
      ?W ->
        protocol_name(r, acc, <<p :: binary, ?w>>)
      ?X ->
        protocol_name(r, acc, <<p :: binary, ?x>>)
      ?Y ->
        protocol_name(r, acc, <<p :: binary, ?y>>)
      ?Z ->
        protocol_name(r, acc, <<p :: binary, ?z>>)
      ^c ->
        protocol_name(r, acc, <<p :: binary, c>>)
    end
  end

  defp protocol_name(r, acc, p) do
    protocol_list_sep(r, [p | acc])
  end

  defp protocol_version(<<c, r :: bits>>, acc, p)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9) or c === ?!) or c === ?#) or c === ?$) or c === ?%) or c === ?&) or c === ?') or c === ?*) or c === ?+) or c === ?-) or c === ?.) or c === ?^) or c === ?_) or c === ?`) or c === ?|) or c === ?~ do
    case (c) do
      ?A ->
        protocol_version(r, acc, <<p :: binary, ?a>>)
      ?B ->
        protocol_version(r, acc, <<p :: binary, ?b>>)
      ?C ->
        protocol_version(r, acc, <<p :: binary, ?c>>)
      ?D ->
        protocol_version(r, acc, <<p :: binary, ?d>>)
      ?E ->
        protocol_version(r, acc, <<p :: binary, ?e>>)
      ?F ->
        protocol_version(r, acc, <<p :: binary, ?f>>)
      ?G ->
        protocol_version(r, acc, <<p :: binary, ?g>>)
      ?H ->
        protocol_version(r, acc, <<p :: binary, ?h>>)
      ?I ->
        protocol_version(r, acc, <<p :: binary, ?i>>)
      ?J ->
        protocol_version(r, acc, <<p :: binary, ?j>>)
      ?K ->
        protocol_version(r, acc, <<p :: binary, ?k>>)
      ?L ->
        protocol_version(r, acc, <<p :: binary, ?l>>)
      ?M ->
        protocol_version(r, acc, <<p :: binary, ?m>>)
      ?N ->
        protocol_version(r, acc, <<p :: binary, ?n>>)
      ?O ->
        protocol_version(r, acc, <<p :: binary, ?o>>)
      ?P ->
        protocol_version(r, acc, <<p :: binary, ?p>>)
      ?Q ->
        protocol_version(r, acc, <<p :: binary, ?q>>)
      ?R ->
        protocol_version(r, acc, <<p :: binary, ?r>>)
      ?S ->
        protocol_version(r, acc, <<p :: binary, ?s>>)
      ?T ->
        protocol_version(r, acc, <<p :: binary, ?t>>)
      ?U ->
        protocol_version(r, acc, <<p :: binary, ?u>>)
      ?V ->
        protocol_version(r, acc, <<p :: binary, ?v>>)
      ?W ->
        protocol_version(r, acc, <<p :: binary, ?w>>)
      ?X ->
        protocol_version(r, acc, <<p :: binary, ?x>>)
      ?Y ->
        protocol_version(r, acc, <<p :: binary, ?y>>)
      ?Z ->
        protocol_version(r, acc, <<p :: binary, ?z>>)
      ^c ->
        protocol_version(r, acc, <<p :: binary, c>>)
    end
  end

  defp protocol_version(r, acc, p) do
    protocol_list_sep(r, [p | acc])
  end

  defp protocol_list_sep(<<>>, acc) do
    :lists.reverse(acc)
  end

  defp protocol_list_sep(<<c, r :: bits>>, acc)
      when c === ?\s or c === ?\t do
    protocol_list_sep(r, acc)
  end

  defp protocol_list_sep(<<?,, r :: bits>>, acc) do
    protocol_list(r, acc)
  end

  def parse_variant_key(variantKey, numMembers) do
    list = :cow_http_struct_hd.parse_list(variantKey)
    for inner <- list do
      case (inner) do
        {:with_params, innerList, %{}} ->
          ^numMembers = length(innerList)
          for item <- innerList do
            case (item) do
              {:with_params, {:token, value}, %{}} ->
                value
              {:with_params, {:string, value}, %{}} ->
                value
            end
          end
      end
    end
  end

  def variant_key(variantKeys) do
    :cow_http_struct_hd.list(for innerList <- variantKeys do
                               {:with_params,
                                  for value <- innerList do
                                    {:with_params, {:string, value}, %{}}
                                  end,
                                  %{}}
                             end)
  end

  def parse_variants(variants) do
    {dict0,
       order} = :cow_http_struct_hd.parse_dictionary(variants)
    dict = :maps.map(fn _, {:with_params, list, %{}} ->
                          for item <- list do
                            case (item) do
                              {:with_params, {:token, value}, %{}} ->
                                value
                              {:with_params, {:string, value}, %{}} ->
                                value
                            end
                          end
                     end,
                       dict0)
    for key <- order do
      {key, :maps.get(key, dict)}
    end
  end

  def variants(variants) do
    :cow_http_struct_hd.dictionary(for {key,
                                          list} <- variants do
                                     {key,
                                        {:with_params,
                                           for value <- list do
                                             {:with_params, {:string, value},
                                                %{}}
                                           end,
                                           %{}}}
                                   end)
  end

  def parse_vary("*") do
    :"*"
  end

  def parse_vary(vary) do
    nonempty(token_ci_list(vary, []))
  end

  def parse_www_authenticate(authenticate) do
    nonempty(www_auth_list(authenticate, []))
  end

  defp www_auth_list(<<>>, acc) do
    :lists.reverse(acc)
  end

  defp www_auth_list(<<c, r :: bits>>, acc)
      when (c === ?\s or c === ?\t) or c === ?, do
    www_auth_list(r, acc)
  end

  defp www_auth_list(<<c, r :: bits>>, acc)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9) or c === ?!) or c === ?#) or c === ?$) or c === ?%) or c === ?&) or c === ?') or c === ?*) or c === ?+) or c === ?-) or c === ?.) or c === ?^) or c === ?_) or c === ?`) or c === ?|) or c === ?~ do
    case (c) do
      ?A ->
        www_auth_scheme(r, acc, <<<<>> :: binary, ?a>>)
      ?B ->
        www_auth_scheme(r, acc, <<<<>> :: binary, ?b>>)
      ?C ->
        www_auth_scheme(r, acc, <<<<>> :: binary, ?c>>)
      ?D ->
        www_auth_scheme(r, acc, <<<<>> :: binary, ?d>>)
      ?E ->
        www_auth_scheme(r, acc, <<<<>> :: binary, ?e>>)
      ?F ->
        www_auth_scheme(r, acc, <<<<>> :: binary, ?f>>)
      ?G ->
        www_auth_scheme(r, acc, <<<<>> :: binary, ?g>>)
      ?H ->
        www_auth_scheme(r, acc, <<<<>> :: binary, ?h>>)
      ?I ->
        www_auth_scheme(r, acc, <<<<>> :: binary, ?i>>)
      ?J ->
        www_auth_scheme(r, acc, <<<<>> :: binary, ?j>>)
      ?K ->
        www_auth_scheme(r, acc, <<<<>> :: binary, ?k>>)
      ?L ->
        www_auth_scheme(r, acc, <<<<>> :: binary, ?l>>)
      ?M ->
        www_auth_scheme(r, acc, <<<<>> :: binary, ?m>>)
      ?N ->
        www_auth_scheme(r, acc, <<<<>> :: binary, ?n>>)
      ?O ->
        www_auth_scheme(r, acc, <<<<>> :: binary, ?o>>)
      ?P ->
        www_auth_scheme(r, acc, <<<<>> :: binary, ?p>>)
      ?Q ->
        www_auth_scheme(r, acc, <<<<>> :: binary, ?q>>)
      ?R ->
        www_auth_scheme(r, acc, <<<<>> :: binary, ?r>>)
      ?S ->
        www_auth_scheme(r, acc, <<<<>> :: binary, ?s>>)
      ?T ->
        www_auth_scheme(r, acc, <<<<>> :: binary, ?t>>)
      ?U ->
        www_auth_scheme(r, acc, <<<<>> :: binary, ?u>>)
      ?V ->
        www_auth_scheme(r, acc, <<<<>> :: binary, ?v>>)
      ?W ->
        www_auth_scheme(r, acc, <<<<>> :: binary, ?w>>)
      ?X ->
        www_auth_scheme(r, acc, <<<<>> :: binary, ?x>>)
      ?Y ->
        www_auth_scheme(r, acc, <<<<>> :: binary, ?y>>)
      ?Z ->
        www_auth_scheme(r, acc, <<<<>> :: binary, ?z>>)
      ^c ->
        www_auth_scheme(r, acc, <<<<>> :: binary, c>>)
    end
  end

  defp www_auth_basic_before_realm(<<c, r :: bits>>, acc)
      when c === ?\s or c === ?\t do
    www_auth_basic_before_realm(r, acc)
  end

  defp www_auth_basic_before_realm(<<"realm=\"", r :: bits>>, acc) do
    www_auth_basic(r, acc, <<>>)
  end

  defp www_auth_basic(<<?", r :: bits>>, acc, realm) do
    www_auth_list_sep(r, [{:basic, realm} | acc])
  end

  defp www_auth_basic(<<?\\, c, r :: bits>>, acc, realm)
      when c === ?\t or (c > 31 and c !== 127) do
    www_auth_basic(r, acc, <<realm :: binary, c>>)
  end

  defp www_auth_basic(<<c, r :: bits>>, acc, realm) when c === ?\t or
                                               (c > 31 and c !== 127) do
    www_auth_basic(r, acc, <<realm :: binary, c>>)
  end

  defp www_auth_scheme(<<c, r :: bits>>, acc, scheme)
      when c === ?\s or c === ?\t do
    case (scheme) do
      "basic" ->
        www_auth_basic_before_realm(r, acc)
      "bearer" ->
        www_auth_params_list(r, acc, :bearer, [])
      "digest" ->
        www_auth_params_list(r, acc, :digest, [])
      _ ->
        www_auth_params_list(r, acc, scheme, [])
    end
  end

  defp www_auth_scheme(<<c, r :: bits>>, acc, scheme)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9) or c === ?!) or c === ?#) or c === ?$) or c === ?%) or c === ?&) or c === ?') or c === ?*) or c === ?+) or c === ?-) or c === ?.) or c === ?^) or c === ?_) or c === ?`) or c === ?|) or c === ?~ do
    case (c) do
      ?A ->
        www_auth_scheme(r, acc, <<scheme :: binary, ?a>>)
      ?B ->
        www_auth_scheme(r, acc, <<scheme :: binary, ?b>>)
      ?C ->
        www_auth_scheme(r, acc, <<scheme :: binary, ?c>>)
      ?D ->
        www_auth_scheme(r, acc, <<scheme :: binary, ?d>>)
      ?E ->
        www_auth_scheme(r, acc, <<scheme :: binary, ?e>>)
      ?F ->
        www_auth_scheme(r, acc, <<scheme :: binary, ?f>>)
      ?G ->
        www_auth_scheme(r, acc, <<scheme :: binary, ?g>>)
      ?H ->
        www_auth_scheme(r, acc, <<scheme :: binary, ?h>>)
      ?I ->
        www_auth_scheme(r, acc, <<scheme :: binary, ?i>>)
      ?J ->
        www_auth_scheme(r, acc, <<scheme :: binary, ?j>>)
      ?K ->
        www_auth_scheme(r, acc, <<scheme :: binary, ?k>>)
      ?L ->
        www_auth_scheme(r, acc, <<scheme :: binary, ?l>>)
      ?M ->
        www_auth_scheme(r, acc, <<scheme :: binary, ?m>>)
      ?N ->
        www_auth_scheme(r, acc, <<scheme :: binary, ?n>>)
      ?O ->
        www_auth_scheme(r, acc, <<scheme :: binary, ?o>>)
      ?P ->
        www_auth_scheme(r, acc, <<scheme :: binary, ?p>>)
      ?Q ->
        www_auth_scheme(r, acc, <<scheme :: binary, ?q>>)
      ?R ->
        www_auth_scheme(r, acc, <<scheme :: binary, ?r>>)
      ?S ->
        www_auth_scheme(r, acc, <<scheme :: binary, ?s>>)
      ?T ->
        www_auth_scheme(r, acc, <<scheme :: binary, ?t>>)
      ?U ->
        www_auth_scheme(r, acc, <<scheme :: binary, ?u>>)
      ?V ->
        www_auth_scheme(r, acc, <<scheme :: binary, ?v>>)
      ?W ->
        www_auth_scheme(r, acc, <<scheme :: binary, ?w>>)
      ?X ->
        www_auth_scheme(r, acc, <<scheme :: binary, ?x>>)
      ?Y ->
        www_auth_scheme(r, acc, <<scheme :: binary, ?y>>)
      ?Z ->
        www_auth_scheme(r, acc, <<scheme :: binary, ?z>>)
      ^c ->
        www_auth_scheme(r, acc, <<scheme :: binary, c>>)
    end
  end

  defp www_auth_list_sep(<<>>, acc) do
    :lists.reverse(acc)
  end

  defp www_auth_list_sep(<<c, r :: bits>>, acc)
      when c === ?\s or c === ?\t do
    www_auth_list_sep(r, acc)
  end

  defp www_auth_list_sep(<<?,, r :: bits>>, acc) do
    www_auth_list(r, acc)
  end

  defp www_auth_params_list(<<>>, acc, scheme, params) do
    :lists.reverse([{scheme,
                       :lists.reverse(nonempty(params))} |
                        acc])
  end

  defp www_auth_params_list(<<c, r :: bits>>, acc, scheme, params)
      when (c === ?\s or c === ?\t) or c === ?, do
    www_auth_params_list(r, acc, scheme, params)
  end

  defp www_auth_params_list(<<"algorithm=", c, r :: bits>>, acc, scheme, params)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9) or c === ?!) or c === ?#) or c === ?$) or c === ?%) or c === ?&) or c === ?') or c === ?*) or c === ?+) or c === ?-) or c === ?.) or c === ?^) or c === ?_) or c === ?`) or c === ?|) or c === ?~ do
    www_auth_token(r, acc, scheme, params, "algorithm", <<c>>)
  end

  defp www_auth_params_list(<<"domain=\"", r :: bits>>, acc, scheme, params) do
    www_auth_quoted(r, acc, scheme, params, "domain", <<>>)
  end

  defp www_auth_params_list(<<"error=\"", r :: bits>>, acc, scheme, params) do
    www_auth_quoted(r, acc, scheme, params, "error", <<>>)
  end

  defp www_auth_params_list(<<"error_description=\"", r :: bits>>, acc, scheme, params) do
    www_auth_quoted(r, acc, scheme, params, "error_description", <<>>)
  end

  defp www_auth_params_list(<<"error_uri=\"", r :: bits>>, acc, scheme, params) do
    www_auth_quoted(r, acc, scheme, params, "error_uri", <<>>)
  end

  defp www_auth_params_list(<<"nonce=\"", r :: bits>>, acc, scheme, params) do
    www_auth_quoted(r, acc, scheme, params, "nonce", <<>>)
  end

  defp www_auth_params_list(<<"opaque=\"", r :: bits>>, acc, scheme, params) do
    www_auth_quoted(r, acc, scheme, params, "opaque", <<>>)
  end

  defp www_auth_params_list(<<"qop=\"", r :: bits>>, acc, scheme, params) do
    www_auth_quoted(r, acc, scheme, params, "qop", <<>>)
  end

  defp www_auth_params_list(<<"realm=\"", r :: bits>>, acc, scheme, params) do
    www_auth_quoted(r, acc, scheme, params, "realm", <<>>)
  end

  defp www_auth_params_list(<<"scope=\"", r :: bits>>, acc, scheme, params) do
    www_auth_quoted(r, acc, scheme, params, "scope", <<>>)
  end

  defp www_auth_params_list(<<"stale=false", r :: bits>>, acc, scheme, params) do
    www_auth_params_list_sep(r, acc, scheme,
                               [{"stale", "false"} | params])
  end

  defp www_auth_params_list(<<"stale=true", r :: bits>>, acc, scheme, params) do
    www_auth_params_list_sep(r, acc, scheme,
                               [{"stale", "true"} | params])
  end

  defp www_auth_params_list(<<c, r :: bits>>, acc, scheme, params)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9) or c === ?!) or c === ?#) or c === ?$) or c === ?%) or c === ?&) or c === ?') or c === ?*) or c === ?+) or c === ?-) or c === ?.) or c === ?^) or c === ?_) or c === ?`) or c === ?|) or c === ?~ do
    case (c) do
      ?A ->
        www_auth_param(r, acc, scheme, params,
                         <<<<>> :: binary, ?a>>)
      ?B ->
        www_auth_param(r, acc, scheme, params,
                         <<<<>> :: binary, ?b>>)
      ?C ->
        www_auth_param(r, acc, scheme, params,
                         <<<<>> :: binary, ?c>>)
      ?D ->
        www_auth_param(r, acc, scheme, params,
                         <<<<>> :: binary, ?d>>)
      ?E ->
        www_auth_param(r, acc, scheme, params,
                         <<<<>> :: binary, ?e>>)
      ?F ->
        www_auth_param(r, acc, scheme, params,
                         <<<<>> :: binary, ?f>>)
      ?G ->
        www_auth_param(r, acc, scheme, params,
                         <<<<>> :: binary, ?g>>)
      ?H ->
        www_auth_param(r, acc, scheme, params,
                         <<<<>> :: binary, ?h>>)
      ?I ->
        www_auth_param(r, acc, scheme, params,
                         <<<<>> :: binary, ?i>>)
      ?J ->
        www_auth_param(r, acc, scheme, params,
                         <<<<>> :: binary, ?j>>)
      ?K ->
        www_auth_param(r, acc, scheme, params,
                         <<<<>> :: binary, ?k>>)
      ?L ->
        www_auth_param(r, acc, scheme, params,
                         <<<<>> :: binary, ?l>>)
      ?M ->
        www_auth_param(r, acc, scheme, params,
                         <<<<>> :: binary, ?m>>)
      ?N ->
        www_auth_param(r, acc, scheme, params,
                         <<<<>> :: binary, ?n>>)
      ?O ->
        www_auth_param(r, acc, scheme, params,
                         <<<<>> :: binary, ?o>>)
      ?P ->
        www_auth_param(r, acc, scheme, params,
                         <<<<>> :: binary, ?p>>)
      ?Q ->
        www_auth_param(r, acc, scheme, params,
                         <<<<>> :: binary, ?q>>)
      ?R ->
        www_auth_param(r, acc, scheme, params,
                         <<<<>> :: binary, ?r>>)
      ?S ->
        www_auth_param(r, acc, scheme, params,
                         <<<<>> :: binary, ?s>>)
      ?T ->
        www_auth_param(r, acc, scheme, params,
                         <<<<>> :: binary, ?t>>)
      ?U ->
        www_auth_param(r, acc, scheme, params,
                         <<<<>> :: binary, ?u>>)
      ?V ->
        www_auth_param(r, acc, scheme, params,
                         <<<<>> :: binary, ?v>>)
      ?W ->
        www_auth_param(r, acc, scheme, params,
                         <<<<>> :: binary, ?w>>)
      ?X ->
        www_auth_param(r, acc, scheme, params,
                         <<<<>> :: binary, ?x>>)
      ?Y ->
        www_auth_param(r, acc, scheme, params,
                         <<<<>> :: binary, ?y>>)
      ?Z ->
        www_auth_param(r, acc, scheme, params,
                         <<<<>> :: binary, ?z>>)
      ^c ->
        www_auth_param(r, acc, scheme, params,
                         <<<<>> :: binary, c>>)
    end
  end

  defp www_auth_param(<<?=, ?", r :: bits>>, acc, scheme, params,
            k) do
    www_auth_quoted(r, acc, scheme, params, k, <<>>)
  end

  defp www_auth_param(<<?=, c, r :: bits>>, acc, scheme, params, k)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9) or c === ?!) or c === ?#) or c === ?$) or c === ?%) or c === ?&) or c === ?') or c === ?*) or c === ?+) or c === ?-) or c === ?.) or c === ?^) or c === ?_) or c === ?`) or c === ?|) or c === ?~ do
    www_auth_token(r, acc, scheme, params, k, <<c>>)
  end

  defp www_auth_param(<<c, r :: bits>>, acc, scheme, params, k)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9) or c === ?!) or c === ?#) or c === ?$) or c === ?%) or c === ?&) or c === ?') or c === ?*) or c === ?+) or c === ?-) or c === ?.) or c === ?^) or c === ?_) or c === ?`) or c === ?|) or c === ?~ do
    case (c) do
      ?A ->
        www_auth_param(r, acc, scheme, params,
                         <<k :: binary, ?a>>)
      ?B ->
        www_auth_param(r, acc, scheme, params,
                         <<k :: binary, ?b>>)
      ?C ->
        www_auth_param(r, acc, scheme, params,
                         <<k :: binary, ?c>>)
      ?D ->
        www_auth_param(r, acc, scheme, params,
                         <<k :: binary, ?d>>)
      ?E ->
        www_auth_param(r, acc, scheme, params,
                         <<k :: binary, ?e>>)
      ?F ->
        www_auth_param(r, acc, scheme, params,
                         <<k :: binary, ?f>>)
      ?G ->
        www_auth_param(r, acc, scheme, params,
                         <<k :: binary, ?g>>)
      ?H ->
        www_auth_param(r, acc, scheme, params,
                         <<k :: binary, ?h>>)
      ?I ->
        www_auth_param(r, acc, scheme, params,
                         <<k :: binary, ?i>>)
      ?J ->
        www_auth_param(r, acc, scheme, params,
                         <<k :: binary, ?j>>)
      ?K ->
        www_auth_param(r, acc, scheme, params,
                         <<k :: binary, ?k>>)
      ?L ->
        www_auth_param(r, acc, scheme, params,
                         <<k :: binary, ?l>>)
      ?M ->
        www_auth_param(r, acc, scheme, params,
                         <<k :: binary, ?m>>)
      ?N ->
        www_auth_param(r, acc, scheme, params,
                         <<k :: binary, ?n>>)
      ?O ->
        www_auth_param(r, acc, scheme, params,
                         <<k :: binary, ?o>>)
      ?P ->
        www_auth_param(r, acc, scheme, params,
                         <<k :: binary, ?p>>)
      ?Q ->
        www_auth_param(r, acc, scheme, params,
                         <<k :: binary, ?q>>)
      ?R ->
        www_auth_param(r, acc, scheme, params,
                         <<k :: binary, ?r>>)
      ?S ->
        www_auth_param(r, acc, scheme, params,
                         <<k :: binary, ?s>>)
      ?T ->
        www_auth_param(r, acc, scheme, params,
                         <<k :: binary, ?t>>)
      ?U ->
        www_auth_param(r, acc, scheme, params,
                         <<k :: binary, ?u>>)
      ?V ->
        www_auth_param(r, acc, scheme, params,
                         <<k :: binary, ?v>>)
      ?W ->
        www_auth_param(r, acc, scheme, params,
                         <<k :: binary, ?w>>)
      ?X ->
        www_auth_param(r, acc, scheme, params,
                         <<k :: binary, ?x>>)
      ?Y ->
        www_auth_param(r, acc, scheme, params,
                         <<k :: binary, ?y>>)
      ?Z ->
        www_auth_param(r, acc, scheme, params,
                         <<k :: binary, ?z>>)
      ^c ->
        www_auth_param(r, acc, scheme, params,
                         <<k :: binary, c>>)
    end
  end

  defp www_auth_param(r, acc, scheme, params, newScheme) do
    www_auth_scheme(r,
                      [{scheme, :lists.reverse(params)} | acc], newScheme)
  end

  defp www_auth_token(<<c, r :: bits>>, acc, scheme, params, k, v)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9) or c === ?!) or c === ?#) or c === ?$) or c === ?%) or c === ?&) or c === ?') or c === ?*) or c === ?+) or c === ?-) or c === ?.) or c === ?^) or c === ?_) or c === ?`) or c === ?|) or c === ?~ do
    www_auth_token(r, acc, scheme, params, k,
                     <<v :: binary, c>>)
  end

  defp www_auth_token(r, acc, scheme, params, k, v) do
    www_auth_params_list_sep(r, acc, scheme,
                               [{k, v} | params])
  end

  defp www_auth_quoted(<<?", r :: bits>>, acc, scheme, params, k, v) do
    www_auth_params_list_sep(r, acc, scheme,
                               [{k, v} | params])
  end

  defp www_auth_quoted(<<?\\, c, r :: bits>>, acc, scheme, params, k,
            v)
      when c === ?\t or (c > 31 and c !== 127) do
    www_auth_quoted(r, acc, scheme, params, k,
                      <<v :: binary, c>>)
  end

  defp www_auth_quoted(<<c, r :: bits>>, acc, scheme, params, k, v)
      when c === ?\t or (c > 31 and c !== 127) do
    www_auth_quoted(r, acc, scheme, params, k,
                      <<v :: binary, c>>)
  end

  defp www_auth_params_list_sep(<<>>, acc, scheme, params) do
    :lists.reverse([{scheme, :lists.reverse(params)} | acc])
  end

  defp www_auth_params_list_sep(<<c, r :: bits>>, acc, scheme, params)
      when c === ?\s or c === ?\t do
    www_auth_params_list_sep(r, acc, scheme, params)
  end

  defp www_auth_params_list_sep(<<?,, r :: bits>>, acc, scheme, params) do
    www_auth_params_list_after_sep(r, acc, scheme, params)
  end

  defp www_auth_params_list_after_sep(<<>>, acc, scheme, params) do
    :lists.reverse([{scheme, :lists.reverse(params)} | acc])
  end

  defp www_auth_params_list_after_sep(<<c, r :: bits>>, acc, scheme, params)
      when (c === ?\s or c === ?\t) or c === ?, do
    www_auth_params_list_after_sep(r, acc, scheme, params)
  end

  defp www_auth_params_list_after_sep(r, acc, scheme, params) do
    www_auth_params_list(r, acc, scheme, params)
  end

  def parse_x_forwarded_for(xForwardedFor) do
    nonempty(nodeid_list(xForwardedFor, []))
  end

  defp nodeid_list(<<>>, acc) do
    :lists.reverse(acc)
  end

  defp nodeid_list(<<c, r :: bits>>, acc)
      when (c === ?\s or c === ?\t) or c === ?, do
    nodeid_list(r, acc)
  end

  defp nodeid_list(<<c, r :: bits>>, acc)
      when ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9) or c === ?:) or c === ?.) or c === ?_) or c === ?-) or c === ?[) or c === ?] do
    nodeid(r, acc, <<c>>)
  end

  defp nodeid(<<c, r :: bits>>, acc, t)
      when ((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9) or c === ?:) or c === ?.) or c === ?_) or c === ?-) or c === ?[) or c === ?] do
    nodeid(r, acc, <<t :: binary, c>>)
  end

  defp nodeid(r, acc, t) do
    nodeid_list_sep(r, [t | acc])
  end

  defp nodeid_list_sep(<<>>, acc) do
    :lists.reverse(acc)
  end

  defp nodeid_list_sep(<<c, r :: bits>>, acc)
      when c === ?\s or c === ?\t do
    nodeid_list_sep(r, acc)
  end

  defp nodeid_list_sep(<<?,, r :: bits>>, acc) do
    nodeid_list(r, acc)
  end

  defp nonempty(l) when l !== [] do
    l
  end

  defp token_list(<<>>, acc) do
    :lists.reverse(acc)
  end

  defp token_list(<<c, r :: bits>>, acc)
      when (c === ?\s or c === ?\t) or c === ?, do
    token_list(r, acc)
  end

  defp token_list(<<c, r :: bits>>, acc)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9) or c === ?!) or c === ?#) or c === ?$) or c === ?%) or c === ?&) or c === ?') or c === ?*) or c === ?+) or c === ?-) or c === ?.) or c === ?^) or c === ?_) or c === ?`) or c === ?|) or c === ?~ do
    token(r, acc, <<c>>)
  end

  defp token(<<c, r :: bits>>, acc, t)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9) or c === ?!) or c === ?#) or c === ?$) or c === ?%) or c === ?&) or c === ?') or c === ?*) or c === ?+) or c === ?-) or c === ?.) or c === ?^) or c === ?_) or c === ?`) or c === ?|) or c === ?~ do
    token(r, acc, <<t :: binary, c>>)
  end

  defp token(r, acc, t) do
    token_list_sep(r, [t | acc])
  end

  defp token_list_sep(<<>>, acc) do
    :lists.reverse(acc)
  end

  defp token_list_sep(<<c, r :: bits>>, acc)
      when c === ?\s or c === ?\t do
    token_list_sep(r, acc)
  end

  defp token_list_sep(<<?,, r :: bits>>, acc) do
    token_list(r, acc)
  end

  defp token_ci_list(<<>>, acc) do
    :lists.reverse(acc)
  end

  defp token_ci_list(<<c, r :: bits>>, acc)
      when (c === ?\s or c === ?\t) or c === ?, do
    token_ci_list(r, acc)
  end

  defp token_ci_list(<<c, r :: bits>>, acc)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9) or c === ?!) or c === ?#) or c === ?$) or c === ?%) or c === ?&) or c === ?') or c === ?*) or c === ?+) or c === ?-) or c === ?.) or c === ?^) or c === ?_) or c === ?`) or c === ?|) or c === ?~ do
    case (c) do
      ?A ->
        token_ci(r, acc, <<<<>> :: binary, ?a>>)
      ?B ->
        token_ci(r, acc, <<<<>> :: binary, ?b>>)
      ?C ->
        token_ci(r, acc, <<<<>> :: binary, ?c>>)
      ?D ->
        token_ci(r, acc, <<<<>> :: binary, ?d>>)
      ?E ->
        token_ci(r, acc, <<<<>> :: binary, ?e>>)
      ?F ->
        token_ci(r, acc, <<<<>> :: binary, ?f>>)
      ?G ->
        token_ci(r, acc, <<<<>> :: binary, ?g>>)
      ?H ->
        token_ci(r, acc, <<<<>> :: binary, ?h>>)
      ?I ->
        token_ci(r, acc, <<<<>> :: binary, ?i>>)
      ?J ->
        token_ci(r, acc, <<<<>> :: binary, ?j>>)
      ?K ->
        token_ci(r, acc, <<<<>> :: binary, ?k>>)
      ?L ->
        token_ci(r, acc, <<<<>> :: binary, ?l>>)
      ?M ->
        token_ci(r, acc, <<<<>> :: binary, ?m>>)
      ?N ->
        token_ci(r, acc, <<<<>> :: binary, ?n>>)
      ?O ->
        token_ci(r, acc, <<<<>> :: binary, ?o>>)
      ?P ->
        token_ci(r, acc, <<<<>> :: binary, ?p>>)
      ?Q ->
        token_ci(r, acc, <<<<>> :: binary, ?q>>)
      ?R ->
        token_ci(r, acc, <<<<>> :: binary, ?r>>)
      ?S ->
        token_ci(r, acc, <<<<>> :: binary, ?s>>)
      ?T ->
        token_ci(r, acc, <<<<>> :: binary, ?t>>)
      ?U ->
        token_ci(r, acc, <<<<>> :: binary, ?u>>)
      ?V ->
        token_ci(r, acc, <<<<>> :: binary, ?v>>)
      ?W ->
        token_ci(r, acc, <<<<>> :: binary, ?w>>)
      ?X ->
        token_ci(r, acc, <<<<>> :: binary, ?x>>)
      ?Y ->
        token_ci(r, acc, <<<<>> :: binary, ?y>>)
      ?Z ->
        token_ci(r, acc, <<<<>> :: binary, ?z>>)
      ^c ->
        token_ci(r, acc, <<<<>> :: binary, c>>)
    end
  end

  defp token_ci(<<c, r :: bits>>, acc, t)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9) or c === ?!) or c === ?#) or c === ?$) or c === ?%) or c === ?&) or c === ?') or c === ?*) or c === ?+) or c === ?-) or c === ?.) or c === ?^) or c === ?_) or c === ?`) or c === ?|) or c === ?~ do
    case (c) do
      ?A ->
        token_ci(r, acc, <<t :: binary, ?a>>)
      ?B ->
        token_ci(r, acc, <<t :: binary, ?b>>)
      ?C ->
        token_ci(r, acc, <<t :: binary, ?c>>)
      ?D ->
        token_ci(r, acc, <<t :: binary, ?d>>)
      ?E ->
        token_ci(r, acc, <<t :: binary, ?e>>)
      ?F ->
        token_ci(r, acc, <<t :: binary, ?f>>)
      ?G ->
        token_ci(r, acc, <<t :: binary, ?g>>)
      ?H ->
        token_ci(r, acc, <<t :: binary, ?h>>)
      ?I ->
        token_ci(r, acc, <<t :: binary, ?i>>)
      ?J ->
        token_ci(r, acc, <<t :: binary, ?j>>)
      ?K ->
        token_ci(r, acc, <<t :: binary, ?k>>)
      ?L ->
        token_ci(r, acc, <<t :: binary, ?l>>)
      ?M ->
        token_ci(r, acc, <<t :: binary, ?m>>)
      ?N ->
        token_ci(r, acc, <<t :: binary, ?n>>)
      ?O ->
        token_ci(r, acc, <<t :: binary, ?o>>)
      ?P ->
        token_ci(r, acc, <<t :: binary, ?p>>)
      ?Q ->
        token_ci(r, acc, <<t :: binary, ?q>>)
      ?R ->
        token_ci(r, acc, <<t :: binary, ?r>>)
      ?S ->
        token_ci(r, acc, <<t :: binary, ?s>>)
      ?T ->
        token_ci(r, acc, <<t :: binary, ?t>>)
      ?U ->
        token_ci(r, acc, <<t :: binary, ?u>>)
      ?V ->
        token_ci(r, acc, <<t :: binary, ?v>>)
      ?W ->
        token_ci(r, acc, <<t :: binary, ?w>>)
      ?X ->
        token_ci(r, acc, <<t :: binary, ?x>>)
      ?Y ->
        token_ci(r, acc, <<t :: binary, ?y>>)
      ?Z ->
        token_ci(r, acc, <<t :: binary, ?z>>)
      ^c ->
        token_ci(r, acc, <<t :: binary, c>>)
    end
  end

  defp token_ci(r, acc, t) do
    token_ci_list_sep(r, [t | acc])
  end

  defp token_ci_list_sep(<<>>, acc) do
    :lists.reverse(acc)
  end

  defp token_ci_list_sep(<<c, r :: bits>>, acc)
      when c === ?\s or c === ?\t do
    token_ci_list_sep(r, acc)
  end

  defp token_ci_list_sep(<<?,, r :: bits>>, acc) do
    token_ci_list(r, acc)
  end

  defp join_token_list([]) do
    []
  end

  defp join_token_list([h | t]) do
    join_token_list(t, [h])
  end

  defp join_token_list([], acc) do
    :lists.reverse(acc)
  end

  defp join_token_list([h | t], acc) do
    join_token_list(t, [h, ", " | acc])
  end

end