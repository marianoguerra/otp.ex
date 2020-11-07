defmodule :cow_link do
  use Bitwise
  def parse_link(link) do
    before_target(link, [])
  end

  defp before_target(<<>>, acc) do
    :lists.reverse(acc)
  end

  defp before_target(<<?<, r :: bits>>, acc) do
    target(r, acc, <<>>)
  end

  defp before_target(<<c, r :: bits>>, acc)
      when c === ?\s or c === ?\t do
    before_target(r, acc)
  end

  defp target(<<?>, r :: bits>>, acc, t) do
    param_sep(r, acc, t, [])
  end

  defp target(<<c, r :: bits>>, acc, t) do
    target(r, acc, <<t :: binary, c>>)
  end

  defp param_sep(<<>>, acc, t, p) do
    :lists.reverse(acc_link(acc, t, p))
  end

  defp param_sep(<<?,, r :: bits>>, acc, t, p) do
    before_target(r, acc_link(acc, t, p))
  end

  defp param_sep(<<?;, r :: bits>>, acc, t, p) do
    before_param(r, acc, t, p)
  end

  defp param_sep(<<c, r :: bits>>, acc, t, p)
      when c === ?\s or c === ?\t do
    param_sep(r, acc, t, p)
  end

  defp before_param(<<c, r :: bits>>, acc, t, p)
      when c === ?\s or c === ?\t do
    before_param(r, acc, t, p)
  end

  defp before_param(<<c, r :: bits>>, acc, t, p)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9) or c === ?!) or c === ?#) or c === ?$) or c === ?%) or c === ?&) or c === ?') or c === ?*) or c === ?+) or c === ?-) or c === ?.) or c === ?^) or c === ?_) or c === ?`) or c === ?|) or c === ?~ do
    case (c) do
      ?A ->
        param(r, acc, t, p, <<<<>> :: binary, ?a>>)
      ?B ->
        param(r, acc, t, p, <<<<>> :: binary, ?b>>)
      ?C ->
        param(r, acc, t, p, <<<<>> :: binary, ?c>>)
      ?D ->
        param(r, acc, t, p, <<<<>> :: binary, ?d>>)
      ?E ->
        param(r, acc, t, p, <<<<>> :: binary, ?e>>)
      ?F ->
        param(r, acc, t, p, <<<<>> :: binary, ?f>>)
      ?G ->
        param(r, acc, t, p, <<<<>> :: binary, ?g>>)
      ?H ->
        param(r, acc, t, p, <<<<>> :: binary, ?h>>)
      ?I ->
        param(r, acc, t, p, <<<<>> :: binary, ?i>>)
      ?J ->
        param(r, acc, t, p, <<<<>> :: binary, ?j>>)
      ?K ->
        param(r, acc, t, p, <<<<>> :: binary, ?k>>)
      ?L ->
        param(r, acc, t, p, <<<<>> :: binary, ?l>>)
      ?M ->
        param(r, acc, t, p, <<<<>> :: binary, ?m>>)
      ?N ->
        param(r, acc, t, p, <<<<>> :: binary, ?n>>)
      ?O ->
        param(r, acc, t, p, <<<<>> :: binary, ?o>>)
      ?P ->
        param(r, acc, t, p, <<<<>> :: binary, ?p>>)
      ?Q ->
        param(r, acc, t, p, <<<<>> :: binary, ?q>>)
      ?R ->
        param(r, acc, t, p, <<<<>> :: binary, ?r>>)
      ?S ->
        param(r, acc, t, p, <<<<>> :: binary, ?s>>)
      ?T ->
        param(r, acc, t, p, <<<<>> :: binary, ?t>>)
      ?U ->
        param(r, acc, t, p, <<<<>> :: binary, ?u>>)
      ?V ->
        param(r, acc, t, p, <<<<>> :: binary, ?v>>)
      ?W ->
        param(r, acc, t, p, <<<<>> :: binary, ?w>>)
      ?X ->
        param(r, acc, t, p, <<<<>> :: binary, ?x>>)
      ?Y ->
        param(r, acc, t, p, <<<<>> :: binary, ?y>>)
      ?Z ->
        param(r, acc, t, p, <<<<>> :: binary, ?z>>)
      ^c ->
        param(r, acc, t, p, <<<<>> :: binary, c>>)
    end
  end

  defp param(<<?=, ?", r :: bits>>, acc, t, p, k) do
    quoted(r, acc, t, p, k, <<>>)
  end

  defp param(<<?=, c, r :: bits>>, acc, t, p, k)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9) or c === ?!) or c === ?#) or c === ?$) or c === ?%) or c === ?&) or c === ?') or c === ?*) or c === ?+) or c === ?-) or c === ?.) or c === ?^) or c === ?_) or c === ?`) or c === ?|) or c === ?~ do
    value(r, acc, t, p, k, <<c>>)
  end

  defp param(<<c, r :: bits>>, acc, t, p, k)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9) or c === ?!) or c === ?#) or c === ?$) or c === ?%) or c === ?&) or c === ?') or c === ?*) or c === ?+) or c === ?-) or c === ?.) or c === ?^) or c === ?_) or c === ?`) or c === ?|) or c === ?~ do
    case (c) do
      ?A ->
        param(r, acc, t, p, <<k :: binary, ?a>>)
      ?B ->
        param(r, acc, t, p, <<k :: binary, ?b>>)
      ?C ->
        param(r, acc, t, p, <<k :: binary, ?c>>)
      ?D ->
        param(r, acc, t, p, <<k :: binary, ?d>>)
      ?E ->
        param(r, acc, t, p, <<k :: binary, ?e>>)
      ?F ->
        param(r, acc, t, p, <<k :: binary, ?f>>)
      ?G ->
        param(r, acc, t, p, <<k :: binary, ?g>>)
      ?H ->
        param(r, acc, t, p, <<k :: binary, ?h>>)
      ?I ->
        param(r, acc, t, p, <<k :: binary, ?i>>)
      ?J ->
        param(r, acc, t, p, <<k :: binary, ?j>>)
      ?K ->
        param(r, acc, t, p, <<k :: binary, ?k>>)
      ?L ->
        param(r, acc, t, p, <<k :: binary, ?l>>)
      ?M ->
        param(r, acc, t, p, <<k :: binary, ?m>>)
      ?N ->
        param(r, acc, t, p, <<k :: binary, ?n>>)
      ?O ->
        param(r, acc, t, p, <<k :: binary, ?o>>)
      ?P ->
        param(r, acc, t, p, <<k :: binary, ?p>>)
      ?Q ->
        param(r, acc, t, p, <<k :: binary, ?q>>)
      ?R ->
        param(r, acc, t, p, <<k :: binary, ?r>>)
      ?S ->
        param(r, acc, t, p, <<k :: binary, ?s>>)
      ?T ->
        param(r, acc, t, p, <<k :: binary, ?t>>)
      ?U ->
        param(r, acc, t, p, <<k :: binary, ?u>>)
      ?V ->
        param(r, acc, t, p, <<k :: binary, ?v>>)
      ?W ->
        param(r, acc, t, p, <<k :: binary, ?w>>)
      ?X ->
        param(r, acc, t, p, <<k :: binary, ?x>>)
      ?Y ->
        param(r, acc, t, p, <<k :: binary, ?y>>)
      ?Z ->
        param(r, acc, t, p, <<k :: binary, ?z>>)
      ^c ->
        param(r, acc, t, p, <<k :: binary, c>>)
    end
  end

  defp quoted(<<?", r :: bits>>, acc, t, p, k, v) do
    param_sep(r, acc, t, [{k, v} | p])
  end

  defp quoted(<<?\\, c, r :: bits>>, acc, t, p, k, v)
      when c === ?\t or (c > 31 and c !== 127) do
    quoted(r, acc, t, p, k, <<v :: binary, c>>)
  end

  defp quoted(<<c, r :: bits>>, acc, t, p, k, v)
      when c === ?\t or (c > 31 and c !== 127) do
    quoted(r, acc, t, p, k, <<v :: binary, c>>)
  end

  defp value(<<c, r :: bits>>, acc, t, p, k, v)
      when (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((c === ?a or c === ?b) or c === ?c) or c === ?d) or c === ?e) or c === ?f) or c === ?g) or c === ?h) or c === ?i) or c === ?j) or c === ?k) or c === ?l) or c === ?m) or c === ?n) or c === ?o) or c === ?p) or c === ?q) or c === ?r) or c === ?s) or c === ?t) or c === ?u) or c === ?v) or c === ?w) or c === ?x) or c === ?y) or c === ?z) or c === ?A) or c === ?B) or c === ?C) or c === ?D) or c === ?E) or c === ?F) or c === ?G) or c === ?H) or c === ?I) or c === ?J) or c === ?K) or c === ?L) or c === ?M) or c === ?N) or c === ?O) or c === ?P) or c === ?Q) or c === ?R) or c === ?S) or c === ?T) or c === ?U) or c === ?V) or c === ?W) or c === ?X) or c === ?Y) or c === ?Z) or c === ?0) or c === ?1) or c === ?2) or c === ?3) or c === ?4) or c === ?5) or c === ?6) or c === ?7) or c === ?8) or c === ?9) or c === ?!) or c === ?#) or c === ?$) or c === ?%) or c === ?&) or c === ?') or c === ?*) or c === ?+) or c === ?-) or c === ?.) or c === ?^) or c === ?_) or c === ?`) or c === ?|) or c === ?~ do
    value(r, acc, t, p, k, <<v :: binary, c>>)
  end

  defp value(r, acc, t, p, k, v) do
    param_sep(r, acc, t, [{k, v} | p])
  end

  defp acc_link(acc, target, params0) do
    params1 = :lists.reverse(params0)
    {:value, {_, rel}, params2} = :lists.keytake("rel", 1,
                                                   params1)
    params = filter_out_duplicates(params2, %{})
    [%{:target => target,
         :rel
         =>
         for << <<c>> <- rel >>, into: <<>> do
           <<case (c) do
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
             end>>
         end,
         :attributes => params} |
         acc]
  end

  defp filter_out_duplicates([], _) do
    []
  end

  defp filter_out_duplicates([{"rel", _} | tail], state) do
    filter_out_duplicates(tail, state)
  end

  defp filter_out_duplicates([{"anchor", _} | tail], state = %{:anchor => true}) do
    filter_out_duplicates(tail, state)
  end

  defp filter_out_duplicates([{"media", _} | tail], state = %{:media => true}) do
    filter_out_duplicates(tail, state)
  end

  defp filter_out_duplicates([{"title", _} | tail], state = %{:title => true}) do
    filter_out_duplicates(tail, state)
  end

  defp filter_out_duplicates([{"title*", _} | tail],
            state = %{:title_star => true}) do
    filter_out_duplicates(tail, state)
  end

  defp filter_out_duplicates([{"type", _} | tail], state = %{:type => true}) do
    filter_out_duplicates(tail, state)
  end

  defp filter_out_duplicates([tuple = {"anchor", _} | tail], state) do
    [tuple | filter_out_duplicates(tail,
                                     %{state | :anchor => true})]
  end

  defp filter_out_duplicates([tuple = {"media", _} | tail], state) do
    [tuple | filter_out_duplicates(tail,
                                     %{state | :media => true})]
  end

  defp filter_out_duplicates([tuple = {"title", _} | tail], state) do
    [tuple | filter_out_duplicates(tail,
                                     %{state | :title => true})]
  end

  defp filter_out_duplicates([tuple = {"title*", _} | tail], state) do
    [tuple | filter_out_duplicates(tail,
                                     %{state | :title_star => true})]
  end

  defp filter_out_duplicates([tuple = {"type", _} | tail], state) do
    [tuple | filter_out_duplicates(tail,
                                     %{state | :type => true})]
  end

  defp filter_out_duplicates([tuple | tail], state) do
    [tuple | filter_out_duplicates(tail, state)]
  end

  def resolve_link(link, contextURI) do
    resolve_link(link, contextURI, %{})
  end

  def resolve_link(link = %{:target => targetURI}, :undefined, _) do
    case (:uri_string.parse(targetURI)) do
      uRIMap = %{:scheme => _} ->
        %{link | :target => :uri_string.normalize(uRIMap)}
      _ ->
        false
    end
  end

  def resolve_link(link = %{:attributes => params}, contextURI,
           opts) do
    allowAnchor = :maps.get(:allow_anchor, opts, true)
    case (:lists.keyfind("anchor", 1, params)) do
      false ->
        do_resolve_link(link, contextURI)
      {_, anchor} when allowAnchor ->
        do_resolve_link(link, resolve(anchor, contextURI))
      _ ->
        false
    end
  end

  defp do_resolve_link(link = %{:target => targetURI}, contextURI) do
    %{link
      |
      :target
      =>
      :uri_string.recompose(resolve(targetURI, contextURI))}
  end

  defp resolve(uRI, baseURI) do
    case (resolve1(ensure_map_uri(uRI), baseURI)) do
      targetURI = %{:path => path0} ->
        %{:path => path} = :uri_string.normalize(%{:path
                                                   =>
                                                   path0},
                                                   [:return_map])
        %{targetURI | :path => path}
      targetURI ->
        targetURI
    end
  end

  defp resolve1(uRI = %{:scheme => _}, _) do
    uRI
  end

  defp resolve1(uRI = %{:host => _}, baseURI) do
    %{:scheme => scheme} = ensure_map_uri(baseURI)
    %{uRI | :scheme => scheme}
  end

  defp resolve1(uRI = %{:path => <<>>}, baseURI0) do
    baseURI = ensure_map_uri(baseURI0)
    keys = (case (:maps.is_key(:query, uRI)) do
              true ->
                [:scheme, :host, :port, :path]
              false ->
                [:scheme, :host, :port, :path, :query]
            end)
    :maps.merge(uRI, :maps.with(keys, baseURI))
  end

  defp resolve1(uRI = %{:path => <<"/", _ :: bits>>}, baseURI0) do
    baseURI = ensure_map_uri(baseURI0)
    :maps.merge(uRI,
                  :maps.with([:scheme, :host, :port], baseURI))
  end

  defp resolve1(uRI = %{:path => path}, baseURI0) do
    baseURI = ensure_map_uri(baseURI0)
    :maps.merge(%{uRI
                  |
                  :path => merge_paths(path, baseURI)},
                  :maps.with([:scheme, :host, :port], baseURI))
  end

  defp merge_paths(path, %{:host => _, :path => <<>>}) do
    <<?/, path :: binary>>
  end

  defp merge_paths(path, %{:path => basePath0}) do
    case (:string.split(basePath0, <<?/>>, :trailing)) do
      [basePath, _] ->
        <<basePath :: binary, ?/, path :: binary>>
      [_] ->
        <<?/, path :: binary>>
    end
  end

  defp ensure_map_uri(uRI) when is_map(uRI) do
    uRI
  end

  defp ensure_map_uri(uRI) do
    :uri_string.parse(:erlang.iolist_to_binary(uRI))
  end

  def link(links) do
    :lists.join(", ",
                  for link <- links do
                    do_link(link)
                  end)
  end

  defp do_link(%{:target => targetURI, :rel => rel,
              :attributes => params}) do
    [?<, targetURI, ">; rel=\"", rel, ?",
       for {key, value} <- params do
         ["; ", key, "=\"",
            escape(:erlang.iolist_to_binary(value), <<>>), ?"]
       end]
  end

  defp escape(<<>>, acc) do
    acc
  end

  defp escape(<<?\\, r :: bits>>, acc) do
    escape(r, <<acc :: binary, ?\\, ?\\>>)
  end

  defp escape(<<?", r :: bits>>, acc) do
    escape(r, <<acc :: binary, ?\\, ?">>)
  end

  defp escape(<<c, r :: bits>>, acc) do
    escape(r, <<acc :: binary, c>>)
  end

end