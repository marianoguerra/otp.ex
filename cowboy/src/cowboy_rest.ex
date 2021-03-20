defmodule :cowboy_rest do
  use Bitwise
  @behaviour :cowboy_sub_protocol
  require Record
  Record.defrecord(:r_state, :state, method: :undefined,
                                 handler: :undefined, handler_state: :undefined,
                                 allowed_methods: :undefined,
                                 content_types_p: [],
                                 content_type_a: :undefined, languages_p: [],
                                 language_a: :undefined, charsets_p: :undefined,
                                 charset_a: :undefined, ranges_a: [],
                                 exists: false, etag: :undefined,
                                 last_modified: :undefined, expires: :undefined)
  def upgrade(req0, env, handler, handlerState0) do
    method = :cowboy_req.method(req0)
    case (service_available(req0,
                              r_state(method: method, handler: handler,
                                  handler_state: handlerState0))) do
      {:ok, req, result} ->
        {:ok, req, Map.put(env, :result, result)}
      {mod, req, handlerState} ->
        mod.upgrade(req, env, handler, handlerState)
      {mod, req, handlerState, opts} ->
        mod.upgrade(req, env, handler, handlerState, opts)
    end
  end

  def upgrade(req, env, handler, handlerState, _Opts) do
    upgrade(req, env, handler, handlerState)
  end

  defp service_available(req, state) do
    expect(req, state, :service_available, true,
             &known_methods/2, 503)
  end

  defp known_methods(req, state = r_state(method: method)) do
    case (call(req, state, :known_methods)) do
      :no_call when method === "HEAD" or method === "GET" or
                      method === "POST" or method === "PUT" or method === "PATCH" or
                      method === "DELETE" or method === "OPTIONS"
                    ->
        next(req, state, &uri_too_long/2)
      :no_call ->
        next(req, state, 501)
      {:stop, req2, state2} ->
        terminate(req2, state2)
      {switch, req2, state2} when :erlang.element(1,
                                                    switch) === :switch_handler
                                  ->
        switch_handler(switch, req2, state2)
      {list, req2, state2} ->
        case (:lists.member(method, list)) do
          true ->
            next(req2, state2, &uri_too_long/2)
          false ->
            next(req2, state2, 501)
        end
    end
  end

  defp uri_too_long(req, state) do
    expect(req, state, :uri_too_long, false,
             &allowed_methods/2, 414)
  end

  defp allowed_methods(req, state = r_state(method: method)) do
    case (call(req, state, :allowed_methods)) do
      :no_call when method === "HEAD" or method === "GET" ->
        next(req, state, &malformed_request/2)
      :no_call when method === "OPTIONS" ->
        next(req, r_state(state, allowed_methods: ["HEAD", "GET", "OPTIONS"]),
               &malformed_request/2)
      :no_call ->
        method_not_allowed(req, state, ["HEAD", "GET", "OPTIONS"])
      {:stop, req2, state2} ->
        terminate(req2, state2)
      {switch, req2, state2} when :erlang.element(1,
                                                    switch) === :switch_handler
                                  ->
        switch_handler(switch, req2, state2)
      {list, req2, state2} ->
        case (:lists.member(method, list)) do
          true when method === "OPTIONS" ->
            next(req2, r_state(state2, allowed_methods: list),
                   &malformed_request/2)
          true ->
            next(req2, state2, &malformed_request/2)
          false ->
            method_not_allowed(req2, state2, list)
        end
    end
  end

  defp method_not_allowed(req, state, []) do
    req2 = :cowboy_req.set_resp_header("allow", <<>>, req)
    respond(req2, state, 405)
  end

  defp method_not_allowed(req, state, methods) do
    <<", ",
        allow :: binary>> = (for m <- methods, into: <<>> do
                               <<", ", m :: binary>>
                             end)
    req2 = :cowboy_req.set_resp_header("allow", allow, req)
    respond(req2, state, 405)
  end

  defp malformed_request(req, state) do
    expect(req, state, :malformed_request, false,
             &is_authorized/2, 400)
  end

  defp is_authorized(req, state) do
    case (call(req, state, :is_authorized)) do
      :no_call ->
        forbidden(req, state)
      {:stop, req2, state2} ->
        terminate(req2, state2)
      {switch, req2, state2} when :erlang.element(1,
                                                    switch) === :switch_handler
                                  ->
        switch_handler(switch, req2, state2)
      {true, req2, state2} ->
        forbidden(req2, state2)
      {{false, authHead}, req2, state2} ->
        req3 = :cowboy_req.set_resp_header("www-authenticate", authHead, req2)
        respond(req3, state2, 401)
    end
  end

  defp forbidden(req, state) do
    expect(req, state, :forbidden, false, &rate_limited/2,
             403)
  end

  defp rate_limited(req, state) do
    case (call(req, state, :rate_limited)) do
      :no_call ->
        valid_content_headers(req, state)
      {:stop, req2, state2} ->
        terminate(req2, state2)
      {switch, req2, state2} when :erlang.element(1,
                                                    switch) === :switch_handler
                                  ->
        switch_handler(switch, req2, state2)
      {false, req2, state2} ->
        valid_content_headers(req2, state2)
      {{true, retryAfter0}, req2, state2} ->
        retryAfter = (cond do
                        (is_integer(retryAfter0) and retryAfter0 >= 0) ->
                          :erlang.integer_to_binary(retryAfter0)
                        is_tuple(retryAfter0) ->
                          :cowboy_clock.rfc1123(retryAfter0)
                      end)
        req3 = :cowboy_req.set_resp_header("retry-after", retryAfter, req2)
        respond(req3, state2, 429)
    end
  end

  defp valid_content_headers(req, state) do
    expect(req, state, :valid_content_headers, true,
             &valid_entity_length/2, 501)
  end

  defp valid_entity_length(req, state) do
    expect(req, state, :valid_entity_length, true,
             &options/2, 413)
  end

  defp options(req,
            state = r_state(allowed_methods: methods, method: "OPTIONS")) do
    case (call(req, state, :options)) do
      :no_call when methods === [] ->
        req2 = :cowboy_req.set_resp_header("allow", <<>>, req)
        respond(req2, state, 200)
      :no_call ->
        <<", ",
            allow :: binary>> = (for m <- methods, into: <<>> do
                                   <<", ", m :: binary>>
                                 end)
        req2 = :cowboy_req.set_resp_header("allow", allow, req)
        respond(req2, state, 200)
      {:stop, req2, state2} ->
        terminate(req2, state2)
      {switch, req2, state2} when :erlang.element(1,
                                                    switch) === :switch_handler
                                  ->
        switch_handler(switch, req2, state2)
      {:ok, req2, state2} ->
        respond(req2, state2, 200)
    end
  end

  defp options(req, state) do
    content_types_provided(req, state)
  end

  defp content_types_provided(req, state) do
    case (call(req, state, :content_types_provided)) do
      :no_call ->
        state2 = r_state(state, content_types_p: [{{"text", "html", :"*"},
                                               :to_html}])
        try do
          :cowboy_req.parse_header("accept", req)
        catch
          _, _ ->
            respond(req, state2, 400)
        else
          :undefined ->
            languages_provided(Map.put(req, :media_type,
                                              {"text", "html", []}),
                                 r_state(state2, content_type_a: {{"text", "html", []},
                                                              :to_html}))
          accept ->
            choose_media_type(req, state2,
                                prioritize_accept(accept))
        end
      {:stop, req2, state2} ->
        terminate(req2, state2)
      {switch, req2, state2} when :erlang.element(1,
                                                    switch) === :switch_handler
                                  ->
        switch_handler(switch, req2, state2)
      {[], req2, state2} ->
        not_acceptable(req2, state2)
      {cTP, req2, state2} ->
        cTP2 = (for p <- cTP do
                  normalize_content_types(p)
                end)
        state3 = r_state(state2, content_types_p: cTP2)
        try do
          :cowboy_req.parse_header("accept", req2)
        catch
          _, _ ->
            respond(req2, state3, 400)
        else
          :undefined ->
            {pMT0, _Fun} = (headCTP = hd(cTP2))
            pMT = (case (pMT0) do
                     {type, subType, :"*"} ->
                       {type, subType, []}
                     _ ->
                       pMT0
                   end)
            languages_provided(Map.put(req2, :media_type, pMT),
                                 r_state(state3, content_type_a: headCTP))
          accept ->
            choose_media_type(req2, state3,
                                prioritize_accept(accept))
        end
    end
  end

  defp normalize_content_types({contentType, callback})
      when is_binary(contentType) do
    {:cow_http_hd.parse_content_type(contentType), callback}
  end

  defp normalize_content_types(normalized) do
    normalized
  end

  defp prioritize_accept(accept) do
    :lists.sort(fn {mediaTypeA, quality, _AcceptParamsA},
                     {mediaTypeB, quality, _AcceptParamsB} ->
                     prioritize_mediatype(mediaTypeA, mediaTypeB)
                   {_MediaTypeA, qualityA, _AcceptParamsA},
                     {_MediaTypeB, qualityB, _AcceptParamsB} ->
                     qualityA > qualityB
                end,
                  accept)
  end

  defp prioritize_mediatype({typeA, subTypeA, paramsA},
            {typeB, subTypeB, paramsB}) do
    case (typeB) do
      ^typeA ->
        case (subTypeB) do
          ^subTypeA ->
            length(paramsA) > length(paramsB)
          "*" ->
            true
          _Any ->
            false
        end
      "*" ->
        true
      _Any ->
        false
    end
  end

  defp choose_media_type(req, state, []) do
    not_acceptable(req, state)
  end

  defp choose_media_type(req, state = r_state(content_types_p: cTP),
            [mediaType | tail]) do
    match_media_type(req, state, tail, cTP, mediaType)
  end

  defp match_media_type(req, state, accept, [], _MediaType) do
    choose_media_type(req, state, accept)
  end

  defp match_media_type(req, state, accept, cTP,
            mediaType = {{"*", "*", _Params_A}, _QA, _APA}) do
    match_media_type_params(req, state, accept, cTP,
                              mediaType)
  end

  defp match_media_type(req, state, accept,
            cTP = [{{type, subType_P, _PP}, _Fun} | _Tail],
            mediaType = {{type, subType_A, _PA}, _QA, _APA})
      when subType_P === subType_A or subType_A === "*" do
    match_media_type_params(req, state, accept, cTP,
                              mediaType)
  end

  defp match_media_type(req, state, accept, [_Any | tail], mediaType) do
    match_media_type(req, state, accept, tail, mediaType)
  end

  defp match_media_type_params(req, state, accept,
            [provided = {{tP, sTP, :"*"}, _Fun} | tail],
            mediaType = {{tA, _STA, params_A0}, _QA, _APA}) do
    case (:lists.keytake("charset", 1, params_A0)) do
      {:value, {_, charset}, params_A} when tA === "text" ->
        case (call(req, state, :charsets_provided)) do
          :no_call ->
            languages_provided(Map.put(req, :media_type,
                                              {tP, sTP, params_A0}),
                                 r_state(state, content_type_a: provided))
          {:stop, req2, state2} ->
            terminate(req2, state2)
          {switch, req2, state2} when :erlang.element(1,
                                                        switch) === :switch_handler
                                      ->
            switch_handler(switch, req2, state2)
          {cP, req2, state2} ->
            state3 = r_state(state2, charsets_p: cP)
            case (:lists.member(charset, cP)) do
              false ->
                match_media_type(req2, state3, accept, tail, mediaType)
              true ->
                languages_provided(Map.put(req2, :media_type,
                                                   {tP, sTP, params_A}),
                                     r_state(state3, content_type_a: provided, 
                                                 charset_a: charset))
            end
        end
      _ ->
        languages_provided(Map.put(req, :media_type,
                                          {tP, sTP, params_A0}),
                             r_state(state, content_type_a: provided))
    end
  end

  defp match_media_type_params(req, state, accept,
            [provided = {pMT = {tP, sTP, params_P0}, fun} | tail],
            mediaType = {{_TA, _STA, params_A}, _QA, _APA}) do
    case (:lists.sort(params_P0) === :lists.sort(params_A)) do
      true when tP === "text" ->
        {charset, params_P} = (case (:lists.keytake("charset", 1,
                                                      params_P0)) do
                                 false ->
                                   {:undefined, params_P0}
                                 {:value, {_, charset0}, params_P1} ->
                                   {charset0, params_P1}
                               end)
        languages_provided(Map.put(req, :media_type,
                                          {tP, sTP, params_P}),
                             r_state(state, content_type_a: {{tP, sTP, params_P},
                                                         fun}, 
                                        charset_a: charset))
      true ->
        languages_provided(Map.put(req, :media_type, pMT),
                             r_state(state, content_type_a: provided))
      false ->
        match_media_type(req, state, accept, tail, mediaType)
    end
  end

  defp languages_provided(req, state) do
    case (call(req, state, :languages_provided)) do
      :no_call ->
        charsets_provided(req, state)
      {:stop, req2, state2} ->
        terminate(req2, state2)
      {switch, req2, state2} when :erlang.element(1,
                                                    switch) === :switch_handler
                                  ->
        switch_handler(switch, req2, state2)
      {[], req2, state2} ->
        not_acceptable(req2, state2)
      {lP, req2, state2} ->
        state3 = r_state(state2, languages_p: lP)
        case (:cowboy_req.parse_header("accept-language", req2)) do
          :undefined ->
            set_language(req2, r_state(state3, language_a: hd(lP)))
          acceptLanguage ->
            acceptLanguage2 = prioritize_languages(acceptLanguage)
            choose_language(req2, state3, acceptLanguage2)
        end
    end
  end

  defp prioritize_languages(acceptLanguages) do
    :lists.sort(fn {_TagA, qualityA}, {_TagB, qualityB} ->
                     qualityA > qualityB
                end,
                  acceptLanguages)
  end

  defp choose_language(req, state, []) do
    not_acceptable(req, state)
  end

  defp choose_language(req, state = r_state(languages_p: lP),
            [language | tail]) do
    match_language(req, state, tail, lP, language)
  end

  defp match_language(req, state, accept, [], _Language) do
    choose_language(req, state, accept)
  end

  defp match_language(req, state, _Accept, [provided | _Tail],
            {:"*", _Quality}) do
    set_language(req, r_state(state, language_a: provided))
  end

  defp match_language(req, state, _Accept, [provided | _Tail],
            {provided, _Quality}) do
    set_language(req, r_state(state, language_a: provided))
  end

  defp match_language(req, state, accept, [provided | tail],
            language = {tag, _Quality}) do
    length = byte_size(tag)
    case (provided) do
      <<^tag :: size(length) - binary, ?-, _Any :: bits>> ->
        set_language(req, r_state(state, language_a: provided))
      _Any ->
        match_language(req, state, accept, tail, language)
    end
  end

  defp set_language(req, state = r_state(language_a: language)) do
    req2 = :cowboy_req.set_resp_header("content-language", language, req)
    charsets_provided(Map.put(req2, :language, language),
                        state)
  end

  defp charsets_provided(req, state = r_state(charset_a: charset))
      when charset !== :undefined do
    set_content_type(req, state)
  end

  defp charsets_provided(req, state = r_state(charsets_p: [])) do
    not_acceptable(req, state)
  end

  defp charsets_provided(req, state = r_state(charsets_p: cP))
      when cP !== :undefined do
    case (:cowboy_req.parse_header("accept-charset", req)) do
      :undefined ->
        set_content_type(req, r_state(state, charset_a: hd(cP)))
      acceptCharset0 ->
        acceptCharset = prioritize_charsets(acceptCharset0)
        choose_charset(req, state, acceptCharset)
    end
  end

  defp charsets_provided(req, state) do
    case (call(req, state, :charsets_provided)) do
      :no_call ->
        set_content_type(req, state)
      {:stop, req2, state2} ->
        terminate(req2, state2)
      {switch, req2, state2} when :erlang.element(1,
                                                    switch) === :switch_handler
                                  ->
        switch_handler(switch, req2, state2)
      {cP, req2, state2} ->
        charsets_provided(req2, r_state(state2, charsets_p: cP))
    end
  end

  defp prioritize_charsets(acceptCharsets) do
    :lists.sort(fn {_CharsetA, qualityA},
                     {_CharsetB, qualityB} ->
                     qualityA > qualityB
                end,
                  acceptCharsets)
  end

  defp choose_charset(req, state, []) do
    not_acceptable(req, state)
  end

  defp choose_charset(req, state, [{_, 0} | tail]) do
    choose_charset(req, state, tail)
  end

  defp choose_charset(req, state = r_state(charsets_p: cP),
            [charset | tail]) do
    match_charset(req, state, tail, cP, charset)
  end

  defp match_charset(req, state, accept, [], _Charset) do
    choose_charset(req, state, accept)
  end

  defp match_charset(req, state, _Accept, [provided | _], {"*", _}) do
    set_content_type(req, r_state(state, charset_a: provided))
  end

  defp match_charset(req, state, _Accept, [provided | _],
            {provided, _}) do
    set_content_type(req, r_state(state, charset_a: provided))
  end

  defp match_charset(req, state, accept, [_ | tail], charset) do
    match_charset(req, state, accept, tail, charset)
  end

  defp set_content_type(req,
            state = r_state(content_type_a: {{type, subType, params},
                                         _Fun},
                        charset_a: charset)) do
    paramsBin = set_content_type_build_params(params, [])
    contentType = [type, "/", subType, paramsBin]
    contentType2 = (case ({type, charset}) do
                      {"text", ^charset} when charset !== :undefined ->
                        [contentType, "; charset=", charset]
                      _ ->
                        contentType
                    end)
    req2 = :cowboy_req.set_resp_header("content-type", contentType2, req)
    encodings_provided(Map.put(req2, :charset, charset),
                         state)
  end

  defp set_content_type_build_params(:"*", []) do
    <<>>
  end

  defp set_content_type_build_params([], []) do
    <<>>
  end

  defp set_content_type_build_params([], acc) do
    :lists.reverse(acc)
  end

  defp set_content_type_build_params([{attr, value} | tail], acc) do
    set_content_type_build_params(tail,
                                    [[attr, "=", value], ";" | acc])
  end

  defp encodings_provided(req, state) do
    ranges_provided(req, state)
  end

  defp not_acceptable(req, state) do
    respond(req, state, 406)
  end

  defp ranges_provided(req, state) do
    case (call(req, state, :ranges_provided)) do
      :no_call ->
        variances(req, state)
      {:stop, req2, state2} ->
        terminate(req2, state2)
      {switch, req2, state2} when :erlang.element(1,
                                                    switch) === :switch_handler
                                  ->
        switch_handler(switch, req2, state2)
      {[], req2, state2} ->
        req3 = :cowboy_req.set_resp_header("accept-ranges", "none", req2)
        variances(req3, r_state(state2, ranges_a: []))
      {rP, req2, state2} ->
        <<", ", acceptRanges :: binary>> = (for {r,
                                                _} <- rP, into: <<>> do
                                           <<", ", r :: binary>>
                                         end)
        req3 = :cowboy_req.set_resp_header("accept-ranges", acceptRanges,
                                             req2)
        variances(req3, r_state(state2, ranges_a: rP))
    end
  end

  defp variances(req,
            state = r_state(content_types_p: cTP, languages_p: lP,
                        charsets_p: cP)) do
    variances = (case (cTP) do
                   [] ->
                     []
                   [_] ->
                     []
                   [_ | _] ->
                     ["accept"]
                 end)
    variances2 = (case (lP) do
                    [] ->
                      variances
                    [_] ->
                      variances
                    [_ | _] ->
                      ["accept-language" | variances]
                  end)
    variances3 = (case (cP) do
                    :undefined ->
                      variances2
                    [] ->
                      variances2
                    [_] ->
                      variances2
                    [_ | _] ->
                      ["accept-charset" | variances2]
                  end)
    try do
      variances(req, state, variances3)
    catch
      class, reason ->
        error_terminate(req, state, class, reason,
                          __STACKTRACE__)
    else
      {variances4, req2, state2} ->
        case (for v <- variances4 do
                [", ", v]
              end) do
          [] ->
            resource_exists(req2, state2)
          [[", ", h] | variances5] ->
            req3 = :cowboy_req.set_resp_header("vary", [h | variances5],
                                                 req2)
            resource_exists(req3, state2)
        end
    end
  end

  defp variances(req, state, variances) do
    case (unsafe_call(req, state, :variances)) do
      :no_call ->
        {variances, req, state}
      {handlerVariances, req2, state2} ->
        {variances ++ handlerVariances, req2, state2}
    end
  end

  defp resource_exists(req, state) do
    expect(req, state, :resource_exists, true,
             &if_match_exists/2, &if_match_must_not_exist/2)
  end

  defp if_match_exists(req, state) do
    state2 = r_state(state, exists: true)
    case (:cowboy_req.parse_header("if-match", req)) do
      :undefined ->
        if_unmodified_since_exists(req, state2)
      :"*" ->
        if_unmodified_since_exists(req, state2)
      eTagsList ->
        if_match(req, state2, eTagsList)
    end
  end

  defp if_match(req, state, etagsList) do
    try do
      generate_etag(req, state)
    catch
      class, reason ->
        error_terminate(req, state, class, reason,
                          __STACKTRACE__)
    else
      {{:weak, _}, req2, state2} ->
        precondition_failed(req2, state2)
      {etag, req2, state2} ->
        case (:lists.member(etag, etagsList)) do
          true ->
            if_none_match_exists(req2, state2)
          false ->
            precondition_failed(req2, state2)
        end
    end
  end

  defp if_match_must_not_exist(req, state) do
    case (:cowboy_req.header("if-match", req)) do
      :undefined ->
        is_put_to_missing_resource(req, state)
      _ ->
        precondition_failed(req, state)
    end
  end

  defp if_unmodified_since_exists(req, state) do
    try do
      :cowboy_req.parse_header("if-unmodified-since", req)
    catch
      _, _ ->
        if_none_match_exists(req, state)
    else
      :undefined ->
        if_none_match_exists(req, state)
      ifUnmodifiedSince ->
        if_unmodified_since(req, state, ifUnmodifiedSince)
    end
  end

  defp if_unmodified_since(req, state, ifUnmodifiedSince) do
    try do
      last_modified(req, state)
    catch
      class, reason ->
        error_terminate(req, state, class, reason,
                          __STACKTRACE__)
    else
      {lastModified, req2, state2} ->
        case (lastModified > ifUnmodifiedSince) do
          true ->
            precondition_failed(req2, state2)
          false ->
            if_none_match_exists(req2, state2)
        end
    end
  end

  defp if_none_match_exists(req, state) do
    case (:cowboy_req.parse_header("if-none-match", req)) do
      :undefined ->
        if_modified_since_exists(req, state)
      :"*" ->
        precondition_is_head_get(req, state)
      etagsList ->
        if_none_match(req, state, etagsList)
    end
  end

  defp if_none_match(req, state, etagsList) do
    try do
      generate_etag(req, state)
    catch
      class, reason ->
        error_terminate(req, state, class, reason,
                          __STACKTRACE__)
    else
      {etag, req2, state2} ->
        case (etag) do
          :undefined ->
            precondition_failed(req2, state2)
          ^etag ->
            case (is_weak_match(etag, etagsList)) do
              true ->
                precondition_is_head_get(req2, state2)
              false ->
                method(req2, state2)
            end
        end
    end
  end

  defp is_weak_match(_, []) do
    false
  end

  defp is_weak_match({_, tag}, [{_, tag} | _]) do
    true
  end

  defp is_weak_match(etag, [_ | tail]) do
    is_weak_match(etag, tail)
  end

  defp precondition_is_head_get(req, state = r_state(method: method))
      when method === "HEAD" or method === "GET" do
    not_modified(req, state)
  end

  defp precondition_is_head_get(req, state) do
    precondition_failed(req, state)
  end

  defp if_modified_since_exists(req, state) do
    try do
      :cowboy_req.parse_header("if-modified-since", req)
    catch
      _, _ ->
        method(req, state)
    else
      :undefined ->
        method(req, state)
      ifModifiedSince ->
        if_modified_since_now(req, state, ifModifiedSince)
    end
  end

  defp if_modified_since_now(req, state, ifModifiedSince) do
    case (ifModifiedSince > :erlang.universaltime()) do
      true ->
        method(req, state)
      false ->
        if_modified_since(req, state, ifModifiedSince)
    end
  end

  defp if_modified_since(req, state, ifModifiedSince) do
    try do
      last_modified(req, state)
    catch
      class, reason ->
        error_terminate(req, state, class, reason,
                          __STACKTRACE__)
    else
      {:undefined, req2, state2} ->
        method(req2, state2)
      {lastModified, req2, state2} ->
        case (lastModified > ifModifiedSince) do
          true ->
            method(req2, state2)
          false ->
            not_modified(req2, state2)
        end
    end
  end

  defp not_modified(req, state) do
    req2 = :cowboy_req.delete_resp_header("content-type", req)
    try do
      set_resp_etag(req2, state)
    catch
      class, reason ->
        error_terminate(req, state, class, reason,
                          __STACKTRACE__)
    else
      {req3, state2} ->
        try do
          set_resp_expires(req3, state2)
        catch
          class, reason ->
            error_terminate(req, state2, class, reason,
                              __STACKTRACE__)
        else
          {req4, state3} ->
            respond(req4, state3, 304)
        end
    end
  end

  defp precondition_failed(req, state) do
    respond(req, state, 412)
  end

  defp is_put_to_missing_resource(req, state = r_state(method: "PUT")) do
    moved_permanently(req, state, &is_conflict/2)
  end

  defp is_put_to_missing_resource(req, state) do
    previously_existed(req, state)
  end

  defp moved_permanently(req, state, onFalse) do
    case (call(req, state, :moved_permanently)) do
      {{true, location}, req2, state2} ->
        req3 = :cowboy_req.set_resp_header("location", location, req2)
        respond(req3, state2, 301)
      {false, req2, state2} ->
        onFalse.(req2, state2)
      {:stop, req2, state2} ->
        terminate(req2, state2)
      {switch, req2, state2} when :erlang.element(1,
                                                    switch) === :switch_handler
                                  ->
        switch_handler(switch, req2, state2)
      :no_call ->
        onFalse.(req, state)
    end
  end

  defp previously_existed(req, state) do
    expect(req, state, :previously_existed, false,
             fn r, s ->
                  is_post_to_missing_resource(r, s, 404)
             end,
             fn r, s ->
                  moved_permanently(r, s, &moved_temporarily/2)
             end)
  end

  defp moved_temporarily(req, state) do
    case (call(req, state, :moved_temporarily)) do
      {{true, location}, req2, state2} ->
        req3 = :cowboy_req.set_resp_header("location", location, req2)
        respond(req3, state2, 307)
      {false, req2, state2} ->
        is_post_to_missing_resource(req2, state2, 410)
      {:stop, req2, state2} ->
        terminate(req2, state2)
      {switch, req2, state2} when :erlang.element(1,
                                                    switch) === :switch_handler
                                  ->
        switch_handler(switch, req2, state2)
      :no_call ->
        is_post_to_missing_resource(req, state, 410)
    end
  end

  defp is_post_to_missing_resource(req, state = r_state(method: "POST"), onFalse) do
    allow_missing_post(req, state, onFalse)
  end

  defp is_post_to_missing_resource(req, state, onFalse) do
    respond(req, state, onFalse)
  end

  defp allow_missing_post(req, state, onFalse) do
    expect(req, state, :allow_missing_post, true,
             &accept_resource/2, onFalse)
  end

  defp method(req, state = r_state(method: "DELETE")) do
    delete_resource(req, state)
  end

  defp method(req, state = r_state(method: "PUT")) do
    is_conflict(req, state)
  end

  defp method(req, state = r_state(method: method))
      when method === "POST" or method === "PATCH" do
    accept_resource(req, state)
  end

  defp method(req, state = r_state(method: method))
      when method === "GET" or method === "HEAD" do
    set_resp_body_etag(req, state)
  end

  defp method(req, state) do
    multiple_choices(req, state)
  end

  defp delete_resource(req, state) do
    expect(req, state, :delete_resource, false, 500,
             &delete_completed/2)
  end

  defp delete_completed(req, state) do
    expect(req, state, :delete_completed, true,
             &has_resp_body/2, 202)
  end

  defp is_conflict(req, state) do
    expect(req, state, :is_conflict, false,
             &accept_resource/2, 409)
  end

  defp accept_resource(req, state) do
    case (call(req, state, :content_types_accepted)) do
      :no_call ->
        respond(req, state, 415)
      {:stop, req2, state2} ->
        terminate(req2, state2)
      {switch, req2, state2} when :erlang.element(1,
                                                    switch) === :switch_handler
                                  ->
        switch_handler(switch, req2, state2)
      {cTA, req2, state2} ->
        cTA2 = (for p <- cTA do
                  normalize_content_types(p)
                end)
        try do
          :cowboy_req.parse_header("content-type", req2)
        catch
          _, _ ->
            respond(req2, state2, 415)
        else
          {type = "multipart", subType, params} ->
            contentType = {type, subType,
                             :lists.keydelete("boundary", 1, params)}
            choose_content_type(req2, state2, contentType, cTA2)
          contentType ->
            choose_content_type(req2, state2, contentType, cTA2)
        end
    end
  end

  defp choose_content_type(req, state, _ContentType, []) do
    respond(req, state, 415)
  end

  defp choose_content_type(req, state, contentType,
            [{accepted, fun} | _Tail])
      when accepted === :"*" or accepted === contentType do
    process_content_type(req, state, fun)
  end

  defp choose_content_type(req, state, {type, subType, param},
            [{{type, subType, acceptedParam}, fun} | _Tail])
      when acceptedParam === :"*" or acceptedParam === param do
    process_content_type(req, state, fun)
  end

  defp choose_content_type(req, state, contentType, [_Any | tail]) do
    choose_content_type(req, state, contentType, tail)
  end

  defp process_content_type(req, state = r_state(method: method, exists: exists),
            fun) do
    try do
      case (call(req, state, fun)) do
        {:stop, req2, state2} ->
          terminate(req2, state2)
        {switch, req2, state2} when :erlang.element(1,
                                                      switch) === :switch_handler
                                    ->
          switch_handler(switch, req2, state2)
        {true, req2, state2} when exists ->
          next(req2, state2, &has_resp_body/2)
        {true, req2, state2} ->
          next(req2, state2, &maybe_created/2)
        {false, req2, state2} ->
          respond(req2, state2, 400)
        {{true, resURL}, req2, state2} when method === "POST" ->
          req3 = :cowboy_req.set_resp_header("location", resURL, req2)
          cond do
            exists ->
              respond(req3, state2, 303)
            true ->
              respond(req3, state2, 201)
          end
      end
    catch
      class, reason = {:case_clause, :no_call} ->
        error_terminate(req, state, class, reason,
                          __STACKTRACE__)
    end
  end

  defp maybe_created(req, state = r_state(method: "PUT")) do
    respond(req, state, 201)
  end

  defp maybe_created(req, state) do
    case (:cowboy_req.has_resp_header("location", req)) do
      true ->
        respond(req, state, 201)
      false ->
        has_resp_body(req, state)
    end
  end

  defp has_resp_body(req, state) do
    case (:cowboy_req.has_resp_body(req)) do
      true ->
        multiple_choices(req, state)
      false ->
        respond(req, state, 204)
    end
  end

  defp set_resp_body_etag(req, state) do
    try do
      set_resp_etag(req, state)
    catch
      class, reason ->
        error_terminate(req, state, class, reason,
                          __STACKTRACE__)
    else
      {req2, state2} ->
        set_resp_body_last_modified(req2, state2)
    end
  end

  defp set_resp_body_last_modified(req, state) do
    try do
      last_modified(req, state)
    catch
      class, reason ->
        error_terminate(req, state, class, reason,
                          __STACKTRACE__)
    else
      {lastModified, req2, state2} ->
        case (lastModified) do
          ^lastModified when is_atom(lastModified) ->
            set_resp_body_expires(req2, state2)
          ^lastModified ->
            lastModifiedBin = :cowboy_clock.rfc1123(lastModified)
            req3 = :cowboy_req.set_resp_header("last-modified", lastModifiedBin,
                                                 req2)
            set_resp_body_expires(req3, state2)
        end
    end
  end

  defp set_resp_body_expires(req, state) do
    try do
      set_resp_expires(req, state)
    catch
      class, reason ->
        error_terminate(req, state, class, reason,
                          __STACKTRACE__)
    else
      {req2, state2} ->
        if_range(req2, state2)
    end
  end

  defp if_range(req = %{headers: %{"if-range" => _, "range" => _}},
            state = r_state(etag: etag)) do
    try do
      :cowboy_req.parse_header("if-range", req)
    catch
      _, _ ->
        set_resp_body(req, state)
    else
      ^etag = {:strong, _} ->
        range(req, state)
      _ ->
        set_resp_body(req, state)
    end
  end

  defp if_range(req, state) do
    range(req, state)
  end

  defp range(req, state = r_state(ranges_a: [])) do
    set_resp_body(req, state)
  end

  defp range(req, state) do
    try do
      :cowboy_req.parse_header("range", req)
    catch
      _, _ ->
        range_not_satisfiable(req, state, :undefined)
    else
      :undefined ->
        set_resp_body(req, state)
      {:bytes, bytesRange} ->
        choose_range(req, state, {"bytes", bytesRange})
      range ->
        choose_range(req, state, range)
    end
  end

  defp choose_range(req, state = r_state(ranges_a: rangesAccepted),
            range = {rangeUnit, _}) do
    case (:lists.keyfind(rangeUnit, 1, rangesAccepted)) do
      {_, callback} ->
        range_satisfiable(Map.put(req, :range, range), state,
                            callback)
      false ->
        set_resp_body(req, state)
    end
  end

  defp range_satisfiable(req, state, callback) do
    case (call(req, state, :range_satisfiable)) do
      :no_call ->
        set_ranged_body(req, state, callback)
      {:stop, req2, state2} ->
        terminate(req2, state2)
      {switch, req2, state2} when :erlang.element(1,
                                                    switch) === :switch_handler
                                  ->
        switch_handler(switch, req2, state2)
      {true, req2, state2} ->
        set_ranged_body(req2, state2, callback)
      {false, req2, state2} ->
        range_not_satisfiable(req2, state2, :undefined)
      {{false, int}, req2, state2} when is_integer(int) ->
        range_not_satisfiable(req2, state2,
                                ["*/", :erlang.integer_to_binary(int)])
      {{false, iodata}, req2, state2}
          when is_binary(iodata) or is_list(iodata) ->
        range_not_satisfiable(req2, state2, iodata)
    end
  end

  defp set_ranged_body(req = %{range: {"bytes", _}}, state, :auto) do
    set_ranged_body_auto(req, state)
  end

  defp set_ranged_body(req, state, callback) do
    set_ranged_body_callback(req, state, callback)
  end

  defp set_ranged_body_auto(req,
            state = r_state(handler: handler,
                        content_type_a: {_, callback})) do
    try do
      case (call(req, state, callback)) do
        {:stop, req2, state2} ->
          terminate(req2, state2)
        {switch, req2, state2} when :erlang.element(1,
                                                      switch) === :switch_handler
                                    ->
          switch_handler(switch, req2, state2)
        {body, req2, state2} ->
          maybe_set_ranged_body_auto(req2, state2, body)
      end
    catch
      class, {:case_clause, :no_call} ->
        error_terminate(req, state, class,
                          {:error, {:missing_callback, {handler, callback, 2}},
                             :"A callback specified in content_types_provided/2 is not exported."},
                          __STACKTRACE__)
    end
  end

  defp maybe_set_ranged_body_auto(req = %{range: {_, ranges}}, state, body) do
    size = (case (body) do
              {:sendfile, _, bytes, _} ->
                bytes
              _ ->
                :erlang.iolist_size(body)
            end)
    checks = (for range <- ranges do
                case (range) do
                  {from, :infinity} ->
                    from < size
                  {from, to} ->
                    from < size and from <= to and to <= size
                  neg ->
                    neg !== 0 and - neg < size
                end
              end)
    case (:lists.usort(checks)) do
      [true] ->
        set_ranged_body_auto(req, state, body)
      _ ->
        range_not_satisfiable(req, state,
                                ["*/", :erlang.integer_to_binary(size)])
    end
  end

  defp set_ranged_body_auto(req = %{range: {_, ranges}}, state, body) do
    parts = (for range <- ranges do
               ranged_partition(range, body)
             end)
    case (parts) do
      [onePart] ->
        set_one_ranged_body(req, state, onePart)
      _ when is_tuple(body) ->
        send_multipart_ranged_body(req, state, parts)
      _ ->
        set_multipart_ranged_body(req, state, parts)
    end
  end

  defp ranged_partition(range, {:sendfile, offset0, bytes0, path}) do
    {from, to, offset, bytes} = (case (range) do
                                   {from0, :infinity} ->
                                     {from0, bytes0 - 1, offset0 + from0,
                                        bytes0 - from0}
                                   {from0, to0} ->
                                     {from0, to0, offset0 + from0,
                                        1 + to0 - from0}
                                   neg ->
                                     {bytes0 + neg, bytes0 - 1,
                                        offset0 + bytes0 + neg, - neg}
                                 end)
    {{from, to, bytes0}, {:sendfile, offset, bytes, path}}
  end

  defp ranged_partition(range, data0) do
    total = :erlang.iolist_size(data0)
    {from, to, data} = (case (range) do
                          {from0, :infinity} ->
                            {_, data1} = :cow_iolists.split(from0, data0)
                            {from0, total - 1, data1}
                          {from0, to0} ->
                            {_, data1} = :cow_iolists.split(from0, data0)
                            {data2, _} = :cow_iolists.split(to0 - from0 + 1,
                                                              data1)
                            {from0, to0, data2}
                          neg ->
                            {_, data1} = :cow_iolists.split(total + neg, data0)
                            {total + neg, total - 1, data1}
                        end)
    {{from, to, total}, data}
  end

  defp set_ranged_body_callback(req, state = r_state(handler: handler), callback) do
    try do
      case (call(req, state, callback)) do
        {:stop, req2, state2} ->
          terminate(req2, state2)
        {switch, req2, state2} when :erlang.element(1,
                                                      switch) === :switch_handler
                                    ->
          switch_handler(switch, req2, state2)
        {[oneRange], req2, state2} ->
          set_one_ranged_body(req2, state2, oneRange)
        {ranges, req2, state2} when length(ranges) > 1 ->
          hasSendfile = [] !== (for {_,
                                       {:sendfile, _, _, _}} <- ranges do
                                  true
                                end)
          case (hasSendfile) do
            true ->
              send_multipart_ranged_body(req2, state2, ranges)
            false ->
              set_multipart_ranged_body(req2, state2, ranges)
          end
      end
    catch
      class, {:case_clause, :no_call} ->
        error_terminate(req, state, class,
                          {:error, {:missing_callback, {handler, callback, 2}},
                             :"A callback specified in ranges_provided/2 is not exported."},
                          __STACKTRACE__)
    end
  end

  defp set_one_ranged_body(req0, state, oneRange) do
    {contentRange, body} = prepare_range(req0, oneRange)
    req1 = :cowboy_req.set_resp_header("content-range", contentRange,
                                         req0)
    req = :cowboy_req.set_resp_body(body, req1)
    respond(req, state, 206)
  end

  defp set_multipart_ranged_body(req, state, [firstRange | moreRanges]) do
    boundary = :cow_multipart.boundary()
    contentType = :cowboy_req.resp_header("content-type", req)
    {firstContentRange, firstPartBody} = prepare_range(req,
                                                         firstRange)
    firstPartHead = :cow_multipart.first_part(boundary,
                                                [{"content-type", contentType}, {"content-range",
                                                                      firstContentRange}])
    moreParts = (for nextRange <- moreRanges do
                   (
                     {nextContentRange, nextPartBody} = prepare_range(req,
                                                                        nextRange)
                     nextPartHead = :cow_multipart.part(boundary,
                                                          [{"content-type", contentType}, {"content-range",
                                                                                nextContentRange}])
                     [nextPartHead, nextPartBody]
                   )
                 end)
    body = [firstPartHead, firstPartBody, moreParts,
                                              :cow_multipart.close(boundary)]
    req2 = :cowboy_req.set_resp_header("content-type", ["multipart/byteranges; boundary=", boundary],
                                         req)
    req3 = :cowboy_req.set_resp_body(body, req2)
    respond(req3, state, 206)
  end

  defp send_multipart_ranged_body(req, state, [firstRange | moreRanges]) do
    boundary = :cow_multipart.boundary()
    contentType = :cowboy_req.resp_header("content-type", req)
    req2 = :cowboy_req.set_resp_header("content-type", ["multipart/byteranges; boundary=", boundary],
                                         req)
    req3 = :cowboy_req.stream_reply(206, req2)
    {firstContentRange, firstPartBody} = prepare_range(req,
                                                         firstRange)
    firstPartHead = :cow_multipart.first_part(boundary,
                                                [{"content-type", contentType}, {"content-range",
                                                                      firstContentRange}])
    :cowboy_req.stream_body(firstPartHead, :nofin, req3)
    :cowboy_req.stream_body(firstPartBody, :nofin, req3)
    _ = (for nextRange <- moreRanges do
           (
             {nextContentRange, nextPartBody} = prepare_range(req,
                                                                nextRange)
             nextPartHead = :cow_multipart.part(boundary,
                                                  [{"content-type", contentType}, {"content-range",
                                                                        nextContentRange}])
             :cowboy_req.stream_body(nextPartHead, :nofin, req3)
             :cowboy_req.stream_body(nextPartBody, :nofin, req3)
             [nextPartHead, nextPartBody]
           )
         end)
    :cowboy_req.stream_body(:cow_multipart.close(boundary),
                              :fin, req3)
    terminate(req3, state)
  end

  defp prepare_range(%{range: {rangeUnit, _}},
            {{from, to, total0}, body}) do
    total = (case (total0) do
               :"*" ->
                 "*"
               _ ->
                 :erlang.integer_to_binary(total0)
             end)
    contentRange = [rangeUnit, ?\s,
                                   :erlang.integer_to_binary(from), ?-,
                                                                        :erlang.integer_to_binary(to),
                                                                            ?/,
                                                                                total]
    {contentRange, body}
  end

  defp prepare_range(%{range: {rangeUnit, _}}, {rangeData, body}) do
    {[rangeUnit, ?\s, rangeData], body}
  end

  defp range_not_satisfiable(req, state, :undefined) do
    respond(req, state, 416)
  end

  defp range_not_satisfiable(req0 = %{range: {rangeUnit, _}}, state,
            rangeData) do
    req = :cowboy_req.set_resp_header("content-range",
                                        [rangeUnit, ?\s, rangeData], req0)
    respond(req, state, 416)
  end

  defp set_resp_body(req,
            state = r_state(handler: handler,
                        content_type_a: {_, callback})) do
    try do
      case (call(req, state, callback)) do
        {:stop, req2, state2} ->
          terminate(req2, state2)
        {switch, req2, state2} when :erlang.element(1,
                                                      switch) === :switch_handler
                                    ->
          switch_handler(switch, req2, state2)
        {body, req2, state2} ->
          req3 = :cowboy_req.set_resp_body(body, req2)
          multiple_choices(req3, state2)
      end
    catch
      class, {:case_clause, :no_call} ->
        error_terminate(req, state, class,
                          {:error, {:missing_callback, {handler, callback, 2}},
                             :"A callback specified in content_types_provided/2 is not exported."},
                          __STACKTRACE__)
    end
  end

  defp multiple_choices(req, state) do
    expect(req, state, :multiple_choices, false, 200, 300)
  end

  defp set_resp_etag(req, state) do
    {etag, req2, state2} = generate_etag(req, state)
    case (etag) do
      :undefined ->
        {req2, state2}
      ^etag ->
        req3 = :cowboy_req.set_resp_header("etag", encode_etag(etag),
                                             req2)
        {req3, state2}
    end
  end

  defp encode_etag({:strong, etag}) do
    [?", etag, ?"]
  end

  defp encode_etag({:weak, etag}) do
    ['W/"', etag, ?"]
  end

  defp set_resp_expires(req, state) do
    {expires, req2, state2} = expires(req, state)
    case (expires) do
      ^expires when is_atom(expires) ->
        {req2, state2}
      ^expires when is_binary(expires) ->
        req3 = :cowboy_req.set_resp_header("expires", expires, req2)
        {req3, state2}
      ^expires ->
        expiresBin = :cowboy_clock.rfc1123(expires)
        req3 = :cowboy_req.set_resp_header("expires", expiresBin, req2)
        {req3, state2}
    end
  end

  defp generate_etag(req, state = r_state(etag: :no_call)) do
    {:undefined, req, state}
  end

  defp generate_etag(req, state = r_state(etag: :undefined)) do
    case (unsafe_call(req, state, :generate_etag)) do
      :no_call ->
        {:undefined, req, r_state(state, etag: :no_call)}
      {etag, req2, state2} when is_binary(etag) ->
        etag2 = :cow_http_hd.parse_etag(etag)
        {etag2, req2, r_state(state2, etag: etag2)}
      {etag, req2, state2} ->
        {etag, req2, r_state(state2, etag: etag)}
    end
  end

  defp generate_etag(req, state = r_state(etag: etag)) do
    {etag, req, state}
  end

  defp last_modified(req, state = r_state(last_modified: :no_call)) do
    {:undefined, req, state}
  end

  defp last_modified(req, state = r_state(last_modified: :undefined)) do
    case (unsafe_call(req, state, :last_modified)) do
      :no_call ->
        {:undefined, req, r_state(state, last_modified: :no_call)}
      {lastModified, req2, state2} ->
        {lastModified, req2,
           r_state(state2, last_modified: lastModified)}
    end
  end

  defp last_modified(req, state = r_state(last_modified: lastModified)) do
    {lastModified, req, state}
  end

  defp expires(req, state = r_state(expires: :no_call)) do
    {:undefined, req, state}
  end

  defp expires(req, state = r_state(expires: :undefined)) do
    case (unsafe_call(req, state, :expires)) do
      :no_call ->
        {:undefined, req, r_state(state, expires: :no_call)}
      {expires, req2, state2} ->
        {expires, req2, r_state(state2, expires: expires)}
    end
  end

  defp expires(req, state = r_state(expires: expires)) do
    {expires, req, state}
  end

  defp expect(req, state, callback, expected, onTrue,
            onFalse) do
    case (call(req, state, callback)) do
      :no_call ->
        next(req, state, onTrue)
      {:stop, req2, state2} ->
        terminate(req2, state2)
      {switch, req2, state2} when :erlang.element(1,
                                                    switch) === :switch_handler
                                  ->
        switch_handler(switch, req2, state2)
      {^expected, req2, state2} ->
        next(req2, state2, onTrue)
      {_Unexpected, req2, state2} ->
        next(req2, state2, onFalse)
    end
  end

  defp call(req0,
            state = r_state(handler: handler,
                        handler_state: handlerState0),
            callback) do
    case (:erlang.function_exported(handler, callback,
                                      2)) do
      true ->
        try do
          apply(handler, callback, [req0, handlerState0])
        catch
          class, reason ->
            error_terminate(req0, state, class, reason,
                              __STACKTRACE__)
        else
          :no_call ->
            :no_call
          {result, req, handlerState} ->
            {result, req, r_state(state, handler_state: handlerState)}
        end
      false ->
        :no_call
    end
  end

  defp unsafe_call(req0,
            state = r_state(handler: handler,
                        handler_state: handlerState0),
            callback) do
    case (:erlang.function_exported(handler, callback,
                                      2)) do
      false ->
        :no_call
      true ->
        case (apply(handler, callback,
                      [req0, handlerState0])) do
          :no_call ->
            :no_call
          {result, req, handlerState} ->
            {result, req, r_state(state, handler_state: handlerState)}
        end
    end
  end

  defp next(req, state, next) when is_function(next) do
    next.(req, state)
  end

  defp next(req, state, statusCode)
      when is_integer(statusCode) do
    respond(req, state, statusCode)
  end

  defp respond(req0, state, statusCode) do
    req = (case (:cowboy_req.has_resp_body(req0)) do
             true when statusCode === 200 ->
               req0
             true ->
               req0
             false ->
               :cowboy_req.delete_resp_header("content-type", req0)
           end)
    terminate(:cowboy_req.reply(statusCode, req), state)
  end

  defp switch_handler({:switch_handler, mod}, req,
            r_state(handler_state: handlerState)) do
    {mod, req, handlerState}
  end

  defp switch_handler({:switch_handler, mod, opts}, req,
            r_state(handler_state: handlerState)) do
    {mod, req, handlerState, opts}
  end

  defp error_terminate(req,
            r_state(handler: handler, handler_state: handlerState), class,
            reason, stacktrace) do
    :cowboy_handler.terminate({:crash, class, reason}, req,
                                handlerState, handler)
    :erlang.raise(class, reason, stacktrace)
  end

  defp terminate(req,
            r_state(handler: handler, handler_state: handlerState)) do
    result = :cowboy_handler.terminate(:normal, req,
                                         handlerState, handler)
    {:ok, req, result}
  end

end