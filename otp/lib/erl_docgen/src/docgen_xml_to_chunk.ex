defmodule :m_docgen_xml_to_chunk do
  use Bitwise
  require Record

  Record.defrecord(:r_docs_v1, :docs_v1,
    anno: :undefined,
    beam_language: :erlang,
    format: "application/erlang+html",
    module_doc: :undefined,
    metadata: %{:otp_doc_vsn => {1, 0, 0}},
    docs: :undefined
  )

  Record.defrecord(:r_docs_v1_entry, :docs_v1_entry,
    kind_name_arity: :undefined,
    anno: :undefined,
    signature: :undefined,
    doc: :undefined,
    metadata: :undefined
  )

  def main([_Application, fromBeam, _Escript, toChunk]) do
    name = :filename.basename(:filename.rootname(fromBeam)) ++ '.erl'

    emptyDocs =
      r_docs_v1(
        anno:
          :erl_anno.set_file(
            name,
            :erl_anno.new(0)
          ),
        module_doc: :hidden,
        docs: []
      )

    :ok =
      :file.write_file(
        toChunk,
        :erlang.term_to_binary(emptyDocs, [:compressed])
      )

    :ok
  end

  def main([application, fromXML, fromBeam, _Escript, toChunk]) do
    _ =
      :erlang.process_flag(
        :max_heap_size,
        20 * 1000 * 1000
      )

    case docs(application, fromXML, fromBeam) do
      {:error, reason} ->
        :io.format('Failed to create chunks: ~p~n', [reason])
        :erlang.halt(1)

      {:docs_v1, _, _, _, _, %{:source => s}, []}
      when s !== '../xml/gen_fsm.xml' and s !== '../xml/shell_default.xml' and
             s !== '../xml/user.xml' and s !== '../xml/wxClipboardTextEvent.xml' and
             s !== '../xml/wxDisplayChangedEvent.xml' and s !== '../xml/wxGBSizerItem.xml' and
             s !== '../xml/wxGraphicsBrush.xml' and s !== '../xml/wxGraphicsFont.xml' and
             s !== '../xml/wxGraphicsPen.xml' and s !== '../xml/wxInitDialogEvent.xml' and
             s !== '../xml/wxMaximizeEvent.xml' and s !== '../xml/wxMouseCaptureLostEvent.xml' and
             s !== '../xml/wxPaintEvent.xml' and s !== '../xml/wxPreviewCanvas.xml' and
             s !== '../xml/wxSysColourChangedEvent.xml' and s !== '../xml/wxTaskBarIconEvent.xml' and
             s !== '../xml/wxWindowCreateEvent.xml' and s !== '../xml/wxWindowDestroyEvent.xml' and
             s !== '../xml/wxDataObject.xml' ->
        :io.format('Failed to create chunks: no functions found ~s~n', [s])
        :erlang.halt(1)
        :ok

      docs ->
        :ok =
          :file.write_file(
            toChunk,
            :erlang.term_to_binary(docs, [:compressed])
          )
    end
  end

  Record.defrecord(:r_state, :state, tags: [], cno: [], namespaces: [], dom: [])

  defp initial_state() do
    r_state()
  end

  defp get_dom(r_state(dom: dom)) do
    dom
  end

  defp event(event, _LineNo, state) do
    build_dom(event, state)
  end

  defp build_dom(:startDocument, state) do
    r_state(state, dom: [:startDocument])
  end

  defp build_dom(
         :endDocument,
         r_state(dom: [{tag, attributes, content} | d]) = state
       ) do
    case d do
      [:startDocument] ->
        r_state(state, dom: [{tag, attributes, :lists.reverse(content)}])

      [decl, :startDocument] ->
        r_state(state, dom: [decl, {tag, attributes, :lists.reverse(content)}])

      _ ->
        state
    end
  end

  defp build_dom(
         {:startElement, _Uri, localName, _QName, attributes},
         r_state(tags: t, dom: d) = state
       ) do
    a = parse_attributes(localName, attributes)
    cName = :erlang.list_to_atom(localName)

    r_state(state,
      tags: [cName | t],
      dom: [{cName, :lists.reverse(a), []} | d]
    )
  end

  defp build_dom(
         {:endElement, _Uri, localName, _QName},
         r_state(
           tags: [_ | t],
           dom: [
             [{cName, cAttributes, cContent}, {pName, pAttributes, pContent} = _Parent]
             | d
           ]
         ) = state
       ) do
    case :erlang.list_to_atom(localName) do
      ^cName ->
        sectionDepth =
          length(
            for e <- t, e === :section do
              e
            end
          )

        mappedCName =
          case cName do
            :title ->
              :lists.nth(sectionDepth + 1, [:h1, :h2, :h3])

            :section when sectionDepth > 0 ->
              :div

            ^cName ->
              cName
          end

        r_state(state,
          tags: t,
          dom: [
            {pName, pAttributes,
             [
               {mappedCName, cAttributes, :lists.reverse(cContent)}
               | pContent
             ]}
            | d
          ]
        )

      _ ->
        throw({:dom_error, 'Got end of element: ' ++ localName ++ ' but expected: ' ++ cName})
    end
  end

  defp build_dom(
         {:characters, string},
         r_state(dom: [{name, attributes, content} | d]) = state
       ) do
    htmlEnts = [
      {'&nbsp;', [160]},
      {'&times;', [215]},
      {'&plusmn;', [177]},
      {'&ouml;', 'Ã¶'},
      {'&auml;', 'Ã¤'},
      {'&aring;', 'Ã¥'}
    ]

    noHtmlEnt =
      :lists.foldl(
        fn {pat, sub}, str ->
          :re.replace(str, pat, sub, [:global, :unicode])
        end,
        string,
        htmlEnts
      )

    case :re.run(noHtmlEnt, '&[a-z]*;', [{:capture, :first, :binary}, :unicode]) do
      :nomatch ->
        :ok

      {:match, ["&lt;"]} ->
        :ok

      {:match, ["&gt;"]} ->
        :ok

      else__ ->
        throw({:found_illigal_thing, else__, string})
    end

    newContent = [
      :unicode.characters_to_binary(
        noHtmlEnt,
        :utf8
      )
      | content
    ]

    r_state(state, dom: [{name, attributes, newContent} | d])
  end

  defp build_dom(
         {:ignorableWhitespace, string},
         r_state(dom: [{name, _, _} = _E | _]) = state
       ) do
    case :lists.member(
           name,
           [
             :p,
             :pre,
             :input,
             :code,
             :quote,
             :warning,
             :note,
             :dont,
             :do,
             :c,
             :i,
             :em,
             :strong,
             :seemfa,
             :seeerl,
             :seetype,
             :seeapp,
             :seecom,
             :seecref,
             :seefile,
             :seeguide,
             :tag,
             :item
           ]
         ) do
      true ->
        build_dom({:characters, string}, state)

      false ->
        state
    end
  end

  defp build_dom({:startEntity, sysId}, state) do
    :io.format('startEntity:~p~n', [sysId])
    state
  end

  defp build_dom(_E, state) do
    state
  end

  defp parse_attributes(elName, attributes) do
    parse_attributes(elName, attributes, 1, [])
  end

  defp parse_attributes(_, [], _, acc) do
    acc
  end

  defp parse_attributes(elName, [{_Uri, _Prefix, localName, attrValue} | as], n, acc) do
    parse_attributes(elName, as, n + 1, [{:erlang.list_to_atom(localName), attrValue} | acc])
  end

  defp docs(application, oTPXml, fromBEAM) do
    case :xmerl_sax_parser.file(
           oTPXml,
           [:skip_external_dtd, {:event_fun, &event/3}, {:event_state, initial_state()}]
         ) do
      {:ok, tree, _} ->
        {:ok, {module, chunks}} =
          :beam_lib.chunks(
            fromBEAM,
            [:exports, :abstract_code]
          )

        dom = get_dom(tree)
        :erlang.put(:application, application)

        :erlang.put(
          :module,
          :filename.basename(:filename.rootname(fromBEAM))
        )

        newDom = transform(dom, [])
        chunk = to_chunk(newDom, oTPXml, module, :proplists.get_value(:abstract_code, chunks))
        verify_chunk(module, :proplists.get_value(:exports, chunks), chunk)
        chunk

      else__ ->
        {:error, else__}
    end
  end

  defp verify_chunk(m, exports, r_docs_v1(docs: docs) = doc) do
    exported =
      for {{:function, f, a}, _, _, _, _} <- docs do
        fA = {f, a}
        {m, f, a, :lists.member(fA, exports)}
      end

    :lists.foreach(
      fn {_M, _F, _A, true} ->
        :ok
      end,
      exported
    )

    try do
      :shell_docs.validate(doc)
    catch
      err ->
        throw({:maps.get("en", r_docs_v1(doc, :module_doc)), err})
    end
  end

  defp transform([{:erlref, _Attr, content} | t], acc) do
    module =
      for mod = {:module, _, _} <- content do
        mod
      end

    newContent = content -- module
    [{:module, sinceAttr, [mname]}] = module

    since =
      case :proplists.get_value(
             :since,
             sinceAttr
           ) do
        :undefined ->
          []

        [] ->
          []

        vsn ->
          [{:since, vsn}]
      end

    transform(
      [
        {:module, [{:name, mname} | since], newContent}
        | t
      ],
      acc
    )
  end

  defp transform([{:header, _Attr, _Content} | t], acc) do
    transform(t, acc)
  end

  defp transform([{:section, attr, content} | t], acc) do
    transform(
      t,
      [{:section, attr, transform(content, [])} | acc]
    )
  end

  defp transform([{:list, attr, content} | t], acc) do
    transform([transform_list(attr, content) | t], acc)
  end

  defp transform([{:taglist, attr, content} | t], acc) do
    transform([transform_taglist(attr, content) | t], acc)
  end

  defp transform([{:anno, [], content} | t], acc) do
    transform([content | t], acc)
  end

  defp transform([{:c, [], content} | t], acc) do
    transform(
      t,
      [{:code, [], transform(content, [])} | acc]
    )
  end

  defp transform([{:code, attr, content} | t], acc) do
    transform(
      t,
      [
        {:pre, [], [{:code, a2b(attr), transform(content, [])}]}
        | acc
      ]
    )
  end

  defp transform([{:pre, attr, content} | t], acc) do
    transform(
      t,
      [
        {:pre, [], [{:code, attr, transform(content, [])}]}
        | acc
      ]
    )
  end

  defp transform([{:funcs, _Attr, content} | t], acc) do
    fns = {:functions, [], transform_funcs(content, [])}
    transform(t, [fns | acc])
  end

  defp transform([{:datatypes, _Attr, content} | t], acc) do
    dts = transform(content, [])
    transform(t, [{:datatypes, [], dts} | acc])
  end

  defp transform([{:datatype, _Attr, content} | t], acc) do
    transform(t, transform_datatype(content, []) ++ acc)
  end

  defp transform(
         [{:datatype_title, _Attr, _Content} | t],
         acc
       ) do
    transform(t, acc)
  end

  defp transform([{:desc, _Attr, content} | t], acc) do
    transform(t, [transform(content, []) | acc])
  end

  defp transform([{:strong, attr, content} | t], acc) do
    transform([{:em, attr, content} | t], acc)
  end

  defp transform([{:marker, attrs, content} | t], acc) do
    transform(
      t,
      [{:a, a2b(attrs), transform(content, [])} | acc]
    )
  end

  defp transform([{:url, attrs, content} | t], acc) do
    transform(
      t,
      [{:a, a2b(attrs), transform(content, [])} | acc]
    )
  end

  defp transform([{what, [], content} | t], acc)
       when what === :note or what === :warning or
              what === :do or what === :dont do
    whatP = {:div, [{:class, :erlang.atom_to_binary(what)}], transform(content, [])}
    transform(t, [whatP | acc])
  end

  defp transform([{:type, _, []} | _] = dom, acc) do
    case transform_types(dom, []) do
      {[], t} ->
        transform(t, acc)

      {types, t} ->
        nameSort = fn {:li, a, _}, {:li, b, _} ->
          nameA = :proplists.get_value(:name, a)
          nameB = :proplists.get_value(:name, b)

          cond do
            nameA == nameB ->
              length(a) <= length(b)

            true ->
              nameA < nameB
          end
        end

        transform(
          t,
          [
            {:ul, [{:class, "types"}], :lists.sort(nameSort, types)}
            | acc
          ]
        )
    end
  end

  defp transform([{:type_desc, attr, _Content} | t], acc) do
    true = :proplists.is_defined(:variable, attr)
    transform(t, acc)
  end

  defp transform([{:type, [], content} | t], acc) do
    transform(
      t,
      [{:ul, [{:class, "types"}], transform(content, [])} | acc]
    )
  end

  defp transform([{:v, [], content} | t], acc) do
    transform(
      t,
      [{:li, [{:class, "type"}], transform(content, [])} | acc]
    )
  end

  defp transform([{:d, [], content} | t], acc) do
    transform(
      t,
      [{:li, [{:class, "description"}], transform(content, [])} | acc]
    )
  end

  defp transform([elem = {see, _Attr, _Content} | t], acc)
       when see === :seemfa or see === :seeerl or
              see === :seetype or see === :seeapp or
              see === :seecom or see === :seecref or
              see === :seefile or see === :seeguide do
    transform([transform_see(elem) | t], acc)
  end

  defp transform([{:term, attr, []} | t], acc) do
    transform(
      [
        :erlang.list_to_binary(
          :proplists.get_value(
            :id,
            attr
          )
        )
        | t
      ],
      acc
    )
  end

  defp transform([{:fsummary, _, _} | t], acc) do
    transform(t, acc)
  end

  defp transform([{:input, _, content} | t], acc) do
    transform(t, [transform(content, []) | acc])
  end

  defp transform([{:p, attr, content} | t], acc) do
    transform(
      t,
      [{:p, a2b(attr), transform(content, [])} | acc]
    )
  end

  defp transform([{:div, attr, content} | t], acc) do
    transform(
      t,
      [{:div, a2b(attr), transform(content, [])} | acc]
    )
  end

  defp transform([{tag, attr, content} | t], acc) do
    transform(
      t,
      [{tag, attr, transform(content, [])} | acc]
    )
  end

  defp transform([binary | t], acc) do
    transform(t, [binary | acc])
  end

  defp transform([], acc) do
    :lists.flatten(:lists.reverse(acc))
  end

  defp transform_list([{:type, 'ordered'}], content) do
    {:ol, [],
     for {:item, a2, c2} <- content do
       {:li, a2, c2}
     end}
  end

  defp transform_list(_, content) do
    {:ul, [],
     for {:item, a2, c2} <- content do
       {:li, a2, c2}
     end}
  end

  defp transform_types([{:type, attr, []} | t], acc) do
    case :proplists.is_defined(:name, attr) do
      true ->
        transform_types(t, [{:li, a2b(attr), []} | acc])

      false ->
        true = :proplists.is_defined(:variable, attr)
        transform_types(t, acc)
    end
  end

  defp transform_types([{:type_desc, attr, content} | t], acc) do
    case :proplists.is_defined(:name, attr) do
      true ->
        typeDesc = transform(content, [])

        transform_types(
          t,
          [{:li, a2b(attr) ++ [{:class, "description"}], typeDesc} | acc]
        )

      false ->
        true = :proplists.is_defined(:variable, attr)
        transform_types(t, acc)
    end
  end

  defp transform_types([{:type, _, _} | _T], _Acc) do
    throw(:mixed_type_declarations)
  end

  defp transform_types(dom, acc) do
    {:lists.reverse(acc), dom}
  end

  defp transform_taglist(attr, content) do
    items =
      :lists.map(
        fn
          {:tag, a, c} ->
            {:dt, a, c}

          {:item, a, c} ->
            {:dd, a, c}
        end,
        content
      )

    {:dl, attr, items}
  end

  defp transform_funcs([func | t], acc) do
    transform_funcs(t, func2func(func) ++ acc)
  end

  defp transform_funcs([], acc) do
    :lists.reverse(acc)
  end

  defp func2func({:fsdescription, _Attr, _Contents}) do
    []
  end

  defp func2func({:func, attr, contents}) do
    contentsNoName =
      for nC <- contents,
          :erlang.element(1, nC) != :name do
        nC
      end

    editLink =
      case :proplists.get_value(
             :ghlink,
             attr
           ) do
        :undefined ->
          %{}

        ghLink ->
          %{
            :edit_url => :erlang.iolist_to_binary(['https://github.com/erlang/otp/edit/', ghLink])
          }
      end

    verifyNameList = fn nameList, test ->
      for {:name, t, c} <- nameList do
        :ok = test.(c)
        {:proplists.get_value(:name, t), :proplists.get_value(:arity, t)}
      end
    end

    nameList =
      for {:name, _, _} = name <- contents do
        name
      end

    sinceMD =
      case (for {:name, sinceAttr, _} <- nameList,
                :proplists.get_value(:since, sinceAttr) !== [] do
              :proplists.get_value(:since, sinceAttr)
            end) do
        [] ->
          editLink

        sinces ->
          %{
            editLink
            | :since =>
                :unicode.characters_to_binary(
                  :lists.join(
                    ',',
                    :lists.usort(sinces)
                  )
                )
          }
      end

    functions =
      case nameList do
        [{:name, _, []} | _] ->
          tagsToFA = fn tags ->
            {:proplists.get_value(:name, tags), :proplists.get_value(:arity, tags)}
          end

          _ =
            verifyNameList.(
              nameList,
              fn [] ->
                :ok
              end
            )

          fAs =
            for {:name, fAttr, []} <- nameList do
              tagsToFA.(fAttr)
            end

          sortedFAs = :lists.usort(fAs)

          fAClauses =
            :lists.usort(
              for {:name, fAttr, []} <- nameList do
                {tagsToFA.(fAttr),
                 :proplists.get_value(
                   :clause_i,
                   fAttr
                 )}
              end
            )

          makeFunc = fn {f, a}, mD, doc ->
            specs =
              for {{cF, cA}, c} <- fAClauses, f === cF, a === cA do
                {:function, name} = func_to_atom(cF)
                {name, :erlang.list_to_integer(cA), c}
              end

            {:function,
             [
               {:name, f},
               {:arity, :erlang.list_to_integer(a)},
               {:signature, [:erlang.iolist_to_binary([f, '/', a])]},
               {:meta, %{mD | :signature => specs}}
             ], doc}
          end

          base = makeFunc.(hd(sortedFAs), sinceMD, contentsNoName)
          {baseF, baseA} = hd(sortedFAs)

          mD = %{
            sinceMD
            | :equiv => {:function, :erlang.list_to_atom(baseF), :erlang.list_to_integer(baseA)}
          }

          equiv =
            :lists.map(
              fn fA ->
                makeFunc.(fA, mD, [])
              end,
              tl(sortedFAs)
            )

          [base | equiv]

        ^nameList ->
          fAs =
            :lists.foldl(
              fn {:name, _, nameString}, acc ->
                fAs = func_to_tuple(nameString)

                :lists.foldl(
                  fn fA, fAAcc ->
                    slogan =
                      :maps.get(
                        fA,
                        fAAcc,
                        []
                      )

                    %{
                      fAAcc
                      | fA => [
                          strip_tags(nameString)
                          | slogan
                        ]
                    }
                  end,
                  acc,
                  fAs
                )
              end,
              %{},
              nameList
            )

          _ =
            verifyNameList.(
              nameList,
              fn [_ | _] ->
                :ok
              end
            )

          sortedFAs = :lists.usort(:maps.to_list(fAs))
          {{baseF, baseA}, baseSig} = hd(sortedFAs)

          base =
            {:function,
             [{:name, baseF}, {:arity, baseA}, {:signature, baseSig}, {:meta, sinceMD}],
             contentsNoName}

          equiv =
            for {{f, a}, signature} <- tl(sortedFAs) do
              {:function,
               [
                 {:name, f},
                 {:arity, a},
                 {:signature, signature},
                 {:meta, %{sinceMD | :equiv => {:function, :erlang.list_to_atom(baseF), baseA}}}
               ], []}
            end

          [base | equiv]
      end

    transform(functions, [])
  end

  def func_to_tuple(chars) do
    try do
      [name, args] = :string.split(strip_tags(chars), '(')
      arities = parse_args(:unicode.characters_to_list(args))

      for arity <- arities do
        {:unicode.characters_to_list(name), arity}
      end
    catch
      e, r ->
        :io.format('Failed to parse: ~p~n', [chars])
        :erlang.raise(e, r, __STACKTRACE__)
    end
  end

  defp parse_args(')' ++ _) do
    [0]
  end

  defp parse_args(args) do
    parse_args(:unicode.characters_to_list(args), 1, [])
  end

  defp parse_args([[?[, ?,] | t], arity, []) do
    parse_args(t, arity, [?[]) ++ parse_args(t, arity + 1, [])
  end

  defp parse_args([?, | t], arity, []) do
    parse_args(t, arity + 1, [])
  end

  defp parse_args([open | t], arity, stack)
       when open === ?[ or
              open === ?{ or open === ?( do
    parse_args(t, arity, [open | stack])
  end

  defp parse_args([?] | t], arity, [?[ | stack]) do
    parse_args(t, arity, stack)
  end

  defp parse_args([?} | t], arity, [?{ | stack]) do
    parse_args(t, arity, stack)
  end

  defp parse_args([?) | t], arity, [?( | stack]) do
    parse_args(t, arity, stack)
  end

  defp parse_args([?) | _T], arity, []) do
    [arity]
  end

  defp parse_args([_H | t], arity, stack) do
    parse_args(t, arity, stack)
  end

  defp strip_tags([{_Tag, _Attr, content} | t]) do
    [content | strip_tags(t)]
  end

  defp strip_tags([h | t]) when not is_tuple(h) do
    [h | strip_tags(t)]
  end

  defp strip_tags([]) do
    []
  end

  defp transform_datatype(dom, _Acc) do
    contentsNoName =
      transform(
        for nC <- dom,
            :erlang.element(1, nC) != :name do
          nC
        end,
        []
      )

    for n = {:name, _, _} <- dom do
      case n do
        {:name, nameAttr, []} ->
          {:datatype, nameAttr, contentsNoName}

        {:name, [], content} ->
          [{name, arity}] = func_to_tuple(content)
          signature = strip_tags(content)

          {:datatype,
           [{:name, name}, {:n_vars, :erlang.integer_to_list(arity)}, {:signature, signature}],
           contentsNoName}
      end
    end
  end

  defp transform_see({see, [{:marker, marker}], content}) do
    absMarker =
      case :string.lexemes(marker, '#') do
        [link] ->
          [:erlang.get(:application), ':', :erlang.get(:module), '#', link]

        [appMod, link] ->
          case :string.lexemes(appMod, ':') do
            [mod] ->
              [:erlang.get(:application), ':', mod, '#', link]

            [app, mod] ->
              [app, ':', mod, '#', link]
          end
      end

    {:a,
     [
       {:href, :erlang.iolist_to_binary(absMarker)},
       {:rel, <<"https://erlang.org/doc/link/", :erlang.atom_to_binary(see)::binary>>}
     ], content}
  end

  defp to_chunk(dom, source, module, aST) do
    [{:module, mAttr, mcontent}] = dom

    moduleDocs =
      :lists.flatmap(
        fn
          {tag, _, content}
          when tag === :description or
                 tag === :section ->
            content

          {_, _, _} ->
            []
        end,
        mcontent
      )

    typeMeta =
      add_types(
        aST,
        :maps.from_list([{:source, source} | mAttr])
      )

    typeMap = :maps.get(:types, typeMeta, [])

    anno =
      :erl_anno.set_file(
        :erlang.atom_to_list(module) ++ '.erl',
        :erl_anno.new(0)
      )

    types =
      :lists.flatten(
        for {:datatypes, [], types} <- mcontent do
          types
        end
      )

    typeEntries =
      :lists.map(
        fn {:datatype, attr, descr} ->
          {:function, typeName} =
            func_to_atom(
              :proplists.get_value(
                :name,
                attr
              )
            )

          typeArity =
            case :proplists.get_value(
                   :n_vars,
                   attr
                 ) do
              :undefined ->
                find_type_arity(
                  typeName,
                  typeMap
                )

              arity ->
                :erlang.list_to_integer(arity)
            end

          typeArgs =
            :lists.join(
              ',',
              for i <-
                    :lists.seq(
                      1,
                      typeArity
                    ) do
                :lists.concat(['Arg', i])
              end
            )

          placeholderSig =
            :io_lib.format(
              '-type ~p(~s) :: term().',
              [typeName, typeArgs]
            )

          typeSignature =
            :proplists.get_value(
              :signature,
              attr,
              [:erlang.iolist_to_binary(placeholderSig)]
            )

          metaSig =
            case :maps.get(
                   {typeName, typeArity},
                   typeMap,
                   :undefined
                 ) do
              :undefined ->
                %{}

              sig ->
                %{:signature => [sig]}
            end

          metaDepr =
            case :otp_internal.obsolete_type(
                   module,
                   typeName,
                   typeArity
                 ) do
              {:deprecated, text} ->
                %{
                  metaSig
                  | :deprecated =>
                      :unicode.characters_to_binary(
                        :erl_lint.format_error(
                          {:deprecated_type, {module, typeName, typeArity}, text}
                        )
                      )
                }

              :no ->
                metaSig
            end

          docs_v1_entry(:type, anno, typeName, typeArity, typeSignature, metaDepr, descr)
        end,
        types
      )

    functions =
      :lists.flatten(
        for {:functions, [], functions} <- mcontent do
          functions
        end
      )

    funcEntrys =
      :lists.map(
        fn {:function, attr, fdoc} ->
          {type, name} =
            func_to_atom(
              :proplists.get_value(
                :name,
                attr
              )
            )

          arity = :proplists.get_value(:arity, attr)

          signature =
            :proplists.get_value(
              :signature,
              attr
            )

          fMeta = :proplists.get_value(:meta, attr)
          metaWSpec = add_spec(aST, fMeta)

          metaDepr =
            case :otp_internal.obsolete(
                   module,
                   name,
                   arity
                 ) do
              {:deprecated, text} ->
                %{
                  metaWSpec
                  | :deprecated =>
                      :unicode.characters_to_binary(
                        :erl_lint.format_error({:deprecated, {module, name, arity}, text})
                      )
                }

              {:deprecated, replacement, rel} ->
                %{
                  metaWSpec
                  | :deprecated =>
                      :unicode.characters_to_binary(
                        :erl_lint.format_error(
                          {:deprecated, {module, name, arity}, replacement, rel}
                        )
                      )
                }

              _ ->
                metaWSpec
            end

          docs_v1_entry(type, anno, name, arity, signature, metaDepr, fdoc)
        end,
        functions
      )

    docs_v1(moduleDocs, anno, typeMeta, funcEntrys ++ typeEntries)
  end

  defp docs_v1(docContents, anno, metadata, docs) do
    r_docs_v1(
      anno: anno,
      module_doc: %{"en" => :shell_docs.normalize(docContents)},
      metadata: :maps.merge(metadata, r_docs_v1(r_docs_v1(), :metadata)),
      docs: docs
    )
  end

  defp docs_v1_entry(kind, anno, name, arity, signature, metadata, docContents) do
    annoWLine =
      case metadata do
        %{:signature => [sig | _]} ->
          sigAnno = :erlang.element(2, sig)
          :erl_anno.set_line(:erl_anno.line(sigAnno), anno)

        _NoSignature ->
          anno
      end

    doc =
      case docContents do
        [] ->
          %{}

        ^docContents ->
          %{"en" => :shell_docs.normalize(docContents)}
      end

    {{kind, name, arity}, annoWLine, :lists.flatten(signature), doc, metadata}
  end

  defp func_to_atom(list) do
    case :erl_scan.string(list) do
      {:ok, [{:atom, _, fn__}], _} ->
        {:function, fn__}

      {:ok, [{:var, _, fn__}], _} ->
        {:function, fn__}

      {:ok, [{fn__, _}], _} ->
        {:function, fn__}

      {:ok, [{:var, _, _}, {:":", _}, {:atom, _, fn__}], _} ->
        {:callback, fn__}

      {:ok, [{:var, _, _}, {:":", _}, {:var, _, fn__}], _} ->
        {:callback, fn__}
    end
  end

  defp add_spec(:no_abstract_code, meta) do
    meta
  end

  defp add_spec(
         {:raw_abstract_v1, aST},
         meta = %{:signature => specs}
       ) do
    %{meta | :signature => add_spec_clauses(aST, merge_clauses(specs, %{}))}
  end

  defp add_spec(_, meta) do
    meta
  end

  defp add_types(:no_abstract_code, meta) do
    meta
  end

  defp add_types({:raw_abstract_v1, aST}, meta) do
    %{
      meta
      | :types =>
          :maps.from_list(
            for t = {:attribute, _, tO, {name, _, args}} <- aST,
                tO === :type or tO === :opaque do
              {{name, length(args)}, t}
            end
          )
    }
  end

  defp add_spec_clauses(aST, [{{f, a}, clauses} | t]) do
    [
      filter_clauses(find_spec(aST, f, a), clauses)
      | add_spec_clauses(aST, t)
    ]
  end

  defp add_spec_clauses(_AST, []) do
    []
  end

  defp filter_clauses(spec, [:undefined]) do
    spec
  end

  defp filter_clauses(
         {:attribute, ln, :spec, {fA, clauses}},
         clauseIds
       ) do
    {_, filteredClauses} =
      :lists.foldl(
        fn {tO, _, _, _} = c, {cnt, acc}
           when tO === :type or tO === :opaque ->
          case :lists.member(
                 :erlang.integer_to_list(cnt),
                 clauseIds
               ) do
            true ->
              {cnt + 1, [c | acc]}

            false ->
              {cnt + 1, acc}
          end
        end,
        {1, []},
        clauses
      )

    {:attribute, ln, :spec, {fA, :lists.reverse(filteredClauses)}}
  end

  defp merge_clauses([{f, a, clause} | t], acc) do
    merge_clauses(
      t,
      %{acc | {f, a} => [clause | :maps.get({f, a}, acc, [])]}
    )
  end

  defp merge_clauses([], acc) do
    :maps.to_list(acc)
  end

  defp find_type_arity(
         name,
         [
           {{name, _}, {:attribute, _, tO, {name, _, args}}}
           | _T
         ]
       )
       when tO === :type or tO === :opaque do
    length(args)
  end

  defp find_type_arity(name, [_ | t]) do
    find_type_arity(name, t)
  end

  defp find_type_arity(name, map) when is_map(map) do
    find_type_arity(name, :maps.to_list(map))
  end

  defp find_spec(aST, func, arity) do
    specs =
      :lists.filter(
        fn
          {:attribute, _, :spec, {{f, a}, _}} ->
            f === func and a === arity

          {:attribute, _, :spec, {{_, f, a}, _}} ->
            f === func and a === arity

          _ ->
            false
        end,
        aST
      )

    case specs do
      [s] ->
        s

      [] ->
        :io.format('Could not find spec for ~p/~p~n', [func, arity])
        exit(1)
    end
  end

  defp a2b(attrs) do
    for {tag, value} <- attrs do
      {tag, :unicode.characters_to_binary(value)}
    end
  end
end
