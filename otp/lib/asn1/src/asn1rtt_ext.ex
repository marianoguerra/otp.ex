defmodule :m_asn1rtt_ext do
  use Bitwise

  def transform_to_EXTERNAL1990({_, _, _, _} = val) do
    transform_to_EXTERNAL1990(
      :erlang.tuple_to_list(val),
      []
    )
  end

  def transform_to_EXTERNAL1990(val) when is_tuple(val) do
    val
  end

  defp transform_to_EXTERNAL1990([:EXTERNAL | rest], acc) do
    transform_to_EXTERNAL1990(rest, [:EXTERNAL | acc])
  end

  defp transform_to_EXTERNAL1990([{:syntax, syntax} | rest], acc) do
    transform_to_EXTERNAL1990(
      rest,
      [[:asn1_NOVALUE, syntax] | acc]
    )
  end

  defp transform_to_EXTERNAL1990([{:"presentation-context-id", pCid} | rest], acc) do
    transform_to_EXTERNAL1990(
      rest,
      [[pCid, :asn1_NOVALUE] | acc]
    )
  end

  defp transform_to_EXTERNAL1990([{:"context-negotiation", context_negot} | rest], acc) do
    {_, presentation_Cid, transfer_syntax} = context_negot

    transform_to_EXTERNAL1990(
      rest,
      [[presentation_Cid, transfer_syntax] | acc]
    )
  end

  defp transform_to_EXTERNAL1990([:asn1_NOVALUE | rest], acc) do
    transform_to_EXTERNAL1990(rest, [:asn1_NOVALUE | acc])
  end

  defp transform_to_EXTERNAL1990([data_val_desc, data_value], acc)
       when is_list(data_value) or is_binary(data_value) do
    :erlang.list_to_tuple(
      :lists.reverse([
        [{:"octet-aligned", data_value}, data_val_desc]
        | acc
      ])
    )
  end

  defp transform_to_EXTERNAL1990([data_val_desc, data_value], acc)
       when is_binary(data_value) do
    :erlang.list_to_tuple(
      :lists.reverse([
        [{:"single-ASN1-type", data_value}, data_val_desc]
        | acc
      ])
    )
  end

  defp transform_to_EXTERNAL1990([data_value], acc)
       when is_list(data_value) or
              is_binary(data_value) do
    :erlang.list_to_tuple(
      :lists.reverse([
        {:"octet-aligned", data_value}
        | acc
      ])
    )
  end

  def transform_to_EXTERNAL1990_maps(%{:identification => id, :"data-value" => value} = v) do
    m0 =
      case id do
        {:syntax, dRef} ->
          %{:"direct-reference" => dRef}

        {:"presentation-context-id", indRef} ->
          %{:"indirect-reference" => indRef}

        {:"context-negotiation",
         %{:"presentation-context-id" => indRef, :"transfer-syntax" => dRef}} ->
          %{:"direct-reference" => dRef, :"indirect-reference" => indRef}
      end

    m =
      case v do
        %{:"data-value-descriptor" => dvd} ->
          %{m0 | :"data-value-descriptor" => dvd}

        %{} ->
          m0
      end

    %{m | :encoding => {:"octet-aligned", value}}
  end

  def transform_to_EXTERNAL1990_maps(%{:encoding => _} = v) do
    v
  end

  def transform_to_EXTERNAL1994({:EXTERNAL, dRef, indRef, data_v_desc, encoding} = v) do
    identification =
      case {dRef, indRef} do
        {^dRef, :asn1_NOVALUE} ->
          {:syntax, dRef}

        {:asn1_NOVALUE, ^indRef} ->
          {:"presentation-context-id", indRef}

        _ ->
          {:"context-negotiation", {:"EXTERNAL_identification_context-negotiation", indRef, dRef}}
      end

    case encoding do
      {:"octet-aligned", val} when is_list(val) or is_binary(val) ->
        {:EXTERNAL, identification, data_v_desc, val}

      _ ->
        v
    end
  end

  def transform_to_EXTERNAL1994_maps(v0) do
    identification =
      case v0 do
        %{:"direct-reference" => dRef, :"indirect-reference" => :asn1_NOVALUE} ->
          {:syntax, dRef}

        %{:"direct-reference" => :asn1_NOVALUE, :"indirect-reference" => indRef} ->
          {:"presentation-context-id", indRef}

        %{:"direct-reference" => dRef, :"indirect-reference" => indRef} ->
          {:"context-negotiation",
           %{:"transfer-syntax" => dRef, :"presentation-context-id" => indRef}}
      end

    case v0 do
      %{:encoding => {:"octet-aligned", val}}
      when is_list(val) or
             is_binary(val) ->
        v = %{:identification => identification, :"data-value" => val}

        case v0 do
          %{:"data-value-descriptor" => :asn1_NOVALUE} ->
            v

          %{:"data-value-descriptor" => dvd} ->
            %{v | :"data-value-descriptor" => dvd}
        end

      _ ->
        v =
          for {k, v} <- :maps.to_list(v0),
              v !== :asn1_NOVALUE do
            {k, v}
          end

        :maps.from_list(v)
    end
  end
end
