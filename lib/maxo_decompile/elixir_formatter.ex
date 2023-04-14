defmodule MaxoDecompile.ElixirFormatter do
  def elixir_info(module, elixir_info) do
    data =
      [
        "defmodule ",
        inspect(module),
        " do\n",
        Enum.map(elixir_info.definitions, &format_definition/1),
        "end\n"
      ]
      |> IO.iodata_to_binary()
      |> Code.format_string!()

    {module, data}
  end

  defp format_definition({{name, _arity}, kind, _meta, heads}) do
    Enum.map(heads, fn {_meta, args, _what?, body} ->
      fhead =
        if Regex.match?(~r/^[a-zA-Z\d\_]+$/, "#{name}") do
          ~s[  #{kind} #{name}(#{Enum.map_join(args, ", ", &Macro.to_string/1)}) do\n]
        else
          ~s[  #{kind} unquote(:"#{name}")(#{Enum.map_join(args, ", ", &Macro.to_string/1)}) do\n]
        end

      [fhead, Macro.to_string(body), "  end\n"]
    end)
  end
end
