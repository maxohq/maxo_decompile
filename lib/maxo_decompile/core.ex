defmodule MaxoDecompile.Core do
  alias MaxoDecompile.Util
  alias MaxoDecompile.BeamFinder
  alias MaxoDecompile.Abstract

  def run(modules, opts) do
    opts_as_map = Map.new(opts)
    Enum.each(modules, &process(&1, opts_as_map))
  end

  def process(module_or_path, opts) do
    opts = Map.new(opts)
    {module, data} = module_or_path |> BeamFinder.get!() |> decompile(opts)

    if Map.get(opts, :stdout, true) do
      IO.puts(data)
    else
      File.write("#{module}.ex", data)
    end

    {module, data}
  end

  def decompile(path, opts) do
    format = Util.get_format(opts)

    case :beam_lib.chunks(path, [:debug_info]) do
      {:ok, {module, [debug_info: {:debug_info_v1, backend, data}]}} ->
        from_debug_info(format, module, backend, data)

      {:error, :beam_lib, {:unknown_chunk, _, _}} ->
        Abstract.abstract_code_decompile(path, format)

      {:error, :beam_lib, {:missing_chunk, _, _}} ->
        Abstract.abstract_code_decompile(path, format)

      _ ->
        Mix.raise("Invalid .beam file at #{path}")
    end
  end

  defp from_debug_info(:expanded, module, backend, data) do
    case backend.debug_info(:elixir_v1, module, data, []) do
      {:ok, elixir_info} ->
        format_elixir_info(module, elixir_info)

      {:error, error} ->
        Mix.raise(
          "Failed to extract Elixir debug info for module #{inspect(module)}: #{inspect(error)}"
        )
    end
  end

  defp from_debug_info(format, module, backend, data) do
    case backend.debug_info(:erlang_v1, module, data, []) do
      {:ok, erlang_forms} when format == :erlang ->
        Abstract.format_erlang_forms(module, erlang_forms)

      {:ok, erlang_forms} ->
        Abstract.from_erlang_forms(format, module, erlang_forms)

      {:error, error} ->
        Mix.raise(
          "Failed to extract Erlang debug info for module #{inspect(module)}: #{inspect(error)}"
        )
    end
  end

  defp format_elixir_info(module, elixir_info) do
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
