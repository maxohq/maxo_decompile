defmodule MaxoDecompile.Core do
  alias MaxoDecompile.Util

  def run(modules, opts) do
    opts_as_map = Map.new(opts)
    Enum.each(modules, &process(&1, opts_as_map))
  end

  def process(module_or_path, opts) do
    opts = Map.new(opts)
    {module, data} = module_or_path |> get_beam!() |> decompile(opts)

    if Map.get(opts, :stdout, true) do
      IO.puts(data)
    else
      File.write("#{module}.ex", data)
    end

    {module, data}
  end

  def get_beam!(module_or_path) do
    with :non_existing <- :code.which(Util.module(module_or_path)),
         :non_existing <- :code.which(String.to_atom(module_or_path)),
         :non_existing <- get_beam_file(module_or_path),
         :non_existing <- :code.where_is_file(Util.basename(module_or_path)) do
      Mix.raise("Could not find .beam file for #{module_or_path}")
    end
  end

  def get_beam_file(path) do
    list = String.to_charlist(path)

    if File.exists?(path) and not match?({:error, _, _}, :beam_lib.info(list)) do
      list
    else
      :non_existing
    end
  end

  def decompile(path, opts) do
    format = Util.get_format(opts)

    case :beam_lib.chunks(path, [:debug_info]) do
      {:ok, {module, [debug_info: {:debug_info_v1, backend, data}]}} ->
        from_debug_info(format, module, backend, data)

      {:error, :beam_lib, {:unknown_chunk, _, _}} ->
        abstract_code_decompile(path, format)

      {:error, :beam_lib, {:missing_chunk, _, _}} ->
        abstract_code_decompile(path, format)

      _ ->
        Mix.raise("Invalid .beam file at #{path}")
    end
  end

  defp abstract_code_decompile(_path, :expanded) do
    Mix.raise("OTP 20 is required for decompiling to the expanded format")
  end

  defp abstract_code_decompile(path, format) do
    case :beam_lib.chunks(path, [:abstract_code]) do
      {:ok, {module, erlang_forms}} ->
        from_abstract_code(format, module, erlang_forms)

      _ ->
        Mix.raise("Missing debug info and abstract code for .beam file at #{path}")
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
        format_erlang_forms(module, erlang_forms)

      {:ok, erlang_forms} ->
        from_erlang_forms(format, module, erlang_forms)

      {:error, error} ->
        Mix.raise(
          "Failed to extract Erlang debug info for module #{inspect(module)}: #{inspect(error)}"
        )
    end
  end

  defp from_abstract_code(:erlang, module, forms) do
    format_erlang_forms(module, forms)
  end

  defp from_abstract_code(other, module, forms) do
    from_erlang_forms(other, module, forms)
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

  defp format_erlang_forms(module, erlang_forms) do
    File.open("#{module}.erl", [:write], fn file ->
      # Enum.each(erlang_forms, &IO.puts(file, :erl_pp.form(&1)))
      Enum.each(erlang_forms, &IO.puts(:standard_io, :erl_pp.form(&1)))
    end)
  end

  defp from_erlang_forms(:diff_asm, module, forms) do
    case :compile.noenv_forms(forms, [:S]) do
      {:ok, ^module, res} ->
        {:ok, formatted} = :decompile_diffable_asm.format(res)

        File.open("#{module}.S", [:write], fn file ->
          # :decompile_diffable_asm.beam_listing(file, formatted)
          :decompile_diffable_asm.beam_listing(:standard_io, formatted)
        end)

      {:error, error} ->
        Mix.raise("Failed to compile to diffasm for module #{inspect(module)}: #{inspect(error)}")
    end
  end

  defp from_erlang_forms(format, module, forms) do
    case :compile.noenv_forms(forms, [format]) do
      {:ok, ^module, res} ->
        File.open("#{module}.#{Util.ext(format)}", [:write], fn _file ->
          # :beam_listing.module(file, res)
          # :beam_listing.module(IO.stream(:stdio, :line), res)
          :beam_listing.module(:standard_io, res)
        end)

      {:error, error} ->
        Mix.raise(
          "Failed to compile to #{inspect(format)} for module #{inspect(module)}: #{inspect(error)}"
        )
    end
  end
end
