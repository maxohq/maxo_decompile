defmodule MaxoDecompile.Abstract do
  alias MaxoDecompile.Util

  def abstract_code_decompile(_path, :expanded) do
    Mix.raise("OTP 20 is required for decompiling to the expanded format")
  end

  def abstract_code_decompile(path, format) do
    case :beam_lib.chunks(path, [:abstract_code]) do
      {:ok, {module, erlang_forms}} ->
        from_abstract_code(format, module, erlang_forms)

      _ ->
        Mix.raise("Missing debug info and abstract code for .beam file at #{path}")
    end
  end

  defp from_abstract_code(:erlang, module, forms) do
    format_erlang_forms(module, forms)
  end

  defp from_abstract_code(other, module, forms) do
    from_erlang_forms(other, module, forms)
  end

  def from_erlang_forms(:diff_asm, module, forms) do
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

  def from_erlang_forms(format, module, forms) do
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

  def format_erlang_forms(module, erlang_forms) do
    File.open("#{module}.erl", [:write], fn file ->
      # Enum.each(erlang_forms, &IO.puts(file, :erl_pp.form(&1)))
      Enum.each(erlang_forms, &IO.puts(:standard_io, :erl_pp.form(&1)))
    end)
  end
end
