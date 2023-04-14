defmodule MaxoDecompile.AbstractCode do
  alias MaxoDecompile.ErlangFormatter

  def decompile(_path, :expanded) do
    Mix.raise("OTP 20 is required for decompiling to the expanded format")
  end

  def decompile(path, format) do
    case :beam_lib.chunks(path, [:abstract_code]) do
      {:ok, {module, erlang_forms}} ->
        from_abstract_code(format, module, erlang_forms)

      _ ->
        Mix.raise("Missing debug info and abstract code for .beam file at #{path}")
    end
  end

  defp from_abstract_code(:erlang, module, forms) do
    ErlangFormatter.do_erlang_forms(module, forms)
  end

  defp from_abstract_code(other, module, forms) do
    from_erlang_forms(other, module, forms)
  end

  def from_erlang_forms(:diff_asm, module, forms) do
    case :compile.noenv_forms(forms, [:S]) do
      {:ok, ^module, res} ->
        {:ok, formatted} = :decompile_diffable_asm.format(res)

        # File.open("#{module}.S", [:write], fn file ->
        # :decompile_diffable_asm.beam_listing(file, formatted)

        # {:ok, file} = StringIO.open("")
        :decompile_diffable_asm.beam_listing(:standard_io, formatted)

      # end)

      {:error, error} ->
        Mix.raise("Failed to compile to diffasm for module #{inspect(module)}: #{inspect(error)}")
    end
  end

  def from_erlang_forms(format, module, forms) do
    case :compile.noenv_forms(forms, [format]) do
      {:ok, ^module, res} ->
        # File.open("#{module}.#{Util.ext(format)}", [:write], fn _file ->
        # :beam_listing.module(file, res)
        # :beam_listing.module(IO.stream(:stdio, :line), res)
        :beam_listing.module(:standard_io, res)

      # end)

      {:error, error} ->
        Mix.raise(
          "Failed to compile to #{inspect(format)} for module #{inspect(module)}: #{inspect(error)}"
        )
    end
  end
end
