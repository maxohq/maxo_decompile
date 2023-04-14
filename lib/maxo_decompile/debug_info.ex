defmodule MaxoDecompile.DebugInfo do
  alias MaxoDecompile.AbstractCode
  alias MaxoDecompile.ElixirFormatter
  alias MaxoDecompile.ErlangFormatter

  def decompile(:expanded, module, backend, data) do
    case backend.debug_info(:elixir_v1, module, data, []) do
      {:ok, elixir_info} ->
        ElixirFormatter.elixir_info(module, elixir_info)

      {:error, error} ->
        Mix.raise(
          "Failed to extract Elixir debug info for module #{inspect(module)}: #{inspect(error)}"
        )
    end
  end

  def decompile(format, module, backend, data) do
    case backend.debug_info(:erlang_v1, module, data, []) do
      {:ok, erlang_forms} when format == :erlang ->
        ErlangFormatter.do_erlang_forms(module, erlang_forms)

      {:ok, erlang_forms} ->
        AbstractCode.from_erlang_forms(format, module, erlang_forms)

      {:error, error} ->
        Mix.raise(
          "Failed to extract Erlang debug info for module #{inspect(module)}: #{inspect(error)}"
        )
    end
  end
end
