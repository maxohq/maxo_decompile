defmodule MaxoDecompile.CoreTest do
  use ExUnit.Case
  use MnemeDefaults
  alias MaxoDecompile.Core

  describe "pure_process" do
    test "works for elixir" do
      {mod, code} = Core.pure_process("MaxoDecompile.ExampleModule", to: "ex")

      auto_assert(
        """
        defmodule MaxoDecompile.ExampleModule do
          def unquote(:hey!)() do
            IO.puts("HEY!!!")
          end

          def hello() do
            IO.puts("hello")
          end

          def _HOLA() do
            IO.puts("I'M INJECTED!")
          end
        end\
        """ <- pretty_code(code)
      )
    end

    test "works for erlang" do
      {mod, code} = Core.pure_process("MaxoDecompile.ExampleModule", to: "erl")

      auto_assert(
        """
        defmodule MaxoDecompile.ExampleModule do
          def unquote(:hey!)() do
            IO.puts("HEY!!!")
          end

          def hello() do
            IO.puts("hello")
          end

          def _HOLA() do
            IO.puts("I'M INJECTED!")
          end
        end\
        """ <- pretty_code(code)
      )
    end
  end

  def pretty_code(code) do
    IO.iodata_to_binary(code) |> Code.format_string!() |> IO.iodata_to_binary()
  end
end
