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
        """ <- pretty_elixir_code(code)
      )
    end

    test "works for erlang" do
      {mod, code} = Core.pure_process("MaxoDecompile.ExampleModule", to: "erl")

      auto_assert(
        """
        -file("lib/maxo_decompile/example_module.ex", 11).

        -module('Elixir.MaxoDecompile.ExampleModule').

        -compile([no_auto_import]).

        -export(['_HOLA'/0,'__info__'/1,hello/0,'hey!'/0]).

        -spec '__info__'(attributes | compile | functions | macros | md5 |
                         exports_md5 | module | deprecated | struct) ->
                            any().

        '__info__'(module) ->
            'Elixir.MaxoDecompile.ExampleModule';
        '__info__'(functions) ->
            [{'_HOLA', 0}, {hello, 0}, {'hey!', 0}];
        '__info__'(macros) ->
            [];
        '__info__'(struct) ->
            nil;
        '__info__'(exports_md5) ->
            <<".­A{\\224\\007Þëmô\\eÑú\\236ï\\232">>;
        '__info__'(Key = attributes) ->
            erlang:get_module_info('Elixir.MaxoDecompile.ExampleModule', Key);
        '__info__'(Key = compile) ->
            erlang:get_module_info('Elixir.MaxoDecompile.ExampleModule', Key);
        '__info__'(Key = md5) ->
            erlang:get_module_info('Elixir.MaxoDecompile.ExampleModule', Key);
        '__info__'(deprecated) ->
            [].

        '_HOLA'() ->
            'Elixir.IO':puts(<<"I'M INJECTED!">>).

        hello() ->
            'Elixir.IO':puts(<<"hello">>).

        'hey!'() ->
            'Elixir.IO':puts(<<"HEY!!!">>).

        """ <- code
      )
    end
  end

  def pretty_elixir_code(code) do
    IO.iodata_to_binary(code) |> Code.format_string!() |> IO.iodata_to_binary()
  end
end
