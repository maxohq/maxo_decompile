defmodule MaxoDecompile.CoreTest do
  use ExUnit.Case
  use MnemeDefaults
  alias MaxoDecompile.Core

  describe "pure_process" do
    test "works for elixir" do
      {mod, code} = Core.pure_process("MaxoDecompile.ExampleModule", "ex")

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
      {mod, code} = Core.pure_process("MaxoDecompile.ExampleModule", "erl")

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

    test "works for asm" do
      {mod, code} = Core.pure_process("MaxoDecompile.ExampleModule", "asm")

      auto_assert(
        """
        {module, 'Elixir.MaxoDecompile.ExampleModule'}.  %% version = 0

        {exports, [{'_HOLA',0},
                   {'__info__',1},
                   {hello,0},
                   {'hey!',0},
                   {module_info,0},
                   {module_info,1}]}.

        {attributes, []}.

        {labels, 22}.


        {function, '__info__', 1, 2}.
          {label,1}.
            {line,[]}.
            {func_info,{atom,'Elixir.MaxoDecompile.ExampleModule'},
                       {atom,'__info__'},
                       1}.
          {label,2}.
            {select_val,{x,0},
                        {f,9},
                        {list,[{atom,attributes},
                               {f,8},
                               {atom,compile},
                               {f,8},
                               {atom,deprecated},
                               {f,7},
                               {atom,exports_md5},
                               {f,6},
                               {atom,functions},
                               {f,5},
                               {atom,macros},
                               {f,7},
                               {atom,md5},
                               {f,8},
                               {atom,module},
                               {f,4},
                               {atom,struct},
                               {f,3}]}}.
          {label,3}.
            {move,{atom,nil},{x,0}}.
            return.
          {label,4}.
            {move,{atom,'Elixir.MaxoDecompile.ExampleModule'},{x,0}}.
            return.
          {label,5}.
            {move,{literal,[{'_HOLA',0},{hello,0},{'hey!',0}]},{x,0}}.
            return.
          {label,6}.
            {move,{literal,<<46,173,65,123,148,7,222,235,109,244,27,209,250,158,239,
                             154>>},
                  {x,0}}.
            return.
          {label,7}.
            {move,nil,{x,0}}.
            return.
          {label,8}.
            {move,{x,0},{x,1}}.
            {move,{atom,'Elixir.MaxoDecompile.ExampleModule'},{x,0}}.
            {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.
          {label,9}.
            {call_only,1,{f,21}}. % '-inlined-__info__/1-'/1


        {function, '_HOLA', 0, 11}.
          {label,10}.
            {line,[{location,"lib/maxo_decompile/example_module.ex",12}]}.
            {func_info,{atom,'Elixir.MaxoDecompile.ExampleModule'},{atom,'_HOLA'},0}.
          {label,11}.
            {move,{literal,<<"I'M INJECTED!">>},{x,0}}.
            {call_ext_only,1,{extfunc,'Elixir.IO',puts,1}}.


        {function, hello, 0, 13}.
          {label,12}.
            {line,[{location,"lib/maxo_decompile/example_module.ex",14}]}.
            {func_info,{atom,'Elixir.MaxoDecompile.ExampleModule'},{atom,hello},0}.
          {label,13}.
            {move,{literal,<<"hello">>},{x,0}}.
            {line,[{location,"lib/maxo_decompile/example_module.ex",15}]}.
            {call_ext_only,1,{extfunc,'Elixir.IO',puts,1}}.


        {function, 'hey!', 0, 15}.
          {label,14}.
            {line,[{location,"lib/maxo_decompile/example_module.ex",18}]}.
            {func_info,{atom,'Elixir.MaxoDecompile.ExampleModule'},{atom,'hey!'},0}.
          {label,15}.
            {move,{literal,<<"HEY!!!">>},{x,0}}.
            {line,[{location,"lib/maxo_decompile/example_module.ex",19}]}.
            {call_ext_only,1,{extfunc,'Elixir.IO',puts,1}}.


        {function, module_info, 0, 17}.
          {label,16}.
            {line,[]}.
            {func_info,{atom,'Elixir.MaxoDecompile.ExampleModule'},
                       {atom,module_info},
                       0}.
          {label,17}.
            {move,{atom,'Elixir.MaxoDecompile.ExampleModule'},{x,0}}.
            {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


        {function, module_info, 1, 19}.
          {label,18}.
            {line,[]}.
            {func_info,{atom,'Elixir.MaxoDecompile.ExampleModule'},
                       {atom,module_info},
                       1}.
          {label,19}.
            {move,{x,0},{x,1}}.
            {move,{atom,'Elixir.MaxoDecompile.ExampleModule'},{x,0}}.
            {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.


        {function, '-inlined-__info__/1-', 1, 21}.
          {label,20}.
            {line,[]}.
            {func_info,{atom,'Elixir.MaxoDecompile.ExampleModule'},
                       {atom,'-inlined-__info__/1-'},
                       1}.
          {label,21}.
            {jump,{f,20}}.
        """ <- code
      )
    end

    test "works for diffasm" do
      {mod, code} = Core.pure_process("MaxoDecompile.ExampleModule", "diffasm")

      auto_assert(
        """
        %% -*- encoding:latin-1 -*-
        {module, 'Elixir.MaxoDecompile.ExampleModule'}.  %% version = 0

        {exports, [{'_HOLA',0},
                   {'__info__',1},
                   {hello,0},
                   {'hey!',0},
                   {module_info,0},
                   {module_info,1}]}.

        {attributes, []}.

        {labels, 22}.


        {function, '__info__', 1, {'__info__',1}}.
          {label,1}.
            {func_info,{atom,'Elixir.MaxoDecompile.ExampleModule'},
                       {atom,'__info__'},
                       1}.
          {label,{'__info__',1}}.
            {select_val,{x,0},
                        {f,8},
                        {list,[{atom,attributes},
                               {f,7},
                               {atom,compile},
                               {f,7},
                               {atom,deprecated},
                               {f,6},
                               {atom,exports_md5},
                               {f,5},
                               {atom,functions},
                               {f,4},
                               {atom,macros},
                               {f,6},
                               {atom,md5},
                               {f,7},
                               {atom,module},
                               {f,3},
                               {atom,struct},
                               {f,2}]}}.
          {label,2}.
            {move,{atom,nil},{x,0}}.
            return.
          {label,3}.
            {move,{atom,'Elixir.MaxoDecompile.ExampleModule'},{x,0}}.
            return.
          {label,4}.
            {move,{literal,[{'_HOLA',0},{hello,0},{'hey!',0}]},{x,0}}.
            return.
          {label,5}.
            {move,{literal,<<46,173,65,123,148,7,222,235,109,244,27,209,250,158,239,
                             154>>},
                  {x,0}}.
            return.
          {label,6}.
            {move,nil,{x,0}}.
            return.
          {label,7}.
            {move,{x,0},{x,1}}.
            {move,{atom,'Elixir.MaxoDecompile.ExampleModule'},{x,0}}.
            {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.
          {label,8}.
            {call_only,1,{f,{'-inlined-__info__/1-',1}}}.


        {function, '_HOLA', 0, {'_HOLA',0}}.
          {label,1}.
            {func_info,{atom,'Elixir.MaxoDecompile.ExampleModule'},{atom,'_HOLA'},0}.
          {label,{'_HOLA',0}}.
            {move,{literal,<<"I'M INJECTED!">>},{x,0}}.
            {call_ext_only,1,{extfunc,'Elixir.IO',puts,1}}.


        {function, hello, 0, {hello,0}}.
          {label,1}.
            {func_info,{atom,'Elixir.MaxoDecompile.ExampleModule'},{atom,hello},0}.
          {label,{hello,0}}.
            {move,{literal,<<"hello">>},{x,0}}.
            {call_ext_only,1,{extfunc,'Elixir.IO',puts,1}}.


        {function, 'hey!', 0, {'hey!',0}}.
          {label,1}.
            {func_info,{atom,'Elixir.MaxoDecompile.ExampleModule'},{atom,'hey!'},0}.
          {label,{'hey!',0}}.
            {move,{literal,<<"HEY!!!">>},{x,0}}.
            {call_ext_only,1,{extfunc,'Elixir.IO',puts,1}}.


        {function, module_info, 0, {module_info,0}}.
          {label,1}.
            {func_info,{atom,'Elixir.MaxoDecompile.ExampleModule'},
                       {atom,module_info},
                       0}.
          {label,{module_info,0}}.
            {move,{atom,'Elixir.MaxoDecompile.ExampleModule'},{x,0}}.
            {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


        {function, module_info, 1, {module_info,1}}.
          {label,1}.
            {func_info,{atom,'Elixir.MaxoDecompile.ExampleModule'},
                       {atom,module_info},
                       1}.
          {label,{module_info,1}}.
            {move,{x,0},{x,1}}.
            {move,{atom,'Elixir.MaxoDecompile.ExampleModule'},{x,0}}.
            {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.


        {function, '-inlined-__info__/1-', 1, {'-inlined-__info__/1-',1}}.
          {label,1}.
            {func_info,{atom,'Elixir.MaxoDecompile.ExampleModule'},
                       {atom,'-inlined-__info__/1-'},
                       1}.
          {label,{'-inlined-__info__/1-',1}}.
            {jump,{f,1}}.
        """ <- code
      )
    end
  end

  def pretty_elixir_code(code) do
    IO.iodata_to_binary(code) |> Code.format_string!() |> IO.iodata_to_binary()
  end
end
