defmodule MaxoDecompile.ExampleModule.Extension do
  defmacro __using__(_) do
    quote do
      def _HOLA do
        IO.puts("I'M INJECTED!")
      end
    end
  end
end

defmodule MaxoDecompile.ExampleModule do
  use MaxoDecompile.ExampleModule.Extension

  def hello do
    IO.puts("hello")
  end

  def hey! do
    IO.puts("HEY!!!")
  end
end
