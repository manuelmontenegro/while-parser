# Copyright 2019 Manuel Montenegro
#
# Permission is hereby granted, free of charge, to any person obtaining a copy of this software
# and associated documentation files (the "Software"), to deal in the Software without restriction,
# including without limitation the rights to use, copy, modify, merge, publish, distribute,
# sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all copies or substantial
# portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
# NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
# NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES
# OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
# CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

defmodule WhileParser.Lexer do
  @moduledoc """
  An Elixir wrapper to the automatically generated `lexer_erlang` module. For internal use only.

  Use `WhileParser` module instead.
  """

  @type line_number :: integer
  @type error_msg :: String.t()

  @type token :: {atom, line_number, any} | {atom, line_number}

  @lexer_erlang_module :lexer_erlang

  @spec lexer(String.t() | charlist) :: {:ok, [token]} | {:error, line_number, String.t()}
  def lexer(string) when is_binary(string) do
    string
    |> String.to_charlist()
    |> lexer()
  end

  def lexer(charlist) when is_list(charlist) do
    charlist
    |> @lexer_erlang_module.string()
    |> prepare_result()
  end

  defp prepare_result({:ok, tokens, _}), do: {:ok, tokens}

  defp prepare_result({:error, {line_number, @lexer_erlang_module, error}, _}) do
    {:error, line_number, @lexer_erlang_module.format_error(error) |> Enum.join()}
  end
end
