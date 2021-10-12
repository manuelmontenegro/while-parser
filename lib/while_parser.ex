# Copyright 2021 Manuel Montenegro
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

defmodule WhileParser do
  @moduledoc """
  A parser for the _While_ language.
  """

  @typedoc """
  An error result, consisting of a line number in the source code where the error takes place,
  and an error message.
  """
  @type parse_error() :: {non_neg_integer(), String.t()}

  alias WhileParser.{Parser, JSONConverter, CFG, CFG.CFGBlock}

  @doc """
  Returns the JSON representation of the Abstract Syntax Tree of the program given
  as parameter.

  It receives a `string` with the source code of the program to parse, and a list of
  `options` which are subsequently passed to [Jason](https://hex.pm/packages/jason), the underlying JSON decoder.
  Use `pretty: true` to obtain pretty-printed JSON output.

  It returns either the resulting JSON-encoded string or a `parse_error()`
  """
  @spec parse_to_json(String.t() | [char()], [Jason.encode_opt()]) ::
          {:ok, String.t()} | {:error, parse_error()}
  def parse_to_json(string, options \\ []) do
    with {:ok, ast} <- Parser.parse(string) do
      {:ok, JSONConverter.to_json!(ast, options)}
    else
      {:error, {line_no, _, msg}} -> {:error, {line_no, Enum.join(msg) |> fix_eof_error()}}
    end
  end

  @doc """
  Returns the Abstract Syntax Tree of the program given as parameter as an Erlang map.

  It receives a `string` with the source code of the program to parse.

  It returns either an AST or a `parse_error()`
  """
  @spec parse_to_ast(String.t() | [char()]) :: {:ok, any()} | {:error, parse_error()}
  def parse_to_ast(string) do
    with {:ok, ast} <- Parser.parse(string) do
      {:ok, ast |> JSONConverter.to_map()}
    else
      {:error, {line_no, _, msg}} -> {:error, {line_no, Enum.join(msg) |> fix_eof_error()}}
    end
  end

  @doc """
  Returns the Control Flow Graph of the program given as parameter.

  It receives a `string` with the source code of the program to parse.

  Only the main program is taken into account. Function definitions are ignored.

  It returns either an AST or a `parse_error()` or an `:unsupported` error in case the given program contains
  unsupported statements (`ifnil`, tuple matching, etc.)
  """
  @spec parse_to_cfg(String.t() | [char()]) ::
          {:ok, CFG.t(), CFGBlock.label()} | {:error, parse_error()} | {:error, {:unsupported, term()}}
  def parse_to_cfg(string) do
    with {:ok, {:program, :program, _, kw}} <- Parser.parse(string) do
      CFG.to_cfg(kw[:main_stm])
    else
      {:error, {line_no, _, msg}} -> {:error, {line_no, Enum.join(msg) |> fix_eof_error()}}
    end
  end

  @doc """
  The same as `parse_to_cfg/1` but returns a JSON representation instead.

  It receives a `string` with the source code of the program to parse, and a list of
  `options` which are subsequently passed to [Jason](https://hex.pm/packages/jason), the underlying JSON decoder.
  Use `pretty: true` to obtain pretty-printed JSON output.

  Only the main program is taken into account. Function definitions are ignored.


  It returns either a JSON-encoded string or a `parse_error()` or an `:unsupported` error in case the given program contains
  unsupported statements (`ifnil`, tuple matching, etc.)
  """
  @spec parse_to_cfg_json(String.t() | [char()],  [Jason.encode_opt()]) ::
          {:ok, CFG.t(), CFGBlock.label()} | {:error, parse_error()} | {:error, {:unsupported, term()}}
  def parse_to_cfg_json(string, options \\ []) do
    with {:ok, cfg, init} <- parse_to_cfg(string) do
      {:ok, JSONConverter.to_json!(cfg, options), init}
    end
  end

  defp fix_eof_error("syntax error before: "), do: "unexpected end of file"
  defp fix_eof_error(str), do: str
end
