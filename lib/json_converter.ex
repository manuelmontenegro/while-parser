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

defmodule WhileParser.JSONConverter do
  @moduledoc """
  Functions for converting an AST representation into JSON. For internal use only.

  Use `WhileParser` module instead.
  """

  @valid_categories [:exp, :stm, :program, :declaration, :type]

  def to_map(x) when is_integer(x), do: x
  def to_map(x) when is_binary(x), do: x
  def to_map(true), do: true
  def to_map(false), do: false
  def to_map(list) when is_list(list), do: list |> Enum.map(&to_map/1)

  def to_map({category, type, line, opts}) when category in @valid_categories do
    %{
      category: to_string(category),
      category_sub: to_string(type),
      line: line,
      options: opts |> Enum.map(fn {k, v} -> {k, to_map(v)} end) |> Enum.into(%{})
    }
  end

  def to_json!(ast, options \\ []) do
    ast |> to_map() |> Jason.encode!(options)
  end
end
