defmodule WhileParser.JSONConverter do

  @valid_categories [:exp, :stm, :program, :declaration, :type]

  defp to_map(x) when is_integer(x), do: x
  defp to_map(x) when is_binary(x), do: x
  defp to_map(true), do: true
  defp to_map(false), do: false
  defp to_map(list) when is_list(list), do: list |> Enum.map(&to_map/1)
  defp to_map({category, type, line, opts}) when category in @valid_categories do
    %{
      category: to_string(category),
      category_sub: to_string(type),
      line: line,
      options: opts |> Enum.map(fn {k,v} -> {k, to_map(v)} end) |> Enum.into(%{})
    }
  end

  def to_json!(ast, options \\ []) do
    ast |> to_map() |> Jason.encode!(options)
  end

end
