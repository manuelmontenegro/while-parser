defmodule WhileParser do
  alias WhileParser.{Parser, JSONConverter}

  def parse_to_json(string, options \\ []) do
    with {:ok, ast} <- Parser.parse(string) do
      JSONConverter.to_json!(ast, options)
    else
      {:error, {line_no, _, msg}} -> {:error, {line_no, Enum.join(msg) |> fix_eof_error()}}
    end
  end

  defp fix_eof_error("syntax error before: "), do: "unexpected end of file"
  defp fix_eof_error(str), do: str
end
