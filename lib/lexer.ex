defmodule WhileParser.Lexer do
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
    {:error, line_number, @lexer_erlang_module.format_error(error) |> Enum.join() }
  end
end
