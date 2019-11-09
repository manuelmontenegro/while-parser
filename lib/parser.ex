defmodule WhileParser.Parser do
  def parse(string) do
    with {:ok, tokens} <- WhileParser.Lexer.lexer(string) do
      :parser_erlang.parse(tokens)
    end
  end
end
