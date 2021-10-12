# A parser for the While language

This is an auxiliary library used for other projects related to the course on _Static Analysis of Programs and Constraint Solving_.

Notice that this is only a parser. It checks whether a given string matches the syntax definition of _While_. It does _not_ check any other kind of contextual constraint, such as well-typedness, duplicate definitions, definition-before-use, etc.

The complete definition of the syntax can be found in [Syntax.md](Syntax.md). The corresponding BNF grammar is defined in the [parser_erlang.yrl](src/parser_erlang.yrl) file.

This module can also generate control-flow graphs for a given while program. Note that only basic constructs are allowed (`skip`, assignments, conditional statements and `while` loops).

## Installation

If [available in Hex](https://hex.pm/docs/publish), the package can be installed
by adding `while_parser` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:while_parser, github: "manuelmontenegro/while_parser"}
  ]
end
```

Documentation can be generated with [ExDoc](https://github.com/elixir-lang/ex_doc)
and published on [HexDocs](https://hexdocs.pm). Once published, the docs can
be found at [https://hexdocs.pm/while_parser](https://hexdocs.pm/while_parser).

