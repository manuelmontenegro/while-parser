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

defmodule WhileParser.CFG do
  @moduledoc """
  Functions for transforming a statement into a Control-Flow based representation. For internal use only.

  Use `WhileParser` module instead.
  """

  defmodule CFGBlock do
    @moduledoc """
    This module defines the data types that store the properties of a single block in a control-flow graph.

    Each block is identified by a label, and may contain either a `skip` statement, a single assignment or
    a condition. A block in a CFG may have outgoing edges to other blocks, or to the end of the program.
    """

    @typedoc """
    Type of the label that identifies a block. It is an intenger number.
    """
    @type label() :: integer()


    @typedoc """
    Enumeration of the different block kinds: skip, assignment or conditions
    """
    @type blocktype() :: :skip | :assignment | :condition

    @typedoc """
    Type of a block in a control-flow graph. The `contents` field is a keyword list that
    depends on the value of `type`:

      * If type is `:skip`, the field `contents` has no elements.
      * If type is `:assignment`, the field `contents` contains two keys (`lhs` and `rhs`)
      * If type is `:condition`, the field `contents` contains a key (`condition`)
    """
    @type t :: %CFGBlock{
            label: label(),
            type: blocktype(),
            contents: keyword(),
            succs: [label() | :end]
          }
    defstruct [:label, :type, :contents, :succs]
  end

  @typedoc """
  Type of a control-flow graph. It is a list of CFG blocks.
  """
  @type t() :: [CFGBlock.t()]


  @typep counter() :: pid()

  @spec counter_init() :: {:ok, counter()}
  defp counter_init() do
    Agent.start_link(fn -> 1 end)
  end

  @spec counter_next(counter()) :: CFGBlock.label()
  defp counter_next(c) do
    Agent.get_and_update(c, &{&1, &1 + 1})
  end

  @spec patch_end(CFGBlock.t(), CFGBlock.label()) :: CFGBlock.t()
  defp patch_end(%CFGBlock{} = cfgbl, lab) do
    replace_by_lab = fn
      :end -> lab
      x -> x
    end

    %CFGBlock{cfgbl | succs: cfgbl.succs |> Enum.map(replace_by_lab)}
  end

  @spec patch_end_cfg(t(), CFGBlock.label()) :: t()
  defp patch_end_cfg(cfg, lab) do
    cfg |> Enum.map(&patch_end(&1, lab))
  end

  @spec to_cfg(any(), counter()) :: {:ok, t(), CFGBlock.label()} | {:error, any()}
  defp to_cfg(stm, c)

  defp to_cfg({:stm, :skip, _, _}, c) do
    lab = counter_next(c)

    {:ok,
     [
       %CFGBlock{
         label: lab,
         type: :skip,
         contents: [],
         succs: [:end]
       }
     ], lab}
  end

  defp to_cfg([stm], c) do
    to_cfg(stm, c)
  end

  defp to_cfg([stm | stms], c) do
    with {:ok, cfg1, init1} <- to_cfg(stm, c),
         {:ok, cfg2, init2} <- to_cfg(stms, c) do
      {:ok, patch_end_cfg(cfg1, init2) ++ cfg2, init1}
    end
  end

  defp to_cfg({:stm, :assignment, _, opts}, c) do
    x = Keyword.get(opts, :lhs)
    exp = Keyword.get(opts, :rhs)
    lab = counter_next(c)

    {:ok,
     [
       %CFGBlock{
         label: lab,
         type: :assignment,
         contents: [lhs: x, rhs: exp],
         succs: [:end]
       }
     ], lab}
  end

  defp to_cfg({:stm, :if, _, opts}, c) do
    cond = Keyword.get(opts, :condition)
    stm_then = Keyword.get(opts, :then)
    stm_else = Keyword.get(opts, :else)
    lab = counter_next(c)

    with {:ok, cfg_then, init_then} <- to_cfg(stm_then, c),
         {:ok, cfg_else, init_else} <- to_cfg(stm_else, c) do
      cond_block = %CFGBlock{
        label: lab,
        type: :condition,
        contents: [condition: cond],
        succs: [init_then, init_else]
      }

      {:ok, [cond_block] ++ cfg_then ++ cfg_else, lab}
    end
  end

  defp to_cfg({:stm, :while, _, opts}, c) do
    cond = Keyword.get(opts, :condition)
    body = Keyword.get(opts, :body)

    lab = counter_next(c)

    with {:ok, cfg_body, init_body} <- to_cfg(body, c) do
      cond_block = %CFGBlock{
        label: lab,
        type: :condition,
        contents: [condition: cond],
        succs: [init_body, :end]
      }

      cfg_body = patch_end_cfg(cfg_body, lab)
      {:ok, [cond_block | cfg_body], lab}
    end
  end

  defp to_cfg(stm, _) do
    {:error, {:unsupported, stm}}
  end

  @doc """
  Given the AST of a statement, it returns its corresponding CFG.

  If succeeds, it returns a tuple `{:ok, cfg, init}`, where `init` is the label
  of the entry block in the statement.
  """

  @spec to_cfg(any()) :: {:ok, t(), CFGBlock.label()} | {:error, any()}
  def to_cfg(stm) do
    {:ok, c} = counter_init()
    to_cfg(stm, c)
  end
end
