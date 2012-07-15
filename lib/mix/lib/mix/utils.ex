defmodule Mix.Utils do
  @moduledoc """
  Utilities used throughout Mix and tasks.

  ## Command names and module names

  Throughout this module (and Mix), we use two main terms:

  * command names: are names as given from the command line;
    usually all items are in lowercase and uses dashes instead
    of underscores;

  * module names: valid module names according to Elixir;
  
  Some tasks in this module works exactly with converting
  from one to the other. See `command_to_module_name/2`,
  `module_name_to_command/2`, `get_module/2`.
  """

  @doc """
  Takes a `command` name and try to load a module
  with the command name converted to a module name
  in the given `at` scope.

  Returns `{ :module, module }` in case a module
  exists and is loaded, `{ :error, reason }` otherwise.

  ## Examples

      Mix.Utils.get_module("compile", Mix.Tasks)
      #=> { :module, Mix.Tasks.Compile }

  """
  def get_module(command, at // __MAIN__) do
    module = Module.concat(at, command_to_module_name(command))
    Code.ensure_loaded(module)
  end

  @doc """
  Takes a module and converts it to a command. The nesting
  argument can be given in order to remove the nesting of
  module.

  ## Examples

      module_name_to_command(Mix.Tasks.Compile, 2)
      #=> "compile"

      module_name_to_command("Mix.Tasks.Compile.Elixir", 2)
      #=> "compile.elixir"

  """
  def module_name_to_command(module, nesting // 0)

  def module_name_to_command(module, nesting) when is_atom(module) do
    module_name_to_command(inspect(module), nesting)
  end

  def module_name_to_command(module, nesting) do
    t = Regex.split(%r/\./, to_binary(module))
    t /> Enum.drop(nesting) /> Enum.map(to_lower(&1)) /> Enum.join(".")
  end

  @doc """
  Takes a command and converts it to a module name format.
  
  ## Examples

      command_to_module_name("compile.elixir")
      #=> "Compile.Elixir"

  """
  def command_to_module_name(s) do
    Regex.split(%r/\./, to_binary(s)) />
      Enum.map(to_upper(&1)) />
      Enum.join(".")
  end

  defp to_upper(<<s, t|:binary>>), do: <<:string.to_upper(s)>> <> t
  defp to_upper(<<>>), do: <<>>

  defp to_lower(<<s, t|:binary>>), do: <<:string.to_lower(s)>> <> t
  defp to_lower(<<>>), do: <<>>
end