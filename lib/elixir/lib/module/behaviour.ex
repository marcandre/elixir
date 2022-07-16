defmodule Module.Behaviour do
  # Checking functionality for @behaviours and @impl
  @moduledoc false

  @doc false
  def callbacks(behaviour) do
    for callback <- behaviour_info(behaviour, :callbacks) do
      {pair, _kind} = normalize_macro_or_function_callback(callback)
      pair
    end
  end

  @doc false
  def check_behaviours_and_impls(env, behaviours, impls, all_definitions) do
    context = check_behaviours(context(env), behaviours)

    context =
      if impls != [] do
        {context, impl_contexts} = check_impls(context, behaviours, impls)

        warn_missing_impls(context, impl_contexts, all_definitions)
      else
        context
      end

    check_callbacks(context, all_definitions)
  end

  defp context(env) do
    %{
      # Macro.Env
      env: env,
      # Map containing the non implemented being built
      callbacks: %{}
    }
  end

  defp check_behaviours(context, behaviours) do
    Enum.reduce(behaviours, context, fn behaviour, context ->
      cond do
        not Code.ensure_loaded?(behaviour) ->
          message =
            "@behaviour #{inspect(behaviour)} does not exist (in module #{inspect(context.env.module)})"

          IO.warn(message, context.env)
          context

        not function_exported?(behaviour, :behaviour_info, 1) ->
          message =
            "module #{inspect(behaviour)} is not a behaviour (in module #{inspect(context.env.module)})"

          IO.warn(message, context.env)
          context

        true ->
          :elixir_env.trace({:require, [from_macro: true], behaviour, []}, context.env)
          optional_callbacks = behaviour_info(behaviour, :optional_callbacks)
          callbacks = behaviour_info(behaviour, :callbacks)
          Enum.reduce(callbacks, context, &add_callback(&2, &1, behaviour, optional_callbacks))
      end
    end)
  end

  defp add_callback(context, original, behaviour, optional_callbacks) do
    {callback, kind} = normalize_macro_or_function_callback(original)

    case context.callbacks do
      %{^callback => {_kind, conflict, _optional?}} ->
        message =
          if conflict == behaviour do
            "the behavior #{inspect(conflict)} has been declared twice " <>
              "(conflict in #{format_definition(kind, callback)} in module #{inspect(context.env.module)})"
          else
            "conflicting behaviours found. #{format_definition(kind, callback)} is required by " <>
              "#{inspect(conflict)} and #{inspect(behaviour)} (in module #{inspect(context.env.module)})"
          end

        IO.warn(message, context.env)

      %{} ->
        :ok
    end

    put_in(context.callbacks[callback], {kind, behaviour, original in optional_callbacks})
  end

  defp check_callbacks(context, all_definitions) do
    for {callback, {kind, behaviour, optional?}} <- context.callbacks do
      case :lists.keyfind(callback, 1, all_definitions) do
        false when not optional? ->
          message =
            format_callback(callback, kind, behaviour) <>
              " is not implemented (in module #{inspect(context.env.module)})"

          IO.warn(message, context.env)

        {_, wrong_kind, _, _} when kind != wrong_kind ->
          message =
            format_callback(callback, kind, behaviour) <>
              " was implemented as \"#{wrong_kind}\" but should have been \"#{kind}\" " <>
              "(in module #{inspect(context.env.module)})"

          IO.warn(message, context.env)

        _ ->
          :ok
      end
    end

    :ok
  end

  defp format_callback(callback, kind, module) do
    protocol_or_behaviour = if protocol?(module), do: "protocol ", else: "behaviour "

    format_definition(kind, callback) <>
      " required by " <> protocol_or_behaviour <> inspect(module)
  end

  defp protocol?(module) do
    Code.ensure_loaded?(module) and function_exported?(module, :__protocol__, 1) and
      module.__protocol__(:module) == module
  end

  defp check_impls(context, behaviours, impls) do
    all_callbacks = context.callbacks
    acc = {context, %{}}

    Enum.reduce(impls, acc, fn {fa, impl_context, defaults, kind, line, file, value},
                               {context, impl_contexts} = acc ->
      case impl_behaviours(fa, defaults, kind, value, behaviours, all_callbacks) do
        {:ok, impl_behaviours} ->
          Enum.reduce(impl_behaviours, acc, fn {fa, behaviour}, {context, impl_contexts} ->
            {_, context} = pop_in(context.callbacks[fa])

            impl_contexts =
              Map.update(impl_contexts, behaviour, [impl_context], &[impl_context | &1])

            {context, impl_contexts}
          end)

        {:error, message} ->
          formatted = format_impl_warning(fa, kind, message)
          IO.warn(formatted, %{context.env | line: line, file: file})
          {context, impl_contexts}
      end
    end)
  end

  defp impl_behaviours({function, arity}, defaults, kind, value, behaviours, callbacks) do
    impls = for n <- arity..(arity - defaults), do: {function, n}
    impl_behaviours(impls, kind, value, behaviours, callbacks)
  end

  defp impl_behaviours(_, kind, _, _, _) when kind in [:defp, :defmacrop] do
    {:error, :private_function}
  end

  defp impl_behaviours(_, _, value, [], _) do
    {:error, {:no_behaviours, value}}
  end

  defp impl_behaviours(impls, _, false, _, callbacks) do
    case callbacks_for_impls(impls, callbacks) do
      [] -> {:ok, []}
      [impl | _] -> {:error, {:impl_not_defined, impl}}
    end
  end

  defp impl_behaviours(impls, _, true, _, callbacks) do
    case callbacks_for_impls(impls, callbacks) do
      [] -> {:error, {:impl_defined, callbacks}}
      impls -> {:ok, impls}
    end
  end

  defp impl_behaviours(impls, _, behaviour, behaviours, callbacks) do
    filtered = behaviour_callbacks_for_impls(impls, behaviour, callbacks)

    cond do
      filtered != [] ->
        {:ok, filtered}

      behaviour not in behaviours ->
        {:error, {:behaviour_not_declared, behaviour}}

      true ->
        {:error, {:behaviour_not_defined, behaviour, callbacks}}
    end
  end

  defp behaviour_callbacks_for_impls([], _behaviour, _callbacks) do
    []
  end

  defp behaviour_callbacks_for_impls([fa | tail], behaviour, callbacks) do
    case callbacks[fa] do
      {_, ^behaviour, _} ->
        [{fa, behaviour} | behaviour_callbacks_for_impls(tail, behaviour, callbacks)]

      _ ->
        behaviour_callbacks_for_impls(tail, behaviour, callbacks)
    end
  end

  defp callbacks_for_impls([], _) do
    []
  end

  defp callbacks_for_impls([fa | tail], callbacks) do
    case callbacks[fa] do
      {_, behaviour, _} -> [{fa, behaviour} | callbacks_for_impls(tail, callbacks)]
      nil -> callbacks_for_impls(tail, callbacks)
    end
  end

  defp format_impl_warning(fa, kind, :private_function) do
    "#{format_definition(kind, fa)} is private, @impl attribute is always discarded for private functions/macros"
  end

  defp format_impl_warning(fa, kind, {:no_behaviours, value}) do
    "got \"@impl #{inspect(value)}\" for #{format_definition(kind, fa)} but no behaviour was declared"
  end

  defp format_impl_warning(_, kind, {:impl_not_defined, {fa, behaviour}}) do
    "got \"@impl false\" for #{format_definition(kind, fa)} " <>
      "but it is a callback specified in #{inspect(behaviour)}"
  end

  defp format_impl_warning(fa, kind, {:impl_defined, callbacks}) do
    "got \"@impl true\" for #{format_definition(kind, fa)} " <>
      "but no behaviour specifies such callback#{known_callbacks(callbacks)}"
  end

  defp format_impl_warning(fa, kind, {:behaviour_not_declared, behaviour}) do
    "got \"@impl #{inspect(behaviour)}\" for #{format_definition(kind, fa)} " <>
      "but this behaviour was not declared with @behaviour"
  end

  defp format_impl_warning(fa, kind, {:behaviour_not_defined, behaviour, callbacks}) do
    "got \"@impl #{inspect(behaviour)}\" for #{format_definition(kind, fa)} " <>
      "but this behaviour does not specify such callback#{known_callbacks(callbacks)}"
  end

  defp warn_missing_impls(%{callbacks: callbacks} = context, _impl_contexts, _defs)
       when map_size(callbacks) == 0 do
    context
  end

  defp warn_missing_impls(context, impl_contexts, defs) do
    for {pair, kind, meta, _clauses} <- defs,
        kind in [:def, :defmacro] do
      with {:ok, {_, behaviour, _}} <- Map.fetch(context.callbacks, pair),
           true <- missing_impl_in_context?(meta, behaviour, impl_contexts) do
        message =
          "module attribute @impl was not set for #{format_definition(kind, pair)} " <>
            "callback (specified in #{inspect(behaviour)}). " <>
            "This either means you forgot to add the \"@impl true\" annotation before the " <>
            "definition or that you are accidentally overriding this callback"

        IO.warn(message, %{context.env | line: :elixir_utils.get_line(meta)})
      end
    end

    context
  end

  defp missing_impl_in_context?(meta, behaviour, impl_contexts) do
    case impl_contexts do
      %{^behaviour => known} -> Keyword.get(meta, :context) in known
      %{} -> not Keyword.has_key?(meta, :context)
    end
  end

  defp format_definition(kind, {name, arity}) do
    format_definition(kind) <> " #{name}/#{arity}"
  end

  defp format_definition(:defmacro), do: "macro"
  defp format_definition(:defmacrop), do: "macro"
  defp format_definition(:def), do: "function"
  defp format_definition(:defp), do: "function"

  defp known_callbacks(callbacks) when map_size(callbacks) == 0 do
    ". There are no known callbacks, please specify the proper @behaviour " <>
      "and make sure it defines callbacks"
  end

  defp known_callbacks(callbacks) do
    formatted_callbacks =
      for {{name, arity}, {kind, module, _}} <- callbacks do
        "\n  * " <> Exception.format_mfa(module, name, arity) <> " (#{format_definition(kind)})"
      end

    ". The known callbacks are:\n#{formatted_callbacks}\n"
  end

  defp behaviour_info(module, key) do
    case module.behaviour_info(key) do
      list when is_list(list) -> list
      :undefined -> []
    end
  end

  defp normalize_macro_or_function_callback({function_name, arity}) do
    case :erlang.atom_to_list(function_name) do
      # Macros are always provided one extra argument in behaviour_info/1
      [?M, ?A, ?C, ?R, ?O, ?-] ++ tail ->
        {{:erlang.list_to_atom(tail), arity - 1}, :defmacro}

      _ ->
        {{function_name, arity}, :def}
    end
  end
end
