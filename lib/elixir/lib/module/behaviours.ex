defmodule Module.Behaviours do
  # Checking functionality for @behaviours and @impl
  @moduledoc false

  alias Module.ParallelChecker

  ## Behaviour

  @doc false
  def warnings(module, file, behaviours, impls, cache) do
    context = context(module, file, cache)

    context = check_behaviours(context, behaviours)

    pending_callbacks =
      if impls != [] do
        {non_implemented_callbacks, context} = check_impls(env, behaviours, callbacks, impls)
        warn_missing_impls(env, non_implemented_callbacks, contexts, all_definitions)
        non_implemented_callbacks
      else
        callbacks
      end

    context = check_callbacks(context, pending_callbacks, all_definitions)

    context.warnings
  end

  defp context() do
    %{
      cache: nil,
      file: nil,
      module: nil,
      callbacks: %{},
      warnings: []
    }
  end

  defp behaviour_statement() do
    %{
      line: 0,
      behaviour: nil
    }
  end

  defp check_behaviours(context, behaviours) do
    for behaviour <- behaviours, reduce: context do
      context ->
        ParallelChecker.preload_module(context.cache, behaviour_statement.behaviour)

        case ParallelChecker.fetch_export(
               context.cache,
               behaviour_statement.behaviour,
               :behaviour_info,
               1
             ) do
          {:error, :module} ->
            warn_behaviour(context, behaviour_statement, :undefined_behaviour)

          {:error, :function} ->
            warn_behaviour(context, behaviour_statement, :module_does_not_define_behaviour)

          {:ok, _mode, _def, _reason} ->
            optional_callbacks = behaviour_info(behaviour, :optional_callbacks)
            callbacks = behaviour_info(behaviour, :callbacks)

            Enum.reduce(
              callbacks,
              context,
              &add_callback(&2, &1, behaviour, env, optional_callbacks)
            )
        end
    end
  end

  defp add_callback(context, original, behaviour, env, optional_callbacks) do
    {callback, kind} = normalize_macro_or_function_callback(original)

    context = case context.callbacks do
      %{^callback => {_kind, conflict, _optional?}} ->
          if conflict == behaviour do
            warn_behaviour(context, behaviour_statement, :duplicate_behaviour)
          else
            warn_behaviour(context, behaviour_statement, :conflicting_behaviour)
          end

      %{} ->
        context
    end

    put_in(context.callbacks[callback], {kind, behaviour, original in optional_callbacks})
  end


  @spec check_behaviour(behaviour_statement, context) :: warning | :ok
  defp warn_behaviour(context, behaviour_statement, warning_type) do
    location =
      {context.file, behaviour_statement.line, {context.module, _fun = nil, _arity = nil}}

    error = {warning_type, behaviour_statement.behaviour}

    warn(context, {__MODULE__, error, location})
  end

  # defp check_impls(context, callbacks, impls) do
  #   pending_callbacks =
  #     if impls != [] do
  #       {non_implemented_callbacks, contexts} = check_impls(env, behaviours, callbacks, impls)
  #       warn_missing_impls(env, non_implemented_callbacks, contexts, all_definitions)
  #       non_implemented_callbacks
  #     else
  #       callbacks
  #     end

  #   check_callbacks(env, pending_callbacks, all_definitions)
  #   end

  defp warn(context, warning) do
    %{context | warnings: [warning | context.warnings]}
  end

  #####

  ###

  def format_warning({:undefined_behaviour, behaviour}) do
    [
      "@behaviour ",
      inspect(behaviour),
      " does not exist"
    ]
  end

  def format_warning({:module_does_not_define_behaviour, behaviour}) do
    ["module ", inspect(behaviour), " is not a behaviour"]
  end

  def format_warning({:duplicate_behaviour, behaviour}) do
    ["the behavior ",
    inspect(behaviour),
    " has been declared twice"
  ]
  end

  def format_warning({:conflicting_behaviour, behaviour, conflict, kind, callback}) do
    ["conflicting behaviours found. ",
    format_definition(kind, callback),
    " is required by ",
    inspect(conflict),
    " and ",
    inspect(behaviour)
  ]
  end
end
