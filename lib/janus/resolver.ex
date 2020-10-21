defmodule Janus.Resolver do
  @moduledoc false

  defstruct id: nil,
            input: [],
            output: [],
            resolve: nil

  @type t :: %__MODULE__{
          id: id,
          input: input,
          output: output,
          resolve: resolve_fun
        }

  @type id :: Janus.attr()
  @type input :: [Janus.attr()]
  @type output :: [output_attr, ...] | composed_output
  @type output_attr :: Janus.attr() | composed_output
  @type composed_output :: %{required(Janus.attr()) => output}

  @type input_map :: %{optional(Janus.attr()) => any}
  @type output_map :: %{required(Janus.attr()) => [output_map] | any}
  @type resolve_fun :: (input_map, Janus.env() -> output_map)

  @spec new(id, input, output, resolve_fun, keyword) :: t
  def new(id, input, output, resolve, _opts \\ []) do
    %__MODULE__{
      id: id,
      input: input,
      output: output,
      resolve: resolve
    }
  end
end
