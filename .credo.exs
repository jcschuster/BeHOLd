# This file contains the configuration for Credo and you are probably reading
# this after creating it with `mix credo.gen.config`.
#
# If you find anything wrong or unclear in this file, please report an issue
# on GitHub: https://github.com/rrrene/credo/issues
#
%{
  #
  # You can have as many configs as you like in the `configs:` list.
  configs: [
    %{
      #
      # Run any exec using `mix credo -C <name>`. If no exec name is given
      # "default" is used.
      #
      name: "default",
      #
      # These are the files included in the analysis
      #
      files: %{
        included: ["lib/", "test/"],
        excluded: []
      },
      #
      # Load and configure plugins here:
      #
      plugins: [],
      #
      # If you create your own checks, you must specify the source files for
      # them here, so they can be loaded by Credo before any other checks.
      #
      requires: [],
      #
      # If you want to enforce a style that goes beyond what is spec'ed by the
      # "strict" mode, you can enable additional strictness with overrides.
      #
      # There are three supported overrides that can be used to modify the params
      # that get passed to any check:
      #
      # 1. Elixir Standard: `elixir_version`
      #
      # 2. Custom Parameter: `params`
      #
      #
      # 3. Disable the check completely
      #
      # If a check is disabled that way "Credo" will log a message at the
      # "info" level.
      #
      overrides: [
        %{
          check: Credo.Check.Design.TagTODO,
          params: [exit_status: 0]
        },
        %{
          check: Credo.Check.Design.TagFIXME,
          params: [exit_status: 0]
        }
      ]
    }
  ]
}
