import Lake
open Lake DSL

package "PinnedSha" where
  version := v!"0.1.0"

require batteries from git
  "https://github.com/leanprover-community/batteries" @ "fa08db58b30eb033edcdab331bba000827f9f785"

@[default_target]
lean_lib «PinnedSha» where
