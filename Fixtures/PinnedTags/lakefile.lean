import Lake
open Lake DSL

package "PinnedTags" where
  version := v!"0.1.0"

require batteries from git
  "https://github.com/leanprover-community/batteries" @ "v4.31.0"

@[default_target]
lean_lib «PinnedTags» where
