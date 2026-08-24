module

import LeanUpdate.BumpPinnedDeps

namespace LeanUpdateTest.PinnedTagFallback

def check (what : String) (tags : Array String) (target : String) (expected : Option String) :
    IO Unit := do
  let got := pickNewestNotExceeding tags target
  unless got == expected do
    throw <| IO.userError s!"{what}: pick {tags} at {target} gave {got}, expected {expected}"

/--
Lean ships patch releases that most of the ecosystem never tags — there is no `batteries`
`v4.33.1` — so a dependency that cannot match the toolchain exactly moves to the newest tag it
does publish below it, rather than staying stranded at its old pin.
-/
public def test : IO Unit := do
  check "exact tag wins when published"
    #["v4.32.0", "v4.33.0", "v4.33.1"] "v4.33.1" (some "v4.33.1")

  check "an untagged patch release falls back to the release below it"
    #["v4.31.0", "v4.32.0", "v4.33.0"] "v4.33.1" (some "v4.33.0")

  check "a tag newer than the toolchain is never selected"
    #["v4.33.0", "v4.34.0", "v4.34.0-rc1"] "v4.33.1" (some "v4.33.0")

  check "a stable target skips pre-releases that sort higher"
    #["v4.33.0", "v4.33.1-rc1"] "v4.33.1" (some "v4.33.0")

  check "a pre-release target may land on a pre-release"
    #["v4.33.0", "v4.33.1-rc1"] "v4.33.1-rc2" (some "v4.33.1-rc1")

  check "nothing at or below the target means the pin cannot move"
    #["v4.34.0"] "v4.33.1" none

  check "tags that are not Lean versions are ignored"
    #["main", "nightly", "v1"] "v4.33.1" none

  check "an unreachable remote reports no tags and cannot move"
    #[] "v4.33.1" none

end LeanUpdateTest.PinnedTagFallback
