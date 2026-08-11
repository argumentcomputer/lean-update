module

import LeanUpdate.IO
import LeanUpdate.Terminal
import LeanUpdate.GitHub.Action.Env
import LeanUpdate.GitHub.Action.Input
import LeanUpdate.Input
import LeanUpdate.UpdateLeanToolchain
import LeanUpdate.FindDep

open IO Process System

/-- A `[[require]]` block parsed from a `lakefile.toml`. -/
structure RequireBlock where
  /-- the `name = "..."` value of the require, if present -/
  name : Option String
  /-- the `git = "..."` value of the require, if present -/
  git : Option String
  /-- the `rev = "..."` value of the require, if present -/
  rev : Option String
  /-- index into the file's line array of the `rev = "..."` line, if present -/
  revLine : Option Nat
deriving Inhabited

/-- Extract the first double-quoted substring of a line,
e.g. `rev = "v4.32.0"` yields `v4.32.0`. -/
def extractQuoted (s : String) : Option String :=
  match s.splitOn "\"" with
  | _ :: v :: _ => some v
  | _ => none

#guard extractQuoted "rev = \"v4.32.0\"" == some "v4.32.0"

#guard extractQuoted "rev = v4.32.0" == none

/-- If `line` assigns `key` (i.e. it has the form `key = "..."`), return the quoted value. -/
def tomlKeyValue (line key : String) : Option String :=
  let t := line.trimAscii.copy
  if t.startsWith key then
    let afterKey := (t.drop key.length).copy.trimAscii.copy
    if afterKey.startsWith "=" then extractQuoted line else none
  else
    none

#guard tomlKeyValue "  rev = \"v4.32.0\"" "rev" == some "v4.32.0"

#guard tomlKeyValue "name = \"plausible\"" "rev" == none

/-- Parse the `[[require]]` blocks out of a `lakefile.toml`, given its lines. -/
def parseRequireBlocks (lines : Array String) : Array RequireBlock := Id.run do
  let mut blocks : Array RequireBlock := #[]
  let mut cur : Option RequireBlock := none
  for i in [0:lines.size] do
    let line := lines[i]!
    let t := line.trimAscii.copy
    if t.startsWith "[[require]]" then
      if let some b := cur then blocks := blocks.push b
      cur := some ⟨none, none, none, none⟩
    else if t.startsWith "[" then
      -- any other table header ends the current `[[require]]` block
      if let some b := cur then blocks := blocks.push b
      cur := none
    else if let some b := cur then
      let b := match tomlKeyValue line "name" with
        | some v => { b with name := some v }
        | none => b
      let b := match tomlKeyValue line "git" with
        | some v => { b with git := some v }
        | none => b
      let b := match tomlKeyValue line "rev" with
        | some v => { b with rev := some v, revLine := some i }
        | none => b
      cur := some b
  if let some b := cur then blocks := blocks.push b
  return blocks

#guard
  let lines := #[
    "name = \"LSpec\"",
    "",
    "[[require]]",
    "name = \"plausible\"",
    "git = \"https://github.com/leanprover-community/plausible\"",
    "rev = \"v4.31.0\"",
    "",
    "[[lean_lib]]",
    "name = \"LSpec\""
  ]
  let blocks := parseRequireBlocks lines
  blocks.size == 1 &&
    blocks[0]!.name == some "plausible" &&
    blocks[0]!.rev == some "v4.31.0" &&
    blocks[0]!.revLine == some 5

/-- Whether a git remote has a tag with the given name. -/
def remoteHasTag (gitUrl tag : String) : IO Bool := do
  let out ← IO.Process.output {
    cmd := "git"
    args := #["ls-remote", "--tags", gitUrl, s!"refs/tags/{tag}"]
  }
  pure <| out.exitCode == 0 && !out.stdout.trimAscii.copy.isEmpty

/-- Whether a pinned `rev` looks like a Lean version tag, e.g. `v4.32.0` or `v4.33.0-rc1`.

Anything else — a commit hash, a branch name, or a dependency's own version scheme — is a
deliberate pin that must not be rewritten to a Lean tag. -/
def isLeanVersionTag (rev : String) : Bool :=
  match rev.toList with
  | 'v' :: c :: rest => c.isDigit && ('.' ∈ c :: rest)
  | _ => false

#guard isLeanVersionTag "v4.32.0" == true

#guard isLeanVersionTag "v4.33.0-rc1" == true

#guard isLeanVersionTag "e3cb2f741431ce31bf73549fb52316a57368b06f" == false

#guard isLeanVersionTag "main" == false

#guard isLeanVersionTag "v1" == false

/-- Whether a require block is managed by the `PINNED_DEPS` input.

An empty input list means "every git require pinned to a Lean version tag". A dependency pinned
to a commit hash or a branch is left alone, since rewriting it would silently discard the pin;
naming it in `PINNED_DEPS` opts it back in. -/
def isManaged (managedNames : List String) (b : RequireBlock) : Bool :=
  if b.git.isNone || b.rev.isNone || b.revLine.isNone then
    false
  else match managedNames with
    | [] => match b.rev with
      | some r => isLeanVersionTag r
      | none => false
    | names => match b.name with
      | some n => names.contains n
      | none => false

#guard isManaged [] ⟨some "aesop", some "https://x/aesop", some "v4.32.0", some 3⟩ == true

#guard isManaged [] ⟨some "aesop", some "https://x/aesop", some "abc1234", some 3⟩ == false

#guard isManaged ["aesop"] ⟨some "aesop", some "https://x/aesop", some "abc1234", some 3⟩ == true

/-- The last double-quoted substring of a line,
e.g. `"https://..." @ "v4.32.0"` yields `v4.32.0`. -/
def lastQuoted (s : String) : Option String :=
  let parts := (s.splitOn "\"").toArray
  if parts.size ≥ 3 then parts[parts.size - 2]! else none

#guard lastQuoted "  \"https://x/aesop.git\" @ \"v4.32.0\"" == some "v4.32.0"

#guard lastQuoted "require aesop from git" == none

/-- Replace the last double-quoted value on a line, leaving everything else — the indentation,
and in a `lakefile.lean` the git url — untouched. This rewrites both `rev = "v4.32.0"` in a
`lakefile.toml` and `"https://..." @ "v4.32.0"` in a `lakefile.lean`. -/
def setRevLine (line tag : String) : String :=
  let parts := (line.splitOn "\"").toArray
  if parts.size ≥ 3 then
    String.intercalate "\"" (parts.set! (parts.size - 2) tag).toList
  else
    line

#guard setRevLine "  rev = \"v4.31.0\"" "v4.32.0" == "  rev = \"v4.32.0\""

#guard setRevLine "rev = \"abc123\"" "v4.32.0" == "rev = \"v4.32.0\""

#guard setRevLine "  \"https://x/aesop.git\" @ \"v4.31.0\"" "v4.32.0"
  == "  \"https://x/aesop.git\" @ \"v4.32.0\""

/-- Parse `require ... from git ... @ "tag"` declarations out of a `lakefile.lean`.

The git url and the pinned tag may sit on the `require` line or on the lines just after it, so
each declaration is scanned over a small window ending at the next `require`. -/
def parseLeanRequires (lines : Array String) : Array RequireBlock := Id.run do
  let mut blocks : Array RequireBlock := #[]
  for i in [0:lines.size] do
    let t := lines[i]!.trimAscii.copy
    unless t.startsWith "require " do
      continue
    let name := ((t.drop "require ".length).trimAscii.copy.splitOn " ").headD ""
    let mut git : Option String := none
    let mut rev : Option String := none
    let mut revLine : Option Nat := none
    for j in [i:min lines.size (i + 4)] do
      let l := lines[j]!
      if j > i && (l.trimAscii.copy.startsWith "require ") then
        break
      if git.isNone then
        if let some u := extractQuoted l then
          if u.startsWith "http" || u.startsWith "git@" then
            git := some u
      if (l.splitOn "@ \"").length > 1 then
        rev := lastQuoted l
        revLine := some j
        break
    if git.isSome && rev.isSome then
      blocks := blocks.push ⟨some name, git, rev, revLine⟩
  return blocks

#guard
  let lines := #[
    "import Lake",
    "open Lake DSL",
    "",
    "require aesop from git",
    "  \"https://github.com/leanprover-community/aesop.git\" @ \"v4.32.0\"",
    "",
    "package Example"
  ]
  let blocks := parseLeanRequires lines
  blocks.size == 1 &&
    blocks[0]!.name == some "aesop" &&
    blocks[0]!.git == some "https://github.com/leanprover-community/aesop.git" &&
    blocks[0]!.rev == some "v4.32.0" &&
    blocks[0]!.revLine == some 4

#guard
  let lines :=
    #["require batteries from git \"https://github.com/x/batteries\" @ \"v4.31.0\""]
  let blocks := parseLeanRequires lines
  blocks.size == 1 && blocks[0]!.rev == some "v4.31.0" && blocks[0]!.revLine == some 0

/-- Bump one package to the target release. Returns whether any file was written. -/
def bumpPackage (pinnedDeps : PinnedDeps) (target : String) (targetDir : FilePath) : IO Bool := do
  let toolchainFile := targetDir / "lean-toolchain"
  let currentToolchain := (← IO.FS.readFile toolchainFile).trimAscii.copy

  let tomlFile := targetDir / "lakefile.toml"
  let leanFile := targetDir / "lakefile.lean"
  let tomlExists ← tomlFile.pathExists
  let leanExists ← leanFile.pathExists
  unless tomlExists || leanExists do
    IO.println <| log% s!"No lakefile.toml or lakefile.lean found in {targetDir}. Skipping."
    return false

  let lakefile := if tomlExists then tomlFile else leanFile
  let parse := if tomlExists then parseRequireBlocks else parseLeanRequires
  let content ← IO.FS.readFile lakefile
  let lines := (content.splitOn "\n").toArray
  let blocks := parse lines
  let managed := blocks.filter (isManaged pinnedDeps.val)

  -- Say why a dependency is being left behind, so that a bump which silently does nothing is
  -- explainable from the log.
  for b in blocks do
    unless isManaged pinnedDeps.val b do
      if let (some name, some rev) := (b.name, b.rev) then
        let reason :=
          if pinnedDeps.val.isEmpty then
            s!"{rev} is not a Lean version tag; list it in `pinned_deps` to bump it anyway"
          else
            "it is not listed in `pinned_deps`"
        IO.println <| log% s!"Not managing {name}: {reason}."

  -- The toolchain is the thing being updated, so it always moves to the target. Each managed
  -- dependency moves with it when its remote has the target tag; one that lags keeps its pin
  -- and is reported, and post-update validation decides whether the mixture still builds.
  -- Comparing per file also lets a dependency that tagged the release late catch up on a rerun
  -- after the toolchain has already moved.
  let mut newLines := lines
  let mut bumped : Array String := #[]
  for b in managed do
    if b.rev == some target then
      continue
    if let (some url, some idx) := (b.git, b.revLine) then
      if ← remoteHasTag url target then
        newLines := newLines.set! idx (setRevLine newLines[idx]! target)
        bumped := bumped.push (b.name.getD url)
      else
        IO.println <| log%
          s!"{b.name.getD url} has no {target} tag yet; leaving its pin at {b.rev.getD "?"}."

  let toolchainBumped := s!"leanprover/lean4:{target}" != currentToolchain
  if !toolchainBumped && bumped.isEmpty then
    IO.println <| log% s!"Toolchain already at {target} and no dependency can move; nothing to do."
    return false

  unless bumped.isEmpty do
    IO.FS.writeFile lakefile (String.intercalate "\n" newLines.toList)
  if toolchainBumped then
    IO.FS.writeFile toolchainFile s!"leanprover/lean4:{target}\n"
  let what := (if toolchainBumped then ["lean-toolchain"] else [])
    ++ (if bumped.isEmpty then [] else [s!"{bumped.size} dependency rev(s)"])
  IO.println <| log% s!"Bumped to {target}: {String.intercalate " and " what}."

  -- Refresh the manifest for the bumped dependencies. A failure here is reported but not
  -- fatal: the lakefile and toolchain edits still stand, and post-update validation decides
  -- what to do about a broken build.
  unless bumped.isEmpty do
    let out ← IO.Process.lakeOutput targetDir (args := #["update"] ++ bumped)
    unless out.stdout.isEmpty do IO.print out.stdout
    unless out.stderr.isEmpty do IO.print out.stderr
    if out.exitCode != 0 then
      IO.println <| log% s!"warning: `lake update` exited with {out.exitCode}"
  return true

/-- Bump `lean-toolchain` to the latest release for the configured `ReleaseKindToFetch` /
`ReleaseChannel`, moving the pinned Lean-version tags of managed git dependencies along with
it.

The toolchain bump is never gated on the dependencies: updating the toolchain is the point of
this action. A managed dependency whose remote has the target tag is bumped in lockstep and its
manifest entry refreshed; one that has not tagged the release yet keeps its pin and is
reported, and post-update validation decides whether the mixture still builds. Because each
file is compared to the target individually, a dependency that tags the release late catches up
on a rerun after the toolchain has already moved.

Managed dependencies are the names listed in `PinnedDeps`, or, when that list is empty, every
git `require` in the lakefile pinned to a Lean version tag. A dependency pinned to a commit hash
or a branch is a deliberate pin, so it is reported and left alone unless named in `PinnedDeps`.
Both `lakefile.toml` and `lakefile.lean` are supported; if a package has both, the `.toml` one
wins, as it does in Lake.

Every target directory is bumped to the same release; `pinned_bump` reports `updated` if any of
them changed. -/
public def runBumpPinnedTags : IO Unit := do
  let releaseKind ← GitHub.Action.Input.get ReleaseKindToFetch
  let channel ← GitHub.Action.Input.get ReleaseChannel
  let pinnedDeps ← GitHub.Action.Input.get PinnedDeps
  let dirs ← getTargetLakePackageDirectories

  if releaseKind == .nightly then
    throw <| IO.userError "bumpPinnedTags requires release_kind_to_fetch = 'tagged'"

  let candidates ← getLeanTaggedCandidates channel.stableOnly
  let some newest := candidates[0]?
    | throw <| IO.userError "No tagged Lean release found"
  let target := newest.toString
  IO.println <| log% s!"Latest {releaseKind} Lean release: {target}"
  GitHub.Action.writeGHOutput "latest_lean" target
  GitHub.Action.writeGHEnv "LATEST_LEAN" target

  let mut anyUpdated := false
  for dir in dirs do
    if dirs.size > 1 then
      IO.println <| log% s!"Updating {dir}"
    if ← bumpPackage pinnedDeps target dir then
      anyUpdated := true
  GitHub.Action.writeGHOutput "pinned_bump" (if anyUpdated then "updated" else "skipped")
