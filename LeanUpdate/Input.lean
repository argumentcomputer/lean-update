module

public meta import LeanUpdate.Deriving.ToString
public meta import LeanUpdate.Deriving.HasParser
public meta import LeanUpdate.Deriving.Wrapper
public import LeanUpdate.HasParser
public import LeanUpdate.Wrapper
public import LeanUpdate.GitHub.Action.Input
import LeanUpdate.IO

open System GitHub Action

/-- The kind of Lean release -/
public inductive ReleaseKindToFetch where
  /-- tagged release, such as `v4.30.0` or `v4.31.0-rc2` -/
  | tagged
  /-- nightly release -/
  | nightly
deriving Repr, BEq, ToString, HasParser

public instance : Input ReleaseKindToFetch where
  envName := "RELEASE_KIND_TO_FETCH"
  parse := parseAs ReleaseKindToFetch
  localValue? := some .tagged

/-- Which tagged release channel to track. Only meaningful when `ReleaseKindToFetch` is `tagged`.

Defaults to `stable`: an update lands as a pull request against someone's repository, so pulling
it onto a release candidate should be a deliberate choice. -/
public inductive ReleaseChannel where
  /-- track the latest tagged release, including release candidates -/
  | rc
  /-- track only the latest stable (non pre-release) tagged release -/
  | stable
deriving Repr, BEq, ToString, HasParser

public instance : Input ReleaseChannel where
  envName := "RELEASE_CHANNEL"
  parse := parseAs ReleaseChannel
  localValue? := some .stable

/-- Whether this channel restricts the toolchain search to stable releases only. -/
public def ReleaseChannel.stableOnly : ReleaseChannel → Bool
  | .rc => false
  | .stable => true

/-- The directory of the target Lake package. This is a wrapper around `FilePath`. -/
public structure LakePackageDirectory where
  /-- the raw path supplied by the action input -/
  val : FilePath
deriving Wrapper

public instance : Input LakePackageDirectory where
  envName := "LAKE_PACKAGE_DIRECTORY"
  parse := parseAs LakePackageDirectory
  localValue? := some ⟨FilePath.mk "."⟩

/-- resolve a Lake package directory relative to the GitHub workspace when available -/
public def resolveLakePackageDir (workspace? : Option FilePath) (packageDir : FilePath) : FilePath :=
  if packageDir.isRelative then
    match workspace? with
    | .some workspace => workspace / packageDir
    | .none => packageDir
  else
    packageDir

-- test for resolving a relative input path from the GitHub workspace
#guard
  let workspace : FilePath := "/tmp/workspace"
  let packageDir : FilePath := "."
  let resolved := resolveLakePackageDir (.some workspace) packageDir
  resolved == workspace / packageDir

-- test for preserving an absolute input path
#guard
  let workspace : FilePath := "/tmp/workspace"
  let packageDir : FilePath := "/tmp/other"
  let resolved := resolveLakePackageDir (.some workspace) packageDir
  resolved == packageDir

/-- resolve the target Lake package directory supplied by the action input. -/
public def getTargetLakePackageDirectory : IO FilePath := do
  let packageDir ← GitHub.Action.Input.get LakePackageDirectory
  let workspace? := (← IO.getEnv "GITHUB_WORKSPACE").map FilePath.mk
  pure <| resolveLakePackageDir workspace? packageDir

/-- whether `dir` is itself a Lake package root -/
def hasLakefile (dir : FilePath) : IO Bool := do
  if (← (dir / "lakefile.toml").pathExists) then return true
  (dir / "lakefile.lean").pathExists

/-- Every descendant of `root` that is a Lake package root, in directory order.

Dotted directories are pruned. `.lake` holds the dependency checkouts of an already-configured
package, each a package root in its own right, so descending into one would update vendored
copies of other people's packages instead of the repository's own. -/
partial def lakePackagesUnder (root : FilePath) : IO (Array FilePath) := do
  let mut found : Array FilePath := #[]
  for child in (← root.readDir) do
    if !(← child.path.isDir) then continue
    if child.fileName.startsWith "." then continue
    if (← hasLakefile child.path) then
      found := found.push child.path
    found := found ++ (← lakePackagesUnder child.path)
  return found

/-- Split a directory-list action input into its entries.

Separators are commas and ASCII whitespace, so `a, b`, `a b`, and a YAML block scalar holding one
path per line all parse alike. -/
def splitPackageDirEntries (raw : String) : List String :=
  raw.split (fun c => c == ',' || c.isWhitespace)
    |>.map (fun s => s.trimAscii.copy)
    |>.filter (fun s => !s.isEmpty)
    |>.toList

#guard
  splitPackageDirEntries " Benchmarks/**,\n  !Fixtures/Slow " == ["Benchmarks/**", "!Fixtures/Slow"]

/-- The significant components of `path`, dropping empty and `.` segments. -/
def pathComponents (path : FilePath) : List String :=
  path.components.filter (fun s => !s.isEmpty && s != ".")

/-- whether `dir` is `parent` itself or lies somewhere beneath it

Comparing whole components rather than string prefixes keeps `Benchmarks/Slow`, `Benchmarks/Slow/`
and `./Benchmarks/Slow` the same directory, while refusing to read `Benchmarks/SlowFixture` as
living under `Benchmarks/Slow`. -/
def isAtOrUnder (parent dir : FilePath) : Bool :=
  (pathComponents parent).isPrefixOf (pathComponents dir)

#guard isAtOrUnder "/w/Benchmarks/Slow" "/w/Benchmarks/Slow"
#guard isAtOrUnder "/w/Benchmarks/Slow/" "/w/Benchmarks/Slow/Nested"
#guard isAtOrUnder "./Benchmarks/Slow" "Benchmarks/Slow"
#guard !isAtOrUnder "/w/Benchmarks/Slow" "/w/Benchmarks/SlowFixture"
#guard !isAtOrUnder "/w/Benchmarks/Slow" "/w/Benchmarks"

/-- Resolve the target Lake package directories supplied by the action input.

The input is a comma- or whitespace-separated list of paths, each resolved relative to the
GitHub workspace. An entry ending in `/*` expands to the immediate subdirectories of its parent
that contain a lakefile, so a repository of sibling packages can be updated in one invocation
(e.g. `templates/*`). An entry ending in `/**` expands the same way but walks the whole tree, so
it also reaches a package nested inside another package (e.g. a fixture workspace required by
path from its parent). Both forms sort by path and skip dotted directories such as `.lake`.

An entry prefixed with `!` subtracts instead of adding: it names a directory and drops that
directory together with everything beneath it, which is what lets a broad `/**` cover a tree that
holds a package the update must leave alone. An exclusion carries no glob of its own, since it
already reaches the whole subtree. -/
public def getTargetLakePackageDirectories : IO (Array FilePath) := do
  let packageDir ← GitHub.Action.Input.get LakePackageDirectory
  let workspace? := (← IO.getEnv "GITHUB_WORKSPACE").map FilePath.mk
  let raw := packageDir.val.toString
  let (exclusions, entries) := (splitPackageDirEntries raw).partition (·.startsWith "!")
  let exclusions := exclusions.map (fun entry => (entry.drop 1).copy)
  for entry in exclusions do
    if entry.any (· == '*') then
      throw <| IO.userError <|
        s!"Exclusion '!{entry}' contains a glob. An exclusion names a directory and already " ++
        "covers everything beneath it."
  let mut dirs : Array FilePath := #[]
  for entry in entries do
    if entry.endsWith "/**" then
      let parent := resolveLakePackageDir workspace? (FilePath.mk (entry.dropEnd 3).copy)
      let found ← lakePackagesUnder parent
      dirs := dirs ++ found.qsort (fun a b => a.toString < b.toString)
    else if entry.endsWith "/*" then
      let parent := resolveLakePackageDir workspace? (FilePath.mk (entry.dropEnd 2).copy)
      let mut found : Array FilePath := #[]
      for child in (← parent.readDir) do
        if !(← child.path.isDir) then continue
        if child.fileName.startsWith "." then continue
        if (← hasLakefile child.path) then
          found := found.push child.path
      dirs := dirs ++ found.qsort (fun a b => a.toString < b.toString)
    else
      dirs := dirs.push (resolveLakePackageDir workspace? (FilePath.mk entry))
  let excludedDirs := exclusions.map (fun entry =>
    resolveLakePackageDir workspace? (FilePath.mk entry))
  -- An exclusion matching nothing is far more likely a typo than a deliberate no-op, and the
  -- cost of the typo is that a package meant to be protected is updated instead.
  for (entry, excludedDir) in exclusions.zip excludedDirs do
    unless dirs.any (isAtOrUnder excludedDir ·) do
      IO.println <| log%
        s!"warning: exclusion '!{entry}' matched none of the target Lake package directories"
  let kept := dirs.filter (fun dir => !excludedDirs.any (isAtOrUnder · dir))
  if kept.isEmpty then
    throw <| IO.userError s!"No Lake package directories found for input '{raw}'"
  return kept

/-- The input whether to update the `lean-toolchain` file. -/
public inductive UpdateLeanToolchain where
  | auto
  | never
deriving ToString, HasParser

public instance : Input UpdateLeanToolchain where
  envName := "UPDATE_LEAN_TOOLCHAIN"
  parse := parseAs UpdateLeanToolchain
  localValue? := some .auto

/-- The input whether to perform a legacy update. This is a wrapper around `Bool`. -/
public structure LegacyUpdate where
  val : Bool
deriving Wrapper

public instance : Input LegacyUpdate where
  envName := "LEGACY_UPDATE"
  parse := parseAs LegacyUpdate
  localValue? := some ⟨false⟩

/-- Split the `build_args` action input into arguments for `lake build`. -/
public def splitBuildArgs (buildArgs : String) : Array String :=
  buildArgs
    |>.replace "\n" " "
    |>.replace "\t" " "
    |>.splitOn " "
    |>.filter (fun arg => !arg.isEmpty)
    |> List.toArray

#guard
  let actual := splitBuildArgs "  --log-level=warning  \nFoo\tBar  "
  let expected := #["--log-level=warning", "Foo", "Bar"]
  actual == expected

/-- The arguments passed to `lake build` during post-update validation. -/
public structure BuildArgs where
  /-- the raw arguments after splitting on ASCII whitespace -/
  val : Array String

public instance : Input BuildArgs where
  envName := "BUILD_ARGS"
  parse s := .ok ⟨splitBuildArgs s⟩
  localValue? := some ⟨#["--log-level=warning"]⟩

/-- The input that controls when to trigger updates based on modified files. -/
public inductive UpdateIfModified where
  /-- watch `lean-toolchain` file -/
  | «lean-toolchain»
  /-- watch `lake-manifest.json` file -/
  | «lake-manifest.json»
deriving Repr, BEq, ToString, HasParser

#guard
  let lst : List UpdateIfModified := [.«lean-toolchain», .«lake-manifest.json»]
  lst.map toString == ["lean-toolchain", "lake-manifest.json"]

#guard
  let lst : List String := ["lean-toolchain", "lake-manifest.json"]
  let result := lst
    |>.map (parseAs UpdateIfModified ·)
    |>.map Except.isOk
    |>.all id
  result

public instance : Input UpdateIfModified where
  envName := "UPDATE_IF_MODIFIED"
  parse := parseAs UpdateIfModified
  localValue? := some .«lake-manifest.json»

/-- Names of git dependencies whose pinned `rev` should be bumped to the target Lean version
tag. An empty list means "every git `require` in the lakefile that pins a `rev`". -/
public structure PinnedDeps where
  /-- the dependency names to manage; empty means all git requires with a `rev` -/
  val : List String

public instance : Input PinnedDeps where
  envName := "PINNED_DEPS"
  parse s := (parseList s).map PinnedDeps.mk
  localValue? := some ⟨[]⟩
