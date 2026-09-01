module

import Lean
import LeanUpdate.IO
import LeanUpdate.Input

open System

namespace LeanUpdateTest.PackageDirectoryGlob

def writeLakefile (dir : FilePath) (name : String) : IO Unit := do
  IO.FS.createDirAll dir
  IO.FS.writeFile (dir / name) ""

/-- Lay out a workspace with packages at two depths, a dependency checkout under `.lake`,
and a plain directory holding no lakefile. -/
def buildWorkspace (root : FilePath) : IO Unit := do
  let benchmarks := root / "Benchmarks"
  writeLakefile (benchmarks / "Compile") "lakefile.toml"
  writeLakefile (benchmarks / "Catalog") "lakefile.toml"
  writeLakefile (benchmarks / "Catalog" / "FixtureA") "lakefile.toml"
  writeLakefile (benchmarks / "Catalog" / "FixtureB") "lakefile.lean"
  writeLakefile (benchmarks / "Compile" / ".lake" / "packages" / "mathlib") "lakefile.lean"
  IO.FS.createDirAll (benchmarks / "NotAPackage")

def checkExpansion (expected : FilePath → Array FilePath) : IO Unit := do
  let workspace : FilePath := ⟨← IO.getEnv! "GITHUB_WORKSPACE"⟩
  let got ← getTargetLakePackageDirectories
  let want := expected workspace
  unless got.map (·.toString) == want.map (·.toString) do
    throw <| IO.userError <|
      s!"unexpected expansion\n  got:      {got.map (·.toString)}\n" ++
      s!"  expected: {want.map (·.toString)}"

/-- `/**` reaches the fixtures nested inside `Catalog`, and stops at `.lake`. -/
public def runRecursive : IO Unit :=
  checkExpansion fun workspace =>
    let benchmarks := workspace / "Benchmarks"
    #[
      benchmarks / "Catalog",
      benchmarks / "Catalog" / "FixtureA",
      benchmarks / "Catalog" / "FixtureB",
      benchmarks / "Compile"
    ]

/-- `/*` stays one level down, as it did before `/**` existed. -/
public def runShallow : IO Unit :=
  checkExpansion fun workspace =>
    let benchmarks := workspace / "Benchmarks"
    #[benchmarks / "Catalog", benchmarks / "Compile"]

/-- An excluded directory takes the packages nested inside it with it. -/
public def runExcludeSubtree : IO Unit :=
  checkExpansion fun workspace => #[workspace / "Benchmarks" / "Compile"]

/-- Excluding a nested package leaves the package containing it in the expansion. -/
public def runExcludeNested : IO Unit :=
  checkExpansion fun workspace =>
    let benchmarks := workspace / "Benchmarks"
    #[
      benchmarks / "Catalog",
      benchmarks / "Catalog" / "FixtureB",
      benchmarks / "Compile"
    ]

/-- Expand `packageDir` in a subprocess, returning its exit code and everything it printed. -/
def runInWorkspace (workspace : FilePath) (packageDir mode : String) : IO (UInt32 × String) := do
  let currentExe ← IO.appPath
  let out ← IO.Process.output {
    cmd := currentExe.toString
    args := #[mode]
    env := #[
      ("GITHUB_WORKSPACE", some workspace.toString),
      ("LAKE_PACKAGE_DIRECTORY", some packageDir)
    ]
  }
  pure (out.exitCode, out.stdout ++ out.stderr)

/-- Run an expansion that must succeed, and return what it printed. -/
def runExpectingSuccess (workspace : FilePath) (packageDir mode : String) : IO String := do
  let (exitCode, output) ← runInWorkspace workspace packageDir mode
  if exitCode != 0 then
    throw <| IO.userError s!"{mode} failed for '{packageDir}'\n{output}"
  pure output

/-- Run an expansion that must fail, and return what it reported. -/
def runExpectingFailure (workspace : FilePath) (packageDir mode : String) : IO String := do
  let (exitCode, output) ← runInWorkspace workspace packageDir mode
  if exitCode == 0 then
    throw <| IO.userError s!"{mode} should have failed for '{packageDir}'\n{output}"
  pure output

def checkContains (haystack needle description : String) : IO Unit := do
  unless haystack.contains needle do
    throw <| IO.userError s!"{description}: expected to find {needle} in\n{haystack}"

/--
A package required by path from its parent lives one level below that parent, so `/*` — which
reads only the immediate subdirectories — cannot see it. `/**` walks the tree instead, while
still pruning `.lake` so vendored dependency checkouts are never mistaken for the repository's
own packages.

A `!` entry then carves packages back out of that sweep, which is what makes a `/**` over a
benchmark tree usable when one package in it must not be updated.
-/
public def test : IO Unit := do
  IO.FS.withTempDir fun tempDir => do
    buildWorkspace tempDir
    let _ ← runExpectingSuccess tempDir "Benchmarks/**" "package-glob-recursive"
    let _ ← runExpectingSuccess tempDir "Benchmarks/*" "package-glob-shallow"
    let _ ← runExpectingSuccess tempDir "Benchmarks/** !Benchmarks/Catalog"
      "package-glob-exclude-subtree"
    let _ ← runExpectingSuccess tempDir "Benchmarks/** !Benchmarks/Catalog/FixtureA"
      "package-glob-exclude-nested"

    -- A trailing slash and a `./` prefix name the same directory as the bare path.
    let _ ← runExpectingSuccess tempDir "Benchmarks/** !./Benchmarks/Catalog/"
      "package-glob-exclude-subtree"

    -- `Benchmarks/Compil` is a string prefix of `Benchmarks/Compile` but not a directory
    -- containing it, so nothing is excluded and the mismatch is reported.
    let unmatched ← runExpectingSuccess tempDir "Benchmarks/** !Benchmarks/Compil"
      "package-glob-recursive"
    checkContains unmatched "matched none" "an exclusion matching no package directory"

    let bare ← runExpectingFailure tempDir "Benchmarks/** !" "package-glob-recursive"
    checkContains bare "names no directory" "a bare exclusion marker"

    let globbed ← runExpectingFailure tempDir "Benchmarks/** !Benchmarks/**"
      "package-glob-recursive"
    checkContains globbed "contains a glob" "a globbed exclusion"

    let emptied ← runExpectingFailure tempDir "Benchmarks/** !Benchmarks"
      "package-glob-recursive"
    checkContains emptied "No Lake package directories found" "an exclusion covering every target"

end LeanUpdateTest.PackageDirectoryGlob
