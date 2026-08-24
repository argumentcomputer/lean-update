module

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

def runInWorkspace (workspace : FilePath) (packageDir : String) (mode : String) : IO Unit := do
  let currentExe ← IO.appPath
  let out ← IO.Process.output {
    cmd := currentExe.toString
    args := #[mode]
    env := #[
      ("GITHUB_WORKSPACE", some workspace.toString),
      ("LAKE_PACKAGE_DIRECTORY", some packageDir)
    ]
  }
  if out.exitCode != 0 then
    throw <| IO.userError s!"{mode} failed\nstdout:\n{out.stdout}\nstderr:\n{out.stderr}"

/--
A package required by path from its parent lives one level below that parent, so `/*` — which
reads only the immediate subdirectories — cannot see it. `/**` walks the tree instead, while
still pruning `.lake` so vendored dependency checkouts are never mistaken for the repository's
own packages.
-/
public def test : IO Unit := do
  IO.FS.withTempDir fun tempDir => do
    buildWorkspace tempDir
    runInWorkspace tempDir "Benchmarks/**" "package-glob-recursive"
    runInWorkspace tempDir "Benchmarks/*" "package-glob-shallow"

end LeanUpdateTest.PackageDirectoryGlob
