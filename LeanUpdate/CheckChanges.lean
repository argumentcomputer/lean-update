module

import LeanUpdate.GitHub.Action.Env
import LeanUpdate.Input
public import LeanUpdate.IO

open IO Process System

/-- the result of update command -/
inductive UpdateResult where
  /-- no changes detected. already up to date -/
  | upToDate
  /-- changes detected -/
  | changed (newContent : String)

def UpdateResult.isChanged : UpdateResult → Bool
  | .upToDate => false
  | .changed _ => true

def UpdateResult.newContent : UpdateResult → String
  | .upToDate => ""
  | .changed content =>content

structure CheckChangesResult where
  lakeManifest : UpdateResult
  leanToolchain : UpdateResult

/-- Check if there are any diffs.

### Note
* This function ignores whitespace changes
* This function uses `git diff` to check changes.
* `path` is relative to the `cwd`
-/
def hasGitDiff (cwd : FilePath) (path : String) : IO Bool := do
  let out ← IO.Process.successOutput {
    cmd := "git"
    args := #["diff", "-w", "--", path]
    cwd := some cwd
  }
  pure !out.stdout.isEmpty

def checkFile (targetLakePackageDir : FilePath) (file : FilePath) : IO UpdateResult := do
  let hasDiff ← hasGitDiff targetLakePackageDir s!"{file}"
  if hasDiff then
    let content ← IO.FS.readFile (targetLakePackageDir / file)
    pure <| .changed content.trimAscii.copy
  else
    pure .upToDate

def checkChanges (targetLakePackageDir : FilePath) : IO CheckChangesResult := do
  let leanToolchainResult ← checkFile targetLakePackageDir "lean-toolchain"
  let lakeManifestResult ← checkFile targetLakePackageDir "lake-manifest.json"
  pure { lakeManifest := lakeManifestResult, leanToolchain := leanToolchainResult }

/-- The per-directory changed files, with names prefixed by their directory when more than one
target directory is configured. -/
def changedFilesOf (multi : Bool) (dir : FilePath) (result : CheckChangesResult) : List String :=
  let pfx := if multi then s!"{dir}/" else ""
  [("lake-manifest.json", result.lakeManifest), ("lean-toolchain", result.leanToolchain)]
    |>.filter (fun (_, res) => res.isChanged)
    |>.map (fun (name, _) => pfx ++ name)

/-- Get the list of changed files across all target Lake packages.
Changed file is `lean-toolchain` or `lake-manifest.json`. -/
public def LeanUpdate.getChangedFiles : IO (List String) := do
  let dirs ← getTargetLakePackageDirectories
  let mut changedFiles : List String := []
  for dir in dirs do
    let result ← checkChanges dir
    changedFiles := changedFiles ++ changedFilesOf (dirs.size > 1) dir result
  pure changedFiles

/-- Run the update checker command.

With multiple target directories the reported state is the aggregate: a change in any directory
counts as a change, and the new `lean-toolchain` content is taken from the first directory whose
toolchain moved (they all move to the same release). -/
public def runCheckChanges : IO Unit := do
  let updateIfModified ← GitHub.Action.Input.get UpdateIfModified
  let dirs ← getTargetLakePackageDirectories

  let mut filesChanged := false
  let mut doUpdate := false
  let mut changedFiles : List String := []
  let mut leanToolchainUpdated := false
  let mut newLeanToolchainContent := ""
  for dir in dirs do
    let result ← checkChanges dir
    let dirChanged := [result.lakeManifest, result.leanToolchain].any UpdateResult.isChanged
    filesChanged := filesChanged || dirChanged
    doUpdate := doUpdate ||
      match updateIfModified with
      | .«lean-toolchain» => result.leanToolchain.isChanged
      | .«lake-manifest.json» =>
        -- **TODO** oh no, this is a bit weird...
        -- The input is "lake-manifest.json", but we will trigger update even if only `lean-toolchain` is changed.
        -- I think this is not good and I should fix the misleading input structrue.
        dirChanged
    changedFiles := changedFiles ++ changedFilesOf (dirs.size > 1) dir result
    if result.leanToolchain.isChanged && newLeanToolchainContent.isEmpty then
      newLeanToolchainContent := result.leanToolchain.newContent
    leanToolchainUpdated := leanToolchainUpdated || result.leanToolchain.isChanged

  GitHub.Action.writeGHEnv "FILES_CHANGED" (toString filesChanged)
  GitHub.Action.writeGHEnv "CHANGED_FILES" (toString changedFiles)
  GitHub.Action.writeGHEnv "DO_UPDATE" (toString doUpdate)
  GitHub.Action.writeGHEnv "LEAN_TOOLCHAIN_UPDATED" (toString leanToolchainUpdated)

  if leanToolchainUpdated then
    IO.println s!"info: new lean-toolchain content: {newLeanToolchainContent}"
  else
    IO.println s!"info: lean-toolchain not updated, no new content"
  GitHub.Action.writeGHEnv "NEW_LEAN_TOOLCHAIN_CONTENT" newLeanToolchainContent
