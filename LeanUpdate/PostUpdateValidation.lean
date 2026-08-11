module

import Lean
import LeanUpdate.IO
import LeanUpdate.GitHub.Action.Env
public import LeanUpdate.Input

open IO Process System

/-- the result of `lake build` command -/
public abbrev BuildResult := Except String Unit

/-- the result of `lake test` command -/
public abbrev TestResult := Except String Unit

/-- the result of `lake lint` command -/
public abbrev LintResult := Except String Unit

/-- convert `BuildResult` to string -/
public def BuildResult.toString (result : BuildResult) : String :=
  match result with
  | Except.ok _ => "Build completed successfully"
  | Except.error err => err

/-- convert `TestResult` to string -/
public def TestResult.toString (result : TestResult) : String :=
  match result with
  | Except.ok _ => "All tests passed successfully"
  | Except.error err => err

/-- convert `LintResult` to string -/
public def LintResult.toString (result : LintResult) : String :=
  match result with
  | Except.ok _ => "All lint checks passed successfully"
  | Except.error err => err

/-- the result of the post update validation -/
public structure PostUpdateValidationResult where
  /-- the result of `lake build` command. -/
  buildResult : BuildResult

  /-- the result of `lake test` command.
  this is `none` if `test_driver` not registered -/
  testResult? : Option TestResult

  /-- the result of `lake lint` command.
  this is `none` if `lint_driver` not registered -/
  lintResult? : Option LintResult

/-- Run `lake build` command and get the result. -/
public def runLakeBuild (cwd : FilePath) (buildArgs : BuildArgs) : IO BuildResult := do
  let out ← IO.Process.lakeOutput cwd (args := #["build"] ++ buildArgs.val)
  if out.exitCode == 0 then
    return (Except.ok ())
  let buildOutput := out.stdout.trimAscii.copy ++ "\n" ++ out.stderr.trimAscii.copy
  pure (Except.error buildOutput)

/-- Check if `test_driver` is registered in the target package. -/
public def hasTestDriver (cwd : FilePath) : IO Bool := do
  let out ← IO.Process.lakeOutput cwd (args := #["check-test"])
  return out.exitCode == 0

/-- Run `lake test` command and get the result. -/
public def runLakeTest (cwd : FilePath) : IO TestResult := do
  let out ← IO.Process.lakeOutput cwd (args := #["test"])
  if out.exitCode == 0 then
    return (Except.ok ())
  let testOutput := out.stdout.trimAscii.copy ++ "\n" ++ out.stderr.trimAscii.copy
  pure (Except.error testOutput)

/-- Check if `lint_driver` is registered in the target package. -/
public def hasLintDriver (cwd : FilePath) : IO Bool := do
  let out ← IO.Process.lakeOutput cwd (args := #["check-lint"])
  return out.exitCode == 0

/-- Run `lake lint` command and get the result. -/
public def runLakeLint (cwd : FilePath) : IO LintResult := do
  let out ← IO.Process.lakeOutput cwd (args := #["lint"])
  if out.exitCode == 0 then
    return (Except.ok ())
  let lintOutput := out.stdout.trimAscii.copy ++ "\n" ++ out.stderr.trimAscii.copy
  pure (Except.error lintOutput)

/-- result is success -/
public def PostUpdateValidationResult.isSuccess (result : PostUpdateValidationResult) : Bool :=
  result.buildResult.isOk
    && (result.testResult?.all (·.isOk))
    && (result.lintResult?.all (·.isOk))

/-- result is failure -/
public def PostUpdateValidationResult.isFailure (result : PostUpdateValidationResult) : Bool :=
  !result.isSuccess

/-- Run `lake build`, and `lake test`/`lake lint` when drivers exist, in one directory. -/
def validatePackage (buildArgs : BuildArgs) (targetLakePackageDir : FilePath) :
    IO PostUpdateValidationResult := do
  let buildResult ← runLakeBuild targetLakePackageDir buildArgs

  let hasTestDriverResult ← hasTestDriver targetLakePackageDir
  let testResult? ←
    if hasTestDriverResult then
      IO.println <| log% "Target lake package has a test driver"
      let testResult ← runLakeTest targetLakePackageDir
      pure <| some testResult
    else
      IO.println <| log% "Target lake package does not have a test driver, skipping tests"
      pure none

  let hasLintDriverResult ← hasLintDriver targetLakePackageDir
  let lintResult? ←
    if hasLintDriverResult then
      IO.println <| log% "Target lake package has a lint driver"
      let lintResult ← runLakeLint targetLakePackageDir
      pure <| some lintResult
    else
      IO.println <| log% "Target lake package does not have a lint driver, skipping lint checks"
      pure none

  return { buildResult := buildResult, testResult? := testResult?, lintResult? := lintResult? }

/-- Validate every target Lake package. Failures are aggregated across directories, each error
prefixed with the directory it came from, so one broken package fails the whole validation. -/
public def runPostUpdateValidation : IO PostUpdateValidationResult := do
  let buildArgs ← GitHub.Action.Input.get BuildArgs
  let dirs ← getTargetLakePackageDirectories
  let mut buildErrs : Array String := #[]
  let mut testErrs : Array String := #[]
  let mut lintErrs : Array String := #[]
  let mut hasTest := false
  let mut hasLint := false
  for dir in dirs do
    if dirs.size > 1 then
      IO.println <| log% s!"Validating {dir}"
    let result ← validatePackage buildArgs dir
    if let .error e := result.buildResult then
      buildErrs := buildErrs.push s!"{dir}: {e}"
    if let some testResult := result.testResult? then
      hasTest := true
      if let .error e := testResult then
        testErrs := testErrs.push s!"{dir}: {e}"
    if let some lintResult := result.lintResult? then
      hasLint := true
      if let .error e := lintResult then
        lintErrs := lintErrs.push s!"{dir}: {e}"
  let merge (errs : Array String) : Except String Unit :=
    if errs.isEmpty then .ok () else .error (String.intercalate "\n" errs.toList)
  return {
    buildResult := merge buildErrs
    testResult? := if hasTest then some (merge testErrs) else none
    lintResult? := if hasLint then some (merge lintErrs) else none
  }
