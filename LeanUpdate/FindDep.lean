module

import LeanUpdate.GitHub.Action.Env
public import Lake.Load.Manifest
import LeanUpdate.Input

open Lean Lake Std System

/-- get the dependencies of the Lake package in the given directory

A package that has never run `lake update` has no manifest yet; treat it as having no
dependencies rather than failing, since a `/*` directory expansion only requires a lakefile. -/
public def LeanUpdate.getDependenciesIn (dir : System.FilePath) : IO (List String) := do
  let manifestFilePath := dir / "lake-manifest.json"
  unless (← manifestFilePath.pathExists) do
    return []
  let manifest ← Lake.Manifest.load manifestFilePath
  let packages := manifest.packages
    |> Array.toList
    |>.map fun package => package.name.toString (escape := false)
  return packages

/-- get the dependencies across all target Lake packages -/
public def LeanUpdate.getDependencies : IO (List String) := do
  let dirs ← getTargetLakePackageDirectories
  let mut all : List String := []
  for dir in dirs do
    all := all ++ (← getDependenciesIn dir)
  return all.eraseDups

/-- Check if the Lake package has any dependencies -/
public def LeanUpdate.hasDependency : IO Bool := do
  let deps ← getDependencies
  return !deps.isEmpty

/-- Run the dependency checker command. -/
public def runFindDependencies : IO Unit := do
  let packageNames ← LeanUpdate.getDependencies
  let hasDep ← LeanUpdate.hasDependency
  if hasDep then
    IO.println s!"The repository has some dependencies: {packageNames}"
    GitHub.Action.writeGHOutput "has_dependency" "true"
    GitHub.Action.writeGHEnv "HAS_DEPENDENCY" "true"
  else
    IO.println "The repository has no dependencies."
    GitHub.Action.writeGHOutput "has_dependency" "false"
    GitHub.Action.writeGHEnv "HAS_DEPENDENCY" "false"
