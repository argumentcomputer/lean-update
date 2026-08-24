module

import Test.LakeToolchainResolution
import Test.PackageDirectoryGlob
import Test.PinnedTagFallback
import Test.UpdateDependenciesEnv

/-- Run the test suite and dispatch subprocess invocations used by individual tests.

The self-contained tests run first, so that a test needing an Elan install or a network fetch
cannot mask them by failing on a machine that lacks one. -/
public def main (args : List String) : IO Unit := do
  match args with
  | ["inner"] => LeanUpdateTest.UpdateDependenciesEnv.runInner
  | ["update"] => LeanUpdateTest.UpdateDependenciesEnv.runAsFakeLake args
  | ["toolchain-resolution-inner"] => LeanUpdateTest.LakeToolchainResolution.testInner
  | ["package-glob-recursive"] => LeanUpdateTest.PackageDirectoryGlob.runRecursive
  | ["package-glob-shallow"] => LeanUpdateTest.PackageDirectoryGlob.runShallow
  | _ => do
    LeanUpdateTest.PinnedTagFallback.test
    LeanUpdateTest.PackageDirectoryGlob.test
    LeanUpdateTest.UpdateDependenciesEnv.test
    LeanUpdateTest.LakeToolchainResolution.test
