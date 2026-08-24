module

import Test.LakeToolchainResolution
import Test.PackageDirectoryGlob
import Test.UpdateDependenciesEnv

/-- Run the test suite and dispatch subprocess invocations used by individual tests. -/
public def main (args : List String) : IO Unit := do
  match args with
  | ["inner"] => LeanUpdateTest.UpdateDependenciesEnv.runInner
  | ["update"] => LeanUpdateTest.UpdateDependenciesEnv.runAsFakeLake args
  | ["toolchain-resolution-inner"] => LeanUpdateTest.LakeToolchainResolution.testInner
  | ["package-glob-recursive"] => LeanUpdateTest.PackageDirectoryGlob.runRecursive
  | ["package-glob-shallow"] => LeanUpdateTest.PackageDirectoryGlob.runShallow
  | _ => do
    LeanUpdateTest.UpdateDependenciesEnv.test
    LeanUpdateTest.LakeToolchainResolution.test
    LeanUpdateTest.PackageDirectoryGlob.test
