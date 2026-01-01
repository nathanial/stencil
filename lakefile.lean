import Lake
open Lake DSL

package stencil where
  version := v!"0.1.0"
  leanOptions := #[
    ⟨`autoImplicit, false⟩,
    ⟨`relaxedAutoImplicit, false⟩
  ]

require scribe from git "https://github.com/nathanial/scribe" @ "v0.0.2"
require crucible from git "https://github.com/nathanial/crucible" @ "v0.0.1"

@[default_target]
lean_lib Stencil where
  roots := #[`Stencil]

lean_lib Tests where
  roots := #[`Tests]
  globs := #[.submodules `Tests]

@[test_driver]
lean_exe stencil_tests where
  root := `Tests.Main
