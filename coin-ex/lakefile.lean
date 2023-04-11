import Lake
open Lake DSL

package «coin-ex» {
  -- add package configuration options here
}

@[default_target]
lean_exe «coin-ex» {
  root := `Main
}
