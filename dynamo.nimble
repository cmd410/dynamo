# Package

version       = "0.1.5"
author        = "Crystal Melting Dot"
description   = "A dynamic typing library for Nim"
license       = "LGPL-3.0-or-later"
srcDir        = "src"

# Dependencies

requires "nim >= 1.6.12"

task docs, "Generate documentation":
  exec "nimble test"
  exec "nim doc --path:src --project --index:on --outdir:docs --git.url:https://github.com/cmd410/dynamo --git.commit:v" & version & " src/dynamo.nim"
