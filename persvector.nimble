# Package

version       = "1.0.0"
author        = "Peter Munch-Ellingsen"
description   = "Implementation of Clojures persistent vector in Nim for easy immutable lists."
license       = "MIT"

# Dependencies

requires      "nim >= 0.17.1"

# Skip examples from nimble installation

skipFiles     = @["example.nim"]

