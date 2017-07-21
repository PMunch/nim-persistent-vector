# Persistent Vector
This is an implementation of Clojures persistent vectors in Nim. The default branching factor is 32 as in Clojure, but can be changed by using the boot switch `-d:persvectorbits=n` where n is the power of two to use as the branching factor (and thus defaults to 5 for 32-way branching).

For more information see the vector.nim file and it's doc-strings.
