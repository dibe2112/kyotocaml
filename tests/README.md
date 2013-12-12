## Testprogramms (Overview)  

This directory contains some unit tests for Kyotocaml.
Ounit is used as test framework and all its command line options can be used.

### Source files
test1.ml: Tests for functions in Module Kyotocaml.KCString
test2o.ml: Tests for functions in modules Kyotocaml.KCDb, Kyotocaml.KCDb.Ret (without text database)
test2x.ml: Tests for functions in modules Kyotocaml.KCDb, Kyotocaml.KCDb.Exc (without text database)
test3o.ml: Tests for functions in modules Kyotocaml.KCDb, Kyotocaml.KCIdx.Ret
test3x.ml: Tests for functions in modules Kyotocaml.KCDb, Kyotocaml.KCIdx.Exc 
test4o.ml: Tests for functions in modules Kyotocaml.KCDb, Kyotocaml.KCDb.Ret (Text database only)
test4x.ml: Tests for functions in modules Kyotocaml.KCDb, Kyotocaml.KCDb.Exc (Text database only)
test5.ml: Tests for bulk operations in module Kyotocaml.KCDb and its submodules

### Executables
The tests are created automatically when project *Kyotocaml* is created with command *make all*.
For each .ml-file one byte code (suffix: *.byte*) and one native code (suffix: *.nat*) executable is created.
