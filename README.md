# The Pneuma Programming Language

A description of this project is coming soon ...

## version 0.0.0 -- core 1
 - module checking with expected shape DONE
 - module insertion preventation -- better module checking algorithm DONE
 - equivalence function DONE
## version 0.0.0 -- core 2
 - document pneuma.proto DONE
 - make modules recursive DONE
 - rethink the implicit naming conventions DONE
 - introduce natural numbers DONE
 - exact semantics of Ascriptions DONE?
 - need to tag implicit context DONE?
 - what happens with implicits in types DONE?
 - recheck complete structure DONE
 - testing DONE

## version 0.0.0 -- infrastructure
 - build a parser DONE
 - build a bytecode generator DONE

## version 0.0.0 -- release
 - initialize repository DONE
 - make publishing setup DONE
 - clean project -- make it more consistent
 - testing
 - make base types (Strings? or wait for 0.1.0)
 - use ZIO for speedup?
 - 'no y field' bug in implicits.pneuma (missing implicits resolution in Application and Projection check)

## version 0.1.0
 - inductive datatypes
 - make type classes possible
 - type classes for literal macros
 - mutual recursive modules DONE? i think so
 - comments syntax
 - CLI app
 - documentation

## POTENTIAL BUGS
 - shifting
 - missing tags in implicit context
 - wrong type priority in for example ascription