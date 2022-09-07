### base grammar (CoC + modules)
```haskell
t = $               -- box (type of kinds)
    *               -- kind (type of types)
    x               -- variable
    (x : t) -> t    -- function
    (x : t) => t    -- function type
    t t             -- application
    { x = t }       -- module term
    { x : t }       -- module type
    t.x             -- module selection
```

### inductives

- define data type
- reference an inductive type
- reference a constructor
- pattern matching

```haskell
t = x <t>                       -- inductive type
    data x <t> { x <t> <t> }    -- inductive definition
    t.x <t>                     -- inductive construction
    t match { p -> t }          -- inductive deconstruction
```

### implicits

- define an implicit instance
- require an implicit instance
- access an implicit instance
- when is an implicit instance inserted?
- for every base constructs, there can be an equivalent with implicits involved
- resolution strategy: if there is a type-mismatch, check if implicit insertion could happen -- try to insert implicit

__implicits for lambda calculus__ (work in progress)
```haskell
-- lambda calculus (explicit)
x               -- access name scope
x -> t          -- abstract using name scope
(x : t) => t    -- abstraction type
t t

?               -- access proof scope
(x : t)? => t   -- implicit function type

```

let x : S ?-> T = S2T ? in
    let ? : S = s0 in x

S2T ? <: S ?-> T

### inference

- inference mechanism (where does the type information come from?)

__Inference and Implicits have both the same problem: where does the information come from?__

__use SI calculus for implicits and inference! (combination of synthesis and checking)__

