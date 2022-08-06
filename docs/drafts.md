# Pneuma: Drafts and Ideas
--------------------------

## Grammar
```haskell
t = x               -- variable
    x -> t          -- function
    [x] -> t        -- inferred function
    t => t          -- function type
    (x : t) => t    -- universal function type
    [x] => t        -- inferred function type
    t t             -- application
    { x = t }       -- module term
    { x : t }       -- module type
    { #t = t }      -- implicit definition
    #t -> t         -- implicit function
    #t => t         -- implicit function type
    t #t            -- implicit application
```

## Modified Grammar

```haskell
x = <identifier>
p = x { p }

t = x                           -- variable

    "{" { p "->" t } "}"        -- function
    "(" x ":" t ")" "=>" t      -- function type
    t t                         -- application

    "#" "{" { p "->" t } "}"    -- implicit function
    "#" "(" x ":" t ")" "=>" t  -- implicit function type
    t "#" t                     -- implicit application

    "{" { "[" p "]" "->" t  } "}" -- inferred function
    "[" x ":" t "]" "=>" t        -- inferred function type

    -- TODO must be refined
    "data" x ":" t "{" { x ":" t } "}" -- inductive type

    "{" { x "=" t | "#" t "=" t } "}"  -- module term with implicits
    "{" { x ":" t | "#" t } "}"        -- module type with implicits

```


## Problems

- [ ] inductive types and pattern matching in modules
- [x] existential types in modules
- [x] problem with term level (mis)use

## Playground

```
hidden = {
    opaque n : Nat
    n = 42
}

f : Nat => Nat
f n = n + 1

f hidden.n # f cannot be applied to unknown n
```

```
package : [Y : Type] => ((C : Type) => { put : Nat => C, get : C => Nat } => Y) => Y
package f = f Nat { new = 0, inc n = n + 1, get c = c }
usage : Nat
usage = package (C -> mod -> mod.get (mod.put 42))
```

```
package : [Y : Type] => ((C : Type) => { new: C, inc: C => C, get: C => Nat } => Y) => Y
package f = f Nat { put c = c, get c = c }
usage : Nat
usage = package (C -> mod -> mod.get (mod.inc (mod.inc mod.new)))
```

```
package : [Y : Type] => ((n : Nat) => Y) => Y
package f = f 42
usage : Nat
usage = package (n -> mod.n + 2 )
```

### existentials via modules

```
package : [Y] => ({ C : Type, put : Nat => C, get : C => Nat } => Y) => Y
package f = f { C = Nat, new = 0, inc n = n + 1, get c = c }
usage : Nat
usage = package (C -> mod -> mod.get (mod.put 42))
```

```

Counters = { C : Type, new : C, inc : C => C, get : C => Nat }

use0 : Counters => Nat
use0 c = c.get (c.inc (c.inc c.new))

use1 : (c : Counters) => c.C
use1 c = c.inc (c.inc c.new) 

impl : Counters
impl = { C = Nat, new = 0, inc n = n + 1, get n = n }

main : IO Unit
main =
    _ <- print (use0 impl) # 2
    _ <- print (impl.get (use1 impl)) # 2

```

## inductive data types

```
data Nat : Type {
    Z : Nat
    S : Nat => Nat
}

data List : Type => Type {
    Nil : [A] => List A
    Cons : [A] => A => List A => List A
}

data Vector : Type => Nat => Type {
    Nil : [A] => Vector A 0
    Cons : [A, n] => A => Vector A n => Vector A (S n)
}
```

```
data <name> : <type> {
    <name> : <type>
    ...
}
```

```
f : Nat => Nat
f = {
    Z   -> "Z"
    S n -> "S" ++ f n
}
```

```
# final syntax
infix + : Nat => Nat => Nat
n + Z     = n
n + (S m) = S (n + m)
```


```
data Nat : Type {
    Z : Nat
    S : Nat => Nat
}

Nat = ind_type[] {
    Z = ind_term["Z"]
    S n = ind_term["S", n]
    @pattern ind_term["Z"] = ()
    @pattern ind_term["S", n] = (n)
}
```
