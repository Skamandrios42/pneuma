# Definition of Pneuma

## Roadmap

- pneuma.proto — COC + modules and implicits
    - ordered modules ($\rightarrow$ clear type-checking and evaluation order)
    - unordered modules ($\rightarrow$ order must be inferred)
- pneuma.core — proto.pneuma + type-inference + IDTs ((==)-policy)
- pneuma.basic — pneuma.core + standard-types + extensibilty

---

## pneuma.proto

### grammar
| name  | production                                 |
| ----- | ------------------------------------------ |
| `var` | \(  x  \)                                  |
| `abs` | \(  \lambda x.t  \)                        |
| `app` | \(  t\ t \)                                |
| `typ` | \(  *  \)                                  |
| `phi` | \(  \phi  \)                               |
| `pro` | \(  \pi x:t.t  \)                          |
| `imp` | \(  \phi x:t.t  \)                         |
| `mod` | \(  \{l_i=t_i\mid\phi = t_i\}^{i\in1..n}\) |
| `int` | \(  \{l_i:t_i\mid\phi : t_i\}^{i\in1..n}\) |
| `get` | \(  t.l  \)                                |

### type-checking rules
---
$$
\frac
{
    x:t \in \Gamma\quad t\subset\Theta
}
{
    \Gamma\;\Pi\;\Mu\;\Theta \vdash x : t
}
\qquad\text{(type::var)}
$$

$$
\frac
{
    (\pi x:t_2.t_3)\subset\Theta \quad
    \Gamma,x:t_2\;\Pi\;\Mu\;t_3 \vdash t_1 : t_3
}
{
    \Gamma\;\Pi\;\Mu\;\Theta \vdash \lambda x.t_1 : (\pi x:t_2.t_3)
}
\qquad\text{(type::abs)}
$$

$$
\frac
{
    \Gamma\;\Pi\;\Mu\;\empty \vdash t_1 : (\pi x:t_4.t_3) \quad
    \Gamma\;\Pi\;\Mu\;t_4 \vdash t_2 : t_4 \quad
    t_3\subset\Theta
}
{
    \Gamma\;\Pi\;\Mu\;\Theta \vdash t_1\ t_2 : t_3
}
\qquad\text{(type::app)}
$$

$$
\frac
{*\subset\Theta}
{
    \Gamma\;\Pi\;\Mu\;\Theta \vdash * : *
}
\qquad\text{(type::typ)}
$$

---

- könnte (type::app) zuerst $t_2$ checken und danach $t_1$?

### type checking example

```haskell
:term {
    x = 4
    f = y -> 0.x + y
    g = ? (? (0.f 0.x))
    ? = x -> x + 1
    r = 0.g
}
:type {
    x : Int
    f : Int => Int
    g : (Int => Int)? => Int
    ? : Int => Int
    r : Int
}
```

- __[check x]__ `x : Int`, `4 :? Int` — `Int == Int`
- __[check f]__ `f : Int => Int`, `y -> 0.x + y :? Int => Int` — `y : Int` in `0.x + y :? Int`
- __[check g]__
  - `? (? (0.f 0.x)) :: (Int => Int)? => Int`
  - `? : (Int => Int)`, `? (? (0.f 0.x)) :: Int`

### notes

type mismatch (A? => B) found, but expected B -> search(A, G)

search(A, G, a : A)  --> a
search(A, G, b : C? => A)  --> b search(C, G)


### how to typecheck modules

```haskell
:term {
    X = A -> A,
    y = 42,
    z = y : this.X Int
}

:type {
    X : Type => Type,
    Y : this.X Int,
    Z : Int
}

{
    X = A -> A          :: Type => Type
    y = 42              :: this.X Int,
    z = y : this.X Int  :: Int
}

{
    X = A :: Type -> A :: Type
    y = 42 :: Int <=> (A -> A) Int,
    z = y : this.X Int  :: Int
}

```

### modules with implicits

```haskell
:term {
    X = ?,          ~> A -> A :: (#Type => Type)
    y = 42,         ~> 42 :: Int === (A -> A) Int
    z = y : this.X  ~> 42 :: Int === (A -> A) Int
    # Type = Int    ~> Int :: Type
}

:type {
    X : #(Type) => Type,  ~> #(Type) => Type :: Type
    Y : this.X,           ~> this.X #Type  :: Type
    Z : Int,              ~> Int :: Type
    # Type                ~> Type :: Type
}
```

#### basic case

```python
{ ?T = t0,
  y = ?,
  z = y }

{ ?T,
  y : ?T => T
  z : T }

{ x = t0,
  y = a -> a,
  z = y x }
```

#### type level I

```python
{ ?* = T, y = t0 }

{ ?*, y : ?}
```