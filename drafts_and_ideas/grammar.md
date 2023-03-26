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

- type checking against holed constraint
- restricted types to force inference and resolution in certain positions

__use SI calculus for implicits and inference! (combination of synthesis and checking)__

resolution happens during synthesis, construction during checking

# formal definition

| name          | production                                                                      |
| ------------- | ------------------------------------------------------------------------------- |
| variable      | \(  \text{x}  \)                                                                |
| application   | \(  \text{t}\ \text{t}  \)                                                      |
| kind          | \(  *  \)                                                                       |
| box           | \(  \Box  \)                                                                    |
| query         | \(  \phi  \)                                                                    |
| function      | \(  \lambda \text{x} \ .\ \text{t}  \)                                          |
| implicit      | \(  \phi \text{x} \ .\ \text{t}  \)                                             |
| inferred      | \(  \sigma \text{x} \ .\ \text{t}  \)                                           |
| function type | \(  \Lambda \text{x} : \text{t} \ .\ \text{t}  \)                               |
| implicit type | \(  \Phi \text{x} : \text{t} \ .\ \text{t}  \)                                  |
| inferred type | \(  \Sigma \text{x} : \text{t} \ .\ \text{t}  \)                                |
| module        | \(  \{ \text{l}_i = \text{t}_i \mid \#\text{t}_i = \text{t}_i \}^{i\in1..n}  \) |
| interface     | \(  \{ \text{l}_i : \text{t}_i \mid \#\text{t}_i \}^{i\in1..n}  \)              |
| selection     | \(  \text{t}.\text{l}  \)                                                       |

$$t \rightsquigarrow e$$

$$\Gamma,\Pi,\Mu, \Theta \vdash t : \Tau $$


<!-- $$\frac{\text{x} : \text{t} \in \Gamma}{\Gamma \mid \text{x} : \text{t} \vdash \text{x} : \text{t}} $$

$$ \Gamma \vdash * : \Box $$ -->

<!-- $$\frac{\text{x} : \text{t} \in \Gamma}{\Gamma \langle \text{x} : \text{t} \rangle \vdash \text{x} : \text{t}} $$

$$
\frac
    {\text{x} : \text{t} \in \Gamma}
    {\Gamma
        \langle (\lambda \text{x} \ .\ \text{t}) : (\pi \text{x} : \text{t} \ .\ \text{t}) \rangle
     \vdash
        (\lambda \text{x} \ .\ \text{t}) : (\pi \text{x} : \text{t} \ .\ \text{t})
    }
$$ -->

Text --Parser--> PneumaAst --TypeChecker--> TargetAst --ASM--> ByteCode

_Terms_
- \(  \text{x}  \)
- \(  \text{t}\ \text{t}  \)
- \(  *  \)
- \(  \phi  \)
- \(  \lambda \text{x} \ .\ \text{t}  \)
- \(  \phi \text{x} \ .\ \text{t}  \)
- \(  \sigma \text{x} \ .\ \text{t}  \)
- \(  \Lambda \text{x} : \text{t} \ .\ \text{t}  \)
- \(  \Phi \text{x} : \text{t} \ .\ \text{t}  \)
- \(  \Sigma \text{x} : \text{t} \ .\ \text{t}  \)
- \(  \{ \text{l}_i = \text{t}_i \mid \#\text{t}_i = \text{t}_i \}^{i\in1..n}  \)
- \(  \{ \text{l}_i : \text{t}_i \mid \#\text{t}_i \}^{i\in1..n}  \)
- \(  \text{t}.\text{l}  \)

---

_Contexts_
- $\Gamma$ – variables
- $\Pi$ - implicits
- $\Mu$ - modules (coinduction hypothesis)
- $\Theta$ - type shape
- $\Tau$ - synthesized type

---

$$\Gamma,\Pi,\Mu, \Theta \vdash t : \Tau $$

---

$$\Gamma,\Pi,\Mu, \Theta \vdash * : * $$

$$\frac{x:t \in \Gamma\quad \Theta \equiv t}{\Gamma,\Pi,\Mu, \Theta \vdash x : t}$$

$$\frac{x:t \in \Gamma}{\Gamma,\Pi,\Mu, \Theta \vdash \phi : t}$$
