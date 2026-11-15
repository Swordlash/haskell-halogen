title: Ask the compiler
args: -fdefer-typed-holes

GHC can help you discover the types your code needs. After you stop typing, the page typechecks the module, marks diagnostics and reports the check duration. Checking does not run the program; Compile does both.

## The types of your bindings

Hover over `render`. GHC infers `render :: State -> HH.HTML w Action` even without an explicit signature. The variable `w` means the function works for any such widget type; `H.component` chooses the one it needs.

GHC also infers `initialState :: Applicative f => p -> f State`. It ignores an input of any type `p` and returns state with `pure`, so any `Applicative f` works.

The "Top-level types" list below the editor shows bindings and links to their source lines. This lesson defines its helpers at the top level so they appear there. Local `where` and `let` bindings, imported names such as `H.component`, and instance methods are not listed.

## Holes

Write `_` in place of an expression to ask GHC what type belongs there. For `Increment -> modify _`, it reports:

```
Found hole: _ :: State -> State
  In the first argument of `modify', namely `_'
  Relevant bindings include
    action :: Action
  Valid hole fits include
    bump :: State -> State
    id :: forall a. a -> a
```

The hole needs `State -> State`, because `modify` takes a state-update function. Relevant bindings show useful names in scope. Valid hole fits have compatible types: both `bump` and `id` fit, but only `bump` increments the count. Types narrow the choices; you still choose the intended behaviour.

Named holes start with an underscore. In `Rename new -> modify (_rename new)`, GHC reports `_rename :: Text -> State -> State` and may suggest a misspelling. Some existing functions fit by type alone without doing what you want. Write a function that actually updates the name.

Types can contain holes too. With `PartialTypeSignatures`, `label :: State -> _` lets GHC infer the result and report that the wildcard stands for `Text`.

## Why it runs

Expression holes normally prevent compilation. This lesson passes `-fdefer-typed-holes`, turning them into warnings so the rest of the component can run while you work.

Evaluating an unfinished hole still fails at runtime. Pressing `+1` before filling its hole fails that action with the hole diagnostic.

## Exercise

Replace `modify _` with `modify bump`. Define `rename :: Text -> State -> State` at the top level, with `rename new st = st {name = new}`, and use it instead of `_rename`. Replace the wildcard in `label`'s type with `Text`.

Warnings disappear after the next live check, and `rename` appears in the type list. Compile: the button should increment the count, and the field should update the name.
