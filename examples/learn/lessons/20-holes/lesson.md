title: Ask the compiler
args: -fdefer-typed-holes

GHC knows the type of everything in your module, and you can ask it. The page asks all the time: a moment after you stop typing, it has GHC typecheck your code without running it, marks what GHC says in the editor, lists it under the editor, and says how long that took ("Checked in 0.4 s"). Compile still compiles and runs; the check only reads.

## The types of your bindings

Hover over `render` in the editor: a tooltip says `render :: State -> HH.HTML w Action`. There is no type signature for `render` in the code: that is the type GHC inferred. The `w` in it is a type variable, which says that `render` works whatever `w` is, and `H.component` picks the one it needs. `initialState` has no signature either, and GHC gives it the most general type its code allows, `Applicative f => p -> f State`: it ignores its argument, of any type `p`, and returns a `State` with `pure`, in any `Applicative` at all.

Under the editor, "Top-level types" lists every binding of the module with its type, and a click on one takes you to its line. Only the module's top-level bindings are there, which is why this lesson's code has `render`, `handleAction` and the rest at the top level rather than under a `where` as the earlier lessons did: a binding under a `where` or a `let`, a name a module imports, such as `H.component`, and an instance's methods show no type here.

## Holes

A hole is a question to the compiler: an underscore, `_`, where an expression goes. In `handleAction`, `Increment -> modify _` asks what `modify` needs, and the warning at that line answers:

```
Found hole: _ :: State -> State
  In the first argument of `modify', namely `_'
  Relevant bindings include
    action :: Action
  Valid hole fits include
    bump :: State -> State
    id :: forall a. a -> a
```

Read it from the top. The first line is the hole's type: `modify` changes the state with a function, so the hole is a function from a `State` to a `State`. "Relevant bindings" are what is in scope there and might help, with their types. "Valid hole fits" are the names in scope whose type fits the hole: `bump`, which adds one to the count, and `id`, which changes nothing. A fit has the right type; whether it does the right thing is for you to say, and here it is `bump`.

A hole can have a name, an underscore and then a word: `_rename` in `Rename new -> modify (_rename new)` is a hole too (GHC adds that it may be a misspelled name), and its message says `_rename :: Text -> State -> State`. Nothing in scope does that yet: `seq` fits, and so does `initialState`, by its type alone, which is exactly why a type cannot tell you what you meant. That type is what a function giving the state a new name has to be, so write one.

A type can have a hole as well. `label :: State -> _` leaves the result for GHC to fill, which it does with the `PartialTypeSignatures` extension the module turns on at its top, and says so: "Found type wildcard `_` standing for `Text`".

## Why it runs

A hole is normally an error: the program does not compile. This lesson's header passes GHC `-fdefer-typed-holes`, which makes each one a warning and compiles the rest, so that the component runs and the types of everything else are listed while you work on the holes. A hole the program reaches while it runs is then an error at that moment: press `+1` before you fill the first, and that action fails with the hole's message instead of counting.

## Exercise

Fill the holes, with the types GHC gave you. Replace `modify _` with `modify bump`. Write `rename :: Text -> State -> State` at the top level, so that `rename new st` is `st` with `new` as its name (`st {name = new}`), and call it in place of `_rename`. Write `Text` for the `_` in `label`'s type. Each warning goes as you fix its hole, a moment after you stop typing, and `rename` arrives in the list of types. Then Compile: the button counts, and typing in the box renames.
