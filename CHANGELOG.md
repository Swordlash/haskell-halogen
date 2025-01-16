# Revision history for haskell-halogen

## 0.6.0 -- 2025-01-16

- Add `Halogen.Svg` modules
- Use `RequiredTypeArguments` for slot proxies

## 0.5.1 -- 2025-01-15

Fix `for` HTML property.

## 0.5.0 -- 2025-01-15

Allow monadic `initialState` function.

## 0.4.0 -- 2025-01-10

Fix unnecessary DOM replaces in foreign code

## 0.3.2 -- 2025-01-10

Add `-fexpose-all-unfoldings` to JS build and export `label` and `p` mistakenly hidden from `Halogen.HTML`

## 0.3.1 -- 2025-01-09

Use `isTrue#` for `reallyUnsafePtrEquality#`

## 0.3.0 -- 2025-01-08

Fix bugs related to style and class rendering, add IO specialisation, add `input` type to `H.Slot` to avoid `unsafeCoerce`.

## 0.2.0 -- 2024-12-28

More complete release, with events, properties and Clay integration

## 0.1.0 -- 2024-12-21

* First version. Released on an unsuspecting world.
