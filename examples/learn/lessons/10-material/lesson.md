title: Material components
layers: material
args: -package haskell-halogen-material

`haskell-halogen-material` wraps Google's Material Components as Halogen children. This form uses a text field, checkbox and button, each mounted with `HH.slot` as in lesson 4. The library also provides radio buttons, lists and tabs.

## The slots

`Slots` declares the children's queries, outputs and indices:

- `TextField.textField` emits `TextField.InputChanged` with the current text on each edit. The parent converts it to `Name t`. Its `Int` index allows multiple fields under one label.
- `Checkbox.checkbox` emits `Checkbox.CheckboxChange` with the checked state.
- `Button.button` emits `Button.ButtonClicked`, converted here to `Send`.

Each component takes a specification as input. Start with a default such as `TextField.emptyTextFieldSpec` and update fields, for example `TextField.label = Just "Name"`. The parent stores child outputs in `State` and reads them when handling `Send`.

## The monad

Material's JavaScript adds behaviours such as floating labels and ripples after elements mount. The components require `MonadMaterial m` for those browser effects. Text fields and checkboxes also require `MonadUUID m` to generate IDs connecting labels to inputs.

Your component declares these capabilities while remaining polymorphic in `m`. The lesson wrapper chooses `BrowserDOM`, which provides both instances. A custom application monad can provide the same instances. Native `BrowserDOM` bindings allow type checking but do not run Material in a browser.

The layer includes `material.js` and `material.css`. The tutorial loads both before mounting your component, just as an application would include them in its page.

## Exercise

Add an email field and show both submitted values:

1. Add `email` to `State` and `Email Text` to the action type.
2. Mount another `TextField.textField` with `HH.slot "field" 1`, label it `"Email"`, and convert its `InputChanged` output to `Email`. The index distinguishes it from the existing field.
3. Update `Send` to include `st.email` alongside `st.name` in `sent`.
