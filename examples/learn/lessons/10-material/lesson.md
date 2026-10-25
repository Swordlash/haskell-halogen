title: Material components
layers: material
args: -package haskell-halogen-material

`haskell-halogen-material` wraps Google's Material Components for the web as Halogen components: a text field, a checkbox, a button, and others (radio buttons, lists, tabs). Each one is a child component, used with `HH.slot` as the counter was in lesson 4, so this form is a parent with three kinds of children.

## The slots

`Slots` names them. Each slot's type says what the child answers to and what it raises:

- `TextField.textField` raises `TextField.InputChanged` with the text on every keystroke, which becomes `Name t`. Its slot is numbered by an `Int`, so that a form can have several.
- `Checkbox.checkbox` raises `Checkbox.CheckboxChange` with whether it is now checked.
- `Button.button` raises `Button.ButtonClicked`, here `Send`.

Each takes a spec as its input: the library's empty one, `TextField.emptyTextFieldSpec`, with the fields you need changed, `TextField.label = Just "Name"`. The parent keeps what its children tell it in its `State`, where `Send` reads it.

## The monad

Material's components are drawn by its JavaScript: once a text field is on the page, Material's code takes over the element, to float its label and draw its ripple. That is an effect a component can only have in a browser, so the library asks for it as a class, `MonadMaterial m`, and `TextField.textField` works in any `m` with an instance. The text field and the checkbox also make fresh ids, to tie each label to its input, which is `MonadUUID m`.

So `component` names those two constraints and is still polymorphic in `m`: it says what it needs, not where it runs. The program around it picks the monad, `BrowserDOM`, which has both instances; an application with a monad of its own gives that monad the two instances and uses the same component. Natively, `BrowserDOM`'s `MonadMaterial` instance only lets code type-check.

That JavaScript, and the stylesheet that makes the components look like Material, come with the layer as `material.js` and `material.css`. The program around your module adds both to the page, as an application's `index.html` would, and mounts the component once they have loaded.

## Exercise

Add a second text field for an email address, and show both values when the form is sent:

1. a field `email` in `State` and an action `Email Text`;
2. a second `HH.slot "field" 1 TextField.textField`, labelled `"Email"`, whose `InputChanged` becomes `Email`. It shares the slot `"field"` with the first one and is told apart by its number;
3. `Send` puts `st.email` into `sent` beside `st.name`.
