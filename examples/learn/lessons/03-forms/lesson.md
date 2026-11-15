title: Properties and events

The browser stores text typed into an `<input>`. To keep component state and the field in sync, connect them in both directions:

- `HE.onValueInput SetName` produces a `SetName` action containing the current text. `handleAction` saves it in state.
- `HP.value state.name` sets the displayed value from state. When `Submit` clears `name`, the field clears too.

`HP` is `Halogen.HTML.Properties`. It provides `HP.value`, `HP.placeholder`, `HP.type_` and `HP.checked`. Input types such as `InputCheckbox` and button types such as `ButtonSubmit` come from `DOM.HTML.Indexed`.

Checkboxes use `HP.checked` and `HE.onChecked`, which supplies a `Bool`. Properties are checked against the element type: `HP.checked` on a button does not compile. Reading records with `state.name` requires `OverloadedRecordDot`.

## Submitting

Pressing Enter in the field or clicking a `ButtonSubmit` button submits the form. `HE.onSubmit` converts that event into `Submit`.

The browser would normally submit the form and navigate, reloading this page. Wrap the handler in `HE.preventingDefault` to prevent that behaviour.

Calling `preventDefault` from `Web.Event.Event` through `H.liftEffect` inside an action depends on the action running before browser dispatch ends. If the action is queued, it may be too late. `HE.preventingDefault` acts in the event listener before handing the action to Halogen, so it also works when the component is busy.

## Exercise

Add `submitted :: [Text]` to `State`, initially `[]`. Make `Submit` append the current name, using `T.toUpper` when Shout is checked. Import `Data.Text qualified as T`.

Display submitted names below the form in an `HH.ul_`. Build its children with `[HH.li_ [HH.text item] | item <- state.submitted]`.
