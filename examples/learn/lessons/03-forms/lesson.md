title: Properties and events

What is typed into a form's field is kept by the browser, in the `<input>`, not in your state. A component that needs it binds the two together, both ways:

- `HE.onValueInput SetName` turns every change to the field into the action `SetName`, with the field's text as it is now, which `handleAction` writes into the state;
- `HP.value state.name` sets the field's text from the state, so the field follows when the state changes for another reason: `Submit` empties `name`, and the field empties with it.

`HP` is `Halogen.HTML.Properties`. Beside `HP.value` it has `HP.placeholder`, `HP.type_` (given `InputCheckbox` or `ButtonSubmit`, from `DOM.HTML.Indexed`) and `HP.checked`. The checkbox is bound the same way, with `HP.checked` and `HE.onChecked`, whose action gets a `Bool`. Each property is checked against its element: `HP.checked` on a button does not compile. `state.name` reads a field of the record, which needs `OverloadedRecordDot` at the top of the file.

## Submitting

Pressing Enter in the field, or clicking the button of type `ButtonSubmit`, submits the form, and `HE.onSubmit` turns that into `Submit`. The browser has a default action for a submit too: it sends the form off and loads the page that comes back, which here is this page, from scratch. `HE.preventingDefault` around the handler keeps it from doing that.

You could also prevent it yourself: call `preventDefault`, from `Web.Event.Event`, on the event in `handleAction`, inside `H.liftEffect`, which runs it at once. That is in time only when the action runs while the browser is still dispatching the event, which is so only if Halogen was free when the event arrived. If it was busy with other work, the action waits its turn, and by then the browser has submitted the form. `HE.preventingDefault` prevents the default in the event listener itself, before the action is handed to Halogen, so it works whatever else is going on.

## Exercise

Keep what is submitted. Add a field `submitted :: [Text]` to `State`, starting as `[]`, and have `Submit` add the name to its end, in capitals when Shout is ticked (`T.toUpper`, with `Data.Text` imported as `T`). Show the list under the form: an `HH.ul_` with an `HH.li_` for each item, which a list comprehension such as `[HH.li_ [HH.text item] | item <- state.submitted]` makes.
