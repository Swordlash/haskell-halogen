// Example.Late's side in the page: an entry pushed with its number in its
// state, the number of the entry the page is at (0 for one with none), and
// the dialog's methods.
function halogen_late_push(n) {
  history.pushState({halogenLate: n}, '');
}

function halogen_late_entry() {
  return history.state?.halogenLate ?? 0;
}

function halogen_late_show_modal(dialog) {
  dialog.showModal();
}

function halogen_late_close(dialog) {
  dialog.close();
}
