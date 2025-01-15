import {MDCTabBar} from '@material/tab-bar';
import {MDCList} from '@material/list';
import {MDCRipple} from '@material/ripple';
import {MDCTextField} from '@material/textfield';
import {MDCFormField} from '@material/form-field';
import {MDCRadio} from '@material/radio';

function halogen_init_material_ripple(element) {
  return new MDCRipple(element);
}

function halogen_destroy_material_ripple(mdcRipple) {
  mdcRipple.destroy();
}

function halogen_init_material_list(element) {
  return new MDCList(element);
}

function halogen_init_material_list_items(mdcList) {
  return mdcList.listElements.map((listItemEl) => new MDCRipple(listItemEl));
}

function halogen_destroy_material_list(mdcList) {
  mdcList.destroy();
}

function halogen_init_material_tab_bar(element) {
  return new MDCTabBar(element);
}

function halogen_destroy_material_tab_bar(mdcTabBar) {
  mdcTabBar.destroy();
}

function halogen_init_material_text_field(element) {
  return new MDCTextField(element);
}

function halogen_destroy_material_text_field(mdcTextField) {
  mdcTextField.destroy();
}

function halogen_init_material_radio_button(element) {
  const radio = new MDCRadio(element);
  const formField = new MDCFormField(element.parentElement);
  formField.input = radio;

  return formField;
}

function halogen_destroy_material_radio_button(mdcFormField) {
  mdcFormField.input.destroy();
  mdcFormField.destroy();
}