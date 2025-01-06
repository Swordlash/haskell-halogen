import {MDCTabBar} from '@material/tab-bar';
import {MDCList} from '@material/list';
import {MDCRipple} from '@material/ripple';

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