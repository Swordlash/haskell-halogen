import {MDCList} from '@material/list';

if (!window.Halogen)
  window.Halogen = {};

window.Halogen.init_material_list = function(element) {
  return new MDCList(element);
}