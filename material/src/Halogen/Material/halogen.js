import {MDCRipple} from '@material/ripple';

//export var Halogen = {};

window.Halogen = {};
window.Halogen.init_material_button = function(element) {
  return new MDCRipple(element);
}