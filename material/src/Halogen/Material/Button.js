import {MDCRipple} from '@material/ripple';

if (!window.Halogen)
  window.Halogen = {};

window.Halogen.init_material_button = function(element) {
  return new MDCRipple(element);
}