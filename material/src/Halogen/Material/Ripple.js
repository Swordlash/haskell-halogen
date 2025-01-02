import {MDCRipple} from '@material/ripple';

if (!window.Halogen)
  window.Halogen = {};

window.Halogen.init_material_ripple = function(element) {
  return new MDCRipple(element);
}