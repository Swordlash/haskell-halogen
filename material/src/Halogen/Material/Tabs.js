import {MDCTabBar} from '@material/tab-bar';

if (!window.Halogen) 
  window.Halogen = {};

window.Halogen.init_material_tab_bar = function(element) {
  return new MDCTabBar(element);
}