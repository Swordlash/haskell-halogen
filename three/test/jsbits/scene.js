// What the suite reads of a mounted scene (Test.Page): the renderer leaves
// its holder on its canvas as __halogenThree, and the objects are found in
// its scene by name. Disposals are counted by wrapping Three.js's own
// dispose, on the module the renderer imports (one instance per URL).

function halogen_three_test_holders() {
  var holders = [];
  var canvases = document.querySelectorAll("canvas");
  for (var i = 0; i < canvases.length; i++) if (canvases[i].__halogenThree) holders.push(canvases[i].__halogenThree);
  return holders;
}

// How many times the scenes on the page have been drawn.
function halogen_three_test_drawn() {
  return halogen_three_test_holders().reduce(function (n, holder) { return n + (holder.drawn || 0); }, 0);
}

function halogen_three_test_live() { return halogen_three_test_holders().length; }

// The colour of the named mesh's material, -1 if there is no such mesh.
function halogen_three_test_color(name) {
  var holders = halogen_three_test_holders();
  for (var i = 0; i < holders.length; i++) {
    var object = holders[i].scene && holders[i].scene.getObjectByName(name);
    if (object && object.material && object.material.color) return object.material.color.getHex();
  }
  return -1;
}

var halogen_three_test_disposed = { geometries: 0, materials: 0 };

function halogen_three_test_watch_disposals(url) {
  return import(url).then(function (three) {
    var watch = function (prototype, kind) {
      if (prototype.__halogenTestWatched) return;
      var dispose = prototype.dispose;
      prototype.dispose = function () {
        halogen_three_test_disposed[kind]++;
        return dispose.apply(this, arguments);
      };
      prototype.__halogenTestWatched = true;
    };
    watch(three.BufferGeometry.prototype, "geometries");
    watch(three.Material.prototype, "materials");
    return null;
  });
}

function halogen_three_test_disposed_geometries() { return halogen_three_test_disposed.geometries; }
function halogen_three_test_disposed_materials() { return halogen_three_test_disposed.materials; }
