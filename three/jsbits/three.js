// The Three.js side of Halogen.Three: a renderer on the component's canvas,
// a scene the reconciler builds out of Three.js objects, a camera, and the
// pointer events Three.js does not have, raycast from the canvas.
//
// One holder per mounted scene. It is made before Three.js has loaded (the
// Haskell side keeps it from the start) and filled in once the module has.

function halogen_three_new_holder() {
  return {
    three: null,
    renderer: null,
    scene: null,
    camera: null,
    // The orthographic camera's height, which its frustum is fitted to.
    height: 1,
    canvas: null,
    ready: false,
    frame: 0,
    observer: null,
    listeners: [],
    // The renderer's own pointer handlers (Halogen.Three's orbit), by kind:
    // "down", "move", "up", "wheel".
    gesture: {},
    raycaster: null,
    pointer: null,
    // Scratch for the depth of a hit.
    depth: null,
    // The mesh the pointer went down on, for a click.
    down: null,
    // The objects under the pointer (the mesh hit and its ancestors), and
    // the hit, for pointerover and pointerout.
    hover: [],
    hoverHit: null,
  };
}

function halogen_three_initialize(holder, moduleUrl, canvas, done) {
  holder.canvas = canvas;
  import(moduleUrl)
    .then(function (three) {
      holder.three = three;
      // alpha: a scene with no background shows the page behind it.
      holder.renderer = new three.WebGLRenderer({ canvas: canvas, antialias: true, alpha: true });
      holder.renderer.setPixelRatio(Math.min(globalThis.devicePixelRatio || 1, 2));
      holder.scene = new three.Scene();
      holder.raycaster = new three.Raycaster();
      holder.pointer = new three.Vector2();
      holder.depth = new three.Vector3();
      // What a mesh is made of before its props say, never drawn: shared,
      // and so never disposed of with a mesh.
      holder.emptyGeometry = new three.BufferGeometry();
      holder.emptyGeometry.__halogenShared = true;
      holder.emptyMaterial = new three.MeshBasicMaterial();
      holder.emptyMaterial.__halogenShared = true;
      // For a page's own tools (and the test suite) to find the scene.
      canvas.__halogenThree = holder;
      halogen_three_listen(holder);
      holder.ready = true;
      halogen_three_observe(holder);
      done(null);
    })
    .catch(function (error) {
      console.error("Could not load or initialize Three.js", error);
      done(null);
    });
}

function halogen_three_ready(holder) { return holder.ready; }

// The canvas follows its parent's size, as Pixi's does: the parent is
// observed, and the drawing buffer and the canvas's style are set to its
// size. Styling the canvas, rather than leaving it at 100%, keeps a parent
// whose height is its content's from growing with the drawing buffer (a
// canvas's own size is its buffer's in CSS pixels, twice the screen's on a
// display of two pixels to one). A resize clears the buffer, so the scene
// is drawn again at once, inside the observer's delivery: drawing changes
// no layout, so that is no observer loop.
function halogen_three_observe(holder) {
  var parent = holder.canvas.parentElement;
  if (parent && typeof ResizeObserver !== "undefined") {
    holder.observer = new ResizeObserver(function () { halogen_three_resize(holder); });
    holder.observer.observe(parent);
  }
  halogen_three_resize(holder);
}

function halogen_three_resize(holder) {
  if (!holder.ready) return;
  var canvas = holder.canvas;
  var parent = canvas.parentElement;
  // A parent taken out of the page, or hidden, has nothing to show.
  if (!parent || !canvas.isConnected) return;
  var width = parent.clientWidth;
  var height = parent.clientHeight;
  if (width === 0 || height === 0) return;
  var size = holder.renderer.getSize(new holder.three.Vector2());
  if (size.x === width && size.y === height) return;
  holder.renderer.setSize(width, height, true);
  halogen_three_fit(holder);
  halogen_three_draw(holder);
}

// The camera's frustum, for the canvas's shape.
function halogen_three_fit(holder) {
  var camera = holder.camera;
  if (!camera) return;
  var size = holder.renderer.getSize(new holder.three.Vector2());
  var aspect = size.x > 0 && size.y > 0 ? size.x / size.y : 1;
  if (camera.isPerspectiveCamera) {
    camera.aspect = aspect;
  } else {
    var half = holder.height / 2;
    camera.left = -half * aspect;
    camera.right = half * aspect;
    camera.top = half;
    camera.bottom = -half;
  }
  camera.updateProjectionMatrix();
}

function halogen_three_place_camera(camera, near, far, px, py, pz, tx, ty, tz) {
  camera.near = near;
  camera.far = far;
  camera.up.set(0, 1, 0);
  camera.position.set(px, py, pz);
  camera.lookAt(tx, ty, tz);
}

function halogen_three_set_perspective(holder, fov, near, far, px, py, pz, tx, ty, tz) {
  if (!holder.camera || !holder.camera.isPerspectiveCamera) holder.camera = new holder.three.PerspectiveCamera();
  holder.camera.fov = fov;
  halogen_three_place_camera(holder.camera, near, far, px, py, pz, tx, ty, tz);
  halogen_three_fit(holder);
}

function halogen_three_set_orthographic(holder, height, near, far, px, py, pz, tx, ty, tz) {
  if (!holder.camera || !holder.camera.isOrthographicCamera) holder.camera = new holder.three.OrthographicCamera();
  holder.height = height;
  halogen_three_place_camera(holder.camera, near, far, px, py, pz, tx, ty, tz);
  halogen_three_fit(holder);
}

// A colour, or none (a negative number): the page shows through.
function halogen_three_set_background(holder, color) {
  holder.scene.background = color < 0 ? null : new holder.three.Color(color);
}

// The scene is drawn on demand, not in a loop: once in the next frame,
// however many changes are asked for before it.
function halogen_three_request_render(holder) {
  if (!holder.ready || holder.frame) return;
  holder.frame = requestAnimationFrame(function () {
    holder.frame = 0;
    halogen_three_draw(holder);
  });
}

function halogen_three_draw(holder) {
  if (!holder.ready || !holder.camera) return;
  holder.renderer.render(holder.scene, holder.camera);
  holder.drawn = (holder.drawn || 0) + 1;
}

// The world is the scene's only child: anything else there is a world left
// behind.
function halogen_three_set_root(holder, root) {
  var scene = holder.scene;
  for (var i = scene.children.length - 1; i >= 0; i--) if (scene.children[i] !== root) scene.remove(scene.children[i]);
  if (root.parent !== scene) scene.add(root);
}

function halogen_three_new_object(holder, name) {
  var three = holder.three;
  switch (name) {
    case "group": return new three.Group();
    case "mesh": return new three.Mesh(holder.emptyGeometry, holder.emptyMaterial);
    case "ambientLight": return new three.AmbientLight();
    case "directionalLight": return new three.DirectionalLight();
    case "pointLight": return new three.PointLight();
    case "axes": return new three.AxesHelper(1);
    default: return null;
  }
}

// The child at that index among its parent's children, as the reconciler
// counts them.
function halogen_three_insert_child_ix(parent, child, index) {
  if (child.parent !== parent) parent.add(child);
  var children = parent.children;
  var at = children.indexOf(child);
  if (at !== index && index < children.length) {
    children.splice(at, 1);
    children.splice(index, 0, child);
  }
}

// Out of the tree, and its own GPU resources let go of: Three.js keeps a
// geometry's buffers and a material's program until told. Only its own: the
// reconciler takes each of its children out of it in turn as it halts them,
// which lets theirs go.
function halogen_three_remove_child(parent, child) {
  parent.remove(child);
  halogen_three_dispose_own(child);
}

function halogen_three_dispose_own(object) {
  if (object.isMesh || object.isLine) {
    if (object.geometry && !object.geometry.__halogenShared) object.geometry.dispose();
    if (object.material && !object.material.__halogenShared) object.material.dispose();
  } else if (object.isLight && object.dispose) {
    object.dispose();
  }
}

function halogen_three_parent_of(object) { return object.parent ?? null; }
function halogen_three_same_object(a, b) { return a === b; }

// A geometry by kind (Halogen.Three's order): box, sphere, plane, cylinder,
// torus. The old one is disposed of.
function halogen_three_set_geometry(holder, object, kind, a, b, c, d) {
  var three = holder.three;
  var geometry;
  switch (kind) {
    case 0: geometry = new three.BoxGeometry(a, b, c); break;
    case 1: geometry = new three.SphereGeometry(a, b, c); break;
    case 2: geometry = new three.PlaneGeometry(a, b); break;
    case 3: geometry = new three.CylinderGeometry(a, b, c, d); break;
    default: geometry = new three.TorusGeometry(a, b, c, d); break;
  }
  halogen_three_swap_geometry(object, geometry);
}

function halogen_three_clear_geometry(holder, object) { halogen_three_swap_geometry(object, holder.emptyGeometry); }

function halogen_three_swap_geometry(object, geometry) {
  var old = object.geometry;
  object.geometry = geometry;
  if (old && old !== geometry && !old.__halogenShared) old.dispose();
}

// A material by kind: basic, standard, phong. One of the kind the mesh has
// already is changed in place; another kind replaces it, and the old one is
// disposed of.
function halogen_three_set_material(holder, object, kind, color, opacity, wireframe, a, b) {
  var three = holder.three;
  var Kind = kind === 0 ? three.MeshBasicMaterial : kind === 1 ? three.MeshStandardMaterial : three.MeshPhongMaterial;
  var material = object.material;
  if (!material || material.__halogenShared || material.constructor !== Kind) {
    material = new Kind();
    halogen_three_swap_material(object, material);
  }
  material.color.setHex(color);
  material.opacity = opacity;
  var transparent = opacity < 1;
  // A material that turns transparent or opaque is another program.
  if (material.transparent !== transparent) {
    material.transparent = transparent;
    material.needsUpdate = true;
  }
  material.wireframe = wireframe;
  if (kind === 1) {
    material.metalness = a;
    material.roughness = b;
  } else if (kind === 2) {
    material.shininess = a;
  }
}

function halogen_three_clear_material(holder, object) { halogen_three_swap_material(object, holder.emptyMaterial); }

function halogen_three_swap_material(object, material) {
  var old = object.material;
  object.material = material;
  if (old && old !== material && !old.__halogenShared) old.dispose();
}

function halogen_three_set_position(object, x, y, z) { object.position.set(x, y, z); }
function halogen_three_set_rotation(object, x, y, z) { object.rotation.set(x, y, z); }
function halogen_three_set_scale(object, x, y, z) { object.scale.set(x, y, z); }
function halogen_three_set_visible(object, visible) { object.visible = visible; }
function halogen_three_set_name(object, name) { object.name = name; }
function halogen_three_set_light(object, color, intensity) {
  if (!object.isLight) return;
  object.color.setHex(color);
  object.intensity = intensity;
}

// An object's handlers, by event name: the renderer finds them when a ray
// meets the object or a mesh in it.
var halogen_three_hover_handlers = 0;
function halogen_three_is_hover(eventName) { return eventName === "pointerover" || eventName === "pointerout"; }
function halogen_three_on(object, eventName, callback) {
  var handlers = object.__halogenHandlers || (object.__halogenHandlers = {});
  if (!handlers[eventName] && halogen_three_is_hover(eventName)) halogen_three_hover_handlers++;
  handlers[eventName] = callback;
}
function halogen_three_off(object, eventName, callback) {
  var handlers = object.__halogenHandlers;
  if (!handlers || handlers[eventName] !== callback) return;
  delete handlers[eventName];
  if (halogen_three_is_hover(eventName)) halogen_three_hover_handlers--;
}

// The nearest visible mesh under the pointer, or null. A point outside the
// canvas (a drag that left it) meets nothing: a ray through it could meet
// an object out of view.
function halogen_three_hit(holder, event) {
  if (!holder.ready || !holder.camera) return null;
  var rect = holder.canvas.getBoundingClientRect();
  if (!rect.width || !rect.height) return null;
  var x = ((event.clientX - rect.left) / rect.width) * 2 - 1;
  var y = 1 - ((event.clientY - rect.top) / rect.height) * 2;
  if (x < -1 || x > 1 || y < -1 || y > 1) return null;
  holder.pointer.set(x, y);
  // The camera's matrices are brought up to date at a draw; a pointer event
  // between a camera move and its frame casts from where the camera is now.
  holder.camera.updateMatrixWorld();
  holder.raycaster.setFromCamera(holder.pointer, holder.camera);
  var hits = holder.raycaster.intersectObjects(holder.scene.children, true);
  for (var i = 0; i < hits.length; i++) {
    if (hits[i].object.isMesh && halogen_three_shown(hits[i].object) && halogen_three_in_view(holder, hits[i])) return hits[i];
  }
  return null;
}

// Whether a hit is between the camera's near and far planes: the raycaster
// casts from the camera's position to infinity whatever the camera clips,
// so a mesh nearer than the near plane (or beyond the far one), which is not
// drawn, could otherwise take a click from the mesh drawn behind it. The
// depth is the point's distance along the view axis, as the planes are set,
// for either kind of camera.
function halogen_three_in_view(holder, hit) {
  var depth = -holder.depth.copy(hit.point).applyMatrix4(holder.camera.matrixWorldInverse).z;
  return depth >= holder.camera.near && depth <= holder.camera.far;
}

// Three.js's raycaster does not ask whether an object is shown.
function halogen_three_shown(object) {
  for (var o = object; o; o = o.parent) if (!o.visible) return false;
  return true;
}

// The object or the nearest of its ancestors with a handler for the event.
function halogen_three_listener(object, eventName) {
  for (var o = object; o; o = o.parent) if (o.__halogenHandlers && o.__halogenHandlers[eventName]) return o;
  return null;
}

function halogen_three_event(hit, event) {
  return {
    x: hit.point.x,
    y: hit.point.y,
    z: hit.point.z,
    name: hit.object.name || "",
    button: event.button ?? 0,
    // The modifier keys held, as bits: Shift 1, Ctrl 2, Alt 4, Meta 8.
    modifiers: (event.shiftKey ? 1 : 0) | (event.ctrlKey ? 2 : 0) | (event.altKey ? 4 : 0) | (event.metaKey ? 8 : 0),
  };
}

function halogen_three_dispatch(target, eventName, hit, event) {
  if (target) target.__halogenHandlers[eventName](halogen_three_event(hit, event));
}

function halogen_three_path(object) {
  var path = [];
  for (var o = object; o; o = o.parent) path.push(o);
  return path;
}

// Over and out, as the pointer's mesh changes: out of the objects it left
// (the mesh and those of its ancestors that are not the new one's), over
// those it came to.
function halogen_three_hover(holder, hit, event) {
  var before = holder.hover;
  var after = hit ? halogen_three_path(hit.object) : [];
  var left = holder.hoverHit;
  holder.hover = after;
  holder.hoverHit = hit;
  for (var i = 0; i < before.length; i++) {
    var o = before[i];
    if (after.indexOf(o) < 0 && o.__halogenHandlers && o.__halogenHandlers.pointerout) halogen_three_dispatch(o, "pointerout", left, event);
  }
  for (var j = after.length - 1; j >= 0; j--) {
    var p = after[j];
    if (before.indexOf(p) < 0 && p.__halogenHandlers && p.__halogenHandlers.pointerover) halogen_three_dispatch(p, "pointerover", hit, event);
  }
}

// The canvas's listeners: the renderer's own handlers first (the orbit, which
// decides whether a release ends a drag rather than a click), then the
// objects'.
function halogen_three_listen(holder) {
  var canvas = holder.canvas;
  var on = function (type, listener, options) {
    canvas.addEventListener(type, listener, options);
    holder.listeners.push([type, listener]);
  };
  var gesture = function (kind, event) {
    var handler = holder.gesture[kind];
    if (handler) handler(event);
  };
  on("pointerdown", function (event) {
    // A drag that leaves the canvas is still the canvas's.
    if (event.isPrimary && canvas.setPointerCapture) {
      try { canvas.setPointerCapture(event.pointerId); } catch (_) { /* the pointer is gone */ }
    }
    gesture("down", event);
    var hit = halogen_three_hit(holder, event);
    holder.down = hit ? hit.object : null;
    if (hit) halogen_three_dispatch(halogen_three_listener(hit.object, "pointerdown"), "pointerdown", hit, event);
  });
  on("pointermove", function (event) {
    gesture("move", event);
    if (halogen_three_hover_handlers > 0) halogen_three_hover(holder, halogen_three_hit(holder, event), event);
  });
  on("pointerup", function (event) {
    gesture("up", event);
    var down = holder.down;
    holder.down = null;
    var hit = halogen_three_hit(holder, event);
    if (!hit) return;
    halogen_three_dispatch(halogen_three_listener(hit.object, "pointerup"), "pointerup", hit, event);
    // A click is a press and a release on one listener.
    var target = halogen_three_listener(hit.object, "click");
    if (target && down && halogen_three_listener(down, "click") === target) halogen_three_dispatch(target, "click", hit, event);
  });
  on("pointercancel", function (event) {
    gesture("up", event);
    holder.down = null;
  });
  on("pointerleave", function (event) {
    if (holder.hover.length) halogen_three_hover(holder, null, event);
  });
  on("wheel", function (event) { gesture("wheel", event); }, { passive: false });
}

function halogen_three_on_gesture(holder, kind, callback) { holder.gesture[kind] = callback; }

function halogen_three_event_x(event) { return event.x; }
function halogen_three_event_y(event) { return event.y; }
function halogen_three_event_z(event) { return event.z; }
function halogen_three_event_name(event) { return event.name; }
function halogen_three_event_button(event) { return event.button; }
function halogen_three_event_modifiers(event) { return event.modifiers; }

function halogen_three_pointer_id(event) { return event.pointerId ?? 0; }
function halogen_three_is_primary(event) { return event.isPrimary !== false; }
function halogen_three_client_x(event) { return event.clientX; }
function halogen_three_client_y(event) { return event.clientY; }
function halogen_three_button(event) { return event.button ?? 0; }
function halogen_three_delta_y(event) {
  // A wheel that counts in lines or pages, in pixels as most do.
  return event.deltaMode === 1 ? event.deltaY * 16 : event.deltaMode === 2 ? event.deltaY * 400 : event.deltaY;
}
function halogen_three_prevent_default(event) { event.preventDefault(); }
function halogen_three_canvas_height(holder) { return holder.canvas ? holder.canvas.clientHeight : 0; }

function halogen_three_schedule_timeout(callback, milliseconds) { return setTimeout(callback, milliseconds); }
function halogen_three_cancel_timeout(timer) { clearTimeout(timer); }

// Everything let go of: the listeners, the observer, whatever is still in
// the scene (the reconciler has taken out what it built, but a world left
// behind would hold its own), the shared placeholders, and the renderer
// with its WebGL context, which a page has only a few of.
function halogen_three_destroy(holder) {
  holder.ready = false;
  if (holder.frame) cancelAnimationFrame(holder.frame);
  holder.frame = 0;
  if (holder.observer) holder.observer.disconnect();
  holder.observer = null;
  var canvas = holder.canvas;
  for (var i = 0; i < holder.listeners.length; i++) canvas.removeEventListener(holder.listeners[i][0], holder.listeners[i][1]);
  holder.listeners = [];
  holder.gesture = {};
  holder.hover = [];
  holder.hoverHit = null;
  if (holder.scene) {
    holder.scene.traverse(function (object) {
      var handlers = object.__halogenHandlers;
      if (handlers) for (var name in handlers) halogen_three_off(object, name, handlers[name]);
      halogen_three_dispose_own(object);
    });
    holder.scene.clear();
  }
  if (holder.emptyGeometry) holder.emptyGeometry.dispose();
  if (holder.emptyMaterial) holder.emptyMaterial.dispose();
  if (holder.renderer) {
    holder.renderer.dispose();
    holder.renderer.forceContextLoss();
  }
  if (canvas && canvas.__halogenThree === holder) delete canvas.__halogenThree;
}
