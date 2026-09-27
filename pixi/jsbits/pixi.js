function halogen_pixi_new_application() { return { pixi: null, app: null, ready: false }; }
function halogen_pixi_initialize_application(holder, moduleUrl, canvas, done) {
  import(moduleUrl)
    .then(function (pixi) {
      holder.pixi = pixi;
      holder.app = new pixi.Application();
      return holder.app.init({ canvas: canvas, resizeTo: canvas.parentElement, preference: "webgl", antialias: true, autoDensity: true, resolution: Math.min(globalThis.devicePixelRatio || 1, 2), backgroundColor: 0x111827 });
    })
    .then(function () { holder.ready = true; done(null); })
    .catch(function (error) { console.error("Could not load or initialize PixiJS", error); done(null); });
}
function halogen_pixi_application_created(holder) { return holder.app !== null; }
function halogen_pixi_application_ready(holder) { return holder.ready; }
function halogen_pixi_destroy_application(holder) { holder.app.destroy(false, { children: true, texture: false, textureSource: false }); }
function halogen_pixi_new_container(holder) { return new holder.pixi.Container(); }
function halogen_pixi_new_graphics(holder) { return new holder.pixi.Graphics(); }
function halogen_pixi_add_to_stage(holder, child) { holder.app.stage.addChild(child); }
function halogen_pixi_add_child(parent, child) { parent.addChild(child); halogen_pixi_touch(parent); }
function halogen_pixi_remove_child(parent, child) { parent.removeChild(child); halogen_pixi_touch(parent); }
function halogen_pixi_parent_of(object) { return object.parent ?? null; }
function halogen_pixi_set_child_index(parent, child, index) { parent.setChildIndex(child, index); }
function halogen_pixi_destroy_object(object) { object.destroy({ children: true, texture: false, textureSource: false }); }
function halogen_pixi_clear_graphics(object) { object.clear(); }
function halogen_pixi_move_to(object, x, y) { object.moveTo(x, y); }
function halogen_pixi_line_to(object, x, y) { object.lineTo(x, y); }
function halogen_pixi_rect(object, x, y, width, height) { object.rect(x, y, width, height); }
function halogen_pixi_circle(object, x, y, radius) { object.circle(x, y, radius); }
function halogen_pixi_ellipse(object, x, y, radiusX, radiusY) { object.ellipse(x, y, radiusX, radiusY); }
function halogen_pixi_quadratic_curve_to(object, controlX, controlY, endX, endY) { object.quadraticCurveTo(controlX, controlY, endX, endY); }
function halogen_pixi_bezier_curve_to(object, control1X, control1Y, control2X, control2Y, endX, endY) { object.bezierCurveTo(control1X, control1Y, control2X, control2Y, endX, endY); }
function halogen_pixi_arc(object, x, y, radius, startAngle, endAngle, anticlockwise) { object.arc(x, y, radius, startAngle, endAngle, anticlockwise); }
function halogen_pixi_svg_path(holder, object, commands) { object.path(new holder.pixi.GraphicsPath(commands)); }
function halogen_pixi_fill(object, color, alpha) { object.fill({ color: color, alpha: alpha }); }
function halogen_pixi_stroke(object, color, width, alpha) { object.stroke({ color: color, width: width, alpha: alpha }); }
function halogen_pixi_new_text(holder) { return new holder.pixi.Text({ text: "", style: {} }); }
function halogen_pixi_set_system_text(object, value, family, size, color, align) {
  object.__halogenFontRequest = null;
  object.text = value;
  object.style = { fontFamily: family, fontSize: size, fill: color, align: align };
}
function halogen_pixi_set_asset_text(holder, object, value, family, source, size, color, align) {
  var request = {};
  object.__halogenFontRequest = request;
  object.text = value;
  // The asset's family is not named until its face has loaded. Pixi measures a
  // font's ascent and descent once per font string and caches them; named any
  // earlier, it would measure whatever face the browser substitutes and keep
  // that, sizing the text's texture for the wrong glyphs and cutting off what
  // does not fit. Until then the text is drawn in the generic fallback, whose
  // font string is a different one.
  object.style = { fontFamily: "sans-serif", fontSize: size, fill: color, align: align };
  holder.pixi.Assets.load({ src: source, data: { family: family } })
    .then(function () {
      if (object.destroyed || object.__halogenFontRequest !== request) return;
      object.style = { fontFamily: family, fontSize: size, fill: color, align: align };
      halogen_pixi_refresh_outline(object);
    })
    .catch(function (error) { console.error("Could not load PixiJS font", source, error); });
}
function halogen_pixi_clear_text(object) { object.__halogenFontRequest = null; object.text = ""; }
function halogen_pixi_clear_texture(holder, object) {
  object.__halogenAsset = null;
  object.__halogenSize = null;
  object.texture = holder.pixi.Texture.EMPTY;
  halogen_pixi_resize(object);
}
function halogen_pixi_set_texture(holder, object, asset) {
  object.__halogenAsset = asset;
  holder.pixi.Assets.load(asset)
    .then(function (texture) {
      if (object.destroyed || object.__halogenAsset !== asset) return;
      object.texture = texture;
      halogen_pixi_resize(object);
      halogen_pixi_refresh_outline(object);
    })
    .catch(function (error) { console.error("Could not load PixiJS texture", asset, error); });
}
// A sprite's on-screen size and its transform scale both land on the same
// Pixi property, and the texture they are relative to arrives asynchronously.
// Keep both requests on the object and derive the scale from whichever of
// them is known.
function halogen_pixi_resize(object) {
  var size = object.__halogenSize;
  var scale = object.__halogenScale || { x: 1, y: 1 };
  var texture = size ? object.texture : null;
  var naturalWidth = texture ? texture.orig.width : 0;
  var naturalHeight = texture ? texture.orig.height : 0;
  object.scale.set(
    naturalWidth ? (size.width / naturalWidth) * scale.x : scale.x,
    naturalHeight ? (size.height / naturalHeight) * scale.y : scale.y
  );
}
// Draw a border around what the object actually turned out to be. Nothing in
// the scene can know that — a label's extent is whatever the font laid out —
// so the measurement happens here, against the same bounds Pixi hit-tests.
//
// The outline is a child, so it is measured with itself detached, and it
// cancels out the object's own scale so that the stroke stays one unit wide
// however the object was sized.
// A group drawn into a texture ('cacheAsTexture') is drawn again when
// anything inside it has changed: Pixi itself does not notice. Every change
// the reconciler or a load makes passes through here or through a child
// being added or removed.
function halogen_pixi_touch(object) {
  for (var o = object; o; o = o.parent) if (o.__halogenCached) o.__halogenDirty = true;
}
function halogen_pixi_set_cache_as_texture(holder, object, resolution) {
  object.cacheAsTexture({ resolution: resolution });
  object.__halogenCached = true;
  object.__halogenDirty = false;
  holder.__halogenCaches = holder.__halogenCaches || new Set();
  holder.__halogenCaches.add(object);
  if (!holder.__halogenCacheTick) {
    // Before the frame is rendered, once a frame at most.
    holder.__halogenCacheTick = function () {
      holder.__halogenCaches.forEach(function (o) {
        if (o.destroyed || !o.__halogenCached) { holder.__halogenCaches.delete(o); return; }
        if (o.__halogenDirty) { o.__halogenDirty = false; o.updateCacheTexture(); }
      });
    };
    holder.app.ticker.add(holder.__halogenCacheTick);
  }
}
function halogen_pixi_clear_cache_as_texture(object) {
  object.cacheAsTexture(false);
  delete object.__halogenCached;
  delete object.__halogenDirty;
}
function halogen_pixi_refresh_outline(object) {
  halogen_pixi_touch(object);
  var spec = object.__halogenOutline;
  if (!spec) return;
  var graphics = object.__halogenOutlineGraphics;
  if (!graphics) {
    graphics = new spec.holder.pixi.Graphics();
    graphics.eventMode = "none";
    object.__halogenOutlineGraphics = graphics;
  }
  if (graphics.parent) graphics.parent.removeChild(graphics);
  var bounds = object.getLocalBounds();
  var scaleX = object.scale.x || 1;
  var scaleY = object.scale.y || 1;
  graphics.clear();
  graphics.scale.set(1 / scaleX, 1 / scaleY);
  graphics.rect(
    bounds.x * scaleX - spec.padding,
    bounds.y * scaleY - spec.padding,
    bounds.width * scaleX + spec.padding * 2,
    bounds.height * scaleY + spec.padding * 2
  );
  graphics.stroke({ color: spec.color, width: spec.width, alpha: spec.alpha });
  object.addChild(graphics);
}
function halogen_pixi_set_outline(holder, object, color, width, alpha, padding) {
  object.__halogenOutline = { holder: holder, color: color, width: width, alpha: alpha, padding: padding };
  halogen_pixi_refresh_outline(object);
}
function halogen_pixi_clear_outline(object) {
  object.__halogenOutline = null;
  var graphics = object.__halogenOutlineGraphics;
  object.__halogenOutlineGraphics = null;
  if (graphics) graphics.destroy();
}
function halogen_pixi_new_sprite(holder) { return new holder.pixi.Sprite(holder.pixi.Texture.EMPTY); }
function halogen_pixi_center_anchor(object) { object.anchor.set(0.5); }
function halogen_pixi_set_position(object, x, y) { object.position.set(x, y); }
function halogen_pixi_set_scale(object, x, y) { object.__halogenScale = { x: x, y: y }; halogen_pixi_resize(object); }
function halogen_pixi_set_rotation(object, rotation) { object.rotation = rotation; }
function halogen_pixi_set_size(object, width, height) { object.__halogenSize = { width: width, height: height }; halogen_pixi_resize(object); }
function halogen_pixi_on(object, eventType, callback) { object.on(eventType, callback); }
function halogen_pixi_off(object, eventType, callback) { object.off(eventType, callback); }
function halogen_pixi_set_event_mode(object, mode) { object.eventMode = mode; }
function halogen_pixi_set_cursor(object, cursor) { object.cursor = cursor; }
function halogen_pixi_set_rect_hit_area(holder, object, x, y, width, height) { object.hitArea = new holder.pixi.Rectangle(x, y, width, height); }
function halogen_pixi_set_circle_hit_area(holder, object, x, y, radius) { object.hitArea = new holder.pixi.Circle(x, y, radius); }
function halogen_pixi_set_polygon_hit_area(holder, object, coordinates) { object.hitArea = new holder.pixi.Polygon(coordinates.split(",").map(Number)); }
function halogen_pixi_clear_hit_area(object) { object.hitArea = null; }
function halogen_pixi_enable_stage_events(holder) { holder.app.stage.eventMode = "static"; holder.app.stage.hitArea = holder.app.screen; }
function halogen_pixi_on_pointer_down(holder, callback) { holder.app.stage.on("pointerdown", callback); }
function halogen_pixi_on_pointer_move(holder, callback) { holder.app.stage.on("globalpointermove", callback); }
function halogen_pixi_on_pointer_end(holder, callback) { holder.app.stage.on("pointerup", callback); holder.app.stage.on("pointerupoutside", callback); holder.app.stage.on("pointercancel", callback); }
function halogen_pixi_on_wheel(canvas, callback) { canvas.addEventListener("wheel", callback, { passive: false }); }
function halogen_pixi_remove_wheel(canvas, callback) { canvas.removeEventListener("wheel", callback); }
// What a handler reads of an event, copied as the event is dispatched: Pixi
// hands one event object on from event to event, so a read made later, by a
// thunk or after the handler has blocked, would read another event. A
// callback called with no event (a timeout, a promise) gets what it was given.
function halogen_pixi_snapshot(event) {
  if (!event || typeof event !== "object") return event;
  var global = event.global ? { x: event.global.x, y: event.global.y } : null;
  var target = event.currentTarget;
  var local = null;
  if (global && target && event.getLocalPosition) {
    var point = event.getLocalPosition(target);
    local = { x: point.x, y: point.y };
  }
  return {
    pointerId: event.pointerId,
    global: global,
    button: event.button,
    currentTarget: target,
    clientX: event.clientX,
    clientY: event.clientY,
    deltaY: event.deltaY,
    preventDefault: function () { event.preventDefault(); },
    getLocalPosition: function (object) { return object === target && local ? local : object.toLocal(global); }
  };
}
function halogen_pixi_pointer_id(event) { return event.pointerId; }
function halogen_pixi_global_x(event) { return event.global.x; }
function halogen_pixi_global_y(event) { return event.global.y; }
function halogen_pixi_local_x(object, event) { return event.getLocalPosition(object).x; }
function halogen_pixi_local_y(object, event) { return event.getLocalPosition(object).y; }
function halogen_pixi_current_target(event) { return event.currentTarget; }
function halogen_pixi_event_button(event) { return event.button; }
function halogen_pixi_prevent_default(event) { event.preventDefault(); }
function halogen_pixi_client_x(event) { return event.clientX; }
function halogen_pixi_client_y(event) { return event.clientY; }
function halogen_pixi_delta_y(event) { return event.deltaY; }
function halogen_pixi_canvas_left(canvas) { return canvas.getBoundingClientRect().left; }
function halogen_pixi_canvas_top(canvas) { return canvas.getBoundingClientRect().top; }
function halogen_pixi_canvas_width(canvas) { return canvas.getBoundingClientRect().width; }
function halogen_pixi_canvas_height(canvas) { return canvas.getBoundingClientRect().height; }
function halogen_pixi_screen_width(holder) { return holder.app.screen.width; }
function halogen_pixi_screen_height(holder) { return holder.app.screen.height; }
function halogen_pixi_schedule_timeout(callback, milliseconds) { return setTimeout(callback, milliseconds); }
function halogen_pixi_cancel_timeout(timer) { clearTimeout(timer); }
