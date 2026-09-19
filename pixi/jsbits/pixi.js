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
function halogen_pixi_add_child(parent, child) { parent.addChild(child); }
function halogen_pixi_remove_child(parent, child) { parent.removeChild(child); }
function halogen_pixi_parent_of(object) { return object.parent ?? null; }
function halogen_pixi_clear_container(container) { container.removeChildren().forEach(function (child) { child.destroy({ children: true, texture: false, textureSource: false }); }); }
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
  var style = { fontFamily: family, fontSize: size, fill: color, align: align };
  object.style = style;
  holder.pixi.Assets.load({ src: source, data: { family: family } })
    .then(function () {
      // Restyle once the face is in, so the glyphs are not left in the fallback.
      if (!object.destroyed && object.__halogenFontRequest === request) object.style = style;
    })
    .catch(function (error) { console.error("Could not load PixiJS font", source, error); });
}
function halogen_pixi_set_texture(holder, object, asset) {
  object.__halogenAsset = asset;
  holder.pixi.Assets.load(asset)
    .then(function (texture) { if (!object.destroyed && object.__halogenAsset === asset) object.texture = texture; })
    .catch(function (error) { console.error("Could not load PixiJS texture", asset, error); });
}
function halogen_pixi_new_sprite(holder) { return new holder.pixi.Sprite(holder.pixi.Texture.EMPTY); }
function halogen_pixi_center_anchor(object) { object.anchor.set(0.5); }
function halogen_pixi_set_position(object, x, y) { object.position.set(x, y); }
function halogen_pixi_set_scale(object, x, y) { object.scale.set(x, y); }
function halogen_pixi_set_rotation(object, rotation) { object.rotation = rotation; }
function halogen_pixi_set_size(object, width, height) { object.width = width; object.height = height; }
function halogen_pixi_on_tap(object, callback) { object.eventMode = "static"; object.cursor = "pointer"; object.on("pointertap", callback); }
function halogen_pixi_on(object, eventType, callback) { object.on(eventType, callback); }
function halogen_pixi_off(object, eventType, callback) { object.off(eventType, callback); }
function halogen_pixi_set_event_mode(object, mode) { object.eventMode = mode; }
function halogen_pixi_set_cursor(object, cursor) { object.cursor = cursor; }
function halogen_pixi_set_rect_hit_area(holder, object, x, y, width, height) { object.hitArea = new holder.pixi.Rectangle(x, y, width, height); }
function halogen_pixi_set_circle_hit_area(holder, object, x, y, radius) { object.hitArea = new holder.pixi.Circle(x, y, radius); }
function halogen_pixi_clear_hit_area(object) { object.hitArea = null; }
function halogen_pixi_enable_stage_events(holder) { holder.app.stage.eventMode = "static"; holder.app.stage.hitArea = holder.app.screen; }
function halogen_pixi_on_pointer_down(holder, callback) { holder.app.stage.on("pointerdown", callback); }
function halogen_pixi_on_pointer_move(holder, callback) { holder.app.stage.on("globalpointermove", callback); }
function halogen_pixi_on_pointer_end(holder, callback) { holder.app.stage.on("pointerup", callback); holder.app.stage.on("pointerupoutside", callback); holder.app.stage.on("pointercancel", callback); }
function halogen_pixi_on_wheel(canvas, callback) { canvas.addEventListener("wheel", callback, { passive: false }); }
function halogen_pixi_remove_wheel(canvas, callback) { canvas.removeEventListener("wheel", callback); }
function halogen_pixi_pointer_id(event) { return event.pointerId; }
function halogen_pixi_global_x(event) { return event.global.x; }
function halogen_pixi_global_y(event) { return event.global.y; }
function halogen_pixi_local_x(object, event) { return event.getLocalPosition(object).x; }
function halogen_pixi_local_y(object, event) { return event.getLocalPosition(object).y; }
function halogen_pixi_current_target(event) { return event.currentTarget; }
function halogen_pixi_event_button(event) { return event.button; }
function halogen_pixi_stop_propagation(event) { event.stopPropagation(); }
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
