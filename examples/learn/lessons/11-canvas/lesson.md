title: A canvas scene
layers: pixi
args: -package haskell-halogen-pixi

Halogen can describe canvas scenes using the same approach as HTML. `Halogen.Canvas.Elements` (`CE`) and `Halogen.Canvas.Properties` (`CP`) provide shapes, groups and handlers such as `CE.rectangle_`, `CE.circle_`, `CE.keyedGroup` and `CP.onClick`.

These functions build a `VDom` tree, reconciled by the same code as HTML. The `haskell-halogen-pixi` backend turns its nodes into PixiJS display objects.

## The scene

`scene` converts state into a `Pixi.View`, containing the camera, interaction settings and scene nodes. `Pixi.defaultInteraction` enables drag-to-pan and scroll-to-zoom. `Pixi.defaultCamera` places the origin at the canvas centre.

Shapes are positioned by their centres. `CE.rectangle_ (Pixi.Point 0 0) (Pixi.Point 320 200) ...` creates the background, and the circle's horizontal position comes from `st.x`.

The child `Pixi.component`, mounted in the `"canvas"` slot, receives the scene as input. Updates patch only changed properties. Moving the ball redraws its shape without rebuilding the rest. Pads use their numbers as keys in `CE.keyedGroup`, preserving their display objects across reordering.

`CP.onClick (\_ -> Just i)` makes the canvas emit `Pixi.Fired i`, received by the parent as `Canvas (Pixi.Fired i)`. Clicking a pad moves the ball to it. Camera changes also produce outputs, which this example ignores.

PixiJS loads from a CDN when the first canvas mounts, so the scene may appear shortly after compilation finishes.

## Exercise

Make the ball glide instead of jumping:

1. Change the click handler to update only `target`.
2. Add `startFrames` to `H.onInitialize`. Subscribe with `void (H.subscribe (Frame <$ Window.animationFrame))`, importing `Halogen.Query.Event.Window qualified as Window`.
3. On each `Frame`, move `x` six pixels toward `target`, without overshooting. Read state with `get` and call `put` only `when (st.x /= st.target)`. Once the ball arrives, unchanged frames no longer trigger renders.
