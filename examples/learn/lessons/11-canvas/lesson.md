title: A canvas scene
layers: pixi
args: -package haskell-halogen-pixi

A component can draw on a canvas the way it renders HTML. `Halogen.Canvas.Elements` (`CE`) and `Halogen.Canvas.Properties` (`CP`), in `haskell-halogen-core`, are a second set of elements and properties: `CE.rectangle_`, `CE.circle_`, `CE.keyedGroup`, `CP.onClick`. What they build is the same `VDom` tree that `HH.div` builds, and it is reconciled by the same machinery. Only the last step differs: `haskell-halogen-pixi` makes each node a PixiJS display object instead of a DOM element.

## The scene

`scene` is a function from the state to a `Pixi.View`: a camera, how the reader may move it (`Pixi.defaultInteraction` lets you drag to pan and scroll to zoom), and the nodes. `Pixi.defaultCamera` puts the origin at the canvas's centre, and shapes are placed by their centre too: `CE.rectangle_ (Pixi.Point 0 0) (Pixi.Point 320 200) ...` is the background, filling the canvas, and the ball is a circle at `st.x`.

`Pixi.component` is an ordinary child component, in the slot `"canvas"`, whose input is the scene. Every render hands it the scene for the new state, and the reconciler compares it with the last one: when the ball moves, only the circle's shape has changed, so the circle is drawn again where it is now, and nothing else is touched. Nodes in a `CE.keyedGroup` carry keys, here the pads' numbers, so that a pad keeps its display object even if the list is reordered.

A handler on a node, `CP.onClick (\_ -> Just i)`, makes the component raise `Pixi.Fired i`, which arrives in the parent as `Canvas (Pixi.Fired i)`. Click a pad, and the ball jumps to it. The component also raises when the camera moves, which this one ignores.

PixiJS is not in the layer: the library imports it from a CDN the first time a canvas mounts, so the scene appears a moment after the compile.

## Exercise

Make the ball glide to the pad instead of jumping:

1. a click sets only `target`;
2. an `Initialize` action, named in `H.initialize`, subscribes to the screen's frames, as lesson 5 subscribed to a clock: `void (H.subscribe (Frame <$ Window.animationFrame))`, with `Halogen.Query.Event.Window qualified as Window`;
3. each `Frame` moves `x` six pixels towards `target`, and stops there. Read the state with `get`, and `put` a new one only `when (st.x /= st.target)`: a frame that changes nothing then renders nothing.
