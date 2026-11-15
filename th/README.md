# haskell-halogen-th

Write HTML and CSS templates with embedded Haskell for [haskell-halogen](https://github.com/Swordlash/haskell-halogen) and [Clay](https://hackage.haskell.org/package/clay). The `hh` quasi-quoter produces `HTML w i`, including embedded SVG. Use `svg` for standalone SVG and `clay` for `Css`.

```haskell
{-# LANGUAGE QuasiQuotes #-}
import Halogen.TH (hh, clay)

render :: State -> H.ComponentHTML Action () m
render st = [hh|
  <div class="counter {{ if st.on then "on" else "" }}">
    <button onClick={{ const Decrement }}>−</button>
    <span class="count">{{ show st.count }}</span>
    <button onClick={{ const Increment }}>+</button>
    {% if st.count > 9 %}<em>That is a lot.</em>{% end %}
    <ul>
      {% for (i, item) in zip [1 :: Int ..] st.items %}
        <li>{{ show i }}. {{ item }}</li>
      {% end %}
    </ul>
  </div>
|]

style :: Css
style = [clay|
  .counter {
    display: flex; gap: 8px; color: {{ ink }};
    &.on { font-weight: bold; }
    > .count { min-width: 2em; }
    @media (max-width: 600px) { flex-direction: column; }
  }
|]
```

Quotes generate the same calls you would write by hand. `<div class="x">` becomes `HH.div [HP.class_ "x"]`, `onClick={{ … }}` uses `HE.onClick`, and `.a { color: red }` uses `".a" ? ("color" -: "red")`. Those functions' types still apply, so unsupported attributes and incorrect handlers produce type errors.

## The Haskell inside

Put Haskell expressions inside `{{ expression }}`. The quoter uses the compiling GHC's own parser and the surrounding module's extensions, including `OverloadedRecordDot`, `LambdaCase` and `BlockArguments`. Supported compiler versions are GHC 9.10 through 9.14.

Using GHC's parser avoids the extension gaps in `haskell-src-meta` and `haskell-src-exts`. The package also translates forms that `ghc-hs-meta` does not cover, including `case`, `do`, `let`, comprehensions and record updates, and supports GHC beyond that library's 9.12 limit.

Parse error messages include the original file, line and column, such as `hh: Mod.hs:12: column 31: …`. By default, GHC attaches diagnostics to the quote itself. Enable `haskell-halogen-th-plugin` with `-fplugin=Halogen.TH.Plugin` to place parser and type-checker errors directly on the embedded expression, including in a language server.

Nested quasi-quotes and splices are unsupported. In particular, `|]` would close the outer quote.

The expected type depends on where the expression appears:

| Where | What |
| --- | --- |
| among the children of an element | an `HTML w i`, a list of them, a `Text`, a `Css` (embedded as a `<style>` element), or a `Maybe` of one of those (the `Children` class) |
| the value of an attribute | what the property takes: `Text` for `id`, `Bool` for `disabled`, an `Int` for `colspan`, an `InputType` for `type`, a handler for `onClick`, a `Css` or a `Text` for `style`, a `RefLabel` for `ref` |
| part of an attribute's text, `class="a {{ b }}"` | a `Text` |
| among the attributes, `<div {{ p }}>` | an `IProp r i`, a list of them, or a `Maybe` (the `Props` class): a router link is `<a {{ Router.linkProps router Followed route }}>` (`haskell-halogen-router`) |
| a value in CSS, `color: {{ c }}` | anything Clay renders (its `Val` class): a `Color`, a `Size`, a `Text`, … |
| part of a value, `border: 1px solid {{ c }}` | the same |
| a selector, `{{ s }} { … }` | a `Selector`; in text, `.size-{{ n }} { … }`, a `Text` |
| among the rules, `.a { {{ mixin }} color: red }` | a `Css` |

Template statements work in child lists, attribute lists, attribute text and CSS rule blocks. For example, a class can be conditional: `class="item {% if done %}done{% end %}"`. Supported statements are:

- `{% if c %} … {% elif c %} … {% else %} … {% end %}`
- `{% for pattern in list %} … {% end %}`, with any pattern: `{% for (i, x) in zip [1 ..] xs %}`
- `{% let pattern = e %}`, which binds for the rest of its sequence
- `{# a comment #}`

Close each statement within the sequence where it started. For example, a loop over an element's children must end inside that element. `{% end %}`, `{% endif %}` and `{% endfor %}` are interchangeable.

## HTML

`hh` produces one element. Use `hhs` to produce `[HTML w i]`, including when a template starts with a statement.

- Known tags call `Halogen.HTML.Elements` functions. Unknown tags such as `<my-widget>` use `HH.element`. Void tags such as `<br>` and `<input>` do not require closing tags, though they are accepted. Other tags must close in the correct order; errors report their line.
- Attributes map case-insensitively to `Halogen.HTML.Properties`; handlers map to `Halogen.HTML.Events`. Thus `colspan` and `colSpan`, or `onclick` and `onClick`, are equivalent. Unknown names such as `data-x`, `aria-label` and `role` use `HP.attr`. Boolean attributes can stand alone or take a `Bool`; numeric values can be written directly. Typed properties such as `type`, `value`, `step` and `width` accept Haskell values. Writing those values as text sets an attribute instead.
- `style="…"` uses `HP.styleText`. `style={{ x }}` accepts `Css` or `Text` through `Styled`. A `Css` value among children becomes a `<style>` element.
- Whitespace follows JSX rules. Single-line text keeps its spaces. For multiline text, leading whitespace is trimmed except on the first line, trailing whitespace except on the last, empty lines are removed, and remaining lines join with one space. Tags, expressions and statements separate text segments, so indentation around them does not create spaces. Put required spaces on the same line, as in `Hello, {{ name }}`, or insert `{{ " " :: Text }}`. Only ASCII whitespace is trimmed; `&nbsp;` remains. Entities are decoded and HTML comments are removed.

## SVG

Inside an `hh` quote, `<svg>` switches its contents to the SVG namespace. Use `svg` for one standalone SVG element or `svgs` for a list of SVG elements.

```haskell
board :: [(Double, Double)] -> [PathCommand] -> HTML w Action
board towns road = [hh|
  <svg viewBox={{ (0, 0, 640, 480) }} class="board">
    <path d={{ road }} stroke-width="6" class="road"/>
    {% for (x, y) in towns %}<circle cx={{ x }} cy={{ y }} r="8" onClick={{ const (Pick x y) }}/>{% end %}
  </svg>
|]
```

- Known SVG tags call `Halogen.Svg.Elements` functions in the SVG namespace. Unknown tags such as `<stop>` and `<linearGradient>` use `Halogen.Svg.Elements.element`. Their attributes are text except for class, style, refs and handlers. Names preserve SVG casing, including `foreignObject` and `viewBox`. Inside `<foreignObject>`, content switches back to HTML.
- Attributes accept either SVG names, such as `stroke-width`, or Haskell names, such as `strokeWidth`. Numeric values accept literals or `Double`. `d` takes `[PathCommand]`, `fill` and `stroke` take `Color`, `transform` takes `[Transform]`, and `viewBox` takes four `Double`s. Text values set attributes directly. `class` uses the attribute because SVG's `className` is not writable. Event handlers are the same as HTML handlers.
- SVG tags can close themselves, as in `<path d={{ p }}/>` .

## CSS

`clay` produces `Css` from complete rules or a group of declarations used as a mixin. The output preserves the CSS you wrote.

- Declarations use Clay's `key` with textual property names and values. Typed property functions are not involved, so vendor prefixes are written explicitly and `!important`, `calc()`, `var()` and custom properties pass through.
- Selectors use `element` and nest with `?`. Selector lists are combined. `&:hover`, `&.open`, `&[attr]` and `&::before` refine the parent through Clay's `&`; `> .child` uses `<?`, and `& .deep` selects descendants.
- Media queries map to `query`, `queryNot` or `queryOnly` and can appear inside rules. `@keyframes`, `@font-face` and `@import` use Clay's corresponding operations. Other at-rules are rejected because Clay cannot represent them.

## In this repository

The package lives in `th/`. Run its native tests with `cabal test th-test --builddir=dist-newstyle/native`. Each test compares a quote with equivalent HTML or CSS built by hand.

WebAssembly quotes require `shared: True` in the Cabal project, as jsbits does. The wasm Template Haskell interpreter loads the cross-compiler's `libHSghc` to parse expressions; you do not need to build another copy.

The JavaScript backend is unsupported because expanding quotes requires too much memory. Its Template Haskell runner links all splice dependencies into JavaScript, including 1.7 GB of `ghc` library objects. The example with five quotes took two minutes and 12.7 GB to compile; the 45-quote test module was killed after exceeding 17 GB. The package itself can compile there, but using its quotes is impractical.
