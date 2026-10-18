# haskell-halogen-th

HTML and CSS as they are written, with Haskell inside, for
[haskell-halogen](https://github.com/Swordlash/haskell-halogen) and
[Clay](https://hackage.haskell.org/package/clay): two quasi-quoters, `hh` for
a Halogen `HTML w i` and `clay` for a `Css`.

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

The result is what the same markup written with `Halogen.HTML` and Clay by
hand would be: `<div class="x">` is `HH.div [HP.class_ "x"]`, `onClick={{ … }}`
is `HE.onClick`, `.a { color: red }` is `".a" ? ("color" -: "red")`. The
types are those functions' types, so an attribute on the wrong element or a
handler of the wrong type is a type error, at the line of the quote.

## The Haskell inside

`{{ expression }}` is any Haskell expression. It is parsed by GHC's own
parser (through `ghc-lib-parser`), with the extensions of the module the
quote is in, so `OverloadedRecordDot`, `LambdaCase`, `BlockArguments` and the
rest work as they do in that module. A parse error names the file, the line
and the column (in the file: `hh: Mod.hs:12: column 31: …`). GHC still
reports it at the quote; the GHC plugin of `haskell-halogen-th-plugin`
(`-fplugin=Halogen.TH.Plugin`) reports it where it is, in the compiler and
in a language server alike. Two things cannot be inside a quote: another quasi-quote
(`|]` would end the outer one) and a splice.

What a value may be depends on where it stands:

| Where | What |
| --- | --- |
| among the children of an element | an `HTML w i`, a list of them, a `Text`, a `Css` (embedded as a `<style>` element), or a `Maybe` of one of those (the `Children` class) |
| the value of an attribute | what the property takes: `Text` for `id`, `Bool` for `disabled`, an `Int` for `colspan`, an `InputType` for `type`, a handler for `onClick`, a `Css` or a `Text` for `style`, a `RefLabel` for `ref` |
| part of an attribute's text, `class="a {{ b }}"` | a `Text` |
| among the attributes, `<div {{ p }}>` | an `IProp r i`, a list of them, or a `Maybe` (the `Props` class) |
| a value in CSS, `color: {{ c }}` | anything Clay renders (its `Val` class): a `Color`, a `Size`, a `Text`, … |
| part of a value, `border: 1px solid {{ c }}` | the same |
| a selector, `{{ s }} { … }` | a `Selector`; in text, `.size-{{ n }} { … }`, a `Text` |
| among the rules, `.a { {{ mixin }} color: red }` | a `Css` |

Statements shape the sequence they stand in: the children of an element,
its attributes, the text of an attribute (`class="item {% if done %}done{% end %}"`)
or the rules of a block alike:

- `{% if c %} … {% elif c %} … {% else %} … {% end %}`
- `{% for pattern in list %} … {% end %}`, with any pattern: `{% for (i, x) in zip [1 ..] xs %}`
- `{% let pattern = e %}`, which binds for the rest of its sequence
- `{# a comment #}`

A statement is closed in the sequence it opened: a `{% for %}` among the
children of one element ends among them. `{% end %}`, `{% endif %}` and
`{% endfor %}` are the same.

## HTML

`hh` makes one element. For a list of nodes, or a statement at the top,
`hhs` makes an `[HTML w i]`.

- An element is its function from `Halogen.HTML.Elements`, with the
  children it takes; a name without one (`<my-widget>`) is `HH.element`.
  Void elements (`<br>`, `<input>`, …) take no end tag, and tolerate one.
  Every other tag must be closed, in order, or the quote fails with the line.
- An attribute is its function from `Halogen.HTML.Properties`, by name in
  any case (`colspan`, `colSpan`), a handler one from `Halogen.HTML.Events`
  (`onClick`, `onclick`). A name without one (`data-x`, `aria-label`, `role`)
  is `HP.attr`. A boolean attribute stands alone (`<input disabled>`) or
  takes a `Bool`; a number is written as a number. A property of its own type
  (`type`, `value`, `step`, `width`) takes its Haskell value, or, written as
  text, is set as the attribute.
- `style="…"` is `HP.styleText`; `style={{ x }}` takes Clay's `Css` or a `Text` (the `Styled` class). A `Css` among the children, `{{ style }}`, is a `<style>` element with it.
- Text follows JSX: a text with a newline in it has every line trimmed and
  the lines joined by one space, so indentation never becomes a text node;
  a text on one line is kept as it is. Entities are decoded, `<!-- -->` is
  dropped.

## CSS

`clay` makes a `Css`: rules, or declarations alone (a mixin). The style
rendered is the one written.

- A declaration is `key` over the property and the value as text: Clay's
  typed functions are not used, so a vendor prefix is written, not added,
  and `!important`, `calc()`, `var()` and custom properties pass through.
- A selector is `element` over its text, nested with `?`; a list is
  combined. `&:hover`, `&.open`, `&[attr]`, `&::before` refine the enclosing
  selector (Clay's `&`); `> .child` is a child (`<?`); `& .deep` a
  descendant.
- `@media [not | only] [type] [and (feature[: value])]…` is `query`,
  `queryNot` or `queryOnly`, nested in a rule or not. `@keyframes name { from
  … 50% … to … }`, `@font-face { … }` and `@import "file";` are Clay's.
  Other at-rules are refused, since Clay has no rule for them.

## In this repository

The package is `th/`. Its tests run natively (`cabal test th-test
--builddir=dist-newstyle/native`); each one compares a quote with the same
markup or style written by hand.

Building a quote on the WebAssembly backend needs Template Haskell there,
that is `shared: True` in the wasm project (as the jsbits already do); the
first build compiles `ghc-lib-parser`, which takes a while and is then in the
store. That build runs `alex` and `happy`, and they have to be native
programs on `PATH` (`cabal install alex happy --constraint='happy <2.2'` with the host GHC): cabal
builds them for wasm too, as `alex.wasm` and `happy.wasm`, which it then
cannot run, and falls back to `PATH`.
