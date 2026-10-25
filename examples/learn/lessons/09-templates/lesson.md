title: HTML and CSS templates
layers: th
args: -package haskell-halogen-th

The `HH` functions of the earlier lessons are exact, but they do not look like HTML. `haskell-halogen-th` lets you write markup as markup: `hh`, from `Halogen.TH`, is a quasi-quoter (it needs `QuasiQuotes`) that reads HTML with Haskell inside and turns it, while the module compiles, into the `HH` calls you would have written. The types are those calls' types, so a handler of the wrong type, or an attribute an element does not have, is a type error at the quote.

## Haskell inside the HTML

`{{ ... }}` holds a Haskell expression, read by GHC's own parser with the module's extensions (`OverloadedStrings`, `OverloadedRecordDot`). Where it stands decides what it must be:

- among an element's children, what to show: `Hello, {{ st.name }}`;
- as an attribute's value, what the property takes: `title={{ "Written for " <> st.name }}` is a `Text`, `disabled={{ st.likes >= 3 }}` a `Bool`;
- as a handler, a function from the event to an action: `onClick={{ \_ -> Like }}` is lesson 2's `onClick`.

`{% if ... %} ... {% end %}` decides whether something is there at all, even inside an attribute: `class="likes {% if st.likes > 0 %}liked{% end %}"`.

Whitespace follows JSX's rule: a line break next to a tag, with the spaces around it, is nothing, while spaces within a line are text, as in `{{ ... }} likes`. On a quote of one line the spaces around the element would be text too, and `hh` makes exactly one element: so the element goes on lines of its own.

The first compile here takes a while, as the compiler loads `ghc-lib-parser`, the copy of GHC's parser that reads the Haskell in a quote. GHC's JavaScript backend cannot build that library, so `haskell-halogen-th` is not for that backend; this page's compiler is the WebAssembly one.

## CSS

`clay` does the same for CSS: rules as written, blocks nested in blocks, `{{ ... }}` for values, and a value of Clay's `Css` type as the result. A `Css` among an element's children becomes a `<style>` element.

## Exercise

1. Show `st.items` in an `<ol class="items">` holding `{% for item in st.items %}`, `<li>{{ item }}</li>` and `{% end %}`, each on a line of its own. The `for` is a list comprehension written in the quote: its body is repeated for each item.
2. Write `style :: Css` with `[clay| ... |]` (import `clay` beside `hh`, and `Clay (Css)`): a `.card` block holding `.liked { color: {{ accent }}; }` and rules for `.items`, with `accent = C.rgb 0xc0 0x30 0x60` (import `Clay qualified as C`).
3. Put `{{ style }}` first in the card. Click Like, and the count turns red.
