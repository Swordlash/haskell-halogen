title: HTML and CSS templates
layers: th
args: -package haskell-halogen-th

`haskell-halogen-th` lets you write HTML templates with embedded Haskell. Import `hh` from `Halogen.TH` and enable `QuasiQuotes`. At compile time, the quoter converts the markup into the same `HH` calls used in earlier lessons. Their types still apply, so invalid properties and incorrect handlers produce type errors.

## Haskell inside the HTML

Put Haskell expressions inside `{{ ... }}`. GHC parses them with the module's extensions, including `OverloadedStrings` and `OverloadedRecordDot`. Their expected type depends on their position:

- Among children, they provide content: `Hello, {{ st.name }}`.
- As attribute values, they match the property type: `title={{ "Written for " <> st.name }}` uses `Text`, while `disabled={{ st.likes >= 3 }}` uses `Bool`.
- As event handlers, they convert an event into an action: `onClick={{ \_ -> Like }}`.

Use `{% if ... %} ... {% end %}` for conditional content. It also works inside attributes: `class="likes {% if st.likes > 0 %}liked{% end %}"`.

Whitespace follows JSX rules. Indentation and line breaks beside tags disappear, but spaces within a line remain, as in `{{ ... }} likes`. A single-line quote also preserves spaces around its element. Since `hh` must return exactly one element, put the element on its own lines or omit those surrounding spaces.

The quoter uses GHC's own parser. It supports the WebAssembly compiler running here. The JavaScript backend is unsupported because linking that parser for Template Haskell requires too much memory.

## CSS

`clay` converts CSS templates into Clay's `Css` type. Write rules and nested blocks normally, using `{{ ... }}` for Haskell values. Inserting a `Css` value among HTML children creates a `<style>` element.

## Exercise

1. Display `st.items` in `<ol class="items">`. Inside it, write `{% for item in st.items %}`, `<li>{{ item }}</li>` and `{% end %}` on separate lines. The loop repeats its body for each item.
2. Import `clay` alongside `hh`, `Clay (Css)` and `Clay qualified as C`. Define `style :: Css` with a `[clay| ... |]` quote. Add a `.card` block containing `.liked { color: {{ accent }}; }` and styles for `.items`. Define `accent = C.rgb 0xc0 0x30 0x60`.
3. Insert `{{ style }}` first inside the card. Compile and click Like: the count turns red.
