// The code editor: CodeMirror 6 with its legacy Haskell mode, line numbers,
// GHC's diagnostics as lint markers, the types of the module's top-level
// bindings as tooltips over their names, Ctrl/Cmd+Enter to compile and Tab
// for two spaces.
//
// Tab is the editor's, so it does not move the focus on, as it does
// everywhere else: CodeMirror's own way out is kept (its tab-focus mode, see
// https://codemirror.net/examples/tab/), and said in the editor's
// description for a screen reader: Escape, then Tab or Shift+Tab within two
// seconds, moves the focus out of the editor, and Ctrl+M (Shift+Alt+M on a
// Mac, defaultKeymap's toggleTabFocusMode) makes Tab move it until pressed
// again. The Halogen app owns the element the editor lives in (it renders
// it empty and never patches inside it) and talks to the editor only through
// these functions; the editor tells the app what happens through one callback,
// with a JSON string: {"type":"change","text":...} or {"type":"compile"}.
import { Compartment, EditorState, StateEffect, StateField } from "@codemirror/state";
import {
  EditorView,
  drawSelection,
  highlightActiveLine,
  highlightActiveLineGutter,
  hoverTooltip,
  keymap,
  lineNumbers,
} from "@codemirror/view";
import { defaultKeymap, history, historyKeymap, indentLess } from "@codemirror/commands";
import { StreamLanguage, bracketMatching, defaultHighlightStyle, indentUnit, syntaxHighlighting } from "@codemirror/language";
import { haskell } from "@codemirror/legacy-modes/mode/haskell";
import { lintGutter, setDiagnostics } from "@codemirror/lint";

const theme = EditorView.theme({
  "&": { height: "100%", fontSize: "13px" },
  ".cm-scroller": { fontFamily: "var(--mono)", lineHeight: "1.5" },
  ".cm-content": { caretColor: "var(--fg)" },
  ".cm-gutters": { background: "var(--panel)", color: "var(--muted)", borderRight: "1px solid var(--border)" },
  ".cm-activeLine": { background: "var(--active-line)" },
  ".cm-activeLineGutter": { background: "var(--active-line)" },
  "&.cm-focused": { outline: "none" },
  ".cm-tooltip": { background: "var(--panel)", color: "var(--fg)", border: "1px solid var(--border)" },
  ".cm-learn-type": { fontFamily: "var(--mono)", padding: "2px 6px", whiteSpace: "pre-wrap", maxWidth: "70ch" },
});

// The types the app last gave (showTypes), by name: what the tooltips say.
const setTypes = StateEffect.define();
const types = StateField.define({
  create: () => new Map(),
  update(value, transaction) {
    for (const effect of transaction.effects) if (effect.is(setTypes)) value = effect.value;
    return value;
  },
});

// A Haskell name's characters: what the word under the pointer is made of.
const nameChar = /[\p{L}\p{N}_']/u;

// Over a name that is one of the bindings', its type. The name is found by
// its characters rather than by CodeMirror's words, which end at a prime
// (x'). A name right after a dot is a qualified one (H.component) or a
// record's field (st.count), neither of them the module's own binding, and
// shows nothing; a local binding that has the name of a top-level one shows
// the top-level one's type, which the tooltip cannot tell from it.
const typeTooltip = hoverTooltip((view, pos) => {
  const known = view.state.field(types);
  if (known.size === 0) return null;
  const line = view.state.doc.lineAt(pos);
  const text = line.text;
  let from = pos - line.from;
  let to = from;
  while (from > 0 && nameChar.test(text[from - 1])) from--;
  while (to < text.length && nameChar.test(text[to])) to++;
  if (from === to || text[from - 1] === ".") return null;
  const binding = known.get(text.slice(from, to));
  if (!binding) return null;
  return {
    pos: line.from + from,
    end: line.from + to,
    above: true,
    create() {
      const dom = document.createElement("div");
      dom.className = "cm-learn-type";
      dom.textContent = `${binding.name} :: ${binding.type}`;
      return { dom };
    },
  };
});

export function create(parent, text, readOnly, onEvent) {
  const editable = new Compartment();
  const send = (event) => {
    try {
      onEvent(JSON.stringify(event));
    } catch (error) {
      console.error("learn: editor event", error);
    }
  };
  const view = new EditorView({
    parent,
    state: EditorState.create({
      doc: text,
      extensions: [
        lineNumbers(),
        highlightActiveLineGutter(),
        highlightActiveLine(),
        drawSelection(),
        history(),
        bracketMatching(),
        indentUnit.of("  "),
        StreamLanguage.define(haskell),
        syntaxHighlighting(defaultHighlightStyle, { fallback: true }),
        lintGutter(),
        types,
        typeTooltip,
        theme,
        editable.of([EditorState.readOnly.of(readOnly), EditorView.editable.of(!readOnly)]),
        keymap.of([
          {
            key: "Mod-Enter",
            run: () => {
              send({ type: "compile" });
              return true;
            },
          },
          {
            key: "Tab",
            run: (v) => {
              if (v.state.readOnly) return false;
              v.dispatch(v.state.replaceSelection("  "), { scrollIntoView: true, userEvent: "input" });
              return true;
            },
            shift: indentLess,
          },
          ...defaultKeymap,
          ...historyKeymap,
        ]),
        EditorView.updateListener.of((update) => {
          if (update.docChanged) send({ type: "change", text: update.state.doc.toString() });
        }),
        EditorView.contentAttributes.of({
          "aria-label": "Lesson.hs",
          "aria-description": "Tab indents. Press Escape, then Tab, to leave the editor.",
        }),
      ],
    }),
  });
  view.learnEditable = editable;
  return view;
}

export function text(view) {
  return view.state.doc.toString();
}

// Replace the whole text; the app hears of it as a change, as of typing.
export function setText(view, text) {
  view.dispatch({ changes: { from: 0, to: view.state.doc.length, insert: text } });
}

export function setReadOnly(view, readOnly) {
  view.dispatch({
    effects: view.learnEditable.reconfigure([EditorState.readOnly.of(readOnly), EditorView.editable.of(!readOnly)]),
  });
}

// Diagnostics as [{line, column, endLine, endColumn, severity, message}],
// lines and columns from 1 as GHC counts them, the end exclusive; with no end,
// the marker runs to the end of the word.
export function showDiagnostics(view, diagnostics) {
  const doc = view.state.doc;
  const position = (line, column) => {
    const l = doc.line(Math.min(Math.max(line, 1), doc.lines));
    return Math.min(l.from + Math.max(column - 1, 0), l.to);
  };
  const marks = diagnostics.map((d) => {
    const from = position(d.line, d.column);
    let to = d.endLine ? position(d.endLine, d.endColumn) : from;
    if (to <= from) {
      const word = view.state.wordAt(from);
      to = word && word.to > from ? word.to : Math.min(from + 1, doc.lineAt(from).to);
    }
    return { from, to: Math.max(to, from), severity: d.severity === "warning" ? "warning" : "error", message: d.message };
  });
  view.dispatch(setDiagnostics(view.state, marks));
}

// The module's top-level bindings, as [{line, column, name, type}], in
// place of those given before: the tooltips' types.
export function showTypes(view, bindings) {
  view.dispatch({ effects: setTypes.of(new Map(bindings.map((b) => [b.name, b]))) });
}

export function goto(view, line, column) {
  const doc = view.state.doc;
  const l = doc.line(Math.min(Math.max(line, 1), doc.lines));
  const at = Math.min(l.from + Math.max(column - 1, 0), l.to);
  view.dispatch({ selection: { anchor: at }, scrollIntoView: true });
  view.focus();
}

export function destroy(view) {
  view.destroy();
}
