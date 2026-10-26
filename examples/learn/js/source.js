// Source mode: which line of the reader's code made an element of the
// compiled result. The playground marks every element Lesson builds with a
// data-learn-src="<line>:<column>" attribute (learn/Playground.hs, run with
// --learn-spans), so the page needs nothing from the program to tell: it reads
// the attribute of the element under the pointer.
//
// While it is on, the element under the pointer (the nearest one with a mark,
// within the result) is outlined with a box fixed over its bounding rectangle,
// as Halogen.DevTools outlines a component, labelled with where it was built,
// and the app hears {"type":"hover","line","column"}, or {"type":"leave"} when
// the pointer is over no marked element. A click on a marked element is the
// reader's way to go to its line, not the program's click: it is stopped in
// the capture phase, before the program's handlers see it, and the app hears
// {"type":"goto","line","column"}. The app also asks for the element a
// position of the editor's cursor made (outline), which is outlined while the
// pointer is over no element. Events reach the app as JSON strings, through
// one callback, as the editor's do.
//
// A mark is where the element was built in the code as it was compiled, and
// the reader may have edited it since: every position the app hears, and the
// label's, is where that code is in the editor now, and the cursor's position
// the app gives is the editor's now, both mapped through the editor's changes
// since the compile (editor.js's positions, given to start). An element
// whose code has been deleted since has no line: the label says so, the app
// hears "leave" for it, and a click on it goes nowhere (and is still not the
// program's).
//
// The box is placed again whenever the page scrolls (anywhere: the result's
// column scrolls on its own) or is resized, and at every pointer event, so it
// follows the element; it hides when the element leaves the page.

export function start(result, positions, onEvent) {
  const send = (event) => {
    try {
      onEvent(JSON.stringify(event));
    } catch (error) {
      console.error("learn: source event", error);
    }
  };
  const box = document.createElement("div");
  box.className = "learn-source-box";
  box.hidden = true;
  const label = document.createElement("span");
  label.className = "learn-source-label";
  box.append(label);
  document.body.append(box);
  result.classList.add("learn-sourcing");

  let hovered = null; // the marked element under the pointer
  let cursor = null; // the one the editor's cursor made, if any

  const marked = (target) => {
    const element = target instanceof Element ? target.closest("[data-learn-src]") : null;
    return element && result.contains(element) ? element : null;
  };
  // Where the element's code is in the editor now, or null.
  const now = (element) => positions.current(position(element));
  const shown = () => hovered ?? cursor;
  const place = () => {
    const element = shown();
    if (!element || !element.isConnected) {
      box.hidden = true;
      return;
    }
    const r = element.getBoundingClientRect();
    box.style.left = `${r.left}px`;
    box.style.top = `${r.top}px`;
    box.style.width = `${r.width}px`;
    box.style.height = `${r.height}px`;
    const at = now(element);
    label.textContent = at ? `Lesson.hs:${at.line}:${at.column}` : "Lesson.hs: deleted since the compile";
    box.hidden = false;
  };

  const onPointer = (event) => {
    const element = marked(event.target);
    if (element !== hovered) {
      hovered = element;
      const at = element && now(element);
      send(at ? { type: "hover", ...at } : { type: "leave" });
    }
    place();
  };
  const onLeave = () => {
    if (hovered) {
      hovered = null;
      send({ type: "leave" });
    }
    place();
  };
  const onClick = (event) => {
    const element = marked(event.target);
    if (!element) return;
    event.preventDefault();
    event.stopImmediatePropagation();
    const at = now(element);
    if (at) send({ type: "goto", ...at });
  };

  result.addEventListener("pointerover", onPointer);
  result.addEventListener("pointermove", onPointer);
  result.addEventListener("pointerleave", onLeave);
  result.addEventListener("click", onClick, true);
  // Any scroll, the result column's included, and capture: scroll does not
  // bubble.
  window.addEventListener("scroll", place, true);
  window.addEventListener("resize", place);

  return {
    // The element whose mark is this position of the editor, or else the
    // one on its line that starts last at or before its column, or none,
    // the position taken back to the compiled code first: none for code
    // written since.
    outline(line, column) {
      const then = positions.compiled({ line, column });
      let best = null;
      let bestColumn = 0;
      for (const element of then ? result.querySelectorAll("[data-learn-src]") : []) {
        const at = position(element);
        if (at.line !== then.line || at.column > then.column) continue;
        if (at.column === then.column) {
          best = element;
          break;
        }
        if (at.column > bestColumn) {
          best = element;
          bestColumn = at.column;
        }
      }
      cursor = best;
      place();
    },
    stop() {
      result.removeEventListener("pointerover", onPointer);
      result.removeEventListener("pointermove", onPointer);
      result.removeEventListener("pointerleave", onLeave);
      result.removeEventListener("click", onClick, true);
      window.removeEventListener("scroll", place, true);
      window.removeEventListener("resize", place);
      result.classList.remove("learn-sourcing");
      box.remove();
    },
  };
}

export function outline(handle, line, column) {
  handle.outline(line, column);
}

export function stop(handle) {
  handle.stop();
}

function position(element) {
  const [line, column] = element.dataset.learnSrc.split(":").map(Number);
  return { line, column };
}
