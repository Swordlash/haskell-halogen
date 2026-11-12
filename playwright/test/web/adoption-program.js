// Plays a Halogen program adopting adoption.html, in the way the page's
// ?mode= says: "clean" takes the page as it is (it listens to the button,
// sets each text to what it already is, and takes away the space between
// the tags and the comment, which no render has), and each other mode
// repairs one thing as well. Then it answers the probe, started and idle.
const mode = new URLSearchParams(location.search).get("mode") ?? "clean";
const app = document.getElementById("app");
const counter = app.querySelector(".counter");
const count = app.querySelector(".count");

const tidy = (element) => {
  for (const node of [...element.childNodes]) {
    if (node.nodeType === Node.COMMENT_NODE || (node.nodeType === Node.TEXT_NODE && node.data.trim() === "")) node.remove();
    else if (node.nodeType === Node.ELEMENT_NODE) tidy(node);
    else if (node.nodeType === Node.TEXT_NODE) node.data = String(node.data);
  }
};
tidy(app);
app.querySelector("button").addEventListener("click", () => {
  count.textContent = String(Number(count.textContent) + 1);
});

switch (mode) {
  case "replace": {
    // An equal element, but another one: what a render that did not
    // recognise the server's element does.
    const p = document.createElement("p");
    p.className = "count";
    p.textContent = "0";
    count.replaceWith(p);
    break;
  }
  case "text":
    count.firstChild.data = "1";
    break;
  case "add": {
    const note = document.createElement("p");
    note.className = "note";
    note.textContent = "added";
    counter.append(note);
    break;
  }
  case "remove":
    app.querySelector("h1").remove();
    break;
  case "attribute":
    count.setAttribute("data-count", "0");
    break;
  case "island":
    // What a library fills in, and an attribute the page's own script
    // writes, whatever the server sent: neither is the program's repair.
    app.querySelector(".island").innerHTML = "<b>typeset again</b>";
    counter.setAttribute("data-stuck", "");
    break;
  case "outside":
    document.getElementById("outside").textContent = "changed by the program, outside its element";
    break;
}

if (globalThis.__halogenProbe) {
  __halogenProbe.activity = async () => ({ trees: 1, running: 0, waiting: 0 });
}
