-- | A list to keep, written as HTML and CSS with Haskell in them: the
-- markup through @hh@, the stylesheet through @clay@.
module Example.Quotes (component) where

import Clay (Css)
import Clay qualified as C
import Data.Row (Empty)
import Data.Text qualified as T
import Halogen qualified as H
import Halogen.TH (clay, hh)
import Protolude hiding (State)
import UnliftIO (MonadUnliftIO)
import Web.Event.Event (Event, preventDefault)

data State = State
  { items :: [(Int, Text)]
  , draft :: Text
  , next :: Int
  , showDone :: Bool
  , done :: [Int]
  }

data Action
  = Draft Text
  | -- | The form's submit: the page must not reload.
    Add Event
  | Toggle Int
  | Remove Int
  | ShowDone Bool

component :: forall m. (MonadUnliftIO m) => H.Component H.VoidF () () m
component =
  H.mkComponent
    H.ComponentSpec
      { initialState = const (pure State {items = [(1, "Read the README"), (2, "Quote some HTML")], draft = "", next = 3, showDone = True, done = []})
      , render
      , eval = H.mkEval H.defaultEval {H.handleAction = handleAction}
      }
  where
    render :: State -> H.ComponentHTML Action Empty m
    render st =
      [hh|
      <section class="quotes">
        {{ style }}
        <h2>A list, quoted</h2>
        <p class="hint">
          The markup is an <code>[hh| … |&#93;</code> quote and the style a
          <code>[clay| … |&#93;</code> quote, with Haskell in <code>{{ "{{ }}" :: Text }}</code>.
        </p>
        <form onSubmit={{ Add }}>
          <input class="draft" placeholder="Something to do" value={{ st.draft }} onValueInput={{ Draft }}>
          <button type="submit" disabled={{ T.null (T.strip st.draft) }}>Add</button>
        </form>
        <label class="filter">
          <input type="checkbox" checked={{ st.showDone }} onChecked={{ ShowDone }}>
          Show what is done ({{ show (length st.done) :: Text }})
        </label>
        <ul class="items">
          {% for (i, item) in st.items %}
            {% let isDone = i `elem` st.done %}
            {% if st.showDone || not isDone %}
              <li class="item {% if isDone %}done{% end %}">
                <input type="checkbox" checked={{ isDone }} onChecked={{ const (Toggle i) }}>
                <span class="text">{{ item }}</span>
                <button class="remove" title="Remove" onClick={{ const (Remove i) }}>×</button>
              </li>
            {% end %}
          {% end %}
        </ul>
        {% if null st.items %}
          <p class="empty">Nothing left to do.</p>
        {% elif length st.items > 5 %}
          <p class="empty">{{ show (length st.items) :: Text }} things: that is a lot.</p>
        {% end %}
      </section>
    |]

    handleAction = \case
      Draft t -> modify $ \st -> st {draft = t}
      Add ev -> do
        H.liftEffect (preventDefault ev)
        modify $ \st ->
          let t = T.strip st.draft
           in if T.null t then st else st {items = st.items <> [(st.next, t)], next = st.next + 1, draft = ""}
      Toggle i -> modify $ \st -> st {done = if i `elem` st.done then filter (/= i) st.done else i : st.done}
      Remove i -> modify $ \st -> st {items = filter ((/= i) . fst) st.items, done = filter (/= i) st.done}
      ShowDone b -> modify $ \st -> st {showDone = b}

-- | The stylesheet: CSS as written, with the palette from Haskell.
style :: Css
style =
  [clay|
  .quotes {
    max-width: 480px; margin: 32px auto; padding: 0 20px;
    font-family: system-ui, sans-serif; color: {{ ink }};
    h2 { margin: 0 0 4px 0; }
    .hint { color: {{ muted }}; font-size: 14px; margin-top: 0; }
    code { background: {{ panel }}; padding: 1px 4px; border-radius: 3px; }
    form { display: flex; gap: 8px; margin: 16px 0 8px 0;
      .draft { flex: 1; padding: 6px 8px; {{ bordered }} }
    }
    .filter { font-size: 14px; color: {{ muted }}; display: flex; align-items: center; gap: 6px; }
    .items { list-style: none; padding: 0; margin: 12px 0;
      .item { display: flex; align-items: center; gap: 8px; padding: 8px; {{ bordered }} margin-bottom: 6px;
        .text { flex: 1; }
        &.done .text { text-decoration: line-through; color: {{ muted }}; }
        .remove { border: none; background: none; cursor: pointer; font-size: 18px; color: {{ muted }};
          &:hover { color: {{ accent }}; }
        }
      }
    }
    .empty { color: {{ muted }}; font-style: italic; }
    @media (max-width: 400px) { form { flex-direction: column; } }
  }
|]
  where
    ink = C.rgb 0x22 0x22 0x22
    muted = C.rgb 0x77 0x77 0x77
    panel = C.rgb 0xf0 0xf0 0xf0
    accent = C.rgb 0xc0 0x30 0x30
    bordered = [clay| border: 1px solid {{ C.rgb 0xd0 0xd0 0xd0 }}; border-radius: 6px; |]
