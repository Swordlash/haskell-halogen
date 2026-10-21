-- | Hydration in a real page: a component mounted with
-- 'Halogen.VDom.Driver.hydrateUI' over HTML the browser parsed, as a page
-- arrives with what "Halogen.Static" rendered. The core suite tests the
-- same against the in-memory document, which has no parser; here the
-- markup is what a parser makes of it, comments and all.
module Test.Hydrate (spec) where

import Data.Row (Empty, type (.==))
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.HTML.Events qualified as HE
import Halogen.HTML.Properties qualified as HP
import Protolude hiding (find)
import Test.Hspec.Halogen

-- | Renders this, and nothing else.
static :: forall m. (Monad m) => H.ComponentHTML () Empty m -> H.Component H.VoidF () Void m
static html =
  H.mkComponent
    H.ComponentSpec
      { initialState = \_ -> pure ()
      , render = \_ -> html
      , eval = H.mkEval H.defaultEval
      }

data Count = Increment

-- | A button that counts its clicks: two texts in a paragraph, the second
-- the count, and an emphasis after them once it has been clicked.
counter :: forall m. (Monad m) => H.Component H.VoidF () Void m
counter =
  H.mkComponent
    H.ComponentSpec
      { initialState = \_ -> pure (0 :: Int)
      , render = \n ->
          HH.div_
            [ HH.p_ ([HH.text "count ", HH.text (show n)] <> [HH.em_ [HH.text "!"] | n > 0])
            , HH.button [HE.onClick (const Increment)] [HH.text "+"]
            ]
      , eval = H.mkEval H.defaultEval {H.handleAction = \Increment -> modify (+ 1)}
      }

newtype Edit = Edited Text

-- | A text field and what it holds.
field :: forall m. (Monad m) => H.Component H.VoidF () Void m
field =
  H.mkComponent
    H.ComponentSpec
      { initialState = \_ -> pure "prerendered"
      , render = \txt ->
          HH.div_
            [ HH.input [HP.value txt, HE.onValueInput Edited]
            , HH.span [HP.class_ (HH.ClassName "echo")] [HH.text txt]
            ]
      , eval = H.mkEval H.defaultEval {H.handleAction = \(Edited txt) -> put txt}
      }

type Slots = ("child" .== H.Slot H.VoidF Void ())

-- | A child component in a slot, between two elements of its parent.
parent :: forall m. (Monad m) => H.Component H.VoidF () Void m
parent =
  H.mkComponent
    H.ComponentSpec
      { initialState = \_ -> pure ()
      , render = \_ ->
          HH.div_ [HH.h2_ [HH.text "parent"], HH.slot_ "child" () (static (HH.em_ [HH.text "child"])) (), HH.p_ [HH.text "after"]]
            :: H.ComponentHTML () Slots m
      , eval = H.mkEval H.defaultEval
      }

spec :: forall m -> (MonadBrowserTest m) => Spec
spec m = do
  it "adopts the elements of markup that matches" $ runPage $ do
    page <- htmlContainer "<div><p>count <!---->0</p><button>+</button></div>"
    before <- traverse (findIn page) ["div", "p", "button"]
    ui <- hydrate m page counter ()
    after <- traverse (find ui) ["div", "p", "button"]
    zipWithM_ shouldBeSameElement after before
    outerHTML (root ui) `shouldReturn` "<div><div><p>count 0</p><button>+</button></div></div>"

  it "attaches the listeners, and patches what it adopted" $ runPage $ do
    page <- htmlContainer "<div><p>count <!---->0</p><button>+</button></div>"
    paragraph <- findIn page "p"
    ui <- hydrate m page counter ()
    find ui "button" >>= click
    find ui "p" >>= (`shouldHaveText` "count 1!")
    find ui "p" >>= (`shouldBeSameElement` paragraph)
    -- The comment between the texts is gone: left there, it would have
    -- taken the place the emphasis was inserted at.
    find ui "p" >>= outerHTML >>= (`shouldBe` "<p>count 1<em>!</em></p>")

  it "splits a text the page has as one" $ runPage $ do
    page <- htmlContainer "<div><p>count 0</p><button>+</button></div>"
    ui <- hydrate m page counter ()
    find ui "button" >>= click
    find ui "p" >>= outerHTML >>= (`shouldBe` "<p>count 1<em>!</em></p>")

  it "replaces an element whose tag differs" $ runPage $ do
    page <- htmlContainer "<div><span>x</span></div>"
    outer <- findIn page "div"
    ui <- hydrate m page (static (HH.div_ [HH.p_ [HH.text "x"]])) ()
    find ui "div" >>= (`shouldBeSameElement` outer)
    (length <$> findAll ui "span") `shouldReturn` 0
    find ui "p" >>= (`shouldHaveText` "x")

  it "builds a node the page is missing" $ runPage $ do
    page <- htmlContainer "<ul><li>a</li></ul>"
    firstItem <- findIn page "li"
    ui <- hydrate m page (static (HH.ul_ [HH.li_ [HH.text "a"], HH.li_ [HH.text "b"]])) ()
    items@(stillFirst : _) <- findAll ui "li"
    stillFirst `shouldBeSameElement` firstItem
    traverse textContent items `shouldReturn` ["a", "b"]

  it "removes a node the component does not render" $ runPage $ do
    page <- htmlContainer "<ul><li>a</li><li>b</li></ul>"
    firstItem <- findIn page "li"
    ui <- hydrate m page (static (HH.ul_ [HH.li_ [HH.text "a"]])) ()
    [item] <- findAll ui "li"
    item `shouldBeSameElement` firstItem
    outerHTML (root ui) `shouldReturn` "<div><ul><li>a</li></ul></div>"

  it "takes out what is around the HTML in its container" $ runPage $ do
    page <- htmlContainer "\n  <!-- prerendered -->\n  <div><p>count <!---->0</p><button>+</button></div>\n"
    outer <- findIn page "div"
    ui <- hydrate m page counter ()
    find ui "div" >>= (`shouldBeSameElement` outer)
    outerHTML (root ui) `shouldReturn` "<div><div><p>count 0</p><button>+</button></div></div>"

  it "keeps a prerendered field, and listens to it" $ runPage $ do
    page <- htmlContainer "<div><input value=\"prerendered\"><span class=\"echo\">prerendered</span></div>"
    input <- findIn page "input"
    ui <- hydrate m page field ()
    find ui "input" >>= (`shouldBeSameElement` input)
    getProperty input "value" `shouldReturn` "prerendered"
    clear input
    typeText input "edited"
    find ui ".echo" >>= (`shouldHaveText` "edited")

  it "hydrates a child component in its slot" $ runPage $ do
    page <- htmlContainer "<div><h2>parent</h2><em>child</em><p>after</p></div>"
    before <- traverse (findIn page) ["h2", "em", "p"]
    ui <- hydrate m page parent ()
    after <- traverse (find ui) ["h2", "em", "p"]
    zipWithM_ shouldBeSameElement after before

  it "builds a child the page has nothing for, where its slot is" $ runPage $ do
    page <- htmlContainer "<div><h2>parent</h2></div>"
    heading <- findIn page "h2"
    ui <- hydrate m page parent ()
    find ui "h2" >>= (`shouldBeSameElement` heading)
    outerHTML (root ui) `shouldReturn` "<div><div><h2>parent</h2><em>child</em><p>after</p></div></div>"
