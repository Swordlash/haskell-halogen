{-# LANGUAGE CPP #-}

-- | "Halogen.Latest": the latest answer wins. The pure tests run on every
-- backend. The component test runs natively, on "Halogen.Test.Native".
module Test.Latest (spec) where

import Control.Monad.State (State, modify, runState)
import Data.List (delete)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust)
import Halogen.Latest
import Prelude
import Test.Hspec (Spec, describe, it, shouldBe)

#if !defined(HALOGEN_WASI) && (defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH))
import Test.Hspec (xdescribe)
#else
import Control.Monad (unless, void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Row (Empty)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.HTML.Events qualified as HE
import Halogen.HTML.Properties qualified as HP
import Halogen.Test.Native qualified as T
import Test.Hspec (shouldReturn)
#endif

spec :: Spec
spec = describe "Halogen.Latest" $ do
  describe "every order of up to five asks" $ do
    it "takes an answer exactly when nothing newer was taken, and holds the latest" $ do
      let (count, failures) = foldl' check (0 :: Int, []) runs
          check (n, bad) run = case judge run of
            Nothing -> let n' = n + 1 in n' `seq` (n', bad)
            Just why -> (n + 1, take 5 (bad <> [(run, why)]))
      failures `shouldBe` []
      count `shouldBe` 151126

  describe "against a snapshot, at every point of every order of up to four asks" $ do
    it "madeAfter says which asks were made after it, and stillAnswerable whether one made before it can still be taken" $ do
      let (count, failures) = foldl' check (0 :: Int, []) (concatMap runsOf [0 .. 4])
          check (n, bad) run = case judgeSnapshots run of
            Nothing -> let n' = n + 1 in n' `seq` (n', bad)
            Just why -> (n + 1, take 5 (bad <> [(run, why)]))
      failures `shouldBe` []
      count `shouldBe` 11229

  describe "supersede" $ do
    it "drops an answer to an ask made before it" $ do
      let (a, asks) = newAsk noAsks
      takes a (supersede asks) `shouldBe` Nothing
    it "takes an answer to an ask made after it" $ do
      let (_, asks) = newAsk noAsks
          (b, asks') = newAsk (supersede asks)
      fmap (const ()) (takes b asks') `shouldBe` Just ()
    it "drops the answer to every ask that was out at the time" $ do
      let (a, asks1) = newAsk noAsks
          (b, asks2) = newAsk asks1
          cut = supersede asks2
      (takes a cut, takes b cut) `shouldBe` (Nothing, Nothing)

  describe "asking and answered" $ do
    it "apply the change exactly when the answer is taken" $ do
      let program :: State Shown (Bool, Bool)
          program = do
            first <- asking (.pending) (\a s -> s {pending = a})
            second <- asking (.pending) (\a s -> s {pending = a})
            two <- answered (.pending) (\a s -> s {pending = a}) second (\s -> s {shown = Just "second"})
            one <- answered (.pending) (\a s -> s {pending = a}) first (\s -> s {shown = Just "first"})
            pure (two, one)
          ((tookSecond, tookFirst), end) = runState program Shown {shown = Nothing, pending = noAsks}
      (tookSecond, tookFirst) `shouldBe` (True, False)
      end.shown `shouldBe` Just "second"
    it "leave the state as it was for an answer that is dropped" $ do
      let program :: State Shown Bool
          program = do
            first <- asking (.pending) (\a s -> s {pending = a})
            _ <- asking (.pending) (\a s -> s {pending = a})
            modify (\s -> s {pending = supersede s.pending})
            answered (.pending) (\a s -> s {pending = a}) first (\s -> s {shown = Just "first"})
          before = Shown {shown = Just "kept", pending = noAsks}
          (took, end) = runState program before
      took `shouldBe` False
      end.shown `shouldBe` Just "kept"

  componentSpec

-- | A state with an answer shown and its asks.
data Shown = Shown {shown :: Maybe String, pending :: Asks}

----------------------------------------------------------------------
-- Every order

-- | What can happen in a run: an ask is made, the answer to an ask comes
-- (by the ask's number, from 1 in the order they were made), or the asks
-- out are superseded.
data Event = Make | Answer Int | Supersede
  deriving stock (Eq, Show)

-- | Every run of up to five asks. Each ask is answered once or never,
-- after it is made, in any order. Up to two supersedes come at any point,
-- never two in a row.
runs :: [[Event]]
runs = concatMap runsOf [0 .. 5]

-- | Every such run of exactly this many asks.
runsOf :: Int -> [[Event]]
runsOf total = go total 0 [] (0 :: Int) False
  where
    go :: Int -> Int -> [Int] -> Int -> Bool -> [[Event]]
    go n made out sups justSuperseded =
      [[] | made == n]
        <> [Make : rest | made < n, rest <- go n (made + 1) (out <> [made + 1]) sups False]
        <> [Answer k : rest | k <- out, rest <- go n made (delete k out) sups False]
        <> [Supersede : rest | sups < 2, not justSuperseded, rest <- go n made out (sups + 1) True]

-- | A model component, run through the events. Its view shows the number
-- of the ask whose answer it took last. A supersede clears the view, as a
-- component does when its input changes.
data Model = Model
  { asks :: Asks
  , tickets :: Map.Map Int Ask
  , made :: Int
  , cut :: Int
  -- ^ The asks up to this number were made before the last supersede.
  , latest :: Maybe Int
  -- ^ The latest ask whose answer was taken, in the whole run.
  , view :: Maybe Int
  , live :: [Int]
  -- ^ The asks made after the last supersede whose answers came.
  }

-- | 'Nothing' when every rule holds through the run, or what went wrong.
judge :: [Event] -> Maybe String
judge = walk Model {asks = noAsks, tickets = Map.empty, made = 0, cut = 0, latest = Nothing, view = Nothing, live = []}
  where
    walk m = \case
      [] ->
        let expected = if null m.live then Nothing else Just (maximum m.live)
         in if m.view == expected
              then Nothing
              else Just ("at the end the view holds " <> show m.view <> ", not " <> show expected)
      Make : rest ->
        let (ask, asks') = newAsk m.asks
         in walk m {asks = asks', tickets = Map.insert (m.made + 1) ask m.tickets, made = m.made + 1} rest
      Supersede : rest ->
        walk m {asks = supersede m.asks, cut = m.made, view = Nothing, live = []} rest
      Answer k : rest ->
        let shouldTake = k > m.cut && maybe True (k >) m.latest
            m' = if k > m.cut then m {live = k : m.live} else m
         in case takes (m.tickets Map.! k) m.asks of
              Just asks'
                | not shouldTake -> Just ("the answer to ask " <> show k <> " was taken, but it is too old")
                | otherwise -> walk m' {asks = asks', latest = Just k, view = Just k} rest
              Nothing
                | shouldTake -> Just ("the answer to ask " <> show k <> " was dropped, but nothing newer was taken")
                | otherwise -> walk m' rest

-- | A snapshot is the 'Asks' at some point of a run, with how many asks
-- were made by then. At every point of the run, against every snapshot
-- taken so far: 'madeAfter' holds exactly for the tickets made after the
-- snapshot, and 'stillAnswerable' holds exactly when 'takes' would take
-- some ticket made before it. 'Nothing' when both agree throughout, or
-- what went wrong.
judgeSnapshots :: [Event] -> Maybe String
judgeSnapshots = walk noAsks [] []
  where
    walk :: Asks -> [(Int, Ask)] -> [(Int, Asks)] -> [Event] -> Maybe String
    walk now tickets earlier events =
      let snapshots = (length tickets, now) : earlier
          wrong =
            [ "madeAfter says " <> show (madeAfter t snapshot) <> " for ask " <> show k <> " and the snapshot after " <> show madeThen <> " asks"
            | (madeThen, snapshot) <- snapshots
            , (k, t) <- tickets
            , madeAfter t snapshot /= (k > madeThen)
            ]
              <> [ "stillAnswerable says " <> show (stillAnswerable snapshot now) <> " for the snapshot after " <> show madeThen <> " asks, with " <> show (length tickets) <> " made"
                 | (madeThen, snapshot) <- snapshots
                 , stillAnswerable snapshot now /= any (\(k, t) -> k <= madeThen && isJust (takes t now)) tickets
                 ]
       in case wrong of
            why : _ -> Just why
            [] -> case events of
              [] -> Nothing
              Make : rest -> let (t, now') = newAsk now in walk now' (tickets <> [(length tickets + 1, t)]) snapshots rest
              Supersede : rest -> walk (supersede now) tickets snapshots rest
              Answer k : rest -> walk (fromMaybe now (lookup k tickets >>= (`takes` now))) tickets snapshots rest

----------------------------------------------------------------------
-- A component

#if !defined(HALOGEN_WASI) && (defined(javascript_HOST_ARCH) || defined(wasm32_HOST_ARCH))

componentSpec :: Spec
componentSpec = xdescribe "a component that asks twice (native only)" $ pure ()

#else

data Loads = Loads {showing :: Text, dropped :: Int, loads :: Asks}

data LoadAction = Load Text

-- | Loads on a click: @button.one@ asks for "one", @button.two@ for "two".
-- Each ask waits on the gate in a fork. The view shows the answer taken,
-- and how many were dropped.
loader :: forall m. (MonadIO m) => T.Gate Text Text -> H.Component H.VoidF () Void m
loader gate =
  H.component
    (\() -> pure Loads {showing = "", dropped = 0, loads = noAsks})
    render
    [H.onAction act]
  where
    act (Load what) = do
      ask <- asking (.loads) (\a s -> s {loads = a})
      void $ H.fork $ do
        r <- H.async (liftIO (T.hold gate what))
        took <- answered (.loads) (\a s -> s {loads = a}) ask (\s -> s {showing = r})
        unless took $ modify (\s -> s {dropped = s.dropped + 1})
    render :: Loads -> H.ComponentHTML LoadAction Empty m
    render s =
      HH.div_
        [ HH.p [HP.class_ (HH.ClassName "view")] [HH.text s.showing]
        , HH.p [HP.class_ (HH.ClassName "dropped")] [HH.text (Text.pack (show s.dropped))]
        , HH.button [HP.class_ (HH.ClassName "one"), HE.onClick (const (Load "one"))] [HH.text "one"]
        , HH.button [HP.class_ (HH.ClassName "two"), HE.onClick (const (Load "two"))] [HH.text "two"]
        ]

-- | Click "one", then "two", each once the program holds its ask. Then
-- hand the test the two held asks, told apart by what they ask.
twoAsks :: T.Gate Text Text -> T.View H.VoidF () Void -> (T.Held Text Text -> T.Held Text Text -> IO ()) -> IO ()
twoAsks gate view body = do
  let settled = T.within (("the view:\n" <>) <$> T.html view) (T.settleHolding view (T.holds gate))
  settled
  T.click view "button.one"
  settled
  T.click view "button.two"
  settled
  asks <- T.held gate
  let named what = filter (\h -> h.asked == what) asks
  case (named "one", named "two") of
    ([one], [two]) -> body one two
    _ -> fail ("expected one ask for each button, held: " <> show (map (.asked) asks))

componentSpec :: Spec
componentSpec = describe "a component that asks twice" $ do
  it "keeps the later answer when the earlier one comes after it" $ do
    gate <- T.newGate
    T.withView id (loader gate) () $ \view -> twoAsks gate view $ \one two -> do
      let holding = T.within (T.html view) (T.settleHolding view (T.holds gate))
          settled = T.within (T.html view) (T.settle view)
      T.answer gate two "TWO"
      holding
      T.textOf view ".view" `shouldReturn` "TWO"
      T.answer gate one "ONE"
      settled
      T.textOf view ".view" `shouldReturn` "TWO"
      T.textOf view ".dropped" `shouldReturn` "1"

  it "shows the earlier answer until the later one comes" $ do
    gate <- T.newGate
    T.withView id (loader gate) () $ \view -> twoAsks gate view $ \one two -> do
      let holding = T.within (T.html view) (T.settleHolding view (T.holds gate))
          settled = T.within (T.html view) (T.settle view)
      T.answer gate one "ONE"
      holding
      T.textOf view ".view" `shouldReturn` "ONE"
      T.answer gate two "TWO"
      settled
      T.textOf view ".view" `shouldReturn` "TWO"
      T.textOf view ".dropped" `shouldReturn` "0"

#endif
