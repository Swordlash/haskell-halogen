{-# OPTIONS_GHC -Wno-deprecated-type-abstractions #-}

module Halogen.Material.Tabs
  ( tabsComponent
  , IconPosition (..)
  , TabSpec (..)
  , TabsSpec (..)
  , TabsOutput (..)
  , TabsQuery (..)
  , emptyTabsSpec
  )
where

import Clay qualified as C
import Control.Monad.Extra (pureIf)
import Data.Functor qualified as F
import Data.List.NonEmpty ((!!))
import Data.Row
import Data.Text qualified as T
import Halogen qualified as H hiding (Initialize)
import Halogen.Component
import Halogen.HTML qualified as HH
import Halogen.HTML.Events qualified as HE
import Halogen.HTML.Properties qualified as HP
import Halogen.HTML.Properties.ARIA qualified as HPA
import Halogen.Material.Icons
import Halogen.Material.Monad
import Protolude

data IconPosition = Leading | Stacked
  deriving (Eq)

data TabSpec = TabSpec
  { label :: Maybe Text
  , icon :: Maybe (Icon, IconPosition)
  }

data TabsSpec slots i m = TabsSpec
  { tabs :: NonEmpty (TabSpec, HH.ComponentHTML i slots m)
  , selectedTab :: Int
  , extraStyle :: C.Css
  }

emptyTabsSpec :: TabsSpec slots i m
emptyTabsSpec =
  TabsSpec
    { tabs = pure (TabSpec Nothing Nothing, HH.text "")
    , selectedTab = 0
    , extraStyle = mempty
    }

data TabsState slots i m = TabsState
  { mdcTabBar :: Maybe MDCTabBar
  , tabs :: NonEmpty (TabSpec, HH.ComponentHTML i slots m)
  , selectedTab :: Int
  , extraStyle :: C.Css
  }

data TabsAction i
  = Initialize
  | Finalize
  | TabSelected Int
  | ChildAction i

data TabsQuery slots q a
  = forall label input' output' slot.
    (HasType label (H.Slot q input' output' slot) slots, KnownSymbol label, Ord slot) =>
    ParentQuery (Proxy label) slot (q a)
  | GetSelectedTab (Int -> a)

data TabsOutput i
  = SelectedTab Int
  | ChildOutput i

tabsComponent
  :: forall q slots i m
   . (MonadMaterial m)
  => H.Component (TabsQuery slots q) (TabsSpec slots i m) (TabsOutput i) m
tabsComponent =
  H.mkComponent $
    H.ComponentSpec
      { initialState = \TabsSpec {..} -> pure TabsState {mdcTabBar = Nothing, ..}
      , render
      , eval = H.mkEval $ H.defaultEval {handleAction, handleQuery, initialize = Just Initialize, finalize = Just Finalize}
      }
  where
    ref = H.RefLabel "tabBar"

    render TabsState {..} =
      HH.div
        [HP.style (C.display C.flex <> C.flexDirection C.column <> extraStyle)]
        [header, HH.mapHTMLAction ChildAction tab]
      where
        (tabHeaders, tabContents) = F.unzip tabs
        tab = tabContents !! selectedTab

        header =
          HH.div [HP.class_ (HH.ClassName "mdc-tab-bar"), HPA.role "tablist", HP.ref ref] $
            pure $
              HH.div [HP.class_ (HH.ClassName "mdc-tab-scroller")] $
                pure $
                  HH.div [HP.class_ (HH.ClassName "mdc-tab-scroller__scroll-area")] $
                    pure $
                      HH.div [HP.class_ (HH.ClassName "mdc-tab-scroller__scroll-content")] $
                        map renderTab $
                          zip [0 ..] $
                            toList tabHeaders

        renderTab (i, TabSpec {..}) =
          HH.button
            [ HP.classes $
                HH.ClassName "mdc-tab"
                  : catMaybes [pureIf (i == selectedTab) (HH.ClassName "mdc-tab--active"), pureIf (fmap snd icon == Just Stacked) (HH.ClassName "mdc-tab--stacked")]
            , HPA.role "tab"
            , HPA.selected (T.toLower $ show $ selectedTab == i)
            , HP.tabIndex (if i == selectedTab then 0 else -1)
            , HE.onClick (const $ TabSelected i)
            ]
            [ HH.span [HP.class_ (HH.ClassName "mdc-tab__content")] $
                catMaybes
                  [ HH.span [HP.class_ (HH.ClassName "mdc-tab__text-label")] . pure . HH.text <$> label
                  , HH.span [HP.classes [HH.ClassName "mdc-tab__icon", HH.ClassName "material-icons"], HPA.hidden "true"] . pure . HH.text . iconText . fst <$> icon
                  ]
            , HH.span
                [HP.classes (HH.ClassName "mdc-tab-indicator" : pureIf (i == selectedTab) (HH.ClassName "mdc-tab-indicator--active"))]
                [ HH.span [HP.classes [HH.ClassName "mdc-tab-indicator__content", HH.ClassName "mdc-tab-indicator__content--underline"]] []
                ]
            , HH.span [HP.class_ (HH.ClassName "mdc-tab__ripple")] []
            ]

    handleQuery
      :: TabsQuery slots q x
      -> H.HalogenM (TabsState slots i m) (TabsAction i) slots (TabsOutput i) m (Maybe x)
    handleQuery = \case
      GetSelectedTab k -> Just . k <$> gets (.selectedTab)
      ParentQuery (Proxy @lab) slot q -> H.query lab slot q

    handleAction = \case
      Initialize -> do
        H.getHTMLElementRef ref >>= \case
          Nothing -> panic "Cannot initialize Tab Bar, no HTML element found\n"
          Just e -> do
            mdcTabBar <- lift $ initTabBar e
            modify $ \s -> s {mdcTabBar = Just mdcTabBar}
      Finalize -> traverse_ (lift . destroyTabBar) =<< gets (.mdcTabBar)
      TabSelected i -> do
        modify $ \s -> s {selectedTab = i} :: TabsState slots i m
        H.raise $ SelectedTab i
      ChildAction i -> H.raise $ ChildOutput i
