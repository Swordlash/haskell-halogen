module Halogen.Material.List
  ( ElemRenderer (..)
  , ElemTextRenderer (..)
  , ListCfg (..)
  , ListElem (..)
  , ListQuery (..)
  , ListOutput (..)
  , MDCList (..)
  , list
  )
where

import Clay (Css)
import Data.Row (HasType)
import Halogen qualified as H hiding (Initialize)
import Halogen.Component
import Halogen.HTML qualified as HH
import Halogen.HTML.Properties qualified as HP
import Halogen.HTML.Properties.ARIA qualified as HPA
import Halogen.Material.Icons qualified as Icon
import Halogen.Material.Monad
import Halogen.VDom.DOM.Monad
import Protolude hiding (list, log)

data ElemTextRenderer a
  = Oneline (a -> Text)
  | Twoline (a -> (Text, Text))

isTwoLine :: ElemTextRenderer a -> Bool
isTwoLine Twoline {} = True
isTwoLine Oneline {} = False

data ElemRenderer a slots i m = ElemRenderer
  { iconRenderer :: Maybe (a -> Icon.Icon)
  , textRenderer :: ElemTextRenderer a
  , metaRenderer :: Maybe (a -> HH.ComponentHTML i slots m)
  }

data ListElem a
  = ListElem a
  | Separator

isSeparator :: ListElem a -> Bool
isSeparator Separator = True
isSeparator _ = False

data ListCfg a slots i m = ListCfg
  { items :: [ListElem a]
  , elemRenderer :: ElemRenderer a slots i m
  , extraStyle :: Css
  }

data ListState a slots i m = ListState
  { mdcList :: Maybe MDCList
  , mdcItems :: [MDCRipple]
  , items :: [ListElem a]
  , elemRenderer :: ElemRenderer a slots i m
  , extraStyle :: Css
  }

data ListAction i
  = Initialize
  | Finalize
  | InitRipples
  | DestroyRipples
  | ParentAction i

data ListQuery slots q a
  = forall label output' slot.
    (HasType label (H.Slot q output' slot) slots, KnownSymbol label, Ord slot) =>
    ParentQuery (Proxy label) slot (q a)
  | GetSelectedIndex (Int -> a)

data ListOutput i
  = SelectedIndex Int
  | ParentOutput i

list
  :: forall a q slots i m
   . (MonadIO m, MonadDOM m, MonadMaterial m)
  => H.Component (ListQuery slots q) (ListCfg a slots i m) (ListOutput i) m
list =
  H.mkComponent $
    H.ComponentSpec
      { initialState = \ListCfg {..} -> ListState {mdcList = Nothing, mdcItems = [], ..}
      , render
      , eval = H.mkEval $ H.defaultEval {handleAction, handleQuery, initialize = Just Initialize, receive = const $ Just InitRipples, finalize = Just DestroyRipples}
      }
  where
    ref = H.RefLabel "list"

    render :: ListState a slots i m -> HH.ComponentHTML (ListAction i) slots m
    render ListState {elemRenderer = ElemRenderer {..}, ..} =
      HH.ul
        [ HP.ref ref
        , HP.style extraStyle
        , HP.classes $
            fold
              [ [HH.ClassName "mdc-deprecated-list"]
              , [HH.ClassName "mdc-deprecated-list--two-line" | isTwoLine textRenderer]
              , [HH.ClassName "mdc-deprecated-list--icon-list" | isJust iconRenderer]
              ]
        ]
        elems
      where
        elClassName x = HH.ClassName $ if isSeparator x then "mdc-deprecated-list-divider" else "mdc-deprecated-list-item"

        elems = case items of
          [] -> []
          (x : xs) ->
            HH.li [HP.class_ (elClassName x), HP.tabIndex 0] (renderElem x)
              : map (\y -> HH.li [HP.class_ $ elClassName y] (renderElem y)) xs

        renderElem Separator = []
        renderElem (ListElem a) =
          catMaybes
            [ Just $ HH.span [HP.class_ (HH.ClassName "mdc-deprecated-list-item__ripple")] []
            , HH.span [HP.classes [HH.ClassName "mdc-deprecated-list-item__graphic", HH.ClassName "material-icons"], HPA.hidden "true"] . pure . HH.text . Icon.iconText . ($ a) <$> iconRenderer
            , Just $ HH.span [HP.class_ (HH.ClassName "mdc-deprecated-list-item__text")] (renderText a)
            , HH.span [HP.class_ (HH.ClassName "mdc-deprecated-list-item__meta")] . pure . HH.mapHTMLAction ParentAction . ($ a) <$> metaRenderer
            ]

        renderText a = case textRenderer of
          Oneline f -> [HH.text (f a)]
          Twoline f ->
            let (p, s) = f a
             in [ HH.span [HP.class_ (HH.ClassName "mdc-deprecated-list-item__primary-text")] [HH.text p]
                , HH.span [HP.class_ (HH.ClassName "mdc-deprecated-list-item__secondary-text")] [HH.text s]
                ]

    handleQuery
      :: ListQuery slots q x
      -> H.HalogenM (ListState a slots i m) (ListAction i) slots (ListOutput i) m (Maybe x)
    handleQuery = \case
      GetSelectedIndex k -> pure $ Just $ k (-1) -- TODO
      ParentQuery lab slot q -> H.query lab slot q

    handleAction = \case
      Initialize ->
        H.getHTMLElementRef ref >>= \case
          Just el -> do
            mdcList <- lift $ initList el
            modify $ \s -> s {mdcList = Just mdcList}
            handleAction InitRipples
          Nothing -> lift $ log "Cannot initialize list, no HTML element found"
      InitRipples -> do
        handleAction DestroyRipples -- first destroy old ones
        gets (.mdcList) >>= \case
          Just l ->
            void $ lift $ initListItems l
          Nothing -> lift $ log "Cannot initialize ripples, MDCList not initialized"
      DestroyRipples ->
        traverse_ (lift . destroyRipple) =<< gets (.mdcItems)
      Finalize -> do
        handleAction DestroyRipples
        traverse_ (lift . destroyList) =<< gets (.mdcList)
      ParentAction i -> H.raise $ ParentOutput i
