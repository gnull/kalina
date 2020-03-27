{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Menu where

import Data.Maybe
import Data.List
import Data.Text () -- Instances
import qualified Data.Text as T
import Control.Monad.IO.Class (MonadIO(..))

import Data.Functor ((<&>))

import Control.Lens

import GenericFeed

import Brick
import Brick.Markup
import Brick.Widgets.List
import Brick.Widgets.Center
import Brick.Widgets.Border
import Graphics.Vty.Input.Events

instance Splittable [] where
  splitAt = Data.List.splitAt

type MyList n e = GenericList n [] e
type L e = MyList () e

toGenericList :: [a] -> L a
toGenericList x = list () x 1

data MenuState
  = LevelFeeds    (L (String, Maybe CacheEntry))
  | LevelItems    (L (String, Maybe CacheEntry)) (L (GenericItem, ItemStatus))
  | LevelContents (L (String, Maybe CacheEntry)) (L (GenericItem, ItemStatus))

data State = State { innerState :: CacheFile, menuState :: MenuState }

initialState :: CacheFile -> State
initialState c = State { innerState = c
                       , menuState = LevelFeeds $ toGenericList c
                       }

-- This allows to read or modify the `fs' field of MenuState.
fsListL :: Lens' MenuState (L (String, Maybe CacheEntry))
fsListL f (LevelFeeds fs) = LevelFeeds <$> f fs
fsListL f (LevelItems fs is) = (\x -> LevelItems x is) <$> f fs
fsListL f (LevelContents fs is) = (\x -> LevelContents x is) <$> f fs

-- This allows to modify the `is' field of MenuState. If the field is not
-- present, nothing happens. But it doesn't allow reading the field, since it
-- may not be present.
isListL :: Traversal' MenuState (L (GenericItem, ItemStatus))
isListL _ s@(LevelFeeds _) = pure s
isListL f (LevelItems fs is) = LevelItems fs <$> f is
isListL f (LevelContents fs is) = LevelContents fs <$> f is

updateMenuState :: CacheFile -> MenuState -> MenuState
updateMenuState c ms = over isListL (patchGenList fstEqual (snd $ fromJust $ snd $ selectedElement $ ms' ^. fsListL)) ms'
  where
    ms' = over fsListL (patchGenList fstEqual c) ms
    -- These two are utility functions
    patchGenList :: (a -> a -> Bool) -> [a] -> L a -> L a
    patchGenList eq l' l = listReplace l' (Just pos) l
      where
        sel = selectedElement l
        pos = fromJust $ findIndex (eq sel) l'
    fstEqual :: Eq a => (a, b) -> (a, b) -> Bool
    fstEqual (x, _) (y, _) = x == y

selectedElement :: L x -> x
selectedElement = snd . fromJust . listSelectedElement

selectedElementL :: Lens' (L x) x
selectedElementL f l = (\y -> listModify (const y) l) <$> f x
  where
    x = selectedElement l

-- TODO: Make these two functions work with State instead of MenuState

stateDown :: MenuState -> MenuState
stateDown s@(LevelFeeds fs) =
  let f = selectedElement fs
  in case snd f of
    Nothing -> s
    Just (_, is) -> LevelItems fs $ toGenericList is
stateDown (LevelItems fs is) =
  let (i, _) = selectedElement is
      is' = listModify (const (i, True)) is
      fs' = listModify (\(u, c) -> (u, c <&> \(gf, _) -> (gf, listElements is'))) fs
      -- fs' = listModify (second $ (Just .) $ const $ listElements is') fs
  in LevelContents fs' is'
stateDown s@(LevelContents _ _) = s

stateUp :: MenuState -> MenuState
stateUp s@(LevelFeeds _) = s
stateUp (LevelItems fs _) = LevelFeeds fs
stateUp (LevelContents fs is) = LevelItems fs is

renderContents :: GenericItem -> Widget ()
renderContents (GenericItem {..}) =
  txtWrap $ T.unlines $ catMaybes
    [ ("Title: " <>) <$> giTitle
    , ("Link: " <>) <$> giURL
    , ("Author: " <>) <$> giAuthor
    , ("Date: " <>) <$> giDate
    , Just ""
    , giBody
    ]

renderItem :: Bool -> (GenericItem , ItemStatus)-> Widget ()
renderItem _ (GenericItem {..}, r) = padRight Max $ markup
  $ (@? if r then "read-item" else "unread-item") $
      (if r then "   " else " N ")
   <> (fromMaybe "*No Date*" giDate)
   <> "  "
   <> (T.unwords $ T.words $ fromMaybe "*Empty*" giTitle)

renderFeed :: Bool -> (String, Maybe CacheEntry) -> Widget ()
renderFeed _ f = (txt " × ")
              <+> (hLimit 6 $ padRight Max $ markup $ unreadCount @? readStatus)
              <+> vLimit 1 vBorder
              <+> (padRight Max $ markup $ (" " <> caption) @? readStatus)
  where
    (unread, total, caption) = case f of
      (_, Just (gf, is)) -> ( length $ filter (not . snd) is
                            , length is
                            , gfTitle gf <> " (" <> gfURL gf <> ")")
      (u, Nothing) -> (0, 0, T.pack u)
    readStatus = if unread == 0 then "read-item" else "unread-item"
    unreadCount = T.pack $ show unread <> "/" <> show total

drawMenu :: MenuState -> Widget ()
drawMenu s =
    case s of
      LevelFeeds fs -> g $ renderList renderFeed True fs
      LevelItems _ is -> g $ renderList renderItem True is
      LevelContents _ is -> f $ padBottom Max $ renderContents (fst $ selectedElement is)
  where
    f x = vBox
      [x
      , str ""
      , vLimit 3 $ borderWithLabel (str "help") $
            str " q - back/quit "
        <+> vBorder
        <+> str " r - fetch selected feed "
        <+> vBorder
        <+> str " R - fetch all feeds "
        <+> vBorder
        <+> str " Enter - open an entry "
        <+> vBorder
        <+> str " h,j,k,l - navigation "]
    g x = vCenter $ f x

handleMenu :: (FilePath -> IO ()) -> State -> Event -> EventM () (Next State)
handleMenu queue st@(State _ s) (EvKey (KChar 'r') _) = continue =<< fmap (\y -> st {menuState = y}) x
  where
    x = do
      let (u, _) = selectedElement fs
      liftIO $ queue u
      pure s
    fs = case s of
      LevelFeeds fs' -> fs'
      LevelItems fs' _ -> fs'
      LevelContents fs' _ -> fs'
handleMenu queue st@(State c _) (EvKey (KChar 'R') _) = do
  liftIO $ sequence_ $ fmap (queue . fst) c
  continue st
handleMenu _ st@(State _ s) (EvKey (KChar 'q') _) =
  case s of
    LevelFeeds _ -> halt st
    _ -> continue $ st {menuState = stateUp s}
handleMenu _ st@(State _ s) (EvKey KEnter _) = continue $ st {menuState = stateDown s}
handleMenu _ st@(State _ s) e = continue =<< fmap (\y -> st {menuState = y}) x
  where
    x = case s of
      LevelFeeds fs -> do
        fs' <- handleListEventVi handleListEvent e fs
        pure $ LevelFeeds fs'
      LevelItems fs is -> do
        is' <- handleListEventVi handleListEvent e is
        pure $ LevelItems fs is'
      LevelContents _ _ -> pure s
