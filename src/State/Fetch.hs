module State.Fetch
 ( FetchStatus(..)
 , FetchState
 , fetchInitial
 , fetchUpdate
 , fetchLookup
 )
where

import Prelude hiding (lookup)

import Data.Maybe (fromMaybe)
import Data.Map.Strict (Map, insert, lookup, empty)

data FetchStatus = FetchOK
                 | FetchStarted
                 | FetchFailed
                 | FetchNothing
  deriving (Eq)

type FetchState = Map FilePath FetchStatus

fetchInitial :: FetchState
fetchInitial = empty

fetchUpdate :: FilePath -> FetchStatus -> FetchState -> FetchState
fetchUpdate = insert

fetchLookup :: FilePath -> FetchState -> FetchStatus
fetchLookup u = fromMaybe FetchNothing . lookup u
