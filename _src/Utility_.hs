module Utility_ where

import Control.Lens

import Control.Monad.Reader (ReaderT(..))
import Control.Monad.State (StateT(..), liftM)

-- disable mutability locally in State monad
liftReaderT :: Monad m => ReaderT r m a -> StateT r m a
liftReaderT r = StateT $ \e -> liftM (\a -> (a,e)) (runReaderT r e)

-- I'm not sure is this thing obeys Lens laws. I just couldn't compile my code
-- without it.
--
-- Basically, it says that inside each (outer) s there's an (inner) s, to which
-- you can apply an action which returns (). And after that, the outer s will
-- remain s.
preservingResult :: Lens s s s ()
preservingResult f x = const x <$> f x
