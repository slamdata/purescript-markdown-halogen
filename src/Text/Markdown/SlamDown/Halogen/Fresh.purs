module Text.Markdown.SlamDown.Halogen.Fresh
  ( FreshT
  , Fresh
  , runFresh
  , fresh
  ) where

import Prelude

import Data.Identity (Identity, runIdentity)
import Data.NaturalTransformation (Natural)
import Control.Monad.Reader.Trans (ReaderT, runReaderT, ask)
import Control.Monad.State.Trans (StateT, evalStateT)
import Control.Monad.State.Class (get, modify)

type FreshT m = ReaderT String (StateT Int m)
type Fresh = FreshT Identity

fresh
  ∷ ∀ m
  . (Monad m)
  ⇒ FreshT m String
fresh = do
  prefix ← ask
  n ← get ∷ FreshT m Int
  modify (_ + 1)
  pure (prefix <> "-" <> show n)

runFreshT
  ∷ ∀ m
  . (Monad m)
  ⇒ String
  → Natural (FreshT m) m
runFreshT prefix m =
  evalStateT
    (runReaderT m prefix)
    1

runFresh
  ∷ ∀ a
  . String
  → Fresh a
  → a
runFresh prefix =
  runIdentity
    <<< runFreshT prefix
