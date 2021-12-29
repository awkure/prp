{-# LANGUAGE DeriveAnyClass #-}

module PRP.Con where

import           Control.Monad.ST  (ST)
import qualified Control.Monad.ST  as ST
import           Data.STRef        (STRef)
import qualified Data.STRef.Strict as ST

import           Control.Arrow     ((>>>))
import qualified Data.Default      as Default
import           Data.Tuple        (swap)

import           PRP.Prelude       hiding (read)
import qualified PRP.Val           as PRP

class Monad m => WitnessM (m :: Type -> Type) where
  data Witness m :: Type -> Type
  discard :: m x
  fill    :: x -> m (Witness m x)
  watch   :: Witness m x -> (x -> m ()) -> m ()
  with    :: Witness m x -> (x -> m ()) -> m ()
  write   :: Joinable x => Witness m x -> x -> m ()

type Joinable x = (Eq x)

unary
  :: ( WitnessM m
     , Joinable x
     , Joinable y
     )
  => ((x, y) -> (x, y))
  -> Witness m x
  -> Witness m y
  -> m ()
unary f xs ys = do
  let
    update x y = do
      let (x', y') = f (x, y)
      write xs x' *> write ys y'

  watch xs \x -> with ys \y -> update x y
  watch ys \y -> with xs \x -> update x y

binary
  :: ( WitnessM m
     , Joinable x
     , Joinable y
     , Joinable z
     )
  => ((x, y, z) -> (x, y, z))
  -> Witness m x
  -> Witness m y
  -> Witness m z
  -> m ()
binary f xs ys zs = do
  let
    update x y z = do
      let (x', y', z') = f (x, y, z)

      write xs x'
      write ys y'
      write zs z'

  watch xs \x -> with ys \y -> with zs \z -> update x y z
  watch ys \y -> with xs \x -> with zs \z -> update x y z
  watch zs \z -> with ys \y -> with xs \x -> update x y z

unify :: forall x m . (WitnessM m, Joinable x) => Witness m x -> Witness m x -> m ()
unify = unary swap

--------------------------------------------------------------------------------
-- * Instances
--------------------------------------------------------------------------------

data WitnessRef s a = WitnessRef
  { val    :: PRP.Val a
  , action :: ST s a
  }

instance WitnessM (ST s) where
  newtype Witness (ST s) a
    = Witness
    { ref :: STRef s (WitnessRef s a)
    }
  write Witness{ref} updated = do
    ST.modifySTRef' ref \case
      WitnessRef { val = PRP.Undefined, action } -> WitnessRef { val = PRP.Exact updated, action = action }
      WitnessRef { val = PRP.Exact v, action }
        | v == updated -> WitnessRef { val = PRP.Exact v, action }
        | otherwise -> WitnessRef { val = PRP.Conflict, action }
      e@WitnessRef { val = PRP.Conflict } -> e

  watch witness update = do
    let
      cont = read witness >>= \case
          PRP.Undefined -> pure ()
          PRP.Conflict  -> pure ()
          PRP.Exact v   -> update v
    ST.modifySTRef'

  discard = undefined
  fill = undefined
  with = undefined

make :: a -> ST s (Witness (ST s) a)
make = fmap Witness <$> ST.newSTRef . PRP.Exact

read :: Witness (ST s) a -> ST s (PRP.Val a)
read = ST.readSTRef . ref

unsafeRead :: Witness (ST s) a -> ST s a
unsafeRead Witness{ref} = ST.readSTRef ref >>= \case
  PRP.Undefined -> error "unsafeRead: undefined"
  PRP.Exact v   -> pure v
  PRP.Conflict  -> error "unsafeRead: conflict"


-- ex :: (Int, Int, Int)
-- ex = ST.runST phi
--   where
--     phi :: ST s (Int, Int, Int)
--     phi = do
--       x <- make 2
--       y <- make 3
--       z <- make 5

--       unify x y
--       unify x z
--       unify y z

--       x <- unsafeRead x
--       y <- unsafeRead y
--       z <- unsafeRead z

--       pure (x, y, z)

