module PRP.Prelude
    ( module Exports
    , show
    ) where

import           Control.Applicative    as Exports hiding (many, some)
import           Control.Exception.Safe as Exports (Exception (..), try)
import           Control.Monad          as Exports

import           Data.Default           as Exports
import           Data.Kind              as Exports (Type)
import           Data.Map               as Exports (Map)
import           Data.Set               as Exports (Set)
import           Data.String            as Exports
import           Data.Text              as Exports (Text, pack, unpack)
import           Data.Void              as Exports

import           Prelude                as Exports hiding (show)

import qualified Data.Text
import qualified Prelude

{-# INLINE show #-}

show :: Prelude.Show a => a -> Data.Text.Text
show x = Data.Text.pack (Prelude.show x)

{-# SPECIALIZE PRP.Prelude.show :: Data.Text.Text -> Data.Text.Text #-}
{-# SPECIALIZE PRP.Prelude.show :: Prelude.String -> Data.Text.Text #-}
