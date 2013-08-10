module Patterns where

import Data.Tree
import Data.Tree.Zipper


import Data.Maybe

import Data.List (groupBy, partition, sort)
import Data.Ord
import Data.Function (on)
import qualified Data.IntMap as M
import Data.IntMap (IntMap)

import Control.Arrow ((&&&),first)
--------------------------------


       
data Linear = Linear Int Int



data Pattern = Base Linear Int | Derived Linear Pattern | Mixed Linear [Pattern]
