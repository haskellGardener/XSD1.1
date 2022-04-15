{-# OPTIONS_GHC
    -Wno-name-shadowing
    -fwarn-unused-imports
#-}
{-| Time-stamp: <2022-04-15 17:17:11 CDT>

Module      : Unique
Copyright   : Robert Lee, © 2022
License     : ISC

Maintainer  : robert.lee@chicago.vc
Stability   : None
Portability : non-portable (GHC extensions)

Contumacy   : Best viewed with unbroken/unwrapped 154 column display.

Description : Provide support for unique system values paird with a create stamp.
-}

{-
infixr 9  .
infixr 8  ^, ^^, ⋆⋆
infixl 7  ⋆, /, ‘quot‘, ‘rem‘, ‘div‘, ‘mod‘
infixl 6  +, -
infixr 5  :, ++
infix  4  ==, /=, <, <=, >=, >
infixl 4  <$, <*>, <*, *>, <**>
infixr 3  &&
infixr 2  ||
infixl 1  ?, >>, >>=
infixr 1  =<<, <=<, >=>
infixr 0  $, $!, ‘seq‘

⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅ Omega Symbol Key ⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅
                   early or abnormal termination ⋅⋅⋅ Ω
                            termination (normal) ⋅⋅⋅ ω
                                    a new thread ⋅⋅⋅ ⋔
          code that can throw an error exception ⋅⋅⋅ ⏈
                                  loop-like code ⋅⋅⋅ ➿
                              a loop-like repeat ⋅⋅⋅ ↺
                           end of loop-like code ⋅⋅⋅ 🔚
               an uninterruptible exception mask ⋅⋅⋅ ☔
                code that can emit IO exceptions ⋅⋅⋅ ☢
                a warning about troublesome code ⋅⋅⋅ ⚠
  an imperative concerning imprudent code change ⋅⋅⋅ ⚡
                  a forbidden/nonsense condition ⋅⋅⋅ ⛞
                          a timed race condition ⋅⋅⋅ 🏁
⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅
-}

module Unique
  ( Unique                     -- Do not export the Value constructor -- ⚡
  , getUniqueCreated
  , unique
  )

where

-- Local Imports

-- Explicit Imports

import Control.Concurrent.STM
  ( TVar
  , atomically
  , newTVarIO
  , readTVar
  , writeTVar
  )
import Data.Hourglass   ( DateTime )
import Data.Word        ( Word64 )
import System.Hourglass ( dateCurrent )
import System.IO.Unsafe ( unsafePerformIO )

-- Qualified Imports

-- Undisciplined Imports

import Prelude

-- import Text.XML hiding (Name)

-- End of Imports
-- -----------------------------------------------------------------------------------------------------------------------------------------------------

data Unique = Unique Word64 DateTime -- Do not export the Value constructor -- ⚡

instance Eq Unique where
  (==) (Unique a _) (Unique b _) = a == b

instance Ord Unique where
  compare (Unique a _) (Unique b _) = a `compare` b

instance Show Unique where
  show (Unique _ created) = show created

-- | getUniqueCreated DateTime from Unique.
getUniqueCreated :: Unique -> DateTime
getUniqueCreated (Unique _ created) = created

{-# NOINLINE __UniqueWord #-}
__UniqueWord :: TVar Word64
__UniqueWord = unsafePerformIO (newTVarIO 0)

-- | unique creates a Unique.
unique :: IO Unique
unique = do
  currentDate <- dateCurrent
  atomically $ do
    uniqueWord <- readTVar __UniqueWord >>= pure . succ
    writeTVar __UniqueWord uniqueWord
    pure $ Unique uniqueWord currentDate
