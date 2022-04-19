{-# OPTIONS_GHC
    -Wno-name-shadowing
    -fwarn-unused-imports
#-}
{-|

Module      : Unique
Copyright   : Robert Lee, © 2022
License     : ISC

Maintainer  : robert.lee@chicago.vc
Stability   : None
Portability : non-portable (GHC extensions)

Contumacy   : Best viewed with unbroken/unwrapped 154 column display.

Description : Provide support for unique system values paired with time stamps.
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
  ( Unique                 -- Do not export the Value constructor ⚡
  , deleteUniqueStamp
  , getUniqueCreated
  , getUniqueInt
  , getUniqueStamp
  , unique
  , upToDateUniqueStamp
  )

where

-- Local Imports

-- Explicit Imports

import Control.Concurrent.STM
  ( TVar
  , atomically
  , modifyTVar'
  , newTVarIO
  , readTVar
  , writeTVar
  )
import Data.Hourglass   ( DateTime )
import System.Hourglass ( dateCurrent )
import System.IO.Unsafe ( unsafePerformIO )

-- Qualified Imports

import Data.IntMap.Strict as M

-- Undisciplined Imports

import Prelude hiding (lookup)

-- End of Imports
-- -----------------------------------------------------------------------------------------------------------------------------------------------------

{-# NOINLINE __UniqueInt #-} -- Do not export ⚡
__UniqueInt :: TVar Int
__UniqueInt = unsafePerformIO (newTVarIO 0)

data Unique = Unique Int DateTime -- Do not export the Value constructor ⚡

instance Eq Unique where
  (==) (Unique a _) (Unique b _) = a == b

instance Ord Unique where
  compare (Unique a _) (Unique b _) = a `compare` b

instance Show Unique where
  show (Unique _ created) = show created

-- | getUniqueCreated DateTime from Unique.
getUniqueCreated :: Unique -> DateTime
getUniqueCreated (Unique _ created) = created

-- | getUniqueInt Int from Unique.
getUniqueInt :: Unique -> Int
getUniqueInt (Unique i _) = i

-- | unique creates a Unique.
unique :: IO Unique
unique = do
  currentDate <- dateCurrent
  atomically $ do
    uniqueInt <- readTVar __UniqueInt >>= pure . succ
    writeTVar __UniqueInt uniqueInt
    pure $ Unique uniqueInt currentDate

-- -----------------------------------------------------------------------------------------------------------------------------------------------------
-- Support for Unique with UniqueStamp.

{-# NOINLINE __UniqueStampIntMap #-} -- Do not export ⚡
__UniqueStampIntMap :: TVar (M.IntMap UniqueStamp)
__UniqueStampIntMap = unsafePerformIO (newTVarIO M.empty)

data UniqueStamp =
  UniqueStamp
  { created      :: DateTime
  , latestUpdate :: DateTime
  , priorUpdate  :: DateTime
  } deriving (Eq, Ord, Show)

data UniqueStampCondition
  = UniqueStampConditionNew
  | UniqueStampConditionOneUpdate
  | UniqueStampConditionMultipleUpdates
    deriving (Eq, Ord, Show)

uniqueStampCondition :: UniqueStamp -> UniqueStampCondition
uniqueStampCondition UniqueStamp {..}
  | created == latestUpdate = UniqueStampConditionNew
  | created == priorUpdate  = UniqueStampConditionOneUpdate
  | otherwise               = UniqueStampConditionMultipleUpdates

defaultUniqueStamp :: Unique -> UniqueStamp
defaultUniqueStamp (Unique _ created_pp) =
  UniqueStamp
  { created      = created_pp
  , latestUpdate = created_pp
  , priorUpdate  = created_pp
  }

-- | getUniqueStamp will create a UniqueStamp if one does not already exist.
getUniqueStamp :: Unique -> IO UniqueStamp
getUniqueStamp pp@(Unique i created_pp) =
  atomically $ do
    uniqueStampIntMap <- readTVar __UniqueStampIntMap
    case M.lookup i uniqueStampIntMap of
      Just uniqueStamp -> pure uniqueStamp
      Nothing ->
        let insertedMap = M.insert i defaultStamp uniqueStampIntMap
        in writeTVar __UniqueStampIntMap insertedMap
             >> pure defaultStamp
  where
    defaultStamp = defaultUniqueStamp pp -- used when no element matches pp

-- | upToDateUniqueStamp updates latestUpdate, and where needed, priorUpdate.
--   As with getUniqueStamp, upToDateUniqueStamp will create a UniqueStamp if
--   one does not already exist.
upToDateUniqueStamp :: Unique -> IO UniqueStamp
upToDateUniqueStamp pp@(Unique i created_pp) = do
  currentDate <- dateCurrent
  atomically $ do
    uniqueStampIntMap <- readTVar __UniqueStampIntMap
    let mUniqueStamp = lookup i uniqueStampIntMap
        (alteredMap, uniqueStamp) =
          case mUniqueStamp of
            Nothing ->
              let newStamp = UniqueStamp
                             { created      = created_pp
                             , latestUpdate = currentDate
                             , priorUpdate  = created_pp
                             }
                  insertedMap = insert i newStamp uniqueStampIntMap
              in (insertedMap, newStamp)
            Just oldUniqueStamp ->
              let adjustedStamp = oldUniqueStamp
                                  { latestUpdate = currentDate
                                  , priorUpdate  = latestUpdate oldUniqueStamp
                                  }
                  adjustedMap = adjust (const adjustedStamp) i uniqueStampIntMap
              in (adjustedMap, adjustedStamp)
    writeTVar __UniqueStampIntMap alteredMap
    pure uniqueStamp

-- | deleteUniqueStamp
deleteUniqueStamp :: Unique -> IO ()
deleteUniqueStamp (Unique i _) =
  atomically . modifyTVar' __UniqueStampIntMap $ M.delete i
