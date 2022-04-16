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
  ( Unique                 -- Do not export the Value constructor ⚡
  , deleteUniqueStampPair
  , getUniqueCreated
  , getUniqueStampPair
  , unique
  , upToDateUniqueStampPair
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

import Prelude

-- End of Imports
-- -----------------------------------------------------------------------------------------------------------------------------------------------------

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

{-# NOINLINE __UniqueInt #-} -- Do not export ⚡
__UniqueInt :: TVar Int
__UniqueInt = unsafePerformIO (newTVarIO 0)

-- | unique creates a Unique.
unique :: IO Unique
unique = do
  currentDate <- dateCurrent
  atomically $ do
    uniqueInt <- readTVar __UniqueInt >>= pure . succ
    writeTVar __UniqueInt uniqueInt
    pure $ Unique uniqueInt currentDate

-- -----------------------------------------------------------------------------------------------------------------------------------------------------
-- Support for Unique with latest timestamp.

insertLookup :: M.Key -> v -> M.IntMap v -> (Maybe v, M.IntMap v)
insertLookup k v m = M.insertLookupWithKey (\_ a _ -> a) k v m

{-# NOINLINE __UniqueUpdateIntMap #-} -- Do not export ⚡
__UniqueUpdateIntMap :: TVar (M.IntMap (Unique, DateTime))
__UniqueUpdateIntMap = unsafePerformIO (newTVarIO M.empty)

-- | getUniqueStampPair
getUniqueStampPair :: Unique -> IO (DateTime, DateTime)
getUniqueStampPair pp@(Unique i created) =
  atomically $ do
    uniqueUpdateIntMap <- readTVar __UniqueUpdateIntMap
    case M.lookup i uniqueUpdateIntMap of
      Just (_, lastUpdated) -> pure (created, lastUpdated)
      Nothing ->
        let insertedMap = M.insert i (pp, created) uniqueUpdateIntMap
        in writeTVar __UniqueUpdateIntMap insertedMap
             >> pure (created, created)

-- | upToDateUniqueStampPair
upToDateUniqueStampPair :: Unique -> IO (DateTime, DateTime)
upToDateUniqueStampPair pp@(Unique i created) = do
  currentDate <- dateCurrent
  atomically $ do
    uniqueUpdateIntMap <- readTVar __UniqueUpdateIntMap
    let (lastUpdated, newMap) =
          case insertLookup i (pp,currentDate) uniqueUpdateIntMap of
            (Just (_, lastUpdated), newMap) -> (lastUpdated, newMap)
            (Nothing              , newMap) -> (currentDate, newMap)
    writeTVar __UniqueUpdateIntMap newMap
    pure (created, lastUpdated)

-- | deleteUniqueStampPair
deleteUniqueStampPair :: Unique -> IO ()
deleteUniqueStampPair (Unique i _) =
  atomically . modifyTVar' __UniqueUpdateIntMap $ M.delete i
