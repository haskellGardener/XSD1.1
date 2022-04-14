{-# Language ExistentialQuantification, MultiParamTypeClasses
  , FlexibleInstances, GeneralizedNewtypeDeriving, NegativeLiterals, MultiWayIf #-}
{-| Time-stamp: <2022-04-14 13:17:25 CDT>

Module      : RegexOut
Copyright   : Robert Lee, © 2017-2022
License     : ISC

Maintainer  : robert.lee@chicago.vc
Stability   : None
Portability : non-portable (GHC extensions)

Contumacy   : Best viewed with unbroken/unwrapped 154 column display.
Latin       : Dies irae, dies illa solvet saeclum in favilla.

Description : Provide regex support for use with XML Schema 1.1.

 /$$   /$$  /$$$$$$  /$$$$$$$          /$$         /$$         /$$$$$$$
| $$  / $$ /$$__  $$| $$__  $$       /$$$$       /$$$$        | $$__  $$
|  $$/ $$/| $$  \__/| $$  \ $$      |_  $$      |_  $$        | $$  \ $$  /$$$$$$   /$$$$$$   /$$$$$$  /$$   /$$
 \  $$$$/ |  $$$$$$ | $$  | $$        | $$        | $$        | $$$$$$$/ /$$__  $$ /$$__  $$ /$$__  $$|  $$ /$$/
  >$$  $$  \____  $$| $$  | $$        | $$        | $$        | $$__  $$| $$$$$$$$| $$  \ $$| $$$$$$$$ \  $$$$/
 /$$/\  $$ /$$  \ $$| $$  | $$        | $$        | $$        | $$  \ $$| $$_____/| $$  | $$| $$_____/  >$$  $$
| $$  \ $$|  $$$$$$/| $$$$$$$/       /$$$$$$ /$$ /$$$$$$      | $$  | $$|  $$$$$$$|  $$$$$$$|  $$$$$$$ /$$/\  $$
|__/  |__/ \______/ |_______/       |______/|__/|______/      |__/  |__/ \_______/ \____  $$ \_______/|__/  \__/
                                                                                   /$$  \ $$
                                                                                  |  $$$$$$/
                                                                                   \______/
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

                      correct, but not efficient ⋅⋅⋅ η
⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅
-}

module RegexOut
where


-- Local Imports

-- import Lading
import Regex
import Parsers (anchorParser)

-- Explicit Imports

import Data.Attoparsec.Text (parseOnly)

-- Qualified Imports

import qualified Data.Text as T

-- Undisciplined Imports

import ClassyPrelude

-- End of Imports
-- -----------------------------------------------------------------------------------------------------------------------------------------------------

-- Objectives
-- 2. Create XSD regex from AST     : Take Aeson Parser AST and produce XML Schema 1.1 regex string.

outIn :: Text -> Maybe (Bool,Text,Text,Text)
outIn text = case parseOnly (anchorParser re) text of
               Left _ -> Nothing
               Right regex -> Just $ let st = scribeR regex
                                         ct = canonR regex
                                         sameP = st == text
                                     in (sameP, st, ct, text)

runRE :: Text -> Text -> Either Text (RE,Text)
runRE reText candidateText = case parseOnly (anchorParser re) reText of
                               Left t -> Left $ T.pack t
                               Right r -> case parseOnly (anchorParser $ selegoR r) normalized of
                                            Left t1 -> Left $ T.pack t1
                                            Right res -> Right res
  where normalized = case parseOnly normalize candidateText of
                       Left _ -> ""
                       Right (t,_) -> t

runRE' :: RE -> Text -> Either Text (RE,Text)
runRE' re_pp candidateText = case parseOnly (anchorParser $ selegoR re_pp) normalized of
                               Left t1 -> Left $ T.pack t1
                               Right res -> Right res
  where normalized = case parseOnly normalize candidateText of
                       Left _ -> ""
                       Right (t,_) -> t
