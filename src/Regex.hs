{-# Language ExistentialQuantification, MultiParamTypeClasses, FlexibleInstances, GeneralizedNewtypeDeriving, NegativeLiterals #-}
{-| Time-stamp: <2018-06-25 09:30:31 robert>

Module      : Builtin
Copyright   : (c) Robert Lee, 2017-2018
License     : ISC

Maintainer  : robert.lee@chicago.vc
Stability   : None
Portability : non-portable (GHC extensions)

Contumacy   : Best viewed with unbroken/unwrapped 154 column display.

Description : Provide lexical and value correct types for use with XML Schema 1.1.

3.2 Special Built-in Datatypes  Support    Haskell type
    3.2.2 anyAtomicType         ✓          AnyAtomicType

3.3 Primitive datatypes         Support    Haskell type
    3.3.1  string               ✓          Stringxs
    3.3.2  boolean              ✓          Boolean
    3.3.3  decimal              ✓          Decimal
    3.3.4  float                ✓          Floatxs
    3.3.5  double               ✓          Doublexs
    3.3.6  duration             ✓          Durationxs
    3.3.7  dateTime             ✓          DateTimexs
    3.3.8  time                 ✓          Timexs
    3.3.9  date                 ✓          Datexs
    3.3.10 gYearMonth           ✓          GYearMonth
    3.3.11 gYear                ✓          GYear
    3.3.12 gMonthDay            ✓          GMonthDay
    3.3.13 gDay                 ✓          GDay
    3.3.14 gMonth               ✓          GMonth
    3.3.15 hexBinary            ✓          HexBinary
    3.3.16 base64Binary         ✓          Base64Binary
    3.3.17 anyURI               ✓          AnyURI
    3.3.18 QName                ✓          QName
    3.3.19 NOTATION             ✓          NOTATION

3.4 Derived datatypes           Support    Haskell type
    3.4.1  normalizedString     ✓          NormalizedString
    3.4.2  token                ✓          Token
    3.4.3  language             ✓          Language
    3.4.4  NMTOKEN              ✓          NMTOKEN
    3.4.5  NMTOKENS             ✓          NMTOKENS
    3.4.6  Name                 ✓          Name
    3.4.7  NCName               ✓          NCName
    3.4.8  ID                   ✓          ID
    3.4.9  IDREF                ✓          IDREF
    3.4.10 IDREFS               ✓          IDREFS
    3.4.11 ENTITY               ✓          ENTITY
    3.4.12 ENTITIES             ✓          ENTITIES
    3.4.13 integer              ✓          Integer
    3.4.14 nonPositiveInteger   ✓          NonPositiveInteger
    3.4.15 negativeInteger      ✓          NegativeInteger
    3.4.16 long                 ✓          Long
    3.4.17 int                  ✓          Intxs
    3.4.18 short                ✓          Short
    3.4.19 byte                 ✓          Byte
    3.4.20 nonNegativeInteger   ✓          NonNegativeInteger
    3.4.21 unsignedLong         ✓          UnsignedLong
    3.4.22 unsignedInt          ✓          UnsignedInt
    3.4.23 unsignedShort        ✓          UnsignedShort
    3.4.24 unsignedByte         ✓          UnsignedByte
    3.4.25 positiveInteger      ✓          PositiveInteger
    3.4.26 yearMonthDuration    ✓          YearMonthDuration
    3.4.27 dayTimeDuration      ✓          DayTimeDuration
    3.4.28 dateTimeStamp        ✓          DateTimeStampxs

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
⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅
-}

module Regex
where


-- Local Imports

import Lading
import Parsers

-- Explicit Imports

import Data.Ix     (inRange)
import Data.Maybe  (fromJust)
import Text.Read   (read)

-- Qualified Imports

import qualified Data.ByteString        as B
import qualified Data.ByteString.Char8  as B8
import qualified Data.Char              as C
import qualified Data.List              as DL
import qualified Data.Text              as T

-- Undisciplined Imports

import ClassyPrelude hiding (IO)
import Data.Attoparsec.Text
-- import Text.XML hiding (Name)

-- End of Imports
-- -----------------------------------------------------------------------------------------------------------------------------------------------------


data RE = RE [Branch]

data Branch = Branch [Piece]

data Piece = Piece Atom (Maybe Quantifier)

data Atom = Atom

data Quantifier = Quantifier

data NormalChar = NormalChar Char

data CharClass = SingleCharEsc
               | CharClassEsc
               | CharClassExprC CharGroup
               | WildcardEsc

-- | A character class expression (charClassExpr) is a ·character group· surrounded by '[' and ']' characters.                  
data CharClassExpr = CharClassExpr CharGroup

data CharGroup = CharGroup (Either PosCharGroup NegCharGroup) (Maybe CharClassExpr)

data PosCharGroup = PosCharGroup [CharGroupPart]

data NegCharGroup = NegCharGroup PosCharGroup

data CharGroupPart = CharGroupPartSingle SingleChar
                   | CharGroupPartRange
                   | CharGroupPartClass

data SingleChar = SingleCharEsc
                | SingleCharNoEsc



