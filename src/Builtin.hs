{-# Language
    AllowAmbiguousTypes
  , ExistentialQuantification
  , FlexibleInstances
  , GeneralizedNewtypeDeriving
  , MultiParamTypeClasses
  , NegativeLiterals
  , QuasiQuotes
  , ScopedTypeVariables
  , TypeApplications
 #-}
{-|

Module      : Builtin
Copyright   : Robert Lee, © 2017-2022
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


 /$$   /$$  /$$$$$$  /$$$$$$$          /$$         /$$
| $$  / $$ /$$__  $$| $$__  $$       /$$$$       /$$$$
|  $$/ $$/| $$  \__/| $$  \ $$      |_  $$      |_  $$
 \  $$$$/ |  $$$$$$ | $$  | $$        | $$        | $$
  >$$  $$  \____  $$| $$  | $$        | $$        | $$
 /$$/\  $$ /$$  \ $$| $$  | $$        | $$        | $$
| $$  \ $$|  $$$$$$/| $$$$$$$/       /$$$$$$ /$$ /$$$$$$
|__/  |__/ \______/ |_______/       |______/|__/|______/



 /$$   /$$ /$$      /$$ /$$              /$$$$$$            /$$                                                 /$$         /$$
| $$  / $$| $$$    /$$$| $$             /$$__  $$          | $$                                               /$$$$       /$$$$
|  $$/ $$/| $$$$  /$$$$| $$            | $$  \__/  /$$$$$$$| $$$$$$$   /$$$$$$  /$$$$$$/$$$$   /$$$$$$       |_  $$      |_  $$
 \  $$$$/ | $$ $$/$$ $$| $$            |  $$$$$$  /$$_____/| $$__  $$ /$$__  $$| $$_  $$_  $$ |____  $$        | $$        | $$
  >$$  $$ | $$  $$$| $$| $$             \____  $$| $$      | $$  \ $$| $$$$$$$$| $$ \ $$ \ $$  /$$$$$$$        | $$        | $$
 /$$/\  $$| $$\  $ | $$| $$             /$$  \ $$| $$      | $$  | $$| $$_____/| $$ | $$ | $$ /$$__  $$        | $$        | $$
| $$  \ $$| $$ \/  | $$| $$$$$$$$      |  $$$$$$/|  $$$$$$$| $$  | $$|  $$$$$$$| $$ | $$ | $$|  $$$$$$$       /$$$$$$ /$$ /$$$$$$
|__/  |__/|__/     |__/|________/       \______/  \_______/|__/  |__/ \_______/|__/ |__/ |__/ \_______/      |______/|__/|______/

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

module Builtin
    ( Aggregatio (..)
    , AnyAtomicType
    , AnyAtomicTypes (..)
    , AnySimpleType
    , AnySimpleTypes (..)
    , AnyURI             -- Value constructor(s) must not be exported.                                                                               -- ⚡
    , Base64Binary       -- Value constructor(s) must not be exported.                                                                               -- ⚡
    , Boolean            -- Value constructor(s) must not be exported.                                                                               -- ⚡
    , Byte (..)          -- Value constructor is OK to export.
    , Datexs             -- Value constructor(s) must not be exported.                                                                               -- ⚡
    , DateTimexs         -- Value constructor(s) must not be exported.                                                                               -- ⚡
    , DateTimeStampxs    -- Value constructor(s) must not be exported.                                                                               -- ⚡
    , DayTimeDuration    -- Value constructor(s) must not be exported.                                                                               -- ⚡
    , Decimal (..)       -- Value constructor is OK to export.
    , Doublexs (..)      -- Value constructor is OK to export.
    , Durationxs         -- Value constructor(s) must not be exported.                                                                               -- ⚡
    , ENTITIES           -- Value constructor(s) must not be exported.                                                                               -- ⚡
    , ENTITY             -- Value constructor(s) must not be exported.                                                                               -- ⚡
    , ExplicitTimezone(..)
    , FacetC(..)
    , Ords(..)
    , Cardinalities(..)
    , PrimitiveType
    , OtherBuiltinType
    , Floatxs (..)       -- Value constructor is OK to export.
    , GDay               -- Value constructor(s) must not be exported.                                                                               -- ⚡
    , GMonth             -- Value constructor(s) must not be exported.                                                                               -- ⚡
    , GMonthDay          -- Value constructor(s) must not be exported.                                                                               -- ⚡
    , GYear              -- Value constructor(s) must not be exported.                                                                               -- ⚡
    , GYearMonth         -- Value constructor(s) must not be exported.                                                                               -- ⚡
    , HexBinary          -- Value constructor(s) must not be exported.                                                                               -- ⚡
    , ID                 -- Value constructor(s) must not be exported.                                                                               -- ⚡
    , IDREF              -- Value constructor(s) must not be exported.                                                                               -- ⚡
    , IDREFS             -- Value constructor(s) must not be exported.                                                                               -- ⚡
    , Intxs (..)         -- Value constructor is OK to export.
    , Language           -- Value constructor(s) must not be exported.                                                                               -- ⚡
    , Long (..)          -- Value constructor is OK to export.
    , NCName             -- Value constructor(s) must not be exported.                                                                               -- ⚡
    , Name               -- Value constructor(s) must not be exported.                                                                               -- ⚡
    , Names              -- Value constructor(s) must not be exported.                                                                               -- ⚡
    , NegativeInteger    -- Value constructor(s) must not be exported.                                                                               -- ⚡
    , NMTOKEN            -- Value constructor(s) must not be exported.                                                                               -- ⚡
    , NMTOKENS           -- Value constructor(s) must not be exported.                                                                               -- ⚡
    , NOTATION           -- Value constructor(s) must not be exported.                                                                               -- ⚡
    , NonNegativeInteger -- Value constructor(s) must not be exported.                                                                               -- ⚡
    , NonPositiveInteger -- Value constructor(s) must not be exported.                                                                               -- ⚡
    , NormalizedString   -- Value constructor(s) must not be exported.                                                                               -- ⚡
    , PositiveInteger    -- Value constructor(s) must not be exported.                                                                               -- ⚡
    , QName (..)         -- Value constructors are fine as they are dependant on NCName.
    , Res (..)
    , Short (..)         -- Value constructor is OK to export.
    , Stringxs           -- Value constructor(s) must not be exported.                                                                               -- ⚡
    , Timexs             -- Value constructor(s) must not be exported.                                                                               -- ⚡
    , Token              -- Value constructor(s) must not be exported.                                                                               -- ⚡
    , Transformatio (..)
    , UnsignedByte (..)  -- Value constructor is OK to export.
    , UnsignedInt (..)   -- Value constructor is OK to export.
    , UnsignedLong (..)  -- Value constructor is OK to export.
    , UnsignedShort (..) -- Value constructor is OK to export.
    , YearMonthDuration  -- Value constructor(s) must not be exported.                                                                               -- ⚡
    , anchorParser
    , byteParser
    , collapse
    , durationNormalize
    , eitherToMaybe
    , entitiesParser
    , gYearP
    , gMonthP
    , hexBinaryParser
    , iDREFSParser
    , integerParser
    , intxsParser
    , languageParser
    , languagePattern
    , longParser
    , minMax
    , mStringToNS
    , nCNameCharParser
    , nCNameCharPattern
    , nCNameParser
    , nCNamePattern
    , nCNameStartCharParser
    , nCNameStartCharPattern
    , nCNameToENTITY
    , nCNameToID
    , nCNameToIDREF
    , nameCharParser
    , nameCharPattern
    , nameParser
    , namePattern
    , nameStartCharPattern
    , namesParser
    , namesPattern
    , negativeIntegerParser
    , nmtokenParser
    , nmtokenPattern
    , nmtokensParser
    , nmtokensPattern
    , nonNegativeIntegerParser
    , nonNegativeIntegerPattern
    , nonPositiveIntegerParser
    , nsToString
    , parsedP
    , positiveIntegerParser
    , qNameParser
    , qNamePattern
    , shortParser
    , unsignedByteParser
    , unsignedIntParser
    , unsignedLongParser
    , unsignedShortParser
    , xmlWhite
    , yearMonthDayP
    , zeroMax
    , asMany
    , aLPHA
    , absoluteIRI
    , dIGIT
    , decOctet
    , genDelims
    , h16
    , hEXDIG
    , iPliteral
    , iPv4address
    , iPv6address
    , iPvFuture
    , iauthority
    , ifragment
    , ihierPart
    , ihost
    , ipath
    , ipathABEmpty
    , ipathAbsolute
    , ipathNoScheme
    , ipathRootless
    , ipchar
    , iprivate
    , iquery
    , iregName
    , irelativePart
    , irelativeRef
    , iriParser
    , iriReference
    , isegment
    , isegmentNz
    , iunreserved
    , iuserinfo
    , ls32
    , pORT
    , pctEncoded
    , reserved
    , scheme
    , subDelims
    , ucschar
    , unreserved
    -- , doIt
    -- , makeFundies
    )
where

-- Local Imports

import Lading
import Parsers

-- Explicit Imports

import Control.Monad (fail)
import Data.Int    (Int8, Int16)
import Data.Ix     (inRange)
import Data.Maybe  (fromJust)
import Data.String.Here.Uninterpolated (hereLit)
import Data.Word   (Word16)
import Numeric     (showEFloat)
import Text.Read   (read)

-- Qualified Imports
import qualified Data.Hourglass         as H
import qualified Data.ByteString        as B
import qualified Data.ByteString.Base16 as Hex
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8  as B8
import qualified Data.Char              as C
import qualified Data.List              as DL
import qualified Data.Scientific        as SC
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as TE

-- Undisciplined Imports

import ClassyPrelude hiding (IO, String)
import Data.Attoparsec.Text
-- import Text.XML hiding (Name)

-- End of Imports
-- -----------------------------------------------------------------------------------------------------------------------------------------------------

-- The parsers are not forgiving of extra whitespace. Trim and/or collapse strings as required before applying them.                                 -- ⚡
-- The parsers should be anchored before applying them.                                                                                              -- ⚡

-- Classes -----------------------------------------------------------------------------------------------------------------------------------------
                             -- Latin         ➙ English
class Res a b where          -- Res           ➙ thing
  redde :: a -> b            -- redde         ➙ return or give back
  recipe :: b -> Maybe a     -- recipe        ➙ take

class Transformatio a where  -- Transformatio ➙ transform
  scribe :: a -> Text        -- scribe        ➙ write text
  fac :: Text -> Maybe a     -- fac           ➙ create or make
  canon :: a -> Text         -- canon         ➙ canonical
  canon = scribe

class Aggregatio a b where   -- Aggregatio    ➙ Aggregate
  contrahe :: [b] -> Maybe a -- contrahe      ➙ assemble
  divide :: a -> [b]         -- divide        ➙ divide

-- Facets ------------------------------------------------------------------------------------------------------------------------------------------

class AnySimpleType a
instance AnySimpleType AnyURI
instance AnySimpleType Base64Binary
instance AnySimpleType Boolean
instance AnySimpleType Byte
instance AnySimpleType DateTimeStampxs
instance AnySimpleType DateTimexs
instance AnySimpleType Datexs
instance AnySimpleType DayTimeDuration
instance AnySimpleType Decimal
instance AnySimpleType Doublexs
instance AnySimpleType Durationxs
instance AnySimpleType ENTITIES
instance AnySimpleType ENTITY
instance AnySimpleType Floatxs
instance AnySimpleType GDay
instance AnySimpleType GMonth
instance AnySimpleType GMonthDay
instance AnySimpleType GYear
instance AnySimpleType GYearMonth
instance AnySimpleType HexBinary
instance AnySimpleType ID
instance AnySimpleType IDREF
instance AnySimpleType IDREFS
instance AnySimpleType Integer
instance AnySimpleType Intxs
instance AnySimpleType Language
instance AnySimpleType Long
instance AnySimpleType NCName
instance AnySimpleType NMTOKEN
instance AnySimpleType NMTOKENS
instance AnySimpleType NOTATION
instance AnySimpleType Name
instance AnySimpleType NegativeInteger
instance AnySimpleType NonNegativeInteger
instance AnySimpleType NonPositiveInteger
instance AnySimpleType NormalizedString
instance AnySimpleType PositiveInteger
instance AnySimpleType QName
instance AnySimpleType Short
instance AnySimpleType Stringxs
instance AnySimpleType Timexs
instance AnySimpleType Token
instance AnySimpleType UnsignedByte
instance AnySimpleType UnsignedInt
instance AnySimpleType UnsignedLong
instance AnySimpleType UnsignedShort
instance AnySimpleType YearMonthDuration

class AnySimpleType a => AnyAtomicType a
instance AnyAtomicType AnyURI
instance AnyAtomicType Base64Binary
instance AnyAtomicType Boolean
instance AnyAtomicType Byte
instance AnyAtomicType DateTimeStampxs
instance AnyAtomicType DateTimexs
instance AnyAtomicType Datexs
instance AnyAtomicType DayTimeDuration
instance AnyAtomicType Decimal
instance AnyAtomicType Doublexs
instance AnyAtomicType Durationxs
instance AnyAtomicType ENTITY
instance AnyAtomicType Floatxs
instance AnyAtomicType GDay
instance AnyAtomicType GMonth
instance AnyAtomicType GMonthDay
instance AnyAtomicType GYear
instance AnyAtomicType GYearMonth
instance AnyAtomicType HexBinary
instance AnyAtomicType ID
instance AnyAtomicType IDREF
instance AnyAtomicType Integer
instance AnyAtomicType Intxs
instance AnyAtomicType Language
instance AnyAtomicType Long
instance AnyAtomicType NCName
instance AnyAtomicType NMTOKEN
instance AnyAtomicType NOTATION
instance AnyAtomicType Name
instance AnyAtomicType NegativeInteger
instance AnyAtomicType NonNegativeInteger
instance AnyAtomicType NonPositiveInteger
instance AnyAtomicType NormalizedString
instance AnyAtomicType PositiveInteger
instance AnyAtomicType QName
instance AnyAtomicType Short
instance AnyAtomicType Stringxs
instance AnyAtomicType Timexs
instance AnyAtomicType Token
instance AnyAtomicType UnsignedByte
instance AnyAtomicType UnsignedInt
instance AnyAtomicType UnsignedLong
instance AnyAtomicType UnsignedShort
instance AnyAtomicType YearMonthDuration

class AnyAtomicType a => PrimitiveType a
instance PrimitiveType Stringxs
instance PrimitiveType Boolean
instance PrimitiveType Decimal
instance PrimitiveType Floatxs
instance PrimitiveType Doublexs
instance PrimitiveType Durationxs
instance PrimitiveType DateTimexs
instance PrimitiveType Timexs
instance PrimitiveType Datexs
instance PrimitiveType GYearMonth
instance PrimitiveType GYear
instance PrimitiveType GMonthDay
instance PrimitiveType GDay
instance PrimitiveType GMonth
instance PrimitiveType HexBinary
instance PrimitiveType Base64Binary
instance PrimitiveType AnyURI
instance PrimitiveType QName
instance PrimitiveType NOTATION

class AnyAtomicType a => OtherBuiltinType a
instance OtherBuiltinType Byte
instance OtherBuiltinType DateTimeStampxs
instance OtherBuiltinType DayTimeDuration
instance OtherBuiltinType ENTITY
instance OtherBuiltinType ID
instance OtherBuiltinType IDREF
instance OtherBuiltinType Integer
instance OtherBuiltinType Intxs
instance OtherBuiltinType Language
instance OtherBuiltinType Long
instance OtherBuiltinType NCName
instance OtherBuiltinType NMTOKEN
instance OtherBuiltinType Name
instance OtherBuiltinType NegativeInteger
instance OtherBuiltinType NonNegativeInteger
instance OtherBuiltinType NonPositiveInteger
instance OtherBuiltinType NormalizedString
instance OtherBuiltinType PositiveInteger
instance OtherBuiltinType Short
instance OtherBuiltinType Token
instance OtherBuiltinType UnsignedByte
instance OtherBuiltinType UnsignedInt
instance OtherBuiltinType UnsignedLong
instance OtherBuiltinType UnsignedShort
instance OtherBuiltinType YearMonthDuration

type Annotation = () -- Placeholder

-- 4.3.4 pattern -- This is a lexical facet.

newtype Pattern = Pattern Text -- Placeholder

-- 4.3.5 enumeration -- This is a value-based facet.

-- class FacetEnum a base where
--   enumC :: Maybe ([Annotation], [base], (base -> Bool), a) -- Do not evaluate 'a'. It is used for identifying the instance Type.
--   enumC = Nothing -- Placeholder

type Assertion = () -- Placeholder

data ExplicitTimezone
  = TZOffRequired
  | TZOffProhibited
  | TZOffOptional

data Ords
  = OrdFalse
  | OrdPartial
  | OrdTotal
    deriving (Eq, Ord, Show)

data Cardinalities
  = CardFinite
  | CardInfinite
    deriving (Eq, Ord, Show)

data WhiteSpace
  = WSPreserve
  | WSCollapse
  | WSReplace
    deriving (Eq, Ord, Show)

class AnySimpleType a => FacetC a where
  facetOrdC         :: Ords                             -- Value-based facet. See F.1 Fundamental Facets.
  facetBoundedC     :: Bool                             -- Value-based facet. See F.1 Fundamental Facets.
  facetCardinalityC :: Cardinalities                    -- Value-based facet. See F.1 Fundamental Facets.
  facetNumericC     :: Bool                             -- Value-based facet. See F.1 Fundamental Facets.
  facetWhiteSpace   :: (Bool, WhiteSpace, [Annotation]) -- Pre-lexical facet. Bool represents Fixed.

                                       -- Fixed        Value          Validator
  minLength        :: Maybe ([Annotation], Bool, NonNegativeInteger, (a -> Bool)) -- Value-based facet. Bool represents Fixed.
  maxLength        :: Maybe ([Annotation], Bool, NonNegativeInteger, (a -> Bool)) -- Value-based facet. Bool represents Fixed.
  valueLengthC     :: Maybe ([Annotation], Bool, NonNegativeInteger, (a -> Bool)) -- Value-based facet. Must always be True (Spec): Bool represents Fixed.
  explicitTimezone :: Maybe ([Annotation], Bool, ExplicitTimezone  , (a -> Bool)) -- Value-based facet. Bool represents Fixed.
  assertions       :: Maybe ([Annotation],       [Assertion]       , (a -> Bool)) -- Value-based facet.
  fractionDigits   :: Maybe ([Annotation], Bool, NonNegativeInteger, (a -> Bool)) -- Value-based facet. Bool represents Fixed.
  totalDigits      :: Maybe ([Annotation], Bool, PositiveInteger   , (a -> Bool)) -- Value-based facet. Bool represents Fixed.
  minInclusive     :: Maybe ([Annotation], Bool, a                 , (a -> Bool)) -- Value-based facet. Bool represents Fixed.
  minEnclusive     :: Maybe ([Annotation], Bool, a                 , (a -> Bool)) -- Value-based facet. Bool represents Fixed.
  maxInclusive     :: Maybe ([Annotation], Bool, a                 , (a -> Bool)) -- Value-based facet. Bool represents Fixed.
  maxEnclusive     :: Maybe ([Annotation], Bool, a                 , (a -> Bool)) -- Value-based facet. Bool represents Fixed.
  patternC         :: Maybe ([Annotation],       [Pattern]         , (Text -> Bool), a) -- Lexical facet. NB Do not evaluate 'a', type only.

  -- Defaults
  facetWhiteSpace  = (True, WSCollapse, [])
  minLength        = Nothing
  maxLength        = Nothing
  valueLengthC     = Nothing
  explicitTimezone = Nothing
  assertions       = Nothing
  fractionDigits   = Nothing
  totalDigits      = Nothing
  minInclusive     = Nothing
  minEnclusive     = Nothing
  maxInclusive     = Nothing
  maxEnclusive     = Nothing
  patternC         = Nothing


instance FacetC Stringxs
  where facetOrdC         = OrdFalse
        facetBoundedC     = False
        facetCardinalityC = CardInfinite
        facetNumericC     = False
        facetWhiteSpace   = (False, WSPreserve, [])

instance FacetC Boolean
  where facetOrdC         = OrdFalse
        facetBoundedC     = False
        facetCardinalityC = CardFinite
        facetNumericC     = False

instance FacetC Decimal
  where facetOrdC         = OrdPartial
        facetBoundedC     = True
        facetCardinalityC = CardFinite
        facetNumericC     = True

instance FacetC Floatxs
  where facetOrdC         = OrdPartial
        facetBoundedC     = True
        facetCardinalityC = CardFinite
        facetNumericC     = True

instance FacetC Doublexs
  where facetOrdC         = OrdTotal
        facetBoundedC     = False
        facetCardinalityC = CardInfinite
        facetNumericC     = True

instance FacetC Durationxs
  where facetOrdC         = OrdPartial
        facetBoundedC     = False
        facetCardinalityC = CardInfinite
        facetNumericC     = False

instance FacetC DateTimexs
  where facetOrdC         = OrdPartial
        facetBoundedC     = False
        facetCardinalityC = CardInfinite
        facetNumericC     = False
        explicitTimezone  = Just ([], False, TZOffOptional, const False)

instance FacetC Timexs
  where facetOrdC         = OrdPartial
        facetBoundedC     = False
        facetCardinalityC = CardInfinite
        facetNumericC     = False
        explicitTimezone  = Just ([], False, TZOffOptional, const False)

instance FacetC Datexs
  where facetOrdC         = OrdPartial
        facetBoundedC     = False
        facetCardinalityC = CardInfinite
        facetNumericC     = False
        explicitTimezone  = Just ([], False, TZOffOptional, const False)

instance FacetC GYearMonth
  where facetOrdC         = OrdPartial
        facetBoundedC     = False
        facetCardinalityC = CardInfinite
        facetNumericC     = False
        explicitTimezone  = Just ([], False, TZOffOptional, const False)

instance FacetC GYear
  where facetOrdC         = OrdPartial
        facetBoundedC     = False
        facetCardinalityC = CardInfinite
        facetNumericC     = False
        explicitTimezone  = Just ([], False, TZOffOptional, const False)

instance FacetC GMonthDay
  where facetOrdC         = OrdPartial
        facetBoundedC     = False
        facetCardinalityC = CardInfinite
        facetNumericC     = False
        explicitTimezone  = Just ([], False, TZOffOptional, const False)

instance FacetC GDay
  where facetOrdC         = OrdPartial
        facetBoundedC     = False
        facetCardinalityC = CardInfinite
        facetNumericC     = False
        explicitTimezone  = Just ([], False, TZOffOptional, const False)

instance FacetC GMonth
  where facetOrdC         = OrdPartial
        facetBoundedC     = False
        facetCardinalityC = CardInfinite
        facetNumericC     = False
        explicitTimezone  = Just ([], False, TZOffOptional, const False)

instance FacetC HexBinary
  where facetOrdC         = OrdFalse
        facetBoundedC     = False
        facetCardinalityC = CardInfinite
        facetNumericC     = False

instance FacetC Base64Binary
  where facetOrdC         = OrdFalse
        facetBoundedC     = False
        facetCardinalityC = CardInfinite
        facetNumericC     = False

instance FacetC AnyURI
  where facetOrdC         = OrdFalse
        facetBoundedC     = False
        facetCardinalityC = CardInfinite
        facetNumericC     = False

instance FacetC QName
  where facetOrdC         = OrdFalse
        facetBoundedC     = False
        facetCardinalityC = CardInfinite
        facetNumericC     = False

instance FacetC NOTATION
  where facetOrdC         = OrdFalse
        facetBoundedC     = False
        facetCardinalityC = CardInfinite
        facetNumericC     = False

instance FacetC NormalizedString
  where facetOrdC         = OrdFalse
        facetBoundedC     = False
        facetCardinalityC = CardInfinite
        facetNumericC     = False
        facetWhiteSpace   = (False, WSReplace, [])

instance FacetC Token
  where facetOrdC         = OrdFalse
        facetBoundedC     = False
        facetCardinalityC = CardInfinite
        facetNumericC     = False

instance FacetC Language
  where facetOrdC         = OrdFalse
        facetBoundedC     = False
        facetCardinalityC = CardInfinite
        facetNumericC     = False

instance FacetC NMTOKEN
  where facetOrdC         = OrdFalse
        facetBoundedC     = False
        facetCardinalityC = CardInfinite
        facetNumericC     = False

instance FacetC NMTOKENS
  where facetOrdC         = OrdFalse
        facetBoundedC     = False
        facetCardinalityC = CardInfinite
        facetNumericC     = False
        minLength         = Just ([], False, fromJust $ recipe (1 :: Integer), const False)

instance FacetC Name
  where facetOrdC         = OrdFalse
        facetBoundedC     = False
        facetCardinalityC = CardInfinite
        facetNumericC     = False

instance FacetC NCName
  where facetOrdC         = OrdFalse
        facetBoundedC     = False
        facetCardinalityC = CardInfinite
        facetNumericC     = False

instance FacetC ID
  where facetOrdC         = OrdFalse
        facetBoundedC     = False
        facetCardinalityC = CardInfinite
        facetNumericC     = False

instance FacetC IDREF
  where facetOrdC         = OrdFalse
        facetBoundedC     = False
        facetCardinalityC = CardInfinite
        facetNumericC     = False

instance FacetC IDREFS
  where facetOrdC         = OrdFalse
        facetBoundedC     = False
        facetCardinalityC = CardInfinite
        facetNumericC     = False
        minLength         = Just ([], False, fromJust $ recipe (1 :: Integer), const False)

instance FacetC ENTITY
  where facetOrdC         = OrdFalse
        facetBoundedC     = False
        facetCardinalityC = CardInfinite
        facetNumericC     = False

instance FacetC ENTITIES
  where facetOrdC         = OrdFalse
        facetBoundedC     = False
        facetCardinalityC = CardInfinite
        facetNumericC     = False
        minLength         = Just ([], False, NonNegativeInteger 1, const False)

instance FacetC Integer
  where facetOrdC         = OrdTotal
        facetBoundedC     = False
        facetCardinalityC = CardInfinite
        facetNumericC     = True
        fractionDigits    = Just ([], True, NonNegativeInteger 0, const False) -- (a -> Bool))

instance FacetC NonPositiveInteger
  where facetOrdC         = OrdTotal
        facetBoundedC     = False
        facetCardinalityC = CardInfinite
        facetNumericC     = True
        fractionDigits    = Just ([], True,  NonNegativeInteger 0, const False) -- (a -> Bool))
        maxInclusive      = Just ([], False, NonPositiveInteger 0, const False)

instance FacetC NegativeInteger
  where facetOrdC         = OrdTotal
        facetBoundedC     = False
        facetCardinalityC = CardInfinite
        facetNumericC     = True
        fractionDigits    = Just ([], True,  NonNegativeInteger 0, const False) -- (a -> Bool))
        maxInclusive      = Just ([], False, NegativeInteger -1, const False)

instance FacetC Long
  where facetOrdC         = OrdTotal
        facetBoundedC     = True
        facetCardinalityC = CardFinite
        facetNumericC     = True
        fractionDigits    = Just ([], True, NonNegativeInteger 0, const False) -- (a -> Bool))
        maxInclusive      = Just ([], False, Long 9223372036854775807, const False)
        minInclusive      = Just ([], False, Long -9223372036854775808, const False)

instance FacetC Intxs
  where facetOrdC         = OrdTotal
        facetBoundedC     = True
        facetCardinalityC = CardFinite
        facetNumericC     = True
        fractionDigits    = Just ([], True, NonNegativeInteger 0, const False) -- (a -> Bool))
        maxInclusive      = Just ([], False, Intxs  2147483647, const False)
        minInclusive      = Just ([], False, Intxs -2147483648, const False)

instance FacetC Short
  where facetOrdC         = OrdTotal
        facetBoundedC     = True
        facetCardinalityC = CardFinite
        facetNumericC     = True
        fractionDigits    = Just ([], True, NonNegativeInteger 0, const False) -- (a -> Bool))
        maxInclusive      = Just ([], False, Short  32767, const False)
        minInclusive      = Just ([], False, Short -32768, const False)

instance FacetC Byte
  where facetOrdC         = OrdTotal
        facetBoundedC     = True
        facetCardinalityC = CardFinite
        facetNumericC     = True
        fractionDigits    = Just ([], True, NonNegativeInteger 0, const False) -- (a -> Bool))
        maxInclusive      = Just ([], False, Byte  127, const False)
        minInclusive      = Just ([], False, Byte -128, const False)

instance FacetC NonNegativeInteger
  where facetOrdC         = OrdTotal
        facetBoundedC     = False
        facetCardinalityC = CardInfinite
        facetNumericC     = True
        fractionDigits    = Just ([], True, NonNegativeInteger 0, const False) -- (a -> Bool))
        minInclusive      = Just ([], False, NonNegativeInteger 0, const False)

instance FacetC UnsignedLong
  where facetOrdC         = OrdTotal
        facetBoundedC     = True
        facetCardinalityC = CardFinite
        facetNumericC     = True
        fractionDigits    = Just ([], True, NonNegativeInteger 0, const False) -- (a -> Bool))
        maxInclusive      = Just ([], False, UnsignedLong 18446744073709551615, const False)
        minInclusive      = Just ([], False, UnsignedLong 0, const False)

instance FacetC UnsignedInt
  where facetOrdC         = OrdTotal
        facetBoundedC     = True
        facetCardinalityC = CardFinite
        facetNumericC     = True
        fractionDigits    = Just ([], True, NonNegativeInteger 0, const False) -- (a -> Bool))
        maxInclusive      = Just ([], False, UnsignedInt 4294967295, const False)
        minInclusive      = Just ([], False, UnsignedInt 0, const False)

instance FacetC UnsignedShort
  where facetOrdC         = OrdTotal
        facetBoundedC     = True
        facetCardinalityC = CardFinite
        facetNumericC     = True
        fractionDigits    = Just ([], True, NonNegativeInteger 0, const False) -- (a -> Bool))
        maxInclusive      = Just ([], False, UnsignedShort 65535, const False)
        minInclusive      = Just ([], False, UnsignedShort 0, const False)

instance FacetC UnsignedByte
  where facetOrdC         = OrdTotal
        facetBoundedC     = True
        facetCardinalityC = CardFinite
        facetNumericC     = True
        fractionDigits    = Just ([], True, NonNegativeInteger 0, const False) -- (a -> Bool))
        maxInclusive      = Just ([], False, UnsignedByte 255, const False)
        minInclusive      = Just ([], False, UnsignedByte 0, const False)

instance FacetC PositiveInteger
  where facetOrdC         = OrdTotal
        facetBoundedC     = False
        facetCardinalityC = CardInfinite
        facetNumericC     = True
        fractionDigits    = Just ([], True, NonNegativeInteger 0, const False) -- (a -> Bool))
        minInclusive      = Just ([], False, PositiveInteger 1, const False)

instance FacetC YearMonthDuration
  where facetOrdC         = OrdPartial
        facetBoundedC     = False
        facetCardinalityC = CardInfinite
        facetNumericC     = False

instance FacetC DayTimeDuration
  where facetOrdC         = OrdPartial
        facetBoundedC     = False
        facetCardinalityC = CardInfinite
        facetNumericC     = False

instance FacetC DateTimeStampxs
  where facetOrdC         = OrdPartial
        facetBoundedC     = False
        facetCardinalityC = CardInfinite
        facetNumericC     = False
        explicitTimezone  = Just ([], False, TZOffRequired, const False)

-- Types -------------------------------------------------------------------------------------------------------------------------------------------

-- | anyAtomicType is a special ·restriction· of anySimpleType. The ·value· and ·lexical spaces· of anyAtomicType are
-- | the unions of the ·value· and ·lexical spaces· of all the ·primitive· datatypes, and anyAtomicType is their ·base type·.
-- | For further details of anyAtomicType and its representation as a Simple Type Definition, see Built-in Simple Type Definitions (§4.1.6).

data AnyAtomicTypes
  = AA_Base64Binary       Base64Binary
  | AA_Boolean            Boolean
  | AA_Byte               Byte
  | AA_Datexs             Datexs
  | AA_DayTimeDuration    DayTimeDuration
  | AA_Decimal            Decimal
  | AA_Doublexs           Doublexs
  | AA_Durationxs         Durationxs
  | AA_ENTITY             ENTITY
  | AA_Floatxs            Floatxs
  | AA_GDay               GDay
  | AA_GMonth             GMonth
  | AA_GMonthDay          GMonthDay
  | AA_GYear              GYear
  | AA_GYearMonth         GYearMonth
  | AA_HexBinary          HexBinary
  | AA_ID                 ID
  | AA_IDREF              IDREF
  | AA_Integer            Integer
  | AA_Intxs              Intxs
  | AA_Language           Language
  | AA_Long               Long
  | AA_NCName             NCName
  | AA_NMTOKEN            NMTOKEN
  | AA_NOTATION           NOTATION
  | AA_Name               Name
  | AA_NegativeInteger    NegativeInteger
  | AA_NonNegativeInteger NonNegativeInteger
  | AA_NonPositiveInteger NonPositiveInteger
  | AA_NormalizedString   NormalizedString
  | AA_PositiveInteger    PositiveInteger
  | AA_QName              QName
  | AA_Short              Short
  | AA_Stringxs           Stringxs
  | AA_Timexs             Timexs
  | AA_Token              Token
  | AA_UnsignedByte       UnsignedByte
  | AA_UnsignedInt        UnsignedInt
  | AA_UnsignedLong       UnsignedLong
  | AA_UnsignedShort      UnsignedShort
  | AA_YearMonthDuration  YearMonthDuration
    deriving (Eq, Ord, Show)

data AnySimpleTypes
  = AS_Base64Binary       Base64Binary
  | AS_Boolean            Boolean
  | AS_Byte               Byte
  | AS_Datexs             Datexs
  | AS_DayTimeDuration    DayTimeDuration
  | AS_Decimal            Decimal
  | AS_Doublexs           Doublexs
  | AS_Durationxs         Durationxs
  | AS_ENTITIES           ENTITIES
  | AS_ENTITY             ENTITY
  | AS_Floatxs            Floatxs
  | AS_GDay               GDay
  | AS_GMonth             GMonth
  | AS_GMonthDay          GMonthDay
  | AS_GYear              GYear
  | AS_GYearMonth         GYearMonth
  | AS_HexBinary          HexBinary
  | AS_ID                 ID
  | AS_IDREF              IDREF
  | AS_IDREFS             IDREFS
  | AS_Integer            Integer
  | AS_Intxs              Intxs
  | AS_Language           Language
  | AS_Long               Long
  | AS_NCName             NCName
  | AS_NMTOKEN            NMTOKEN
  | AS_NMTOKENS           NMTOKENS
  | AS_NOTATION           NOTATION
  | AS_Name               Name
  | AS_NegativeInteger    NegativeInteger
  | AS_NonNegativeInteger NonNegativeInteger
  | AS_NonPositiveInteger NonPositiveInteger
  | AS_NormalizedString   NormalizedString
  | AS_PositiveInteger    PositiveInteger
  | AS_QName              QName
  | AS_Short              Short
  | AS_Stringxs           Stringxs
  | AS_Timexs             Timexs
  | AS_Token              Token
  | AS_UnsignedByte       UnsignedByte
  | AS_UnsignedInt        UnsignedInt
  | AS_UnsignedLong       UnsignedLong
  | AS_UnsignedShort      UnsignedShort
  | AS_YearMonthDuration  YearMonthDuration
    deriving (Eq, Ord, Show)

-- Plain XML stuff -------------------------------------------------------------------------------------------------------------------------------------
-- Names and Tokens (See https://www.w3.org/TR/REC-xml/#NT-Name)

i_p99P :: Int -> Bool
i_p99P candidate = inRange (0, 999999999) candidate                             -- See 5.4 Partial Implementation of Infinite Datatypes.             -- ⚡

i64_p99P :: Int64 -> Bool
i64_p99P candidate = inRange (0, 999999999) candidate                           -- See 5.4 Partial Implementation of Infinite Datatypes.             -- ⚡

i64_p189P :: Int64 -> Bool
i64_p189P candidate = inRange (0, 999999999999999999) candidate                 -- See 5.4 Partial Implementation of Infinite Datatypes.             -- ⚡

i_p189P :: (Integral i) => i -> Bool -- This works even when Int is 32 bits.
i_p189P = i64_p189P . fromIntegral

newtype Name = Name Text
  deriving (Eq, Ord, Show)

instance Transformatio Name
  where
    scribe (Name text) = text
    fac = parseCollapse nameParser

newtype Names = Names [Name]
  deriving (Eq, Ord, Show)

instance Transformatio Names
  where
    scribe (Names names) = T.intercalate " " $ map scribe names
    fac = parseCollapse namesParser

instance Aggregatio Names Name
  where
    contrahe [] = Nothing
    contrahe x = Just $ Names x
    divide (Names xs) = xs

newtype NMTOKEN = NMTOKEN Text
  deriving (Eq, Ord, Read, Show)

instance Transformatio NMTOKEN
  where
    scribe (NMTOKEN text) = text
    fac = parseCollapse nmtokenParser

newtype NMTOKENS = NMTOKENS [NMTOKEN]
  deriving (Eq, Ord, Read, Show)

instance Transformatio NMTOKENS
  where
    scribe (NMTOKENS nmtoken) = T.intercalate " " $ map scribe nmtoken
    fac = parseCollapse nmtokensParser

instance Aggregatio NMTOKENS NMTOKEN
  where
    contrahe [] = Nothing
    contrahe nmtokenList = Just $ NMTOKENS nmtokenList
    divide (NMTOKENS nmtokenList) = nmtokenList

nameStartCharPattern :: Text
nameStartCharPattern = T.pack
  [hereLit|":"
           | [A-Z] | "_" | [a-z] | [#xC0-#xD6] | [#xD8-#xF6]
           | [#xF8-#x2FF] | [#x370-#x37D] | [#x37F-#x1FFF] | [#x200C-#x200D]
           | [#x2070-#x218F] | [#x2C00-#x2FEF] | [#x3001-#xD7FF] | [#xF900-#xFDCF] | [#xFDF0-#xFFFD]
           | [#x10000-#xEFFFF]
           |]
   -- hereLit is needed because of a bug in ghci which barfs errors (that don't really exist) without hereLit.

nameCharPattern :: Text
nameCharPattern = "NameStartChar | \"-\" | \".\" | [0-9] | #xB7 | [#x0300-#x036F] | [#x203F-#x2040]"

namePattern :: Text
namePattern = "NameStartChar (NameChar)*"

namesPattern :: Text
namesPattern = "Name (#x20 Name)*"

nmtokenPattern :: Text
nmtokenPattern = "(NameChar)+"

nmtokensPattern :: Text
nmtokensPattern = "NMTOKEN (#x20 NMTOKEN)*"

nameParser :: Parser Name
nameParser = do
  startChar <- nameStartCharParser
  nameChars <- many' nameCharParser
  pure . Name . T.pack $ startChar:nameChars

namesParser :: Parser Names
namesParser = do
  name <- nameParser
  names <- extrasParser nameParser
  pure . Names $ name:names

nmtokenParser :: Parser NMTOKEN
nmtokenParser = many1 nameCharParser >>= pure . NMTOKEN . T.pack

nmtokensParser :: Parser NMTOKENS
nmtokensParser = do
  nmtoken <- nmtokenParser
  nmtokens <- extrasParser nmtokenParser
  pure . NMTOKENS $ nmtoken:nmtokens

-- XMLSchema 1.1 stuff --------------------------------------------------------------------------------------------------------------------------------
-- NCN Non Colonized Names  (See https://www.w3.org/TR/xmlschema11-2/, https://www.w3.org/TR/xml-names11/#ns-qualnames)

newtype NCName = NCName Text
  deriving (Eq, Ord, Show)

instance Transformatio NCName
  where scribe (NCName text) = text
        fac = parseCollapse nCNameParser

nCNameStartCharPattern :: Text
nCNameStartCharPattern = "NameStartChar - ':'"

nCNameStartCharParser :: Parser Char
nCNameStartCharParser = do
  candidate <- nameStartCharParser
  if candidate == ':'
  then fail ""
  else pure candidate

nCNameCharPattern :: Text
nCNameCharPattern = "NameChar - ':'"

nCNameCharParser :: Parser Char
nCNameCharParser = do
  candidate <- nameCharParser
  if candidate == ':'
  then fail ""
  else pure candidate

nCNamePattern :: Text
nCNamePattern = "NCNameStartChar NCNameChar*"

nCNameParser :: Parser NCName
nCNameParser = do
  nCNameStartChar <- nCNameStartCharParser
  nCNameChars <- many' nCNameCharParser
  pure . NCName . T.pack $ nCNameStartChar:nCNameChars

type Prefix = NCName
type LocalPart = NCName

data QName
  = PrefixedName Prefix LocalPart
  | UnprefixedName LocalPart
    deriving (Eq, Ord, Show)

instance Transformatio QName where
  fac = parseCollapse qNameParser
  scribe (PrefixedName prefix localpart) = T.intercalate ":" $ map scribe [prefix, localpart]
  scribe (UnprefixedName localpart) = scribe localpart

qNamePattern :: Text
qNamePattern = "QName ::= PrefixedName | UnprefixedName"

qNameParser :: Parser QName
qNameParser =
  do prefix <- nCNameParser
     void $ char ':'
     localPart <- nCNameParser
     pure $ PrefixedName prefix localPart
  <|> (nCNameParser >>= pure . UnprefixedName) -- This will work if the calling parser is anchored on the end.

newtype NOTATION = NOTATION QName
  deriving (Eq, Ord, Show)

instance Transformatio NOTATION where
  fac = fmap NOTATION . parseCollapse qNameParser
  scribe (NOTATION (PrefixedName prefix localpart)) = T.intercalate ":" $ map scribe [prefix, localpart]
  scribe (NOTATION (UnprefixedName localpart)) = scribe localpart

newtype Boolean = Boolean Bool
  deriving (Eq, Ord, Show)

instance Transformatio Boolean
  where
    scribe (Boolean b) = T.toLower $ tShow b
    canon  (Boolean b) = T.toLower $ tShow b -- Explicit canon.
    fac = parseCollapse booleanParser

instance Res Boolean Bool
  where
    redde (Boolean b) = b
    recipe = Just . Boolean

booleanParser :: Parser Boolean
booleanParser =
  (string "true" <|> string "1" >> pure (Boolean True))
  <|>
  (string "false" <|> string "0" >> pure (Boolean False))

newtype Stringxs = Stringxs Text
  deriving (Eq, Ord, Show)

instance Transformatio Stringxs
  where
    scribe (Stringxs text) = text
    fac candidate = case parseOnly (anchorParser stringXMLparser) candidate of -- No Collapse. Stringxs is as is.
                      Left _ -> Nothing
                      Right () -> Just $ Stringxs candidate

stringXMLparser :: Parser ()
stringXMLparser =
  (many1 $ satisfy (\c -> let e = C.ord c
                          in e /= 0xFFFE && e /= 0xFFFF -- NB: Text auto-excludes the surrogate blocks.
                   )
  ) >> pure ()

-- Character Range
-- [2]  Char ::= [#x1-#xD7FF] | [#xE000-#xFFFD] | [#x10000-#x10FFFF]
--        /* any Unicode character, excluding the surrogate blocks, FFFE, and FFFF. */
-- [2a] RestrictedChar ::= [#x1-#x8] | [#xB-#xC] | [#xE-#x1F] | [#x7F-#x84] | [#x86-#x9F]

newtype NormalizedString = NormalizedString Text
  deriving (Eq, Ord, Show)

instance Transformatio NormalizedString
  where
    scribe (NormalizedString text) = text
    fac candidate = case parseOnly (anchorParser stringXMLparser) candidate of
                      Left _ -> Nothing
                      Right () -> Just . NormalizedString $ replaceXmlWhite candidate

newtype Token = Token Text
  deriving (Eq, Ord, Show)

instance Transformatio Token
  where
    scribe (Token text) = text
    fac candidate = case parseOnly (anchorParser stringXMLparser) candidate of
                      Left _ -> Nothing
                      Right () -> Just . Token . collapse $ candidate

newtype ID = ID Text
  deriving (Eq, Ord, Show)

instance Transformatio ID
  where
    scribe (ID text) = text
    fac candidate = (\(NCName text) -> ID text) <$> fac candidate

nCNameToID :: NCName -> ID
nCNameToID (NCName text) = ID text

newtype IDREF = IDREF Text
  deriving (Eq, Ord, Show)

instance Transformatio IDREF
  where
    scribe (IDREF text) = text
    fac candidate = nCNameToIDREF <$> fac candidate

nCNameToIDREF :: NCName -> IDREF
nCNameToIDREF (NCName text) = IDREF text

newtype IDREFS = IDREFS [IDREF]
  deriving (Eq, Ord, Show)

instance Transformatio IDREFS
  where
    scribe (IDREFS idrefs) = T.intercalate " " $ map scribe idrefs
    fac = parseCollapse iDREFSParser

instance Aggregatio IDREFS IDREF
  where
    contrahe [] = Nothing
    contrahe idref = Just $ IDREFS idref
    divide (IDREFS idrefs) = idrefs

iDREFSParser :: Parser IDREFS
iDREFSParser = do
  idref <- nCNameParser >>= pure . nCNameToIDREF
  idrefs <- extrasParser nCNameParser >>= pure . map nCNameToIDREF
  pure . IDREFS $ idref:idrefs

newtype ENTITY = ENTITY Text
  deriving (Eq, Ord, Show)

instance Transformatio ENTITY
  where
    scribe (ENTITY text) = text
    fac candidate = nCNameToENTITY <$> fac candidate

nCNameToENTITY :: NCName -> ENTITY
nCNameToENTITY (NCName text) = ENTITY text

newtype ENTITIES = ENTITIES [ENTITY]
  deriving (Eq, Ord, Show)

instance Transformatio ENTITIES
  where
    scribe (ENTITIES entities) = T.intercalate " " $ map scribe entities
    fac = parseCollapse entitiesParser

instance Aggregatio ENTITIES ENTITY
  where
    contrahe [] = Nothing
    contrahe x = Just $ ENTITIES x
    divide (ENTITIES xs) = xs

entitiesParser :: Parser ENTITIES
entitiesParser = do
  entity <- nCNameParser >>= pure . nCNameToENTITY
  entities <- extrasParser nCNameParser >>= pure . map nCNameToENTITY
  pure . ENTITIES $ entity:entities

newtype HexBinary = HexBinary B.ByteString
  deriving (Eq, Ord, Show)

instance Res HexBinary B.ByteString
  where
    redde (HexBinary bytestring) = bytestring
    recipe = Just . HexBinary

instance Transformatio HexBinary
  where
    scribe (HexBinary bytestring) = TE.decodeUtf8 $ Hex.encode bytestring
    canon = T.toUpper . scribe
    fac = parseCollapse hexBinaryParser

hexBinaryParser :: Parser HexBinary
hexBinaryParser = do
  hexOctets <- many' hexOctet -- can be empty according to Lexical space specification.
  case Hex.decode $ B8.concat hexOctets of
    Right goodHex -> pure $ HexBinary goodHex
    _ -> fail "bad string"
  where
    hexOctet = count 2 (satisfy $ inClass "A-Fa-f0-9") >>= pure . B8.pack

newtype Base64Binary = Base64Binary B.ByteString
  deriving (Eq, Show, Ord)

instance Res Base64Binary B.ByteString
  where
    redde (Base64Binary bytestring) = bytestring
    recipe = Just . Base64Binary

instance Transformatio Base64Binary
  where
    scribe (Base64Binary bytestring)
      = let f :: B.ByteString -> [] B.ByteString -- See: RFC 2045 6.7. (5) Quoted-Printable Content-Transfer-Encoding
            f "" = []
            f bs = let (line,rest) = B.splitAt 76 bs
                   in line:f rest
        in TE.decodeUtf8 . B.intercalate "\r\n" . f $ B64.encode bytestring -- Line output: legal but not canonical.
    canon (Base64Binary bytestring) = TE.decodeUtf8 $ B64.encode bytestring
    fac candidate =
      Base64Binary <$> ( eitherToMaybe . B64.decode . TE.encodeUtf8
                       $ T.filter (\c -> isNothing $ T.find (== c) xmlWhiteTx) candidate
                       )

-- The lexical space of Base64Binary should be more closely inspected.                                                                               -- ⚠

newtype Language = Language Token
  deriving (Eq, Ord, Show)

instance Transformatio Language
  where
    scribe (Language token) = scribe token
    fac candidate = Language <$> parseCollapse languageParser candidate

instance Res Language Token
  where
    redde (Language token) = token
    recipe = fac . canon

languageParser :: Parser Token -- [a-zA-Z]{1,8}(-[a-zA-Z0-9]{1,8})*
languageParser = do
  initial <- takeWhile1 (inClass "a-zA-Z")
  if T.length initial <= 8
  then do
    inits <- many' $ do void $ char '-'
                        initial' <- takeWhile1 (inClass "a-zA-Z0-9")
                        if T.length initial' <= 8
                        then pure initial'
                        else fail "Initial' length too long."
    pure . Token $ T.intercalate "-" (initial:inits)
  else fail "Initial length too long."

languagePattern :: Text
languagePattern = "[a-zA-Z]{1,8}(-[a-zA-Z0-9]{1,8})*"


nonNegativeIntegerPattern :: Text
nonNegativeIntegerPattern = "[\\-+]?[0-9]+"

newtype NonNegativeInteger = NonNegativeInteger Integer
  deriving (Eq, Ord, Show)

instance Transformatio NonNegativeInteger
  where
    scribe (NonNegativeInteger i) = tShow i
    fac = parseCollapse nonNegativeIntegerParser

instance Res NonNegativeInteger Integer
  where
    redde (NonNegativeInteger i) = i
    recipe candidateInteger
      | candidateInteger >= 0 = Just $ NonNegativeInteger candidateInteger
      | otherwise = Nothing

nonNegativeIntegerParser :: Parser NonNegativeInteger
nonNegativeIntegerParser =
  choice [ char '-' >> many1 (char '0') >> pure (NonNegativeInteger 0)
         , (char '+' <|> peekChar') >> digits
         ]
  where
    digits = many1 digit >>= pure . NonNegativeInteger . read -- Parsed read safe.

newtype PositiveInteger = PositiveInteger Integer
  deriving (Eq, Num, Ord, Show)

instance Transformatio PositiveInteger
  where
    scribe (PositiveInteger i) = tShow i
    fac = parseCollapse positiveIntegerParser

instance Res PositiveInteger Integer
  where
    redde (PositiveInteger i) = i
    recipe candidateInteger
      | candidateInteger > 0 = Just $ PositiveInteger candidateInteger
      | otherwise = Nothing

positiveIntegerParser :: Parser PositiveInteger
positiveIntegerParser = do
  digits <- (char '+' <|> peekChar') >> many1 digit
  let candidateInteger :: Integer
      candidateInteger = read digits -- Parsed read safe.
  if candidateInteger > 0
  then pure $ PositiveInteger candidateInteger
  else fail "Non-positive value"

newtype NonPositiveInteger = NonPositiveInteger Integer
  deriving (Eq, Num, Ord, Show)

instance Transformatio NonPositiveInteger
  where
    scribe (NonPositiveInteger i) = tShow i
    fac = parseCollapse nonPositiveIntegerParser

instance Res NonPositiveInteger Integer
  where
    redde (NonPositiveInteger i) = i
    recipe candidateInteger
      | candidateInteger <= 0 = Just $ NonPositiveInteger candidateInteger
      | otherwise = Nothing

nonPositiveIntegerParser :: Parser NonPositiveInteger
nonPositiveIntegerParser = do
  choice [ cinClass "+-" >> many1 (char '0') >> pure (NonPositiveInteger 0)
         , many1 (char '0') >> pure (NonPositiveInteger 0)
         , do void $ char '-'
              digits <- many1 digit
              pure . NonPositiveInteger . read $ "-" ++ digits -- Parsed read safe.
         ]

newtype NegativeInteger = NegativeInteger Integer
  deriving (Eq, Num, Ord, Show)

instance Transformatio NegativeInteger
  where
    scribe (NegativeInteger i) = tShow i
    fac = parseCollapse negativeIntegerParser

instance Res NegativeInteger Integer
  where
    redde (NegativeInteger i) = i
    recipe candidateInteger
      | candidateInteger < 0 = Just $ NegativeInteger candidateInteger
      | otherwise = Nothing

negativeIntegerParser :: Parser NegativeInteger
negativeIntegerParser = do
  void $ char '-'
  digits <- many1 digit
  let candidateInteger :: Integer
      candidateInteger = read $ "-" ++ digits -- Parsed read safe.
  if candidateInteger < 0
  then pure $ NegativeInteger candidateInteger
  else fail "Non-negative value"

instance Transformatio Integer
  where
    scribe i = tShow i
    fac = parseCollapse integerParser

integerParser :: Parser Integer
integerParser = do
  sign <- char '-' <|> char '+' <|> pure '+'
  digits <- many1 digit
  pure . read $ noPlus sign digits -- Parsed read safe.
  where
    noPlus :: Char -> [] Char -> [] Char -- noPlus cures the integral read problem with a + at the beginning of digits.
    noPlus '+' digits = digits
    noPlus sign digits = sign:digits

newtype Long = Long Int64
  deriving (Eq, Num, Ord, Show)

instance Transformatio Long
  where
    scribe (Long i) = tShow i
    fac = parseCollapse longParser

instance Res Long Int64
  where
    redde (Long int64) = int64
    recipe = Just . Long

-- -----------------------------------------------------------------------------------------------------------------------
--
-- Warning:  `read` over FiniteBit numbers will overflow and underflow.
--           Overflow and underflow are not allowed in XML Schema.
--
-- Solution: Use Integer parser and constrain values by bounds.
--
-- -----------------------------------------------------------------------------------------------------------------------

longParser :: Parser Long -- maxinclusive 9223372036854775807, mininclusive -9223372036854775808
longParser = do
  integer <- integerParser
  if inRange (-9223372036854775808,9223372036854775807) integer
  then pure . Long $ fromIntegral integer
  else fail "Out of bounds"

newtype Intxs = Intxs Int32
  deriving (Eq, Num, Ord, Show)

instance Transformatio Intxs
  where
    scribe (Intxs i) = tShow i
    fac = parseCollapse intxsParser

instance Res Intxs Int32
  where
    redde (Intxs int32) = int32
    recipe = Just . Intxs

intxsParser :: Parser Intxs -- maxinclusive 2147483647, mininclusive -2147483648
intxsParser = do
  integer <- integerParser
  if inRange (-2147483648,2147483647) integer
  then pure . Intxs $ fromIntegral integer
  else fail "Out of bounds"

newtype Short = Short Int16
  deriving (Eq, Num, Ord, Show)

instance Transformatio Short
  where
    scribe (Short i) = tShow i
    fac = parseCollapse shortParser

instance Res Short Int16
  where
    redde (Short int16) = int16
    recipe = Just . Short

shortParser :: Parser Short -- maxinclusive 65535, mininclusive -65536
shortParser = do
  integer <- integerParser
  if inRange (-65536,65535) integer
  then pure . Short $ fromIntegral integer
  else fail "Out of bounds"

newtype Byte = Byte Int8
  deriving (Eq, Num, Ord, Show)

instance Transformatio Byte
  where
    scribe (Byte i) = tShow i
    fac = parseCollapse byteParser

instance Res Byte Int8
  where
    redde (Byte int8) = int8
    recipe = Just . Byte

byteParser :: Parser Byte -- maxinclusive 127, mininclusive -128
byteParser = do
  integer <- integerParser
  if inRange (-128, 127) integer
  then pure . Byte $ fromIntegral integer
  else fail "Out of bounds"

newtype UnsignedLong = UnsignedLong Word64
  deriving (Eq, Num, Ord, Show)

instance Transformatio UnsignedLong
  where
    scribe (UnsignedLong i) = tShow i
    fac = parseCollapse unsignedLongParser

instance Res UnsignedLong Word64
  where
    redde (UnsignedLong word64) = word64
    recipe = Just . UnsignedLong

unsignedLongParser :: Parser UnsignedLong -- maxinclusive 18446744073709551615, mininclusive 0
unsignedLongParser = do
  integer <- integerParser
  if inRange (0,18446744073709551615) integer
  then pure . UnsignedLong $ fromIntegral integer
  else fail "Out of bounds"

newtype UnsignedInt = UnsignedInt Word32
  deriving (Eq, Num, Ord, Show)

instance Transformatio UnsignedInt
  where
    scribe (UnsignedInt i) = tShow i
    fac = parseCollapse unsignedIntParser

instance Res UnsignedInt Word32
  where
    redde (UnsignedInt word32) = word32
    recipe = Just . UnsignedInt

unsignedIntParser :: Parser UnsignedInt -- maxinclusive 4294967295, mininclusive 0
unsignedIntParser = do
  integer <- integerParser
  if inRange (0,4294967295) integer
  then pure . UnsignedInt $ fromIntegral integer
  else fail "Out of bounds"

newtype UnsignedShort = UnsignedShort Word16
  deriving (Eq, Num, Ord, Show)

instance Transformatio UnsignedShort
  where
    scribe (UnsignedShort i) = tShow i
    fac = parseCollapse unsignedShortParser

instance Res UnsignedShort Word16
  where
    redde (UnsignedShort word16) = word16
    recipe = Just . UnsignedShort

unsignedShortParser :: Parser UnsignedShort -- maxinclusive 65536, mininclusive 0
unsignedShortParser = do
  integer <- integerParser
  if inRange (0,65536) integer
  then pure . UnsignedShort $ fromIntegral integer
  else fail "Out of bounds"

newtype UnsignedByte = UnsignedByte Word8
  deriving (Eq, Num, Ord, Show)

instance Transformatio UnsignedByte
  where
    scribe (UnsignedByte i) = tShow i
    fac = parseCollapse unsignedByteParser

instance Res UnsignedByte Word8
  where
    redde (UnsignedByte word8) = word8
    recipe = Just . UnsignedByte

unsignedByteParser :: Parser UnsignedByte -- maxinclusive 256, mininclusive 0
unsignedByteParser = do
  integer <- integerParser
  if inRange (0,256) integer
  then pure . UnsignedByte $ fromIntegral integer
  else fail "Out of bounds"

newtype Decimal = Decimal SC.Scientific
  deriving (Eq, Num, Ord, Show)

instance Transformatio Decimal
  where
    scribe (Decimal scientificValue) =
      T.pack . SC.formatScientific SC.Fixed Nothing $ scientificValue
    canon decimal_pp =
      let t = scribe decimal_pp
      in if T.isSuffixOf ".0" t
         then T.dropEnd 2 t -- For integers, the decimal point and fractional part are prohibited.
         else t
    fac = parseCollapse decimalParser

-- | Decimal lexical space pattern: (\+|-)?([0-9]+(\.[0-9]*)?|\.[0-9]+)
decimalParser :: Parser Decimal
decimalParser = do
  sign       <- choice [ char '-' >> pure "-", char '+' >> pure "", pure "" ]
  digits     <- many digit
  decpoint   <- choice [ char '.' >> pure ".", pure "" ]
  fracdigits <- many digit
  guard $ not (null digits && null fracdigits)
  pure . Decimal . read $ sign                                            -- Parsed read safe.
                       ++ (null digits ? "0" $ digits)                    -- Provide a complementary zero to avoid a read error.                     -- ⚡
                       ++ (null fracdigits ? "" $ decpoint ++ fracdigits) -- On empty fracdigits don't present a decimal point lest read error.      -- ⚡

newtype Floatxs = Floatxs Float
  deriving (Eq, Ord, Show)

instance Transformatio Floatxs
  where
    fac = parseCollapse floatxsParser
    scribe (Floatxs f) | isInfinite f && f > 0 = "INF"
                       | isInfinite f && f < 0 = "-INF"
                       | otherwise = tshow f
    canon pp@(Floatxs f)
      | not $ isInfinite f = T.toUpper . T.pack $ (showEFloat Nothing f) "" -- Force the 'e' to be 'E' according to spec.
      | otherwise = scribe pp

-- | Floatxs lexical space pattern: (\+|-)?([0-9]+(\.[0-9]*)?|\.[0-9]+)([Ee](\+|-)?[0-9]+)?|(\+|-)?INF|NaN
--                                    sign?( num+ (   .num*)?|  .num+ )( Ee   sign?  num+)?|  sign?INF|NaN
--                                               ratPar       dotNums    expPar
--                                                     floatxs                              INF        NaN
floatxsParser :: Parser Floatxs
floatxsParser = choice [floatxs, inf, notANumber]
  where
    signPar = choice [ char '+' >> pure "+"
                     , char '-' >> pure "-"
                     , pure "+"
                     ]
    ratPar = do
      whole <- many1 digit
      frac <- choice [ char '.' >> many digit >>= pure . ('.':)
                     , pure ""
                     ]
      pure $ whole ++ (frac == "." ? ".0" $ frac) -- read will fail without a numeral trailing an orphan '.'.
    dotNums = do
      void "."
      digits <- many1 digit
      pure $ '.':digits
    expPar = do
      skip (inClass "Ee")
      sign <- signPar
      digits <- many1 digit
      pure $ "E" ++ (sign == "-" ? "-" $ "") ++ digits
    floatxs = do
      sign <- signPar
      nums <- choice [ ratPar, dotNums ]
      expo <- choice [ expPar , pure "" ]
      let f :: Float
          f = SC.toRealFloat . read $ sign ++ "0" ++ nums ++ expo -- Provide a complementary zero to avoid a read error.                             -- ⚡
      pure $ if f == 0 && sign == "-"
             then Floatxs (-0.0 :: Float)
             else Floatxs f
    inf = do
      sign <- signPar
      void "INF"
      pure . Floatxs $ read (sign == "-" ? "-Infinity" $ "Infinity") -- Parsed read safe.
    notANumber = "NaN" >> (pure . Floatxs $ read "NaN")              -- Parsed read safe.

-- Doublexs is the same code as Floatxs except for minor Double vs Float type annotations.
newtype Doublexs = Doublexs Double
  deriving (Eq, Ord, Show)

instance Transformatio Doublexs
  where
    fac = parseCollapse doublexsParser
    scribe (Doublexs f) | isInfinite f && f > 0 = "INF"
                        | isInfinite f && f < 0 = "-INF"
                        | otherwise = tshow f
    canon pp@(Doublexs f) | not $ isInfinite f = T.toUpper . T.pack $ (showEFloat Nothing f) "" -- Force the 'e' to be 'E' according to spec.
                          | otherwise = scribe pp

-- | Doublexs lexical space pattern: (\+|-)?([0-9]+(\.[0-9]*)?|\.[0-9]+)([Ee](\+|-)?[0-9]+)?|(\+|-)?INF|NaN
--                                    sign?( num+ (   .num*)?|  .num+ )( Ee   sign?  num+)?|  sign?INF|NaN
--                                               ratPar       dotNums    expPar
--                                                     doublexs                              INF        NaN
doublexsParser :: Parser Doublexs
doublexsParser = choice [doublexs, inf, notANumber]
  where
    signPar = choice [ char '+' >> pure "+"
                     , char '-' >> pure "-"
                     , pure "+"
                     ]
    ratPar = do
      whole <- many1 digit
      frac <- choice [ char '.' >> many digit >>= pure . ('.':)
                     , pure ""
                     ]
      pure $ whole ++ (frac == "." ? ".0" $ frac) -- read will fail without a numeral trailing an orphan '.'.
    dotNums = do
      void "."
      digits <- many1 digit
      pure $ '.':digits
    expPar = do
      skip (inClass "Ee")
      sign <- signPar
      digits <- many1 digit
      pure $ "E" ++ (sign == "-" ? "-" $ "") ++ digits
    doublexs = do
      sign <- signPar
      nums <- choice [ ratPar, dotNums ]
      expo <- choice [ expPar , pure "" ]
      let f :: Double
          f = SC.toRealFloat . read $ sign ++ "0" ++ nums ++ expo -- Provide a complementary zero to avoid a read error.                             -- ⚡
      pure $ if f == 0 && sign == "-"
             then Doublexs (-0.0 :: Double)
             else Doublexs f
    inf = do
      sign <- signPar
      void "INF"
      pure . Doublexs $ read (sign == "-" ? "-Infinity" $ "Infinity") -- Parsed read safe.
    notANumber = "NaN" >> (pure . Doublexs $ read "NaN")              -- Parsed read safe.

-- Durationxs ---------------------------------------------------------------------------------------------------------------------------------------

periodpIntP :: Int -> Bool -- Both periodpIntP and durMaxN must change to 32 bit compatible versions when arch is 32
periodpIntP | maxBound @Int > 2 ^ 31 = i_p189P
            | otherwise = i_p99P

periodpInt64P :: Int64 -> Bool
periodpInt64P | maxBound @Int > 2 ^ 31 = i64_p189P
              | otherwise = i64_p99P

periodMaxN :: Int
periodMaxN | maxBound @Int > 2 ^ 31 = 18
           | otherwise = 9

-- defaultDurationxs :: Durationxs
-- defaultDurationxs = Durationxs False (H.Period 0 0 0) (H.Duration 0 0 0 0)

data Durationxs = Durationxs Bool H.Period H.Duration
  deriving (Ord, Show) -- NB Eq instance manually defined below.

instance Res Durationxs (Bool, H.Period, H.Duration)
  where
    redde (Durationxs n p d) = (n, p, d)
    recipe ( n
           , period@(H.Period y m d)
           , duration@(H.Duration (H.Hours h) (H.Minutes mn) (H.Seconds s) (H.NanoSeconds ns))
           )
      |    periodpIntP y && periodpIntP m && periodpIntP d
        && periodpInt64P h && periodpInt64P mn && periodpInt64P s && i64_p99P ns -- See 5.4 Partial Implementation of Infinite Datatypes.            -- ⚡
         = Just $ Durationxs n period duration
      | otherwise = Nothing

instance Res Durationxs Durationxs -- This is used internally by the parser.
  where
    redde durationxs = durationxs
    recipe (Durationxs n
              period@(H.Period y m d)
              duration@(H.Duration (H.Hours h) (H.Minutes mn) (H.Seconds s) (H.NanoSeconds ns))
           )
      |    periodpIntP y
        && periodpIntP m
        && periodpIntP d
        && periodpInt64P h
        && periodpInt64P mn
        && periodpInt64P s
        && i64_p99P ns
         = Just $ Durationxs n period duration -- See 5.4 Partial Implementation of Infinite Datatypes.                                              -- ⚡
      | otherwise = Nothing

instance Eq Durationxs
  where
    (==) a b = let (Durationxs negativePA periodA durationA) = durationNormalize a
                   (Durationxs negativePB periodB durationB) = durationNormalize b
               in    negativePA == negativePB
                  && periodA    == periodB
                  && durationA  == durationB

instance Transformatio Durationxs
  where
    fac = parseCollapse durationxsParser
    scribe (Durationxs negativeP p d) =
      case T.append (negativeP ? "-P" $ "P") . T.concat $ map mf listing of
        "P" -> "P0Y"
        "-P" -> "-P0Y"
        whatever -> whatever
      where
        listing = [ ("Y", H.periodYears p)
                  , ("M", H.periodMonths p)
                  , ("D", H.periodDays p)
                  , ("T", fromIntegral (H.durationHours d)
                        + fromIntegral (H.durationMinutes d)
                        + fromIntegral (H.durationSeconds d)
                        + fromIntegral ((\(H.NanoSeconds i) -> i) $ H.durationNs d))
                  , ("H", fromIntegral $ H.durationHours d)
                  , ("N", fromIntegral $ H.durationMinutes d)
                  , ("S", fromIntegral (H.durationSeconds d)
                        + fromIntegral ((\(H.NanoSeconds i) -> i) $ H.durationNs d)
                    )
                  ]
        mf ("T",0) = ""
        mf ("T",_) = "T"
        mf ("S", 0) = ""
        mf ("S", _) = if ((\(H.NanoSeconds i) -> i) $ H.durationNs d) > 0
                      then T.concat [ tshow $ (fromIntegral $ H.durationSeconds d :: Int)
                                    , T.pack . drop 1 . nsToString $ H.durationNs d
                                    , "S"
                                    ]
                      else T.append (tshow $ (fromIntegral $ H.durationSeconds d :: Int)) "S"
        mf ("M",0) = if H.periodDays p /= 0 && H.periodYears p /= 0
                     then "0M" -- Gaps are not allowed: P1Y2D is illegal. P1Y0M2D is legal.
                     else ""   -- P2D is legal as is P0M2D.
        mf ("N",0) = if H.durationHours d /= 0 && (H.durationSeconds d /= 0 || H.durationNs d /= 0)
                     then "0M" -- Gaps are not allowed: PT1H2S is illegal. PT1H0M2S is legal.
                     else ""   -- PT2S is allowed as is PT0M2S.
        mf (_,0) = ""
        mf ("N",i) = T.append (tshow i) "M"
        mf (l,i) = T.append (tshow i) l

    canon = scribe . durationNormalize

durationNormalize :: Durationxs -> Durationxs
durationNormalize (Durationxs negativeP H.Period {..} H.Duration {..}) =
  Durationxs negativeP
             (H.Period (fromIntegral $ snd finalYear)
                       (fromIntegral $ snd yearMonth)
                       (fromIntegral $ snd monthDay))
             (H.Duration (H.Hours       $ snd dayHour)
                         (H.Minutes     $ snd hourMin)
                         (H.Seconds     $ snd minSec)
                         (H.NanoSeconds $ snd secNs))
  where
    ns, secs, mins, hours, days, months, years :: Int64
    ns     = (\(H.NanoSeconds i) -> i) durationNs
    secs   = (\(H.Seconds     i) -> i) durationSeconds
    mins   = (\(H.Minutes     i) -> i) durationMinutes
    hours  = (\(H.Hours       i) -> i) durationHours
    days   = fromIntegral              periodDays
    months = fromIntegral              periodMonths
    years  = fromIntegral              periodYears

    -- (overflow to next larger unit, remainder in same unit)
    secNs, minSec, hourMin, dayHour, monthDay, yearMonth, finalYear :: (Int64, Int64)
    secNs     = (0, ns) -- ns is constrained and should never overflow.
    minSec    = quotRem (secs   + fst secNs   ) 60
    hourMin   = quotRem (mins   + fst minSec  ) 60
    dayHour   = quotRem (hours  + fst hourMin ) 24
    monthDay  = (0, days + fst dayHour) -- Days never increment months canonically. Ambiguous number of days in a month.
    yearMonth = quotRem months 12       -- Days play no role in incrementing months canonically.
    finalYear = (0, years + fst yearMonth)

mStringToNS :: Chars -> Maybe H.NanoSeconds
mStringToNS ""                  = Just $ H.NanoSeconds 0
mStringToNS ('.':'.':_)         = Nothing
mStringToNS ('0':'.':'.':_)     = Nothing
mStringToNS ('.':'0':'.':_)     = Nothing
mStringToNS ('0':'.':'0':'.':_) = Nothing
mStringToNS ('.':xs)            = mStringToNS xs
mStringToNS ('0':'.':xs)        = mStringToNS xs
mStringToNS pp
  | length pp > 9 = Nothing -- extra numerals beyond length 9 are not in spec for H.NanoSeconds
  | otherwise = do
      num <- readMay pp :: Maybe Int64
      pure $ H.NanoSeconds $ num * 10 ^ (9 - DL.length pp)

nsToString :: H.NanoSeconds -> Chars
nsToString (H.NanoSeconds 0) = "0.0"
nsToString (H.NanoSeconds ns) =
  "0." ++ (DL.dropWhileEnd ('0'==) $ (replicate (9 - num) '0' ++ clean))
  where
    num = length clean :: Int
    clean = DL.take 9 $ show ns -- extra numerals beyond length 9 are not in spec for H.NanoSeconds, purge them.

-- defaultDurationxs :: Durationxs
-- defaultDurationxs = Durationxs False (H.Period 0 0 0) (H.Duration 0 0 0 0)

durationxsParser :: Parser Durationxs
durationxsParser = do
  negativeP <- (optional "-") >>= pure . isJust -- Negative predicate
  void "P"                                      -- Mandatory P
  candidate <- choice [ymdhmsDuration, hmsDuration]
           >>= pure . (\(Durationxs _ period duration) -> Durationxs negativeP period duration)
  guard $ isJust (recipe @Durationxs $ durationNormalize candidate)
  pure candidate
  where
    ymdhmsDuration = do
      (period,duration) <- ymdhmsduration'
      pure $ Durationxs False (periodFac period) (durationFac duration)

    hmsDuration = do
      duration <- hmsDuration'
      pure $ Durationxs False (H.Period 0 0 0) (durationFac duration)

    periodFac (y,m,d') =
      H.Period (y  == "" ? 0 $ read y ) -- Parsed read safe
               (m  == "" ? 0 $ read m ) -- Parsed read safe
               (d' == "" ? 0 $ read d') -- Parsed read safe
    durationFac (h,m,s,fracs') =
      H.Duration (H.Hours   (h == "" ? 0 $ read h)) -- Parsed read safe
                 (H.Minutes (m == "" ? 0 $ read m)) -- Parsed read safe
                 (H.Seconds (s == "" ? 0 $ read s)) -- Parsed read safe
                 (unsafeStringToNS fracs')

    ymdhmsduration' = do
      ymd' <- choice [ ymd
                     , md >>= pure . (\(m,d') -> ("",m ,d'))
                     , d  >>= pure . (\   d'  -> ("","",d'))
                     ]
      hmsf' <- hmsDuration' <|> pure ("","","","")
      pure (ymd', hmsf')
    ymd = do
      digits <- many1 digit
      guard (length digits <= periodMaxN) -- See 5.4 Partial Implementation of Infinite Datatypes.                                                   -- ⚡
      void "Y"
      (m',d') <- md <|> pure ("","")
      pure (digits,m',d')
    md = do
      digits <- many1 digit
      guard (length digits <= periodMaxN) -- See 5.4 Partial Implementation of Infinite Datatypes.                                                   -- ⚡
      void "M"
      d' <- d <|> pure ""
      pure (digits, d')
    d = do
      digits <- many1 digit
      guard (length digits <= periodMaxN) -- See 5.4 Partial Implementation of Infinite Datatypes.                                                   -- ⚡
      "D" >> pure digits

    hmsDuration' = do
      void "T" -- Mandatory T
      choice [ hmsf
             , msf >>= pure . (\(m,s,fracs') -> ("",m ,s,fracs'))
             , sf  >>= pure . (\(  s,fracs') -> ("","",s,fracs'))
             ]
    hmsf = do
      digits <- many1 digit
      guard (length digits < 19) -- See 5.4 Partial Implementation of Infinite Datatypes.                                                            -- ⚡
      void "H"
      (m,s,fracs') <- msf <|>  pure ("","","")
      pure (digits,m,s,fracs')
    msf = do
      digits <- many1 digit
      guard (length digits < 19) -- See 5.4 Partial Implementation of Infinite Datatypes.                                                            -- ⚡
      void "M"
      (s,fracs') <- sf <|>  pure ("","")
      pure (digits, s, fracs')
    sf = do
      digits <- many1 digit
      guard (length digits < 19) -- See 5.4 Partial Implementation of Infinite Datatypes.                                                            -- ⚡
      fracs' <- fracs <|>  pure ""
      void "S"
      pure (digits, fracs')
    fracs = do
      void "."
      digits <- many1 digit
      guard (length digits < 10) -- See 5.4 Partial Implementation of Infinite Datatypes.                                                            -- ⚡
      pure digits
    unsafeStringToNS = fromJust . mStringToNS

newtype YearMonthDuration = YearMonthDuration Durationxs
  deriving (Eq, Ord, Show)

instance Transformatio YearMonthDuration
  where
    fac candidate = do
      ymd <- parseCollapse durationxsParser $ candidate
      res <- recipe ymd
      pure res
    scribe (YearMonthDuration durationxs) = scribe durationxs
    canon  (YearMonthDuration durationxs) = canon  durationxs

instance Res YearMonthDuration Durationxs
  where
    redde (YearMonthDuration durationxs) = durationxs
    recipe durationxs@(Durationxs _ (H.Period y m 0) (H.Duration 0 0 0 0))
      |    periodpIntP y
        && periodpIntP m
        = Just $ YearMonthDuration durationxs -- See 5.4 Partial Implementation of Infinite Datatypes.                                               -- ⚡
      | otherwise = Nothing
    recipe _ = Nothing

instance Res YearMonthDuration (Bool, H.Period)
  where
    redde (YearMonthDuration (Durationxs b period _)) = (b,period)
    recipe (b, p@(H.Period y m 0))
      | periodpIntP y && periodpIntP m
          = Just $ YearMonthDuration (Durationxs b p (H.Duration 0 0 0 0)) -- See 5.4 Partial Implementation of Infinite Datatypes.                  -- ⚡
      | otherwise = Nothing
    recipe _ = Nothing

newtype DayTimeDuration = DayTimeDuration Durationxs
  deriving (Eq, Ord, Show)

instance Transformatio DayTimeDuration
  where
    fac candidate = do
      dtdur <- parseCollapse durationxsParser $ candidate
      res <- recipe dtdur
      pure res
    scribe (DayTimeDuration durationxs) = scribe durationxs
    canon  (DayTimeDuration durationxs) = canon  durationxs

instance Res DayTimeDuration Durationxs
  where
    redde (DayTimeDuration durationxs) = durationxs
    recipe durationxs@(Durationxs _
                         (H.Period 0 0 d)
                         (H.Duration
                             (H.Hours h)
                             (H.Minutes m)
                             (H.Seconds s)
                             (H.NanoSeconds ns)
                         )
                      )
      |    periodpIntP d
        && periodpInt64P h
        && periodpInt64P m
        && periodpInt64P s
        && i64_p99P ns
        = Just $ DayTimeDuration durationxs -- See 5.4 Partial Implementation of Infinite Datatypes.                                                 -- ⚡
      | otherwise = Nothing
    recipe _ = Nothing

-- Gregorian date types ------------------------------------------------------------------------------------------------------------------------------

-- pairAlts :: [] (Parser a, Parser a) -> Parser [a] -- Helper function for those two something parsers.
-- pairAlts [] = pure []
-- pairAlts parsers = choice $ map parse2 parsers

tzOffParser :: Parser H.TimezoneOffset -- Lexical Representation   (0[0-9]|1[0-3]):[0-5][0-9]|14:00
tzOffParser = do
  (s,h,m) <- zulu <|> signedTz
  pure H.TimezoneOffset { timezoneOffsetToMinutes = (s == '-' ? negate $ id) (read h * 60 + read m) } -- Parsed read safe.
  where
    zulu = char 'Z' >> pure ('+',"00","00")

    signedTz = do
      sign <- char '+' <|> char '-'
      (h,m) <- hm <|> fourteen
      pure (sign, h, m)

    fourteen = string "14:00" >> pure ("14","00")

    hm = do
      h <- hour
      void $ char ':'
      m <- minute
      pure (h,m)

    hour = parse2 (char '0' , digit         )
       <|> parse2 (char '1' , cinClass "0-3")

    minute = do
      firstDigit <- cinClass "0-5"
      secondDigit <- digit
      pure [firstDigit, secondDigit]

gYearP :: Int -> Bool
gYearP year
  | year < 0 = periodpIntP $ negate year
  | otherwise = periodpIntP year -- See 5.4 Partial Implementation of Infinite Datatypes.                                                            -- ⚡

gMonthP :: Int -> Bool
gMonthP candidate = inRange (1,12) candidate

gDayP :: Int -> Bool
gDayP candidate = inRange (1,31) candidate

gTzOffP :: Maybe H.TimezoneOffset -> Bool
gTzOffP Nothing = True
gTzOffP (Just H.TimezoneOffset {..}) =
  inRange (-840, 840) timezoneOffsetToMinutes

-- --------------------------------------------------------------------------------------------------------------------------------------------------
-- GYear stanzas

data GYear =
  GYear { gYear :: Int
        , gYearTzOff :: Maybe H.TimezoneOffset
        }
  deriving (Eq, Ord, Show)

instance Transformatio GYear
  where
    fac candidate = parseCollapse gYearParser $ candidate
    scribe (GYear year Nothing) = yearTx year
    scribe (GYear year (Just H.TimezoneOffset {..}))
      = T.append (yearTx year) (tzTx timezoneOffsetToMinutes)

instance Res GYear (Int, Maybe H.TimezoneOffset)
  where
    redde (GYear year mTzOff) = (year, mTzOff)
    recipe (year, mTzOff)
      | gYearP year && gTzOffP mTzOff = Just $ GYear year mTzOff
      | otherwise = Nothing

yearTx :: Int -> Text
yearTx year
  | year < 0  = T.pack $ '-' : preFillWith '0' 4 (year * -1)
  | otherwise = T.pack $       preFillWith '0' 4 year

-- | tzTx is equivalent to ·timezoneCanonicalFragmentMap· (t) → timezoneFrag in E.3.6 Canonical Mappings.
tzTx :: Int -> Text
tzTx 0 = "Z" -- This is canonical. See ·timezoneCanonicalFragmentMap· in E.3.6 Canonical Mappings.
tzTx tzMinutes
  | tzMinutes < 0 = T.append "-" . T.drop 1 $ tzTx (tzMinutes * -1)
  | otherwise =
      let (h,m) = quotRem tzMinutes 60
          hs = preFillWith '0' 2 h
          ms = preFillWith '0' 2 m
      in T.pack $ concat ["+", hs, ":", ms]

gYearParser :: Parser GYear -- Lexical Representation   -?([1-9][0-9]{3,}|0[0-9]{3})(Z|(\+|-)((0[0-9]|1[0-3]):[0-5][0-9]|14:00))?
gYearParser = do
  negativeP <- (optional "-") >>= pure . isJust -- Negative predicate
  firstDigit <- digit
  yDigits <- many1 digit >>= pure . (firstDigit:)
  guard $ not (firstDigit == '0' && length yDigits > 4)
       && inRange (4,periodMaxN) (length yDigits) -- See 5.4 Partial Implementation of Infinite Datatypes.                                           -- ⚡
  mTzOff <- optional tzOffParser
  pure GYear { gYear = (negativeP ? negate $ id) $ read yDigits -- Parsed read safe
             , gYearTzOff = mTzOff
             }

-- --------------------------------------------------------------------------------------------------------------------------------------------------
-- GMonth stanzas

data GMonth =
  GMonth { gMonth      :: Int
         , gMonthTzOff :: Maybe H.TimezoneOffset
         }
  deriving (Show, Ord, Eq)

instance Transformatio GMonth
  where
    fac candidate = parseCollapse gMonthParser $ candidate
    scribe (GMonth month Nothing) = monthTx month
    scribe (GMonth month (Just H.TimezoneOffset {..}))
      = T.append (monthTx month) (tzTx timezoneOffsetToMinutes)

instance Res GMonth (Int, Maybe H.TimezoneOffset)
  where
    redde (GMonth month mTzOff) = (month, mTzOff)
    recipe (month, mTzOff)
      | gMonthP month && gTzOffP mTzOff = Just $ GMonth month mTzOff
      | otherwise = Nothing

monthTx :: Int -> Text
monthTx month = T.pack $ "--" ++ preFillWith '0' 2 month

gMonthParser :: Parser GMonth -- Lexical Representation   --(0[1-9]|1[0-2])(Z|(\+|-)((0[0-9]|1[0-3]):[0-5][0-9]|14:00))?
gMonthParser = do
  void "--"
  mDigits <- parse2 (char '0' , cinClass "1-9")
         <|> parse2 (char '1' , cinClass "0-2")
  mTzOff <- optional tzOffParser
  pure GMonth { gMonth = read mDigits -- Parsed read safe
              , gMonthTzOff = mTzOff
              }

-- --------------------------------------------------------------------------------------------------------------------------------------------------
-- GDay stanzas

data GDay
  = GDay { gDay :: Int
         , gDayTzOff :: Maybe H.TimezoneOffset
         }
  deriving (Eq, Ord, Show)

instance Transformatio GDay
  where
    fac candidate = parseCollapse gDayParser $ candidate
    scribe (GDay day Nothing) = dayTx day
    scribe (GDay day (Just H.TimezoneOffset {..}))
      = T.append (dayTx day) (tzTx timezoneOffsetToMinutes)

instance Res GDay (Int, Maybe H.TimezoneOffset)
  where
    redde (GDay day mTzOff) = (day, mTzOff)
    recipe (day, mTzOff)
      | gDayP day && gTzOffP mTzOff = Just $ GDay day mTzOff
      | otherwise = Nothing

dayTx :: Int -> Text
dayTx day = T.pack $ "---" ++ preFillWith '0' 2 day

gDayParser :: Parser GDay -- Lexical Representation   ---(0[1-9]|[12][0-9]|3[01])(Z|(\+|-)((0[0-9]|1[0-3]):[0-5][0-9]|14:00))?
gDayParser = do
  void "---"
  digits <- parse2 (char     '0'  , cinClass "1-9")
        <|> parse2 (cinClass "12" , digit         )
        <|> parse2 (char     '3'  , cinClass "01" )

  mTzOff <- optional tzOffParser
  pure GDay { gDay = read digits -- Parsed read safe.
            , gDayTzOff = mTzOff
            }

-- --------------------------------------------------------------------------------------------------------------------------------------------------
-- GYearMonth stanzas

data GYearMonth =
  GYearMonth { gYMYear         :: Int -- The year can be negative
             , gYMonth         :: Int -- The month is never negative
             , gYearMonthTzOff :: Maybe H.TimezoneOffset
             }
  deriving (Eq, Ord, Show)

instance Transformatio GYearMonth
  where
    fac candidate = parseCollapse gYearMonthParser $ candidate
    scribe (GYearMonth year month Nothing) = T.concat [yMYearTx year, "-", yMonthTx month]
    scribe (GYearMonth year month (Just H.TimezoneOffset {..}))
      = T.append (scribe $ GYearMonth year month Nothing)
                 (tzTx timezoneOffsetToMinutes)

instance Res GYearMonth (Int, Int, Maybe H.TimezoneOffset)
  where
    redde (GYearMonth year month mTzOff) = (year, month, mTzOff)
    recipe (year, month, mTzOff)
      |    gYearP year
        && gMonthP month
        && gTzOffP mTzOff
        = Just $ GYearMonth year month mTzOff
      | otherwise = Nothing

yMYearTx :: Int -> Text
yMYearTx year
  | year < 0  = T.pack $ '-' : preFillWith '0' 4 (year * (-1))
  | otherwise = T.pack $ preFillWith '0' 4 year

yMonthTx :: Int -> Text
yMonthTx month = T.pack $ preFillWith '0' 2 month

gYearMonthParser :: Parser GYearMonth -- Lexical Representation -?([1-9][0-9]{3,}|0[0-9]{3})-(0[1-9]|1[0-2])(Z|(\+|-)((0[0-9]|1[0-3]):[0-5][0-9]|14:00))?
gYearMonthParser = do
  negativeP <- (optional "-") >>= pure . isJust -- Negative predicate
  firstDigit <- digit
  yDigits <- many1 digit >>= pure . (firstDigit:)
  guard $ not (firstDigit == '0' && length yDigits > 4)
       && inRange (4,periodMaxN) (length yDigits) -- See 5.4 Partial Implementation of Infinite Datatypes.                                           -- ⚡
  void "-"
  mDigits <- parse2 (char '0' , cinClass "1-9")
         <|> parse2 (char '1' , cinClass "0-2")
  mTzOff <- optional tzOffParser
  pure GYearMonth { gYMYear = (negativeP ? negate $ id) $ read yDigits -- Parsed read safe.
                  , gYMonth = read mDigits                             -- Parsed read safe.
                  , gYearMonthTzOff = mTzOff
                  }

data GMonthDay =
  GMonthDay { gMDMonth       :: Int
            , gMDay          :: Int
            , gMonthDayTzOff :: Maybe H.TimezoneOffset
            }
  deriving (Eq, Ord, Show)

instance Transformatio GMonthDay
  where
    fac candidate = parseCollapse gMonthDayParser $ candidate
    scribe (GMonthDay month day Nothing) =
      T.concat [ "--"
               , T.pack $ preFillWith '0' 2 month
               , "-"
               , T.pack $ preFillWith '0' 2 day
               ]
    scribe (GMonthDay month day (Just H.TimezoneOffset {..})) =
      T.append (scribe $ GMonthDay month day Nothing)
               (tzTx timezoneOffsetToMinutes)

instance Res GMonthDay (Int, Int, Maybe H.TimezoneOffset)
  where
    redde (GMonthDay month day mTzOff) = (month, day, mTzOff)
    recipe (month, day, mTzOff)
      |    gMonthDayP month day
        && gTzOffP mTzOff
        = Just $ GMonthDay month day mTzOff
      | otherwise = Nothing

gMonthDayParser :: Parser GMonthDay -- Lexical Representation --(0[1-9]|1[0-2])-(0[1-9]|[12][0-9]|3[01])(Z|(\+|-)((0[0-9]|1[0-3]):[0-5][0-9]|14:00))?
gMonthDayParser = do
  void "--"
  mDigits <- parse2 (char '0' , cinClass "1-9")
         <|> parse2 (char '1' , cinClass "0-2")
  void "-"
  dDigits <- parse2 (char     '0'  , cinClass "1-9")
         <|> parse2 (cinClass "12" , digit         )
         <|> parse2 (char     '3'  , cinClass "01" )
  mTzOff <- optional tzOffParser
  let month = read mDigits -- Parsed read safe.
      day   = read dDigits -- Parsed read safe.
  guard $ gMonthDayP month day
  pure GMonthDay { gMDMonth = month
                 , gMDay = day
                 , gMonthDayTzOff = mTzOff
                 }

gMonthDayP :: Int -> Int -> Bool
gMonthDayP  1 day = inRange (1,31) day -- January
gMonthDayP  2 day = inRange (1,29) day -- February
gMonthDayP  3 day = inRange (1,31) day -- March
gMonthDayP  4 day = inRange (1,30) day -- April
gMonthDayP  5 day = inRange (1,31) day -- May
gMonthDayP  6 day = inRange (1,30) day -- June
gMonthDayP  7 day = inRange (1,31) day -- July
gMonthDayP  8 day = inRange (1,31) day -- August
gMonthDayP  9 day = inRange (1,30) day -- September
gMonthDayP 10 day = inRange (1,31) day -- October
gMonthDayP 11 day = inRange (1,30) day -- November
gMonthDayP 12 day = inRange (1,31) day -- December
gMonthDayP _ _ = False

-- --------------------------------------------------------------------------------------------------------------------------------------------------
-- Datexs stanzas

data Datexs =
  Datexs { dateYear  :: Int -- The year can be negative
         , dateMonth :: Int
         , dateDay   :: Int
         , dateTzOff :: Maybe H.TimezoneOffset
         }
  deriving (Eq, Ord, Show)

instance Transformatio Datexs
  where
    fac candidate = parseCollapse datexsParser $ candidate
    scribe (Datexs year month day Nothing) =
      T.concat [ yearTx (fromIntegral year)
               , "-"
               , T.pack $ preFillWith '0' 2 month
               , "-"
               , T.pack $ preFillWith '0' 2 day
               ]
    scribe (Datexs year month day (Just H.TimezoneOffset {..})) =
      T.append (scribe $ Datexs year month day Nothing)
               (tzTx timezoneOffsetToMinutes)

instance Res Datexs (Int, Int, Int, Maybe H.TimezoneOffset)
  where
    redde (Datexs year month day mTzOff) = (year, month, day, mTzOff)
    recipe (year, month, day, mTzOff)
      |    gYearP year
        && yearMonthDayP year month day
        && gTzOffP mTzOff
        = Just $ Datexs year month day mTzOff
      | otherwise = Nothing

-- Lexical Representation -?([1-9][0-9]{3,}|0[0-9]{3})-(0[1-9]|1[0-2])-(0[1-9]|[12][0-9]|3[01])(Z|(\+|-)((0[0-9]|1[0-3]):[0-5][0-9]|14:00))?

datexsParser :: Parser Datexs
datexsParser = do
  negativeP <- (optional "-") >>= pure . isJust -- Negative predicate
  firstDigit <- digit
  yDigits <- many1 digit >>= pure . (firstDigit:)
  guard $ not (firstDigit == '0' && length yDigits > 4)
       && inRange (4,periodMaxN) (length yDigits) -- See 5.4 Partial Implementation of Infinite Datatypes.                                           -- ⚡
  void "-"
  mDigits <- parse2 (char '0' , cinClass "1-9")
         <|> parse2 (char '1' , cinClass "0-2")
  void "-"
  dDigits <- parse2 (char     '0'  , cinClass "1-9")
         <|> parse2 (cinClass "12" , digit         )
         <|> parse2 (char     '3'  , cinClass "01" )
  mTzOff <- optional tzOffParser
  let month = read mDigits -- Parsed read safe.
      day   = read dDigits -- Parsed read safe.
      year  = read yDigits -- Parsed read safe.
  guard $ yearMonthDayP year month day
  pure Datexs { dateYear  = (negativeP ? negate $ id) year
              , dateMonth = month
              , dateDay   = day
              , dateTzOff = mTzOff
              }

-- day is no more than 28 if ·month· is 2 and ·year· is not divisible by 4, or is divisible by 100 but not by 400. See 3.3.9.1 Value Space.
yearMonthDayP :: Int -> Int -> Int -> Bool
yearMonthDayP year month day = H.daysInMonth year (toEnum $ month - 1) >= day

-- --------------------------------------------------------------------------------------------------------------------------------------------------
-- Timexs stanzas

data Timexs =
  Timexs { timeOfDay :: H.TimeOfDay
         , timeTzOff :: Maybe H.TimezoneOffset
         }
  deriving (Eq, Ord, Show)

instance Transformatio Timexs
  where
    fac candidate = parseCollapse timexsParser $ candidate
    scribe (Timexs (H.TimeOfDay
                       (H.Hours hour)
                       (H.Minutes minute)
                       (H.Seconds seconds)
                       nanos
                   ) Nothing
           )
      = T.concat [ T.pack $ preFillWith '0' 2 hour
                 , ":"
                 , T.pack $ preFillWith '0' 2 minute
                 , ":"
                 , T.pack $ preFillWith '0' 2 seconds
                 , if nanos == 0
                   then T.empty
                   else T.pack . drop 1 $ nsToString nanos
                 ]
    scribe (Timexs tod (Just H.TimezoneOffset {..}))
      = T.append (scribe $ Timexs tod Nothing) (tzTx timezoneOffsetToMinutes)
instance Res Timexs (H.TimeOfDay, Maybe H.TimezoneOffset)
  where
    redde (Timexs tod mTzOff) = (tod, mTzOff)
    recipe (tod@(H.TimeOfDay
                    (H.Hours hour)
                    (H.Minutes minute)
                    (H.Seconds seconds)
                    (H.NanoSeconds nanos)
                )
           , mTzOff
           )
      |    inRange (0,23) hour
        && inRange (0,59) minute
        && inRange (0,59) seconds -- Seconds have been denuded of leap second capability in 1.1 of the standard. See I.3 Date/time Datatypes.
        && i64_p99P nanos
        && gTzOffP mTzOff
        = Just $ Timexs tod mTzOff
      | otherwise = Nothing

-- Lexical Representation (([01][0-9]|2[0-3]):[0-5][0-9]:[0-5][0-9](\.[0-9]+)?|(24:00:00(\.0+)?))(Z|(\+|-)((0[0-9]|1[0-3]):[0-5][0-9]|14:00))?

timexsParser :: Parser Timexs
timexsParser = do
  (h,m,s,f) <-
    choice [ "24:00:00" >> optional (char '.' >> many1 (char '0')) >> pure (0,0,0,0)
           , do hour <- parse2 (cinClass "01", digit)
                    <|> parse2 (char '2', cinClass "0-3")
                void ":"
                minute <- parse2 (cinClass "0-5", digit)
                void ":"
                seconds <- parse2 (cinClass "0-5", digit)
                mFrac <- optional fracF
                let h = H.Hours   $ read hour    -- Parsed read safe.
                    m = H.Minutes $ read minute  -- Parsed read safe.
                    s = H.Seconds $ read seconds -- Parsed read safe.
                    f = fromMaybe 0 (unsafeStringToNS <$> mFrac)
                pure (h,m,s,f)
           ]
  mTzOff <- optional tzOffParser
  pure Timexs { timeOfDay = H.TimeOfDay h m s f
              , timeTzOff = mTzOff
              }

  where
    fracF = do
      void "."
      digits <- many1 digit
      guard (length digits < 10) -- See 5.4 Partial Implementation of Infinite Datatypes.                                                            -- ⚡
      pure digits
    unsafeStringToNS = fromJust . mStringToNS

-- --------------------------------------------------------------------------------------------------------------------------------------------------
-- DateTimexs stanzas

{-
The dateTimeLexicalRep production is equivalent to this regular expression once whitespace/comment is removed.

    -?([1-9][0-9]{3,}|0[0-9]{3})                                                 -- Datexs identical
    -(0[1-9]|1[0-2])                                                             -- Datexs identical
    -(0[1-9]|[12][0-9]|3[01])                                                    -- Datexs identical
    T                                                                            -- Mandatory T
    (([01][0-9]|2[0-3]):[0-5][0-9]:[0-5][0-9](\\.[0-9]+)?|(24:00:00(\\.0+)?))    -- Timexs identical
    (Z|(\\+|-)((0[0-9]|1[0-3]):[0-5][0-9]|14:00))?                                -- Datexs/Timexs identical

-}

data DateTimexs =
  DateTimexs { dateTimeYear  :: Int -- The year can be negative
             , dateTimeMonth :: Int
             , dateTimeDay   :: Int
             , dateTimeOfDay :: H.TimeOfDay
             , dateTimeTzOff :: Maybe H.TimezoneOffset
             }
  deriving (Eq, Ord, Show)

dateTimexsParser :: Parser DateTimexs
dateTimexsParser = do
  Datexs{..} <- datexsParser
  void "T"                                          -- Mandatory T
  Timexs{..} <- timexsParser
  pure DateTimexs { dateTimeYear  = dateYear
                  , dateTimeMonth = dateMonth
                  , dateTimeDay   = dateDay
                  , dateTimeOfDay = timeOfDay
                  , dateTimeTzOff = timeTzOff
                  }

instance Transformatio DateTimexs
  where
    fac candidate = parseCollapse dateTimexsParser $ candidate
    scribe (DateTimexs year month day (H.TimeOfDay
                                          (H.Hours hour)
                                          (H.Minutes minute)
                                          (H.Seconds seconds)
                                          nanos
                                      ) Nothing
           )
      = T.concat [ yearTx (fromIntegral year)
                 , "-"
                 , T.pack $ preFillWith '0' 2 month
                 , "-"
                 , T.pack $ preFillWith '0' 2 day
                 , "T"
                 , T.pack $ preFillWith '0' 2 hour
                 , ":"
                 , T.pack $ preFillWith '0' 2 minute
                 , ":"
                 , T.pack $ preFillWith '0' 2 seconds
                 , if nanos == 0
                   then T.empty
                   else T.pack . drop 1 $ nsToString nanos
                 ]
    scribe (DateTimexs year month day tod (Just H.TimezoneOffset {..}))
      = T.append (scribe $ DateTimexs year month day tod Nothing)
                 (tzTx timezoneOffsetToMinutes)

instance Res DateTimexs (Int, Int, Int, H.TimeOfDay, Maybe H.TimezoneOffset)
  where
    redde (DateTimexs year month day tod mTzOff) = (year, month, day, tod, mTzOff)
    recipe (year, month, day
           , tod@(H.TimeOfDay
                    (H.Hours hour)
                    (H.Minutes minute)
                    (H.Seconds seconds)
                    (H.NanoSeconds nanos)
                 )
           , mTzOff
           )
      |    gYearP year
        && yearMonthDayP year month day
        && inRange (0,23) hour
        && inRange (0,59) minute
        && inRange (0,59) seconds -- Seconds have been denuded of leap second capability in 1.1 of the standard. See I.3 Date/time Datatypes.
        && i64_p99P nanos
        && gTzOffP mTzOff
        = Just $ DateTimexs year month day tod mTzOff
      | otherwise = Nothing

instance Res DateTimexs (Datexs, Timexs)
  where
    redde (DateTimexs year month day tod mTzOff)
      = (Datexs year month day mTzOff, Timexs tod mTzOff)
    recipe (Datexs year month day (Just tzDate), Timexs tod Nothing)
      = Just $ DateTimexs year month day tod (Just tzDate)
    recipe (Datexs year month day Nothing, Timexs tod (Just tzTime))
      = Just $ DateTimexs year month day tod (Just tzTime)
    recipe (Datexs year month day mTzOffDxs, Timexs tod mTzOffTxs)
      | mTzOffDxs == mTzOffTxs
        = Just $ DateTimexs year month day tod mTzOffDxs
      | otherwise = Nothing

instance Res DateTimexs Datexs
  where
    redde (DateTimexs year month day _ mTzOff)
      = Datexs year month day mTzOff
    recipe (Datexs year month day mTzOff)
      = Just $ DateTimexs year month day leastTime mTzOff

instance Res DateTimexs (H.DateTime, Maybe H.TimezoneOffset)
  where
    redde (DateTimexs year month day tod mTzOff)
      = (H.DateTime { dtDate = H.Date year (toEnum $ month - 1) day
                    , dtTime = tod
                    }
        , mTzOff
        )
    recipe (H.DateTime{..}, mTzOff)
      = recipe ( H.dateYear dtDate
               , fromEnum (H.dateMonth dtDate) + 1
               , H.dateDay dtDate
               , dtTime
               , mTzOff
               )

instance Res DateTimexs H.DateTime
  where
    redde (DateTimexs year month day tod _) =
      H.DateTime { dtDate = H.Date year (toEnum $ month - 1) day, dtTime = tod }
    recipe H.DateTime{..} =
      recipe ( H.dateYear dtDate
             , fromEnum (H.dateMonth dtDate) + 1
             , H.dateDay dtDate
             , dtTime
             , Nothing :: Maybe H.TimezoneOffset
             )

-- --------------------------------------------------------------------------------------------------------------------------------------------------
-- DateTimeStampxs stanzas

data DateTimeStampxs =
  DateTimeStampxs Int -- The year can be negative
                  Int
                  Int
                  H.TimeOfDay
                  H.TimezoneOffset -- DateTimeStamp has timezone as mandatory unlike DateTime.
  deriving (Eq, Ord, Show)

-- data DateTimeStampxs = DateTimeStampxs { dateTimeStampYear  :: Int -- The year can be negative
--                                        , dateTimeStampMonth :: Int
--                                        , dateTimeStampDay   :: Int
--                                        , dateTimeStampOfDay :: H.TimeOfDay
--                                        , dateTimeStampTzOff :: H.TimezoneOffset -- DateTimeStamp has timezone as mandatory unlike DateTime.
--                                        }
--                        deriving (Show, Eq)

dateTimeStampxsParser :: Parser DateTimeStampxs
dateTimeStampxsParser = do
  dtxs <- dateTimexsParser
  case recipe dtxs of
    Just dts -> pure dts
    Nothing -> fail "Timezone is mandatory on DateTimeStamp"

instance Transformatio DateTimeStampxs
  where
    fac candidate = parseCollapse dateTimeStampxsParser $ candidate
    scribe (DateTimeStampxs year month day tod tzOff)
      = scribe (DateTimexs year month day tod (Just tzOff))

instance Res DateTimeStampxs DateTimexs
  where
    redde (DateTimeStampxs year month day tod tzOff)
      = DateTimexs year month day tod (Just tzOff)
    recipe (DateTimexs year month day tod (Just tzDate))
      = Just $ DateTimeStampxs year month day tod tzDate
    recipe _ = Nothing

instance Res DateTimeStampxs (Datexs, Timexs)
  where
    redde dts = redde (redde dts :: DateTimexs)
    recipe pair = recipe @DateTimexs pair >>= recipe

instance Res DateTimeStampxs Datexs
  where
    redde dts = redde (redde dts :: DateTimexs)
    recipe datexs = recipe @DateTimexs datexs >>= recipe

instance Res DateTimeStampxs (H.DateTime, H.TimezoneOffset)
  where
    redde (DateTimeStampxs year month day tod tzOff) =
      (H.DateTime { dtDate = H.Date year (toEnum $ month - 1) day, dtTime = tod }, tzOff)
    recipe (H.DateTime{..}, tzOff) =
      recipe @DateTimexs
             ( H.dateYear dtDate
             , fromEnum (H.dateMonth dtDate) + 1
             , H.dateDay dtDate
             , dtTime
             , Just tzOff
             )
      >>= recipe

-- --------------------------------------------------------------------------------------------------------------------------------------------------
-- AnyURI stanzas

newtype AnyURI = AnyURI T.Text
  deriving (Show)

instance Transformatio AnyURI
  where
    fac candidate = const (AnyURI $ collapse candidate) <$> parseCollapse iriReference candidate
    scribe (AnyURI text) = text

-- 3.3.18 QName

-- [Definition:]   QName represents XML qualified names.
-- The ·value space· of QName is the set of tuples {namespace name, local part},
-- where namespace name is an anyURI and local part is an NCName.
-- The ·lexical space· of QName is the set of strings that ·match· the QName production of [Namespaces in XML].

-- -- | Convert a 'Rational' to a 'String' using the given number of decimals.
-- -- If the number of decimals is not given the full precision is showed using (DDD) for repeating digits.
-- -- E.g., 13.7/3 is shown as \"4.5(6)\".
-- showRational :: Maybe Int -> Rational -> String
-- showRational (Just n) r =
--     let d = round (abs r * 10^n)
--         s = show (d :: Integer)
--         s' = replicate (n - length s + 1) '0' ++ s
--         (h, f) = splitAt (length s' - n) s'
--     in  (if r < 0 then "-" else "") ++ h ++ "." ++ f
-- -- The length of the repeating digits is related to the totient function of the denominator.
-- -- This means that the complexity of computing them is at least as bad as factoring, i.e., it quickly becomes infeasible.
-- showRational Nothing r =
--     let (iproper, fproper) = properFraction (abs r) :: (Integer, Rational)
--         si = if r < 0 then "-" ++ show iproper else show iproper
--         decimals f = loop f [] ""
--         loop x fs ds =
--             if x == 0 then ds
--             else case DL.findIndex (x ==) fs of
--                    Just i  -> let (l, r) = splitAt i ds in l ++ "(" ++ r ++ ")"
--                    Nothing -> let (c, f) = properFraction (10 * x) :: (Integer, Rational) in loop f (fs ++ [x]) (ds ++ show c)
--     in if fproper == 0 then si else si ++ "." ++ decimals fproper


-- --  Datatype                       ordered    bounded  cardinality         numeric
-- doIt = T.concat [ makeFundies "Stringxs          " OrdFalse   "False"    CardInfinite "False"
--        , makeFundies "Boolean           " OrdFalse   "False"    CardFinite   "False"
--        , makeFundies "Decimal           " OrdPartial "True "    CardFinite   "True "
--        , makeFundies "Floatxs           " OrdPartial "True "    CardFinite   "True "
--        , makeFundies "Doublexs          " OrdTotal   "False"    CardInfinite "True "
--        , makeFundies "Durationxs        " OrdPartial "False"    CardInfinite "False"
--        , makeFundies "DateTimexs        " OrdPartial "False"    CardInfinite "False"
--        , makeFundies "Timexs            " OrdPartial "False"    CardInfinite "False"
--        , makeFundies "Datexs            " OrdPartial "False"    CardInfinite "False"
--        , makeFundies "GYearMonth        " OrdPartial "False"    CardInfinite "False"
--        , makeFundies "GYear             " OrdPartial "False"    CardInfinite "False"
--        , makeFundies "GMonthDay         " OrdPartial "False"    CardInfinite "False"
--        , makeFundies "GDay              " OrdPartial "False"    CardInfinite "False"
--        , makeFundies "GMonth            " OrdPartial "False"    CardInfinite "False"
--        , makeFundies "HexBinary         " OrdFalse   "False"    CardInfinite "False"
--        , makeFundies "Base64Binary      " OrdFalse   "False"    CardInfinite "False"
--        , makeFundies "AnyURI            " OrdFalse   "False"    CardInfinite "False"
--        , makeFundies "QName             " OrdFalse   "False"    CardInfinite "False"
--        , makeFundies "NOTATION          " OrdFalse   "False"    CardInfinite "False"
--        , makeFundies "NormalizedString  " OrdFalse   "False"    CardInfinite "False"
--        , makeFundies "Token             " OrdFalse   "False"    CardInfinite "False"
--        , makeFundies "Language          " OrdFalse   "False"    CardInfinite "False"
--        , makeFundies "NMTOKEN           " OrdFalse   "False"    CardInfinite "False"
--        , makeFundies "NMTOKENS          " OrdFalse   "False"    CardInfinite "False"
--        , makeFundies "Name              " OrdFalse   "False"    CardInfinite "False"
--        , makeFundies "NCName            " OrdFalse   "False"    CardInfinite "False"
--        , makeFundies "ID                " OrdFalse   "False"    CardInfinite "False"
--        , makeFundies "IDREF             " OrdFalse   "False"    CardInfinite "False"
--        , makeFundies "IDREFS            " OrdFalse   "False"    CardInfinite "False"
--        , makeFundies "ENTITY            " OrdFalse   "False"    CardInfinite "False"
--        , makeFundies "ENTITIES          " OrdFalse   "False"    CardInfinite "False"
--        , makeFundies "Integer           " OrdTotal   "False"    CardInfinite "True "
--        , makeFundies "NonPositiveInteger" OrdTotal   "False"    CardInfinite "True "
--        , makeFundies "NegativeInteger   " OrdTotal   "False"    CardInfinite "True "
--        , makeFundies "Long              " OrdTotal   "True "    CardFinite   "True "
--        , makeFundies "Intxs             " OrdTotal   "True "    CardFinite   "True "
--        , makeFundies "Short             " OrdTotal   "True "    CardFinite   "True "
--        , makeFundies "Byte              " OrdTotal   "True "    CardFinite   "True "
--        , makeFundies "NonNegativeInteger" OrdTotal   "False"    CardInfinite "True "
--        , makeFundies "UnsignedLong      " OrdTotal   "True "    CardFinite   "True "
--        , makeFundies "UnsignedInt       " OrdTotal   "True "    CardFinite   "True "
--        , makeFundies "UnsignedShort     " OrdTotal   "True "    CardFinite   "True "
--        , makeFundies "UnsignedByte      " OrdTotal   "True "    CardFinite   "True "
--        , makeFundies "PositiveInteger   " OrdTotal   "False"    CardInfinite "True "
--        , makeFundies "YearMonthDuration " OrdPartial "False"    CardInfinite "False"
--        , makeFundies "DayTimeDuration   " OrdPartial "False"    CardInfinite "False"
--        , makeFundies "DateTimeStampxs   " OrdPartial "False"    CardInfinite "False"
--        ]

-- makeFundies :: Text -> FacetOrd -> Text -> FacetCardinality -> Text -> Text
-- makeFundies type_pp ord_pp bound_pp card_pp numeric_pp =
--   T.concat [ "\ninstance FacetC " , type_pp
--            , "\n  where facetOrdC         = " , tshow ord_pp
--            , "\n        facetBoundedC     = " , bound_pp
--            , "\n        facetCardinalityC = " , tshow card_pp
--            , "\n        facetNumericC     = " , numeric_pp
--            , "\n"
--            ]





-- class FacetPattern a where
--   patternC :: Maybe ([Annotation], [Pattern], (Text -> Bool), a) -- Do not evaluate 'a'. It is used for identifying the instance Type.
--   patternC = Nothing

-- 4.3.7 maxInclusive -- This is a value-based facet.

-- class AnySimpleType a => FacetMinMax a where
--   minInclusive :: Maybe ([Annotation], Bool, a, (a -> Bool))
--   minEnclusive :: Maybe ([Annotation], Bool, a, (a -> Bool))
--   maxInclusive :: Maybe ([Annotation], Bool, a, (a -> Bool))
--   maxEnclusive :: Maybe ([Annotation], Bool, a, (a -> Bool))
--   minInclusive = Nothing
--   minEnclusive = Nothing
--   maxInclusive = Nothing
--   maxEnclusive = Nothing

-- 4.3.11 totalDigits -- This is a value-based facet.

-- class FacetTotalDigits a where
--   totalDigits :: Maybe ([Annotation], Bool, PositiveInteger, (a -> Bool))
--   totalDigits = Nothing

-- 4.3.12 fractionDigits -- This is a value-based facet.

-- class FacetFractionDigits a where
--   fractionDigits :: Maybe ([Annotation], Bool, NonNegativeInteger, (a -> Bool))
--   fractionDigits = Nothing

-- 4.3.13 Assertions -- This is a value-based facet.



-- class FacetAssertions a where
--   assertions :: Maybe ([Annotation], [Assertion], (a -> Bool))
--   assertions = Nothing

-- 4.3.14 explicitTimezone -- This is a value-based facet.



-- class FacetExplicitTimezone a where
--   explicitTimezone :: Maybe ([Annotation], Bool, ExplicitTimezone, (a -> Bool))
--   explicitTimezone = Nothing


-- See 4.3.1 length. -- This is a value-based facet.

-- class FacetLength a where
--   --                                  Fixed? Length              Validation Test
--   minLength   :: Maybe ([Annotation], Bool, NonNegativeInteger, (a -> Bool))
--   maxLength   :: Maybe ([Annotation], Bool, NonNegativeInteger, (a -> Bool))
--   valueLengthC :: Maybe ([Annotation], Bool, NonNegativeInteger, (a -> Bool))
--   minLength   = Nothing
--   maxLength   = Nothing
--   valueLengthC = Nothing
