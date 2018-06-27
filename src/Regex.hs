{-# Language ExistentialQuantification, MultiParamTypeClasses, FlexibleInstances, GeneralizedNewtypeDeriving, NegativeLiterals #-}
{-| Time-stamp: <2018-06-27 07:47:55 robert>

Module      : Builtin
Copyright   : (c) Robert Lee, 2017-2018
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

-- | G.1 Regular expressions and branches
--   A regular expression is composed from zero or more ·branches·, separated by '|' characters.
data RE = RE [Branch]
          deriving (Show, Eq)

-- | A branch consists of zero or more ·pieces·, concatenated together.
data Branch = Branch [Piece]
              deriving (Show, Eq)

-- | G.2 Pieces, atoms, quantifiers
--   A piece is an ·atom·, possibly followed by a ·quantifier·. 
data Piece = Piece Atom (Maybe Quantifier)
             deriving (Show, Eq)

-- | An atom is either a ·normal character·, a ·character class·, or a parenthesized ·regular expression·.
data Atom = AtomNormal    NormalChar
          | AtomCharClass CharClass
          | AtomRE        RE
            deriving (Show, Eq)
                     
-- | A quantifier is one of '?', '*', or '+', or a string of the form {n,m} or {n,} ,
--   which have the meanings defined in the table above. 
data Quantifier = QuantifierMaybeOne  Quantity
                | QuantifierMaybeMany Quantity
                | QuantifierMany      Quantity
                  deriving (Show, Eq)

data Quantity = QuantRange  QuantExact QuantExact
              | QuantMin    QuantExact
              | QuantExactQ QuantExact
                deriving (Show, Eq)

data QuantExact = QuantExact Int                         
                  deriving (Show, Eq)

-- | G.3 Characters and metacharacters
data NormalChar = NormalChar Char
                  deriving (Show, Eq)

-- | G.4 Character Classes
data CharClass = SingleCharEscC SingleCharEsc
               | CharClassEsc
               | CharClassExprC CharGroup
               | WildcardEscC   WildcardEsc
                 deriving (Show, Eq)

-- | G.4.1 Character class expressions
-- | A character class expression (charClassExpr) is a ·character group· surrounded by '[' and ']' characters.
data CharClassExpr = CharClassExpr CharGroup
                     deriving (Show, Eq)

data CharGroup = CharGroup (Either PosCharGroup NegCharGroup) (Maybe CharClassExpr)
                 deriving (Show, Eq)

data PosCharGroup = PosCharGroup [CharGroupPart]
                    deriving (Show, Eq)

data NegCharGroup = NegCharGroup PosCharGroup
                    deriving (Show, Eq)

data CharGroupPart = CharGroupPartSingle   SingleChar
                   | CharGroupPartRange    CharRange
                   | CharGroupPartClassEsc SingleCharEsc
                     deriving (Show, Eq)

data SingleChar = SingleChar (Either SingleCharEsc SingleCharNoEsc)
                  deriving (Show, Eq)

data CharRange = CharRange SingleChar SingleChar
                 deriving (Show, Eq)

-- | G.4.2 Character Class Escapes
data SingleCharNoEsc = SingleCharNoEsc Char
                       deriving (Show, Eq)

-- | G.4.2.1 Single-character escapes
data SingleCharEsc = SingleCharEsc Char
                     deriving (Show, Eq)

{- | G.4.2 Character Class Escapes
     A character class escape is a short sequence of characters that identifies a predefined
     character class.  The valid character class escapes are the ·multi-character escapes·,
     and the ·category escapes· (including the ·block escapes·).
-}
data CharClassEsc = CharClassEscMultiCharEsc MultiCharEsc
                  | CharClassEscCatEsc       CatEsc
                  | CharClassEscComplEsc     ComplEsc
                    deriving (Show, Eq)

{- | G.4.2.2 Category escapes
     [Unicode Database] specifies a number of possible values for the "General Category" property
     and provides mappings from code points to specific character properties.
     The set containing all characters that have property X can be identified with a category
     escape \p{X} (using a lower-case 'p').  The complement of this set is specified with the
     category escape  \P{X} (using an upper-case 'P').  For all X, if X is a recognized
     character-property code, then [\P{X}] = [^\p{X}].
-}

data CatEsc = CatEsc CharProp
              deriving (Show, Eq)

data ComplEsc = ComplEsc CharProp
                deriving (Show, Eq)

data CharProp = CharProp (Either IsCategory IsBlock)
                deriving (Show, Eq)

{- | Categories
     [88] IsCategory  ::= Letters | Marks | Numbers | Punctuation | Separators | Symbols | Others
     [89] Letters     ::= 'L' [ultmo]?
     [90] Marks       ::= 'M' [nce]?
     [91] Numbers     ::= 'N' [dlo]?
     [92] Punctuation ::= 'P' [cdseifo]?
     [93] Separators  ::= 'Z' [slp]?
     [94] Symbols     ::= 'S' [mcko]?
     [95] Others      ::= 'C' [cfon]?
-}
data IsCategory = LettersCat     Letters
                | MarksCat       Marks
                | NumbersCat     Numbers
                | PunctuationCat Punctuation
                | SeparatorsCat  Separators
                | SymbolsCat     Symbols
                | OthersCat      Others
                  deriving (Show, Eq)

data Letters = L  -- | All Letters
             | Lu -- | uppercase
             | Ll -- | lowercase
             | Lt -- | titlecase
             | Lm -- | modifier
             | Lo -- | other
               deriving (Show, Eq, Bounded, Enum)

letters :: Parser IsCategory
letters = choice (map parserPair (enumFromTo minBound maxBound :: [] Letters)) >>= pure . LettersCat . fst
                        
data Marks = M  -- | All Marks
           | Mn -- | nonspacing
           | Mc -- | spacing combining
           | Me -- | enclosing
             deriving (Show, Eq, Bounded, Enum)

marks :: Parser IsCategory
marks = choice (map parserPair (enumFromTo minBound maxBound :: [] Marks)) >>= pure . MarksCat . fst

data Numbers = N  -- | All Numbers
             | Nd -- | decimal digit
             | Nl -- | letter
             | No -- | other
               deriving (Show, Eq, Bounded, Enum)

numbers :: Parser IsCategory
numbers = choice (map parserPair (enumFromTo minBound maxBound :: [] Numbers)) >>= pure . NumbersCat . fst

data Punctuation = P  -- | All Punctuation
                 | Pc -- | connector
                 | Pd -- | dash
                 | Ps -- | open
                 | Pe -- | close
                 | Pi -- | initial quote (may behave like Ps or Pe depending on usage)
                 | Pf -- | final quote (may behave like Ps or Pe depending on usage)
                 | Po -- | other
                   deriving (Show, Eq, Bounded, Enum)

punctuation :: Parser IsCategory
punctuation = choice (map parserPair (enumFromTo minBound maxBound :: [] Punctuation)) >>= pure . PunctuationCat . fst

data Separators = Z  -- | All Separators
                | Zs -- | space
                | Zl -- | line
                | Zp -- | paragraph
                  deriving (Show, Eq, Bounded, Enum)

separators :: Parser IsCategory
separators = choice (map parserPair (enumFromTo minBound maxBound :: [] Separators)) >>= pure . SeparatorsCat . fst

data Symbols = S  -- | All Symbols
             | Sm -- | math
             | Sc -- | currency
             | Sk -- | modifier
             | So -- | other
               deriving (Show, Eq, Bounded, Enum)

symbols :: Parser IsCategory
symbols = choice (map parserPair (enumFromTo minBound maxBound :: [] Symbols)) >>= pure . SymbolsCat . fst

data Others = C  -- | All Others
            | Cc -- | control
            | Cf -- | format
            | Co -- | private use
            | Cn -- | not assigned
              deriving (Show, Eq, Bounded, Enum)

others :: Parser IsCategory
others = choice (map parserPair (enumFromTo minBound maxBound :: [] Others)) >>= pure . OthersCat . fst
                       
isCategory :: Parser IsCategory
isCategory = do
  choice [letters, marks, numbers, punctuation, separators, symbols, others]

parserPair :: (Show a) => a -> Parser (a, Text)
parserPair a = do
  res <- string . T.pack $ show a
  pure (a, res)
                       
data IsBlock = IsBlock
               deriving (Show, Eq)

-- | A multi-character escape provides a simple way to identify any of a commonly used set of characters.
data MultiCharEsc = MultiCharEsc Char
                    deriving (Show, Eq)

multiCharEsc :: Parser MultiCharEsc
multiCharEsc = do
  void $ char '\\'
  satisfy (inClass "sSiIcCdDwW") >>= pure . MultiCharEsc
                             
-- | The wildcard character is a metacharacter which matches almost any single character
data WildcardEsc = WildcardEsc
                   deriving (Show, Eq)

wildCardEsc :: Parser Char
wildCardEsc  = char '.'
