{-# Language ExistentialQuantification, MultiParamTypeClasses
  , FlexibleInstances, GeneralizedNewtypeDeriving, NegativeLiterals, MultiWayIf #-}
{-| Time-stamp: <2018-07-03 19:52:55 CDT>

Module      : Regex
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

                      correct, but not efficient ⋅⋅⋅ η
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
import qualified Data.List              as L
import qualified Data.Text              as T

-- Undisciplined Imports

import ClassyPrelude hiding (IO, traceShow)
import Data.Attoparsec.Text
-- import Text.XML hiding (Name)

-- End of Imports
-- -----------------------------------------------------------------------------------------------------------------------------------------------------

-- Objectives
-- 1. Create AST from XSD regex     : Take XML Schema 1.1 regex string and produce Aeson Parser AST.
-- 2. Create XSD regex from AST     : Take Aeson Parser AST and produce XML Schema 1.1 regex string.
-- 3. Validate string               : Take (XML string, Aeson Parser AST) and produce {True, False}.
-- 4. Create non-XSD regex from AST : Take Aeson Parser AST and produce non-XML Schema 1.1 regex string.

-- See W3C XML Schema Definition Language (XSD) 1.1 Part 2: Datatypes
-- § G Regular Expressions

-- | § G.1 Regular expressions and branches
--   A regular expression is composed from zero or more ·branches·, separated by '|' characters.
data RE = RE [Branch]
          deriving (Show, Eq)

-- | Required by the standard: Anchor before use (e.g. parseOnly (anchorParser re) "xyz").
--   However, do not place an endOfInput (anchor) inside re as it will never work correctly!                                                         -- ⚡
re :: Parser RE
re = do opt <- option Nothing $ (branch >>= pure . Just)
        case opt of
          Nothing -> pure $ RE []
          Just b1 -> do branches <- many' (char '|' >> branch)
                        pure $ RE (b1:branches)

-- | A branch consists of zero or more ·pieces·, concatenated together.
data Branch = Branch [Piece]
              deriving (Show, Eq)

branch :: Parser Branch
branch = do pieces <- many' piece
            pure $ Branch pieces

-- | § G.2 Pieces, atoms, quantifiers
--   A piece is an ·atom·, possibly followed by a ·quantifier·.
data Piece = Piece Atom (Maybe Quantifier)
             deriving (Show, Eq)

piece :: Parser Piece
piece = do a <- atom
           opt <- option Nothing (quantifier >>= pure . Just)
           pure $ Piece a opt

-- | An atom is either a ·normal character·, a ·character class·, or a parenthesized ·regular expression·.
data Atom = AtomNormal    NormalChar
          | AtomCharClass CharClass
          | AtomRE        RE
            deriving (Show, Eq)

atom :: Parser Atom
atom = choice [ atomRE     >>= pure . AtomRE
              , charClass  >>= pure . AtomCharClass
              , normalChar >>= pure . AtomNormal
              ]
  where atomRE = do
          skipC '('
          r <- re
          skipC ')'
          pure r

-- | A quantifier is one of '?', '*', or '+', or a string of the form {n,m} or {n,} ,
--   which have the meanings defined in the table above.
data Quantifier = QuantifierMaybeOne
                | QuantifierMaybeMany
                | QuantifierMany
                | QuintifierQuantity Quantity
                  deriving (Show, Eq)

quantifier :: Parser Quantifier
quantifier = choice [ char '?' >> pure QuantifierMaybeOne
                    , char '*' >> pure QuantifierMaybeMany
                    , char '+' >> pure QuantifierMany
                    , quintifierQuantity
                    ]
  where quintifierQuantity = do skipC '{'
                                q <- quantity
                                skipC '}'
                                pure $ QuintifierQuantity q

data Quantity = QuantRange  QuantExact QuantExact
              | QuantMin    QuantExact
              | QuantExactQ QuantExact
                deriving (Show, Eq)

quantity :: Parser Quantity
quantity = choice [ quantRange
                  , quantMin
                  , quantExact >>= pure . QuantExactQ
                  ]
  where quantRange = do l@(QuantExact ln) <- quantExact
                        skipC ','
                        r@(QuantExact rn) <- quantExact
                        guard $ ln <= rn
                        pure $ QuantRange l r
        quantMin = do l <- quantExact
                      skipC ','
                      pure $ QuantMin l

data QuantExact = QuantExact Int
                  deriving (Show, Eq)

quantExact :: Parser QuantExact
quantExact = do n <- decimal
                guard $ n >= 0
                pure $ QuantExact n

-- | § G.3 Characters and metacharacters
-- NormalChar ::= [^.\?*+{}()|#x5B#x5D]	/* N.B.: #x5B = '[', #x5D = ']' */
data NormalChar = NormalChar Char
                  deriving (Show, Eq)

normalChar :: Parser NormalChar
normalChar = (satisfy $ notInClass ".\\?*+{}()|[]") >>= pure . NormalChar

-- | § G.4 Character Classes
data CharClass = CharClassSingle SingleCharEsc
               | CharClassEscC   CharClassEsc
               | CharClassExprC  CharClassExpr
               | CharClassWild   WildcardEsc
                 deriving (Show, Eq)

charClass :: Parser CharClass
charClass = choice [ charClassExpr >>= pure . CharClassExprC
                   , charClassEsc  >>= pure . CharClassEscC
                   , singleCharEsc >>= pure . CharClassSingle
                   , wildCardEsc   >>= pure . CharClassWild
                   ]

-- | § G.4.1 Character class expressions
-- | A character class expression (charClassExpr) is a ·character group· surrounded by '[' and ']' characters.
data CharClassExpr = CharClassExpr CharGroup
                     deriving (Show, Eq)

charClassExpr :: Parser CharClassExpr
charClassExpr = do skipC '['
                   cg <- charGroup
                   skipC ']'
                   pure $ CharClassExpr cg

-- See https://www.regular-expressions.info/charclasssubtract.html
data CharGroup = CharGroup (Either PosCharGroup NegCharGroup) (Maybe CharClassExpr) -- The CharClassExpr is a subtraction group.
                 deriving (Show, Eq)

charGroup :: Parser CharGroup
charGroup = do cg <- choice [ negCharGroup >>= pure . Right
                            , posCharGroup >>= pure . Left
                            ]
               guard $ case cg of
                         Left                (PosCharGroup xs)  -> all legal xs
                         Right (NegCharGroup (PosCharGroup xs)) -> all legal xs -- See rules.

               mCE <- option Nothing $ do skipC '-'
                                          cce <- charClassExpr
                                          pure $ Just cce
               pure $ CharGroup cg mCE
  where
    legal (CharGroupPartRange ( CharRange   (SingleChar(Right(SingleCharNoEsc '-'))) _))  = False
    legal (CharGroupPartRange ( CharRange _ (SingleChar(Right(SingleCharNoEsc '-')))  ))  = False
    legal _ = True

data PosCharGroup = PosCharGroup [CharGroupPart]
                    deriving (Show, Eq)

posCharGroup :: Parser PosCharGroup
posCharGroup = many1 charGroupPart >>= pure . PosCharGroup

data NegCharGroup = NegCharGroup PosCharGroup
                    deriving (Show, Eq)

negCharGroup :: Parser NegCharGroup
negCharGroup = do skipC '^'
                  posCharGroup >>= pure . NegCharGroup

{- |
If a charGroupPart starts with a singleChar and this is immediately
followed by a hyphen, then the following rules apply.

  1. If the hyphen is immediately followed by '[', then the hyphen is
     not part of the charGroupPart: instead, it is recognized as a
     character-class subtraction operator.

  2. If the hyphen is immediately followed by ']', then the hyphen is
     recognized as a singleChar and is part of the charGroupPart.

  3. If the hyphen is immediately followed by '-[', then the hyphen is
     recognized as a singleChar and is part of the charGroupPart.

  4. Otherwise, the hyphen must be immediately followed by some
     singleChar other than a hyphen. In this case the hyphen is not
     part of the charGroupPart; instead it is recognized, together with
     the immediately preceding and following instances of singleChar,
     as a charRange.

  5. If the hyphen is followed by any other character sequence, then
     the string in which it occurs is not recognized as a regular
     expression.

It is an error if either of the two singleChars in a charRange is a
SingleCharNoEsc comprising an unescaped hyphen.

    Note: The rule just given resolves what would otherwise be the
    ambiguous interpretation of some strings, e.g. '[a-k-z]'; it also
    constrains regular expressions in ways not expressed in the
    grammar. For example, the rule (not the grammar) excludes the
    string '[--z]' from the regular expression language defined here.

-}

data CharGroupPart = CharGroupPartSingle   SingleChar
                   | CharGroupPartRange    CharRange
                   | CharGroupPartClassEsc CharClassEsc
                     deriving (Show, Eq)

charGroupPart :: Parser CharGroupPart
charGroupPart = choice [ charRange    >>= pure . CharGroupPartRange
                       , charClassEsc >>= pure . CharGroupPartClassEsc
                       , groupSingleDash
                       ]

  where groupSingleDash = do p1 <- peekChar'
                             s <- singleChar
                             p2 <- peekChar'
                             guard $ not (p1 == '-' && p2 == '[')
                             pure $ CharGroupPartSingle s

data SingleChar = SingleChar (Either SingleCharEsc SingleCharNoEsc)
                  deriving (Show, Eq)

data CharRange = CharRange SingleChar SingleChar
                 deriving (Show, Eq)

charRange :: Parser CharRange
charRange = do l <- singleChar
               skipC '-'
               r <- singleChar
               pure $ CharRange l r

singleChar :: Parser SingleChar
singleChar = choice [ singleCharEsc   >>= pure . Left
                    , singleCharNoEsc >>= pure . Right
                    ] >>= pure . SingleChar

data SingleCharNoEsc = SingleCharNoEsc Char
                       deriving (Show, Eq)

singleCharNoEsc :: Parser SingleCharNoEsc
singleCharNoEsc = do c <- satisfy $ notInClass "[]"
                     pure $ SingleCharNoEsc c

{- | § G.4.2 Character Class Escapes
     A character class escape is a short sequence of characters that identifies a predefined
     character class.  The valid character class escapes are the ·multi-character escapes·,
     and the ·category escapes· (including the ·block escapes·).
-}
data CharClassEsc = CharClassEscMultiCharEsc MultiCharEsc
                  | CharClassEscCatEsc       CatEsc
                  | CharClassEscComplEsc     ComplEsc
                    deriving (Show, Eq)

charClassEsc :: Parser CharClassEsc
charClassEsc = choice [ multiCharEsc >>= pure . CharClassEscMultiCharEsc
                      , catEsc       >>= pure . CharClassEscCatEsc
                      , complEsc     >>= pure . CharClassEscComplEsc
                      ]

-- | § G.4.2.1 Single-character escapes
data SingleCharEsc = SingleCharEsc Char
                     deriving (Show, Eq)

singleCharEsc :: Parser SingleCharEsc
singleCharEsc = do skipC '\\'
                   c <- satisfy $ inClass "-nrt\\|.?*+(){}[]^"
                   pure $ SingleCharEsc c

{- | § G.4.2.2 Category escapes
     [Unicode Database] specifies a number of possible values for the "General Category" property
     and provides mappings from code points to specific character properties.
     The set containing all characters that have property X can be identified with a category
     escape \p{X} (using a lower-case 'p').  The complement of this set is specified with the
     category escape  \P{X} (using an upper-case 'P').  For all X, if X is a recognized
     character-property code, then [\P{X}] = [^\p{X}].
-}

data CatEsc = CatEsc CharProp                             -- CatEsc ∩ CompEsc = ∅, CatEsc - CompEsc = CatEsc
              deriving (Show, Eq)

catEsc :: Parser CatEsc
catEsc = do skipS "\\p{"
            cEsc <- charPropParse
            skipC '}'
            pure $ CatEsc cEsc

data ComplEsc = ComplEsc CharProp                         -- CompEsc ∩ CatEsc = ∅, CompEsc - CatEsc = CompEsc
                deriving (Show, Eq)

complEsc :: Parser ComplEsc
complEsc = do skipS "\\P{"
              cEsc <- charPropParse >>= pure . ComplEsc
              skipC '}'
              pure cEsc

data CharProp = CharProp (Either IsCategory IsBlock)
                deriving (Show, Eq)

charPropParse :: Parser CharProp
charPropParse = (isBlock >>= pure . CharProp . Right) <|> (isCategory >>= pure . CharProp . Left)

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
letters = choice (map parserPair (revEnum :: [] Letters)) >>= pure . LettersCat . fst

data Marks = M  -- | All Marks
           | Mn -- | nonspacing
           | Mc -- | spacing combining
           | Me -- | enclosing
             deriving (Show, Eq, Bounded, Enum)

marks :: Parser IsCategory
marks = choice (map parserPair (revEnum :: [] Marks)) >>= pure . MarksCat . fst

data Numbers = N  -- | All Numbers
             | Nd -- | decimal digit
             | Nl -- | letter
             | No -- | other
               deriving (Show, Eq, Bounded, Enum)

numbers :: Parser IsCategory
numbers = choice (map parserPair (revEnum :: [] Numbers)) >>= pure . NumbersCat . fst

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
punctuation = choice (map parserPair (revEnum :: [] Punctuation)) >>= pure . PunctuationCat . fst

data Separators = Z  -- | All Separators
                | Zs -- | space
                | Zl -- | line
                | Zp -- | paragraph
                  deriving (Show, Eq, Bounded, Enum)

separators :: Parser IsCategory
separators = choice (map parserPair (revEnum :: [] Separators)) >>= pure . SeparatorsCat . fst

data Symbols = S  -- | All Symbols
             | Sm -- | math
             | Sc -- | currency
             | Sk -- | modifier
             | So -- | other
               deriving (Show, Eq, Bounded, Enum)

symbols :: Parser IsCategory
symbols = choice (map parserPair (revEnum :: [] Symbols)) >>= pure . SymbolsCat . fst

data Others = C  -- | All Others
            | Cc -- | control
            | Cf -- | format
            | Co -- | private use
            | Cn -- | not assigned
              deriving (Show, Eq, Bounded, Enum)

others :: Parser IsCategory
others = choice (map parserPair (revEnum :: [] Others)) >>= pure . OthersCat . fst

isCategory :: Parser IsCategory
isCategory = choice [letters, marks, numbers, punctuation, separators, symbols, others]

{- | § G.4.2.3 Block escapes
     For any Unicode block, the normalized block name of that block is
     the string of characters formed by stripping out white space and
     underbar characters from the block name as given in [Unicode
     Database], while retaining hyphens and preserving case
     distinctions.

     A block escape expression denotes the set of characters in a
     given Unicode block. For any Unicode block B, with ·normalized
     block name· X, the set containing all characters defined in block
     B can be identified with the block escape \p{IsX} (using
     lower-case 'p'). The complement of this set is denoted by the
     block escape \P{IsX} (using upper-case 'P'). For all X, if X is a
     normalized block name recognized by the processor, then [\P{IsX}]
     = [^\p{IsX}].
-}

data IsBlock = IsBlock UnicodeBlockName
               deriving (Show, Eq)

isBlock :: Parser IsBlock
isBlock = unicodeBlockMatch >>= pure . IsBlock . fst

unicodeBlockMatch :: Parser (UnicodeBlockName, Text)
unicodeBlockMatch = choice $ map unicodeBlockPair lengthOrderedUnicodeBlockNames

unicodeBlockPair :: UnicodeBlockName -> Parser (UnicodeBlockName, Text)
unicodeBlockPair ubnomen = do
  resText <- L.foldl1 foldF baseSegs
  pure (ubnomen, resText)
  where baseText = T.pack $ show ubnomen
        baseSegs = map (asciiCI) $ T.splitOn "_" baseText
        foldF pl pr = do pl >> satisfy (inClass "- _") >> pr

lengthOrderedUnicodeBlockNames :: [] UnicodeBlockName
lengthOrderedUnicodeBlockNames = L.sortBy (\nomenA nomenB -> L.length (show nomenB) `compare` L.length (show nomenA))
                               $ enumFrom minBound

unicodeBlockNameRanges :: [] (UnicodeBlockName, Parser Char)
unicodeBlockNameRanges = fmap (\(nomen, UBN s f) -> (nomen, satisfy $ inClass [C.chr s, '-', C.chr f])) ubnns

matchUnicodeBlockName :: UnicodeBlockName -> Parser Char
matchUnicodeBlockName nomen = case L.lookup nomen unicodeBlockNameRanges of
                                Just parser -> parser
                                Nothing -> fail $ show nomen ++ ": not found in lookup."

whichBlock :: Parser (UnicodeBlockName, Char)
whichBlock = choice $ map parsePair unicodeBlockNameRanges -- This is not efficient, but it is correct.                                             -- η

-- NB. Use asciiCI for case insensitive matching.
data UnicodeBlockName = AEGEAN_NUMBERS
                      | ALCHEMICAL_SYMBOLS
                      | ALPHABETIC_PRESENTATION_FORMS
                      | ANCIENT_GREEK_MUSICAL_NOTATION
                      | ANCIENT_GREEK_NUMBERS
                      | ANCIENT_SYMBOLS
                      | ARABIC
                      | ARABIC_PRESENTATION_FORMS_A
                      | ARABIC_PRESENTATION_FORMS_B
                      | ARABIC_SUPPLEMENT
                      | ARMENIAN
                      | ARROWS
                      | AVESTAN
                      | BALINESE
                      | BAMUM
                      | BAMUM_SUPPLEMENT
                      | BASIC_LATIN
                      | BATAK
                      | BENGALI
                      | BLOCK_ELEMENTS
                      | BOPOMOFO
                      | BOPOMOFO_EXTENDED
                      | BOX_DRAWING
                      | BRAHMI
                      | BRAILLE_PATTERNS
                      | BUGINESE
                      | BUHID
                      | BYZANTINE_MUSICAL_SYMBOLS
                      | CARIAN
                      | CHAM
                      | CHEROKEE
                      | CJK_COMPATIBILITY
                      | CJK_COMPATIBILITY_FORMS
                      | CJK_COMPATIBILITY_IDEOGRAPHS
                      | CJK_COMPATIBILITY_IDEOGRAPHS_SUPPLEMENT
                      | CJK_RADICALS_SUPPLEMENT
                      | CJK_STROKES
                      | CJK_SYMBOLS_AND_PUNCTUATION
                      | CJK_UNIFIED_IDEOGRAPHS
                      | CJK_UNIFIED_IDEOGRAPHS_EXTENSION_A
                      | CJK_UNIFIED_IDEOGRAPHS_EXTENSION_B
                      | CJK_UNIFIED_IDEOGRAPHS_EXTENSION_C
                      | CJK_UNIFIED_IDEOGRAPHS_EXTENSION_D
                      | COMBINING_DIACRITICAL_MARKS
                      | COMBINING_DIACRITICAL_MARKS_FOR_SYMBOLS
                      | COMBINING_DIACRITICAL_MARKS_SUPPLEMENT
                      | COMBINING_HALF_MARKS
                      | COMMON_INDIC_NUMBER_FORMS
                      | CONTROL_PICTURES
                      | COPTIC
                      | COUNTING_ROD_NUMERALS
                      | CUNEIFORM
                      | CUNEIFORM_NUMBERS_AND_PUNCTUATION
                      | CURRENCY_SYMBOLS
                      | CYPRIOT_SYLLABARY
                      | CYRILLIC
                      | CYRILLIC_EXTENDED_A
                      | CYRILLIC_EXTENDED_B
                      | CYRILLIC_SUPPLEMENT
                      | DESERET
                      | DEVANAGARI
                      | DEVANAGARI_EXTENDED
                      | DINGBATS
                      | DOMINO_TILES
                      | EGYPTIAN_HIEROGLYPHS
                      | EMOTICONS
                      | ENCLOSED_ALPHANUMERICS
                      | ENCLOSED_ALPHANUMERIC_SUPPLEMENT
                      | ENCLOSED_CJK_LETTERS_AND_MONTHS
                      | ENCLOSED_IDEOGRAPHIC_SUPPLEMENT
                      | ETHIOPIC
                      | ETHIOPIC_EXTENDED
                      | ETHIOPIC_EXTENDED_A
                      | ETHIOPIC_SUPPLEMENT
                      | GENERAL_PUNCTUATION
                      | GEOMETRIC_SHAPES
                      | GEORGIAN
                      | GEORGIAN_SUPPLEMENT
                      | GLAGOLITIC
                      | GOTHIC
                      | GREEK_AND_COPTIC
                      | GREEK_EXTENDED
                      | GUJARATI
                      | GURMUKHI
                      | HALFWIDTH_AND_FULLWIDTH_FORMS
                      | HANGUL_COMPATIBILITY_JAMO
                      | HANGUL_JAMO
                      | HANGUL_JAMO_EXTENDED_A
                      | HANGUL_JAMO_EXTENDED_B
                      | HANGUL_SYLLABLES
                      | HANUNOO
                      | HEBREW
                      | HIGH_PRIVATE_USE_SURROGATES
                      | HIGH_SURROGATES
                      | HIRAGANA
                      | IDEOGRAPHIC_DESCRIPTION_CHARACTERS
                      | IMPERIAL_ARAMAIC
                      | INSCRIPTIONAL_PAHLAVI
                      | INSCRIPTIONAL_PARTHIAN
                      | IPA_EXTENSIONS
                      | JAVANESE
                      | KAITHI
                      | KANA_SUPPLEMENT
                      | KANBUN
                      | KANGXI_RADICALS
                      | KANNADA
                      | KATAKANA
                      | KATAKANA_PHONETIC_EXTENSIONS
                      | KAYAH_LI
                      | KHAROSHTHI
                      | KHMER
                      | KHMER_SYMBOLS
                      | LAO
                      | LATIN_1_SUPPLEMENT
                      | LATIN_EXTENDED_A
                      | LATIN_EXTENDED_ADDITIONAL
                      | LATIN_EXTENDED_B
                      | LATIN_EXTENDED_C
                      | LATIN_EXTENDED_D
                      | LEPCHA
                      | LETTERLIKE_SYMBOLS
                      | LIMBU
                      | LINEAR_B_IDEOGRAMS
                      | LINEAR_B_SYLLABARY
                      | LISU
                      | LOW_SURROGATES
                      | LYCIAN
                      | LYDIAN
                      | MAHJONG_TILES
                      | MALAYALAM
                      | MANDAIC
                      | MATHEMATICAL_ALPHANUMERIC_SYMBOLS
                      | MATHEMATICAL_OPERATORS
                      | MEETEI_MAYEK
                      | MISCELLANEOUS_MATHEMATICAL_SYMBOLS_A
                      | MISCELLANEOUS_MATHEMATICAL_SYMBOLS_B
                      | MISCELLANEOUS_SYMBOLS
                      | MISCELLANEOUS_SYMBOLS_AND_ARROWS
                      | MISCELLANEOUS_SYMBOLS_AND_PICTOGRAPHS
                      | MISCELLANEOUS_TECHNICAL
                      | MODIFIER_TONE_LETTERS
                      | MONGOLIAN
                      | MUSICAL_SYMBOLS
                      | MYANMAR
                      | MYANMAR_EXTENDED_A
                      | NEW_TAI_LUE
                      | NKO
                      | NUMBER_FORMS
                      | OGHAM
                      | OLD_ITALIC
                      | OLD_PERSIAN
                      | OLD_SOUTH_ARABIAN
                      | OLD_TURKIC
                      | OL_CHIKI
                      | OPTICAL_CHARACTER_RECOGNITION
                      | ORIYA
                      | OSMANYA
                      | PHAGS_PA
                      | PHAISTOS_DISC
                      | PHOENICIAN
                      | PHONETIC_EXTENSIONS
                      | PHONETIC_EXTENSIONS_SUPPLEMENT
                      | PLAYING_CARDS
                      | PRIVATE_USE_AREA
                      | REJANG
                      | RUMI_NUMERAL_SYMBOLS
                      | RUNIC
                      | SAMARITAN
                      | SAURASHTRA
                      | SHAVIAN
                      | SINHALA
                      | SMALL_FORM_VARIANTS
                      | SPACING_MODIFIER_LETTERS
                      | SPECIALS
                      | SUNDANESE
                      | SUPERSCRIPTS_AND_SUBSCRIPTS
                      | SUPPLEMENTAL_ARROWS_A
                      | SUPPLEMENTAL_ARROWS_B
                      | SUPPLEMENTAL_MATHEMATICAL_OPERATORS
                      | SUPPLEMENTAL_PUNCTUATION
                      | SUPPLEMENTARY_PRIVATE_USE_AREA_A
                      | SUPPLEMENTARY_PRIVATE_USE_AREA_B
                      | SYLOTI_NAGRI
                      | SYRIAC
                      | TAGALOG
                      | TAGBANWA
                      | TAGS
                      | TAI_LE
                      | TAI_THAM
                      | TAI_VIET
                      | TAI_XUAN_JING_SYMBOLS
                      | TAMIL
                      | TELUGU
                      | THAANA
                      | THAI
                      | TIBETAN
                      | TIFINAGH
                      | TRANSPORT_AND_MAP_SYMBOLS
                      | UGARITIC
                      | UNIFIED_CANADIAN_ABORIGINAL_SYLLABICS
                      | UNIFIED_CANADIAN_ABORIGINAL_SYLLABICS_EXTENDED
                      | VAI
                      | VARIATION_SELECTORS
                      | VARIATION_SELECTORS_SUPPLEMENT
                      | VEDIC_EXTENSIONS
                      | VERTICAL_FORMS
                      | YIJING_HEXAGRAM_SYMBOLS
                      | YI_RADICALS
                      | YI_SYLLABLES
                        deriving (Show, Enum, Ord, Bounded, Eq)

data UnicodeBlockNumeric = UBN Int Int
                           deriving (Show, Ord, Eq)

ubnns :: [ ( UnicodeBlockName                               , UnicodeBlockNumeric   )]
ubnns =  [ ( BASIC_LATIN                                    , UBN 0x0000   0x007F   )
         , ( LATIN_1_SUPPLEMENT                             , UBN 0x0080   0x00FF   )
         , ( LATIN_EXTENDED_A                               , UBN 0x0100   0x017F   )
         , ( LATIN_EXTENDED_B                               , UBN 0x0180   0x024F   )
         , ( IPA_EXTENSIONS                                 , UBN 0x0250   0x02AF   )
         , ( SPACING_MODIFIER_LETTERS                       , UBN 0x02B0   0x02FF   )
         , ( COMBINING_DIACRITICAL_MARKS                    , UBN 0x0300   0x036F   )
         , ( GREEK_AND_COPTIC                               , UBN 0x0370   0x03FF   )
         , ( CYRILLIC                                       , UBN 0x0400   0x04FF   )
         , ( CYRILLIC_SUPPLEMENT                            , UBN 0x0500   0x052F   )
         , ( ARMENIAN                                       , UBN 0x0530   0x058F   )
         , ( HEBREW                                         , UBN 0x0590   0x05FF   )
         , ( ARABIC                                         , UBN 0x0600   0x06FF   )
         , ( SYRIAC                                         , UBN 0x0700   0x074F   )
         , ( ARABIC_SUPPLEMENT                              , UBN 0x0750   0x077F   )
         , ( THAANA                                         , UBN 0x0780   0x07BF   )
         , ( NKO                                            , UBN 0x07C0   0x07FF   )
         , ( SAMARITAN                                      , UBN 0x0800   0x083F   )
         , ( MANDAIC                                        , UBN 0x0840   0x085F   )
         , ( DEVANAGARI                                     , UBN 0x0900   0x097F   )
         , ( BENGALI                                        , UBN 0x0980   0x09FF   )
         , ( GURMUKHI                                       , UBN 0x0A00   0x0A7F   )
         , ( GUJARATI                                       , UBN 0x0A80   0x0AFF   )
         , ( ORIYA                                          , UBN 0x0B00   0x0B7F   )
         , ( TAMIL                                          , UBN 0x0B80   0x0BFF   )
         , ( TELUGU                                         , UBN 0x0C00   0x0C7F   )
         , ( KANNADA                                        , UBN 0x0C80   0x0CFF   )
         , ( MALAYALAM                                      , UBN 0x0D00   0x0D7F   )
         , ( SINHALA                                        , UBN 0x0D80   0x0DFF   )
         , ( THAI                                           , UBN 0x0E00   0x0E7F   )
         , ( LAO                                            , UBN 0x0E80   0x0EFF   )
         , ( TIBETAN                                        , UBN 0x0F00   0x0FFF   )
         , ( MYANMAR                                        , UBN 0x1000   0x109F   )
         , ( GEORGIAN                                       , UBN 0x10A0   0x10FF   )
         , ( HANGUL_JAMO                                    , UBN 0x1100   0x11FF   )
         , ( ETHIOPIC                                       , UBN 0x1200   0x137F   )
         , ( ETHIOPIC_SUPPLEMENT                            , UBN 0x1380   0x139F   )
         , ( CHEROKEE                                       , UBN 0x13A0   0x13FF   )
         , ( UNIFIED_CANADIAN_ABORIGINAL_SYLLABICS          , UBN 0x1400   0x167F   )
         , ( OGHAM                                          , UBN 0x1680   0x169F   )
         , ( RUNIC                                          , UBN 0x16A0   0x16FF   )
         , ( TAGALOG                                        , UBN 0x1700   0x171F   )
         , ( HANUNOO                                        , UBN 0x1720   0x173F   )
         , ( BUHID                                          , UBN 0x1740   0x175F   )
         , ( TAGBANWA                                       , UBN 0x1760   0x177F   )
         , ( KHMER                                          , UBN 0x1780   0x17FF   )
         , ( MONGOLIAN                                      , UBN 0x1800   0x18AF   )
         , ( UNIFIED_CANADIAN_ABORIGINAL_SYLLABICS_EXTENDED , UBN 0x18B0   0x18FF   )
         , ( LIMBU                                          , UBN 0x1900   0x194F   )
         , ( TAI_LE                                         , UBN 0x1950   0x197F   )
         , ( NEW_TAI_LUE                                    , UBN 0x1980   0x19DF   )
         , ( KHMER_SYMBOLS                                  , UBN 0x19E0   0x19FF   )
         , ( BUGINESE                                       , UBN 0x1A00   0x1A1F   )
         , ( TAI_THAM                                       , UBN 0x1A20   0x1AAF   )
         , ( BALINESE                                       , UBN 0x1B00   0x1B7F   )
         , ( SUNDANESE                                      , UBN 0x1B80   0x1BBF   )
         , ( BATAK                                          , UBN 0x1BC0   0x1BFF   )
         , ( LEPCHA                                         , UBN 0x1C00   0x1C4F   )
         , ( OL_CHIKI                                       , UBN 0x1C50   0x1C7F   )
         , ( VEDIC_EXTENSIONS                               , UBN 0x1CD0   0x1CFF   )
         , ( PHONETIC_EXTENSIONS                            , UBN 0x1D00   0x1D7F   )
         , ( PHONETIC_EXTENSIONS_SUPPLEMENT                 , UBN 0x1D80   0x1DBF   )
         , ( COMBINING_DIACRITICAL_MARKS_SUPPLEMENT         , UBN 0x1DC0   0x1DFF   )
         , ( LATIN_EXTENDED_ADDITIONAL                      , UBN 0x1E00   0x1EFF   )
         , ( GREEK_EXTENDED                                 , UBN 0x1F00   0x1FFF   )
         , ( GENERAL_PUNCTUATION                            , UBN 0x2000   0x206F   )
         , ( SUPERSCRIPTS_AND_SUBSCRIPTS                    , UBN 0x2070   0x209F   )
         , ( CURRENCY_SYMBOLS                               , UBN 0x20A0   0x20CF   )
         , ( COMBINING_DIACRITICAL_MARKS_FOR_SYMBOLS        , UBN 0x20D0   0x20FF   )
         , ( LETTERLIKE_SYMBOLS                             , UBN 0x2100   0x214F   )
         , ( NUMBER_FORMS                                   , UBN 0x2150   0x218F   )
         , ( ARROWS                                         , UBN 0x2190   0x21FF   )
         , ( MATHEMATICAL_OPERATORS                         , UBN 0x2200   0x22FF   )
         , ( MISCELLANEOUS_TECHNICAL                        , UBN 0x2300   0x23FF   )
         , ( CONTROL_PICTURES                               , UBN 0x2400   0x243F   )
         , ( OPTICAL_CHARACTER_RECOGNITION                  , UBN 0x2440   0x245F   )
         , ( ENCLOSED_ALPHANUMERICS                         , UBN 0x2460   0x24FF   )
         , ( BOX_DRAWING                                    , UBN 0x2500   0x257F   )
         , ( BLOCK_ELEMENTS                                 , UBN 0x2580   0x259F   )
         , ( GEOMETRIC_SHAPES                               , UBN 0x25A0   0x25FF   )
         , ( MISCELLANEOUS_SYMBOLS                          , UBN 0x2600   0x26FF   )
         , ( DINGBATS                                       , UBN 0x2700   0x27BF   )
         , ( MISCELLANEOUS_MATHEMATICAL_SYMBOLS_A           , UBN 0x27C0   0x27EF   )
         , ( SUPPLEMENTAL_ARROWS_A                          , UBN 0x27F0   0x27FF   )
         , ( BRAILLE_PATTERNS                               , UBN 0x2800   0x28FF   )
         , ( SUPPLEMENTAL_ARROWS_B                          , UBN 0x2900   0x297F   )
         , ( MISCELLANEOUS_MATHEMATICAL_SYMBOLS_B           , UBN 0x2980   0x29FF   )
         , ( SUPPLEMENTAL_MATHEMATICAL_OPERATORS            , UBN 0x2A00   0x2AFF   )
         , ( MISCELLANEOUS_SYMBOLS_AND_ARROWS               , UBN 0x2B00   0x2BFF   )
         , ( GLAGOLITIC                                     , UBN 0x2C00   0x2C5F   )
         , ( LATIN_EXTENDED_C                               , UBN 0x2C60   0x2C7F   )
         , ( COPTIC                                         , UBN 0x2C80   0x2CFF   )
         , ( GEORGIAN_SUPPLEMENT                            , UBN 0x2D00   0x2D2F   )
         , ( TIFINAGH                                       , UBN 0x2D30   0x2D7F   )
         , ( ETHIOPIC_EXTENDED                              , UBN 0x2D80   0x2DDF   )
         , ( CYRILLIC_EXTENDED_A                            , UBN 0x2DE0   0x2DFF   )
         , ( SUPPLEMENTAL_PUNCTUATION                       , UBN 0x2E00   0x2E7F   )
         , ( CJK_RADICALS_SUPPLEMENT                        , UBN 0x2E80   0x2EFF   )
         , ( KANGXI_RADICALS                                , UBN 0x2F00   0x2FDF   )
         , ( IDEOGRAPHIC_DESCRIPTION_CHARACTERS             , UBN 0x2FF0   0x2FFF   )
         , ( CJK_SYMBOLS_AND_PUNCTUATION                    , UBN 0x3000   0x303F   )
         , ( HIRAGANA                                       , UBN 0x3040   0x309F   )
         , ( KATAKANA                                       , UBN 0x30A0   0x30FF   )
         , ( BOPOMOFO                                       , UBN 0x3100   0x312F   )
         , ( HANGUL_COMPATIBILITY_JAMO                      , UBN 0x3130   0x318F   )
         , ( KANBUN                                         , UBN 0x3190   0x319F   )
         , ( BOPOMOFO_EXTENDED                              , UBN 0x31A0   0x31BF   )
         , ( CJK_STROKES                                    , UBN 0x31C0   0x31EF   )
         , ( KATAKANA_PHONETIC_EXTENSIONS                   , UBN 0x31F0   0x31FF   )
         , ( ENCLOSED_CJK_LETTERS_AND_MONTHS                , UBN 0x3200   0x32FF   )
         , ( CJK_COMPATIBILITY                              , UBN 0x3300   0x33FF   )
         , ( CJK_UNIFIED_IDEOGRAPHS_EXTENSION_A             , UBN 0x3400   0x4DBF   )
         , ( YIJING_HEXAGRAM_SYMBOLS                        , UBN 0x4DC0   0x4DFF   )
         , ( CJK_UNIFIED_IDEOGRAPHS                         , UBN 0x4E00   0x9FFF   )
         , ( YI_SYLLABLES                                   , UBN 0xA000   0xA48F   )
         , ( YI_RADICALS                                    , UBN 0xA490   0xA4CF   )
         , ( LISU                                           , UBN 0xA4D0   0xA4FF   )
         , ( VAI                                            , UBN 0xA500   0xA63F   )
         , ( CYRILLIC_EXTENDED_B                            , UBN 0xA640   0xA69F   )
         , ( BAMUM                                          , UBN 0xA6A0   0xA6FF   )
         , ( MODIFIER_TONE_LETTERS                          , UBN 0xA700   0xA71F   )
         , ( LATIN_EXTENDED_D                               , UBN 0xA720   0xA7FF   )
         , ( SYLOTI_NAGRI                                   , UBN 0xA800   0xA82F   )
         , ( COMMON_INDIC_NUMBER_FORMS                      , UBN 0xA830   0xA83F   )
         , ( PHAGS_PA                                       , UBN 0xA840   0xA87F   )
         , ( SAURASHTRA                                     , UBN 0xA880   0xA8DF   )
         , ( DEVANAGARI_EXTENDED                            , UBN 0xA8E0   0xA8FF   )
         , ( KAYAH_LI                                       , UBN 0xA900   0xA92F   )
         , ( REJANG                                         , UBN 0xA930   0xA95F   )
         , ( HANGUL_JAMO_EXTENDED_A                         , UBN 0xA960   0xA97F   )
         , ( JAVANESE                                       , UBN 0xA980   0xA9DF   )
         , ( CHAM                                           , UBN 0xAA00   0xAA5F   )
         , ( MYANMAR_EXTENDED_A                             , UBN 0xAA60   0xAA7F   )
         , ( TAI_VIET                                       , UBN 0xAA80   0xAADF   )
         , ( ETHIOPIC_EXTENDED_A                            , UBN 0xAB00   0xAB2F   )
         , ( MEETEI_MAYEK                                   , UBN 0xABC0   0xABFF   )
         , ( HANGUL_SYLLABLES                               , UBN 0xAC00   0xD7AF   )
         , ( HANGUL_JAMO_EXTENDED_B                         , UBN 0xD7B0   0xD7FF   )
         , ( HIGH_SURROGATES                                , UBN 0xD800   0xDB7F   )
         , ( HIGH_PRIVATE_USE_SURROGATES                    , UBN 0xDB80   0xDBFF   )
         , ( LOW_SURROGATES                                 , UBN 0xDC00   0xDFFF   )
         , ( PRIVATE_USE_AREA                               , UBN 0xE000   0xF8FF   )
         , ( CJK_COMPATIBILITY_IDEOGRAPHS                   , UBN 0xF900   0xFAFF   )
         , ( ALPHABETIC_PRESENTATION_FORMS                  , UBN 0xFB00   0xFB4F   )
         , ( ARABIC_PRESENTATION_FORMS_A                    , UBN 0xFB50   0xFDFF   )
         , ( VARIATION_SELECTORS                            , UBN 0xFE00   0xFE0F   )
         , ( VERTICAL_FORMS                                 , UBN 0xFE10   0xFE1F   )
         , ( COMBINING_HALF_MARKS                           , UBN 0xFE20   0xFE2F   )
         , ( CJK_COMPATIBILITY_FORMS                        , UBN 0xFE30   0xFE4F   )
         , ( SMALL_FORM_VARIANTS                            , UBN 0xFE50   0xFE6F   )
         , ( ARABIC_PRESENTATION_FORMS_B                    , UBN 0xFE70   0xFEFF   )
         , ( HALFWIDTH_AND_FULLWIDTH_FORMS                  , UBN 0xFF00   0xFFEF   )
         , ( SPECIALS                                       , UBN 0xFFF0   0xFFFF   )
         , ( LINEAR_B_SYLLABARY                             , UBN 0x10000  0x1007F  )
         , ( LINEAR_B_IDEOGRAMS                             , UBN 0x10080  0x100FF  )
         , ( AEGEAN_NUMBERS                                 , UBN 0x10100  0x1013F  )
         , ( ANCIENT_GREEK_NUMBERS                          , UBN 0x10140  0x1018F  )
         , ( ANCIENT_SYMBOLS                                , UBN 0x10190  0x101CF  )
         , ( PHAISTOS_DISC                                  , UBN 0x101D0  0x101FF  )
         , ( LYCIAN                                         , UBN 0x10280  0x1029F  )
         , ( CARIAN                                         , UBN 0x102A0  0x102DF  )
         , ( OLD_ITALIC                                     , UBN 0x10300  0x1032F  )
         , ( GOTHIC                                         , UBN 0x10330  0x1034F  )
         , ( UGARITIC                                       , UBN 0x10380  0x1039F  )
         , ( OLD_PERSIAN                                    , UBN 0x103A0  0x103DF  )
         , ( DESERET                                        , UBN 0x10400  0x1044F  )
         , ( SHAVIAN                                        , UBN 0x10450  0x1047F  )
         , ( OSMANYA                                        , UBN 0x10480  0x104AF  )
         , ( CYPRIOT_SYLLABARY                              , UBN 0x10800  0x1083F  )
         , ( IMPERIAL_ARAMAIC                               , UBN 0x10840  0x1085F  )
         , ( PHOENICIAN                                     , UBN 0x10900  0x1091F  )
         , ( LYDIAN                                         , UBN 0x10920  0x1093F  )
         , ( KHAROSHTHI                                     , UBN 0x10A00  0x10A5F  )
         , ( OLD_SOUTH_ARABIAN                              , UBN 0x10A60  0x10A7F  )
         , ( AVESTAN                                        , UBN 0x10B00  0x10B3F  )
         , ( INSCRIPTIONAL_PARTHIAN                         , UBN 0x10B40  0x10B5F  )
         , ( INSCRIPTIONAL_PAHLAVI                          , UBN 0x10B60  0x10B7F  )
         , ( OLD_TURKIC                                     , UBN 0x10C00  0x10C4F  )
         , ( RUMI_NUMERAL_SYMBOLS                           , UBN 0x10E60  0x10E7F  )
         , ( BRAHMI                                         , UBN 0x11000  0x1107F  )
         , ( KAITHI                                         , UBN 0x11080  0x110CF  )
         , ( CUNEIFORM                                      , UBN 0x12000  0x123FF  )
         , ( CUNEIFORM_NUMBERS_AND_PUNCTUATION              , UBN 0x12400  0x1247F  )
         , ( EGYPTIAN_HIEROGLYPHS                           , UBN 0x13000  0x1342F  )
         , ( BAMUM_SUPPLEMENT                               , UBN 0x16800  0x16A3F  )
         , ( KANA_SUPPLEMENT                                , UBN 0x1B000  0x1B0FF  )
         , ( BYZANTINE_MUSICAL_SYMBOLS                      , UBN 0x1D000  0x1D0FF  )
         , ( MUSICAL_SYMBOLS                                , UBN 0x1D100  0x1D1FF  )
         , ( ANCIENT_GREEK_MUSICAL_NOTATION                 , UBN 0x1D200  0x1D24F  )
         , ( TAI_XUAN_JING_SYMBOLS                          , UBN 0x1D300  0x1D35F  )
         , ( COUNTING_ROD_NUMERALS                          , UBN 0x1D360  0x1D37F  )
         , ( MATHEMATICAL_ALPHANUMERIC_SYMBOLS              , UBN 0x1D400  0x1D7FF  )
         , ( MAHJONG_TILES                                  , UBN 0x1F000  0x1F02F  )
         , ( DOMINO_TILES                                   , UBN 0x1F030  0x1F09F  )
         , ( PLAYING_CARDS                                  , UBN 0x1F0A0  0x1F0FF  )
         , ( ENCLOSED_ALPHANUMERIC_SUPPLEMENT               , UBN 0x1F100  0x1F1FF  )
         , ( ENCLOSED_IDEOGRAPHIC_SUPPLEMENT                , UBN 0x1F200  0x1F2FF  )
         , ( MISCELLANEOUS_SYMBOLS_AND_PICTOGRAPHS          , UBN 0x1F300  0x1F5FF  )
         , ( EMOTICONS                                      , UBN 0x1F600  0x1F64F  )
         , ( TRANSPORT_AND_MAP_SYMBOLS                      , UBN 0x1F680  0x1F6FF  )
         , ( ALCHEMICAL_SYMBOLS                             , UBN 0x1F700  0x1F77F  )
         , ( CJK_UNIFIED_IDEOGRAPHS_EXTENSION_B             , UBN 0x20000  0x2A6DF  )
         , ( CJK_UNIFIED_IDEOGRAPHS_EXTENSION_C             , UBN 0x2A700  0x2B73F  )
         , ( CJK_UNIFIED_IDEOGRAPHS_EXTENSION_D             , UBN 0x2B740  0x2B81F  )
         , ( CJK_COMPATIBILITY_IDEOGRAPHS_SUPPLEMENT        , UBN 0x2F800  0x2FA1F  )
         , ( TAGS                                           , UBN 0xE0000  0xE007F  )
         , ( VARIATION_SELECTORS_SUPPLEMENT                 , UBN 0xE0100  0xE01EF  )
         , ( SUPPLEMENTARY_PRIVATE_USE_AREA_A               , UBN 0xF0000  0xFFFFF  )
         , ( SUPPLEMENTARY_PRIVATE_USE_AREA_B               , UBN 0x100000 0x10FFFF )
         ]

-- | § G.4.2.5 Multi-character escapes
--   A multi-character escape provides a simple way to identify any of a commonly used set of characters.
data MultiCharEsc = MultiCharEsc Char
                    deriving (Show, Eq)

multiCharEsc :: Parser MultiCharEsc
multiCharEsc = do
  skipC '\\'
  satisfy (inClass "sSiIcCdDwW") >>= pure . MultiCharEsc

-- | The wildcard character is a metacharacter which matches almost any single character
data WildcardEsc = WildcardEsc
                   deriving (Show, Eq)

wildCardEsc :: Parser WildcardEsc
wildCardEsc  = skipC '.' >> pure WildcardEsc

-- End of § G
-- -----------------------------------------------------------------------------------------------------------------------------------------------------

{-

# Blocks-6.0.0.txt
# Date: 2010-06-04, 11:12:00 PDT [KW]
#
# Unicode Character Database
# Copyright (c) 1991-2010 Unicode, Inc.
# For terms of use, see http://www.unicode.org/terms_of_use.html
# For documentation, see http://www.unicode.org/reports/tr44/
#
# Note:   The casing of block names is not normative.
#         For example, "Basic Latin" and "BASIC LATIN" are equivalent.
#
# Format:
# Start Code..End Code; Block Name

# ================================================

# Note:   When comparing block names, casing, whitespace, hyphens,
#         and underbars are ignored.
#         For example, "Latin Extended-A" and "latin extended a" are equivalent.
#         For more information on the comparison of property values,
#            see UAX #44: http://www.unicode.org/reports/tr44/
#
#  All code points not explicitly listed for Block
#  have the value No_Block.

# Property:	Block
#
# @missing: 0000..10FFFF; No_Block

0000..007F; Basic Latin
0080..00FF; Latin-1 Supplement
0100..017F; Latin Extended-A
0180..024F; Latin Extended-B
0250..02AF; IPA Extensions
02B0..02FF; Spacing Modifier Letters
0300..036F; Combining Diacritical Marks
0370..03FF; Greek and Coptic
0400..04FF; Cyrillic
0500..052F; Cyrillic Supplement
0530..058F; Armenian
0590..05FF; Hebrew
0600..06FF; Arabic
0700..074F; Syriac
0750..077F; Arabic Supplement
0780..07BF; Thaana
07C0..07FF; NKo
0800..083F; Samaritan
0840..085F; Mandaic
0900..097F; Devanagari
0980..09FF; Bengali
0A00..0A7F; Gurmukhi
0A80..0AFF; Gujarati
0B00..0B7F; Oriya
0B80..0BFF; Tamil
0C00..0C7F; Telugu
0C80..0CFF; Kannada
0D00..0D7F; Malayalam
0D80..0DFF; Sinhala
0E00..0E7F; Thai
0E80..0EFF; Lao
0F00..0FFF; Tibetan
1000..109F; Myanmar
10A0..10FF; Georgian
1100..11FF; Hangul Jamo
1200..137F; Ethiopic
1380..139F; Ethiopic Supplement
13A0..13FF; Cherokee
1400..167F; Unified Canadian Aboriginal Syllabics
1680..169F; Ogham
16A0..16FF; Runic
1700..171F; Tagalog
1720..173F; Hanunoo
1740..175F; Buhid
1760..177F; Tagbanwa
1780..17FF; Khmer
1800..18AF; Mongolian
18B0..18FF; Unified Canadian Aboriginal Syllabics Extended
1900..194F; Limbu
1950..197F; Tai Le
1980..19DF; New Tai Lue
19E0..19FF; Khmer Symbols
1A00..1A1F; Buginese
1A20..1AAF; Tai Tham
1B00..1B7F; Balinese
1B80..1BBF; Sundanese
1BC0..1BFF; Batak
1C00..1C4F; Lepcha
1C50..1C7F; Ol Chiki
1CD0..1CFF; Vedic Extensions
1D00..1D7F; Phonetic Extensions
1D80..1DBF; Phonetic Extensions Supplement
1DC0..1DFF; Combining Diacritical Marks Supplement
1E00..1EFF; Latin Extended Additional
1F00..1FFF; Greek Extended
2000..206F; General Punctuation
2070..209F; Superscripts and Subscripts
20A0..20CF; Currency Symbols
20D0..20FF; Combining Diacritical Marks for Symbols
2100..214F; Letterlike Symbols
2150..218F; Number Forms
2190..21FF; Arrows
2200..22FF; Mathematical Operators
2300..23FF; Miscellaneous Technical
2400..243F; Control Pictures
2440..245F; Optical Character Recognition
2460..24FF; Enclosed Alphanumerics
2500..257F; Box Drawing
2580..259F; Block Elements
25A0..25FF; Geometric Shapes
2600..26FF; Miscellaneous Symbols
2700..27BF; Dingbats
27C0..27EF; Miscellaneous Mathematical Symbols-A
27F0..27FF; Supplemental Arrows-A
2800..28FF; Braille Patterns
2900..297F; Supplemental Arrows-B
2980..29FF; Miscellaneous Mathematical Symbols-B
2A00..2AFF; Supplemental Mathematical Operators
2B00..2BFF; Miscellaneous Symbols and Arrows
2C00..2C5F; Glagolitic
2C60..2C7F; Latin Extended-C
2C80..2CFF; Coptic
2D00..2D2F; Georgian Supplement
2D30..2D7F; Tifinagh
2D80..2DDF; Ethiopic Extended
2DE0..2DFF; Cyrillic Extended-A
2E00..2E7F; Supplemental Punctuation
2E80..2EFF; CJK Radicals Supplement
2F00..2FDF; Kangxi Radicals
2FF0..2FFF; Ideographic Description Characters
3000..303F; CJK Symbols and Punctuation
3040..309F; Hiragana
30A0..30FF; Katakana
3100..312F; Bopomofo
3130..318F; Hangul Compatibility Jamo
3190..319F; Kanbun
31A0..31BF; Bopomofo Extended
31C0..31EF; CJK Strokes
31F0..31FF; Katakana Phonetic Extensions
3200..32FF; Enclosed CJK Letters and Months
3300..33FF; CJK Compatibility
3400..4DBF; CJK Unified Ideographs Extension A
4DC0..4DFF; Yijing Hexagram Symbols
4E00..9FFF; CJK Unified Ideographs
A000..A48F; Yi Syllables
A490..A4CF; Yi Radicals
A4D0..A4FF; Lisu
A500..A63F; Vai
A640..A69F; Cyrillic Extended-B
A6A0..A6FF; Bamum
A700..A71F; Modifier Tone Letters
A720..A7FF; Latin Extended-D
A800..A82F; Syloti Nagri
A830..A83F; Common Indic Number Forms
A840..A87F; Phags-pa
A880..A8DF; Saurashtra
A8E0..A8FF; Devanagari Extended
A900..A92F; Kayah Li
A930..A95F; Rejang
A960..A97F; Hangul Jamo Extended-A
A980..A9DF; Javanese
AA00..AA5F; Cham
AA60..AA7F; Myanmar Extended-A
AA80..AADF; Tai Viet
AB00..AB2F; Ethiopic Extended-A
ABC0..ABFF; Meetei Mayek
AC00..D7AF; Hangul Syllables
D7B0..D7FF; Hangul Jamo Extended-B
D800..DB7F; High Surrogates
DB80..DBFF; High Private Use Surrogates
DC00..DFFF; Low Surrogates
E000..F8FF; Private Use Area
F900..FAFF; CJK Compatibility Ideographs
FB00..FB4F; Alphabetic Presentation Forms
FB50..FDFF; Arabic Presentation Forms-A
FE00..FE0F; Variation Selectors
FE10..FE1F; Vertical Forms
FE20..FE2F; Combining Half Marks
FE30..FE4F; CJK Compatibility Forms
FE50..FE6F; Small Form Variants
FE70..FEFF; Arabic Presentation Forms-B
FF00..FFEF; Halfwidth and Fullwidth Forms
FFF0..FFFF; Specials
10000..1007F; Linear B Syllabary
10080..100FF; Linear B Ideograms
10100..1013F; Aegean Numbers
10140..1018F; Ancient Greek Numbers
10190..101CF; Ancient Symbols
101D0..101FF; Phaistos Disc
10280..1029F; Lycian
102A0..102DF; Carian
10300..1032F; Old Italic
10330..1034F; Gothic
10380..1039F; Ugaritic
103A0..103DF; Old Persian
10400..1044F; Deseret
10450..1047F; Shavian
10480..104AF; Osmanya
10800..1083F; Cypriot Syllabary
10840..1085F; Imperial Aramaic
10900..1091F; Phoenician
10920..1093F; Lydian
10A00..10A5F; Kharoshthi
10A60..10A7F; Old South Arabian
10B00..10B3F; Avestan
10B40..10B5F; Inscriptional Parthian
10B60..10B7F; Inscriptional Pahlavi
10C00..10C4F; Old Turkic
10E60..10E7F; Rumi Numeral Symbols
11000..1107F; Brahmi
11080..110CF; Kaithi
12000..123FF; Cuneiform
12400..1247F; Cuneiform Numbers and Punctuation
13000..1342F; Egyptian Hieroglyphs
16800..16A3F; Bamum Supplement
1B000..1B0FF; Kana Supplement
1D000..1D0FF; Byzantine Musical Symbols
1D100..1D1FF; Musical Symbols
1D200..1D24F; Ancient Greek Musical Notation
1D300..1D35F; Tai Xuan Jing Symbols
1D360..1D37F; Counting Rod Numerals
1D400..1D7FF; Mathematical Alphanumeric Symbols
1F000..1F02F; Mahjong Tiles
1F030..1F09F; Domino Tiles
1F0A0..1F0FF; Playing Cards
1F100..1F1FF; Enclosed Alphanumeric Supplement
1F200..1F2FF; Enclosed Ideographic Supplement
1F300..1F5FF; Miscellaneous Symbols And Pictographs
1F600..1F64F; Emoticons
1F680..1F6FF; Transport And Map Symbols
1F700..1F77F; Alchemical Symbols
20000..2A6DF; CJK Unified Ideographs Extension B
2A700..2B73F; CJK Unified Ideographs Extension C
2B740..2B81F; CJK Unified Ideographs Extension D
2F800..2FA1F; CJK Compatibility Ideographs Supplement
E0000..E007F; Tags
E0100..E01EF; Variation Selectors Supplement
F0000..FFFFF; Supplementary Private Use Area-A
100000..10FFFF; Supplementary Private Use Area-B



-}
