{-# Language ExistentialQuantification, MultiParamTypeClasses, TupleSections
  , FlexibleInstances, GeneralizedNewtypeDeriving, NegativeLiterals, MultiWayIf #-}
{-| Time-stamp: <2018-07-04 22:30:23 CDT>

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

-- Qualified Imports

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Text as T
import qualified Numeric   as N

import qualified Data.Map.Strict as M

-- Undisciplined Imports

import ClassyPrelude
import Data.Attoparsec.Text

-- End of Imports
-- -----------------------------------------------------------------------------------------------------------------------------------------------------

-- Objectives
-- 1. Create AST from XSD regex     : Take XML Schema 1.1 regex string and produce Aeson Parser AST.
-- 2. Create XSD regex from AST     : Take Aeson Parser AST and produce XML Schema 1.1 regex string.
-- 3. Validate string               : Take (XML string, Aeson Parser AST) and produce {True, False}.
-- 4. Create non-XSD regex from AST : Take Aeson Parser AST and produce non-XML Schema 1.1 regex string.

class TransRegex a where             -- TransRegex ➙ transform
  scribeBR :: Bool -> a -> Text
  canonR :: a -> Text                -- canonR     ➙ canonical text
  canonR a  = scribeBR True a
  scribeR :: a -> Text               -- scribeR    ➙ write text
  scribeR a = scribeBR False a

instance TransRegex RE
  where scribeBR b (RE branches) = T.intercalate "|" $ map (scribeBR b) branches

instance TransRegex Branch
  where scribeBR b (Branch pieces) = T.concat $ map (scribeBR b) pieces

instance TransRegex Piece
  where scribeBR b (Piece atm mquant) = T.append (scribeBR b atm) $ maybe T.empty (scribeBR b) mquant

instance TransRegex Atom
  where scribeBR b (AtomNormal    atm) = scribeBR b atm
        scribeBR b (AtomCharClass atm) = scribeBR b atm
        scribeBR b (AtomRE        atm) = T.concat ["(", scribeBR b atm, ")"]

instance TransRegex Quantifier
  where scribeBR _b QuantifierMaybeOne        = T.singleton '?'
        scribeBR _b QuantifierMaybeMany       = T.singleton '*'
        scribeBR _b QuantifierMany            = T.singleton '+'
        scribeBR b (QuintifierQuantity quant) = T.concat ["{", (scribeBR b quant), "}"]

instance TransRegex Quantity
  where scribeBR b (QuantRange  qel qer) = T.concat [scribeBR b qel, ",", scribeBR b qer]
        scribeBR b (QuantMin    qe) = T.append (scribeBR b qe) ","
        scribeBR b (QuantExactQ qe) = scribeBR b qe

instance TransRegex QuantExact
  where scribeBR _b (QuantExact i) = tShow i

instance TransRegex NormalChar
  where scribeBR False (NormalChar c) = T.singleton c
        scribeBR True  (NormalChar c) = case M.lookup (Left $ C.ord c) eNumEntityMap of
                                          Nothing -> if | C.ord c <= 0x1FFF -> T.singleton c
                                                        | otherwise -> 
                                                            let stringA = (N.showHex $ C.ord c) ""
                                                                stringB = if | L.length stringA < 6 -> replicate (6 - L.length stringA) '0' ++ stringA
                                                                             | otherwise -> stringA
                                                            in T.concat ["&#x", T.toUpper $ T.pack stringB, ";"]
                                          Just t -> T.concat ["&", t, ";"]

instance TransRegex CharClass
  where scribeBR b (CharClassSingle cc) = scribeBR b cc
        scribeBR b (CharClassEscC   cc) = scribeBR b cc
        scribeBR b (CharClassExprC  cc) = scribeBR b cc
        scribeBR b (CharClassWild   cc) = scribeBR b cc

instance TransRegex CharClassExpr
  where scribeBR b (CharClassExpr cg) = T.concat ["[", scribeBR b cg, "]"]

instance TransRegex CharGroup
  where scribeBR b (CharGroup (Left  cg) mce) = T.append (scribeBR b cg)
                                              $ maybe T.empty (T.cons '-' . scribeBR b) mce
        scribeBR b (CharGroup (Right cg) mce) = T.append (scribeBR b cg)
                                              $ maybe T.empty (T.cons '-' . scribeBR b) mce

instance TransRegex NegCharGroup
  where scribeBR b (NegCharGroup pcg) = T.cons '^' $ scribeBR b pcg

instance TransRegex PosCharGroup
  where scribeBR b (PosCharGroup cgp) = T.concat $ map (scribeBR b) cgp

instance TransRegex CharGroupPart
  where scribeBR b (CharGroupPartSingle   cgp) = scribeBR b cgp
        scribeBR b (CharGroupPartRange    cgp) = scribeBR b cgp
        scribeBR b (CharGroupPartClassEsc cgp) = scribeBR b cgp

instance TransRegex CharClassEsc
  where scribeBR b (CharClassEscMultiCharEsc cesc) = scribeBR b cesc
        scribeBR b (CharClassEscCatEsc       cesc) = scribeBR b cesc
        scribeBR b (CharClassEscComplEsc     cesc) = scribeBR b cesc

instance TransRegex CharRange
  where scribeBR b (CharRange l r) = T.concat [scribeBR b l, "-", scribeBR b r]

instance TransRegex SingleChar
  where scribeBR b (SingleChar (Left  se )) = scribeBR b se
        scribeBR b (SingleChar (Right sne)) = scribeBR b sne

instance TransRegex SingleCharNoEsc
  where scribeBR False (SingleCharNoEsc c) = T.singleton c
        scribeBR True  (SingleCharNoEsc c) = canonR $ NormalChar c

instance TransRegex SingleCharEsc
  where scribeBR _b (SingleCharEsc c) = T.cons '\\' $ T.singleton c

instance TransRegex CatEsc
  where scribeBR b (CatEsc cp) = T.concat ["\\p{", scribeBR b cp, "}"]

instance TransRegex ComplEsc
  where scribeBR b (ComplEsc cp) = T.concat ["\\P{", scribeBR b cp, "}"]

instance TransRegex CharProp
  where scribeBR b (CharProp (Left  i)) = scribeBR b i
        scribeBR b (CharProp (Right i)) = scribeBR b i

instance TransRegex IsCategory
  where scribeBR b (LettersCat     cat) = scribeBR b cat
        scribeBR b (MarksCat       cat) = scribeBR b cat
        scribeBR b (NumbersCat     cat) = scribeBR b cat
        scribeBR b (PunctuationCat cat) = scribeBR b cat
        scribeBR b (SeparatorsCat  cat) = scribeBR b cat
        scribeBR b (SymbolsCat     cat) = scribeBR b cat
        scribeBR b (OthersCat      cat) = scribeBR b cat

instance TransRegex Letters
  where scribeBR _b = tShow

instance TransRegex Marks
  where scribeBR _b = tShow

instance TransRegex Numbers
  where scribeBR _b = tShow

instance TransRegex Punctuation
  where scribeBR _b = tShow

instance TransRegex Separators
  where scribeBR _b = tShow

instance TransRegex Symbols
  where scribeBR _b = tShow

instance TransRegex Others
  where scribeBR _b = tShow

instance TransRegex IsBlock
  where scribeBR False (IsBlock _ t) = t
        scribeBR True  (IsBlock u _) = T.append "Is" $ scribeBR True u

instance TransRegex MultiCharEsc
  where scribeBR _b (MultiCharEsc c) = T.cons '\\' $ T.singleton c

instance TransRegex WildcardEsc
  where scribeBR _b WildcardEsc = "."

instance TransRegex UnicodeBlockName
  where scribeBR _b UNRECOGNIZED_BLOCK                             = "UnrecognizedBlock"
        scribeBR _b BASIC_LATIN                                    = "BasicLatin"
        scribeBR _b LATIN_1_SUPPLEMENT                             = "Latin-1Supplement"
        scribeBR _b LATIN_EXTENDED_A                               = "LatinExtended-A"
        scribeBR _b LATIN_EXTENDED_B                               = "LatinExtended-B"
        scribeBR _b IPA_EXTENSIONS                                 = "IPAExtensions"
        scribeBR _b SPACING_MODIFIER_LETTERS                       = "SpacingModifierLetters"
        scribeBR _b COMBINING_DIACRITICAL_MARKS                    = "CombiningDiacriticalMarks"
        scribeBR _b GREEK_AND_COPTIC                               = "GreekandCoptic"
        scribeBR _b CYRILLIC                                       = "Cyrillic"
        scribeBR _b CYRILLIC_SUPPLEMENT                            = "CyrillicSupplement"
        scribeBR _b ARMENIAN                                       = "Armenian"
        scribeBR _b HEBREW                                         = "Hebrew"
        scribeBR _b ARABIC                                         = "Arabic"
        scribeBR _b SYRIAC                                         = "Syriac"
        scribeBR _b ARABIC_SUPPLEMENT                              = "ArabicSupplement"
        scribeBR _b THAANA                                         = "Thaana"
        scribeBR _b NKO                                            = "NKo"
        scribeBR _b SAMARITAN                                      = "Samaritan"
        scribeBR _b MANDAIC                                        = "Mandaic"
        scribeBR _b DEVANAGARI                                     = "Devanagari"
        scribeBR _b BENGALI                                        = "Bengali"
        scribeBR _b GURMUKHI                                       = "Gurmukhi"
        scribeBR _b GUJARATI                                       = "Gujarati"
        scribeBR _b ORIYA                                          = "Oriya"
        scribeBR _b TAMIL                                          = "Tamil"
        scribeBR _b TELUGU                                         = "Telugu"
        scribeBR _b KANNADA                                        = "Kannada"
        scribeBR _b MALAYALAM                                      = "Malayalam"
        scribeBR _b SINHALA                                        = "Sinhala"
        scribeBR _b THAI                                           = "Thai"
        scribeBR _b LAO                                            = "Lao"
        scribeBR _b TIBETAN                                        = "Tibetan"
        scribeBR _b MYANMAR                                        = "Myanmar"
        scribeBR _b GEORGIAN                                       = "Georgian"
        scribeBR _b HANGUL_JAMO                                    = "HangulJamo"
        scribeBR _b ETHIOPIC                                       = "Ethiopic"
        scribeBR _b ETHIOPIC_SUPPLEMENT                            = "EthiopicSupplement"
        scribeBR _b CHEROKEE                                       = "Cherokee"
        scribeBR _b UNIFIED_CANADIAN_ABORIGINAL_SYLLABICS          = "UnifiedCanadianAboriginalSyllabics"
        scribeBR _b OGHAM                                          = "Ogham"
        scribeBR _b RUNIC                                          = "Runic"
        scribeBR _b TAGALOG                                        = "Tagalog"
        scribeBR _b HANUNOO                                        = "Hanunoo"
        scribeBR _b BUHID                                          = "Buhid"
        scribeBR _b TAGBANWA                                       = "Tagbanwa"
        scribeBR _b KHMER                                          = "Khmer"
        scribeBR _b MONGOLIAN                                      = "Mongolian"
        scribeBR _b UNIFIED_CANADIAN_ABORIGINAL_SYLLABICS_EXTENDED = "UnifiedCanadianAboriginalSyllabicsExtended"
        scribeBR _b LIMBU                                          = "Limbu"
        scribeBR _b TAI_LE                                         = "TaiLe"
        scribeBR _b NEW_TAI_LUE                                    = "NewTaiLue"
        scribeBR _b KHMER_SYMBOLS                                  = "KhmerSymbols"
        scribeBR _b BUGINESE                                       = "Buginese"
        scribeBR _b TAI_THAM                                       = "TaiTham"
        scribeBR _b BALINESE                                       = "Balinese"
        scribeBR _b SUNDANESE                                      = "Sundanese"
        scribeBR _b BATAK                                          = "Batak"
        scribeBR _b LEPCHA                                         = "Lepcha"
        scribeBR _b OL_CHIKI                                       = "OlChiki"
        scribeBR _b VEDIC_EXTENSIONS                               = "VedicExtensions"
        scribeBR _b PHONETIC_EXTENSIONS                            = "PhoneticExtensions"
        scribeBR _b PHONETIC_EXTENSIONS_SUPPLEMENT                 = "PhoneticExtensionsSupplement"
        scribeBR _b COMBINING_DIACRITICAL_MARKS_SUPPLEMENT         = "CombiningDiacriticalMarksSupplement"
        scribeBR _b LATIN_EXTENDED_ADDITIONAL                      = "LatinExtendedAdditional"
        scribeBR _b GREEK_EXTENDED                                 = "GreekExtended"
        scribeBR _b GENERAL_PUNCTUATION                            = "GeneralPunctuation"
        scribeBR _b SUPERSCRIPTS_AND_SUBSCRIPTS                    = "SuperscriptsandSubscripts"
        scribeBR _b CURRENCY_SYMBOLS                               = "CurrencySymbols"
        scribeBR _b COMBINING_DIACRITICAL_MARKS_FOR_SYMBOLS        = "CombiningDiacriticalMarksforSymbols"
        scribeBR _b LETTERLIKE_SYMBOLS                             = "LetterlikeSymbols"
        scribeBR _b NUMBER_FORMS                                   = "NumberForms"
        scribeBR _b ARROWS                                         = "Arrows"
        scribeBR _b MATHEMATICAL_OPERATORS                         = "MathematicalOperators"
        scribeBR _b MISCELLANEOUS_TECHNICAL                        = "MiscellaneousTechnical"
        scribeBR _b CONTROL_PICTURES                               = "ControlPictures"
        scribeBR _b OPTICAL_CHARACTER_RECOGNITION                  = "OpticalCharacterRecognition"
        scribeBR _b ENCLOSED_ALPHANUMERICS                         = "EnclosedAlphanumerics"
        scribeBR _b BOX_DRAWING                                    = "BoxDrawing"
        scribeBR _b BLOCK_ELEMENTS                                 = "BlockElements"
        scribeBR _b GEOMETRIC_SHAPES                               = "GeometricShapes"
        scribeBR _b MISCELLANEOUS_SYMBOLS                          = "MiscellaneousSymbols"
        scribeBR _b DINGBATS                                       = "Dingbats"
        scribeBR _b MISCELLANEOUS_MATHEMATICAL_SYMBOLS_A           = "MiscellaneousMathematicalSymbols-A"
        scribeBR _b SUPPLEMENTAL_ARROWS_A                          = "SupplementalArrows-A"
        scribeBR _b BRAILLE_PATTERNS                               = "BraillePatterns"
        scribeBR _b SUPPLEMENTAL_ARROWS_B                          = "SupplementalArrows-B"
        scribeBR _b MISCELLANEOUS_MATHEMATICAL_SYMBOLS_B           = "MiscellaneousMathematicalSymbols-B"
        scribeBR _b SUPPLEMENTAL_MATHEMATICAL_OPERATORS            = "SupplementalMathematicalOperators"
        scribeBR _b MISCELLANEOUS_SYMBOLS_AND_ARROWS               = "MiscellaneousSymbolsandArrows"
        scribeBR _b GLAGOLITIC                                     = "Glagolitic"
        scribeBR _b LATIN_EXTENDED_C                               = "LatinExtended-C"
        scribeBR _b COPTIC                                         = "Coptic"
        scribeBR _b GEORGIAN_SUPPLEMENT                            = "GeorgianSupplement"
        scribeBR _b TIFINAGH                                       = "Tifinagh"
        scribeBR _b ETHIOPIC_EXTENDED                              = "EthiopicExtended"
        scribeBR _b CYRILLIC_EXTENDED_A                            = "CyrillicExtended-A"
        scribeBR _b SUPPLEMENTAL_PUNCTUATION                       = "SupplementalPunctuation"
        scribeBR _b CJK_RADICALS_SUPPLEMENT                        = "CJKRadicalsSupplement"
        scribeBR _b KANGXI_RADICALS                                = "KangxiRadicals"
        scribeBR _b IDEOGRAPHIC_DESCRIPTION_CHARACTERS             = "IdeographicDescriptionCharacters"
        scribeBR _b CJK_SYMBOLS_AND_PUNCTUATION                    = "CJKSymbolsandPunctuation"
        scribeBR _b HIRAGANA                                       = "Hiragana"
        scribeBR _b KATAKANA                                       = "Katakana"
        scribeBR _b BOPOMOFO                                       = "Bopomofo"
        scribeBR _b HANGUL_COMPATIBILITY_JAMO                      = "HangulCompatibilityJamo"
        scribeBR _b KANBUN                                         = "Kanbun"
        scribeBR _b BOPOMOFO_EXTENDED                              = "BopomofoExtended"
        scribeBR _b CJK_STROKES                                    = "CJKStrokes"
        scribeBR _b KATAKANA_PHONETIC_EXTENSIONS                   = "KatakanaPhoneticExtensions"
        scribeBR _b ENCLOSED_CJK_LETTERS_AND_MONTHS                = "EnclosedCJKLettersandMonths"
        scribeBR _b CJK_COMPATIBILITY                              = "CJKCompatibility"
        scribeBR _b CJK_UNIFIED_IDEOGRAPHS_EXTENSION_A             = "CJKUnifiedIdeographsExtensionA"
        scribeBR _b YIJING_HEXAGRAM_SYMBOLS                        = "YijingHexagramSymbols"
        scribeBR _b CJK_UNIFIED_IDEOGRAPHS                         = "CJKUnifiedIdeographs"
        scribeBR _b YI_SYLLABLES                                   = "YiSyllables"
        scribeBR _b YI_RADICALS                                    = "YiRadicals"
        scribeBR _b LISU                                           = "Lisu"
        scribeBR _b VAI                                            = "Vai"
        scribeBR _b CYRILLIC_EXTENDED_B                            = "CyrillicExtended-B"
        scribeBR _b BAMUM                                          = "Bamum"
        scribeBR _b MODIFIER_TONE_LETTERS                          = "ModifierToneLetters"
        scribeBR _b LATIN_EXTENDED_D                               = "LatinExtended-D"
        scribeBR _b SYLOTI_NAGRI                                   = "SylotiNagri"
        scribeBR _b COMMON_INDIC_NUMBER_FORMS                      = "CommonIndicNumberForms"
        scribeBR _b PHAGS_PA                                       = "Phags-pa"
        scribeBR _b SAURASHTRA                                     = "Saurashtra"
        scribeBR _b DEVANAGARI_EXTENDED                            = "DevanagariExtended"
        scribeBR _b KAYAH_LI                                       = "KayahLi"
        scribeBR _b REJANG                                         = "Rejang"
        scribeBR _b HANGUL_JAMO_EXTENDED_A                         = "HangulJamoExtended-A"
        scribeBR _b JAVANESE                                       = "Javanese"
        scribeBR _b CHAM                                           = "Cham"
        scribeBR _b MYANMAR_EXTENDED_A                             = "MyanmarExtended-A"
        scribeBR _b TAI_VIET                                       = "TaiViet"
        scribeBR _b ETHIOPIC_EXTENDED_A                            = "EthiopicExtended-A"
        scribeBR _b MEETEI_MAYEK                                   = "MeeteiMayek"
        scribeBR _b HANGUL_SYLLABLES                               = "HangulSyllables"
        scribeBR _b HANGUL_JAMO_EXTENDED_B                         = "HangulJamoExtended-B"
        scribeBR _b HIGH_SURROGATES                                = "HighSurrogates"
        scribeBR _b HIGH_PRIVATE_USE_SURROGATES                    = "HighPrivateUseSurrogates"
        scribeBR _b LOW_SURROGATES                                 = "LowSurrogates"
        scribeBR _b PRIVATE_USE_AREA                               = "PrivateUseArea"
        scribeBR _b CJK_COMPATIBILITY_IDEOGRAPHS                   = "CJKCompatibilityIdeographs"
        scribeBR _b ALPHABETIC_PRESENTATION_FORMS                  = "AlphabeticPresentationForms"
        scribeBR _b ARABIC_PRESENTATION_FORMS_A                    = "ArabicPresentationForms-A"
        scribeBR _b VARIATION_SELECTORS                            = "VariationSelectors"
        scribeBR _b VERTICAL_FORMS                                 = "VerticalForms"
        scribeBR _b COMBINING_HALF_MARKS                           = "CombiningHalfMarks"
        scribeBR _b CJK_COMPATIBILITY_FORMS                        = "CJKCompatibilityForms"
        scribeBR _b SMALL_FORM_VARIANTS                            = "SmallFormVariants"
        scribeBR _b ARABIC_PRESENTATION_FORMS_B                    = "ArabicPresentationForms-B"
        scribeBR _b HALFWIDTH_AND_FULLWIDTH_FORMS                  = "HalfwidthandFullwidthForms"
        scribeBR _b SPECIALS                                       = "Specials"
        scribeBR _b LINEAR_B_SYLLABARY                             = "LinearBSyllabary"
        scribeBR _b LINEAR_B_IDEOGRAMS                             = "LinearBIdeograms"
        scribeBR _b AEGEAN_NUMBERS                                 = "AegeanNumbers"
        scribeBR _b ANCIENT_GREEK_NUMBERS                          = "AncientGreekNumbers"
        scribeBR _b ANCIENT_SYMBOLS                                = "AncientSymbols"
        scribeBR _b PHAISTOS_DISC                                  = "PhaistosDisc"
        scribeBR _b LYCIAN                                         = "Lycian"
        scribeBR _b CARIAN                                         = "Carian"
        scribeBR _b OLD_ITALIC                                     = "OldItalic"
        scribeBR _b GOTHIC                                         = "Gothic"
        scribeBR _b UGARITIC                                       = "Ugaritic"
        scribeBR _b OLD_PERSIAN                                    = "OldPersian"
        scribeBR _b DESERET                                        = "Deseret"
        scribeBR _b SHAVIAN                                        = "Shavian"
        scribeBR _b OSMANYA                                        = "Osmanya"
        scribeBR _b CYPRIOT_SYLLABARY                              = "CypriotSyllabary"
        scribeBR _b IMPERIAL_ARAMAIC                               = "ImperialAramaic"
        scribeBR _b PHOENICIAN                                     = "Phoenician"
        scribeBR _b LYDIAN                                         = "Lydian"
        scribeBR _b KHAROSHTHI                                     = "Kharoshthi"
        scribeBR _b OLD_SOUTH_ARABIAN                              = "OldSouthArabian"
        scribeBR _b AVESTAN                                        = "Avestan"
        scribeBR _b INSCRIPTIONAL_PARTHIAN                         = "InscriptionalParthian"
        scribeBR _b INSCRIPTIONAL_PAHLAVI                          = "InscriptionalPahlavi"
        scribeBR _b OLD_TURKIC                                     = "OldTurkic"
        scribeBR _b RUMI_NUMERAL_SYMBOLS                           = "RumiNumeralSymbols"
        scribeBR _b BRAHMI                                         = "Brahmi"
        scribeBR _b KAITHI                                         = "Kaithi"
        scribeBR _b CUNEIFORM                                      = "Cuneiform"
        scribeBR _b CUNEIFORM_NUMBERS_AND_PUNCTUATION              = "CuneiformNumbersandPunctuation"
        scribeBR _b EGYPTIAN_HIEROGLYPHS                           = "EgyptianHieroglyphs"
        scribeBR _b BAMUM_SUPPLEMENT                               = "BamumSupplement"
        scribeBR _b KANA_SUPPLEMENT                                = "KanaSupplement"
        scribeBR _b BYZANTINE_MUSICAL_SYMBOLS                      = "ByzantineMusicalSymbols"
        scribeBR _b MUSICAL_SYMBOLS                                = "MusicalSymbols"
        scribeBR _b ANCIENT_GREEK_MUSICAL_NOTATION                 = "AncientGreekMusicalNotation"
        scribeBR _b TAI_XUAN_JING_SYMBOLS                          = "TaiXuanJingSymbols"
        scribeBR _b COUNTING_ROD_NUMERALS                          = "CountingRodNumerals"
        scribeBR _b MATHEMATICAL_ALPHANUMERIC_SYMBOLS              = "MathematicalAlphanumericSymbols"
        scribeBR _b MAHJONG_TILES                                  = "MahjongTiles"
        scribeBR _b DOMINO_TILES                                   = "DominoTiles"
        scribeBR _b PLAYING_CARDS                                  = "PlayingCards"
        scribeBR _b ENCLOSED_ALPHANUMERIC_SUPPLEMENT               = "EnclosedAlphanumericSupplement"
        scribeBR _b ENCLOSED_IDEOGRAPHIC_SUPPLEMENT                = "EnclosedIdeographicSupplement"
        scribeBR _b MISCELLANEOUS_SYMBOLS_AND_PICTOGRAPHS          = "MiscellaneousSymbolsAndPictographs"
        scribeBR _b EMOTICONS                                      = "Emoticons"
        scribeBR _b TRANSPORT_AND_MAP_SYMBOLS                      = "TransportAndMapSymbols"
        scribeBR _b ALCHEMICAL_SYMBOLS                             = "AlchemicalSymbols"
        scribeBR _b CJK_UNIFIED_IDEOGRAPHS_EXTENSION_B             = "CJKUnifiedIdeographsExtensionB"
        scribeBR _b CJK_UNIFIED_IDEOGRAPHS_EXTENSION_C             = "CJKUnifiedIdeographsExtensionC"
        scribeBR _b CJK_UNIFIED_IDEOGRAPHS_EXTENSION_D             = "CJKUnifiedIdeographsExtensionD"
        scribeBR _b CJK_COMPATIBILITY_IDEOGRAPHS_SUPPLEMENT        = "CJKCompatibilityIdeographsSupplement"
        scribeBR _b TAGS                                           = "Tags"
        scribeBR _b VARIATION_SELECTORS_SUPPLEMENT                 = "VariationSelectorsSupplement"
        scribeBR _b SUPPLEMENTARY_PRIVATE_USE_AREA_A               = "SupplementaryPrivateUseArea-A"
        scribeBR _b SUPPLEMENTARY_PRIVATE_USE_AREA_B               = "SupplementaryPrivateUseArea-B"

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
normalChar = choice [ entityChar
                    , entityHexChar
                    , entityNumChar
                    , (satisfy $ notInClass ".\\?*+{}()|[]")
                    ] >>= pure . NormalChar

entityChar :: Parser Char
entityChar = do skipC '&'
                cs <- minMax 1 maxEntityName . satisfy $ inClass "a-zA-Z"
                skipC ';'
                case M.lookup (T.pack cs) entityENumMap of
                  Nothing -> fail "entity unrecognized"
                  Just (Left  e) -> pure $ C.chr e
                  Just (Right _) -> fail "multichar entity ref not supported."

entityHexChar :: Parser Char
entityHexChar = do skipS "&#x"
                   cs <- minMax 1 6 . satisfy $ inClass "0-9A-Fa-f"
                   let mc = do int <- case N.readHex cs of
                                        (i, _):_ -> Just i
                                        _ -> Nothing
                               guard $ int <= 0x10FFFF -- Char max val is 0x10FFF. Will throw exception on larger value.                             -- ⚡
                               pure $ C.chr int
                   c <- case mc of
                          Just c -> pure c
                          Nothing -> fail "entity number not recognized"
                   skipC ';'
                   pure c

entityNumChar :: Parser Char
entityNumChar = do skipS "&#"
                   cs <- minMax 1 10 . satisfy $ inClass "0-9"
                   let mc = do int <- readMay cs
                               guard $ int <= 0x10FFFF -- Char max val is 0x10FFF. Will throw exception on larger value.                             -- ⚡
                               pure $ C.chr int
                   c <- case mc of
                          Just c -> pure c
                          Nothing -> fail "entity number not recognized"
                   skipC ';'
                   pure c

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
singleCharNoEsc = do c <- choice [ entityChar
                                 , entityHexChar
                                 , entityNumChar
                                 , satisfy $ notInClass "[]"
                                 ]
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

data IsBlock = IsBlock UnicodeBlockName Text
               deriving (Show, Eq)

isBlock :: Parser IsBlock
isBlock = do (ubn,matchedText) <- unicodeBlockMatch
             pure $ IsBlock ubn matchedText

unicodeBlockMatch :: Parser (UnicodeBlockName, Text)
unicodeBlockMatch = do
  isP <- option False (string "Is" >> pure True)
  res@(ubn,t) <- if | not isP -> choice searchBlocks
                    | otherwise -> choice includeUnrecognized
  pure $ if | isP -> (ubn, T.append "Is" t)
            | otherwise -> res
  where searchBlocks = map unicodeBlockPair lengthOrderedUnicodeBlockNames
        includeUnrecognized = searchBlocks ++ (unrecognizedBlock:[])
        unrecognizedBlock = do ubs <- many1 $ do peeked <- peekChar'
                                                 guard $ peeked /= '}'
                                                 anyChar -- consumes and returns the non-'}' char.
                               pure (UNRECOGNIZED_BLOCK, T.pack ubs)

unicodeBlockPair :: UnicodeBlockName -> Parser (UnicodeBlockName, Text)
unicodeBlockPair ubnomen = string (scribeR ubnomen) >>= pure . (ubnomen,)

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
data UnicodeBlockName = UNRECOGNIZED_BLOCK
                      | AEGEAN_NUMBERS
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
ubnns =  [ ( UNRECOGNIZED_BLOCK                             , UBN 0x0000   0x10FFFF )
         , ( BASIC_LATIN                                    , UBN 0x0000   0x007F   )
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
multiCharEsc = char '\\' >> satisfy (inClass "sSiIcCdDwW") >>= pure . MultiCharEsc

-- | The wildcard character is a metacharacter which matches almost any single character
data WildcardEsc = WildcardEsc
                   deriving (Show, Eq)

wildCardEsc :: Parser WildcardEsc
wildCardEsc  = skipC '.' >> pure WildcardEsc


eNumEntityMap :: M.Map (Either Int [Int]) Text
eNumEntityMap = M.fromList . map (\(t,e) -> (e,t))
                           $ L.sortBy (\a b -> T.length (fst b) `compare` T.length (fst a)) -- shortest last take precedence.
                             entityENum
                             
entityENumMap :: M.Map Text (Either Int [Int])
entityENumMap = M.fromList entityENum

maxEntityName :: Int
maxEntityName = L.maximum $ map (T.length . fst) entityENum

entityENum :: [] (Text, Either Int [Int])                              -- Right multichar entities need work and support?                            -- ⚠
entityENum = [ ("Aacute"                          , Left 0x000C1)
             , ("aacute"                          , Left 0x000E1)
             , ("Abreve"                          , Left 0x00102)
             , ("abreve"                          , Left 0x00103)
             , ("acd"                             , Left 0x0223F)
             , ("Acirc"                           , Left 0x000C2)
             , ("acirc"                           , Left 0x000E2)
             , ("ac"                              , Left 0x0223E)
             , ("acute"                           , Left 0x000B4)
             , ("Acy"                             , Left 0x00410)
             , ("acy"                             , Left 0x00430)
             , ("AElig"                           , Left 0x000C6)
             , ("aelig"                           , Left 0x000E6)
             , ("af"                              , Left 0x02061)
             , ("Afr"                             , Left 0x1D504)
             , ("afr"                             , Left 0x1D51E)
             , ("Agrave"                          , Left 0x000C0)
             , ("agrave"                          , Left 0x000E0)
             , ("aleph"                           , Left 0x02135)
             , ("alpha"                           , Left 0x003B1)
             , ("Amacr"                           , Left 0x00100)
             , ("amacr"                           , Left 0x00101)
             , ("amalg"                           , Left 0x02A3F)
             , ("amp"                             , Left 0x00026)
             , ("andand"                          , Left 0x02A55)
             , ("andd"                            , Left 0x02A5C)
             , ("and"                             , Left 0x02227)
             , ("And"                             , Left 0x02A53)
             , ("andslope"                        , Left 0x02A58)
             , ("andv"                            , Left 0x02A5A)
             , ("ange"                            , Left 0x029A4)
             , ("ang"                             , Left 0x02220)
             , ("angle"                           , Left 0x02220)
             , ("angmsdaa"                        , Left 0x029A8)
             , ("angmsdab"                        , Left 0x029A9)
             , ("angmsdac"                        , Left 0x029AA)
             , ("angmsdad"                        , Left 0x029AB)
             , ("angmsdae"                        , Left 0x029AC)
             , ("angmsdaf"                        , Left 0x029AD)
             , ("angmsdag"                        , Left 0x029AE)
             , ("angmsdah"                        , Left 0x029AF)
             , ("angmsd"                          , Left 0x02221)
             , ("angrt"                           , Left 0x0221F)
             , ("angrtvbd"                        , Left 0x0299D)
             , ("angrtvb"                         , Left 0x022BE)
             , ("angsph"                          , Left 0x02222)
             , ("angst"                           , Left 0x0212B)
             , ("angzarr"                         , Left 0x0237C)
             , ("Aogon"                           , Left 0x00104)
             , ("aogon"                           , Left 0x00105)
             , ("Aopf"                            , Left 0x1D538)
             , ("aopf"                            , Left 0x1D552)
             , ("apacir"                          , Left 0x02A6F)
             , ("ape"                             , Left 0x0224A)
             , ("apE"                             , Left 0x02A70)
             , ("apid"                            , Left 0x0224B)
             , ("ap"                              , Left 0x02248)
             , ("apos"                            , Left 0x00027)
             , ("ApplyFunction"                   , Left 0x02061)
             , ("approxeq"                        , Left 0x0224A)
             , ("approx"                          , Left 0x02248)
             , ("Aring"                           , Left 0x000C5)
             , ("aring"                           , Left 0x000E5)
             , ("Ascr"                            , Left 0x1D49C)
             , ("ascr"                            , Left 0x1D4B6)
             , ("Assign"                          , Left 0x02254)
             , ("ast"                             , Left 0x0002A)
             , ("asympeq"                         , Left 0x0224D)
             , ("asymp"                           , Left 0x02248)
             , ("Atilde"                          , Left 0x000C3)
             , ("atilde"                          , Left 0x000E3)
             , ("Auml"                            , Left 0x000C4)
             , ("auml"                            , Left 0x000E4)
             , ("awconint"                        , Left 0x02233)
             , ("awint"                           , Left 0x02A11)
             , ("backcong"                        , Left 0x0224C)
             , ("backepsilon"                     , Left 0x003F6)
             , ("backprime"                       , Left 0x02035)
             , ("backsimeq"                       , Left 0x022CD)
             , ("backsim"                         , Left 0x0223D)
             , ("Backslash"                       , Left 0x02216)
             , ("barvee"                          , Left 0x022BD)
             , ("Barv"                            , Left 0x02AE7)
             , ("barwedge"                        , Left 0x02305)
             , ("barwed"                          , Left 0x02305)
             , ("Barwed"                          , Left 0x02306)
             , ("bbrk"                            , Left 0x023B5)
             , ("bbrktbrk"                        , Left 0x023B6)
             , ("bcong"                           , Left 0x0224C)
             , ("Bcy"                             , Left 0x00411)
             , ("bcy"                             , Left 0x00431)
             , ("because"                         , Left 0x02235)
             , ("Because"                         , Left 0x02235)
             , ("becaus"                          , Left 0x02235)
             , ("bemptyv"                         , Left 0x029B0)
             , ("bepsi"                           , Left 0x003F6)
             , ("bernou"                          , Left 0x0212C)
             , ("Bernoullis"                      , Left 0x0212C)
             , ("beta"                            , Left 0x003B2)
             , ("beth"                            , Left 0x02136)
             , ("between"                         , Left 0x0226C)
             , ("Bfr"                             , Left 0x1D505)
             , ("bfr"                             , Left 0x1D51F)
             , ("bigcap"                          , Left 0x022C2)
             , ("bigcirc"                         , Left 0x025EF)
             , ("bigcup"                          , Left 0x022C3)
             , ("bigodot"                         , Left 0x02A00)
             , ("bigoplus"                        , Left 0x02A01)
             , ("bigotimes"                       , Left 0x02A02)
             , ("bigsqcup"                        , Left 0x02A06)
             , ("bigstar"                         , Left 0x02605)
             , ("bigtriangledown"                 , Left 0x025BD)
             , ("bigtriangleup"                   , Left 0x025B3)
             , ("biguplus"                        , Left 0x02A04)
             , ("bigvee"                          , Left 0x022C1)
             , ("bigwedge"                        , Left 0x022C0)
             , ("bkarow"                          , Left 0x0290D)
             , ("blacklozenge"                    , Left 0x029EB)
             , ("blacksquare"                     , Left 0x025AA)
             , ("blacktriangledown"               , Left 0x025BE)
             , ("blacktriangle"                   , Left 0x025B4)
             , ("blacktriangleleft"               , Left 0x025C2)
             , ("blacktriangleright"              , Left 0x025B8)
             , ("blank"                           , Left 0x02423)
             , ("blk12"                           , Left 0x02592)
             , ("blk14"                           , Left 0x02591)
             , ("blk34"                           , Left 0x02593)
             , ("block"                           , Left 0x02588)
             , ("bnot"                            , Left 0x02310)
             , ("bNot"                            , Left 0x02AED)
             , ("Bopf"                            , Left 0x1D539)
             , ("bopf"                            , Left 0x1D553)
             , ("bot"                             , Left 0x022A5)
             , ("bottom"                          , Left 0x022A5)
             , ("bowtie"                          , Left 0x022C8)
             , ("boxbox"                          , Left 0x029C9)
             , ("boxdl"                           , Left 0x02510)
             , ("boxdL"                           , Left 0x02555)
             , ("boxDl"                           , Left 0x02556)
             , ("boxDL"                           , Left 0x02557)
             , ("boxdr"                           , Left 0x0250C)
             , ("boxdR"                           , Left 0x02552)
             , ("boxDr"                           , Left 0x02553)
             , ("boxDR"                           , Left 0x02554)
             , ("boxhd"                           , Left 0x0252C)
             , ("boxHd"                           , Left 0x02564)
             , ("boxhD"                           , Left 0x02565)
             , ("boxHD"                           , Left 0x02566)
             , ("boxh"                            , Left 0x02500)
             , ("boxH"                            , Left 0x02550)
             , ("boxhu"                           , Left 0x02534)
             , ("boxHu"                           , Left 0x02567)
             , ("boxhU"                           , Left 0x02568)
             , ("boxHU"                           , Left 0x02569)
             , ("boxminus"                        , Left 0x0229F)
             , ("boxplus"                         , Left 0x0229E)
             , ("boxtimes"                        , Left 0x022A0)
             , ("boxul"                           , Left 0x02518)
             , ("boxuL"                           , Left 0x0255B)
             , ("boxUl"                           , Left 0x0255C)
             , ("boxUL"                           , Left 0x0255D)
             , ("boxur"                           , Left 0x02514)
             , ("boxuR"                           , Left 0x02558)
             , ("boxUr"                           , Left 0x02559)
             , ("boxUR"                           , Left 0x0255A)
             , ("boxvh"                           , Left 0x0253C)
             , ("boxvH"                           , Left 0x0256A)
             , ("boxVh"                           , Left 0x0256B)
             , ("boxVH"                           , Left 0x0256C)
             , ("boxv"                            , Left 0x02502)
             , ("boxV"                            , Left 0x02551)
             , ("boxvl"                           , Left 0x02524)
             , ("boxvL"                           , Left 0x02561)
             , ("boxVl"                           , Left 0x02562)
             , ("boxVL"                           , Left 0x02563)
             , ("boxvr"                           , Left 0x0251C)
             , ("boxvR"                           , Left 0x0255E)
             , ("boxVr"                           , Left 0x0255F)
             , ("boxVR"                           , Left 0x02560)
             , ("bprime"                          , Left 0x02035)
             , ("breve"                           , Left 0x002D8)
             , ("Breve"                           , Left 0x002D8)
             , ("brvbar"                          , Left 0x000A6)
             , ("Bscr"                            , Left 0x0212C)
             , ("bscr"                            , Left 0x1D4B7)
             , ("bsemi"                           , Left 0x0204F)
             , ("bsime"                           , Left 0x022CD)
             , ("bsim"                            , Left 0x0223D)
             , ("bsolb"                           , Left 0x029C5)
             , ("bsol"                            , Left 0x0005C)
             , ("bullet"                          , Left 0x02022)
             , ("bull"                            , Left 0x02022)
             , ("bumpe"                           , Left 0x0224F)
             , ("bumpE"                           , Left 0x02AAE)
             , ("Bumpeq"                          , Left 0x0224E)
             , ("bumpeq"                          , Left 0x0224F)
             , ("bump"                            , Left 0x0224E)
             , ("Cacute"                          , Left 0x00106)
             , ("cacute"                          , Left 0x00107)
             , ("capand"                          , Left 0x02A44)
             , ("capbrcup"                        , Left 0x02A49)
             , ("capcap"                          , Left 0x02A4B)
             , ("capcup"                          , Left 0x02A47)
             , ("capdot"                          , Left 0x02A40)
             , ("CapitalDifferentialD"            , Left 0x02145)
             , ("cap"                             , Left 0x02229)
             , ("Cap"                             , Left 0x022D2)
             , ("caret"                           , Left 0x02041)
             , ("caron"                           , Left 0x002C7)
             , ("Cayleys"                         , Left 0x0212D)
             , ("ccaps"                           , Left 0x02A4D)
             , ("Ccaron"                          , Left 0x0010C)
             , ("ccaron"                          , Left 0x0010D)
             , ("Ccedil"                          , Left 0x000C7)
             , ("ccedil"                          , Left 0x000E7)
             , ("Ccirc"                           , Left 0x00108)
             , ("ccirc"                           , Left 0x00109)
             , ("Cconint"                         , Left 0x02230)
             , ("ccups"                           , Left 0x02A4C)
             , ("ccupssm"                         , Left 0x02A50)
             , ("Cdot"                            , Left 0x0010A)
             , ("cdot"                            , Left 0x0010B)
             , ("Cedilla"                         , Left 0x000B8)
             , ("cedil"                           , Left 0x000B8)
             , ("cemptyv"                         , Left 0x029B2)
             , ("centerdot"                       , Left 0x000B7)
             , ("CenterDot"                       , Left 0x000B7)
             , ("cent"                            , Left 0x000A2)
             , ("Cfr"                             , Left 0x0212D)
             , ("cfr"                             , Left 0x1D520)
             , ("CHcy"                            , Left 0x00427)
             , ("chcy"                            , Left 0x00447)
             , ("check"                           , Left 0x02713)
             , ("checkmark"                       , Left 0x02713)
             , ("chi"                             , Left 0x003C7)
             , ("circeq"                          , Left 0x02257)
             , ("circlearrowleft"                 , Left 0x021BA)
             , ("circlearrowright"                , Left 0x021BB)
             , ("circledast"                      , Left 0x0229B)
             , ("circledcirc"                     , Left 0x0229A)
             , ("circleddash"                     , Left 0x0229D)
             , ("CircleDot"                       , Left 0x02299)
             , ("circledR"                        , Left 0x000AE)
             , ("circledS"                        , Left 0x024C8)
             , ("circ"                            , Left 0x002C6)
             , ("CircleMinus"                     , Left 0x02296)
             , ("CirclePlus"                      , Left 0x02295)
             , ("CircleTimes"                     , Left 0x02297)
             , ("cire"                            , Left 0x02257)
             , ("cirE"                            , Left 0x029C3)
             , ("cirfnint"                        , Left 0x02A10)
             , ("cir"                             , Left 0x025CB)
             , ("cirmid"                          , Left 0x02AEF)
             , ("cirscir"                         , Left 0x029C2)
             , ("ClockwiseContourIntegral"        , Left 0x02232)
             , ("CloseCurlyDoubleQuote"           , Left 0x0201D)
             , ("CloseCurlyQuote"                 , Left 0x02019)
             , ("clubs"                           , Left 0x02663)
             , ("clubsuit"                        , Left 0x02663)
             , ("colone"                          , Left 0x02254)
             , ("Colone"                          , Left 0x02A74)
             , ("coloneq"                         , Left 0x02254)
             , ("colon"                           , Left 0x0003A)
             , ("Colon"                           , Left 0x02237)
             , ("comma"                           , Left 0x0002C)
             , ("commat"                          , Left 0x00040)
             , ("compfn"                          , Left 0x02218)
             , ("comp"                            , Left 0x02201)
             , ("complement"                      , Left 0x02201)
             , ("complexes"                       , Left 0x02102)
             , ("congdot"                         , Left 0x02A6D)
             , ("cong"                            , Left 0x02245)
             , ("Congruent"                       , Left 0x02261)
             , ("conint"                          , Left 0x0222E)
             , ("Conint"                          , Left 0x0222F)
             , ("ContourIntegral"                 , Left 0x0222E)
             , ("Copf"                            , Left 0x02102)
             , ("copf"                            , Left 0x1D554)
             , ("coprod"                          , Left 0x02210)
             , ("Coproduct"                       , Left 0x02210)
             , ("copy"                            , Left 0x000A9)
             , ("copysr"                          , Left 0x02117)
             , ("CounterClockwiseContourIntegral" , Left 0x02233)
             , ("cross"                           , Left 0x02717)
             , ("Cross"                           , Left 0x02A2F)
             , ("Cscr"                            , Left 0x1D49E)
             , ("cscr"                            , Left 0x1D4B8)
             , ("csube"                           , Left 0x02AD1)
             , ("csub"                            , Left 0x02ACF)
             , ("csupe"                           , Left 0x02AD2)
             , ("csup"                            , Left 0x02AD0)
             , ("ctdot"                           , Left 0x022EF)
             , ("cudarrl"                         , Left 0x02938)
             , ("cudarrr"                         , Left 0x02935)
             , ("cuepr"                           , Left 0x022DE)
             , ("cuesc"                           , Left 0x022DF)
             , ("cularr"                          , Left 0x021B6)
             , ("cularrp"                         , Left 0x0293D)
             , ("cupbrcap"                        , Left 0x02A48)
             , ("CupCap"                          , Left 0x0224D)
             , ("cupcap"                          , Left 0x02A46)
             , ("cupcup"                          , Left 0x02A4A)
             , ("cupdot"                          , Left 0x0228D)
             , ("cup"                             , Left 0x0222A)
             , ("Cup"                             , Left 0x022D3)
             , ("cupor"                           , Left 0x02A45)
             , ("curarr"                          , Left 0x021B7)
             , ("curarrm"                         , Left 0x0293C)
             , ("curlyeqprec"                     , Left 0x022DE)
             , ("curlyeqsucc"                     , Left 0x022DF)
             , ("curlyvee"                        , Left 0x022CE)
             , ("curlywedge"                      , Left 0x022CF)
             , ("curren"                          , Left 0x000A4)
             , ("curvearrowleft"                  , Left 0x021B6)
             , ("curvearrowright"                 , Left 0x021B7)
             , ("cuvee"                           , Left 0x022CE)
             , ("cuwed"                           , Left 0x022CF)
             , ("cwconint"                        , Left 0x02232)
             , ("cwint"                           , Left 0x02231)
             , ("cylcty"                          , Left 0x0232D)
             , ("dagger"                          , Left 0x02020)
             , ("Dagger"                          , Left 0x02021)
             , ("daleth"                          , Left 0x02138)
             , ("darr"                            , Left 0x02193)
             , ("Darr"                            , Left 0x021A1)
             , ("dArr"                            , Left 0x021D3)
             , ("dash"                            , Left 0x02010)
             , ("dashv"                           , Left 0x022A3)
             , ("Dashv"                           , Left 0x02AE4)
             , ("dbkarow"                         , Left 0x0290F)
             , ("dblac"                           , Left 0x002DD)
             , ("Dcaron"                          , Left 0x0010E)
             , ("dcaron"                          , Left 0x0010F)
             , ("Dcy"                             , Left 0x00414)
             , ("dcy"                             , Left 0x00434)
             , ("ddagger"                         , Left 0x02021)
             , ("ddarr"                           , Left 0x021CA)
             , ("DD"                              , Left 0x02145)
             , ("dd"                              , Left 0x02146)
             , ("DDotrahd"                        , Left 0x02911)
             , ("ddotseq"                         , Left 0x02A77)
             , ("deg"                             , Left 0x000B0)
             , ("Del"                             , Left 0x02207)
             , ("Delta"                           , Left 0x00394)
             , ("delta"                           , Left 0x003B4)
             , ("demptyv"                         , Left 0x029B1)
             , ("dfisht"                          , Left 0x0297F)
             , ("Dfr"                             , Left 0x1D507)
             , ("dfr"                             , Left 0x1D521)
             , ("dHar"                            , Left 0x02965)
             , ("dharl"                           , Left 0x021C3)
             , ("dharr"                           , Left 0x021C2)
             , ("DiacriticalAcute"                , Left 0x000B4)
             , ("DiacriticalDot"                  , Left 0x002D9)
             , ("DiacriticalDoubleAcute"          , Left 0x002DD)
             , ("DiacriticalGrave"                , Left 0x00060)
             , ("DiacriticalTilde"                , Left 0x002DC)
             , ("diam"                            , Left 0x022C4)
             , ("diamond"                         , Left 0x022C4)
             , ("Diamond"                         , Left 0x022C4)
             , ("diamondsuit"                     , Left 0x02666)
             , ("diams"                           , Left 0x02666)
             , ("die"                             , Left 0x000A8)
             , ("DifferentialD"                   , Left 0x02146)
             , ("digamma"                         , Left 0x003DD)
             , ("disin"                           , Left 0x022F2)
             , ("divide"                          , Left 0x000F7)
             , ("divideontimes"                   , Left 0x022C7)
             , ("div"                             , Left 0x000F7)
             , ("divonx"                          , Left 0x022C7)
             , ("DJcy"                            , Left 0x00402)
             , ("djcy"                            , Left 0x00452)
             , ("dlcorn"                          , Left 0x0231E)
             , ("dlcrop"                          , Left 0x0230D)
             , ("dollar"                          , Left 0x00024)
             , ("Dopf"                            , Left 0x1D53B)
             , ("dopf"                            , Left 0x1D555)
             , ("DotDot"                          , Left 0x020DC)
             , ("doteqdot"                        , Left 0x02251)
             , ("doteq"                           , Left 0x02250)
             , ("DotEqual"                        , Left 0x02250)
             , ("Dot"                             , Left 0x000A8)
             , ("dot"                             , Left 0x002D9)
             , ("dotminus"                        , Left 0x02238)
             , ("dotplus"                         , Left 0x02214)
             , ("dotsquare"                       , Left 0x022A1)
             , ("doublebarwedge"                  , Left 0x02306)
             , ("DoubleContourIntegral"           , Left 0x0222F)
             , ("DoubleDot"                       , Left 0x000A8)
             , ("DoubleDownArrow"                 , Left 0x021D3)
             , ("DoubleLeftArrow"                 , Left 0x021D0)
             , ("DoubleLeftRightArrow"            , Left 0x021D4)
             , ("DoubleLeftTee"                   , Left 0x02AE4)
             , ("DoubleLongLeftArrow"             , Left 0x027F8)
             , ("DoubleLongLeftRightArrow"        , Left 0x027FA)
             , ("DoubleLongRightArrow"            , Left 0x027F9)
             , ("DoubleRightArrow"                , Left 0x021D2)
             , ("DoubleRightTee"                  , Left 0x022A8)
             , ("DoubleUpArrow"                   , Left 0x021D1)
             , ("DoubleUpDownArrow"               , Left 0x021D5)
             , ("DoubleVerticalBar"               , Left 0x02225)
             , ("DownArrowBar"                    , Left 0x02913)
             , ("downarrow"                       , Left 0x02193)
             , ("DownArrow"                       , Left 0x02193)
             , ("Downarrow"                       , Left 0x021D3)
             , ("DownArrowUpArrow"                , Left 0x021F5)
             , ("DownBreve"                       , Left 0x00311)
             , ("downdownarrows"                  , Left 0x021CA)
             , ("downharpoonleft"                 , Left 0x021C3)
             , ("downharpoonright"                , Left 0x021C2)
             , ("DownLeftRightVector"             , Left 0x02950)
             , ("DownLeftTeeVector"               , Left 0x0295E)
             , ("DownLeftVectorBar"               , Left 0x02956)
             , ("DownLeftVector"                  , Left 0x021BD)
             , ("DownRightTeeVector"              , Left 0x0295F)
             , ("DownRightVectorBar"              , Left 0x02957)
             , ("DownRightVector"                 , Left 0x021C1)
             , ("DownTeeArrow"                    , Left 0x021A7)
             , ("DownTee"                         , Left 0x022A4)
             , ("drbkarow"                        , Left 0x02910)
             , ("drcorn"                          , Left 0x0231F)
             , ("drcrop"                          , Left 0x0230C)
             , ("Dscr"                            , Left 0x1D49F)
             , ("dscr"                            , Left 0x1D4B9)
             , ("DScy"                            , Left 0x00405)
             , ("dscy"                            , Left 0x00455)
             , ("dsol"                            , Left 0x029F6)
             , ("Dstrok"                          , Left 0x00110)
             , ("dstrok"                          , Left 0x00111)
             , ("dtdot"                           , Left 0x022F1)
             , ("dtrif"                           , Left 0x025BE)
             , ("dtri"                            , Left 0x025BF)
             , ("duarr"                           , Left 0x021F5)
             , ("duhar"                           , Left 0x0296F)
             , ("dwangle"                         , Left 0x029A6)
             , ("DZcy"                            , Left 0x0040F)
             , ("dzcy"                            , Left 0x0045F)
             , ("dzigrarr"                        , Left 0x027FF)
             , ("Eacute"                          , Left 0x000C9)
             , ("eacute"                          , Left 0x000E9)
             , ("easter"                          , Left 0x02A6E)
             , ("Ecaron"                          , Left 0x0011A)
             , ("ecaron"                          , Left 0x0011B)
             , ("Ecirc"                           , Left 0x000CA)
             , ("ecirc"                           , Left 0x000EA)
             , ("ecir"                            , Left 0x02256)
             , ("ecolon"                          , Left 0x02255)
             , ("Ecy"                             , Left 0x0042D)
             , ("ecy"                             , Left 0x0044D)
             , ("eDDot"                           , Left 0x02A77)
             , ("Edot"                            , Left 0x00116)
             , ("edot"                            , Left 0x00117)
             , ("eDot"                            , Left 0x02251)
             , ("ee"                              , Left 0x02147)
             , ("efDot"                           , Left 0x02252)
             , ("Efr"                             , Left 0x1D508)
             , ("efr"                             , Left 0x1D522)
             , ("eg"                              , Left 0x02A9A)
             , ("Egrave"                          , Left 0x000C8)
             , ("egrave"                          , Left 0x000E8)
             , ("egsdot"                          , Left 0x02A98)
             , ("egs"                             , Left 0x02A96)
             , ("Element"                         , Left 0x02208)
             , ("elinters"                        , Left 0x0FFFD)
             , ("el"                              , Left 0x02A99)
             , ("ell"                             , Left 0x02113)
             , ("elsdot"                          , Left 0x02A97)
             , ("els"                             , Left 0x02A95)
             , ("Emacr"                           , Left 0x00112)
             , ("emacr"                           , Left 0x00113)
             , ("empty"                           , Left 0x02205)
             , ("emptyset"                        , Left 0x02205)
             , ("EmptySmallSquare"                , Left 0x025FB)
             , ("EmptyVerySmallSquare"            , Left 0x025AB)
             , ("emptyv"                          , Left 0x02205)
             , ("emsp13"                          , Left 0x02004)
             , ("emsp14"                          , Left 0x02005)
             , ("emsp"                            , Left 0x02003)
             , ("ENG"                             , Left 0x0014A)
             , ("eng"                             , Left 0x0014B)
             , ("ensp"                            , Left 0x02002)
             , ("Eogon"                           , Left 0x00118)
             , ("eogon"                           , Left 0x00119)
             , ("Eopf"                            , Left 0x1D53C)
             , ("eopf"                            , Left 0x1D556)
             , ("epar"                            , Left 0x022D5)
             , ("eparsl"                          , Left 0x029E3)
             , ("eplus"                           , Left 0x02A71)
             , ("epsi"                            , Left 0x003F5)
             , ("epsiv"                           , Left 0x003B5)
             , ("eqcirc"                          , Left 0x02256)
             , ("eqcolon"                         , Left 0x02255)
             , ("eqsim"                           , Left 0x02242)
             , ("eqslantgtr"                      , Left 0x02A96)
             , ("eqslantless"                     , Left 0x02A95)
             , ("Equal"                           , Left 0x02A75)
             , ("equals"                          , Left 0x0003D)
             , ("EqualTilde"                      , Left 0x02242)
             , ("equest"                          , Left 0x0225F)
             , ("Equilibrium"                     , Left 0x021CC)
             , ("equivDD"                         , Left 0x02A78)
             , ("equiv"                           , Left 0x02261)
             , ("eqvparsl"                        , Left 0x029E5)
             , ("erarr"                           , Left 0x02971)
             , ("erDot"                           , Left 0x02253)
             , ("escr"                            , Left 0x0212F)
             , ("Escr"                            , Left 0x02130)
             , ("esdot"                           , Left 0x02250)
             , ("esim"                            , Left 0x02242)
             , ("Esim"                            , Left 0x02A73)
             , ("eta"                             , Left 0x003B7)
             , ("ETH"                             , Left 0x000D0)
             , ("eth"                             , Left 0x000F0)
             , ("Euml"                            , Left 0x000CB)
             , ("euml"                            , Left 0x000EB)
             , ("excl"                            , Left 0x00021)
             , ("exist"                           , Left 0x02203)
             , ("Exists"                          , Left 0x02203)
             , ("expectation"                     , Left 0x02130)
             , ("exponentiale"                    , Left 0x02147)
             , ("ExponentialE"                    , Left 0x02147)
             , ("fallingdotseq"                   , Left 0x02252)
             , ("Fcy"                             , Left 0x00424)
             , ("fcy"                             , Left 0x00444)
             , ("female"                          , Left 0x02640)
             , ("ffilig"                          , Left 0x0FB03)
             , ("fflig"                           , Left 0x0FB00)
             , ("ffllig"                          , Left 0x0FB04)
             , ("Ffr"                             , Left 0x1D509)
             , ("ffr"                             , Left 0x1D523)
             , ("filig"                           , Left 0x0FB01)
             , ("FilledSmallSquare"               , Left 0x025FC)
             , ("FilledVerySmallSquare"           , Left 0x025AA)
             , ("flat"                            , Left 0x0266D)
             , ("fllig"                           , Left 0x0FB02)
             , ("fltns"                           , Left 0x025B1)
             , ("fnof"                            , Left 0x00192)
             , ("Fopf"                            , Left 0x1D53D)
             , ("fopf"                            , Left 0x1D557)
             , ("forall"                          , Left 0x02200)
             , ("ForAll"                          , Left 0x02200)
             , ("fork"                            , Left 0x022D4)
             , ("forkv"                           , Left 0x02AD9)
             , ("Fouriertrf"                      , Left 0x02131)
             , ("fpartint"                        , Left 0x02A0D)
             , ("frac12"                          , Left 0x000BD)
             , ("frac13"                          , Left 0x02153)
             , ("frac14"                          , Left 0x000BC)
             , ("frac15"                          , Left 0x02155)
             , ("frac16"                          , Left 0x02159)
             , ("frac18"                          , Left 0x0215B)
             , ("frac23"                          , Left 0x02154)
             , ("frac25"                          , Left 0x02156)
             , ("frac34"                          , Left 0x000BE)
             , ("frac35"                          , Left 0x02157)
             , ("frac38"                          , Left 0x0215C)
             , ("frac45"                          , Left 0x02158)
             , ("frac56"                          , Left 0x0215A)
             , ("frac58"                          , Left 0x0215D)
             , ("frac78"                          , Left 0x0215E)
             , ("frown"                           , Left 0x02322)
             , ("Fscr"                            , Left 0x02131)
             , ("fscr"                            , Left 0x1D4BB)
             , ("gacute"                          , Left 0x001F5)
             , ("Gammad"                          , Left 0x003DC)
             , ("gammad"                          , Left 0x003DD)
             , ("Gamma"                           , Left 0x00393)
             , ("gamma"                           , Left 0x003B3)
             , ("gap"                             , Left 0x02A86)
             , ("Gbreve"                          , Left 0x0011E)
             , ("gbreve"                          , Left 0x0011F)
             , ("Gcedil"                          , Left 0x00122)
             , ("Gcirc"                           , Left 0x0011C)
             , ("gcirc"                           , Left 0x0011D)
             , ("Gcy"                             , Left 0x00413)
             , ("gcy"                             , Left 0x00433)
             , ("Gdot"                            , Left 0x00120)
             , ("gdot"                            , Left 0x00121)
             , ("ge"                              , Left 0x02265)
             , ("gE"                              , Left 0x02267)
             , ("gel"                             , Left 0x022DB)
             , ("gEl"                             , Left 0x02A8C)
             , ("geq"                             , Left 0x02265)
             , ("geqq"                            , Left 0x02267)
             , ("geqslant"                        , Left 0x02A7E)
             , ("gescc"                           , Left 0x02AA9)
             , ("gesdot"                          , Left 0x02A80)
             , ("gesdoto"                         , Left 0x02A82)
             , ("gesdotol"                        , Left 0x02A84)
             , ("ges"                             , Left 0x02A7E)
             , ("gesles"                          , Left 0x02A94)
             , ("Gfr"                             , Left 0x1D50A)
             , ("gfr"                             , Left 0x1D524)
             , ("ggg"                             , Left 0x022D9)
             , ("gg"                              , Left 0x0226B)
             , ("Gg"                              , Left 0x022D9)
             , ("gimel"                           , Left 0x02137)
             , ("GJcy"                            , Left 0x00403)
             , ("gjcy"                            , Left 0x00453)
             , ("gla"                             , Left 0x02AA5)
             , ("glE"                             , Left 0x02A92)
             , ("glj"                             , Left 0x02AA4)
             , ("gl"                              , Left 0x02277)
             , ("gnap"                            , Left 0x02A8A)
             , ("gnapprox"                        , Left 0x02A8A)
             , ("gnE"                             , Left 0x02269)
             , ("gne"                             , Left 0x02A88)
             , ("gneq"                            , Left 0x02A88)
             , ("gneqq"                           , Left 0x02269)
             , ("gnsim"                           , Left 0x022E7)
             , ("Gopf"                            , Left 0x1D53E)
             , ("gopf"                            , Left 0x1D558)
             , ("grave"                           , Left 0x00060)
             , ("GreaterEqual"                    , Left 0x02265)
             , ("GreaterEqualLess"                , Left 0x022DB)
             , ("GreaterFullEqual"                , Left 0x02267)
             , ("GreaterGreater"                  , Left 0x02AA2)
             , ("GreaterLess"                     , Left 0x02277)
             , ("GreaterSlantEqual"               , Left 0x02A7E)
             , ("GreaterTilde"                    , Left 0x02273)
             , ("gscr"                            , Left 0x0210A)
             , ("Gscr"                            , Left 0x1D4A2)
             , ("gsime"                           , Left 0x02A8E)
             , ("gsim"                            , Left 0x02273)
             , ("gsiml"                           , Left 0x02A90)
             , ("gtcc"                            , Left 0x02AA7)
             , ("gtcir"                           , Left 0x02A7A)
             , ("gtdot"                           , Left 0x022D7)
             , ("gt"                              , Left 0x0003E)
             , ("Gt"                              , Left 0x0226B)
             , ("gtlPar"                          , Left 0x02995)
             , ("gtquest"                         , Left 0x02A7C)
             , ("gtrapprox"                       , Left 0x02A86)
             , ("gtrarr"                          , Left 0x02978)
             , ("gtrdot"                          , Left 0x022D7)
             , ("gtreqless"                       , Left 0x022DB)
             , ("gtreqqless"                      , Left 0x02A8C)
             , ("gtrless"                         , Left 0x02277)
             , ("gtrsim"                          , Left 0x02273)
             , ("Hacek"                           , Left 0x002C7)
             , ("hairsp"                          , Left 0x0200A)
             , ("half"                            , Left 0x000BD)
             , ("hamilt"                          , Left 0x0210B)
             , ("HARDcy"                          , Left 0x0042A)
             , ("hardcy"                          , Left 0x0044A)
             , ("harrcir"                         , Left 0x02948)
             , ("harr"                            , Left 0x02194)
             , ("hArr"                            , Left 0x021D4)
             , ("harrw"                           , Left 0x021AD)
             , ("Hat"                             , Left 0x0005E)
             , ("hbar"                            , Left 0x0210F)
             , ("Hcirc"                           , Left 0x00124)
             , ("hcirc"                           , Left 0x00125)
             , ("hearts"                          , Left 0x02665)
             , ("heartsuit"                       , Left 0x02665)
             , ("hellip"                          , Left 0x02026)
             , ("hercon"                          , Left 0x022B9)
             , ("Hfr"                             , Left 0x0210C)
             , ("hfr"                             , Left 0x1D525)
             , ("HilbertSpace"                    , Left 0x0210B)
             , ("hksearow"                        , Left 0x02925)
             , ("hkswarow"                        , Left 0x02926)
             , ("hoarr"                           , Left 0x021FF)
             , ("homtht"                          , Left 0x0223B)
             , ("hookleftarrow"                   , Left 0x021A9)
             , ("hookrightarrow"                  , Left 0x021AA)
             , ("Hopf"                            , Left 0x0210D)
             , ("hopf"                            , Left 0x1D559)
             , ("horbar"                          , Left 0x02015)
             , ("HorizontalLine"                  , Left 0x02500)
             , ("Hscr"                            , Left 0x0210B)
             , ("hscr"                            , Left 0x1D4BD)
             , ("hslash"                          , Left 0x0210F)
             , ("Hstrok"                          , Left 0x00126)
             , ("hstrok"                          , Left 0x00127)
             , ("HumpDownHump"                    , Left 0x0224E)
             , ("HumpEqual"                       , Left 0x0224F)
             , ("hybull"                          , Left 0x02043)
             , ("hyphen"                          , Left 0x02010)
             , ("Iacute"                          , Left 0x000CD)
             , ("iacute"                          , Left 0x000ED)
             , ("Icirc"                           , Left 0x000CE)
             , ("icirc"                           , Left 0x000EE)
             , ("ic"                              , Left 0x02063)
             , ("Icy"                             , Left 0x00418)
             , ("icy"                             , Left 0x00438)
             , ("Idot"                            , Left 0x00130)
             , ("IEcy"                            , Left 0x00415)
             , ("iecy"                            , Left 0x00435)
             , ("iexcl"                           , Left 0x000A1)
             , ("iff"                             , Left 0x021D4)
             , ("Ifr"                             , Left 0x02111)
             , ("ifr"                             , Left 0x1D526)
             , ("Igrave"                          , Left 0x000CC)
             , ("igrave"                          , Left 0x000EC)
             , ("iiiint"                          , Left 0x02A0C)
             , ("iiint"                           , Left 0x0222D)
             , ("ii"                              , Left 0x02148)
             , ("iinfin"                          , Left 0x029DC)
             , ("iiota"                           , Left 0x02129)
             , ("IJlig"                           , Left 0x00132)
             , ("ijlig"                           , Left 0x00133)
             , ("Imacr"                           , Left 0x0012A)
             , ("imacr"                           , Left 0x0012B)
             , ("image"                           , Left 0x02111)
             , ("ImaginaryI"                      , Left 0x02148)
             , ("imagline"                        , Left 0x02110)
             , ("imagpart"                        , Left 0x02111)
             , ("imath"                           , Left 0x00131)
             , ("Im"                              , Left 0x02111)
             , ("imof"                            , Left 0x022B7)
             , ("imped"                           , Left 0x001B5)
             , ("Implies"                         , Left 0x021D2)
             , ("incare"                          , Left 0x02105)
             , ("infin"                           , Left 0x0221E)
             , ("infintie"                        , Left 0x029DD)
             , ("in"                              , Left 0x02208)
             , ("inodot"                          , Left 0x00131)
             , ("intcal"                          , Left 0x022BA)
             , ("integers"                        , Left 0x02124)
             , ("Integral"                        , Left 0x0222B)
             , ("intercal"                        , Left 0x022BA)
             , ("Intersection"                    , Left 0x022C2)
             , ("intlarhk"                        , Left 0x02A17)
             , ("int"                             , Left 0x0222B)
             , ("Int"                             , Left 0x0222C)
             , ("intprod"                         , Left 0x02A3C)
             , ("InvisibleComma"                  , Left 0x02063)
             , ("InvisibleTimes"                  , Left 0x02062)
             , ("IOcy"                            , Left 0x00401)
             , ("iocy"                            , Left 0x00451)
             , ("Iogon"                           , Left 0x0012E)
             , ("iogon"                           , Left 0x0012F)
             , ("Iopf"                            , Left 0x1D540)
             , ("iopf"                            , Left 0x1D55A)
             , ("iota"                            , Left 0x003B9)
             , ("iprod"                           , Left 0x02A3C)
             , ("iquest"                          , Left 0x000BF)
             , ("Iscr"                            , Left 0x02110)
             , ("iscr"                            , Left 0x1D4BE)
             , ("isindot"                         , Left 0x022F5)
             , ("isinE"                           , Left 0x022F9)
             , ("isin"                            , Left 0x02208)
             , ("isins"                           , Left 0x022F4)
             , ("isinsv"                          , Left 0x022F3)
             , ("isinv"                           , Left 0x02208)
             , ("Itilde"                          , Left 0x00128)
             , ("itilde"                          , Left 0x00129)
             , ("it"                              , Left 0x02062)
             , ("Iukcy"                           , Left 0x00406)
             , ("iukcy"                           , Left 0x00456)
             , ("Iuml"                            , Left 0x000CF)
             , ("iuml"                            , Left 0x000EF)
             , ("Jcirc"                           , Left 0x00134)
             , ("jcirc"                           , Left 0x00135)
             , ("Jcy"                             , Left 0x00419)
             , ("jcy"                             , Left 0x00439)
             , ("Jfr"                             , Left 0x1D50D)
             , ("jfr"                             , Left 0x1D527)
             , ("jmath"                           , Left 0x0006A)
             , ("Jopf"                            , Left 0x1D541)
             , ("jopf"                            , Left 0x1D55B)
             , ("Jscr"                            , Left 0x1D4A5)
             , ("jscr"                            , Left 0x1D4BF)
             , ("Jsercy"                          , Left 0x00408)
             , ("jsercy"                          , Left 0x00458)
             , ("Jukcy"                           , Left 0x00404)
             , ("jukcy"                           , Left 0x00454)
             , ("kappa"                           , Left 0x003BA)
             , ("kappav"                          , Left 0x003F0)
             , ("Kcedil"                          , Left 0x00136)
             , ("kcedil"                          , Left 0x00137)
             , ("Kcy"                             , Left 0x0041A)
             , ("kcy"                             , Left 0x0043A)
             , ("Kfr"                             , Left 0x1D50E)
             , ("kfr"                             , Left 0x1D528)
             , ("kgreen"                          , Left 0x00138)
             , ("KHcy"                            , Left 0x00425)
             , ("khcy"                            , Left 0x00445)
             , ("KJcy"                            , Left 0x0040C)
             , ("kjcy"                            , Left 0x0045C)
             , ("Kopf"                            , Left 0x1D542)
             , ("kopf"                            , Left 0x1D55C)
             , ("Kscr"                            , Left 0x1D4A6)
             , ("kscr"                            , Left 0x1D4C0)
             , ("lAarr"                           , Left 0x021DA)
             , ("Lacute"                          , Left 0x00139)
             , ("lacute"                          , Left 0x0013A)
             , ("laemptyv"                        , Left 0x029B4)
             , ("lagran"                          , Left 0x02112)
             , ("Lambda"                          , Left 0x0039B)
             , ("lambda"                          , Left 0x003BB)
             , ("langd"                           , Left 0x02991)
             , ("lang"                            , Left 0x02329)
             , ("Lang"                            , Left 0x0300A)
             , ("langle"                          , Left 0x02329)
             , ("Laplacetrf"                      , Left 0x02112)
             , ("lap"                             , Left 0x02A85)
             , ("laquo"                           , Left 0x000AB)
             , ("larrbfs"                         , Left 0x0291F)
             , ("larrb"                           , Left 0x021E4)
             , ("larrfs"                          , Left 0x0291D)
             , ("larrhk"                          , Left 0x021A9)
             , ("larr"                            , Left 0x02190)
             , ("Larr"                            , Left 0x0219E)
             , ("lArr"                            , Left 0x021D0)
             , ("larrlp"                          , Left 0x021AB)
             , ("larrpl"                          , Left 0x02939)
             , ("larrsim"                         , Left 0x02973)
             , ("larrtl"                          , Left 0x021A2)
             , ("latail"                          , Left 0x02919)
             , ("lAtail"                          , Left 0x0291B)
             , ("late"                            , Left 0x02AAD)
             , ("lat"                             , Left 0x02AAB)
             , ("lbarr"                           , Left 0x0290C)
             , ("lBarr"                           , Left 0x0290E)
             , ("lbbrk"                           , Left 0x03014)
             , ("lbrace"                          , Left 0x0007B)
             , ("lbrack"                          , Left 0x0005B)
             , ("lbrke"                           , Left 0x0298B)
             , ("lbrksld"                         , Left 0x0298F)
             , ("lbrkslu"                         , Left 0x0298D)
             , ("Lcaron"                          , Left 0x0013D)
             , ("lcaron"                          , Left 0x0013E)
             , ("Lcedil"                          , Left 0x0013B)
             , ("lcedil"                          , Left 0x0013C)
             , ("lceil"                           , Left 0x02308)
             , ("lcub"                            , Left 0x0007B)
             , ("Lcy"                             , Left 0x0041B)
             , ("lcy"                             , Left 0x0043B)
             , ("ldca"                            , Left 0x02936)
             , ("ldquo"                           , Left 0x0201C)
             , ("ldquor"                          , Left 0x0201E)
             , ("ldrdhar"                         , Left 0x02967)
             , ("ldrushar"                        , Left 0x0294B)
             , ("ldsh"                            , Left 0x021B2)
             , ("LeftAngleBracket"                , Left 0x02329)
             , ("LeftArrowBar"                    , Left 0x021E4)
             , ("leftarrow"                       , Left 0x02190)
             , ("LeftArrow"                       , Left 0x02190)
             , ("Leftarrow"                       , Left 0x021D0)
             , ("LeftArrowRightArrow"             , Left 0x021C6)
             , ("leftarrowtail"                   , Left 0x021A2)
             , ("LeftCeiling"                     , Left 0x02308)
             , ("LeftDoubleBracket"               , Left 0x0301A)
             , ("LeftDownTeeVector"               , Left 0x02961)
             , ("LeftDownVectorBar"               , Left 0x02959)
             , ("LeftDownVector"                  , Left 0x021C3)
             , ("LeftFloor"                       , Left 0x0230A)
             , ("leftharpoondown"                 , Left 0x021BD)
             , ("leftharpoonup"                   , Left 0x021BC)
             , ("leftleftarrows"                  , Left 0x021C7)
             , ("leftrightarrow"                  , Left 0x02194)
             , ("LeftRightArrow"                  , Left 0x02194)
             , ("Leftrightarrow"                  , Left 0x021D4)
             , ("leftrightarrows"                 , Left 0x021C6)
             , ("leftrightharpoons"               , Left 0x021CB)
             , ("leftrightsquigarrow"             , Left 0x021AD)
             , ("LeftRightVector"                 , Left 0x0294E)
             , ("LeftTeeArrow"                    , Left 0x021A4)
             , ("LeftTee"                         , Left 0x022A3)
             , ("LeftTeeVector"                   , Left 0x0295A)
             , ("leftthreetimes"                  , Left 0x022CB)
             , ("LeftTriangleBar"                 , Left 0x029CF)
             , ("LeftTriangleEqual"               , Left 0x022B4)
             , ("LeftTriangle"                    , Left 0x022B2)
             , ("LeftUpDownVector"                , Left 0x02951)
             , ("LeftUpTeeVector"                 , Left 0x02960)
             , ("LeftUpVectorBar"                 , Left 0x02958)
             , ("LeftUpVector"                    , Left 0x021BF)
             , ("LeftVectorBar"                   , Left 0x02952)
             , ("LeftVector"                      , Left 0x021BC)
             , ("leg"                             , Left 0x022DA)
             , ("lEg"                             , Left 0x02A8B)
             , ("le"                              , Left 0x02264)
             , ("lE"                              , Left 0x02266)
             , ("leq"                             , Left 0x02264)
             , ("leqq"                            , Left 0x02266)
             , ("leqslant"                        , Left 0x02A7D)
             , ("lescc"                           , Left 0x02AA8)
             , ("lesdot"                          , Left 0x02A7F)
             , ("lesdoto"                         , Left 0x02A81)
             , ("lesdotor"                        , Left 0x02A83)
             , ("lesges"                          , Left 0x02A93)
             , ("les"                             , Left 0x02A7D)
             , ("lessapprox"                      , Left 0x02A85)
             , ("lessdot"                         , Left 0x022D6)
             , ("lesseqgtr"                       , Left 0x022DA)
             , ("lesseqqgtr"                      , Left 0x02A8B)
             , ("LessEqualGreater"                , Left 0x022DA)
             , ("LessFullEqual"                   , Left 0x02266)
             , ("LessGreater"                     , Left 0x02276)
             , ("lessgtr"                         , Left 0x02276)
             , ("LessLess"                        , Left 0x02AA1)
             , ("lesssim"                         , Left 0x02272)
             , ("LessSlantEqual"                  , Left 0x02A7D)
             , ("LessTilde"                       , Left 0x02272)
             , ("lfisht"                          , Left 0x0297C)
             , ("lfloor"                          , Left 0x0230A)
             , ("Lfr"                             , Left 0x1D50F)
             , ("lfr"                             , Left 0x1D529)
             , ("lgE"                             , Left 0x02A91)
             , ("lg"                              , Left 0x02276)
             , ("lhard"                           , Left 0x021BD)
             , ("lHar"                            , Left 0x02962)
             , ("lharu"                           , Left 0x021BC)
             , ("lharul"                          , Left 0x0296A)
             , ("lhblk"                           , Left 0x02584)
             , ("LJcy"                            , Left 0x00409)
             , ("ljcy"                            , Left 0x00459)
             , ("llarr"                           , Left 0x021C7)
             , ("llcorner"                        , Left 0x0231E)
             , ("Lleftarrow"                      , Left 0x021DA)
             , ("llhard"                          , Left 0x0296B)
             , ("ll"                              , Left 0x0226A)
             , ("Ll"                              , Left 0x022D8)
             , ("lltri"                           , Left 0x025FA)
             , ("Lmidot"                          , Left 0x0013F)
             , ("lmidot"                          , Left 0x00140)
             , ("lmoustache"                      , Left 0x023B0)
             , ("lmoust"                          , Left 0x023B0)
             , ("lnap"                            , Left 0x02A89)
             , ("lnapprox"                        , Left 0x02A89)
             , ("lnE"                             , Left 0x02268)
             , ("lne"                             , Left 0x02A87)
             , ("lneq"                            , Left 0x02A87)
             , ("lneqq"                           , Left 0x02268)
             , ("lnsim"                           , Left 0x022E6)
             , ("loang"                           , Left 0x03018)
             , ("loarr"                           , Left 0x021FD)
             , ("lobrk"                           , Left 0x0301A)
             , ("longleftarrow"                   , Left 0x027F5)
             , ("LongLeftArrow"                   , Left 0x027F5)
             , ("Longleftarrow"                   , Left 0x027F8)
             , ("longleftrightarrow"              , Left 0x027F7)
             , ("LongLeftRightArrow"              , Left 0x027F7)
             , ("Longleftrightarrow"              , Left 0x027FA)
             , ("longmapsto"                      , Left 0x027FC)
             , ("longrightarrow"                  , Left 0x027F6)
             , ("LongRightArrow"                  , Left 0x027F6)
             , ("Longrightarrow"                  , Left 0x027F9)
             , ("looparrowleft"                   , Left 0x021AB)
             , ("looparrowright"                  , Left 0x021AC)
             , ("lopar"                           , Left 0x02985)
             , ("Lopf"                            , Left 0x1D543)
             , ("lopf"                            , Left 0x1D55D)
             , ("loplus"                          , Left 0x02A2D)
             , ("lotimes"                         , Left 0x02A34)
             , ("lowast"                          , Left 0x02217)
             , ("lowbar"                          , Left 0x0005F)
             , ("LowerLeftArrow"                  , Left 0x02199)
             , ("LowerRightArrow"                 , Left 0x02198)
             , ("lozenge"                         , Left 0x025CA)
             , ("lozf"                            , Left 0x029EB)
             , ("loz"                             , Left 0x025CA)
             , ("lpar"                            , Left 0x00028)
             , ("lparlt"                          , Left 0x02993)
             , ("lrarr"                           , Left 0x021C6)
             , ("lrcorner"                        , Left 0x0231F)
             , ("lrhard"                          , Left 0x0296D)
             , ("lrhar"                           , Left 0x021CB)
             , ("lrtri"                           , Left 0x022BF)
             , ("Lscr"                            , Left 0x02112)
             , ("lscr"                            , Left 0x1D4C1)
             , ("lsh"                             , Left 0x021B0)
             , ("Lsh"                             , Left 0x021B0)
             , ("lsime"                           , Left 0x02A8D)
             , ("lsimg"                           , Left 0x02A8F)
             , ("lsim"                            , Left 0x02272)
             , ("lsqb"                            , Left 0x0005B)
             , ("lsquo"                           , Left 0x02018)
             , ("lsquor"                          , Left 0x0201A)
             , ("Lstrok"                          , Left 0x00141)
             , ("lstrok"                          , Left 0x00142)
             , ("ltcc"                            , Left 0x02AA6)
             , ("ltcir"                           , Left 0x02A79)
             , ("ltdot"                           , Left 0x022D6)
             , ("lthree"                          , Left 0x022CB)
             , ("ltimes"                          , Left 0x022C9)
             , ("ltlarr"                          , Left 0x02976)
             , ("lt"                              , Left 0x0003C)
             , ("Lt"                              , Left 0x0226A)
             , ("ltquest"                         , Left 0x02A7B)
             , ("ltrie"                           , Left 0x022B4)
             , ("ltrif"                           , Left 0x025C2)
             , ("ltri"                            , Left 0x025C3)
             , ("ltrPar"                          , Left 0x02996)
             , ("lurdshar"                        , Left 0x0294A)
             , ("luruhar"                         , Left 0x02966)
             , ("macr"                            , Left 0x000AF)
             , ("male"                            , Left 0x02642)
             , ("maltese"                         , Left 0x02720)
             , ("malt"                            , Left 0x02720)
             , ("map"                             , Left 0x021A6)
             , ("Map"                             , Left 0x02905)
             , ("mapstodown"                      , Left 0x021A7)
             , ("mapsto"                          , Left 0x021A6)
             , ("mapstoleft"                      , Left 0x021A4)
             , ("mapstoup"                        , Left 0x021A5)
             , ("marker"                          , Left 0x025AE)
             , ("mcomma"                          , Left 0x02A29)
             , ("Mcy"                             , Left 0x0041C)
             , ("mcy"                             , Left 0x0043C)
             , ("mdash"                           , Left 0x02014)
             , ("mDDot"                           , Left 0x0223A)
             , ("measuredangle"                   , Left 0x02221)
             , ("MediumSpace"                     , Left 0x0205F)
             , ("Mellintrf"                       , Left 0x02133)
             , ("Mfr"                             , Left 0x1D510)
             , ("mfr"                             , Left 0x1D52A)
             , ("mho"                             , Left 0x02127)
             , ("micro"                           , Left 0x000B5)
             , ("midast"                          , Left 0x0002A)
             , ("midcir"                          , Left 0x02AF0)
             , ("middot"                          , Left 0x000B7)
             , ("mid"                             , Left 0x02223)
             , ("minusb"                          , Left 0x0229F)
             , ("minusd"                          , Left 0x02238)
             , ("minusdu"                         , Left 0x02A2A)
             , ("minus"                           , Left 0x02212)
             , ("MinusPlus"                       , Left 0x02213)
             , ("mlcp"                            , Left 0x02ADB)
             , ("mldr"                            , Left 0x02026)
             , ("mnplus"                          , Left 0x02213)
             , ("models"                          , Left 0x022A7)
             , ("Mopf"                            , Left 0x1D544)
             , ("mopf"                            , Left 0x1D55E)
             , ("mp"                              , Left 0x02213)
             , ("Mscr"                            , Left 0x02133)
             , ("mscr"                            , Left 0x1D4C2)
             , ("mstpos"                          , Left 0x0223E)
             , ("mu"                              , Left 0x003BC)
             , ("multimap"                        , Left 0x022B8)
             , ("mumap"                           , Left 0x022B8)
             , ("nabla"                           , Left 0x02207)
             , ("Nacute"                          , Left 0x00143)
             , ("nacute"                          , Left 0x00144)
             , ("nap"                             , Left 0x02249)
             , ("napos"                           , Left 0x00149)
             , ("napprox"                         , Left 0x02249)
             , ("natural"                         , Left 0x0266E)
             , ("naturals"                        , Left 0x02115)
             , ("natur"                           , Left 0x0266E)
             , ("nbsp"                            , Left 0x000A0)
             , ("ncap"                            , Left 0x02A43)
             , ("Ncaron"                          , Left 0x00147)
             , ("ncaron"                          , Left 0x00148)
             , ("Ncedil"                          , Left 0x00145)
             , ("ncedil"                          , Left 0x00146)
             , ("ncong"                           , Left 0x02247)
             , ("ncup"                            , Left 0x02A42)
             , ("Ncy"                             , Left 0x0041D)
             , ("ncy"                             , Left 0x0043D)
             , ("ndash"                           , Left 0x02013)
             , ("nearhk"                          , Left 0x02924)
             , ("nearr"                           , Left 0x02197)
             , ("neArr"                           , Left 0x021D7)
             , ("nearrow"                         , Left 0x02197)
             , ("NegativeMediumSpace"             , Left 0x0200B)
             , ("NegativeThickSpace"              , Left 0x0200B)
             , ("NegativeThinSpace"               , Left 0x0200B)
             , ("NegativeVeryThinSpace"           , Left 0x0200B)
             , ("ne"                              , Left 0x02260)
             , ("nequiv"                          , Left 0x02262)
             , ("nesear"                          , Left 0x02928)
             , ("NestedGreaterGreater"            , Left 0x0226B)
             , ("NestedLessLess"                  , Left 0x0226A)
             , ("NewLine"                         , Left 0x0000A)
             , ("nexist"                          , Left 0x02204)
             , ("nexists"                         , Left 0x02204)
             , ("Nfr"                             , Left 0x1D511)
             , ("nfr"                             , Left 0x1D52B)
             , ("nge"                             , Left 0x02271)
             , ("ngeq"                            , Left 0x02271)
             , ("ngsim"                           , Left 0x02275)
             , ("ngt"                             , Left 0x0226F)
             , ("ngtr"                            , Left 0x0226F)
             , ("nharr"                           , Left 0x021AE)
             , ("nhArr"                           , Left 0x021CE)
             , ("nhpar"                           , Left 0x02AF2)
             , ("ni"                              , Left 0x0220B)
             , ("nisd"                            , Left 0x022FA)
             , ("nis"                             , Left 0x022FC)
             , ("niv"                             , Left 0x0220B)
             , ("NJcy"                            , Left 0x0040A)
             , ("njcy"                            , Left 0x0045A)
             , ("nlarr"                           , Left 0x0219A)
             , ("nlArr"                           , Left 0x021CD)
             , ("nldr"                            , Left 0x02025)
             , ("nleftarrow"                      , Left 0x0219A)
             , ("nLeftarrow"                      , Left 0x021CD)
             , ("nleftrightarrow"                 , Left 0x021AE)
             , ("nLeftrightarrow"                 , Left 0x021CE)
             , ("nle"                             , Left 0x02270)
             , ("nleq"                            , Left 0x02270)
             , ("nless"                           , Left 0x0226E)
             , ("nlsim"                           , Left 0x02274)
             , ("nlt"                             , Left 0x0226E)
             , ("nltrie"                          , Left 0x022EC)
             , ("nltri"                           , Left 0x022EA)
             , ("nmid"                            , Left 0x02224)
             , ("NoBreak"                         , Left 0x02060)
             , ("NonBreakingSpace"                , Left 0x000A0)
             , ("Nopf"                            , Left 0x02115)
             , ("nopf"                            , Left 0x1D55F)
             , ("NotCongruent"                    , Left 0x02262)
             , ("NotCupCap"                       , Left 0x0226D)
             , ("NotDoubleVerticalBar"            , Left 0x02226)
             , ("NotElement"                      , Left 0x02209)
             , ("NotEqual"                        , Left 0x02260)
             , ("NotExists"                       , Left 0x02204)
             , ("NotGreaterEqual"                 , Left 0x02271)
             , ("NotGreater"                      , Left 0x0226F)
             , ("NotGreaterLess"                  , Left 0x02279)
             , ("NotGreaterTilde"                 , Left 0x02275)
             , ("notin"                           , Left 0x02209)
             , ("notinva"                         , Left 0x02209)
             , ("notinvb"                         , Left 0x022F7)
             , ("notinvc"                         , Left 0x022F6)
             , ("not"                             , Left 0x000AC)
             , ("Not"                             , Left 0x02AEC)
             , ("NotLeftTriangleEqual"            , Left 0x022EC)
             , ("NotLeftTriangle"                 , Left 0x022EA)
             , ("NotLessEqual"                    , Left 0x02270)
             , ("NotLessGreater"                  , Left 0x02278)
             , ("NotLess"                         , Left 0x0226E)
             , ("NotLessTilde"                    , Left 0x02274)
             , ("notni"                           , Left 0x0220C)
             , ("notniva"                         , Left 0x0220C)
             , ("notnivb"                         , Left 0x022FE)
             , ("notnivc"                         , Left 0x022FD)
             , ("NotPrecedes"                     , Left 0x02280)
             , ("NotPrecedesSlantEqual"           , Left 0x022E0)
             , ("NotReverseElement"               , Left 0x0220C)
             , ("NotRightTriangleEqual"           , Left 0x022ED)
             , ("NotRightTriangle"                , Left 0x022EB)
             , ("NotSquareSubsetEqual"            , Left 0x022E2)
             , ("NotSquareSupersetEqual"          , Left 0x022E3)
             , ("NotSubsetEqual"                  , Left 0x02288)
             , ("NotSucceeds"                     , Left 0x02281)
             , ("NotSucceedsSlantEqual"           , Left 0x022E1)
             , ("NotSupersetEqual"                , Left 0x02289)
             , ("NotTildeEqual"                   , Left 0x02244)
             , ("NotTildeFullEqual"               , Left 0x02247)
             , ("NotTilde"                        , Left 0x02241)
             , ("NotTildeTilde"                   , Left 0x02249)
             , ("NotVerticalBar"                  , Left 0x02224)
             , ("nparallel"                       , Left 0x02226)
             , ("npar"                            , Left 0x02226)
             , ("npolint"                         , Left 0x02A14)
             , ("nprcue"                          , Left 0x022E0)
             , ("nprec"                           , Left 0x02280)
             , ("npr"                             , Left 0x02280)
             , ("nrarr"                           , Left 0x0219B)
             , ("nrArr"                           , Left 0x021CF)
             , ("nrightarrow"                     , Left 0x0219B)
             , ("nRightarrow"                     , Left 0x021CF)
             , ("nrtrie"                          , Left 0x022ED)
             , ("nrtri"                           , Left 0x022EB)
             , ("nsccue"                          , Left 0x022E1)
             , ("nsc"                             , Left 0x02281)
             , ("Nscr"                            , Left 0x1D4A9)
             , ("nscr"                            , Left 0x1D4C3)
             , ("nshortmid"                       , Left 0x02224)
             , ("nshortparallel"                  , Left 0x02226)
             , ("nsime"                           , Left 0x02244)
             , ("nsimeq"                          , Left 0x02244)
             , ("nsim"                            , Left 0x02241)
             , ("nsmid"                           , Left 0x02224)
             , ("nspar"                           , Left 0x02226)
             , ("nsqsube"                         , Left 0x022E2)
             , ("nsqsupe"                         , Left 0x022E3)
             , ("nsube"                           , Left 0x02288)
             , ("nsub"                            , Left 0x02284)
             , ("nsubseteq"                       , Left 0x02288)
             , ("nsucc"                           , Left 0x02281)
             , ("nsupe"                           , Left 0x02289)
             , ("nsup"                            , Left 0x02285)
             , ("nsupseteq"                       , Left 0x02289)
             , ("ntgl"                            , Left 0x02279)
             , ("Ntilde"                          , Left 0x000D1)
             , ("ntilde"                          , Left 0x000F1)
             , ("ntlg"                            , Left 0x02278)
             , ("ntrianglelefteq"                 , Left 0x022EC)
             , ("ntriangleleft"                   , Left 0x022EA)
             , ("ntrianglerighteq"                , Left 0x022ED)
             , ("ntriangleright"                  , Left 0x022EB)
             , ("nu"                              , Left 0x003BD)
             , ("numero"                          , Left 0x02116)
             , ("num"                             , Left 0x00023)
             , ("numsp"                           , Left 0x02007)
             , ("nvdash"                          , Left 0x022AC)
             , ("nvDash"                          , Left 0x022AD)
             , ("nVdash"                          , Left 0x022AE)
             , ("nVDash"                          , Left 0x022AF)
             , ("nvHarr"                          , Left 0x02904)
             , ("nvinfin"                         , Left 0x029DE)
             , ("nvlArr"                          , Left 0x02902)
             , ("nvrArr"                          , Left 0x02903)
             , ("nwarhk"                          , Left 0x02923)
             , ("nwarr"                           , Left 0x02196)
             , ("nwArr"                           , Left 0x021D6)
             , ("nwarrow"                         , Left 0x02196)
             , ("nwnear"                          , Left 0x02927)
             , ("Oacute"                          , Left 0x000D3)
             , ("oacute"                          , Left 0x000F3)
             , ("oast"                            , Left 0x0229B)
             , ("Ocirc"                           , Left 0x000D4)
             , ("ocirc"                           , Left 0x000F4)
             , ("ocir"                            , Left 0x0229A)
             , ("Ocy"                             , Left 0x0041E)
             , ("ocy"                             , Left 0x0043E)
             , ("odash"                           , Left 0x0229D)
             , ("Odblac"                          , Left 0x00150)
             , ("odblac"                          , Left 0x00151)
             , ("odiv"                            , Left 0x02A38)
             , ("odot"                            , Left 0x02299)
             , ("odsold"                          , Left 0x029BC)
             , ("OElig"                           , Left 0x00152)
             , ("oelig"                           , Left 0x00153)
             , ("ofcir"                           , Left 0x029BF)
             , ("Ofr"                             , Left 0x1D512)
             , ("ofr"                             , Left 0x1D52C)
             , ("ogon"                            , Left 0x002DB)
             , ("Ograve"                          , Left 0x000D2)
             , ("ograve"                          , Left 0x000F2)
             , ("ogt"                             , Left 0x029C1)
             , ("ohbar"                           , Left 0x029B5)
             , ("ohm"                             , Left 0x02126)
             , ("oint"                            , Left 0x0222E)
             , ("olarr"                           , Left 0x021BA)
             , ("olcir"                           , Left 0x029BE)
             , ("olcross"                         , Left 0x029BB)
             , ("olt"                             , Left 0x029C0)
             , ("Omacr"                           , Left 0x0014C)
             , ("omacr"                           , Left 0x0014D)
             , ("Omega"                           , Left 0x003A9)
             , ("omega"                           , Left 0x003C9)
             , ("omid"                            , Left 0x029B6)
             , ("ominus"                          , Left 0x02296)
             , ("Oopf"                            , Left 0x1D546)
             , ("oopf"                            , Left 0x1D560)
             , ("opar"                            , Left 0x029B7)
             , ("OpenCurlyDoubleQuote"            , Left 0x0201C)
             , ("OpenCurlyQuote"                  , Left 0x02018)
             , ("operp"                           , Left 0x029B9)
             , ("oplus"                           , Left 0x02295)
             , ("orarr"                           , Left 0x021BB)
             , ("order"                           , Left 0x02134)
             , ("orderof"                         , Left 0x02134)
             , ("ordf"                            , Left 0x000AA)
             , ("ord"                             , Left 0x02A5D)
             , ("ordm"                            , Left 0x000BA)
             , ("origof"                          , Left 0x022B6)
             , ("or"                              , Left 0x02228)
             , ("Or"                              , Left 0x02A54)
             , ("oror"                            , Left 0x02A56)
             , ("orslope"                         , Left 0x02A57)
             , ("orv"                             , Left 0x02A5B)
             , ("oscr"                            , Left 0x02134)
             , ("Oscr"                            , Left 0x1D4AA)
             , ("Oslash"                          , Left 0x000D8)
             , ("oslash"                          , Left 0x000F8)
             , ("oS"                              , Left 0x024C8)
             , ("osol"                            , Left 0x02298)
             , ("Otilde"                          , Left 0x000D5)
             , ("otilde"                          , Left 0x000F5)
             , ("otimesas"                        , Left 0x02A36)
             , ("otimes"                          , Left 0x02297)
             , ("Otimes"                          , Left 0x02A37)
             , ("Ouml"                            , Left 0x000D6)
             , ("ouml"                            , Left 0x000F6)
             , ("ovbar"                           , Left 0x0233D)
             , ("OverBar"                         , Left 0x000AF)
             , ("OverBrace"                       , Left 0x0FE37)
             , ("OverBracket"                     , Left 0x023B4)
             , ("OverParenthesis"                 , Left 0x0FE35)
             , ("para"                            , Left 0x000B6)
             , ("parallel"                        , Left 0x02225)
             , ("par"                             , Left 0x02225)
             , ("parsim"                          , Left 0x02AF3)
             , ("parsl"                           , Left 0x02AFD)
             , ("PartialD"                        , Left 0x02202)
             , ("part"                            , Left 0x02202)
             , ("Pcy"                             , Left 0x0041F)
             , ("pcy"                             , Left 0x0043F)
             , ("percnt"                          , Left 0x00025)
             , ("period"                          , Left 0x0002E)
             , ("permil"                          , Left 0x02030)
             , ("perp"                            , Left 0x022A5)
             , ("pertenk"                         , Left 0x02031)
             , ("Pfr"                             , Left 0x1D513)
             , ("pfr"                             , Left 0x1D52D)
             , ("Phi"                             , Left 0x003A6)
             , ("phi"                             , Left 0x003D5)
             , ("phiv"                            , Left 0x003C6)
             , ("phmmat"                          , Left 0x02133)
             , ("phone"                           , Left 0x0260E)
             , ("Pi"                              , Left 0x003A0)
             , ("pi"                              , Left 0x003C0)
             , ("pitchfork"                       , Left 0x022D4)
             , ("piv"                             , Left 0x003D6)
             , ("planckh"                         , Left 0x0210E)
             , ("planck"                          , Left 0x0210F)
             , ("plankv"                          , Left 0x0210F)
             , ("plusacir"                        , Left 0x02A23)
             , ("plusb"                           , Left 0x0229E)
             , ("pluscir"                         , Left 0x02A22)
             , ("plusdo"                          , Left 0x02214)
             , ("plusdu"                          , Left 0x02A25)
             , ("pluse"                           , Left 0x02A72)
             , ("plus"                            , Left 0x0002B)
             , ("PlusMinus"                       , Left 0x000B1)
             , ("plusmn"                          , Left 0x000B1)
             , ("plussim"                         , Left 0x02A26)
             , ("plustwo"                         , Left 0x02A27)
             , ("pm"                              , Left 0x000B1)
             , ("Poincareplane"                   , Left 0x0210C)
             , ("pointint"                        , Left 0x02A15)
             , ("Popf"                            , Left 0x02119)
             , ("popf"                            , Left 0x1D561)
             , ("pound"                           , Left 0x000A3)
             , ("prap"                            , Left 0x02AB7)
             , ("prcue"                           , Left 0x0227C)
             , ("precapprox"                      , Left 0x02AB7)
             , ("preccurlyeq"                     , Left 0x0227C)
             , ("PrecedesEqual"                   , Left 0x02AAF)
             , ("Precedes"                        , Left 0x0227A)
             , ("PrecedesSlantEqual"              , Left 0x0227C)
             , ("PrecedesTilde"                   , Left 0x0227E)
             , ("preceq"                          , Left 0x02AAF)
             , ("prec"                            , Left 0x0227A)
             , ("precnapprox"                     , Left 0x02AB9)
             , ("precneqq"                        , Left 0x02AB5)
             , ("precnsim"                        , Left 0x022E8)
             , ("precsim"                         , Left 0x0227E)
             , ("pre"                             , Left 0x02AAF)
             , ("prE"                             , Left 0x02AB3)
             , ("prime"                           , Left 0x02032)
             , ("Prime"                           , Left 0x02033)
             , ("primes"                          , Left 0x02119)
             , ("pr"                              , Left 0x0227A)
             , ("Pr"                              , Left 0x02ABB)
             , ("prnap"                           , Left 0x02AB9)
             , ("prnE"                            , Left 0x02AB5)
             , ("prnsim"                          , Left 0x022E8)
             , ("prod"                            , Left 0x0220F)
             , ("Product"                         , Left 0x0220F)
             , ("profalar"                        , Left 0x0232E)
             , ("profline"                        , Left 0x02312)
             , ("profsurf"                        , Left 0x02313)
             , ("prop"                            , Left 0x0221D)
             , ("Proportional"                    , Left 0x0221D)
             , ("Proportion"                      , Left 0x02237)
             , ("propto"                          , Left 0x0221D)
             , ("prsim"                           , Left 0x0227E)
             , ("prurel"                          , Left 0x022B0)
             , ("Pscr"                            , Left 0x1D4AB)
             , ("pscr"                            , Left 0x1D4C5)
             , ("Psi"                             , Left 0x003A8)
             , ("psi"                             , Left 0x003C8)
             , ("puncsp"                          , Left 0x02008)
             , ("Qfr"                             , Left 0x1D514)
             , ("qfr"                             , Left 0x1D52E)
             , ("qint"                            , Left 0x02A0C)
             , ("Qopf"                            , Left 0x0211A)
             , ("qopf"                            , Left 0x1D562)
             , ("qprime"                          , Left 0x02057)
             , ("Qscr"                            , Left 0x1D4AC)
             , ("qscr"                            , Left 0x1D4C6)
             , ("quaternions"                     , Left 0x0210D)
             , ("quatint"                         , Left 0x02A16)
             , ("questeq"                         , Left 0x0225F)
             , ("quest"                           , Left 0x0003F)
             , ("quot"                            , Left 0x00022)
             , ("rAarr"                           , Left 0x021DB)
             , ("race"                            , Left 0x029DA)
             , ("Racute"                          , Left 0x00154)
             , ("racute"                          , Left 0x00155)
             , ("radic"                           , Left 0x0221A)
             , ("raemptyv"                        , Left 0x029B3)
             , ("rangd"                           , Left 0x02992)
             , ("range"                           , Left 0x029A5)
             , ("rang"                            , Left 0x0232A)
             , ("Rang"                            , Left 0x0300B)
             , ("rangle"                          , Left 0x0232A)
             , ("raquo"                           , Left 0x000BB)
             , ("rarrap"                          , Left 0x02975)
             , ("rarrbfs"                         , Left 0x02920)
             , ("rarrb"                           , Left 0x021E5)
             , ("rarrc"                           , Left 0x02933)
             , ("rarrfs"                          , Left 0x0291E)
             , ("rarrhk"                          , Left 0x021AA)
             , ("rarr"                            , Left 0x02192)
             , ("Rarr"                            , Left 0x021A0)
             , ("rArr"                            , Left 0x021D2)
             , ("rarrlp"                          , Left 0x021AC)
             , ("rarrpl"                          , Left 0x02945)
             , ("rarrsim"                         , Left 0x02974)
             , ("rarrtl"                          , Left 0x021A3)
             , ("Rarrtl"                          , Left 0x02916)
             , ("rarrw"                           , Left 0x0219D)
             , ("ratail"                          , Left 0x0291A)
             , ("rAtail"                          , Left 0x0291C)
             , ("ratio"                           , Left 0x02236)
             , ("rationals"                       , Left 0x0211A)
             , ("rbarr"                           , Left 0x0290D)
             , ("rBarr"                           , Left 0x0290F)
             , ("RBarr"                           , Left 0x02910)
             , ("rbbrk"                           , Left 0x03015)
             , ("rbrace"                          , Left 0x0007D)
             , ("rbrack"                          , Left 0x0005D)
             , ("rbrke"                           , Left 0x0298C)
             , ("rbrksld"                         , Left 0x0298E)
             , ("rbrkslu"                         , Left 0x02990)
             , ("Rcaron"                          , Left 0x00158)
             , ("rcaron"                          , Left 0x00159)
             , ("Rcedil"                          , Left 0x00156)
             , ("rcedil"                          , Left 0x00157)
             , ("rceil"                           , Left 0x02309)
             , ("rcub"                            , Left 0x0007D)
             , ("Rcy"                             , Left 0x00420)
             , ("rcy"                             , Left 0x00440)
             , ("rdca"                            , Left 0x02937)
             , ("rdldhar"                         , Left 0x02969)
             , ("rdquo"                           , Left 0x0201D)
             , ("rdquor"                          , Left 0x0201D)
             , ("rdsh"                            , Left 0x021B3)
             , ("realine"                         , Left 0x0211B)
             , ("real"                            , Left 0x0211C)
             , ("realpart"                        , Left 0x0211C)
             , ("reals"                           , Left 0x0211D)
             , ("rect"                            , Left 0x025AD)
             , ("reg"                             , Left 0x000AE)
             , ("Re"                              , Left 0x0211C)
             , ("ReverseElement"                  , Left 0x0220B)
             , ("ReverseEquilibrium"              , Left 0x021CB)
             , ("ReverseUpEquilibrium"            , Left 0x0296F)
             , ("rfisht"                          , Left 0x0297D)
             , ("rfloor"                          , Left 0x0230B)
             , ("Rfr"                             , Left 0x0211C)
             , ("rfr"                             , Left 0x1D52F)
             , ("rhard"                           , Left 0x021C1)
             , ("rHar"                            , Left 0x02964)
             , ("rharu"                           , Left 0x021C0)
             , ("rharul"                          , Left 0x0296C)
             , ("rho"                             , Left 0x003C1)
             , ("rhov"                            , Left 0x003F1)
             , ("RightAngleBracket"               , Left 0x0232A)
             , ("RightArrowBar"                   , Left 0x021E5)
             , ("rightarrow"                      , Left 0x02192)
             , ("RightArrow"                      , Left 0x02192)
             , ("Rightarrow"                      , Left 0x021D2)
             , ("RightArrowLeftArrow"             , Left 0x021C4)
             , ("rightarrowtail"                  , Left 0x021A3)
             , ("RightCeiling"                    , Left 0x02309)
             , ("RightDoubleBracket"              , Left 0x0301B)
             , ("RightDownTeeVector"              , Left 0x0295D)
             , ("RightDownVectorBar"              , Left 0x02955)
             , ("RightDownVector"                 , Left 0x021C2)
             , ("RightFloor"                      , Left 0x0230B)
             , ("rightharpoondown"                , Left 0x021C1)
             , ("rightharpoonup"                  , Left 0x021C0)
             , ("rightleftarrows"                 , Left 0x021C4)
             , ("rightleftharpoons"               , Left 0x021CC)
             , ("rightrightarrows"                , Left 0x021C9)
             , ("rightsquigarrow"                 , Left 0x0219D)
             , ("RightTeeArrow"                   , Left 0x021A6)
             , ("RightTee"                        , Left 0x022A2)
             , ("RightTeeVector"                  , Left 0x0295B)
             , ("rightthreetimes"                 , Left 0x022CC)
             , ("RightTriangleBar"                , Left 0x029D0)
             , ("RightTriangleEqual"              , Left 0x022B5)
             , ("RightTriangle"                   , Left 0x022B3)
             , ("RightUpDownVector"               , Left 0x0294F)
             , ("RightUpTeeVector"                , Left 0x0295C)
             , ("RightUpVectorBar"                , Left 0x02954)
             , ("RightUpVector"                   , Left 0x021BE)
             , ("RightVectorBar"                  , Left 0x02953)
             , ("RightVector"                     , Left 0x021C0)
             , ("ring"                            , Left 0x002DA)
             , ("risingdotseq"                    , Left 0x02253)
             , ("rlarr"                           , Left 0x021C4)
             , ("rlhar"                           , Left 0x021CC)
             , ("rmoustache"                      , Left 0x023B1)
             , ("rmoust"                          , Left 0x023B1)
             , ("rnmid"                           , Left 0x02AEE)
             , ("roang"                           , Left 0x03019)
             , ("roarr"                           , Left 0x021FE)
             , ("robrk"                           , Left 0x0301B)
             , ("ropar"                           , Left 0x02986)
             , ("Ropf"                            , Left 0x0211D)
             , ("ropf"                            , Left 0x1D563)
             , ("roplus"                          , Left 0x02A2E)
             , ("rotimes"                         , Left 0x02A35)
             , ("RoundImplies"                    , Left 0x02970)
             , ("rpargt"                          , Left 0x02994)
             , ("rpar"                            , Left 0x00029)
             , ("rppolint"                        , Left 0x02A12)
             , ("rrarr"                           , Left 0x021C9)
             , ("Rrightarrow"                     , Left 0x021DB)
             , ("Rscr"                            , Left 0x0211B)
             , ("rscr"                            , Left 0x1D4C7)
             , ("rsh"                             , Left 0x021B1)
             , ("Rsh"                             , Left 0x021B1)
             , ("rsqb"                            , Left 0x0005D)
             , ("rsquo"                           , Left 0x02019)
             , ("rsquor"                          , Left 0x02019)
             , ("rthree"                          , Left 0x022CC)
             , ("rtimes"                          , Left 0x022CA)
             , ("rtrie"                           , Left 0x022B5)
             , ("rtrif"                           , Left 0x025B8)
             , ("rtri"                            , Left 0x025B9)
             , ("rtriltri"                        , Left 0x029CE)
             , ("RuleDelayed"                     , Left 0x029F4)
             , ("ruluhar"                         , Left 0x02968)
             , ("rx"                              , Left 0x0211E)
             , ("Sacute"                          , Left 0x0015A)
             , ("sacute"                          , Left 0x0015B)
             , ("scap"                            , Left 0x02AB8)
             , ("Scaron"                          , Left 0x00160)
             , ("scaron"                          , Left 0x00161)
             , ("sccue"                           , Left 0x0227D)
             , ("Scedil"                          , Left 0x0015E)
             , ("scedil"                          , Left 0x0015F)
             , ("sce"                             , Left 0x02AB0)
             , ("scE"                             , Left 0x02AB4)
             , ("Scirc"                           , Left 0x0015C)
             , ("scirc"                           , Left 0x0015D)
             , ("sc"                              , Left 0x0227B)
             , ("Sc"                              , Left 0x02ABC)
             , ("scnap"                           , Left 0x02ABA)
             , ("scnE"                            , Left 0x02AB6)
             , ("scnsim"                          , Left 0x022E9)
             , ("scpolint"                        , Left 0x02A13)
             , ("scsim"                           , Left 0x0227F)
             , ("Scy"                             , Left 0x00421)
             , ("scy"                             , Left 0x00441)
             , ("sdotb"                           , Left 0x022A1)
             , ("sdote"                           , Left 0x02A66)
             , ("sdot"                            , Left 0x022C5)
             , ("searhk"                          , Left 0x02925)
             , ("searr"                           , Left 0x02198)
             , ("seArr"                           , Left 0x021D8)
             , ("searrow"                         , Left 0x02198)
             , ("sect"                            , Left 0x000A7)
             , ("semi"                            , Left 0x0003B)
             , ("seswar"                          , Left 0x02929)
             , ("setminus"                        , Left 0x02216)
             , ("setmn"                           , Left 0x02216)
             , ("sext"                            , Left 0x02736)
             , ("Sfr"                             , Left 0x1D516)
             , ("sfr"                             , Left 0x1D530)
             , ("sfrown"                          , Left 0x02322)
             , ("sharp"                           , Left 0x0266F)
             , ("SHCHcy"                          , Left 0x00429)
             , ("shchcy"                          , Left 0x00449)
             , ("SHcy"                            , Left 0x00428)
             , ("shcy"                            , Left 0x00448)
             , ("ShortDownArrow"                  , Left 0x02193)
             , ("ShortLeftArrow"                  , Left 0x02190)
             , ("shortmid"                        , Left 0x02223)
             , ("shortparallel"                   , Left 0x02225)
             , ("ShortRightArrow"                 , Left 0x02192)
             , ("ShortUpArrow"                    , Left 0x02191)
             , ("shy"                             , Left 0x000AD)
             , ("Sigma"                           , Left 0x003A3)
             , ("sigma"                           , Left 0x003C3)
             , ("sigmav"                          , Left 0x003C2)
             , ("simdot"                          , Left 0x02A6A)
             , ("sime"                            , Left 0x02243)
             , ("simeq"                           , Left 0x02243)
             , ("simgE"                           , Left 0x02AA0)
             , ("simg"                            , Left 0x02A9E)
             , ("sim"                             , Left 0x0223C)
             , ("simlE"                           , Left 0x02A9F)
             , ("siml"                            , Left 0x02A9D)
             , ("simne"                           , Left 0x02246)
             , ("simplus"                         , Left 0x02A24)
             , ("simrarr"                         , Left 0x02972)
             , ("slarr"                           , Left 0x02190)
             , ("SmallCircle"                     , Left 0x02218)
             , ("smallsetminus"                   , Left 0x02216)
             , ("smashp"                          , Left 0x02A33)
             , ("smeparsl"                        , Left 0x029E4)
             , ("smid"                            , Left 0x02223)
             , ("smile"                           , Left 0x02323)
             , ("smte"                            , Left 0x02AAC)
             , ("smt"                             , Left 0x02AAA)
             , ("SOFTcy"                          , Left 0x0042C)
             , ("softcy"                          , Left 0x0044C)
             , ("solbar"                          , Left 0x0233F)
             , ("solb"                            , Left 0x029C4)
             , ("sol"                             , Left 0x0002F)
             , ("Sopf"                            , Left 0x1D54A)
             , ("sopf"                            , Left 0x1D564)
             , ("spades"                          , Left 0x02660)
             , ("spadesuit"                       , Left 0x02660)
             , ("spar"                            , Left 0x02225)
             , ("sqcap"                           , Left 0x02293)
             , ("sqcup"                           , Left 0x02294)
             , ("Sqrt"                            , Left 0x0221A)
             , ("sqsube"                          , Left 0x02291)
             , ("sqsub"                           , Left 0x0228F)
             , ("sqsubseteq"                      , Left 0x02291)
             , ("sqsubset"                        , Left 0x0228F)
             , ("sqsupe"                          , Left 0x02292)
             , ("sqsup"                           , Left 0x02290)
             , ("sqsupseteq"                      , Left 0x02292)
             , ("sqsupset"                        , Left 0x02290)
             , ("SquareIntersection"              , Left 0x02293)
             , ("square"                          , Left 0x025A1)
             , ("Square"                          , Left 0x025A1)
             , ("SquareSubsetEqual"               , Left 0x02291)
             , ("SquareSubset"                    , Left 0x0228F)
             , ("SquareSupersetEqual"             , Left 0x02292)
             , ("SquareSuperset"                  , Left 0x02290)
             , ("SquareUnion"                     , Left 0x02294)
             , ("squarf"                          , Left 0x025AA)
             , ("squf"                            , Left 0x025AA)
             , ("squ"                             , Left 0x025A1)
             , ("srarr"                           , Left 0x02192)
             , ("Sscr"                            , Left 0x1D4AE)
             , ("sscr"                            , Left 0x1D4C8)
             , ("ssetmn"                          , Left 0x02216)
             , ("ssmile"                          , Left 0x02323)
             , ("sstarf"                          , Left 0x022C6)
             , ("starf"                           , Left 0x02605)
             , ("Star"                            , Left 0x022C6)
             , ("star"                            , Left 0x02606)
             , ("straightepsilon"                 , Left 0x003F5)
             , ("straightphi"                     , Left 0x003D5)
             , ("strns"                           , Left 0x000AF)
             , ("subdot"                          , Left 0x02ABD)
             , ("subedot"                         , Left 0x02AC3)
             , ("sube"                            , Left 0x02286)
             , ("subE"                            , Left 0x02AC5)
             , ("sub"                             , Left 0x02282)
             , ("Sub"                             , Left 0x022D0)
             , ("submult"                         , Left 0x02AC1)
             , ("subne"                           , Left 0x0228A)
             , ("subnE"                           , Left 0x02ACB)
             , ("subplus"                         , Left 0x02ABF)
             , ("subrarr"                         , Left 0x02979)
             , ("subseteq"                        , Left 0x02286)
             , ("subseteqq"                       , Left 0x02AC5)
             , ("SubsetEqual"                     , Left 0x02286)
             , ("subset"                          , Left 0x02282)
             , ("Subset"                          , Left 0x022D0)
             , ("subsetneq"                       , Left 0x0228A)
             , ("subsetneqq"                      , Left 0x02ACB)
             , ("subsim"                          , Left 0x02AC7)
             , ("subsub"                          , Left 0x02AD5)
             , ("subsup"                          , Left 0x02AD3)
             , ("succapprox"                      , Left 0x02AB8)
             , ("succcurlyeq"                     , Left 0x0227D)
             , ("SucceedsEqual"                   , Left 0x02AB0)
             , ("Succeeds"                        , Left 0x0227B)
             , ("SucceedsSlantEqual"              , Left 0x0227D)
             , ("SucceedsTilde"                   , Left 0x0227F)
             , ("succeq"                          , Left 0x02AB0)
             , ("succ"                            , Left 0x0227B)
             , ("succnapprox"                     , Left 0x02ABA)
             , ("succneqq"                        , Left 0x02AB6)
             , ("succnsim"                        , Left 0x022E9)
             , ("succsim"                         , Left 0x0227F)
             , ("SuchThat"                        , Left 0x0220B)
             , ("sum"                             , Left 0x02211)
             , ("Sum"                             , Left 0x02211)
             , ("sung"                            , Left 0x0266A)
             , ("sup1"                            , Left 0x000B9)
             , ("sup2"                            , Left 0x000B2)
             , ("sup3"                            , Left 0x000B3)
             , ("supdot"                          , Left 0x02ABE)
             , ("supdsub"                         , Left 0x02AD8)
             , ("supedot"                         , Left 0x02AC4)
             , ("supe"                            , Left 0x02287)
             , ("supE"                            , Left 0x02AC6)
             , ("SupersetEqual"                   , Left 0x02287)
             , ("Superset"                        , Left 0x02283)
             , ("suphsub"                         , Left 0x02AD7)
             , ("suplarr"                         , Left 0x0297B)
             , ("sup"                             , Left 0x02283)
             , ("Sup"                             , Left 0x022D1)
             , ("supmult"                         , Left 0x02AC2)
             , ("supne"                           , Left 0x0228B)
             , ("supnE"                           , Left 0x02ACC)
             , ("supplus"                         , Left 0x02AC0)
             , ("supseteq"                        , Left 0x02287)
             , ("supseteqq"                       , Left 0x02AC6)
             , ("supset"                          , Left 0x02283)
             , ("Supset"                          , Left 0x022D1)
             , ("supsetneq"                       , Left 0x0228B)
             , ("supsetneqq"                      , Left 0x02ACC)
             , ("supsim"                          , Left 0x02AC8)
             , ("supsub"                          , Left 0x02AD4)
             , ("supsup"                          , Left 0x02AD6)
             , ("swarhk"                          , Left 0x02926)
             , ("swarr"                           , Left 0x02199)
             , ("swArr"                           , Left 0x021D9)
             , ("swarrow"                         , Left 0x02199)
             , ("swnwar"                          , Left 0x0292A)
             , ("szlig"                           , Left 0x000DF)
             , ("Tab"                             , Left 0x00009)
             , ("target"                          , Left 0x02316)
             , ("tau"                             , Left 0x003C4)
             , ("tbrk"                            , Left 0x023B4)
             , ("Tcaron"                          , Left 0x00164)
             , ("tcaron"                          , Left 0x00165)
             , ("Tcedil"                          , Left 0x00162)
             , ("tcedil"                          , Left 0x00163)
             , ("Tcy"                             , Left 0x00422)
             , ("tcy"                             , Left 0x00442)
             , ("tdot"                            , Left 0x020DB)
             , ("telrec"                          , Left 0x02315)
             , ("Tfr"                             , Left 0x1D517)
             , ("tfr"                             , Left 0x1D531)
             , ("there4"                          , Left 0x02234)
             , ("therefore"                       , Left 0x02234)
             , ("Therefore"                       , Left 0x02234)
             , ("Theta"                           , Left 0x00398)
             , ("theta"                           , Left 0x003B8)
             , ("thetav"                          , Left 0x003D1)
             , ("thickapprox"                     , Left 0x02248)
             , ("thicksim"                        , Left 0x0223C)
             , ("ThinSpace"                       , Left 0x02009)
             , ("thinsp"                          , Left 0x02009)
             , ("thkap"                           , Left 0x02248)
             , ("thksim"                          , Left 0x0223C)
             , ("THORN"                           , Left 0x000DE)
             , ("thorn"                           , Left 0x000FE)
             , ("TildeEqual"                      , Left 0x02243)
             , ("TildeFullEqual"                  , Left 0x02245)
             , ("tilde"                           , Left 0x002DC)
             , ("Tilde"                           , Left 0x0223C)
             , ("TildeTilde"                      , Left 0x02248)
             , ("timesbar"                        , Left 0x02A31)
             , ("timesb"                          , Left 0x022A0)
             , ("timesd"                          , Left 0x02A30)
             , ("times"                           , Left 0x000D7)
             , ("tint"                            , Left 0x0222D)
             , ("toea"                            , Left 0x02928)
             , ("topbot"                          , Left 0x02336)
             , ("topcir"                          , Left 0x02AF1)
             , ("Topf"                            , Left 0x1D54B)
             , ("topf"                            , Left 0x1D565)
             , ("topfork"                         , Left 0x02ADA)
             , ("top"                             , Left 0x022A4)
             , ("tosa"                            , Left 0x02929)
             , ("tprime"                          , Left 0x02034)
             , ("trade"                           , Left 0x02122)
             , ("triangledown"                    , Left 0x025BF)
             , ("triangle"                        , Left 0x025B5)
             , ("trianglelefteq"                  , Left 0x022B4)
             , ("triangleleft"                    , Left 0x025C3)
             , ("triangleq"                       , Left 0x0225C)
             , ("trianglerighteq"                 , Left 0x022B5)
             , ("triangleright"                   , Left 0x025B9)
             , ("tridot"                          , Left 0x025EC)
             , ("trie"                            , Left 0x0225C)
             , ("triminus"                        , Left 0x02A3A)
             , ("TripleDot"                       , Left 0x020DB)
             , ("triplus"                         , Left 0x02A39)
             , ("trisb"                           , Left 0x029CD)
             , ("tritime"                         , Left 0x02A3B)
             , ("trpezium"                        , Left 0x0FFFD)
             , ("Tscr"                            , Left 0x1D4AF)
             , ("tscr"                            , Left 0x1D4C9)
             , ("TScy"                            , Left 0x00426)
             , ("tscy"                            , Left 0x00446)
             , ("TSHcy"                           , Left 0x0040B)
             , ("tshcy"                           , Left 0x0045B)
             , ("Tstrok"                          , Left 0x00166)
             , ("tstrok"                          , Left 0x00167)
             , ("twixt"                           , Left 0x0226C)
             , ("twoheadleftarrow"                , Left 0x0219E)
             , ("twoheadrightarrow"               , Left 0x021A0)
             , ("Uacute"                          , Left 0x000DA)
             , ("uacute"                          , Left 0x000FA)
             , ("uarr"                            , Left 0x02191)
             , ("Uarr"                            , Left 0x0219F)
             , ("uArr"                            , Left 0x021D1)
             , ("Uarrocir"                        , Left 0x02949)
             , ("Ubrcy"                           , Left 0x0040E)
             , ("ubrcy"                           , Left 0x0045E)
             , ("Ubreve"                          , Left 0x0016C)
             , ("ubreve"                          , Left 0x0016D)
             , ("Ucirc"                           , Left 0x000DB)
             , ("ucirc"                           , Left 0x000FB)
             , ("Ucy"                             , Left 0x00423)
             , ("ucy"                             , Left 0x00443)
             , ("udarr"                           , Left 0x021C5)
             , ("Udblac"                          , Left 0x00170)
             , ("udblac"                          , Left 0x00171)
             , ("udhar"                           , Left 0x0296E)
             , ("ufisht"                          , Left 0x0297E)
             , ("Ufr"                             , Left 0x1D518)
             , ("ufr"                             , Left 0x1D532)
             , ("Ugrave"                          , Left 0x000D9)
             , ("ugrave"                          , Left 0x000F9)
             , ("uHar"                            , Left 0x02963)
             , ("uharl"                           , Left 0x021BF)
             , ("uharr"                           , Left 0x021BE)
             , ("uhblk"                           , Left 0x02580)
             , ("ulcorner"                        , Left 0x0231C)
             , ("ulcorn"                          , Left 0x0231C)
             , ("ulcrop"                          , Left 0x0230F)
             , ("ultri"                           , Left 0x025F8)
             , ("Umacr"                           , Left 0x0016A)
             , ("umacr"                           , Left 0x0016B)
             , ("uml"                             , Left 0x000A8)
             , ("UnderBar"                        , Left 0x00332)
             , ("UnderBrace"                      , Left 0x0FE38)
             , ("UnderBracket"                    , Left 0x023B5)
             , ("UnderParenthesis"                , Left 0x0FE36)
             , ("Union"                           , Left 0x022C3)
             , ("UnionPlus"                       , Left 0x0228E)
             , ("Uogon"                           , Left 0x00172)
             , ("uogon"                           , Left 0x00173)
             , ("Uopf"                            , Left 0x1D54C)
             , ("uopf"                            , Left 0x1D566)
             , ("UpArrowBar"                      , Left 0x02912)
             , ("UpArrowDownArrow"                , Left 0x021C5)
             , ("uparrow"                         , Left 0x02191)
             , ("UpArrow"                         , Left 0x02191)
             , ("Uparrow"                         , Left 0x021D1)
             , ("updownarrow"                     , Left 0x02195)
             , ("UpDownArrow"                     , Left 0x02195)
             , ("Updownarrow"                     , Left 0x021D5)
             , ("UpEquilibrium"                   , Left 0x0296E)
             , ("upharpoonleft"                   , Left 0x021BF)
             , ("upharpoonright"                  , Left 0x021BE)
             , ("uplus"                           , Left 0x0228E)
             , ("UpperLeftArrow"                  , Left 0x02196)
             , ("UpperRightArrow"                 , Left 0x02197)
             , ("upsi"                            , Left 0x003C5)
             , ("Upsi"                            , Left 0x003D2)
             , ("Upsilon"                         , Left 0x003A5)
             , ("upsilon"                         , Left 0x003C5)
             , ("UpTeeArrow"                      , Left 0x021A5)
             , ("UpTee"                           , Left 0x022A5)
             , ("upuparrows"                      , Left 0x021C8)
             , ("urcorner"                        , Left 0x0231D)
             , ("urcorn"                          , Left 0x0231D)
             , ("urcrop"                          , Left 0x0230E)
             , ("Uring"                           , Left 0x0016E)
             , ("uring"                           , Left 0x0016F)
             , ("urtri"                           , Left 0x025F9)
             , ("Uscr"                            , Left 0x1D4B0)
             , ("uscr"                            , Left 0x1D4CA)
             , ("utdot"                           , Left 0x022F0)
             , ("Utilde"                          , Left 0x00168)
             , ("utilde"                          , Left 0x00169)
             , ("utrif"                           , Left 0x025B4)
             , ("utri"                            , Left 0x025B5)
             , ("uuarr"                           , Left 0x021C8)
             , ("Uuml"                            , Left 0x000DC)
             , ("uuml"                            , Left 0x000FC)
             , ("uwangle"                         , Left 0x029A7)
             , ("vangrt"                          , Left 0x0299C)
             , ("varepsilon"                      , Left 0x003B5)
             , ("varkappa"                        , Left 0x003F0)
             , ("varnothing"                      , Left 0x02205)
             , ("varphi"                          , Left 0x003C6)
             , ("varpi"                           , Left 0x003D6)
             , ("varpropto"                       , Left 0x0221D)
             , ("varrho"                          , Left 0x003F1)
             , ("varr"                            , Left 0x02195)
             , ("vArr"                            , Left 0x021D5)
             , ("varsigma"                        , Left 0x003C2)
             , ("vartheta"                        , Left 0x003D1)
             , ("vartriangleleft"                 , Left 0x022B2)
             , ("vartriangleright"                , Left 0x022B3)
             , ("vBar"                            , Left 0x02AE8)
             , ("Vbar"                            , Left 0x02AEB)
             , ("vBarv"                           , Left 0x02AE9)
             , ("Vcy"                             , Left 0x00412)
             , ("vcy"                             , Left 0x00432)
             , ("vdash"                           , Left 0x022A2)
             , ("vDash"                           , Left 0x022A8)
             , ("Vdash"                           , Left 0x022A9)
             , ("VDash"                           , Left 0x022AB)
             , ("Vdashl"                          , Left 0x02AE6)
             , ("veebar"                          , Left 0x022BB)
             , ("veeeq"                           , Left 0x0225A)
             , ("vee"                             , Left 0x02228)
             , ("Vee"                             , Left 0x022C1)
             , ("vellip"                          , Left 0x022EE)
             , ("verbar"                          , Left 0x0007C)
             , ("Verbar"                          , Left 0x02016)
             , ("VerticalBar"                     , Left 0x02223)
             , ("VerticalLine"                    , Left 0x0007C)
             , ("VerticalSeparator"               , Left 0x02758)
             , ("VerticalTilde"                   , Left 0x02240)
             , ("vert"                            , Left 0x0007C)
             , ("Vert"                            , Left 0x02016)
             , ("VeryThinSpace"                   , Left 0x0200A)
             , ("Vfr"                             , Left 0x1D519)
             , ("vfr"                             , Left 0x1D533)
             , ("vltri"                           , Left 0x022B2)
             , ("Vopf"                            , Left 0x1D54D)
             , ("vopf"                            , Left 0x1D567)
             , ("vprop"                           , Left 0x0221D)
             , ("vrtri"                           , Left 0x022B3)
             , ("Vscr"                            , Left 0x1D4B1)
             , ("vscr"                            , Left 0x1D4CB)
             , ("Vvdash"                          , Left 0x022AA)
             , ("vzigzag"                         , Left 0x0299A)
             , ("Wcirc"                           , Left 0x00174)
             , ("wcirc"                           , Left 0x00175)
             , ("wedbar"                          , Left 0x02A5F)
             , ("wedge"                           , Left 0x02227)
             , ("Wedge"                           , Left 0x022C0)
             , ("wedgeq"                          , Left 0x02259)
             , ("weierp"                          , Left 0x02118)
             , ("Wfr"                             , Left 0x1D51A)
             , ("wfr"                             , Left 0x1D534)
             , ("Wopf"                            , Left 0x1D54E)
             , ("wopf"                            , Left 0x1D568)
             , ("wp"                              , Left 0x02118)
             , ("wreath"                          , Left 0x02240)
             , ("wr"                              , Left 0x02240)
             , ("Wscr"                            , Left 0x1D4B2)
             , ("wscr"                            , Left 0x1D4CC)
             , ("xcap"                            , Left 0x022C2)
             , ("xcirc"                           , Left 0x025EF)
             , ("xcup"                            , Left 0x022C3)
             , ("xdtri"                           , Left 0x025BD)
             , ("Xfr"                             , Left 0x1D51B)
             , ("xfr"                             , Left 0x1D535)
             , ("xharr"                           , Left 0x027F7)
             , ("xhArr"                           , Left 0x027FA)
             , ("Xi"                              , Left 0x0039E)
             , ("xi"                              , Left 0x003BE)
             , ("xlarr"                           , Left 0x027F5)
             , ("xlArr"                           , Left 0x027F8)
             , ("xmap"                            , Left 0x027FC)
             , ("xnis"                            , Left 0x022FB)
             , ("xodot"                           , Left 0x02A00)
             , ("Xopf"                            , Left 0x1D54F)
             , ("xopf"                            , Left 0x1D569)
             , ("xoplus"                          , Left 0x02A01)
             , ("xotime"                          , Left 0x02A02)
             , ("xrarr"                           , Left 0x027F6)
             , ("xrArr"                           , Left 0x027F9)
             , ("Xscr"                            , Left 0x1D4B3)
             , ("xscr"                            , Left 0x1D4CD)
             , ("xsqcup"                          , Left 0x02A06)
             , ("xuplus"                          , Left 0x02A04)
             , ("xutri"                           , Left 0x025B3)
             , ("xvee"                            , Left 0x022C1)
             , ("xwedge"                          , Left 0x022C0)
             , ("Yacute"                          , Left 0x000DD)
             , ("yacute"                          , Left 0x000FD)
             , ("YAcy"                            , Left 0x0042F)
             , ("yacy"                            , Left 0x0044F)
             , ("Ycirc"                           , Left 0x00176)
             , ("ycirc"                           , Left 0x00177)
             , ("Ycy"                             , Left 0x0042B)
             , ("ycy"                             , Left 0x0044B)
             , ("yen"                             , Left 0x000A5)
             , ("Yfr"                             , Left 0x1D51C)
             , ("yfr"                             , Left 0x1D536)
             , ("YIcy"                            , Left 0x00407)
             , ("yicy"                            , Left 0x00457)
             , ("Yopf"                            , Left 0x1D550)
             , ("yopf"                            , Left 0x1D56A)
             , ("Yscr"                            , Left 0x1D4B4)
             , ("yscr"                            , Left 0x1D4CE)
             , ("YUcy"                            , Left 0x0042E)
             , ("yucy"                            , Left 0x0044E)
             , ("yuml"                            , Left 0x000FF)
             , ("Yuml"                            , Left 0x00178)
             , ("Zacute"                          , Left 0x00179)
             , ("zacute"                          , Left 0x0017A)
             , ("Zcaron"                          , Left 0x0017D)
             , ("zcaron"                          , Left 0x0017E)
             , ("Zcy"                             , Left 0x00417)
             , ("zcy"                             , Left 0x00437)
             , ("Zdot"                            , Left 0x0017B)
             , ("zdot"                            , Left 0x0017C)
             , ("zeetrf"                          , Left 0x02128)
             , ("ZeroWidthSpace"                  , Left 0x0200B)
             , ("zeta"                            , Left 0x003B6)
             , ("Zfr"                             , Left 0x02128)
             , ("zfr"                             , Left 0x1D537)
             , ("ZHcy"                            , Left 0x00416)
             , ("zhcy"                            , Left 0x00436)
             , ("zigrarr"                         , Left 0x021DD)
             , ("Zopf"                            , Left 0x02124)
             , ("zopf"                            , Left 0x1D56B)
             , ("Zscr"                            , Left 0x1D4B5)
             , ("zscr"                            , Left 0x1D4CF)
             , ("acE"                             , Right [ 0x0223E, 0x00333 ])
             , ("bnequiv"                         , Right [ 0x02261, 0x020E5 ])
             , ("bne"                             , Right [ 0x0003D, 0x020E5 ])
             , ("bsolhsub"                        , Right [ 0x0005C, 0x02282 ])
             , ("caps"                            , Right [ 0x02229, 0x0FE00 ])
             , ("cups"                            , Right [ 0x0222A, 0x0FE00 ])
             , ("gesl"                            , Right [ 0x022DB, 0x0FE00 ])
             , ("gvertneqq"                       , Right [ 0x02269, 0x0FE00 ])
             , ("gvnE"                            , Right [ 0x02269, 0x0FE00 ])
             , ("lates"                           , Right [ 0x02AAD, 0x0FE00 ])
             , ("lesg"                            , Right [ 0x022DA, 0x0FE00 ])
             , ("lvertneqq"                       , Right [ 0x02268, 0x0FE00 ])
             , ("lvnE"                            , Right [ 0x02268, 0x0FE00 ])
             , ("nang"                            , Right [ 0x02220, 0x020D2 ])
             , ("napE"                            , Right [ 0x02A70, 0x00338 ])
             , ("napid"                           , Right [ 0x0224B, 0x00338 ])
             , ("nbumpe"                          , Right [ 0x0224F, 0x00338 ])
             , ("nbump"                           , Right [ 0x0224E, 0x00338 ])
             , ("ncongdot"                        , Right [ 0x02A6D, 0x00338 ])
             , ("nedot"                           , Right [ 0x02250, 0x00338 ])
             , ("nesim"                           , Right [ 0x02242, 0x00338 ])
             , ("ngeqq"                           , Right [ 0x02267, 0x00338 ])
             , ("ngeqslant"                       , Right [ 0x02A7E, 0x00338 ])
             , ("ngE"                             , Right [ 0x02267, 0x00338 ])
             , ("nges"                            , Right [ 0x02A7E, 0x00338 ])
             , ("nGg"                             , Right [ 0x022D9, 0x00338 ])
             , ("nGt"                             , Right [ 0x0226B, 0x020D2 ])
             , ("nGtv"                            , Right [ 0x0226B, 0x00338 ])
             , ("nleqq"                           , Right [ 0x02266, 0x00338 ])
             , ("nleqslant"                       , Right [ 0x02A7D, 0x00338 ])
             , ("nlE"                             , Right [ 0x02266, 0x00338 ])
             , ("nles"                            , Right [ 0x02A7D, 0x00338 ])
             , ("nLl"                             , Right [ 0x022D8, 0x00338 ])
             , ("nLt"                             , Right [ 0x0226A, 0x020D2 ])
             , ("nLtv"                            , Right [ 0x0226A, 0x00338 ])
             , ("NotEqualTilde"                   , Right [ 0x02242, 0x00338 ])
             , ("NotGreaterFullEqual"             , Right [ 0x02266, 0x00338 ])
             , ("NotGreaterGreater"               , Right [ 0x0226B, 0x00338 ])
             , ("NotGreaterSlantEqual"            , Right [ 0x02A7E, 0x00338 ])
             , ("NotHumpDownHump"                 , Right [ 0x0224E, 0x00338 ])
             , ("NotHumpEqual"                    , Right [ 0x0224F, 0x00338 ])
             , ("notindot"                        , Right [ 0x022F5, 0x00338 ])
             , ("notinE"                          , Right [ 0x022F9, 0x00338 ])
             , ("NotLeftTriangleBar"              , Right [ 0x029CF, 0x00338 ])
             , ("NotLessLess"                     , Right [ 0x0226A, 0x00338 ])
             , ("NotLessSlantEqual"               , Right [ 0x02A7D, 0x00338 ])
             , ("NotNestedGreaterGreater"         , Right [ 0x02AA2, 0x00338 ])
             , ("NotNestedLessLess"               , Right [ 0x02AA1, 0x00338 ])
             , ("NotPrecedesEqual"                , Right [ 0x02AAF, 0x00338 ])
             , ("NotRightTriangleBar"             , Right [ 0x029D0, 0x00338 ])
             , ("NotSquareSubset"                 , Right [ 0x0228F, 0x00338 ])
             , ("NotSquareSuperset"               , Right [ 0x02290, 0x00338 ])
             , ("NotSubset"                       , Right [ 0x02282, 0x020D2 ])
             , ("NotSucceedsEqual"                , Right [ 0x02AB0, 0x00338 ])
             , ("NotSucceedsTilde"                , Right [ 0x0227F, 0x00338 ])
             , ("NotSuperset"                     , Right [ 0x02283, 0x020D2 ])
             , ("nparsl"                          , Right [ 0x02AFD, 0x020E5 ])
             , ("npart"                           , Right [ 0x02202, 0x00338 ])
             , ("npreceq"                         , Right [ 0x02AAF, 0x00338 ])
             , ("npre"                            , Right [ 0x02AAF, 0x00338 ])
             , ("nrarrc"                          , Right [ 0x02933, 0x00338 ])
             , ("nrarrw"                          , Right [ 0x0219D, 0x00338 ])
             , ("nsce"                            , Right [ 0x02AB0, 0x00338 ])
             , ("nsubE"                           , Right [ 0x02AC5, 0x00338 ])
             , ("nsubseteqq"                      , Right [ 0x02AC5, 0x00338 ])
             , ("nsubset"                         , Right [ 0x02282, 0x020D2 ])
             , ("nsucceq"                         , Right [ 0x02AB0, 0x00338 ])
             , ("nsupE"                           , Right [ 0x02AC6, 0x00338 ])
             , ("nsupseteqq"                      , Right [ 0x02AC6, 0x00338 ])
             , ("nsupset"                         , Right [ 0x02283, 0x020D2 ])
             , ("nvap"                            , Right [ 0x0224D, 0x020D2 ])
             , ("nvge"                            , Right [ 0x02265, 0x020D2 ])
             , ("nvgt"                            , Right [ 0x0003E, 0x020D2 ])
             , ("nvle"                            , Right [ 0x02264, 0x020D2 ])
             , ("nvltrie"                         , Right [ 0x022B4, 0x020D2 ])
             , ("nvlt"                            , Right [ 0x0003C, 0x020D2 ])
             , ("nvrtrie"                         , Right [ 0x022B5, 0x020D2 ])
             , ("nvsim"                           , Right [ 0x0223C, 0x020D2 ])
             , ("smtes"                           , Right [ 0x02AAC, 0x0FE00 ])
             , ("sqcaps"                          , Right [ 0x02293, 0x0FE00 ])
             , ("sqcups"                          , Right [ 0x02294, 0x0FE00 ])
             , ("suphsol"                         , Right [ 0x02283, 0x0002F ])
             , ("ThickSpace"                      , Right [ 0x02009, 0x0200A, 0x0200A ])
             , ("varsubsetneqq"                   , Right [ 0x02ACB, 0x0FE00 ])
             , ("varsubsetneq"                    , Right [ 0x0228A, 0x0FE00 ])
             , ("varsupsetneqq"                   , Right [ 0x02ACC, 0x0FE00 ])
             , ("varsupsetneq"                    , Right [ 0x0228B, 0x0FE00 ])
             , ("vnsub"                           , Right [ 0x02282, 0x020D2 ])
             , ("vnsup"                           , Right [ 0x02283, 0x020D2 ])
             , ("vsubne"                          , Right [ 0x0228A, 0x0FE00 ])
             , ("vsubnE"                          , Right [ 0x02ACB, 0x0FE00 ])
             , ("vsupne"                          , Right [ 0x0228B, 0x0FE00 ])
             , ("vsupnE"                          , Right [ 0x02ACC, 0x0FE00 ])
             ]
               
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
