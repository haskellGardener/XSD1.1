{-# Language ExistentialQuantification, MultiParamTypeClasses, TupleSections
  , FlexibleInstances, GeneralizedNewtypeDeriving, NegativeLiterals, MultiWayIf #-}
{-| Time-stamp: <2018-07-08 16:39:11 CDT>

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

-- import Data.Either (isLeft)

-- Qualified Imports

import qualified Data.Char                     as C
import qualified Data.CharSet                  as U
import qualified Data.CharSet.Unicode.Category as U
import qualified Data.List                     as L
import qualified Data.Map.Strict               as M
import qualified Data.Text                     as T
import qualified Numeric                       as N

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

-- -----------------------------------------------------------------------------------------------------------------------------------------------------
-- TransRegex stanzas

class TransRegex a where             -- TransRegex ➙ transform
  scribeBR :: Bool -> a -> Text

  canonR :: a -> Text                -- canonR     ➙ canonical text
  canonR a  = scribeBR True a

  scribeR :: a -> Text               -- scribeR    ➙ write text
  scribeR a = scribeBR False a

  selegoR :: a -> Parser (a,Text)    -- selegoR    ➙ create parser from TransRexgex a (selego selector)
  -- selegoR _ = error "NRPT" -- pure (RE [], T.empty)

pairChar :: (TransRegex t) => t -> Char -> Parser (t, Text)
pairChar pp c = pure (pp,T.singleton c)

pairSnd :: (TransRegex t, TransRegex r) => t -> (r,Text) -> Parser (t, Text)
pairSnd pp (_,text) = pure (pp,text)

pairSnds :: (TransRegex t, TransRegex r) => t -> [] (r,Text) -> Parser (t, Text)
pairSnds pp [] = pure (pp,T.empty)
pairSnds pp xs = pure . (pp,) . T.concat $ map snd xs

-- -----------------------------------------------------------------------------------------------------------------------------------------------------
-- Instance stanzas

instance TransRegex RE
  where scribeBR b (RE branches) = T.intercalate "|" $ map (scribeBR b) branches
        
        selegoR pp@(RE branches) = choice (map selegoR branches) >>= pairSnd pp

instance TransRegex Branch
  where scribeBR b (Branch pieces) = T.concat $ map (scribeBR b) pieces
        selegoR pp@(Branch pieces) = mapM selegoR pieces >>= pairSnds pp

instance TransRegex Piece
  where scribeBR b (Piece atm mquant) = T.append (scribeBR b atm) $ maybe T.empty (scribeBR b) mquant

        selegoR pp@(Piece atm  Nothing)                      =            selegoR atm  >>= pairSnd  pp
        selegoR pp@(Piece atm (Just QuantifierMaybeOne))     = zeroMax 1 (selegoR atm) >>= pairSnds pp
        selegoR pp@(Piece atm (Just QuantifierMaybeMany))    = many'     (selegoR atm) >>= pairSnds pp
        selegoR pp@(Piece atm (Just QuantifierMany))         = many1     (selegoR atm) >>= pairSnds pp
        selegoR pp@(Piece atm (Just (QuantifierQuantity q))) = helper q  (selegoR atm) >>= pairSnds pp
          where helper (QuantRange qel qer) = minMax (fromQE qel) (fromQE qer)
                helper (QuantMin qe)        = minMax (fromQE qe) 10000  -- upper limit breaks spec.
                helper (QuantExactQ qe)     = count (fromQE qe)
                                            
                fromQE :: QuantExact -> Int
                fromQE (QuantExact i) = i
          
instance TransRegex Atom
  where scribeBR b (AtomNormal    atm) = scribeBR b atm
        scribeBR b (AtomCharClass atm) = scribeBR b atm
        scribeBR b (AtomRE        atm) = T.concat ["(", scribeBR b atm, ")"]

        selegoR pp@(AtomNormal    nc) = selegoR nc >>= pairSnd pp
        selegoR pp@(AtomCharClass cc) = selegoR cc >>= pairSnd pp
        selegoR pp@(AtomRE        er) = selegoR er >>= pairSnd pp

instance TransRegex Quantifier
  where scribeBR _b QuantifierMaybeOne        = T.singleton '?'
        scribeBR _b QuantifierMaybeMany       = T.singleton '*'
        scribeBR _b QuantifierMany            = T.singleton '+'
        scribeBR b (QuantifierQuantity quant) = T.concat ["{", (scribeBR b quant), "}"]

        selegoR _ = error "Not supported irregular instance."

instance TransRegex Quantity
  where scribeBR b (QuantRange  qel qer) = T.concat [scribeBR b qel, ",", scribeBR b qer]
        scribeBR b (QuantMin    qe) = T.append (scribeBR b qe) ","
        scribeBR b (QuantExactQ qe) = scribeBR b qe

        selegoR _ = error "Not supported irregular instance."

instance TransRegex QuantExact
  where scribeBR _b (QuantExact i) = tShow i

        selegoR _ = error "Not supported irregular instance."
                    
instance TransRegex NormalChar
  where scribeBR False (NormalChar c) = T.singleton c
        scribeBR True  (NormalChar c) =
          case M.lookup (Left $ C.ord c) eNumEntityMap of
            Nothing -> if | C.ord c <= 0x1FFF -> T.singleton c
                          | otherwise ->
                              let stringA = (N.showHex $ C.ord c) ""
                                  stringB = if | L.length stringA < 6 -> replicate (6 - L.length stringA) '0' ++ stringA
                                               | otherwise            -> stringA
                              in T.concat ["&#x", T.toUpper $ T.pack stringB, ";"]
            Just t -> T.concat ["&", t, ";"]

        selegoR nc@(NormalChar c) = char c >>= pairChar nc

instance TransRegex CharClass
  where scribeBR b (CharClassSingle cc) = scribeBR b cc
        scribeBR b (CharClassEscC   cc) = scribeBR b cc
        scribeBR b (CharClassExprC  cc) = scribeBR b cc
        scribeBR b (CharClassWild   cc) = scribeBR b cc

        selegoR pp@(CharClassSingle cc) = selegoR cc >>= pairSnd pp
        selegoR pp@(CharClassEscC   cc) = selegoR cc >>= pairSnd pp
        selegoR pp@(CharClassExprC  cc) = selegoR cc >>= pairSnd pp
        selegoR pp@(CharClassWild   cc) = selegoR cc >>= pairSnd pp

instance TransRegex CharClassExpr
  where scribeBR b (CharClassExpr cg) = T.concat ["[", scribeBR b cg, "]"]

        selegoR pp@(CharClassExpr cg) = selegoR cg >>= pairSnd pp

instance TransRegex CharGroup
  where scribeBR b (CharGroup (Left  pcg) mce) = T.append (scribeBR b pcg) $ maybe T.empty (T.cons '-' . scribeBR b) mce
        scribeBR b (CharGroup (Right ncg) mce) = T.append (scribeBR b ncg) $ maybe T.empty (T.cons '-' . scribeBR b) mce

        selegoR pp@(CharGroup (Left  pcg) Nothing)    =                     selegoR pcg >>= pairSnd pp
        selegoR pp@(CharGroup (Right ncg) Nothing)    =                     selegoR ncg >>= pairSnd pp
        selegoR pp@(CharGroup (Left  pcg) (Just cce)) = notMatchChar cce >> selegoR pcg >>= pairSnd pp
        selegoR pp@(CharGroup (Right ncg) (Just cce)) = notMatchChar cce >> selegoR ncg >>= pairSnd pp

notMatchChar :: TransRegex tr => tr -> Parser ()
notMatchChar pos = notMatchCharParse $ selegoR pos

instance TransRegex NegCharGroup
  where scribeBR b (NegCharGroup pcg) = T.cons '^' $ scribeBR b pcg

        selegoR pp@(NegCharGroup pcg) = notMatchChar pcg >> anyChar >>= pairChar pp

instance TransRegex PosCharGroup
  where scribeBR b (PosCharGroup cgp) = T.concat $ map (scribeBR b) cgp

        selegoR pp@(PosCharGroup cgp) = choice (map selegoR cgp) >>= pairSnd pp

instance TransRegex CharGroupPart
  where scribeBR b (CharGroupPartSingle   cgp) = scribeBR b cgp
        scribeBR b (CharGroupPartRange    cgp) = scribeBR b cgp
        scribeBR b (CharGroupPartClassEsc cgp) = scribeBR b cgp

        selegoR pp@(CharGroupPartSingle   cgp) = selegoR cgp >>= pairSnd pp
        selegoR pp@(CharGroupPartRange    cgp) = selegoR cgp >>= pairSnd pp
        selegoR pp@(CharGroupPartClassEsc cgp) = selegoR cgp >>= pairSnd pp

instance TransRegex CharClassEsc
  where scribeBR b (CharClassEscMultiCharEsc cesc) = scribeBR b cesc
        scribeBR b (CharClassEscCatEsc       cesc) = scribeBR b cesc
        scribeBR b (CharClassEscComplEsc     cesc) = scribeBR b cesc

        selegoR pp@(CharClassEscMultiCharEsc cesc) = selegoR cesc >>= pairSnd pp
        selegoR pp@(CharClassEscCatEsc       cesc) = selegoR cesc >>= pairSnd pp
        selegoR pp@(CharClassEscComplEsc     cesc) = selegoR cesc >>= pairSnd pp

instance TransRegex CharRange
  where scribeBR b (CharRange l r) = T.concat [scribeBR b l, "-", scribeBR b r]
        selegoR pp@(CharRange l r) = satisfy (\c -> c >= retrieveSingleChar l && c <= retrieveSingleChar r ) >>= pairChar pp
          where retrieveSingleChar :: SingleChar -> Char
                retrieveSingleChar (SingleChar (Left  (SingleCharEsc   c))) = c
                retrieveSingleChar (SingleChar (Right (SingleCharNoEsc c))) = c

instance TransRegex SingleChar
  where scribeBR b (SingleChar (Left  se )) = scribeBR b se
        scribeBR b (SingleChar (Right sne)) = scribeBR b sne

        selegoR pp@(SingleChar (Left  se )) = selegoR se  >>= pairSnd pp
        selegoR pp@(SingleChar (Right sne)) = selegoR sne >>= pairSnd pp

instance TransRegex SingleCharNoEsc
  where scribeBR False (SingleCharNoEsc c) = T.singleton c
        scribeBR True  (SingleCharNoEsc c) = canonR $ NormalChar c

        selegoR scne@(SingleCharNoEsc c) = char c >>= pairChar scne

instance TransRegex SingleCharEsc
  where scribeBR _b (SingleCharEsc c) = T.cons '\\' $ T.singleton c

        selegoR sce@(SingleCharEsc c) = char c >>= pairChar sce

instance TransRegex CatEsc
  where scribeBR b (CatEsc cp) = T.concat ["\\p{", scribeBR b cp, "}"]

        selegoR pp@(CatEsc cp) = selegoR cp >>= pairSnd pp

instance TransRegex ComplEsc
  where scribeBR b (ComplEsc cp) = T.concat ["\\P{", scribeBR b cp, "}"]

        selegoR pp@(ComplEsc cp) = notMatchChar cp >> anyChar >>= pairChar pp

instance TransRegex CharProp
  where scribeBR b (CharProp (Left  i)) = scribeBR b i
        scribeBR b (CharProp (Right i)) = scribeBR b i

        selegoR pp@(CharProp (Left  ic)) = selegoR ic >>= pairSnd pp
        selegoR pp@(CharProp (Right ib)) = selegoR ib >>= pairSnd pp

instance TransRegex IsCategory
  where scribeBR b (LettersCat     cat) = scribeBR b cat
        scribeBR b (MarksCat       cat) = scribeBR b cat
        scribeBR b (NumbersCat     cat) = scribeBR b cat
        scribeBR b (PunctuationCat cat) = scribeBR b cat
        scribeBR b (SeparatorsCat  cat) = scribeBR b cat
        scribeBR b (SymbolsCat     cat) = scribeBR b cat
        scribeBR b (OthersCat      cat) = scribeBR b cat

        selegoR pp@(LettersCat     cat) = selegoR cat >>= pairSnd pp
        selegoR pp@(MarksCat       cat) = selegoR cat >>= pairSnd pp
        selegoR pp@(NumbersCat     cat) = selegoR cat >>= pairSnd pp
        selegoR pp@(PunctuationCat cat) = selegoR cat >>= pairSnd pp
        selegoR pp@(SeparatorsCat  cat) = selegoR cat >>= pairSnd pp
        selegoR pp@(SymbolsCat     cat) = selegoR cat >>= pairSnd pp
        selegoR pp@(OthersCat      cat) = selegoR cat >>= pairSnd pp

-- data Trans = forall s. TransRegex s => TR s

-- trList :: [] (Trans, Text)
-- trList = [(TR L, ""), (TR (NumbersCat Nd), ""), (TR UNRECOGNIZED_BLOCK, "")]

instance TransRegex Letters
  where scribeBR _b = tShow
        selegoR L  = satisfy (\c -> U.member c U.letter          ) >>= pairChar L  -- | All Letters
        selegoR Lu = satisfy (\c -> U.member c U.uppercaseLetter ) >>= pairChar Lu -- | uppercase
        selegoR Ll = satisfy (\c -> U.member c U.lowercaseLetter ) >>= pairChar Ll -- | lowercase
        selegoR Lt = satisfy (\c -> U.member c U.titlecaseLetter ) >>= pairChar Lt -- | titlecase
        selegoR Lm = satisfy (\c -> U.member c U.modifierLetter  ) >>= pairChar Lm -- | modifier
        selegoR Lo = satisfy (\c -> U.member c U.otherLetter     ) >>= pairChar Lo -- | other

instance TransRegex Marks
  where scribeBR _b = tShow

        selegoR M  = satisfy (\c -> U.member c U.mark                 ) >>= pairChar M  -- | All Marks
        selegoR Mn = satisfy (\c -> U.member c U.nonSpacingMark       ) >>= pairChar Mn -- | nonspacing
        selegoR Mc = satisfy (\c -> U.member c U.spacingCombiningMark ) >>= pairChar Mc -- | spacing combining
        selegoR Me = satisfy (\c -> U.member c U.enclosingMark        ) >>= pairChar Me -- | enclosing

instance TransRegex Numbers
  where scribeBR _b = tShow

        selegoR N  = satisfy (\c -> U.member c U.number        ) >>= pairChar N  -- | All Numbers
        selegoR Nd = satisfy (\c -> U.member c U.decimalNumber ) >>= pairChar Nd -- | decimal digit
        selegoR Nl = satisfy (\c -> U.member c U.letterNumber  ) >>= pairChar Nl -- | letter
        selegoR No = satisfy (\c -> U.member c U.otherNumber   ) >>= pairChar No -- | other

instance TransRegex Punctuation
  where scribeBR _b = tShow

        selegoR P  = satisfy (\c -> U.member c U.punctuation          ) >>= pairChar P  -- | All Punctuation
        selegoR Pc = satisfy (\c -> U.member c U.connectorPunctuation ) >>= pairChar Pc -- | connector
        selegoR Pd = satisfy (\c -> U.member c U.dashPunctuation      ) >>= pairChar Pd -- | dash
        selegoR Ps = satisfy (\c -> U.member c U.openPunctuation      ) >>= pairChar Ps -- | open
        selegoR Pe = satisfy (\c -> U.member c U.closePunctuation     ) >>= pairChar Pe -- | close
        selegoR Pi = satisfy (\c -> U.member c U.initialQuote         ) >>= pairChar Pi -- | initial quote
        selegoR Pf = satisfy (\c -> U.member c U.finalQuote           ) >>= pairChar Pf -- | final quote
        selegoR Po = satisfy (\c -> U.member c U.otherPunctuation     ) >>= pairChar Po -- | other

instance TransRegex Separators
  where scribeBR _b = tShow

        selegoR Z  = satisfy (\c -> U.member c U.separator          ) >>= pairChar Z  -- | All Separators
        selegoR Zs = satisfy (\c -> U.member c U.space              ) >>= pairChar Zs -- | space
        selegoR Zl = satisfy (\c -> U.member c U.lineSeparator      ) >>= pairChar Zl -- | line
        selegoR Zp = satisfy (\c -> U.member c U.paragraphSeparator ) >>= pairChar Zp -- | paragraph

instance TransRegex Symbols
  where scribeBR _b = tShow

        selegoR S  = satisfy (\c -> U.member c U.symbol         ) >>= pairChar S  -- | All Symbols
        selegoR Sm = satisfy (\c -> U.member c U.mathSymbol     ) >>= pairChar Sm -- | math
        selegoR Sc = satisfy (\c -> U.member c U.currencySymbol ) >>= pairChar Sc -- | currency
        selegoR Sk = satisfy (\c -> U.member c U.modifierSymbol ) >>= pairChar Sk -- | modifier
        selegoR So = satisfy (\c -> U.member c U.otherSymbol    ) >>= pairChar So -- | other

instance TransRegex Others
  where scribeBR _b = tShow
        selegoR C  = satisfy (\c -> U.member c U.other       ) >>= pairChar C  -- | All Others
        selegoR Cc = satisfy (\c -> U.member c U.control     ) >>= pairChar Cc -- | control
        selegoR Cf = satisfy (\c -> U.member c U.format      ) >>= pairChar Cf -- | format
        selegoR Co = satisfy (\c -> U.member c U.privateUse  ) >>= pairChar Co -- | private use
        selegoR Cn = satisfy (\c -> U.member c U.notAssigned ) >>= pairChar Cn -- | not assigned

instance TransRegex IsBlock
  where scribeBR False (IsBlock _ t) = t
        scribeBR True  (IsBlock u _) = T.append "Is" $ scribeBR True u

        selegoR pp@(IsBlock blockname _) = selegoR blockname >>= pairSnd pp

instance TransRegex MultiCharEsc
  where scribeBR _b (MultiCharEsc c) = T.cons '\\' $ T.singleton c

        selegoR pp@(MultiCharEsc 's') = satisfy (inClass    " \t\n\r")                                                 >>= pairChar pp
        selegoR pp@(MultiCharEsc 'S') = satisfy (notInClass " \t\n\r")                                                 >>= pairChar pp
        selegoR pp@(MultiCharEsc 'i') =                   nameStartCharParser                                          >>= pairChar pp
        selegoR pp@(MultiCharEsc 'I') = notMatchCharParse nameStartCharParser >> anyChar                               >>= pairChar pp
        selegoR pp@(MultiCharEsc 'c') =                   nameCharParser                                               >>= pairChar pp
        selegoR pp@(MultiCharEsc 'C') = notMatchCharParse nameCharParser      >> anyChar                               >>= pairChar pp
        selegoR pp@(MultiCharEsc 'd') = satisfy (\c ->       U.member c U.decimalNumber)                               >>= pairChar pp
        selegoR pp@(MultiCharEsc 'D') = satisfy (\c -> not $ U.member c U.decimalNumber)                               >>= pairChar pp
        selegoR pp@(MultiCharEsc 'w') = satisfy (\c -> not $ L.any (U.member c) [U.punctuation, U.separator, U.other]) >>= pairChar pp
        selegoR pp@(MultiCharEsc 'W') = satisfy (\c ->       L.any (U.member c) [U.punctuation, U.separator, U.other]) >>= pairChar pp
        selegoR (MultiCharEsc c) = fail $ "Faulty MultiCharEsc '" ++ [c,'\'']                                                                        -- ⛞

instance TransRegex WildcardEsc
  where scribeBR _b WildcardEsc = "."
        selegoR WildcardEsc = satisfy (notInClass "\n\r") >>= pairChar WildcardEsc

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

        selegoR pp = matchUnicodeBlockName pp >>= pairChar pp

-- ---------------------------------------------------------------------------------------------------------------------------------------------------
-- Normalize text

-- | normalize takes input text and produces (Normalized text, [] ( length of each Normalized char in the text input
--                                                                , position of each Normalized char in the input
--                                                                )
--                                           )
--
-- Normalized text is free from character entity irritants.
normalize :: Parser (Text, [] (Int, Int))
normalize = do nlPairs <- many1 (match nChar)
               let res :: [] (Char, Int, Int)
                   res = if | null nlPairs -> []                                      -- Prevent tail from throwing an exception.
                            | otherwise -> L.tail $ L.scanl scanF zeroElement nlPairs -- eliminate zero element from the result.
               pure ( T.pack $ map (\(c, _, _) -> c) res
                    , map (\(_, l, pos) -> (l, pos)) res
                    )
  where scanF :: (Char, Int, Int) -> (Text, Char) -> (Char, Int, Int)
        scanF (_, priorMatchedLength, priorPosition) (matchedText, c) = (c, T.length matchedText, priorPosition + priorMatchedLength)

        zeroElement :: (Char, Int, Int)
        zeroElement = (' ', 0, 0) -- ' ' is ignored by scanF.

        nChar :: Parser Char
        nChar = choice [ entityChar
                       , entityHexChar
                       , entityNumChar
                       , anyChar
                       ]

deNormalize :: Text -> (Int, Int) -> Text
deNormalize t (l,p) = T.take l $ T.drop p t

-- ---------------------------------------------------------------------------------------------------------------------------------------------------
-- Regex type and grammar stanzas

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
                | QuantifierQuantity Quantity
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
                                pure $ QuantifierQuantity q

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
                cs <- minMax 1 maxEntityName . satisfy $ inClass "a-zA-Z." -- '.' is found in bold greek letters.
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
unicodeBlockNameRanges = fmap (\(nomen, UBR s f) -> (nomen, satisfy $ inClass [C.chr s, '-', C.chr f])) ubnns

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

data UnicodeBlockRange = UBR Int Int
                         deriving (Show, Ord, Eq)

ubnns :: [ ( UnicodeBlockName                               , UnicodeBlockRange     )]
ubnns =  [ ( UNRECOGNIZED_BLOCK                             , UBR 0x0000   0x10FFFF )
         , ( BASIC_LATIN                                    , UBR 0x0000   0x007F   )
         , ( LATIN_1_SUPPLEMENT                             , UBR 0x0080   0x00FF   )
         , ( LATIN_EXTENDED_A                               , UBR 0x0100   0x017F   )
         , ( LATIN_EXTENDED_B                               , UBR 0x0180   0x024F   )
         , ( IPA_EXTENSIONS                                 , UBR 0x0250   0x02AF   )
         , ( SPACING_MODIFIER_LETTERS                       , UBR 0x02B0   0x02FF   )
         , ( COMBINING_DIACRITICAL_MARKS                    , UBR 0x0300   0x036F   )
         , ( GREEK_AND_COPTIC                               , UBR 0x0370   0x03FF   )
         , ( CYRILLIC                                       , UBR 0x0400   0x04FF   )
         , ( CYRILLIC_SUPPLEMENT                            , UBR 0x0500   0x052F   )
         , ( ARMENIAN                                       , UBR 0x0530   0x058F   )
         , ( HEBREW                                         , UBR 0x0590   0x05FF   )
         , ( ARABIC                                         , UBR 0x0600   0x06FF   )
         , ( SYRIAC                                         , UBR 0x0700   0x074F   )
         , ( ARABIC_SUPPLEMENT                              , UBR 0x0750   0x077F   )
         , ( THAANA                                         , UBR 0x0780   0x07BF   )
         , ( NKO                                            , UBR 0x07C0   0x07FF   )
         , ( SAMARITAN                                      , UBR 0x0800   0x083F   )
         , ( MANDAIC                                        , UBR 0x0840   0x085F   )
         , ( DEVANAGARI                                     , UBR 0x0900   0x097F   )
         , ( BENGALI                                        , UBR 0x0980   0x09FF   )
         , ( GURMUKHI                                       , UBR 0x0A00   0x0A7F   )
         , ( GUJARATI                                       , UBR 0x0A80   0x0AFF   )
         , ( ORIYA                                          , UBR 0x0B00   0x0B7F   )
         , ( TAMIL                                          , UBR 0x0B80   0x0BFF   )
         , ( TELUGU                                         , UBR 0x0C00   0x0C7F   )
         , ( KANNADA                                        , UBR 0x0C80   0x0CFF   )
         , ( MALAYALAM                                      , UBR 0x0D00   0x0D7F   )
         , ( SINHALA                                        , UBR 0x0D80   0x0DFF   )
         , ( THAI                                           , UBR 0x0E00   0x0E7F   )
         , ( LAO                                            , UBR 0x0E80   0x0EFF   )
         , ( TIBETAN                                        , UBR 0x0F00   0x0FFF   )
         , ( MYANMAR                                        , UBR 0x1000   0x109F   )
         , ( GEORGIAN                                       , UBR 0x10A0   0x10FF   )
         , ( HANGUL_JAMO                                    , UBR 0x1100   0x11FF   )
         , ( ETHIOPIC                                       , UBR 0x1200   0x137F   )
         , ( ETHIOPIC_SUPPLEMENT                            , UBR 0x1380   0x139F   )
         , ( CHEROKEE                                       , UBR 0x13A0   0x13FF   )
         , ( UNIFIED_CANADIAN_ABORIGINAL_SYLLABICS          , UBR 0x1400   0x167F   )
         , ( OGHAM                                          , UBR 0x1680   0x169F   )
         , ( RUNIC                                          , UBR 0x16A0   0x16FF   )
         , ( TAGALOG                                        , UBR 0x1700   0x171F   )
         , ( HANUNOO                                        , UBR 0x1720   0x173F   )
         , ( BUHID                                          , UBR 0x1740   0x175F   )
         , ( TAGBANWA                                       , UBR 0x1760   0x177F   )
         , ( KHMER                                          , UBR 0x1780   0x17FF   )
         , ( MONGOLIAN                                      , UBR 0x1800   0x18AF   )
         , ( UNIFIED_CANADIAN_ABORIGINAL_SYLLABICS_EXTENDED , UBR 0x18B0   0x18FF   )
         , ( LIMBU                                          , UBR 0x1900   0x194F   )
         , ( TAI_LE                                         , UBR 0x1950   0x197F   )
         , ( NEW_TAI_LUE                                    , UBR 0x1980   0x19DF   )
         , ( KHMER_SYMBOLS                                  , UBR 0x19E0   0x19FF   )
         , ( BUGINESE                                       , UBR 0x1A00   0x1A1F   )
         , ( TAI_THAM                                       , UBR 0x1A20   0x1AAF   )
         , ( BALINESE                                       , UBR 0x1B00   0x1B7F   )
         , ( SUNDANESE                                      , UBR 0x1B80   0x1BBF   )
         , ( BATAK                                          , UBR 0x1BC0   0x1BFF   )
         , ( LEPCHA                                         , UBR 0x1C00   0x1C4F   )
         , ( OL_CHIKI                                       , UBR 0x1C50   0x1C7F   )
         , ( VEDIC_EXTENSIONS                               , UBR 0x1CD0   0x1CFF   )
         , ( PHONETIC_EXTENSIONS                            , UBR 0x1D00   0x1D7F   )
         , ( PHONETIC_EXTENSIONS_SUPPLEMENT                 , UBR 0x1D80   0x1DBF   )
         , ( COMBINING_DIACRITICAL_MARKS_SUPPLEMENT         , UBR 0x1DC0   0x1DFF   )
         , ( LATIN_EXTENDED_ADDITIONAL                      , UBR 0x1E00   0x1EFF   )
         , ( GREEK_EXTENDED                                 , UBR 0x1F00   0x1FFF   )
         , ( GENERAL_PUNCTUATION                            , UBR 0x2000   0x206F   )
         , ( SUPERSCRIPTS_AND_SUBSCRIPTS                    , UBR 0x2070   0x209F   )
         , ( CURRENCY_SYMBOLS                               , UBR 0x20A0   0x20CF   )
         , ( COMBINING_DIACRITICAL_MARKS_FOR_SYMBOLS        , UBR 0x20D0   0x20FF   )
         , ( LETTERLIKE_SYMBOLS                             , UBR 0x2100   0x214F   )
         , ( NUMBER_FORMS                                   , UBR 0x2150   0x218F   )
         , ( ARROWS                                         , UBR 0x2190   0x21FF   )
         , ( MATHEMATICAL_OPERATORS                         , UBR 0x2200   0x22FF   )
         , ( MISCELLANEOUS_TECHNICAL                        , UBR 0x2300   0x23FF   )
         , ( CONTROL_PICTURES                               , UBR 0x2400   0x243F   )
         , ( OPTICAL_CHARACTER_RECOGNITION                  , UBR 0x2440   0x245F   )
         , ( ENCLOSED_ALPHANUMERICS                         , UBR 0x2460   0x24FF   )
         , ( BOX_DRAWING                                    , UBR 0x2500   0x257F   )
         , ( BLOCK_ELEMENTS                                 , UBR 0x2580   0x259F   )
         , ( GEOMETRIC_SHAPES                               , UBR 0x25A0   0x25FF   )
         , ( MISCELLANEOUS_SYMBOLS                          , UBR 0x2600   0x26FF   )
         , ( DINGBATS                                       , UBR 0x2700   0x27BF   )
         , ( MISCELLANEOUS_MATHEMATICAL_SYMBOLS_A           , UBR 0x27C0   0x27EF   )
         , ( SUPPLEMENTAL_ARROWS_A                          , UBR 0x27F0   0x27FF   )
         , ( BRAILLE_PATTERNS                               , UBR 0x2800   0x28FF   )
         , ( SUPPLEMENTAL_ARROWS_B                          , UBR 0x2900   0x297F   )
         , ( MISCELLANEOUS_MATHEMATICAL_SYMBOLS_B           , UBR 0x2980   0x29FF   )
         , ( SUPPLEMENTAL_MATHEMATICAL_OPERATORS            , UBR 0x2A00   0x2AFF   )
         , ( MISCELLANEOUS_SYMBOLS_AND_ARROWS               , UBR 0x2B00   0x2BFF   )
         , ( GLAGOLITIC                                     , UBR 0x2C00   0x2C5F   )
         , ( LATIN_EXTENDED_C                               , UBR 0x2C60   0x2C7F   )
         , ( COPTIC                                         , UBR 0x2C80   0x2CFF   )
         , ( GEORGIAN_SUPPLEMENT                            , UBR 0x2D00   0x2D2F   )
         , ( TIFINAGH                                       , UBR 0x2D30   0x2D7F   )
         , ( ETHIOPIC_EXTENDED                              , UBR 0x2D80   0x2DDF   )
         , ( CYRILLIC_EXTENDED_A                            , UBR 0x2DE0   0x2DFF   )
         , ( SUPPLEMENTAL_PUNCTUATION                       , UBR 0x2E00   0x2E7F   )
         , ( CJK_RADICALS_SUPPLEMENT                        , UBR 0x2E80   0x2EFF   )
         , ( KANGXI_RADICALS                                , UBR 0x2F00   0x2FDF   )
         , ( IDEOGRAPHIC_DESCRIPTION_CHARACTERS             , UBR 0x2FF0   0x2FFF   )
         , ( CJK_SYMBOLS_AND_PUNCTUATION                    , UBR 0x3000   0x303F   )
         , ( HIRAGANA                                       , UBR 0x3040   0x309F   )
         , ( KATAKANA                                       , UBR 0x30A0   0x30FF   )
         , ( BOPOMOFO                                       , UBR 0x3100   0x312F   )
         , ( HANGUL_COMPATIBILITY_JAMO                      , UBR 0x3130   0x318F   )
         , ( KANBUN                                         , UBR 0x3190   0x319F   )
         , ( BOPOMOFO_EXTENDED                              , UBR 0x31A0   0x31BF   )
         , ( CJK_STROKES                                    , UBR 0x31C0   0x31EF   )
         , ( KATAKANA_PHONETIC_EXTENSIONS                   , UBR 0x31F0   0x31FF   )
         , ( ENCLOSED_CJK_LETTERS_AND_MONTHS                , UBR 0x3200   0x32FF   )
         , ( CJK_COMPATIBILITY                              , UBR 0x3300   0x33FF   )
         , ( CJK_UNIFIED_IDEOGRAPHS_EXTENSION_A             , UBR 0x3400   0x4DBF   )
         , ( YIJING_HEXAGRAM_SYMBOLS                        , UBR 0x4DC0   0x4DFF   )
         , ( CJK_UNIFIED_IDEOGRAPHS                         , UBR 0x4E00   0x9FFF   )
         , ( YI_SYLLABLES                                   , UBR 0xA000   0xA48F   )
         , ( YI_RADICALS                                    , UBR 0xA490   0xA4CF   )
         , ( LISU                                           , UBR 0xA4D0   0xA4FF   )
         , ( VAI                                            , UBR 0xA500   0xA63F   )
         , ( CYRILLIC_EXTENDED_B                            , UBR 0xA640   0xA69F   )
         , ( BAMUM                                          , UBR 0xA6A0   0xA6FF   )
         , ( MODIFIER_TONE_LETTERS                          , UBR 0xA700   0xA71F   )
         , ( LATIN_EXTENDED_D                               , UBR 0xA720   0xA7FF   )
         , ( SYLOTI_NAGRI                                   , UBR 0xA800   0xA82F   )
         , ( COMMON_INDIC_NUMBER_FORMS                      , UBR 0xA830   0xA83F   )
         , ( PHAGS_PA                                       , UBR 0xA840   0xA87F   )
         , ( SAURASHTRA                                     , UBR 0xA880   0xA8DF   )
         , ( DEVANAGARI_EXTENDED                            , UBR 0xA8E0   0xA8FF   )
         , ( KAYAH_LI                                       , UBR 0xA900   0xA92F   )
         , ( REJANG                                         , UBR 0xA930   0xA95F   )
         , ( HANGUL_JAMO_EXTENDED_A                         , UBR 0xA960   0xA97F   )
         , ( JAVANESE                                       , UBR 0xA980   0xA9DF   )
         , ( CHAM                                           , UBR 0xAA00   0xAA5F   )
         , ( MYANMAR_EXTENDED_A                             , UBR 0xAA60   0xAA7F   )
         , ( TAI_VIET                                       , UBR 0xAA80   0xAADF   )
         , ( ETHIOPIC_EXTENDED_A                            , UBR 0xAB00   0xAB2F   )
         , ( MEETEI_MAYEK                                   , UBR 0xABC0   0xABFF   )
         , ( HANGUL_SYLLABLES                               , UBR 0xAC00   0xD7AF   )
         , ( HANGUL_JAMO_EXTENDED_B                         , UBR 0xD7B0   0xD7FF   )
         , ( HIGH_SURROGATES                                , UBR 0xD800   0xDB7F   )
         , ( HIGH_PRIVATE_USE_SURROGATES                    , UBR 0xDB80   0xDBFF   )
         , ( LOW_SURROGATES                                 , UBR 0xDC00   0xDFFF   )
         , ( PRIVATE_USE_AREA                               , UBR 0xE000   0xF8FF   )
         , ( CJK_COMPATIBILITY_IDEOGRAPHS                   , UBR 0xF900   0xFAFF   )
         , ( ALPHABETIC_PRESENTATION_FORMS                  , UBR 0xFB00   0xFB4F   )
         , ( ARABIC_PRESENTATION_FORMS_A                    , UBR 0xFB50   0xFDFF   )
         , ( VARIATION_SELECTORS                            , UBR 0xFE00   0xFE0F   )
         , ( VERTICAL_FORMS                                 , UBR 0xFE10   0xFE1F   )
         , ( COMBINING_HALF_MARKS                           , UBR 0xFE20   0xFE2F   )
         , ( CJK_COMPATIBILITY_FORMS                        , UBR 0xFE30   0xFE4F   )
         , ( SMALL_FORM_VARIANTS                            , UBR 0xFE50   0xFE6F   )
         , ( ARABIC_PRESENTATION_FORMS_B                    , UBR 0xFE70   0xFEFF   )
         , ( HALFWIDTH_AND_FULLWIDTH_FORMS                  , UBR 0xFF00   0xFFEF   )
         , ( SPECIALS                                       , UBR 0xFFF0   0xFFFF   )
         , ( LINEAR_B_SYLLABARY                             , UBR 0x10000  0x1007F  )
         , ( LINEAR_B_IDEOGRAMS                             , UBR 0x10080  0x100FF  )
         , ( AEGEAN_NUMBERS                                 , UBR 0x10100  0x1013F  )
         , ( ANCIENT_GREEK_NUMBERS                          , UBR 0x10140  0x1018F  )
         , ( ANCIENT_SYMBOLS                                , UBR 0x10190  0x101CF  )
         , ( PHAISTOS_DISC                                  , UBR 0x101D0  0x101FF  )
         , ( LYCIAN                                         , UBR 0x10280  0x1029F  )
         , ( CARIAN                                         , UBR 0x102A0  0x102DF  )
         , ( OLD_ITALIC                                     , UBR 0x10300  0x1032F  )
         , ( GOTHIC                                         , UBR 0x10330  0x1034F  )
         , ( UGARITIC                                       , UBR 0x10380  0x1039F  )
         , ( OLD_PERSIAN                                    , UBR 0x103A0  0x103DF  )
         , ( DESERET                                        , UBR 0x10400  0x1044F  )
         , ( SHAVIAN                                        , UBR 0x10450  0x1047F  )
         , ( OSMANYA                                        , UBR 0x10480  0x104AF  )
         , ( CYPRIOT_SYLLABARY                              , UBR 0x10800  0x1083F  )
         , ( IMPERIAL_ARAMAIC                               , UBR 0x10840  0x1085F  )
         , ( PHOENICIAN                                     , UBR 0x10900  0x1091F  )
         , ( LYDIAN                                         , UBR 0x10920  0x1093F  )
         , ( KHAROSHTHI                                     , UBR 0x10A00  0x10A5F  )
         , ( OLD_SOUTH_ARABIAN                              , UBR 0x10A60  0x10A7F  )
         , ( AVESTAN                                        , UBR 0x10B00  0x10B3F  )
         , ( INSCRIPTIONAL_PARTHIAN                         , UBR 0x10B40  0x10B5F  )
         , ( INSCRIPTIONAL_PAHLAVI                          , UBR 0x10B60  0x10B7F  )
         , ( OLD_TURKIC                                     , UBR 0x10C00  0x10C4F  )
         , ( RUMI_NUMERAL_SYMBOLS                           , UBR 0x10E60  0x10E7F  )
         , ( BRAHMI                                         , UBR 0x11000  0x1107F  )
         , ( KAITHI                                         , UBR 0x11080  0x110CF  )
         , ( CUNEIFORM                                      , UBR 0x12000  0x123FF  )
         , ( CUNEIFORM_NUMBERS_AND_PUNCTUATION              , UBR 0x12400  0x1247F  )
         , ( EGYPTIAN_HIEROGLYPHS                           , UBR 0x13000  0x1342F  )
         , ( BAMUM_SUPPLEMENT                               , UBR 0x16800  0x16A3F  )
         , ( KANA_SUPPLEMENT                                , UBR 0x1B000  0x1B0FF  )
         , ( BYZANTINE_MUSICAL_SYMBOLS                      , UBR 0x1D000  0x1D0FF  )
         , ( MUSICAL_SYMBOLS                                , UBR 0x1D100  0x1D1FF  )
         , ( ANCIENT_GREEK_MUSICAL_NOTATION                 , UBR 0x1D200  0x1D24F  )
         , ( TAI_XUAN_JING_SYMBOLS                          , UBR 0x1D300  0x1D35F  )
         , ( COUNTING_ROD_NUMERALS                          , UBR 0x1D360  0x1D37F  )
         , ( MATHEMATICAL_ALPHANUMERIC_SYMBOLS              , UBR 0x1D400  0x1D7FF  )
         , ( MAHJONG_TILES                                  , UBR 0x1F000  0x1F02F  )
         , ( DOMINO_TILES                                   , UBR 0x1F030  0x1F09F  )
         , ( PLAYING_CARDS                                  , UBR 0x1F0A0  0x1F0FF  )
         , ( ENCLOSED_ALPHANUMERIC_SUPPLEMENT               , UBR 0x1F100  0x1F1FF  )
         , ( ENCLOSED_IDEOGRAPHIC_SUPPLEMENT                , UBR 0x1F200  0x1F2FF  )
         , ( MISCELLANEOUS_SYMBOLS_AND_PICTOGRAPHS          , UBR 0x1F300  0x1F5FF  )
         , ( EMOTICONS                                      , UBR 0x1F600  0x1F64F  )
         , ( TRANSPORT_AND_MAP_SYMBOLS                      , UBR 0x1F680  0x1F6FF  )
         , ( ALCHEMICAL_SYMBOLS                             , UBR 0x1F700  0x1F77F  )
         , ( CJK_UNIFIED_IDEOGRAPHS_EXTENSION_B             , UBR 0x20000  0x2A6DF  )
         , ( CJK_UNIFIED_IDEOGRAPHS_EXTENSION_C             , UBR 0x2A700  0x2B73F  )
         , ( CJK_UNIFIED_IDEOGRAPHS_EXTENSION_D             , UBR 0x2B740  0x2B81F  )
         , ( CJK_COMPATIBILITY_IDEOGRAPHS_SUPPLEMENT        , UBR 0x2F800  0x2FA1F  )
         , ( TAGS                                           , UBR 0xE0000  0xE007F  )
         , ( VARIATION_SELECTORS_SUPPLEMENT                 , UBR 0xE0100  0xE01EF  )
         , ( SUPPLEMENTARY_PRIVATE_USE_AREA_A               , UBR 0xF0000  0xFFFFF  )
         , ( SUPPLEMENTARY_PRIVATE_USE_AREA_B               , UBR 0x100000 0x10FFFF )
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
entityENum = [ ("AElig"                                  , Left 0x000C6               ) -- LATIN CAPITAL LETTER AE
             , ("AMP"                                    , Left 0x00026               ) -- AMPERSAND
             , ("Aacgr"                                  , Left 0x00386               ) -- GREEK CAPITAL LETTER ALPHA WITH TONOS
             , ("Aacute"                                 , Left 0x000C1               ) -- LATIN CAPITAL LETTER A WITH ACUTE
             , ("Abreve"                                 , Left 0x00102               ) -- LATIN CAPITAL LETTER A WITH BREVE
             , ("Acirc"                                  , Left 0x000C2               ) -- LATIN CAPITAL LETTER A WITH CIRCUMFLEX
             , ("Acy"                                    , Left 0x00410               ) -- CYRILLIC CAPITAL LETTER A
             , ("Afr"                                    , Left 0x1D504               ) -- MATHEMATICAL FRAKTUR CAPITAL A
             , ("Agr"                                    , Left 0x00391               ) -- GREEK CAPITAL LETTER ALPHA
             , ("Agrave"                                 , Left 0x000C0               ) -- LATIN CAPITAL LETTER A WITH GRAVE
             , ("Alpha"                                  , Left 0x00391               ) -- GREEK CAPITAL LETTER ALPHA
             , ("Amacr"                                  , Left 0x00100               ) -- LATIN CAPITAL LETTER A WITH MACRON
             , ("And"                                    , Left 0x02A53               ) -- DOUBLE LOGICAL AND
             , ("Aogon"                                  , Left 0x00104               ) -- LATIN CAPITAL LETTER A WITH OGONEK
             , ("Aopf"                                   , Left 0x1D538               ) -- MATHEMATICAL DOUBLE-STRUCK CAPITAL A
             , ("ApplyFunction"                          , Left 0x02061               ) -- FUNCTION APPLICATION
             , ("Aring"                                  , Left 0x000C5               ) -- LATIN CAPITAL LETTER A WITH RING ABOVE
             , ("Ascr"                                   , Left 0x1D49C               ) -- MATHEMATICAL SCRIPT CAPITAL A
             , ("Assign"                                 , Left 0x02254               ) -- COLON EQUALS
             , ("Atilde"                                 , Left 0x000C3               ) -- LATIN CAPITAL LETTER A WITH TILDE
             , ("Auml"                                   , Left 0x000C4               ) -- LATIN CAPITAL LETTER A WITH DIAERESIS
             , ("Backslash"                              , Left 0x02216               ) -- SET MINUS
             , ("Barv"                                   , Left 0x02AE7               ) -- SHORT DOWN TACK WITH OVERBAR
             , ("Barwed"                                 , Left 0x02306               ) -- PERSPECTIVE
             , ("Bcy"                                    , Left 0x00411               ) -- CYRILLIC CAPITAL LETTER BE
             , ("Because"                                , Left 0x02235               ) -- BECAUSE
             , ("Bernoullis"                             , Left 0x0212C               ) -- SCRIPT CAPITAL B
             , ("Beta"                                   , Left 0x00392               ) -- GREEK CAPITAL LETTER BETA
             , ("Bfr"                                    , Left 0x1D505               ) -- MATHEMATICAL FRAKTUR CAPITAL B
             , ("Bgr"                                    , Left 0x00392               ) -- GREEK CAPITAL LETTER BETA
             , ("Bopf"                                   , Left 0x1D539               ) -- MATHEMATICAL DOUBLE-STRUCK CAPITAL B
             , ("Breve"                                  , Left 0x002D8               ) -- BREVE
             , ("Bscr"                                   , Left 0x0212C               ) -- SCRIPT CAPITAL B
             , ("Bumpeq"                                 , Left 0x0224E               ) -- GEOMETRICALLY EQUIVALENT TO
             , ("CHcy"                                   , Left 0x00427               ) -- CYRILLIC CAPITAL LETTER CHE
             , ("COPY"                                   , Left 0x000A9               ) -- COPYRIGHT SIGN
             , ("Cacute"                                 , Left 0x00106               ) -- LATIN CAPITAL LETTER C WITH ACUTE
             , ("Cap"                                    , Left 0x022D2               ) -- DOUBLE INTERSECTION
             , ("CapitalDifferentialD"                   , Left 0x02145               ) -- DOUBLE-STRUCK ITALIC CAPITAL D
             , ("Cayleys"                                , Left 0x0212D               ) -- BLACK-LETTER CAPITAL C
             , ("Ccaron"                                 , Left 0x0010C               ) -- LATIN CAPITAL LETTER C WITH CARON
             , ("Ccedil"                                 , Left 0x000C7               ) -- LATIN CAPITAL LETTER C WITH CEDILLA
             , ("Ccirc"                                  , Left 0x00108               ) -- LATIN CAPITAL LETTER C WITH CIRCUMFLEX
             , ("Cconint"                                , Left 0x02230               ) -- VOLUME INTEGRAL
             , ("Cdot"                                   , Left 0x0010A               ) -- LATIN CAPITAL LETTER C WITH DOT ABOVE
             , ("Cedilla"                                , Left 0x000B8               ) -- CEDILLA
             , ("CenterDot"                              , Left 0x000B7               ) -- MIDDLE DOT
             , ("Cfr"                                    , Left 0x0212D               ) -- BLACK-LETTER CAPITAL C
             , ("Chi"                                    , Left 0x003A7               ) -- GREEK CAPITAL LETTER CHI
             , ("CircleDot"                              , Left 0x02299               ) -- CIRCLED DOT OPERATOR
             , ("CircleMinus"                            , Left 0x02296               ) -- CIRCLED MINUS
             , ("CirclePlus"                             , Left 0x02295               ) -- CIRCLED PLUS
             , ("CircleTimes"                            , Left 0x02297               ) -- CIRCLED TIMES
             , ("ClockwiseContourIntegral"               , Left 0x02232               ) -- CLOCKWISE CONTOUR INTEGRAL
             , ("CloseCurlyDoubleQuote"                  , Left 0x0201D               ) -- RIGHT DOUBLE QUOTATION MARK
             , ("CloseCurlyQuote"                        , Left 0x02019               ) -- RIGHT SINGLE QUOTATION MARK
             , ("Colon"                                  , Left 0x02237               ) -- PROPORTION
             , ("Colone"                                 , Left 0x02A74               ) -- DOUBLE COLON EQUAL
             , ("Congruent"                              , Left 0x02261               ) -- IDENTICAL TO
             , ("Conint"                                 , Left 0x0222F               ) -- SURFACE INTEGRAL
             , ("ContourIntegral"                        , Left 0x0222E               ) -- CONTOUR INTEGRAL
             , ("Copf"                                   , Left 0x02102               ) -- DOUBLE-STRUCK CAPITAL C
             , ("Coproduct"                              , Left 0x02210               ) -- N-ARY COPRODUCT
             , ("CounterClockwiseContourIntegral"        , Left 0x02233               ) -- ANTICLOCKWISE CONTOUR INTEGRAL
             , ("Cross"                                  , Left 0x02A2F               ) -- VECTOR OR CROSS PRODUCT
             , ("Cscr"                                   , Left 0x1D49E               ) -- MATHEMATICAL SCRIPT CAPITAL C
             , ("Cup"                                    , Left 0x022D3               ) -- DOUBLE UNION
             , ("CupCap"                                 , Left 0x0224D               ) -- EQUIVALENT TO
             , ("DD"                                     , Left 0x02145               ) -- DOUBLE-STRUCK ITALIC CAPITAL D
             , ("DDotrahd"                               , Left 0x02911               ) -- RIGHTWARDS ARROW WITH DOTTED STEM
             , ("DJcy"                                   , Left 0x00402               ) -- CYRILLIC CAPITAL LETTER DJE
             , ("DScy"                                   , Left 0x00405               ) -- CYRILLIC CAPITAL LETTER DZE
             , ("DZcy"                                   , Left 0x0040F               ) -- CYRILLIC CAPITAL LETTER DZHE
             , ("Dagger"                                 , Left 0x02021               ) -- DOUBLE DAGGER
             , ("Darr"                                   , Left 0x021A1               ) -- DOWNWARDS TWO HEADED ARROW
             , ("Dashv"                                  , Left 0x02AE4               ) -- VERTICAL BAR DOUBLE LEFT TURNSTILE
             , ("Dcaron"                                 , Left 0x0010E               ) -- LATIN CAPITAL LETTER D WITH CARON
             , ("Dcy"                                    , Left 0x00414               ) -- CYRILLIC CAPITAL LETTER DE
             , ("Del"                                    , Left 0x02207               ) -- NABLA
             , ("Delta"                                  , Left 0x00394               ) -- GREEK CAPITAL LETTER DELTA
             , ("Dfr"                                    , Left 0x1D507               ) -- MATHEMATICAL FRAKTUR CAPITAL D
             , ("Dgr"                                    , Left 0x00394               ) -- GREEK CAPITAL LETTER DELTA
             , ("DiacriticalAcute"                       , Left 0x000B4               ) -- ACUTE ACCENT
             , ("DiacriticalDot"                         , Left 0x002D9               ) -- DOT ABOVE
             , ("DiacriticalDoubleAcute"                 , Left 0x002DD               ) -- DOUBLE ACUTE ACCENT
             , ("DiacriticalGrave"                       , Left 0x00060               ) -- GRAVE ACCENT
             , ("DiacriticalTilde"                       , Left 0x002DC               ) -- SMALL TILDE
             , ("Diamond"                                , Left 0x022C4               ) -- DIAMOND OPERATOR
             , ("DifferentialD"                          , Left 0x02146               ) -- DOUBLE-STRUCK ITALIC SMALL D
             , ("Dopf"                                   , Left 0x1D53B               ) -- MATHEMATICAL DOUBLE-STRUCK CAPITAL D
             , ("Dot"                                    , Left 0x000A8               ) -- DIAERESIS
             , ("DotDot"                                 , Left 0x020DC               ) -- COMBINING FOUR DOTS ABOVE
             , ("DotEqual"                               , Left 0x02250               ) -- APPROACHES THE LIMIT
             , ("DoubleContourIntegral"                  , Left 0x0222F               ) -- SURFACE INTEGRAL
             , ("DoubleDot"                              , Left 0x000A8               ) -- DIAERESIS
             , ("DoubleDownArrow"                        , Left 0x021D3               ) -- DOWNWARDS DOUBLE ARROW
             , ("DoubleLeftArrow"                        , Left 0x021D0               ) -- LEFTWARDS DOUBLE ARROW
             , ("DoubleLeftRightArrow"                   , Left 0x021D4               ) -- LEFT RIGHT DOUBLE ARROW
             , ("DoubleLeftTee"                          , Left 0x02AE4               ) -- VERTICAL BAR DOUBLE LEFT TURNSTILE
             , ("DoubleLongLeftArrow"                    , Left 0x027F8               ) -- LONG LEFTWARDS DOUBLE ARROW
             , ("DoubleLongLeftRightArrow"               , Left 0x027FA               ) -- LONG LEFT RIGHT DOUBLE ARROW
             , ("DoubleLongRightArrow"                   , Left 0x027F9               ) -- LONG RIGHTWARDS DOUBLE ARROW
             , ("DoubleRightArrow"                       , Left 0x021D2               ) -- RIGHTWARDS DOUBLE ARROW
             , ("DoubleRightTee"                         , Left 0x022A8               ) -- TRUE
             , ("DoubleUpArrow"                          , Left 0x021D1               ) -- UPWARDS DOUBLE ARROW
             , ("DoubleUpDownArrow"                      , Left 0x021D5               ) -- UP DOWN DOUBLE ARROW
             , ("DoubleVerticalBar"                      , Left 0x02225               ) -- PARALLEL TO
             , ("DownArrow"                              , Left 0x02193               ) -- DOWNWARDS ARROW
             , ("DownArrowBar"                           , Left 0x02913               ) -- DOWNWARDS ARROW TO BAR
             , ("DownArrowUpArrow"                       , Left 0x021F5               ) -- DOWNWARDS ARROW LEFTWARDS OF UPWARDS ARROW
             , ("DownBreve"                              , Left 0x00311               ) -- COMBINING INVERTED BREVE
             , ("DownLeftRightVector"                    , Left 0x02950               ) -- LEFT BARB DOWN RIGHT BARB DOWN HARPOON
             , ("DownLeftTeeVector"                      , Left 0x0295E               ) -- LEFTWARDS HARPOON WITH BARB DOWN FROM BAR
             , ("DownLeftVector"                         , Left 0x021BD               ) -- LEFTWARDS HARPOON WITH BARB DOWNWARDS
             , ("DownLeftVectorBar"                      , Left 0x02956               ) -- LEFTWARDS HARPOON WITH BARB DOWN TO BAR
             , ("DownRightTeeVector"                     , Left 0x0295F               ) -- RIGHTWARDS HARPOON WITH BARB DOWN FROM BAR
             , ("DownRightVector"                        , Left 0x021C1               ) -- RIGHTWARDS HARPOON WITH BARB DOWNWARDS
             , ("DownRightVectorBar"                     , Left 0x02957               ) -- RIGHTWARDS HARPOON WITH BARB DOWN TO BAR
             , ("DownTee"                                , Left 0x022A4               ) -- DOWN TACK
             , ("DownTeeArrow"                           , Left 0x021A7               ) -- DOWNWARDS ARROW FROM BAR
             , ("Downarrow"                              , Left 0x021D3               ) -- DOWNWARDS DOUBLE ARROW
             , ("Dscr"                                   , Left 0x1D49F               ) -- MATHEMATICAL SCRIPT CAPITAL D
             , ("Dstrok"                                 , Left 0x00110               ) -- LATIN CAPITAL LETTER D WITH STROKE
             , ("EEacgr"                                 , Left 0x00389               ) -- GREEK CAPITAL LETTER ETA WITH TONOS
             , ("EEgr"                                   , Left 0x00397               ) -- GREEK CAPITAL LETTER ETA
             , ("ENG"                                    , Left 0x0014A               ) -- LATIN CAPITAL LETTER ENG
             , ("ETH"                                    , Left 0x000D0               ) -- LATIN CAPITAL LETTER ETH
             , ("Eacgr"                                  , Left 0x00388               ) -- GREEK CAPITAL LETTER EPSILON WITH TONOS
             , ("Eacute"                                 , Left 0x000C9               ) -- LATIN CAPITAL LETTER E WITH ACUTE
             , ("Ecaron"                                 , Left 0x0011A               ) -- LATIN CAPITAL LETTER E WITH CARON
             , ("Ecirc"                                  , Left 0x000CA               ) -- LATIN CAPITAL LETTER E WITH CIRCUMFLEX
             , ("Ecy"                                    , Left 0x0042D               ) -- CYRILLIC CAPITAL LETTER E
             , ("Edot"                                   , Left 0x00116               ) -- LATIN CAPITAL LETTER E WITH DOT ABOVE
             , ("Efr"                                    , Left 0x1D508               ) -- MATHEMATICAL FRAKTUR CAPITAL E
             , ("Egr"                                    , Left 0x00395               ) -- GREEK CAPITAL LETTER EPSILON
             , ("Egrave"                                 , Left 0x000C8               ) -- LATIN CAPITAL LETTER E WITH GRAVE
             , ("Element"                                , Left 0x02208               ) -- ELEMENT OF
             , ("Emacr"                                  , Left 0x00112               ) -- LATIN CAPITAL LETTER E WITH MACRON
             , ("EmptySmallSquare"                       , Left 0x025FB               ) -- WHITE MEDIUM SQUARE
             , ("EmptyVerySmallSquare"                   , Left 0x025AB               ) -- WHITE SMALL SQUARE
             , ("Eogon"                                  , Left 0x00118               ) -- LATIN CAPITAL LETTER E WITH OGONEK
             , ("Eopf"                                   , Left 0x1D53C               ) -- MATHEMATICAL DOUBLE-STRUCK CAPITAL E
             , ("Epsilon"                                , Left 0x00395               ) -- GREEK CAPITAL LETTER EPSILON
             , ("Equal"                                  , Left 0x02A75               ) -- TWO CONSECUTIVE EQUALS SIGNS
             , ("EqualTilde"                             , Left 0x02242               ) -- MINUS TILDE
             , ("Equilibrium"                            , Left 0x021CC               ) -- RIGHTWARDS HARPOON OVER LEFTWARDS HARPOON
             , ("Escr"                                   , Left 0x02130               ) -- SCRIPT CAPITAL E
             , ("Esim"                                   , Left 0x02A73               ) -- EQUALS SIGN ABOVE TILDE OPERATOR
             , ("Eta"                                    , Left 0x00397               ) -- GREEK CAPITAL LETTER ETA
             , ("Euml"                                   , Left 0x000CB               ) -- LATIN CAPITAL LETTER E WITH DIAERESIS
             , ("Exists"                                 , Left 0x02203               ) -- THERE EXISTS
             , ("ExponentialE"                           , Left 0x02147               ) -- DOUBLE-STRUCK ITALIC SMALL E
             , ("Fcy"                                    , Left 0x00424               ) -- CYRILLIC CAPITAL LETTER EF
             , ("Ffr"                                    , Left 0x1D509               ) -- MATHEMATICAL FRAKTUR CAPITAL F
             , ("FilledSmallSquare"                      , Left 0x025FC               ) -- BLACK MEDIUM SQUARE
             , ("FilledVerySmallSquare"                  , Left 0x025AA               ) -- BLACK SMALL SQUARE
             , ("Fopf"                                   , Left 0x1D53D               ) -- MATHEMATICAL DOUBLE-STRUCK CAPITAL F
             , ("ForAll"                                 , Left 0x02200               ) -- FOR ALL
             , ("Fouriertrf"                             , Left 0x02131               ) -- SCRIPT CAPITAL F
             , ("Fscr"                                   , Left 0x02131               ) -- SCRIPT CAPITAL F
             , ("GJcy"                                   , Left 0x00403               ) -- CYRILLIC CAPITAL LETTER GJE
             , ("GT"                                     , Left 0x0003E               ) -- GREATER-THAN SIGN
             , ("Gamma"                                  , Left 0x00393               ) -- GREEK CAPITAL LETTER GAMMA
             , ("Gammad"                                 , Left 0x003DC               ) -- GREEK LETTER DIGAMMA
             , ("Gbreve"                                 , Left 0x0011E               ) -- LATIN CAPITAL LETTER G WITH BREVE
             , ("Gcedil"                                 , Left 0x00122               ) -- LATIN CAPITAL LETTER G WITH CEDILLA
             , ("Gcirc"                                  , Left 0x0011C               ) -- LATIN CAPITAL LETTER G WITH CIRCUMFLEX
             , ("Gcy"                                    , Left 0x00413               ) -- CYRILLIC CAPITAL LETTER GHE
             , ("Gdot"                                   , Left 0x00120               ) -- LATIN CAPITAL LETTER G WITH DOT ABOVE
             , ("Gfr"                                    , Left 0x1D50A               ) -- MATHEMATICAL FRAKTUR CAPITAL G
             , ("Gg"                                     , Left 0x022D9               ) -- VERY MUCH GREATER-THAN
             , ("Ggr"                                    , Left 0x00393               ) -- GREEK CAPITAL LETTER GAMMA
             , ("Gopf"                                   , Left 0x1D53E               ) -- MATHEMATICAL DOUBLE-STRUCK CAPITAL G
             , ("GreaterEqual"                           , Left 0x02265               ) -- GREATER-THAN OR EQUAL TO
             , ("GreaterEqualLess"                       , Left 0x022DB               ) -- GREATER-THAN EQUAL TO OR LESS-THAN
             , ("GreaterFullEqual"                       , Left 0x02267               ) -- GREATER-THAN OVER EQUAL TO
             , ("GreaterGreater"                         , Left 0x02AA2               ) -- DOUBLE NESTED GREATER-THAN
             , ("GreaterLess"                            , Left 0x02277               ) -- GREATER-THAN OR LESS-THAN
             , ("GreaterSlantEqual"                      , Left 0x02A7E               ) -- GREATER-THAN OR SLANTED EQUAL TO
             , ("GreaterTilde"                           , Left 0x02273               ) -- GREATER-THAN OR EQUIVALENT TO
             , ("Gscr"                                   , Left 0x1D4A2               ) -- MATHEMATICAL SCRIPT CAPITAL G
             , ("Gt"                                     , Left 0x0226B               ) -- MUCH GREATER-THAN
             , ("HARDcy"                                 , Left 0x0042A               ) -- CYRILLIC CAPITAL LETTER HARD SIGN
             , ("Hacek"                                  , Left 0x002C7               ) -- CARON
             , ("Hat"                                    , Left 0x0005E               ) -- CIRCUMFLEX ACCENT
             , ("Hcirc"                                  , Left 0x00124               ) -- LATIN CAPITAL LETTER H WITH CIRCUMFLEX
             , ("Hfr"                                    , Left 0x0210C               ) -- BLACK-LETTER CAPITAL H
             , ("HilbertSpace"                           , Left 0x0210B               ) -- SCRIPT CAPITAL H
             , ("Hopf"                                   , Left 0x0210D               ) -- DOUBLE-STRUCK CAPITAL H
             , ("HorizontalLine"                         , Left 0x02500               ) -- BOX DRAWINGS LIGHT HORIZONTAL
             , ("Hscr"                                   , Left 0x0210B               ) -- SCRIPT CAPITAL H
             , ("Hstrok"                                 , Left 0x00126               ) -- LATIN CAPITAL LETTER H WITH STROKE
             , ("HumpDownHump"                           , Left 0x0224E               ) -- GEOMETRICALLY EQUIVALENT TO
             , ("HumpEqual"                              , Left 0x0224F               ) -- DIFFERENCE BETWEEN
             , ("IEcy"                                   , Left 0x00415               ) -- CYRILLIC CAPITAL LETTER IE
             , ("IJlig"                                  , Left 0x00132               ) -- LATIN CAPITAL LIGATURE IJ
             , ("IOcy"                                   , Left 0x00401               ) -- CYRILLIC CAPITAL LETTER IO
             , ("Iacgr"                                  , Left 0x0038A               ) -- GREEK CAPITAL LETTER IOTA WITH TONOS
             , ("Iacute"                                 , Left 0x000CD               ) -- LATIN CAPITAL LETTER I WITH ACUTE
             , ("Icirc"                                  , Left 0x000CE               ) -- LATIN CAPITAL LETTER I WITH CIRCUMFLEX
             , ("Icy"                                    , Left 0x00418               ) -- CYRILLIC CAPITAL LETTER I
             , ("Idigr"                                  , Left 0x003AA               ) -- GREEK CAPITAL LETTER IOTA WITH DIALYTIKA
             , ("Idot"                                   , Left 0x00130               ) -- LATIN CAPITAL LETTER I WITH DOT ABOVE
             , ("Ifr"                                    , Left 0x02111               ) -- BLACK-LETTER CAPITAL I
             , ("Igr"                                    , Left 0x00399               ) -- GREEK CAPITAL LETTER IOTA
             , ("Igrave"                                 , Left 0x000CC               ) -- LATIN CAPITAL LETTER I WITH GRAVE
             , ("Im"                                     , Left 0x02111               ) -- BLACK-LETTER CAPITAL I
             , ("Imacr"                                  , Left 0x0012A               ) -- LATIN CAPITAL LETTER I WITH MACRON
             , ("ImaginaryI"                             , Left 0x02148               ) -- DOUBLE-STRUCK ITALIC SMALL I
             , ("Implies"                                , Left 0x021D2               ) -- RIGHTWARDS DOUBLE ARROW
             , ("Int"                                    , Left 0x0222C               ) -- DOUBLE INTEGRAL
             , ("Integral"                               , Left 0x0222B               ) -- INTEGRAL
             , ("Intersection"                           , Left 0x022C2               ) -- N-ARY INTERSECTION
             , ("InvisibleComma"                         , Left 0x02063               ) -- INVISIBLE SEPARATOR
             , ("InvisibleTimes"                         , Left 0x02062               ) -- INVISIBLE TIMES
             , ("Iogon"                                  , Left 0x0012E               ) -- LATIN CAPITAL LETTER I WITH OGONEK
             , ("Iopf"                                   , Left 0x1D540               ) -- MATHEMATICAL DOUBLE-STRUCK CAPITAL I
             , ("Iota"                                   , Left 0x00399               ) -- GREEK CAPITAL LETTER IOTA
             , ("Iscr"                                   , Left 0x02110               ) -- SCRIPT CAPITAL I
             , ("Itilde"                                 , Left 0x00128               ) -- LATIN CAPITAL LETTER I WITH TILDE
             , ("Iukcy"                                  , Left 0x00406               ) -- CYRILLIC CAPITAL LETTER BYELORUSSIAN-UKRAINIAN I
             , ("Iuml"                                   , Left 0x000CF               ) -- LATIN CAPITAL LETTER I WITH DIAERESIS
             , ("Jcirc"                                  , Left 0x00134               ) -- LATIN CAPITAL LETTER J WITH CIRCUMFLEX
             , ("Jcy"                                    , Left 0x00419               ) -- CYRILLIC CAPITAL LETTER SHORT I
             , ("Jfr"                                    , Left 0x1D50D               ) -- MATHEMATICAL FRAKTUR CAPITAL J
             , ("Jopf"                                   , Left 0x1D541               ) -- MATHEMATICAL DOUBLE-STRUCK CAPITAL J
             , ("Jscr"                                   , Left 0x1D4A5               ) -- MATHEMATICAL SCRIPT CAPITAL J
             , ("Jsercy"                                 , Left 0x00408               ) -- CYRILLIC CAPITAL LETTER JE
             , ("Jukcy"                                  , Left 0x00404               ) -- CYRILLIC CAPITAL LETTER UKRAINIAN IE
             , ("KHcy"                                   , Left 0x00425               ) -- CYRILLIC CAPITAL LETTER HA
             , ("KHgr"                                   , Left 0x003A7               ) -- GREEK CAPITAL LETTER CHI
             , ("KJcy"                                   , Left 0x0040C               ) -- CYRILLIC CAPITAL LETTER KJE
             , ("Kappa"                                  , Left 0x0039A               ) -- GREEK CAPITAL LETTER KAPPA
             , ("Kcedil"                                 , Left 0x00136               ) -- LATIN CAPITAL LETTER K WITH CEDILLA
             , ("Kcy"                                    , Left 0x0041A               ) -- CYRILLIC CAPITAL LETTER KA
             , ("Kfr"                                    , Left 0x1D50E               ) -- MATHEMATICAL FRAKTUR CAPITAL K
             , ("Kgr"                                    , Left 0x0039A               ) -- GREEK CAPITAL LETTER KAPPA
             , ("Kopf"                                   , Left 0x1D542               ) -- MATHEMATICAL DOUBLE-STRUCK CAPITAL K
             , ("Kscr"                                   , Left 0x1D4A6               ) -- MATHEMATICAL SCRIPT CAPITAL K
             , ("LJcy"                                   , Left 0x00409               ) -- CYRILLIC CAPITAL LETTER LJE
             , ("LT"                                     , Left 0x0003C               ) -- LESS-THAN SIGN
             , ("Lacute"                                 , Left 0x00139               ) -- LATIN CAPITAL LETTER L WITH ACUTE
             , ("Lambda"                                 , Left 0x0039B               ) -- GREEK CAPITAL LETTER LAMDA
             , ("Lang"                                   , Left 0x027EA               ) -- MATHEMATICAL LEFT DOUBLE ANGLE BRACKET
             , ("Laplacetrf"                             , Left 0x02112               ) -- SCRIPT CAPITAL L
             , ("Larr"                                   , Left 0x0219E               ) -- LEFTWARDS TWO HEADED ARROW
             , ("Lcaron"                                 , Left 0x0013D               ) -- LATIN CAPITAL LETTER L WITH CARON
             , ("Lcedil"                                 , Left 0x0013B               ) -- LATIN CAPITAL LETTER L WITH CEDILLA
             , ("Lcy"                                    , Left 0x0041B               ) -- CYRILLIC CAPITAL LETTER EL
             , ("LeftAngleBracket"                       , Left 0x027E8               ) -- MATHEMATICAL LEFT ANGLE BRACKET
             , ("LeftArrow"                              , Left 0x02190               ) -- LEFTWARDS ARROW
             , ("LeftArrowBar"                           , Left 0x021E4               ) -- LEFTWARDS ARROW TO BAR
             , ("LeftArrowRightArrow"                    , Left 0x021C6               ) -- LEFTWARDS ARROW OVER RIGHTWARDS ARROW
             , ("LeftCeiling"                            , Left 0x02308               ) -- LEFT CEILING
             , ("LeftDoubleBracket"                      , Left 0x027E6               ) -- MATHEMATICAL LEFT WHITE SQUARE BRACKET
             , ("LeftDownTeeVector"                      , Left 0x02961               ) -- DOWNWARDS HARPOON WITH BARB LEFT FROM BAR
             , ("LeftDownVector"                         , Left 0x021C3               ) -- DOWNWARDS HARPOON WITH BARB LEFTWARDS
             , ("LeftDownVectorBar"                      , Left 0x02959               ) -- DOWNWARDS HARPOON WITH BARB LEFT TO BAR
             , ("LeftFloor"                              , Left 0x0230A               ) -- LEFT FLOOR
             , ("LeftRightArrow"                         , Left 0x02194               ) -- LEFT RIGHT ARROW
             , ("LeftRightVector"                        , Left 0x0294E               ) -- LEFT BARB UP RIGHT BARB UP HARPOON
             , ("LeftTee"                                , Left 0x022A3               ) -- LEFT TACK
             , ("LeftTeeArrow"                           , Left 0x021A4               ) -- LEFTWARDS ARROW FROM BAR
             , ("LeftTeeVector"                          , Left 0x0295A               ) -- LEFTWARDS HARPOON WITH BARB UP FROM BAR
             , ("LeftTriangle"                           , Left 0x022B2               ) -- NORMAL SUBGROUP OF
             , ("LeftTriangleBar"                        , Left 0x029CF               ) -- LEFT TRIANGLE BESIDE VERTICAL BAR
             , ("LeftTriangleEqual"                      , Left 0x022B4               ) -- NORMAL SUBGROUP OF OR EQUAL TO
             , ("LeftUpDownVector"                       , Left 0x02951               ) -- UP BARB LEFT DOWN BARB LEFT HARPOON
             , ("LeftUpTeeVector"                        , Left 0x02960               ) -- UPWARDS HARPOON WITH BARB LEFT FROM BAR
             , ("LeftUpVector"                           , Left 0x021BF               ) -- UPWARDS HARPOON WITH BARB LEFTWARDS
             , ("LeftUpVectorBar"                        , Left 0x02958               ) -- UPWARDS HARPOON WITH BARB LEFT TO BAR
             , ("LeftVector"                             , Left 0x021BC               ) -- LEFTWARDS HARPOON WITH BARB UPWARDS
             , ("LeftVectorBar"                          , Left 0x02952               ) -- LEFTWARDS HARPOON WITH BARB UP TO BAR
             , ("Leftarrow"                              , Left 0x021D0               ) -- LEFTWARDS DOUBLE ARROW
             , ("Leftrightarrow"                         , Left 0x021D4               ) -- LEFT RIGHT DOUBLE ARROW
             , ("LessEqualGreater"                       , Left 0x022DA               ) -- LESS-THAN EQUAL TO OR GREATER-THAN
             , ("LessFullEqual"                          , Left 0x02266               ) -- LESS-THAN OVER EQUAL TO
             , ("LessGreater"                            , Left 0x02276               ) -- LESS-THAN OR GREATER-THAN
             , ("LessLess"                               , Left 0x02AA1               ) -- DOUBLE NESTED LESS-THAN
             , ("LessSlantEqual"                         , Left 0x02A7D               ) -- LESS-THAN OR SLANTED EQUAL TO
             , ("LessTilde"                              , Left 0x02272               ) -- LESS-THAN OR EQUIVALENT TO
             , ("Lfr"                                    , Left 0x1D50F               ) -- MATHEMATICAL FRAKTUR CAPITAL L
             , ("Lgr"                                    , Left 0x0039B               ) -- GREEK CAPITAL LETTER LAMDA
             , ("Ll"                                     , Left 0x022D8               ) -- VERY MUCH LESS-THAN
             , ("Lleftarrow"                             , Left 0x021DA               ) -- LEFTWARDS TRIPLE ARROW
             , ("Lmidot"                                 , Left 0x0013F               ) -- LATIN CAPITAL LETTER L WITH MIDDLE DOT
             , ("LongLeftArrow"                          , Left 0x027F5               ) -- LONG LEFTWARDS ARROW
             , ("LongLeftRightArrow"                     , Left 0x027F7               ) -- LONG LEFT RIGHT ARROW
             , ("LongRightArrow"                         , Left 0x027F6               ) -- LONG RIGHTWARDS ARROW
             , ("Longleftarrow"                          , Left 0x027F8               ) -- LONG LEFTWARDS DOUBLE ARROW
             , ("Longleftrightarrow"                     , Left 0x027FA               ) -- LONG LEFT RIGHT DOUBLE ARROW
             , ("Longrightarrow"                         , Left 0x027F9               ) -- LONG RIGHTWARDS DOUBLE ARROW
             , ("Lopf"                                   , Left 0x1D543               ) -- MATHEMATICAL DOUBLE-STRUCK CAPITAL L
             , ("LowerLeftArrow"                         , Left 0x02199               ) -- SOUTH WEST ARROW
             , ("LowerRightArrow"                        , Left 0x02198               ) -- SOUTH EAST ARROW
             , ("Lscr"                                   , Left 0x02112               ) -- SCRIPT CAPITAL L
             , ("Lsh"                                    , Left 0x021B0               ) -- UPWARDS ARROW WITH TIP LEFTWARDS
             , ("Lstrok"                                 , Left 0x00141               ) -- LATIN CAPITAL LETTER L WITH STROKE
             , ("Lt"                                     , Left 0x0226A               ) -- MUCH LESS-THAN
             , ("Map"                                    , Left 0x02905               ) -- RIGHTWARDS TWO-HEADED ARROW FROM BAR
             , ("Mcy"                                    , Left 0x0041C               ) -- CYRILLIC CAPITAL LETTER EM
             , ("MediumSpace"                            , Left 0x0205F               ) -- MEDIUM MATHEMATICAL SPACE
             , ("Mellintrf"                              , Left 0x02133               ) -- SCRIPT CAPITAL M
             , ("Mfr"                                    , Left 0x1D510               ) -- MATHEMATICAL FRAKTUR CAPITAL M
             , ("Mgr"                                    , Left 0x0039C               ) -- GREEK CAPITAL LETTER MU
             , ("MinusPlus"                              , Left 0x02213               ) -- MINUS-OR-PLUS SIGN
             , ("Mopf"                                   , Left 0x1D544               ) -- MATHEMATICAL DOUBLE-STRUCK CAPITAL M
             , ("Mscr"                                   , Left 0x02133               ) -- SCRIPT CAPITAL M
             , ("Mu"                                     , Left 0x0039C               ) -- GREEK CAPITAL LETTER MU
             , ("NJcy"                                   , Left 0x0040A               ) -- CYRILLIC CAPITAL LETTER NJE
             , ("Nacute"                                 , Left 0x00143               ) -- LATIN CAPITAL LETTER N WITH ACUTE
             , ("Ncaron"                                 , Left 0x00147               ) -- LATIN CAPITAL LETTER N WITH CARON
             , ("Ncedil"                                 , Left 0x00145               ) -- LATIN CAPITAL LETTER N WITH CEDILLA
             , ("Ncy"                                    , Left 0x0041D               ) -- CYRILLIC CAPITAL LETTER EN
             , ("NegativeMediumSpace"                    , Left 0x0200B               ) -- ZERO WIDTH SPACE
             , ("NegativeThickSpace"                     , Left 0x0200B               ) -- ZERO WIDTH SPACE
             , ("NegativeThinSpace"                      , Left 0x0200B               ) -- ZERO WIDTH SPACE
             , ("NegativeVeryThinSpace"                  , Left 0x0200B               ) -- ZERO WIDTH SPACE
             , ("NestedGreaterGreater"                   , Left 0x0226B               ) -- MUCH GREATER-THAN
             , ("NestedLessLess"                         , Left 0x0226A               ) -- MUCH LESS-THAN
             , ("NewLine"                                , Left 0x0000A               ) -- LINE FEED (LF)
             , ("Nfr"                                    , Left 0x1D511               ) -- MATHEMATICAL FRAKTUR CAPITAL N
             , ("Ngr"                                    , Left 0x0039D               ) -- GREEK CAPITAL LETTER NU
             , ("NoBreak"                                , Left 0x02060               ) -- WORD JOINER
             , ("NonBreakingSpace"                       , Left 0x000A0               ) -- NO-BREAK SPACE
             , ("Nopf"                                   , Left 0x02115               ) -- DOUBLE-STRUCK CAPITAL N
             , ("Not"                                    , Left 0x02AEC               ) -- DOUBLE STROKE NOT SIGN
             , ("NotCongruent"                           , Left 0x02262               ) -- NOT IDENTICAL TO
             , ("NotCupCap"                              , Left 0x0226D               ) -- NOT EQUIVALENT TO
             , ("NotDoubleVerticalBar"                   , Left 0x02226               ) -- NOT PARALLEL TO
             , ("NotElement"                             , Left 0x02209               ) -- NOT AN ELEMENT OF
             , ("NotEqual"                               , Left 0x02260               ) -- NOT EQUAL TO
             , ("NotEqualTilde"                          , Right [ 0x02242, 0x00338 ] ) -- MINUS TILDE with slash
             , ("NotExists"                              , Left 0x02204               ) -- THERE DOES NOT EXIST
             , ("NotGreater"                             , Left 0x0226F               ) -- NOT GREATER-THAN
             , ("NotGreaterEqual"                        , Left 0x02271               ) -- NEITHER GREATER-THAN NOR EQUAL TO
             , ("NotGreaterFullEqual"                    , Right [ 0x02267, 0x00338 ] ) -- GREATER-THAN OVER EQUAL TO with slash
             , ("NotGreaterGreater"                      , Right [ 0x0226B, 0x00338 ] ) -- MUCH GREATER THAN with slash
             , ("NotGreaterLess"                         , Left 0x02279               ) -- NEITHER GREATER-THAN NOR LESS-THAN
             , ("NotGreaterSlantEqual"                   , Right [ 0x02A7E, 0x00338 ] ) -- GREATER-THAN OR SLANTED EQUAL TO with slash
             , ("NotGreaterTilde"                        , Left 0x02275               ) -- NEITHER GREATER-THAN NOR EQUIVALENT TO
             , ("NotHumpDownHump"                        , Right [ 0x0224E, 0x00338 ] ) -- GEOMETRICALLY EQUIVALENT TO with slash
             , ("NotHumpEqual"                           , Right [ 0x0224F, 0x00338 ] ) -- DIFFERENCE BETWEEN with slash
             , ("NotLeftTriangle"                        , Left 0x022EA               ) -- NOT NORMAL SUBGROUP OF
             , ("NotLeftTriangleBar"                     , Right [ 0x029CF, 0x00338 ] ) -- LEFT TRIANGLE BESIDE VERTICAL BAR with slash
             , ("NotLeftTriangleEqual"                   , Left 0x022EC               ) -- NOT NORMAL SUBGROUP OF OR EQUAL TO
             , ("NotLess"                                , Left 0x0226E               ) -- NOT LESS-THAN
             , ("NotLessEqual"                           , Left 0x02270               ) -- NEITHER LESS-THAN NOR EQUAL TO
             , ("NotLessGreater"                         , Left 0x02278               ) -- NEITHER LESS-THAN NOR GREATER-THAN
             , ("NotLessLess"                            , Right [ 0x0226A, 0x00338 ] ) -- MUCH LESS THAN with slash
             , ("NotLessSlantEqual"                      , Right [ 0x02A7D, 0x00338 ] ) -- LESS-THAN OR SLANTED EQUAL TO with slash
             , ("NotLessTilde"                           , Left 0x02274               ) -- NEITHER LESS-THAN NOR EQUIVALENT TO
             , ("NotNestedGreaterGreater"                , Right [ 0x02AA2, 0x00338 ] ) -- DOUBLE NESTED GREATER-THAN with slash
             , ("NotNestedLessLess"                      , Right [ 0x02AA1, 0x00338 ] ) -- DOUBLE NESTED LESS-THAN with slash
             , ("NotPrecedes"                            , Left 0x02280               ) -- DOES NOT PRECEDE
             , ("NotPrecedesEqual"                       , Right [ 0x02AAF, 0x00338 ] ) -- PRECEDES ABOVE SINGLE-LINE EQUALS SIGN with slash
             , ("NotPrecedesSlantEqual"                  , Left 0x022E0               ) -- DOES NOT PRECEDE OR EQUAL
             , ("NotReverseElement"                      , Left 0x0220C               ) -- DOES NOT CONTAIN AS MEMBER
             , ("NotRightTriangle"                       , Left 0x022EB               ) -- DOES NOT CONTAIN AS NORMAL SUBGROUP
             , ("NotRightTriangleBar"                    , Right [ 0x029D0, 0x00338 ] ) -- VERTICAL BAR BESIDE RIGHT TRIANGLE with slash
             , ("NotRightTriangleEqual"                  , Left 0x022ED               ) -- DOES NOT CONTAIN AS NORMAL SUBGROUP OR EQUAL
             , ("NotSquareSubset"                        , Right [ 0x0228F, 0x00338 ] ) -- SQUARE IMAGE OF with slash
             , ("NotSquareSubsetEqual"                   , Left 0x022E2               ) -- NOT SQUARE IMAGE OF OR EQUAL TO
             , ("NotSquareSuperset"                      , Right [ 0x02290, 0x00338 ] ) -- SQUARE ORIGINAL OF with slash
             , ("NotSquareSupersetEqual"                 , Left 0x022E3               ) -- NOT SQUARE ORIGINAL OF OR EQUAL TO
             , ("NotSubset"                              , Right [ 0x02282, 0x020D2 ] ) -- SUBSET OF with vertical line
             , ("NotSubsetEqual"                         , Left 0x02288               ) -- NEITHER A SUBSET OF NOR EQUAL TO
             , ("NotSucceeds"                            , Left 0x02281               ) -- DOES NOT SUCCEED
             , ("NotSucceedsEqual"                       , Right [ 0x02AB0, 0x00338 ] ) -- SUCCEEDS ABOVE SINGLE-LINE EQUALS SIGN with slash
             , ("NotSucceedsSlantEqual"                  , Left 0x022E1               ) -- DOES NOT SUCCEED OR EQUAL
             , ("NotSucceedsTilde"                       , Right [ 0x0227F, 0x00338 ] ) -- SUCCEEDS OR EQUIVALENT TO with slash
             , ("NotSuperset"                            , Right [ 0x02283, 0x020D2 ] ) -- SUPERSET OF with vertical line
             , ("NotSupersetEqual"                       , Left 0x02289               ) -- NEITHER A SUPERSET OF NOR EQUAL TO
             , ("NotTilde"                               , Left 0x02241               ) -- NOT TILDE
             , ("NotTildeEqual"                          , Left 0x02244               ) -- NOT ASYMPTOTICALLY EQUAL TO
             , ("NotTildeFullEqual"                      , Left 0x02247               ) -- NEITHER APPROXIMATELY NOR ACTUALLY EQUAL TO
             , ("NotTildeTilde"                          , Left 0x02249               ) -- NOT ALMOST EQUAL TO
             , ("NotVerticalBar"                         , Left 0x02224               ) -- DOES NOT DIVIDE
             , ("Nscr"                                   , Left 0x1D4A9               ) -- MATHEMATICAL SCRIPT CAPITAL N
             , ("Ntilde"                                 , Left 0x000D1               ) -- LATIN CAPITAL LETTER N WITH TILDE
             , ("Nu"                                     , Left 0x0039D               ) -- GREEK CAPITAL LETTER NU
             , ("OElig"                                  , Left 0x00152               ) -- LATIN CAPITAL LIGATURE OE
             , ("OHacgr"                                 , Left 0x0038F               ) -- GREEK CAPITAL LETTER OMEGA WITH TONOS
             , ("OHgr"                                   , Left 0x003A9               ) -- GREEK CAPITAL LETTER OMEGA
             , ("Oacgr"                                  , Left 0x0038C               ) -- GREEK CAPITAL LETTER OMICRON WITH TONOS
             , ("Oacute"                                 , Left 0x000D3               ) -- LATIN CAPITAL LETTER O WITH ACUTE
             , ("Ocirc"                                  , Left 0x000D4               ) -- LATIN CAPITAL LETTER O WITH CIRCUMFLEX
             , ("Ocy"                                    , Left 0x0041E               ) -- CYRILLIC CAPITAL LETTER O
             , ("Odblac"                                 , Left 0x00150               ) -- LATIN CAPITAL LETTER O WITH DOUBLE ACUTE
             , ("Ofr"                                    , Left 0x1D512               ) -- MATHEMATICAL FRAKTUR CAPITAL O
             , ("Ogr"                                    , Left 0x0039F               ) -- GREEK CAPITAL LETTER OMICRON
             , ("Ograve"                                 , Left 0x000D2               ) -- LATIN CAPITAL LETTER O WITH GRAVE
             , ("Omacr"                                  , Left 0x0014C               ) -- LATIN CAPITAL LETTER O WITH MACRON
             , ("Omega"                                  , Left 0x003A9               ) -- GREEK CAPITAL LETTER OMEGA
             , ("Omicron"                                , Left 0x0039F               ) -- GREEK CAPITAL LETTER OMICRON
             , ("Oopf"                                   , Left 0x1D546               ) -- MATHEMATICAL DOUBLE-STRUCK CAPITAL O
             , ("OpenCurlyDoubleQuote"                   , Left 0x0201C               ) -- LEFT DOUBLE QUOTATION MARK
             , ("OpenCurlyQuote"                         , Left 0x02018               ) -- LEFT SINGLE QUOTATION MARK
             , ("Or"                                     , Left 0x02A54               ) -- DOUBLE LOGICAL OR
             , ("Oscr"                                   , Left 0x1D4AA               ) -- MATHEMATICAL SCRIPT CAPITAL O
             , ("Oslash"                                 , Left 0x000D8               ) -- LATIN CAPITAL LETTER O WITH STROKE
             , ("Otilde"                                 , Left 0x000D5               ) -- LATIN CAPITAL LETTER O WITH TILDE
             , ("Otimes"                                 , Left 0x02A37               ) -- MULTIPLICATION SIGN IN DOUBLE CIRCLE
             , ("Ouml"                                   , Left 0x000D6               ) -- LATIN CAPITAL LETTER O WITH DIAERESIS
             , ("OverBar"                                , Left 0x0203E               ) -- OVERLINE
             , ("OverBrace"                              , Left 0x023DE               ) -- TOP CURLY BRACKET
             , ("OverBracket"                            , Left 0x023B4               ) -- TOP SQUARE BRACKET
             , ("OverParenthesis"                        , Left 0x023DC               ) -- TOP PARENTHESIS
             , ("PHgr"                                   , Left 0x003A6               ) -- GREEK CAPITAL LETTER PHI
             , ("PSgr"                                   , Left 0x003A8               ) -- GREEK CAPITAL LETTER PSI
             , ("PartialD"                               , Left 0x02202               ) -- PARTIAL DIFFERENTIAL
             , ("Pcy"                                    , Left 0x0041F               ) -- CYRILLIC CAPITAL LETTER PE
             , ("Pfr"                                    , Left 0x1D513               ) -- MATHEMATICAL FRAKTUR CAPITAL P
             , ("Pgr"                                    , Left 0x003A0               ) -- GREEK CAPITAL LETTER PI
             , ("Phi"                                    , Left 0x003A6               ) -- GREEK CAPITAL LETTER PHI
             , ("Pi"                                     , Left 0x003A0               ) -- GREEK CAPITAL LETTER PI
             , ("PlusMinus"                              , Left 0x000B1               ) -- PLUS-MINUS SIGN
             , ("Poincareplane"                          , Left 0x0210C               ) -- BLACK-LETTER CAPITAL H
             , ("Popf"                                   , Left 0x02119               ) -- DOUBLE-STRUCK CAPITAL P
             , ("Pr"                                     , Left 0x02ABB               ) -- DOUBLE PRECEDES
             , ("Precedes"                               , Left 0x0227A               ) -- PRECEDES
             , ("PrecedesEqual"                          , Left 0x02AAF               ) -- PRECEDES ABOVE SINGLE-LINE EQUALS SIGN
             , ("PrecedesSlantEqual"                     , Left 0x0227C               ) -- PRECEDES OR EQUAL TO
             , ("PrecedesTilde"                          , Left 0x0227E               ) -- PRECEDES OR EQUIVALENT TO
             , ("Prime"                                  , Left 0x02033               ) -- DOUBLE PRIME
             , ("Product"                                , Left 0x0220F               ) -- N-ARY PRODUCT
             , ("Proportion"                             , Left 0x02237               ) -- PROPORTION
             , ("Proportional"                           , Left 0x0221D               ) -- PROPORTIONAL TO
             , ("Pscr"                                   , Left 0x1D4AB               ) -- MATHEMATICAL SCRIPT CAPITAL P
             , ("Psi"                                    , Left 0x003A8               ) -- GREEK CAPITAL LETTER PSI
             , ("QUOT"                                   , Left 0x00022               ) -- QUOTATION MARK
             , ("Qfr"                                    , Left 0x1D514               ) -- MATHEMATICAL FRAKTUR CAPITAL Q
             , ("Qopf"                                   , Left 0x0211A               ) -- DOUBLE-STRUCK CAPITAL Q
             , ("Qscr"                                   , Left 0x1D4AC               ) -- MATHEMATICAL SCRIPT CAPITAL Q
             , ("RBarr"                                  , Left 0x02910               ) -- RIGHTWARDS TWO-HEADED TRIPLE DASH ARROW
             , ("REG"                                    , Left 0x000AE               ) -- REGISTERED SIGN
             , ("Racute"                                 , Left 0x00154               ) -- LATIN CAPITAL LETTER R WITH ACUTE
             , ("Rang"                                   , Left 0x027EB               ) -- MATHEMATICAL RIGHT DOUBLE ANGLE BRACKET
             , ("Rarr"                                   , Left 0x021A0               ) -- RIGHTWARDS TWO HEADED ARROW
             , ("Rarrtl"                                 , Left 0x02916               ) -- RIGHTWARDS TWO-HEADED ARROW WITH TAIL
             , ("Rcaron"                                 , Left 0x00158               ) -- LATIN CAPITAL LETTER R WITH CARON
             , ("Rcedil"                                 , Left 0x00156               ) -- LATIN CAPITAL LETTER R WITH CEDILLA
             , ("Rcy"                                    , Left 0x00420               ) -- CYRILLIC CAPITAL LETTER ER
             , ("Re"                                     , Left 0x0211C               ) -- BLACK-LETTER CAPITAL R
             , ("ReverseElement"                         , Left 0x0220B               ) -- CONTAINS AS MEMBER
             , ("ReverseEquilibrium"                     , Left 0x021CB               ) -- LEFTWARDS HARPOON OVER RIGHTWARDS HARPOON
             , ("ReverseUpEquilibrium"                   , Left 0x0296F               ) -- DOWNWARDS HARPOON WITH BARB LEFT BESIDE UPWARDS HARPOON WITH BARB RIGHT
             , ("Rfr"                                    , Left 0x0211C               ) -- BLACK-LETTER CAPITAL R
             , ("Rgr"                                    , Left 0x003A1               ) -- GREEK CAPITAL LETTER RHO
             , ("Rho"                                    , Left 0x003A1               ) -- GREEK CAPITAL LETTER RHO
             , ("RightAngleBracket"                      , Left 0x027E9               ) -- MATHEMATICAL RIGHT ANGLE BRACKET
             , ("RightArrow"                             , Left 0x02192               ) -- RIGHTWARDS ARROW
             , ("RightArrowBar"                          , Left 0x021E5               ) -- RIGHTWARDS ARROW TO BAR
             , ("RightArrowLeftArrow"                    , Left 0x021C4               ) -- RIGHTWARDS ARROW OVER LEFTWARDS ARROW
             , ("RightCeiling"                           , Left 0x02309               ) -- RIGHT CEILING
             , ("RightDoubleBracket"                     , Left 0x027E7               ) -- MATHEMATICAL RIGHT WHITE SQUARE BRACKET
             , ("RightDownTeeVector"                     , Left 0x0295D               ) -- DOWNWARDS HARPOON WITH BARB RIGHT FROM BAR
             , ("RightDownVector"                        , Left 0x021C2               ) -- DOWNWARDS HARPOON WITH BARB RIGHTWARDS
             , ("RightDownVectorBar"                     , Left 0x02955               ) -- DOWNWARDS HARPOON WITH BARB RIGHT TO BAR
             , ("RightFloor"                             , Left 0x0230B               ) -- RIGHT FLOOR
             , ("RightTee"                               , Left 0x022A2               ) -- RIGHT TACK
             , ("RightTeeArrow"                          , Left 0x021A6               ) -- RIGHTWARDS ARROW FROM BAR
             , ("RightTeeVector"                         , Left 0x0295B               ) -- RIGHTWARDS HARPOON WITH BARB UP FROM BAR
             , ("RightTriangle"                          , Left 0x022B3               ) -- CONTAINS AS NORMAL SUBGROUP
             , ("RightTriangleBar"                       , Left 0x029D0               ) -- VERTICAL BAR BESIDE RIGHT TRIANGLE
             , ("RightTriangleEqual"                     , Left 0x022B5               ) -- CONTAINS AS NORMAL SUBGROUP OR EQUAL TO
             , ("RightUpDownVector"                      , Left 0x0294F               ) -- UP BARB RIGHT DOWN BARB RIGHT HARPOON
             , ("RightUpTeeVector"                       , Left 0x0295C               ) -- UPWARDS HARPOON WITH BARB RIGHT FROM BAR
             , ("RightUpVector"                          , Left 0x021BE               ) -- UPWARDS HARPOON WITH BARB RIGHTWARDS
             , ("RightUpVectorBar"                       , Left 0x02954               ) -- UPWARDS HARPOON WITH BARB RIGHT TO BAR
             , ("RightVector"                            , Left 0x021C0               ) -- RIGHTWARDS HARPOON WITH BARB UPWARDS
             , ("RightVectorBar"                         , Left 0x02953               ) -- RIGHTWARDS HARPOON WITH BARB UP TO BAR
             , ("Rightarrow"                             , Left 0x021D2               ) -- RIGHTWARDS DOUBLE ARROW
             , ("Ropf"                                   , Left 0x0211D               ) -- DOUBLE-STRUCK CAPITAL R
             , ("RoundImplies"                           , Left 0x02970               ) -- RIGHT DOUBLE ARROW WITH ROUNDED HEAD
             , ("Rrightarrow"                            , Left 0x021DB               ) -- RIGHTWARDS TRIPLE ARROW
             , ("Rscr"                                   , Left 0x0211B               ) -- SCRIPT CAPITAL R
             , ("Rsh"                                    , Left 0x021B1               ) -- UPWARDS ARROW WITH TIP RIGHTWARDS
             , ("RuleDelayed"                            , Left 0x029F4               ) -- RULE-DELAYED
             , ("SHCHcy"                                 , Left 0x00429               ) -- CYRILLIC CAPITAL LETTER SHCHA
             , ("SHcy"                                   , Left 0x00428               ) -- CYRILLIC CAPITAL LETTER SHA
             , ("SOFTcy"                                 , Left 0x0042C               ) -- CYRILLIC CAPITAL LETTER SOFT SIGN
             , ("Sacute"                                 , Left 0x0015A               ) -- LATIN CAPITAL LETTER S WITH ACUTE
             , ("Sc"                                     , Left 0x02ABC               ) -- DOUBLE SUCCEEDS
             , ("Scaron"                                 , Left 0x00160               ) -- LATIN CAPITAL LETTER S WITH CARON
             , ("Scedil"                                 , Left 0x0015E               ) -- LATIN CAPITAL LETTER S WITH CEDILLA
             , ("Scirc"                                  , Left 0x0015C               ) -- LATIN CAPITAL LETTER S WITH CIRCUMFLEX
             , ("Scy"                                    , Left 0x00421               ) -- CYRILLIC CAPITAL LETTER ES
             , ("Sfr"                                    , Left 0x1D516               ) -- MATHEMATICAL FRAKTUR CAPITAL S
             , ("Sgr"                                    , Left 0x003A3               ) -- GREEK CAPITAL LETTER SIGMA
             , ("ShortDownArrow"                         , Left 0x02193               ) -- DOWNWARDS ARROW
             , ("ShortLeftArrow"                         , Left 0x02190               ) -- LEFTWARDS ARROW
             , ("ShortRightArrow"                        , Left 0x02192               ) -- RIGHTWARDS ARROW
             , ("ShortUpArrow"                           , Left 0x02191               ) -- UPWARDS ARROW
             , ("Sigma"                                  , Left 0x003A3               ) -- GREEK CAPITAL LETTER SIGMA
             , ("SmallCircle"                            , Left 0x02218               ) -- RING OPERATOR
             , ("Sopf"                                   , Left 0x1D54A               ) -- MATHEMATICAL DOUBLE-STRUCK CAPITAL S
             , ("Sqrt"                                   , Left 0x0221A               ) -- SQUARE ROOT
             , ("Square"                                 , Left 0x025A1               ) -- WHITE SQUARE
             , ("SquareIntersection"                     , Left 0x02293               ) -- SQUARE CAP
             , ("SquareSubset"                           , Left 0x0228F               ) -- SQUARE IMAGE OF
             , ("SquareSubsetEqual"                      , Left 0x02291               ) -- SQUARE IMAGE OF OR EQUAL TO
             , ("SquareSuperset"                         , Left 0x02290               ) -- SQUARE ORIGINAL OF
             , ("SquareSupersetEqual"                    , Left 0x02292               ) -- SQUARE ORIGINAL OF OR EQUAL TO
             , ("SquareUnion"                            , Left 0x02294               ) -- SQUARE CUP
             , ("Sscr"                                   , Left 0x1D4AE               ) -- MATHEMATICAL SCRIPT CAPITAL S
             , ("Star"                                   , Left 0x022C6               ) -- STAR OPERATOR
             , ("Sub"                                    , Left 0x022D0               ) -- DOUBLE SUBSET
             , ("Subset"                                 , Left 0x022D0               ) -- DOUBLE SUBSET
             , ("SubsetEqual"                            , Left 0x02286               ) -- SUBSET OF OR EQUAL TO
             , ("Succeeds"                               , Left 0x0227B               ) -- SUCCEEDS
             , ("SucceedsEqual"                          , Left 0x02AB0               ) -- SUCCEEDS ABOVE SINGLE-LINE EQUALS SIGN
             , ("SucceedsSlantEqual"                     , Left 0x0227D               ) -- SUCCEEDS OR EQUAL TO
             , ("SucceedsTilde"                          , Left 0x0227F               ) -- SUCCEEDS OR EQUIVALENT TO
             , ("SuchThat"                               , Left 0x0220B               ) -- CONTAINS AS MEMBER
             , ("Sum"                                    , Left 0x02211               ) -- N-ARY SUMMATION
             , ("Sup"                                    , Left 0x022D1               ) -- DOUBLE SUPERSET
             , ("Superset"                               , Left 0x02283               ) -- SUPERSET OF
             , ("SupersetEqual"                          , Left 0x02287               ) -- SUPERSET OF OR EQUAL TO
             , ("Supset"                                 , Left 0x022D1               ) -- DOUBLE SUPERSET
             , ("THORN"                                  , Left 0x000DE               ) -- LATIN CAPITAL LETTER THORN
             , ("THgr"                                   , Left 0x00398               ) -- GREEK CAPITAL LETTER THETA
             , ("TRADE"                                  , Left 0x02122               ) -- TRADE MARK SIGN
             , ("TSHcy"                                  , Left 0x0040B               ) -- CYRILLIC CAPITAL LETTER TSHE
             , ("TScy"                                   , Left 0x00426               ) -- CYRILLIC CAPITAL LETTER TSE
             , ("Tab"                                    , Left 0x00009               ) -- CHARACTER TABULATION
             , ("Tau"                                    , Left 0x003A4               ) -- GREEK CAPITAL LETTER TAU
             , ("Tcaron"                                 , Left 0x00164               ) -- LATIN CAPITAL LETTER T WITH CARON
             , ("Tcedil"                                 , Left 0x00162               ) -- LATIN CAPITAL LETTER T WITH CEDILLA
             , ("Tcy"                                    , Left 0x00422               ) -- CYRILLIC CAPITAL LETTER TE
             , ("Tfr"                                    , Left 0x1D517               ) -- MATHEMATICAL FRAKTUR CAPITAL T
             , ("Tgr"                                    , Left 0x003A4               ) -- GREEK CAPITAL LETTER TAU
             , ("Therefore"                              , Left 0x02234               ) -- THEREFORE
             , ("Theta"                                  , Left 0x00398               ) -- GREEK CAPITAL LETTER THETA
             , ("ThickSpace"                             , Right [ 0x0205F, 0x0200A ] ) -- space of width 5/18 em
             , ("ThinSpace"                              , Left 0x02009               ) -- THIN SPACE
             , ("Tilde"                                  , Left 0x0223C               ) -- TILDE OPERATOR
             , ("TildeEqual"                             , Left 0x02243               ) -- ASYMPTOTICALLY EQUAL TO
             , ("TildeFullEqual"                         , Left 0x02245               ) -- APPROXIMATELY EQUAL TO
             , ("TildeTilde"                             , Left 0x02248               ) -- ALMOST EQUAL TO
             , ("Topf"                                   , Left 0x1D54B               ) -- MATHEMATICAL DOUBLE-STRUCK CAPITAL T
             , ("TripleDot"                              , Left 0x020DB               ) -- COMBINING THREE DOTS ABOVE
             , ("Tscr"                                   , Left 0x1D4AF               ) -- MATHEMATICAL SCRIPT CAPITAL T
             , ("Tstrok"                                 , Left 0x00166               ) -- LATIN CAPITAL LETTER T WITH STROKE
             , ("Uacgr"                                  , Left 0x0038E               ) -- GREEK CAPITAL LETTER UPSILON WITH TONOS
             , ("Uacute"                                 , Left 0x000DA               ) -- LATIN CAPITAL LETTER U WITH ACUTE
             , ("Uarr"                                   , Left 0x0219F               ) -- UPWARDS TWO HEADED ARROW
             , ("Uarrocir"                               , Left 0x02949               ) -- UPWARDS TWO-HEADED ARROW FROM SMALL CIRCLE
             , ("Ubrcy"                                  , Left 0x0040E               ) -- CYRILLIC CAPITAL LETTER SHORT U
             , ("Ubreve"                                 , Left 0x0016C               ) -- LATIN CAPITAL LETTER U WITH BREVE
             , ("Ucirc"                                  , Left 0x000DB               ) -- LATIN CAPITAL LETTER U WITH CIRCUMFLEX
             , ("Ucy"                                    , Left 0x00423               ) -- CYRILLIC CAPITAL LETTER U
             , ("Udblac"                                 , Left 0x00170               ) -- LATIN CAPITAL LETTER U WITH DOUBLE ACUTE
             , ("Udigr"                                  , Left 0x003AB               ) -- GREEK CAPITAL LETTER UPSILON WITH DIALYTIKA
             , ("Ufr"                                    , Left 0x1D518               ) -- MATHEMATICAL FRAKTUR CAPITAL U
             , ("Ugr"                                    , Left 0x003A5               ) -- GREEK CAPITAL LETTER UPSILON
             , ("Ugrave"                                 , Left 0x000D9               ) -- LATIN CAPITAL LETTER U WITH GRAVE
             , ("Umacr"                                  , Left 0x0016A               ) -- LATIN CAPITAL LETTER U WITH MACRON
             , ("UnderBar"                               , Left 0x0005F               ) -- LOW LINE
             , ("UnderBrace"                             , Left 0x023DF               ) -- BOTTOM CURLY BRACKET
             , ("UnderBracket"                           , Left 0x023B5               ) -- BOTTOM SQUARE BRACKET
             , ("UnderParenthesis"                       , Left 0x023DD               ) -- BOTTOM PARENTHESIS
             , ("Union"                                  , Left 0x022C3               ) -- N-ARY UNION
             , ("UnionPlus"                              , Left 0x0228E               ) -- MULTISET UNION
             , ("Uogon"                                  , Left 0x00172               ) -- LATIN CAPITAL LETTER U WITH OGONEK
             , ("Uopf"                                   , Left 0x1D54C               ) -- MATHEMATICAL DOUBLE-STRUCK CAPITAL U
             , ("UpArrow"                                , Left 0x02191               ) -- UPWARDS ARROW
             , ("UpArrowBar"                             , Left 0x02912               ) -- UPWARDS ARROW TO BAR
             , ("UpArrowDownArrow"                       , Left 0x021C5               ) -- UPWARDS ARROW LEFTWARDS OF DOWNWARDS ARROW
             , ("UpDownArrow"                            , Left 0x02195               ) -- UP DOWN ARROW
             , ("UpEquilibrium"                          , Left 0x0296E               ) -- UPWARDS HARPOON WITH BARB LEFT BESIDE DOWNWARDS HARPOON WITH BARB RIGHT
             , ("UpTee"                                  , Left 0x022A5               ) -- UP TACK
             , ("UpTeeArrow"                             , Left 0x021A5               ) -- UPWARDS ARROW FROM BAR
             , ("Uparrow"                                , Left 0x021D1               ) -- UPWARDS DOUBLE ARROW
             , ("Updownarrow"                            , Left 0x021D5               ) -- UP DOWN DOUBLE ARROW
             , ("UpperLeftArrow"                         , Left 0x02196               ) -- NORTH WEST ARROW
             , ("UpperRightArrow"                        , Left 0x02197               ) -- NORTH EAST ARROW
             , ("Upsi"                                   , Left 0x003D2               ) -- GREEK UPSILON WITH HOOK SYMBOL
             , ("Upsilon"                                , Left 0x003A5               ) -- GREEK CAPITAL LETTER UPSILON
             , ("Uring"                                  , Left 0x0016E               ) -- LATIN CAPITAL LETTER U WITH RING ABOVE
             , ("Uscr"                                   , Left 0x1D4B0               ) -- MATHEMATICAL SCRIPT CAPITAL U
             , ("Utilde"                                 , Left 0x00168               ) -- LATIN CAPITAL LETTER U WITH TILDE
             , ("Uuml"                                   , Left 0x000DC               ) -- LATIN CAPITAL LETTER U WITH DIAERESIS
             , ("VDash"                                  , Left 0x022AB               ) -- DOUBLE VERTICAL BAR DOUBLE RIGHT TURNSTILE
             , ("Vbar"                                   , Left 0x02AEB               ) -- DOUBLE UP TACK
             , ("Vcy"                                    , Left 0x00412               ) -- CYRILLIC CAPITAL LETTER VE
             , ("Vdash"                                  , Left 0x022A9               ) -- FORCES
             , ("Vdashl"                                 , Left 0x02AE6               ) -- LONG DASH FROM LEFT MEMBER OF DOUBLE VERTICAL
             , ("Vee"                                    , Left 0x022C1               ) -- N-ARY LOGICAL OR
             , ("Verbar"                                 , Left 0x02016               ) -- DOUBLE VERTICAL LINE
             , ("Vert"                                   , Left 0x02016               ) -- DOUBLE VERTICAL LINE
             , ("VerticalBar"                            , Left 0x02223               ) -- DIVIDES
             , ("VerticalLine"                           , Left 0x0007C               ) -- VERTICAL LINE
             , ("VerticalSeparator"                      , Left 0x02758               ) -- LIGHT VERTICAL BAR
             , ("VerticalTilde"                          , Left 0x02240               ) -- WREATH PRODUCT
             , ("VeryThinSpace"                          , Left 0x0200A               ) -- HAIR SPACE
             , ("Vfr"                                    , Left 0x1D519               ) -- MATHEMATICAL FRAKTUR CAPITAL V
             , ("Vopf"                                   , Left 0x1D54D               ) -- MATHEMATICAL DOUBLE-STRUCK CAPITAL V
             , ("Vscr"                                   , Left 0x1D4B1               ) -- MATHEMATICAL SCRIPT CAPITAL V
             , ("Vvdash"                                 , Left 0x022AA               ) -- TRIPLE VERTICAL BAR RIGHT TURNSTILE
             , ("Wcirc"                                  , Left 0x00174               ) -- LATIN CAPITAL LETTER W WITH CIRCUMFLEX
             , ("Wedge"                                  , Left 0x022C0               ) -- N-ARY LOGICAL AND
             , ("Wfr"                                    , Left 0x1D51A               ) -- MATHEMATICAL FRAKTUR CAPITAL W
             , ("Wopf"                                   , Left 0x1D54E               ) -- MATHEMATICAL DOUBLE-STRUCK CAPITAL W
             , ("Wscr"                                   , Left 0x1D4B2               ) -- MATHEMATICAL SCRIPT CAPITAL W
             , ("Xfr"                                    , Left 0x1D51B               ) -- MATHEMATICAL FRAKTUR CAPITAL X
             , ("Xgr"                                    , Left 0x0039E               ) -- GREEK CAPITAL LETTER XI
             , ("Xi"                                     , Left 0x0039E               ) -- GREEK CAPITAL LETTER XI
             , ("Xopf"                                   , Left 0x1D54F               ) -- MATHEMATICAL DOUBLE-STRUCK CAPITAL X
             , ("Xscr"                                   , Left 0x1D4B3               ) -- MATHEMATICAL SCRIPT CAPITAL X
             , ("YAcy"                                   , Left 0x0042F               ) -- CYRILLIC CAPITAL LETTER YA
             , ("YIcy"                                   , Left 0x00407               ) -- CYRILLIC CAPITAL LETTER YI
             , ("YUcy"                                   , Left 0x0042E               ) -- CYRILLIC CAPITAL LETTER YU
             , ("Yacute"                                 , Left 0x000DD               ) -- LATIN CAPITAL LETTER Y WITH ACUTE
             , ("Ycirc"                                  , Left 0x00176               ) -- LATIN CAPITAL LETTER Y WITH CIRCUMFLEX
             , ("Ycy"                                    , Left 0x0042B               ) -- CYRILLIC CAPITAL LETTER YERU
             , ("Yfr"                                    , Left 0x1D51C               ) -- MATHEMATICAL FRAKTUR CAPITAL Y
             , ("Yopf"                                   , Left 0x1D550               ) -- MATHEMATICAL DOUBLE-STRUCK CAPITAL Y
             , ("Yscr"                                   , Left 0x1D4B4               ) -- MATHEMATICAL SCRIPT CAPITAL Y
             , ("Yuml"                                   , Left 0x00178               ) -- LATIN CAPITAL LETTER Y WITH DIAERESIS
             , ("ZHcy"                                   , Left 0x00416               ) -- CYRILLIC CAPITAL LETTER ZHE
             , ("Zacute"                                 , Left 0x00179               ) -- LATIN CAPITAL LETTER Z WITH ACUTE
             , ("Zcaron"                                 , Left 0x0017D               ) -- LATIN CAPITAL LETTER Z WITH CARON
             , ("Zcy"                                    , Left 0x00417               ) -- CYRILLIC CAPITAL LETTER ZE
             , ("Zdot"                                   , Left 0x0017B               ) -- LATIN CAPITAL LETTER Z WITH DOT ABOVE
             , ("ZeroWidthSpace"                         , Left 0x0200B               ) -- ZERO WIDTH SPACE
             , ("Zeta"                                   , Left 0x00396               ) -- GREEK CAPITAL LETTER ZETA
             , ("Zfr"                                    , Left 0x02128               ) -- BLACK-LETTER CAPITAL Z
             , ("Zgr"                                    , Left 0x00396               ) -- GREEK CAPITAL LETTER ZETA
             , ("Zopf"                                   , Left 0x02124               ) -- DOUBLE-STRUCK CAPITAL Z
             , ("Zscr"                                   , Left 0x1D4B5               ) -- MATHEMATICAL SCRIPT CAPITAL Z
             , ("aacgr"                                  , Left 0x003AC               ) -- GREEK SMALL LETTER ALPHA WITH TONOS
             , ("aacute"                                 , Left 0x000E1               ) -- LATIN SMALL LETTER A WITH ACUTE
             , ("abreve"                                 , Left 0x00103               ) -- LATIN SMALL LETTER A WITH BREVE
             , ("ac"                                     , Left 0x0223E               ) -- INVERTED LAZY S
             , ("acE"                                    , Right [ 0x0223E, 0x00333 ] ) -- INVERTED LAZY S with double underline
             , ("acd"                                    , Left 0x0223F               ) -- SINE WAVE
             , ("acirc"                                  , Left 0x000E2               ) -- LATIN SMALL LETTER A WITH CIRCUMFLEX
             , ("acute"                                  , Left 0x000B4               ) -- ACUTE ACCENT
             , ("acy"                                    , Left 0x00430               ) -- CYRILLIC SMALL LETTER A
             , ("aelig"                                  , Left 0x000E6               ) -- LATIN SMALL LETTER AE
             , ("af"                                     , Left 0x02061               ) -- FUNCTION APPLICATION
             , ("afr"                                    , Left 0x1D51E               ) -- MATHEMATICAL FRAKTUR SMALL A
             , ("agr"                                    , Left 0x003B1               ) -- GREEK SMALL LETTER ALPHA
             , ("agrave"                                 , Left 0x000E0               ) -- LATIN SMALL LETTER A WITH GRAVE
             , ("alefsym"                                , Left 0x02135               ) -- ALEF SYMBOL
             , ("aleph"                                  , Left 0x02135               ) -- ALEF SYMBOL
             , ("alpha"                                  , Left 0x003B1               ) -- GREEK SMALL LETTER ALPHA
             , ("amacr"                                  , Left 0x00101               ) -- LATIN SMALL LETTER A WITH MACRON
             , ("amalg"                                  , Left 0x02A3F               ) -- AMALGAMATION OR COPRODUCT
             , ("amp"                                    , Left 0x00026               ) -- AMPERSAND
             , ("and"                                    , Left 0x02227               ) -- LOGICAL AND
             , ("andand"                                 , Left 0x02A55               ) -- TWO INTERSECTING LOGICAL AND
             , ("andd"                                   , Left 0x02A5C               ) -- LOGICAL AND WITH HORIZONTAL DASH
             , ("andslope"                               , Left 0x02A58               ) -- SLOPING LARGE AND
             , ("andv"                                   , Left 0x02A5A               ) -- LOGICAL AND WITH MIDDLE STEM
             , ("ang"                                    , Left 0x02220               ) -- ANGLE
             , ("ange"                                   , Left 0x029A4               ) -- ANGLE WITH UNDERBAR
             , ("angle"                                  , Left 0x02220               ) -- ANGLE
             , ("angmsd"                                 , Left 0x02221               ) -- MEASURED ANGLE
             , ("angmsdaa"                               , Left 0x029A8               ) -- MEASURED ANGLE WITH OPEN ARM ENDING IN ARROW POINTING UP AND RIGHT
             , ("angmsdab"                               , Left 0x029A9               ) -- MEASURED ANGLE WITH OPEN ARM ENDING IN ARROW POINTING UP AND LEFT
             , ("angmsdac"                               , Left 0x029AA               ) -- MEASURED ANGLE WITH OPEN ARM ENDING IN ARROW POINTING DOWN AND RIGHT
             , ("angmsdad"                               , Left 0x029AB               ) -- MEASURED ANGLE WITH OPEN ARM ENDING IN ARROW POINTING DOWN AND LEFT
             , ("angmsdae"                               , Left 0x029AC               ) -- MEASURED ANGLE WITH OPEN ARM ENDING IN ARROW POINTING RIGHT AND UP
             , ("angmsdaf"                               , Left 0x029AD               ) -- MEASURED ANGLE WITH OPEN ARM ENDING IN ARROW POINTING LEFT AND UP
             , ("angmsdag"                               , Left 0x029AE               ) -- MEASURED ANGLE WITH OPEN ARM ENDING IN ARROW POINTING RIGHT AND DOWN
             , ("angmsdah"                               , Left 0x029AF               ) -- MEASURED ANGLE WITH OPEN ARM ENDING IN ARROW POINTING LEFT AND DOWN
             , ("angrt"                                  , Left 0x0221F               ) -- RIGHT ANGLE
             , ("angrtvb"                                , Left 0x022BE               ) -- RIGHT ANGLE WITH ARC
             , ("angrtvbd"                               , Left 0x0299D               ) -- MEASURED RIGHT ANGLE WITH DOT
             , ("angsph"                                 , Left 0x02222               ) -- SPHERICAL ANGLE
             , ("angst"                                  , Left 0x000C5               ) -- LATIN CAPITAL LETTER A WITH RING ABOVE
             , ("angzarr"                                , Left 0x0237C               ) -- RIGHT ANGLE WITH DOWNWARDS ZIGZAG ARROW
             , ("aogon"                                  , Left 0x00105               ) -- LATIN SMALL LETTER A WITH OGONEK
             , ("aopf"                                   , Left 0x1D552               ) -- MATHEMATICAL DOUBLE-STRUCK SMALL A
             , ("ap"                                     , Left 0x02248               ) -- ALMOST EQUAL TO
             , ("apE"                                    , Left 0x02A70               ) -- APPROXIMATELY EQUAL OR EQUAL TO
             , ("apacir"                                 , Left 0x02A6F               ) -- ALMOST EQUAL TO WITH CIRCUMFLEX ACCENT
             , ("ape"                                    , Left 0x0224A               ) -- ALMOST EQUAL OR EQUAL TO
             , ("apid"                                   , Left 0x0224B               ) -- TRIPLE TILDE
             , ("apos"                                   , Left 0x00027               ) -- APOSTROPHE
             , ("approx"                                 , Left 0x02248               ) -- ALMOST EQUAL TO
             , ("approxeq"                               , Left 0x0224A               ) -- ALMOST EQUAL OR EQUAL TO
             , ("aring"                                  , Left 0x000E5               ) -- LATIN SMALL LETTER A WITH RING ABOVE
             , ("ascr"                                   , Left 0x1D4B6               ) -- MATHEMATICAL SCRIPT SMALL A
             , ("ast"                                    , Left 0x0002A               ) -- ASTERISK
             , ("asymp"                                  , Left 0x02248               ) -- ALMOST EQUAL TO
             , ("asympeq"                                , Left 0x0224D               ) -- EQUIVALENT TO
             , ("atilde"                                 , Left 0x000E3               ) -- LATIN SMALL LETTER A WITH TILDE
             , ("auml"                                   , Left 0x000E4               ) -- LATIN SMALL LETTER A WITH DIAERESIS
             , ("awconint"                               , Left 0x02233               ) -- ANTICLOCKWISE CONTOUR INTEGRAL
             , ("awint"                                  , Left 0x02A11               ) -- ANTICLOCKWISE INTEGRATION
             , ("b.Delta"                                , Left 0x1D6AB               ) -- MATHEMATICAL BOLD CAPITAL DELTA
             , ("b.Gamma"                                , Left 0x1D6AA               ) -- MATHEMATICAL BOLD CAPITAL GAMMA
             , ("b.Gammad"                               , Left 0x1D7CA               ) -- MATHEMATICAL BOLD CAPITAL DIGAMMA
             , ("b.Lambda"                               , Left 0x1D6B2               ) -- MATHEMATICAL BOLD CAPITAL LAMDA
             , ("b.Omega"                                , Left 0x1D6C0               ) -- MATHEMATICAL BOLD CAPITAL OMEGA
             , ("b.Phi"                                  , Left 0x1D6BD               ) -- MATHEMATICAL BOLD CAPITAL PHI
             , ("b.Pi"                                   , Left 0x1D6B7               ) -- MATHEMATICAL BOLD CAPITAL PI
             , ("b.Psi"                                  , Left 0x1D6BF               ) -- MATHEMATICAL BOLD CAPITAL PSI
             , ("b.Sigma"                                , Left 0x1D6BA               ) -- MATHEMATICAL BOLD CAPITAL SIGMA
             , ("b.Theta"                                , Left 0x1D6AF               ) -- MATHEMATICAL BOLD CAPITAL THETA
             , ("b.Upsi"                                 , Left 0x1D6BC               ) -- MATHEMATICAL BOLD CAPITAL UPSILON
             , ("b.Xi"                                   , Left 0x1D6B5               ) -- MATHEMATICAL BOLD CAPITAL XI
             , ("b.alpha"                                , Left 0x1D6C2               ) -- MATHEMATICAL BOLD SMALL ALPHA
             , ("b.beta"                                 , Left 0x1D6C3               ) -- MATHEMATICAL BOLD SMALL BETA
             , ("b.chi"                                  , Left 0x1D6D8               ) -- MATHEMATICAL BOLD SMALL CHI
             , ("b.delta"                                , Left 0x1D6C5               ) -- MATHEMATICAL BOLD SMALL DELTA
             , ("b.epsi"                                 , Left 0x1D6C6               ) -- MATHEMATICAL BOLD SMALL EPSILON
             , ("b.epsiv"                                , Left 0x1D6DC               ) -- MATHEMATICAL BOLD EPSILON SYMBOL
             , ("b.eta"                                  , Left 0x1D6C8               ) -- MATHEMATICAL BOLD SMALL ETA
             , ("b.gamma"                                , Left 0x1D6C4               ) -- MATHEMATICAL BOLD SMALL GAMMA
             , ("b.gammad"                               , Left 0x1D7CB               ) -- MATHEMATICAL BOLD SMALL DIGAMMA
             , ("b.iota"                                 , Left 0x1D6CA               ) -- MATHEMATICAL BOLD SMALL IOTA
             , ("b.kappa"                                , Left 0x1D6CB               ) -- MATHEMATICAL BOLD SMALL KAPPA
             , ("b.kappav"                               , Left 0x1D6DE               ) -- MATHEMATICAL BOLD KAPPA SYMBOL
             , ("b.lambda"                               , Left 0x1D6CC               ) -- MATHEMATICAL BOLD SMALL LAMDA
             , ("b.mu"                                   , Left 0x1D6CD               ) -- MATHEMATICAL BOLD SMALL MU
             , ("b.nu"                                   , Left 0x1D6CE               ) -- MATHEMATICAL BOLD SMALL NU
             , ("b.omega"                                , Left 0x1D6DA               ) -- MATHEMATICAL BOLD SMALL OMEGA
             , ("b.phi"                                  , Left 0x1D6D7               ) -- MATHEMATICAL BOLD SMALL PHI
             , ("b.phiv"                                 , Left 0x1D6DF               ) -- MATHEMATICAL BOLD PHI SYMBOL
             , ("b.pi"                                   , Left 0x1D6D1               ) -- MATHEMATICAL BOLD SMALL PI
             , ("b.piv"                                  , Left 0x1D6E1               ) -- MATHEMATICAL BOLD PI SYMBOL
             , ("b.psi"                                  , Left 0x1D6D9               ) -- MATHEMATICAL BOLD SMALL PSI
             , ("b.rho"                                  , Left 0x1D6D2               ) -- MATHEMATICAL BOLD SMALL RHO
             , ("b.rhov"                                 , Left 0x1D6E0               ) -- MATHEMATICAL BOLD RHO SYMBOL
             , ("b.sigma"                                , Left 0x1D6D4               ) -- MATHEMATICAL BOLD SMALL SIGMA
             , ("b.sigmav"                               , Left 0x1D6D3               ) -- MATHEMATICAL BOLD SMALL FINAL SIGMA
             , ("b.tau"                                  , Left 0x1D6D5               ) -- MATHEMATICAL BOLD SMALL TAU
             , ("b.thetas"                               , Left 0x1D6C9               ) -- MATHEMATICAL BOLD SMALL THETA
             , ("b.thetav"                               , Left 0x1D6DD               ) -- MATHEMATICAL BOLD THETA SYMBOL
             , ("b.upsi"                                 , Left 0x1D6D6               ) -- MATHEMATICAL BOLD SMALL UPSILON
             , ("b.xi"                                   , Left 0x1D6CF               ) -- MATHEMATICAL BOLD SMALL XI
             , ("b.zeta"                                 , Left 0x1D6C7               ) -- MATHEMATICAL BOLD SMALL ZETA
             , ("bNot"                                   , Left 0x02AED               ) -- REVERSED DOUBLE STROKE NOT SIGN
             , ("backcong"                               , Left 0x0224C               ) -- ALL EQUAL TO
             , ("backepsilon"                            , Left 0x003F6               ) -- GREEK REVERSED LUNATE EPSILON SYMBOL
             , ("backprime"                              , Left 0x02035               ) -- REVERSED PRIME
             , ("backsim"                                , Left 0x0223D               ) -- REVERSED TILDE
             , ("backsimeq"                              , Left 0x022CD               ) -- REVERSED TILDE EQUALS
             , ("barvee"                                 , Left 0x022BD               ) -- NOR
             , ("barwed"                                 , Left 0x02305               ) -- PROJECTIVE
             , ("barwedge"                               , Left 0x02305               ) -- PROJECTIVE
             , ("bbrk"                                   , Left 0x023B5               ) -- BOTTOM SQUARE BRACKET
             , ("bbrktbrk"                               , Left 0x023B6               ) -- BOTTOM SQUARE BRACKET OVER TOP SQUARE BRACKET
             , ("bcong"                                  , Left 0x0224C               ) -- ALL EQUAL TO
             , ("bcy"                                    , Left 0x00431               ) -- CYRILLIC SMALL LETTER BE
             , ("bdquo"                                  , Left 0x0201E               ) -- DOUBLE LOW-9 QUOTATION MARK
             , ("becaus"                                 , Left 0x02235               ) -- BECAUSE
             , ("because"                                , Left 0x02235               ) -- BECAUSE
             , ("bemptyv"                                , Left 0x029B0               ) -- REVERSED EMPTY SET
             , ("bepsi"                                  , Left 0x003F6               ) -- GREEK REVERSED LUNATE EPSILON SYMBOL
             , ("bernou"                                 , Left 0x0212C               ) -- SCRIPT CAPITAL B
             , ("beta"                                   , Left 0x003B2               ) -- GREEK SMALL LETTER BETA
             , ("beth"                                   , Left 0x02136               ) -- BET SYMBOL
             , ("between"                                , Left 0x0226C               ) -- BETWEEN
             , ("bfr"                                    , Left 0x1D51F               ) -- MATHEMATICAL FRAKTUR SMALL B
             , ("bgr"                                    , Left 0x003B2               ) -- GREEK SMALL LETTER BETA
             , ("bigcap"                                 , Left 0x022C2               ) -- N-ARY INTERSECTION
             , ("bigcirc"                                , Left 0x025EF               ) -- LARGE CIRCLE
             , ("bigcup"                                 , Left 0x022C3               ) -- N-ARY UNION
             , ("bigodot"                                , Left 0x02A00               ) -- N-ARY CIRCLED DOT OPERATOR
             , ("bigoplus"                               , Left 0x02A01               ) -- N-ARY CIRCLED PLUS OPERATOR
             , ("bigotimes"                              , Left 0x02A02               ) -- N-ARY CIRCLED TIMES OPERATOR
             , ("bigsqcup"                               , Left 0x02A06               ) -- N-ARY SQUARE UNION OPERATOR
             , ("bigstar"                                , Left 0x02605               ) -- BLACK STAR
             , ("bigtriangledown"                        , Left 0x025BD               ) -- WHITE DOWN-POINTING TRIANGLE
             , ("bigtriangleup"                          , Left 0x025B3               ) -- WHITE UP-POINTING TRIANGLE
             , ("biguplus"                               , Left 0x02A04               ) -- N-ARY UNION OPERATOR WITH PLUS
             , ("bigvee"                                 , Left 0x022C1               ) -- N-ARY LOGICAL OR
             , ("bigwedge"                               , Left 0x022C0               ) -- N-ARY LOGICAL AND
             , ("bkarow"                                 , Left 0x0290D               ) -- RIGHTWARDS DOUBLE DASH ARROW
             , ("blacklozenge"                           , Left 0x029EB               ) -- BLACK LOZENGE
             , ("blacksquare"                            , Left 0x025AA               ) -- BLACK SMALL SQUARE
             , ("blacktriangle"                          , Left 0x025B4               ) -- BLACK UP-POINTING SMALL TRIANGLE
             , ("blacktriangledown"                      , Left 0x025BE               ) -- BLACK DOWN-POINTING SMALL TRIANGLE
             , ("blacktriangleleft"                      , Left 0x025C2               ) -- BLACK LEFT-POINTING SMALL TRIANGLE
             , ("blacktriangleright"                     , Left 0x025B8               ) -- BLACK RIGHT-POINTING SMALL TRIANGLE
             , ("blank"                                  , Left 0x02423               ) -- OPEN BOX
             , ("blk12"                                  , Left 0x02592               ) -- MEDIUM SHADE
             , ("blk14"                                  , Left 0x02591               ) -- LIGHT SHADE
             , ("blk34"                                  , Left 0x02593               ) -- DARK SHADE
             , ("block"                                  , Left 0x02588               ) -- FULL BLOCK
             , ("bne"                                    , Right [ 0x0003D, 0x020E5 ] ) -- EQUALS SIGN with reverse slash
             , ("bnequiv"                                , Right [ 0x02261, 0x020E5 ] ) -- IDENTICAL TO with reverse slash
             , ("bnot"                                   , Left 0x02310               ) -- REVERSED NOT SIGN
             , ("bopf"                                   , Left 0x1D553               ) -- MATHEMATICAL DOUBLE-STRUCK SMALL B
             , ("bot"                                    , Left 0x022A5               ) -- UP TACK
             , ("bottom"                                 , Left 0x022A5               ) -- UP TACK
             , ("bowtie"                                 , Left 0x022C8               ) -- BOWTIE
             , ("boxDL"                                  , Left 0x02557               ) -- BOX DRAWINGS DOUBLE DOWN AND LEFT
             , ("boxDR"                                  , Left 0x02554               ) -- BOX DRAWINGS DOUBLE DOWN AND RIGHT
             , ("boxDl"                                  , Left 0x02556               ) -- BOX DRAWINGS DOWN DOUBLE AND LEFT SINGLE
             , ("boxDr"                                  , Left 0x02553               ) -- BOX DRAWINGS DOWN DOUBLE AND RIGHT SINGLE
             , ("boxH"                                   , Left 0x02550               ) -- BOX DRAWINGS DOUBLE HORIZONTAL
             , ("boxHD"                                  , Left 0x02566               ) -- BOX DRAWINGS DOUBLE DOWN AND HORIZONTAL
             , ("boxHU"                                  , Left 0x02569               ) -- BOX DRAWINGS DOUBLE UP AND HORIZONTAL
             , ("boxHd"                                  , Left 0x02564               ) -- BOX DRAWINGS DOWN SINGLE AND HORIZONTAL DOUBLE
             , ("boxHu"                                  , Left 0x02567               ) -- BOX DRAWINGS UP SINGLE AND HORIZONTAL DOUBLE
             , ("boxUL"                                  , Left 0x0255D               ) -- BOX DRAWINGS DOUBLE UP AND LEFT
             , ("boxUR"                                  , Left 0x0255A               ) -- BOX DRAWINGS DOUBLE UP AND RIGHT
             , ("boxUl"                                  , Left 0x0255C               ) -- BOX DRAWINGS UP DOUBLE AND LEFT SINGLE
             , ("boxUr"                                  , Left 0x02559               ) -- BOX DRAWINGS UP DOUBLE AND RIGHT SINGLE
             , ("boxV"                                   , Left 0x02551               ) -- BOX DRAWINGS DOUBLE VERTICAL
             , ("boxVH"                                  , Left 0x0256C               ) -- BOX DRAWINGS DOUBLE VERTICAL AND HORIZONTAL
             , ("boxVL"                                  , Left 0x02563               ) -- BOX DRAWINGS DOUBLE VERTICAL AND LEFT
             , ("boxVR"                                  , Left 0x02560               ) -- BOX DRAWINGS DOUBLE VERTICAL AND RIGHT
             , ("boxVh"                                  , Left 0x0256B               ) -- BOX DRAWINGS VERTICAL DOUBLE AND HORIZONTAL SINGLE
             , ("boxVl"                                  , Left 0x02562               ) -- BOX DRAWINGS VERTICAL DOUBLE AND LEFT SINGLE
             , ("boxVr"                                  , Left 0x0255F               ) -- BOX DRAWINGS VERTICAL DOUBLE AND RIGHT SINGLE
             , ("boxbox"                                 , Left 0x029C9               ) -- TWO JOINED SQUARES
             , ("boxdL"                                  , Left 0x02555               ) -- BOX DRAWINGS DOWN SINGLE AND LEFT DOUBLE
             , ("boxdR"                                  , Left 0x02552               ) -- BOX DRAWINGS DOWN SINGLE AND RIGHT DOUBLE
             , ("boxdl"                                  , Left 0x02510               ) -- BOX DRAWINGS LIGHT DOWN AND LEFT
             , ("boxdr"                                  , Left 0x0250C               ) -- BOX DRAWINGS LIGHT DOWN AND RIGHT
             , ("boxh"                                   , Left 0x02500               ) -- BOX DRAWINGS LIGHT HORIZONTAL
             , ("boxhD"                                  , Left 0x02565               ) -- BOX DRAWINGS DOWN DOUBLE AND HORIZONTAL SINGLE
             , ("boxhU"                                  , Left 0x02568               ) -- BOX DRAWINGS UP DOUBLE AND HORIZONTAL SINGLE
             , ("boxhd"                                  , Left 0x0252C               ) -- BOX DRAWINGS LIGHT DOWN AND HORIZONTAL
             , ("boxhu"                                  , Left 0x02534               ) -- BOX DRAWINGS LIGHT UP AND HORIZONTAL
             , ("boxminus"                               , Left 0x0229F               ) -- SQUARED MINUS
             , ("boxplus"                                , Left 0x0229E               ) -- SQUARED PLUS
             , ("boxtimes"                               , Left 0x022A0               ) -- SQUARED TIMES
             , ("boxuL"                                  , Left 0x0255B               ) -- BOX DRAWINGS UP SINGLE AND LEFT DOUBLE
             , ("boxuR"                                  , Left 0x02558               ) -- BOX DRAWINGS UP SINGLE AND RIGHT DOUBLE
             , ("boxul"                                  , Left 0x02518               ) -- BOX DRAWINGS LIGHT UP AND LEFT
             , ("boxur"                                  , Left 0x02514               ) -- BOX DRAWINGS LIGHT UP AND RIGHT
             , ("boxv"                                   , Left 0x02502               ) -- BOX DRAWINGS LIGHT VERTICAL
             , ("boxvH"                                  , Left 0x0256A               ) -- BOX DRAWINGS VERTICAL SINGLE AND HORIZONTAL DOUBLE
             , ("boxvL"                                  , Left 0x02561               ) -- BOX DRAWINGS VERTICAL SINGLE AND LEFT DOUBLE
             , ("boxvR"                                  , Left 0x0255E               ) -- BOX DRAWINGS VERTICAL SINGLE AND RIGHT DOUBLE
             , ("boxvh"                                  , Left 0x0253C               ) -- BOX DRAWINGS LIGHT VERTICAL AND HORIZONTAL
             , ("boxvl"                                  , Left 0x02524               ) -- BOX DRAWINGS LIGHT VERTICAL AND LEFT
             , ("boxvr"                                  , Left 0x0251C               ) -- BOX DRAWINGS LIGHT VERTICAL AND RIGHT
             , ("bprime"                                 , Left 0x02035               ) -- REVERSED PRIME
             , ("breve"                                  , Left 0x002D8               ) -- BREVE
             , ("brvbar"                                 , Left 0x000A6               ) -- BROKEN BAR
             , ("bscr"                                   , Left 0x1D4B7               ) -- MATHEMATICAL SCRIPT SMALL B
             , ("bsemi"                                  , Left 0x0204F               ) -- REVERSED SEMICOLON
             , ("bsim"                                   , Left 0x0223D               ) -- REVERSED TILDE
             , ("bsime"                                  , Left 0x022CD               ) -- REVERSED TILDE EQUALS
             , ("bsol"                                   , Left 0x0005C               ) -- REVERSE SOLIDUS
             , ("bsolb"                                  , Left 0x029C5               ) -- SQUARED FALLING DIAGONAL SLASH
             , ("bsolhsub"                               , Left 0x027C8               ) -- REVERSE SOLIDUS PRECEDING SUBSET
             , ("bull"                                   , Left 0x02022               ) -- BULLET
             , ("bullet"                                 , Left 0x02022               ) -- BULLET
             , ("bump"                                   , Left 0x0224E               ) -- GEOMETRICALLY EQUIVALENT TO
             , ("bumpE"                                  , Left 0x02AAE               ) -- EQUALS SIGN WITH BUMPY ABOVE
             , ("bumpe"                                  , Left 0x0224F               ) -- DIFFERENCE BETWEEN
             , ("bumpeq"                                 , Left 0x0224F               ) -- DIFFERENCE BETWEEN
             , ("cacute"                                 , Left 0x00107               ) -- LATIN SMALL LETTER C WITH ACUTE
             , ("cap"                                    , Left 0x02229               ) -- INTERSECTION
             , ("capand"                                 , Left 0x02A44               ) -- INTERSECTION WITH LOGICAL AND
             , ("capbrcup"                               , Left 0x02A49               ) -- INTERSECTION ABOVE BAR ABOVE UNION
             , ("capcap"                                 , Left 0x02A4B               ) -- INTERSECTION BESIDE AND JOINED WITH INTERSECTION
             , ("capcup"                                 , Left 0x02A47               ) -- INTERSECTION ABOVE UNION
             , ("capdot"                                 , Left 0x02A40               ) -- INTERSECTION WITH DOT
             , ("caps"                                   , Right [ 0x02229, 0x0FE00 ] ) -- INTERSECTION with serifs
             , ("caret"                                  , Left 0x02041               ) -- CARET INSERTION POINT
             , ("caron"                                  , Left 0x002C7               ) -- CARON
             , ("ccaps"                                  , Left 0x02A4D               ) -- CLOSED INTERSECTION WITH SERIFS
             , ("ccaron"                                 , Left 0x0010D               ) -- LATIN SMALL LETTER C WITH CARON
             , ("ccedil"                                 , Left 0x000E7               ) -- LATIN SMALL LETTER C WITH CEDILLA
             , ("ccirc"                                  , Left 0x00109               ) -- LATIN SMALL LETTER C WITH CIRCUMFLEX
             , ("ccups"                                  , Left 0x02A4C               ) -- CLOSED UNION WITH SERIFS
             , ("ccupssm"                                , Left 0x02A50               ) -- CLOSED UNION WITH SERIFS AND SMASH PRODUCT
             , ("cdot"                                   , Left 0x0010B               ) -- LATIN SMALL LETTER C WITH DOT ABOVE
             , ("cedil"                                  , Left 0x000B8               ) -- CEDILLA
             , ("cemptyv"                                , Left 0x029B2               ) -- EMPTY SET WITH SMALL CIRCLE ABOVE
             , ("cent"                                   , Left 0x000A2               ) -- CENT SIGN
             , ("centerdot"                              , Left 0x000B7               ) -- MIDDLE DOT
             , ("cfr"                                    , Left 0x1D520               ) -- MATHEMATICAL FRAKTUR SMALL C
             , ("chcy"                                   , Left 0x00447               ) -- CYRILLIC SMALL LETTER CHE
             , ("check"                                  , Left 0x02713               ) -- CHECK MARK
             , ("checkmark"                              , Left 0x02713               ) -- CHECK MARK
             , ("chi"                                    , Left 0x003C7               ) -- GREEK SMALL LETTER CHI
             , ("cir"                                    , Left 0x025CB               ) -- WHITE CIRCLE
             , ("cirE"                                   , Left 0x029C3               ) -- CIRCLE WITH TWO HORIZONTAL STROKES TO THE RIGHT
             , ("circ"                                   , Left 0x002C6               ) -- MODIFIER LETTER CIRCUMFLEX ACCENT
             , ("circeq"                                 , Left 0x02257               ) -- RING EQUAL TO
             , ("circlearrowleft"                        , Left 0x021BA               ) -- ANTICLOCKWISE OPEN CIRCLE ARROW
             , ("circlearrowright"                       , Left 0x021BB               ) -- CLOCKWISE OPEN CIRCLE ARROW
             , ("circledR"                               , Left 0x000AE               ) -- REGISTERED SIGN
             , ("circledS"                               , Left 0x024C8               ) -- CIRCLED LATIN CAPITAL LETTER S
             , ("circledast"                             , Left 0x0229B               ) -- CIRCLED ASTERISK OPERATOR
             , ("circledcirc"                            , Left 0x0229A               ) -- CIRCLED RING OPERATOR
             , ("circleddash"                            , Left 0x0229D               ) -- CIRCLED DASH
             , ("cire"                                   , Left 0x02257               ) -- RING EQUAL TO
             , ("cirfnint"                               , Left 0x02A10               ) -- CIRCULATION FUNCTION
             , ("cirmid"                                 , Left 0x02AEF               ) -- VERTICAL LINE WITH CIRCLE ABOVE
             , ("cirscir"                                , Left 0x029C2               ) -- CIRCLE WITH SMALL CIRCLE TO THE RIGHT
             , ("clubs"                                  , Left 0x02663               ) -- BLACK CLUB SUIT
             , ("clubsuit"                               , Left 0x02663               ) -- BLACK CLUB SUIT
             , ("colon"                                  , Left 0x0003A               ) -- COLON
             , ("colone"                                 , Left 0x02254               ) -- COLON EQUALS
             , ("coloneq"                                , Left 0x02254               ) -- COLON EQUALS
             , ("comma"                                  , Left 0x0002C               ) -- COMMA
             , ("commat"                                 , Left 0x00040               ) -- COMMERCIAL AT
             , ("comp"                                   , Left 0x02201               ) -- COMPLEMENT
             , ("compfn"                                 , Left 0x02218               ) -- RING OPERATOR
             , ("complement"                             , Left 0x02201               ) -- COMPLEMENT
             , ("complexes"                              , Left 0x02102               ) -- DOUBLE-STRUCK CAPITAL C
             , ("cong"                                   , Left 0x02245               ) -- APPROXIMATELY EQUAL TO
             , ("congdot"                                , Left 0x02A6D               ) -- CONGRUENT WITH DOT ABOVE
             , ("conint"                                 , Left 0x0222E               ) -- CONTOUR INTEGRAL
             , ("copf"                                   , Left 0x1D554               ) -- MATHEMATICAL DOUBLE-STRUCK SMALL C
             , ("coprod"                                 , Left 0x02210               ) -- N-ARY COPRODUCT
             , ("copy"                                   , Left 0x000A9               ) -- COPYRIGHT SIGN
             , ("copysr"                                 , Left 0x02117               ) -- SOUND RECORDING COPYRIGHT
             , ("crarr"                                  , Left 0x021B5               ) -- DOWNWARDS ARROW WITH CORNER LEFTWARDS
             , ("cross"                                  , Left 0x02717               ) -- BALLOT X
             , ("cscr"                                   , Left 0x1D4B8               ) -- MATHEMATICAL SCRIPT SMALL C
             , ("csub"                                   , Left 0x02ACF               ) -- CLOSED SUBSET
             , ("csube"                                  , Left 0x02AD1               ) -- CLOSED SUBSET OR EQUAL TO
             , ("csup"                                   , Left 0x02AD0               ) -- CLOSED SUPERSET
             , ("csupe"                                  , Left 0x02AD2               ) -- CLOSED SUPERSET OR EQUAL TO
             , ("ctdot"                                  , Left 0x022EF               ) -- MIDLINE HORIZONTAL ELLIPSIS
             , ("cudarrl"                                , Left 0x02938               ) -- RIGHT-SIDE ARC CLOCKWISE ARROW
             , ("cudarrr"                                , Left 0x02935               ) -- ARROW POINTING RIGHTWARDS THEN CURVING DOWNWARDS
             , ("cuepr"                                  , Left 0x022DE               ) -- EQUAL TO OR PRECEDES
             , ("cuesc"                                  , Left 0x022DF               ) -- EQUAL TO OR SUCCEEDS
             , ("cularr"                                 , Left 0x021B6               ) -- ANTICLOCKWISE TOP SEMICIRCLE ARROW
             , ("cularrp"                                , Left 0x0293D               ) -- TOP ARC ANTICLOCKWISE ARROW WITH PLUS
             , ("cup"                                    , Left 0x0222A               ) -- UNION
             , ("cupbrcap"                               , Left 0x02A48               ) -- UNION ABOVE BAR ABOVE INTERSECTION
             , ("cupcap"                                 , Left 0x02A46               ) -- UNION ABOVE INTERSECTION
             , ("cupcup"                                 , Left 0x02A4A               ) -- UNION BESIDE AND JOINED WITH UNION
             , ("cupdot"                                 , Left 0x0228D               ) -- MULTISET MULTIPLICATION
             , ("cupor"                                  , Left 0x02A45               ) -- UNION WITH LOGICAL OR
             , ("cups"                                   , Right [ 0x0222A, 0x0FE00 ] ) -- UNION with serifs
             , ("curarr"                                 , Left 0x021B7               ) -- CLOCKWISE TOP SEMICIRCLE ARROW
             , ("curarrm"                                , Left 0x0293C               ) -- TOP ARC CLOCKWISE ARROW WITH MINUS
             , ("curlyeqprec"                            , Left 0x022DE               ) -- EQUAL TO OR PRECEDES
             , ("curlyeqsucc"                            , Left 0x022DF               ) -- EQUAL TO OR SUCCEEDS
             , ("curlyvee"                               , Left 0x022CE               ) -- CURLY LOGICAL OR
             , ("curlywedge"                             , Left 0x022CF               ) -- CURLY LOGICAL AND
             , ("curren"                                 , Left 0x000A4               ) -- CURRENCY SIGN
             , ("curvearrowleft"                         , Left 0x021B6               ) -- ANTICLOCKWISE TOP SEMICIRCLE ARROW
             , ("curvearrowright"                        , Left 0x021B7               ) -- CLOCKWISE TOP SEMICIRCLE ARROW
             , ("cuvee"                                  , Left 0x022CE               ) -- CURLY LOGICAL OR
             , ("cuwed"                                  , Left 0x022CF               ) -- CURLY LOGICAL AND
             , ("cwconint"                               , Left 0x02232               ) -- CLOCKWISE CONTOUR INTEGRAL
             , ("cwint"                                  , Left 0x02231               ) -- CLOCKWISE INTEGRAL
             , ("cylcty"                                 , Left 0x0232D               ) -- CYLINDRICITY
             , ("dArr"                                   , Left 0x021D3               ) -- DOWNWARDS DOUBLE ARROW
             , ("dHar"                                   , Left 0x02965               ) -- DOWNWARDS HARPOON WITH BARB LEFT BESIDE DOWNWARDS HARPOON WITH BARB RIGHT
             , ("dagger"                                 , Left 0x02020               ) -- DAGGER
             , ("daleth"                                 , Left 0x02138               ) -- DALET SYMBOL
             , ("darr"                                   , Left 0x02193               ) -- DOWNWARDS ARROW
             , ("dash"                                   , Left 0x02010               ) -- HYPHEN
             , ("dashv"                                  , Left 0x022A3               ) -- LEFT TACK
             , ("dbkarow"                                , Left 0x0290F               ) -- RIGHTWARDS TRIPLE DASH ARROW
             , ("dblac"                                  , Left 0x002DD               ) -- DOUBLE ACUTE ACCENT
             , ("dcaron"                                 , Left 0x0010F               ) -- LATIN SMALL LETTER D WITH CARON
             , ("dcy"                                    , Left 0x00434               ) -- CYRILLIC SMALL LETTER DE
             , ("dd"                                     , Left 0x02146               ) -- DOUBLE-STRUCK ITALIC SMALL D
             , ("ddagger"                                , Left 0x02021               ) -- DOUBLE DAGGER
             , ("ddarr"                                  , Left 0x021CA               ) -- DOWNWARDS PAIRED ARROWS
             , ("ddotseq"                                , Left 0x02A77               ) -- EQUALS SIGN WITH TWO DOTS ABOVE AND TWO DOTS BELOW
             , ("deg"                                    , Left 0x000B0               ) -- DEGREE SIGN
             , ("delta"                                  , Left 0x003B4               ) -- GREEK SMALL LETTER DELTA
             , ("demptyv"                                , Left 0x029B1               ) -- EMPTY SET WITH OVERBAR
             , ("dfisht"                                 , Left 0x0297F               ) -- DOWN FISH TAIL
             , ("dfr"                                    , Left 0x1D521               ) -- MATHEMATICAL FRAKTUR SMALL D
             , ("dgr"                                    , Left 0x003B4               ) -- GREEK SMALL LETTER DELTA
             , ("dharl"                                  , Left 0x021C3               ) -- DOWNWARDS HARPOON WITH BARB LEFTWARDS
             , ("dharr"                                  , Left 0x021C2               ) -- DOWNWARDS HARPOON WITH BARB RIGHTWARDS
             , ("diam"                                   , Left 0x022C4               ) -- DIAMOND OPERATOR
             , ("diamond"                                , Left 0x022C4               ) -- DIAMOND OPERATOR
             , ("diamondsuit"                            , Left 0x02666               ) -- BLACK DIAMOND SUIT
             , ("diams"                                  , Left 0x02666               ) -- BLACK DIAMOND SUIT
             , ("die"                                    , Left 0x000A8               ) -- DIAERESIS
             , ("digamma"                                , Left 0x003DD               ) -- GREEK SMALL LETTER DIGAMMA
             , ("disin"                                  , Left 0x022F2               ) -- ELEMENT OF WITH LONG HORIZONTAL STROKE
             , ("div"                                    , Left 0x000F7               ) -- DIVISION SIGN
             , ("divide"                                 , Left 0x000F7               ) -- DIVISION SIGN
             , ("divideontimes"                          , Left 0x022C7               ) -- DIVISION TIMES
             , ("divonx"                                 , Left 0x022C7               ) -- DIVISION TIMES
             , ("djcy"                                   , Left 0x00452               ) -- CYRILLIC SMALL LETTER DJE
             , ("dlcorn"                                 , Left 0x0231E               ) -- BOTTOM LEFT CORNER
             , ("dlcrop"                                 , Left 0x0230D               ) -- BOTTOM LEFT CROP
             , ("dollar"                                 , Left 0x00024               ) -- DOLLAR SIGN
             , ("dopf"                                   , Left 0x1D555               ) -- MATHEMATICAL DOUBLE-STRUCK SMALL D
             , ("dot"                                    , Left 0x002D9               ) -- DOT ABOVE
             , ("doteq"                                  , Left 0x02250               ) -- APPROACHES THE LIMIT
             , ("doteqdot"                               , Left 0x02251               ) -- GEOMETRICALLY EQUAL TO
             , ("dotminus"                               , Left 0x02238               ) -- DOT MINUS
             , ("dotplus"                                , Left 0x02214               ) -- DOT PLUS
             , ("dotsquare"                              , Left 0x022A1               ) -- SQUARED DOT OPERATOR
             , ("doublebarwedge"                         , Left 0x02306               ) -- PERSPECTIVE
             , ("downarrow"                              , Left 0x02193               ) -- DOWNWARDS ARROW
             , ("downdownarrows"                         , Left 0x021CA               ) -- DOWNWARDS PAIRED ARROWS
             , ("downharpoonleft"                        , Left 0x021C3               ) -- DOWNWARDS HARPOON WITH BARB LEFTWARDS
             , ("downharpoonright"                       , Left 0x021C2               ) -- DOWNWARDS HARPOON WITH BARB RIGHTWARDS
             , ("drbkarow"                               , Left 0x02910               ) -- RIGHTWARDS TWO-HEADED TRIPLE DASH ARROW
             , ("drcorn"                                 , Left 0x0231F               ) -- BOTTOM RIGHT CORNER
             , ("drcrop"                                 , Left 0x0230C               ) -- BOTTOM RIGHT CROP
             , ("dscr"                                   , Left 0x1D4B9               ) -- MATHEMATICAL SCRIPT SMALL D
             , ("dscy"                                   , Left 0x00455               ) -- CYRILLIC SMALL LETTER DZE
             , ("dsol"                                   , Left 0x029F6               ) -- SOLIDUS WITH OVERBAR
             , ("dstrok"                                 , Left 0x00111               ) -- LATIN SMALL LETTER D WITH STROKE
             , ("dtdot"                                  , Left 0x022F1               ) -- DOWN RIGHT DIAGONAL ELLIPSIS
             , ("dtri"                                   , Left 0x025BF               ) -- WHITE DOWN-POINTING SMALL TRIANGLE
             , ("dtrif"                                  , Left 0x025BE               ) -- BLACK DOWN-POINTING SMALL TRIANGLE
             , ("duarr"                                  , Left 0x021F5               ) -- DOWNWARDS ARROW LEFTWARDS OF UPWARDS ARROW
             , ("duhar"                                  , Left 0x0296F               ) -- DOWNWARDS HARPOON WITH BARB LEFT BESIDE UPWARDS HARPOON WITH BARB RIGHT
             , ("dwangle"                                , Left 0x029A6               ) -- OBLIQUE ANGLE OPENING UP
             , ("dzcy"                                   , Left 0x0045F               ) -- CYRILLIC SMALL LETTER DZHE
             , ("dzigrarr"                               , Left 0x027FF               ) -- LONG RIGHTWARDS SQUIGGLE ARROW
             , ("eDDot"                                  , Left 0x02A77               ) -- EQUALS SIGN WITH TWO DOTS ABOVE AND TWO DOTS BELOW
             , ("eDot"                                   , Left 0x02251               ) -- GEOMETRICALLY EQUAL TO
             , ("eacgr"                                  , Left 0x003AD               ) -- GREEK SMALL LETTER EPSILON WITH TONOS
             , ("eacute"                                 , Left 0x000E9               ) -- LATIN SMALL LETTER E WITH ACUTE
             , ("easter"                                 , Left 0x02A6E               ) -- EQUALS WITH ASTERISK
             , ("ecaron"                                 , Left 0x0011B               ) -- LATIN SMALL LETTER E WITH CARON
             , ("ecir"                                   , Left 0x02256               ) -- RING IN EQUAL TO
             , ("ecirc"                                  , Left 0x000EA               ) -- LATIN SMALL LETTER E WITH CIRCUMFLEX
             , ("ecolon"                                 , Left 0x02255               ) -- EQUALS COLON
             , ("ecy"                                    , Left 0x0044D               ) -- CYRILLIC SMALL LETTER E
             , ("edot"                                   , Left 0x00117               ) -- LATIN SMALL LETTER E WITH DOT ABOVE
             , ("ee"                                     , Left 0x02147               ) -- DOUBLE-STRUCK ITALIC SMALL E
             , ("eeacgr"                                 , Left 0x003AE               ) -- GREEK SMALL LETTER ETA WITH TONOS
             , ("eegr"                                   , Left 0x003B7               ) -- GREEK SMALL LETTER ETA
             , ("efDot"                                  , Left 0x02252               ) -- APPROXIMATELY EQUAL TO OR THE IMAGE OF
             , ("efr"                                    , Left 0x1D522               ) -- MATHEMATICAL FRAKTUR SMALL E
             , ("eg"                                     , Left 0x02A9A               ) -- DOUBLE-LINE EQUAL TO OR GREATER-THAN
             , ("egr"                                    , Left 0x003B5               ) -- GREEK SMALL LETTER EPSILON
             , ("egrave"                                 , Left 0x000E8               ) -- LATIN SMALL LETTER E WITH GRAVE
             , ("egs"                                    , Left 0x02A96               ) -- SLANTED EQUAL TO OR GREATER-THAN
             , ("egsdot"                                 , Left 0x02A98               ) -- SLANTED EQUAL TO OR GREATER-THAN WITH DOT INSIDE
             , ("el"                                     , Left 0x02A99               ) -- DOUBLE-LINE EQUAL TO OR LESS-THAN
             , ("elinters"                               , Left 0x023E7               ) -- ELECTRICAL INTERSECTION
             , ("ell"                                    , Left 0x02113               ) -- SCRIPT SMALL L
             , ("els"                                    , Left 0x02A95               ) -- SLANTED EQUAL TO OR LESS-THAN
             , ("elsdot"                                 , Left 0x02A97               ) -- SLANTED EQUAL TO OR LESS-THAN WITH DOT INSIDE
             , ("emacr"                                  , Left 0x00113               ) -- LATIN SMALL LETTER E WITH MACRON
             , ("empty"                                  , Left 0x02205               ) -- EMPTY SET
             , ("emptyset"                               , Left 0x02205               ) -- EMPTY SET
             , ("emptyv"                                 , Left 0x02205               ) -- EMPTY SET
             , ("emsp"                                   , Left 0x02003               ) -- EM SPACE
             , ("emsp13"                                 , Left 0x02004               ) -- THREE-PER-EM SPACE
             , ("emsp14"                                 , Left 0x02005               ) -- FOUR-PER-EM SPACE
             , ("eng"                                    , Left 0x0014B               ) -- LATIN SMALL LETTER ENG
             , ("ensp"                                   , Left 0x02002               ) -- EN SPACE
             , ("eogon"                                  , Left 0x00119               ) -- LATIN SMALL LETTER E WITH OGONEK
             , ("eopf"                                   , Left 0x1D556               ) -- MATHEMATICAL DOUBLE-STRUCK SMALL E
             , ("epar"                                   , Left 0x022D5               ) -- EQUAL AND PARALLEL TO
             , ("eparsl"                                 , Left 0x029E3               ) -- EQUALS SIGN AND SLANTED PARALLEL
             , ("eplus"                                  , Left 0x02A71               ) -- EQUALS SIGN ABOVE PLUS SIGN
             , ("epsi"                                   , Left 0x003B5               ) -- GREEK SMALL LETTER EPSILON
             , ("epsilon"                                , Left 0x003B5               ) -- GREEK SMALL LETTER EPSILON
             , ("epsiv"                                  , Left 0x003F5               ) -- GREEK LUNATE EPSILON SYMBOL
             , ("eqcirc"                                 , Left 0x02256               ) -- RING IN EQUAL TO
             , ("eqcolon"                                , Left 0x02255               ) -- EQUALS COLON
             , ("eqsim"                                  , Left 0x02242               ) -- MINUS TILDE
             , ("eqslantgtr"                             , Left 0x02A96               ) -- SLANTED EQUAL TO OR GREATER-THAN
             , ("eqslantless"                            , Left 0x02A95               ) -- SLANTED EQUAL TO OR LESS-THAN
             , ("equals"                                 , Left 0x0003D               ) -- EQUALS SIGN
             , ("equest"                                 , Left 0x0225F               ) -- QUESTIONED EQUAL TO
             , ("equiv"                                  , Left 0x02261               ) -- IDENTICAL TO
             , ("equivDD"                                , Left 0x02A78               ) -- EQUIVALENT WITH FOUR DOTS ABOVE
             , ("eqvparsl"                               , Left 0x029E5               ) -- IDENTICAL TO AND SLANTED PARALLEL
             , ("erDot"                                  , Left 0x02253               ) -- IMAGE OF OR APPROXIMATELY EQUAL TO
             , ("erarr"                                  , Left 0x02971               ) -- EQUALS SIGN ABOVE RIGHTWARDS ARROW
             , ("escr"                                   , Left 0x0212F               ) -- SCRIPT SMALL E
             , ("esdot"                                  , Left 0x02250               ) -- APPROACHES THE LIMIT
             , ("esim"                                   , Left 0x02242               ) -- MINUS TILDE
             , ("eta"                                    , Left 0x003B7               ) -- GREEK SMALL LETTER ETA
             , ("eth"                                    , Left 0x000F0               ) -- LATIN SMALL LETTER ETH
             , ("euml"                                   , Left 0x000EB               ) -- LATIN SMALL LETTER E WITH DIAERESIS
             , ("euro"                                   , Left 0x020AC               ) -- EURO SIGN
             , ("excl"                                   , Left 0x00021               ) -- EXCLAMATION MARK
             , ("exist"                                  , Left 0x02203               ) -- THERE EXISTS
             , ("expectation"                            , Left 0x02130               ) -- SCRIPT CAPITAL E
             , ("exponentiale"                           , Left 0x02147               ) -- DOUBLE-STRUCK ITALIC SMALL E
             , ("fallingdotseq"                          , Left 0x02252               ) -- APPROXIMATELY EQUAL TO OR THE IMAGE OF
             , ("fcy"                                    , Left 0x00444               ) -- CYRILLIC SMALL LETTER EF
             , ("female"                                 , Left 0x02640               ) -- FEMALE SIGN
             , ("ffilig"                                 , Left 0x0FB03               ) -- LATIN SMALL LIGATURE FFI
             , ("fflig"                                  , Left 0x0FB00               ) -- LATIN SMALL LIGATURE FF
             , ("ffllig"                                 , Left 0x0FB04               ) -- LATIN SMALL LIGATURE FFL
             , ("ffr"                                    , Left 0x1D523               ) -- MATHEMATICAL FRAKTUR SMALL F
             , ("filig"                                  , Left 0x0FB01               ) -- LATIN SMALL LIGATURE FI
             , ("fjlig"                                  , Right [ 0x00066, 0x0006A ] ) -- fj ligature
             , ("flat"                                   , Left 0x0266D               ) -- MUSIC FLAT SIGN
             , ("fllig"                                  , Left 0x0FB02               ) -- LATIN SMALL LIGATURE FL
             , ("fltns"                                  , Left 0x025B1               ) -- WHITE PARALLELOGRAM
             , ("fnof"                                   , Left 0x00192               ) -- LATIN SMALL LETTER F WITH HOOK
             , ("fopf"                                   , Left 0x1D557               ) -- MATHEMATICAL DOUBLE-STRUCK SMALL F
             , ("forall"                                 , Left 0x02200               ) -- FOR ALL
             , ("fork"                                   , Left 0x022D4               ) -- PITCHFORK
             , ("forkv"                                  , Left 0x02AD9               ) -- ELEMENT OF OPENING DOWNWARDS
             , ("fpartint"                               , Left 0x02A0D               ) -- FINITE PART INTEGRAL
             , ("frac12"                                 , Left 0x000BD               ) -- VULGAR FRACTION ONE HALF
             , ("frac13"                                 , Left 0x02153               ) -- VULGAR FRACTION ONE THIRD
             , ("frac14"                                 , Left 0x000BC               ) -- VULGAR FRACTION ONE QUARTER
             , ("frac15"                                 , Left 0x02155               ) -- VULGAR FRACTION ONE FIFTH
             , ("frac16"                                 , Left 0x02159               ) -- VULGAR FRACTION ONE SIXTH
             , ("frac18"                                 , Left 0x0215B               ) -- VULGAR FRACTION ONE EIGHTH
             , ("frac23"                                 , Left 0x02154               ) -- VULGAR FRACTION TWO THIRDS
             , ("frac25"                                 , Left 0x02156               ) -- VULGAR FRACTION TWO FIFTHS
             , ("frac34"                                 , Left 0x000BE               ) -- VULGAR FRACTION THREE QUARTERS
             , ("frac35"                                 , Left 0x02157               ) -- VULGAR FRACTION THREE FIFTHS
             , ("frac38"                                 , Left 0x0215C               ) -- VULGAR FRACTION THREE EIGHTHS
             , ("frac45"                                 , Left 0x02158               ) -- VULGAR FRACTION FOUR FIFTHS
             , ("frac56"                                 , Left 0x0215A               ) -- VULGAR FRACTION FIVE SIXTHS
             , ("frac58"                                 , Left 0x0215D               ) -- VULGAR FRACTION FIVE EIGHTHS
             , ("frac78"                                 , Left 0x0215E               ) -- VULGAR FRACTION SEVEN EIGHTHS
             , ("frasl"                                  , Left 0x02044               ) -- FRACTION SLASH
             , ("frown"                                  , Left 0x02322               ) -- FROWN
             , ("fscr"                                   , Left 0x1D4BB               ) -- MATHEMATICAL SCRIPT SMALL F
             , ("gE"                                     , Left 0x02267               ) -- GREATER-THAN OVER EQUAL TO
             , ("gEl"                                    , Left 0x02A8C               ) -- GREATER-THAN ABOVE DOUBLE-LINE EQUAL ABOVE LESS-THAN
             , ("gacute"                                 , Left 0x001F5               ) -- LATIN SMALL LETTER G WITH ACUTE
             , ("gamma"                                  , Left 0x003B3               ) -- GREEK SMALL LETTER GAMMA
             , ("gammad"                                 , Left 0x003DD               ) -- GREEK SMALL LETTER DIGAMMA
             , ("gap"                                    , Left 0x02A86               ) -- GREATER-THAN OR APPROXIMATE
             , ("gbreve"                                 , Left 0x0011F               ) -- LATIN SMALL LETTER G WITH BREVE
             , ("gcirc"                                  , Left 0x0011D               ) -- LATIN SMALL LETTER G WITH CIRCUMFLEX
             , ("gcy"                                    , Left 0x00433               ) -- CYRILLIC SMALL LETTER GHE
             , ("gdot"                                   , Left 0x00121               ) -- LATIN SMALL LETTER G WITH DOT ABOVE
             , ("ge"                                     , Left 0x02265               ) -- GREATER-THAN OR EQUAL TO
             , ("gel"                                    , Left 0x022DB               ) -- GREATER-THAN EQUAL TO OR LESS-THAN
             , ("geq"                                    , Left 0x02265               ) -- GREATER-THAN OR EQUAL TO
             , ("geqq"                                   , Left 0x02267               ) -- GREATER-THAN OVER EQUAL TO
             , ("geqslant"                               , Left 0x02A7E               ) -- GREATER-THAN OR SLANTED EQUAL TO
             , ("ges"                                    , Left 0x02A7E               ) -- GREATER-THAN OR SLANTED EQUAL TO
             , ("gescc"                                  , Left 0x02AA9               ) -- GREATER-THAN CLOSED BY CURVE ABOVE SLANTED EQUAL
             , ("gesdot"                                 , Left 0x02A80               ) -- GREATER-THAN OR SLANTED EQUAL TO WITH DOT INSIDE
             , ("gesdoto"                                , Left 0x02A82               ) -- GREATER-THAN OR SLANTED EQUAL TO WITH DOT ABOVE
             , ("gesdotol"                               , Left 0x02A84               ) -- GREATER-THAN OR SLANTED EQUAL TO WITH DOT ABOVE LEFT
             , ("gesl"                                   , Right [ 0x022DB, 0x0FE00 ] ) -- GREATER-THAN slanted EQUAL TO OR LESS-THAN
             , ("gesles"                                 , Left 0x02A94               ) -- GREATER-THAN ABOVE SLANTED EQUAL ABOVE LESS-THAN ABOVE SLANTED EQUAL
             , ("gfr"                                    , Left 0x1D524               ) -- MATHEMATICAL FRAKTUR SMALL G
             , ("gg"                                     , Left 0x0226B               ) -- MUCH GREATER-THAN
             , ("ggg"                                    , Left 0x022D9               ) -- VERY MUCH GREATER-THAN
             , ("ggr"                                    , Left 0x003B3               ) -- GREEK SMALL LETTER GAMMA
             , ("gimel"                                  , Left 0x02137               ) -- GIMEL SYMBOL
             , ("gjcy"                                   , Left 0x00453               ) -- CYRILLIC SMALL LETTER GJE
             , ("gl"                                     , Left 0x02277               ) -- GREATER-THAN OR LESS-THAN
             , ("glE"                                    , Left 0x02A92               ) -- GREATER-THAN ABOVE LESS-THAN ABOVE DOUBLE-LINE EQUAL
             , ("gla"                                    , Left 0x02AA5               ) -- GREATER-THAN BESIDE LESS-THAN
             , ("glj"                                    , Left 0x02AA4               ) -- GREATER-THAN OVERLAPPING LESS-THAN
             , ("gnE"                                    , Left 0x02269               ) -- GREATER-THAN BUT NOT EQUAL TO
             , ("gnap"                                   , Left 0x02A8A               ) -- GREATER-THAN AND NOT APPROXIMATE
             , ("gnapprox"                               , Left 0x02A8A               ) -- GREATER-THAN AND NOT APPROXIMATE
             , ("gne"                                    , Left 0x02A88               ) -- GREATER-THAN AND SINGLE-LINE NOT EQUAL TO
             , ("gneq"                                   , Left 0x02A88               ) -- GREATER-THAN AND SINGLE-LINE NOT EQUAL TO
             , ("gneqq"                                  , Left 0x02269               ) -- GREATER-THAN BUT NOT EQUAL TO
             , ("gnsim"                                  , Left 0x022E7               ) -- GREATER-THAN BUT NOT EQUIVALENT TO
             , ("gopf"                                   , Left 0x1D558               ) -- MATHEMATICAL DOUBLE-STRUCK SMALL G
             , ("grave"                                  , Left 0x00060               ) -- GRAVE ACCENT
             , ("gscr"                                   , Left 0x0210A               ) -- SCRIPT SMALL G
             , ("gsim"                                   , Left 0x02273               ) -- GREATER-THAN OR EQUIVALENT TO
             , ("gsime"                                  , Left 0x02A8E               ) -- GREATER-THAN ABOVE SIMILAR OR EQUAL
             , ("gsiml"                                  , Left 0x02A90               ) -- GREATER-THAN ABOVE SIMILAR ABOVE LESS-THAN
             , ("gt"                                     , Left 0x0003E               ) -- GREATER-THAN SIGN
             , ("gtcc"                                   , Left 0x02AA7               ) -- GREATER-THAN CLOSED BY CURVE
             , ("gtcir"                                  , Left 0x02A7A               ) -- GREATER-THAN WITH CIRCLE INSIDE
             , ("gtdot"                                  , Left 0x022D7               ) -- GREATER-THAN WITH DOT
             , ("gtlPar"                                 , Left 0x02995               ) -- DOUBLE LEFT ARC GREATER-THAN BRACKET
             , ("gtquest"                                , Left 0x02A7C               ) -- GREATER-THAN WITH QUESTION MARK ABOVE
             , ("gtrapprox"                              , Left 0x02A86               ) -- GREATER-THAN OR APPROXIMATE
             , ("gtrarr"                                 , Left 0x02978               ) -- GREATER-THAN ABOVE RIGHTWARDS ARROW
             , ("gtrdot"                                 , Left 0x022D7               ) -- GREATER-THAN WITH DOT
             , ("gtreqless"                              , Left 0x022DB               ) -- GREATER-THAN EQUAL TO OR LESS-THAN
             , ("gtreqqless"                             , Left 0x02A8C               ) -- GREATER-THAN ABOVE DOUBLE-LINE EQUAL ABOVE LESS-THAN
             , ("gtrless"                                , Left 0x02277               ) -- GREATER-THAN OR LESS-THAN
             , ("gtrsim"                                 , Left 0x02273               ) -- GREATER-THAN OR EQUIVALENT TO
             , ("gvertneqq"                              , Right [ 0x02269, 0x0FE00 ] ) -- GREATER-THAN BUT NOT EQUAL TO - with vertical stroke
             , ("gvnE"                                   , Right [ 0x02269, 0x0FE00 ] ) -- GREATER-THAN BUT NOT EQUAL TO - with vertical stroke
             , ("hArr"                                   , Left 0x021D4               ) -- LEFT RIGHT DOUBLE ARROW
             , ("hairsp"                                 , Left 0x0200A               ) -- HAIR SPACE
             , ("half"                                   , Left 0x000BD               ) -- VULGAR FRACTION ONE HALF
             , ("hamilt"                                 , Left 0x0210B               ) -- SCRIPT CAPITAL H
             , ("hardcy"                                 , Left 0x0044A               ) -- CYRILLIC SMALL LETTER HARD SIGN
             , ("harr"                                   , Left 0x02194               ) -- LEFT RIGHT ARROW
             , ("harrcir"                                , Left 0x02948               ) -- LEFT RIGHT ARROW THROUGH SMALL CIRCLE
             , ("harrw"                                  , Left 0x021AD               ) -- LEFT RIGHT WAVE ARROW
             , ("hbar"                                   , Left 0x0210F               ) -- PLANCK CONSTANT OVER TWO PI
             , ("hcirc"                                  , Left 0x00125               ) -- LATIN SMALL LETTER H WITH CIRCUMFLEX
             , ("hearts"                                 , Left 0x02665               ) -- BLACK HEART SUIT
             , ("heartsuit"                              , Left 0x02665               ) -- BLACK HEART SUIT
             , ("hellip"                                 , Left 0x02026               ) -- HORIZONTAL ELLIPSIS
             , ("hercon"                                 , Left 0x022B9               ) -- HERMITIAN CONJUGATE MATRIX
             , ("hfr"                                    , Left 0x1D525               ) -- MATHEMATICAL FRAKTUR SMALL H
             , ("hksearow"                               , Left 0x02925               ) -- SOUTH EAST ARROW WITH HOOK
             , ("hkswarow"                               , Left 0x02926               ) -- SOUTH WEST ARROW WITH HOOK
             , ("hoarr"                                  , Left 0x021FF               ) -- LEFT RIGHT OPEN-HEADED ARROW
             , ("homtht"                                 , Left 0x0223B               ) -- HOMOTHETIC
             , ("hookleftarrow"                          , Left 0x021A9               ) -- LEFTWARDS ARROW WITH HOOK
             , ("hookrightarrow"                         , Left 0x021AA               ) -- RIGHTWARDS ARROW WITH HOOK
             , ("hopf"                                   , Left 0x1D559               ) -- MATHEMATICAL DOUBLE-STRUCK SMALL H
             , ("horbar"                                 , Left 0x02015               ) -- HORIZONTAL BAR
             , ("hscr"                                   , Left 0x1D4BD               ) -- MATHEMATICAL SCRIPT SMALL H
             , ("hslash"                                 , Left 0x0210F               ) -- PLANCK CONSTANT OVER TWO PI
             , ("hstrok"                                 , Left 0x00127               ) -- LATIN SMALL LETTER H WITH STROKE
             , ("hybull"                                 , Left 0x02043               ) -- HYPHEN BULLET
             , ("hyphen"                                 , Left 0x02010               ) -- HYPHEN
             , ("iacgr"                                  , Left 0x003AF               ) -- GREEK SMALL LETTER IOTA WITH TONOS
             , ("iacute"                                 , Left 0x000ED               ) -- LATIN SMALL LETTER I WITH ACUTE
             , ("ic"                                     , Left 0x02063               ) -- INVISIBLE SEPARATOR
             , ("icirc"                                  , Left 0x000EE               ) -- LATIN SMALL LETTER I WITH CIRCUMFLEX
             , ("icy"                                    , Left 0x00438               ) -- CYRILLIC SMALL LETTER I
             , ("idiagr"                                 , Left 0x00390               ) -- GREEK SMALL LETTER IOTA WITH DIALYTIKA AND TONOS
             , ("idigr"                                  , Left 0x003CA               ) -- GREEK SMALL LETTER IOTA WITH DIALYTIKA
             , ("iecy"                                   , Left 0x00435               ) -- CYRILLIC SMALL LETTER IE
             , ("iexcl"                                  , Left 0x000A1               ) -- INVERTED EXCLAMATION MARK
             , ("iff"                                    , Left 0x021D4               ) -- LEFT RIGHT DOUBLE ARROW
             , ("ifr"                                    , Left 0x1D526               ) -- MATHEMATICAL FRAKTUR SMALL I
             , ("igr"                                    , Left 0x003B9               ) -- GREEK SMALL LETTER IOTA
             , ("igrave"                                 , Left 0x000EC               ) -- LATIN SMALL LETTER I WITH GRAVE
             , ("ii"                                     , Left 0x02148               ) -- DOUBLE-STRUCK ITALIC SMALL I
             , ("iiiint"                                 , Left 0x02A0C               ) -- QUADRUPLE INTEGRAL OPERATOR
             , ("iiint"                                  , Left 0x0222D               ) -- TRIPLE INTEGRAL
             , ("iinfin"                                 , Left 0x029DC               ) -- INCOMPLETE INFINITY
             , ("iiota"                                  , Left 0x02129               ) -- TURNED GREEK SMALL LETTER IOTA
             , ("ijlig"                                  , Left 0x00133               ) -- LATIN SMALL LIGATURE IJ
             , ("imacr"                                  , Left 0x0012B               ) -- LATIN SMALL LETTER I WITH MACRON
             , ("image"                                  , Left 0x02111               ) -- BLACK-LETTER CAPITAL I
             , ("imagline"                               , Left 0x02110               ) -- SCRIPT CAPITAL I
             , ("imagpart"                               , Left 0x02111               ) -- BLACK-LETTER CAPITAL I
             , ("imath"                                  , Left 0x00131               ) -- LATIN SMALL LETTER DOTLESS I
             , ("imof"                                   , Left 0x022B7               ) -- IMAGE OF
             , ("imped"                                  , Left 0x001B5               ) -- LATIN CAPITAL LETTER Z WITH STROKE
             , ("in"                                     , Left 0x02208               ) -- ELEMENT OF
             , ("incare"                                 , Left 0x02105               ) -- CARE OF
             , ("infin"                                  , Left 0x0221E               ) -- INFINITY
             , ("infintie"                               , Left 0x029DD               ) -- TIE OVER INFINITY
             , ("inodot"                                 , Left 0x00131               ) -- LATIN SMALL LETTER DOTLESS I
             , ("int"                                    , Left 0x0222B               ) -- INTEGRAL
             , ("intcal"                                 , Left 0x022BA               ) -- INTERCALATE
             , ("integers"                               , Left 0x02124               ) -- DOUBLE-STRUCK CAPITAL Z
             , ("intercal"                               , Left 0x022BA               ) -- INTERCALATE
             , ("intlarhk"                               , Left 0x02A17               ) -- INTEGRAL WITH LEFTWARDS ARROW WITH HOOK
             , ("intprod"                                , Left 0x02A3C               ) -- INTERIOR PRODUCT
             , ("iocy"                                   , Left 0x00451               ) -- CYRILLIC SMALL LETTER IO
             , ("iogon"                                  , Left 0x0012F               ) -- LATIN SMALL LETTER I WITH OGONEK
             , ("iopf"                                   , Left 0x1D55A               ) -- MATHEMATICAL DOUBLE-STRUCK SMALL I
             , ("iota"                                   , Left 0x003B9               ) -- GREEK SMALL LETTER IOTA
             , ("iprod"                                  , Left 0x02A3C               ) -- INTERIOR PRODUCT
             , ("iquest"                                 , Left 0x000BF               ) -- INVERTED QUESTION MARK
             , ("iscr"                                   , Left 0x1D4BE               ) -- MATHEMATICAL SCRIPT SMALL I
             , ("isin"                                   , Left 0x02208               ) -- ELEMENT OF
             , ("isinE"                                  , Left 0x022F9               ) -- ELEMENT OF WITH TWO HORIZONTAL STROKES
             , ("isindot"                                , Left 0x022F5               ) -- ELEMENT OF WITH DOT ABOVE
             , ("isins"                                  , Left 0x022F4               ) -- SMALL ELEMENT OF WITH VERTICAL BAR AT END OF HORIZONTAL STROKE
             , ("isinsv"                                 , Left 0x022F3               ) -- ELEMENT OF WITH VERTICAL BAR AT END OF HORIZONTAL STROKE
             , ("isinv"                                  , Left 0x02208               ) -- ELEMENT OF
             , ("it"                                     , Left 0x02062               ) -- INVISIBLE TIMES
             , ("itilde"                                 , Left 0x00129               ) -- LATIN SMALL LETTER I WITH TILDE
             , ("iukcy"                                  , Left 0x00456               ) -- CYRILLIC SMALL LETTER BYELORUSSIAN-UKRAINIAN I
             , ("iuml"                                   , Left 0x000EF               ) -- LATIN SMALL LETTER I WITH DIAERESIS
             , ("jcirc"                                  , Left 0x00135               ) -- LATIN SMALL LETTER J WITH CIRCUMFLEX
             , ("jcy"                                    , Left 0x00439               ) -- CYRILLIC SMALL LETTER SHORT I
             , ("jfr"                                    , Left 0x1D527               ) -- MATHEMATICAL FRAKTUR SMALL J
             , ("jmath"                                  , Left 0x00237               ) -- LATIN SMALL LETTER DOTLESS J
             , ("jopf"                                   , Left 0x1D55B               ) -- MATHEMATICAL DOUBLE-STRUCK SMALL J
             , ("jscr"                                   , Left 0x1D4BF               ) -- MATHEMATICAL SCRIPT SMALL J
             , ("jsercy"                                 , Left 0x00458               ) -- CYRILLIC SMALL LETTER JE
             , ("jukcy"                                  , Left 0x00454               ) -- CYRILLIC SMALL LETTER UKRAINIAN IE
             , ("kappa"                                  , Left 0x003BA               ) -- GREEK SMALL LETTER KAPPA
             , ("kappav"                                 , Left 0x003F0               ) -- GREEK KAPPA SYMBOL
             , ("kcedil"                                 , Left 0x00137               ) -- LATIN SMALL LETTER K WITH CEDILLA
             , ("kcy"                                    , Left 0x0043A               ) -- CYRILLIC SMALL LETTER KA
             , ("kfr"                                    , Left 0x1D528               ) -- MATHEMATICAL FRAKTUR SMALL K
             , ("kgr"                                    , Left 0x003BA               ) -- GREEK SMALL LETTER KAPPA
             , ("kgreen"                                 , Left 0x00138               ) -- LATIN SMALL LETTER KRA
             , ("khcy"                                   , Left 0x00445               ) -- CYRILLIC SMALL LETTER HA
             , ("khgr"                                   , Left 0x003C7               ) -- GREEK SMALL LETTER CHI
             , ("kjcy"                                   , Left 0x0045C               ) -- CYRILLIC SMALL LETTER KJE
             , ("kopf"                                   , Left 0x1D55C               ) -- MATHEMATICAL DOUBLE-STRUCK SMALL K
             , ("kscr"                                   , Left 0x1D4C0               ) -- MATHEMATICAL SCRIPT SMALL K
             , ("lAarr"                                  , Left 0x021DA               ) -- LEFTWARDS TRIPLE ARROW
             , ("lArr"                                   , Left 0x021D0               ) -- LEFTWARDS DOUBLE ARROW
             , ("lAtail"                                 , Left 0x0291B               ) -- LEFTWARDS DOUBLE ARROW-TAIL
             , ("lBarr"                                  , Left 0x0290E               ) -- LEFTWARDS TRIPLE DASH ARROW
             , ("lE"                                     , Left 0x02266               ) -- LESS-THAN OVER EQUAL TO
             , ("lEg"                                    , Left 0x02A8B               ) -- LESS-THAN ABOVE DOUBLE-LINE EQUAL ABOVE GREATER-THAN
             , ("lHar"                                   , Left 0x02962               ) -- LEFTWARDS HARPOON WITH BARB UP ABOVE LEFTWARDS HARPOON WITH BARB DOWN
             , ("lacute"                                 , Left 0x0013A               ) -- LATIN SMALL LETTER L WITH ACUTE
             , ("laemptyv"                               , Left 0x029B4               ) -- EMPTY SET WITH LEFT ARROW ABOVE
             , ("lagran"                                 , Left 0x02112               ) -- SCRIPT CAPITAL L
             , ("lambda"                                 , Left 0x003BB               ) -- GREEK SMALL LETTER LAMDA
             , ("lang"                                   , Left 0x027E8               ) -- MATHEMATICAL LEFT ANGLE BRACKET
             , ("langd"                                  , Left 0x02991               ) -- LEFT ANGLE BRACKET WITH DOT
             , ("langle"                                 , Left 0x027E8               ) -- MATHEMATICAL LEFT ANGLE BRACKET
             , ("lap"                                    , Left 0x02A85               ) -- LESS-THAN OR APPROXIMATE
             , ("laquo"                                  , Left 0x000AB               ) -- LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
             , ("larr"                                   , Left 0x02190               ) -- LEFTWARDS ARROW
             , ("larrb"                                  , Left 0x021E4               ) -- LEFTWARDS ARROW TO BAR
             , ("larrbfs"                                , Left 0x0291F               ) -- LEFTWARDS ARROW FROM BAR TO BLACK DIAMOND
             , ("larrfs"                                 , Left 0x0291D               ) -- LEFTWARDS ARROW TO BLACK DIAMOND
             , ("larrhk"                                 , Left 0x021A9               ) -- LEFTWARDS ARROW WITH HOOK
             , ("larrlp"                                 , Left 0x021AB               ) -- LEFTWARDS ARROW WITH LOOP
             , ("larrpl"                                 , Left 0x02939               ) -- LEFT-SIDE ARC ANTICLOCKWISE ARROW
             , ("larrsim"                                , Left 0x02973               ) -- LEFTWARDS ARROW ABOVE TILDE OPERATOR
             , ("larrtl"                                 , Left 0x021A2               ) -- LEFTWARDS ARROW WITH TAIL
             , ("lat"                                    , Left 0x02AAB               ) -- LARGER THAN
             , ("latail"                                 , Left 0x02919               ) -- LEFTWARDS ARROW-TAIL
             , ("late"                                   , Left 0x02AAD               ) -- LARGER THAN OR EQUAL TO
             , ("lates"                                  , Right [ 0x02AAD, 0x0FE00 ] ) -- LARGER THAN OR slanted EQUAL
             , ("lbarr"                                  , Left 0x0290C               ) -- LEFTWARDS DOUBLE DASH ARROW
             , ("lbbrk"                                  , Left 0x02772               ) -- LIGHT LEFT TORTOISE SHELL BRACKET ORNAMENT
             , ("lbrace"                                 , Left 0x0007B               ) -- LEFT CURLY BRACKET
             , ("lbrack"                                 , Left 0x0005B               ) -- LEFT SQUARE BRACKET
             , ("lbrke"                                  , Left 0x0298B               ) -- LEFT SQUARE BRACKET WITH UNDERBAR
             , ("lbrksld"                                , Left 0x0298F               ) -- LEFT SQUARE BRACKET WITH TICK IN BOTTOM CORNER
             , ("lbrkslu"                                , Left 0x0298D               ) -- LEFT SQUARE BRACKET WITH TICK IN TOP CORNER
             , ("lcaron"                                 , Left 0x0013E               ) -- LATIN SMALL LETTER L WITH CARON
             , ("lcedil"                                 , Left 0x0013C               ) -- LATIN SMALL LETTER L WITH CEDILLA
             , ("lceil"                                  , Left 0x02308               ) -- LEFT CEILING
             , ("lcub"                                   , Left 0x0007B               ) -- LEFT CURLY BRACKET
             , ("lcy"                                    , Left 0x0043B               ) -- CYRILLIC SMALL LETTER EL
             , ("ldca"                                   , Left 0x02936               ) -- ARROW POINTING DOWNWARDS THEN CURVING LEFTWARDS
             , ("ldquo"                                  , Left 0x0201C               ) -- LEFT DOUBLE QUOTATION MARK
             , ("ldquor"                                 , Left 0x0201E               ) -- DOUBLE LOW-9 QUOTATION MARK
             , ("ldrdhar"                                , Left 0x02967               ) -- LEFTWARDS HARPOON WITH BARB DOWN ABOVE RIGHTWARDS HARPOON WITH BARB DOWN
             , ("ldrushar"                               , Left 0x0294B               ) -- LEFT BARB DOWN RIGHT BARB UP HARPOON
             , ("ldsh"                                   , Left 0x021B2               ) -- DOWNWARDS ARROW WITH TIP LEFTWARDS
             , ("le"                                     , Left 0x02264               ) -- LESS-THAN OR EQUAL TO
             , ("leftarrow"                              , Left 0x02190               ) -- LEFTWARDS ARROW
             , ("leftarrowtail"                          , Left 0x021A2               ) -- LEFTWARDS ARROW WITH TAIL
             , ("leftharpoondown"                        , Left 0x021BD               ) -- LEFTWARDS HARPOON WITH BARB DOWNWARDS
             , ("leftharpoonup"                          , Left 0x021BC               ) -- LEFTWARDS HARPOON WITH BARB UPWARDS
             , ("leftleftarrows"                         , Left 0x021C7               ) -- LEFTWARDS PAIRED ARROWS
             , ("leftrightarrow"                         , Left 0x02194               ) -- LEFT RIGHT ARROW
             , ("leftrightarrows"                        , Left 0x021C6               ) -- LEFTWARDS ARROW OVER RIGHTWARDS ARROW
             , ("leftrightharpoons"                      , Left 0x021CB               ) -- LEFTWARDS HARPOON OVER RIGHTWARDS HARPOON
             , ("leftrightsquigarrow"                    , Left 0x021AD               ) -- LEFT RIGHT WAVE ARROW
             , ("leftthreetimes"                         , Left 0x022CB               ) -- LEFT SEMIDIRECT PRODUCT
             , ("leg"                                    , Left 0x022DA               ) -- LESS-THAN EQUAL TO OR GREATER-THAN
             , ("leq"                                    , Left 0x02264               ) -- LESS-THAN OR EQUAL TO
             , ("leqq"                                   , Left 0x02266               ) -- LESS-THAN OVER EQUAL TO
             , ("leqslant"                               , Left 0x02A7D               ) -- LESS-THAN OR SLANTED EQUAL TO
             , ("les"                                    , Left 0x02A7D               ) -- LESS-THAN OR SLANTED EQUAL TO
             , ("lescc"                                  , Left 0x02AA8               ) -- LESS-THAN CLOSED BY CURVE ABOVE SLANTED EQUAL
             , ("lesdot"                                 , Left 0x02A7F               ) -- LESS-THAN OR SLANTED EQUAL TO WITH DOT INSIDE
             , ("lesdoto"                                , Left 0x02A81               ) -- LESS-THAN OR SLANTED EQUAL TO WITH DOT ABOVE
             , ("lesdotor"                               , Left 0x02A83               ) -- LESS-THAN OR SLANTED EQUAL TO WITH DOT ABOVE RIGHT
             , ("lesg"                                   , Right [ 0x022DA, 0x0FE00 ] ) -- LESS-THAN slanted EQUAL TO OR GREATER-THAN
             , ("lesges"                                 , Left 0x02A93               ) -- LESS-THAN ABOVE SLANTED EQUAL ABOVE GREATER-THAN ABOVE SLANTED EQUAL
             , ("lessapprox"                             , Left 0x02A85               ) -- LESS-THAN OR APPROXIMATE
             , ("lessdot"                                , Left 0x022D6               ) -- LESS-THAN WITH DOT
             , ("lesseqgtr"                              , Left 0x022DA               ) -- LESS-THAN EQUAL TO OR GREATER-THAN
             , ("lesseqqgtr"                             , Left 0x02A8B               ) -- LESS-THAN ABOVE DOUBLE-LINE EQUAL ABOVE GREATER-THAN
             , ("lessgtr"                                , Left 0x02276               ) -- LESS-THAN OR GREATER-THAN
             , ("lesssim"                                , Left 0x02272               ) -- LESS-THAN OR EQUIVALENT TO
             , ("lfisht"                                 , Left 0x0297C               ) -- LEFT FISH TAIL
             , ("lfloor"                                 , Left 0x0230A               ) -- LEFT FLOOR
             , ("lfr"                                    , Left 0x1D529               ) -- MATHEMATICAL FRAKTUR SMALL L
             , ("lg"                                     , Left 0x02276               ) -- LESS-THAN OR GREATER-THAN
             , ("lgE"                                    , Left 0x02A91               ) -- LESS-THAN ABOVE GREATER-THAN ABOVE DOUBLE-LINE EQUAL
             , ("lgr"                                    , Left 0x003BB               ) -- GREEK SMALL LETTER LAMDA
             , ("lhard"                                  , Left 0x021BD               ) -- LEFTWARDS HARPOON WITH BARB DOWNWARDS
             , ("lharu"                                  , Left 0x021BC               ) -- LEFTWARDS HARPOON WITH BARB UPWARDS
             , ("lharul"                                 , Left 0x0296A               ) -- LEFTWARDS HARPOON WITH BARB UP ABOVE LONG DASH
             , ("lhblk"                                  , Left 0x02584               ) -- LOWER HALF BLOCK
             , ("ljcy"                                   , Left 0x00459               ) -- CYRILLIC SMALL LETTER LJE
             , ("ll"                                     , Left 0x0226A               ) -- MUCH LESS-THAN
             , ("llarr"                                  , Left 0x021C7               ) -- LEFTWARDS PAIRED ARROWS
             , ("llcorner"                               , Left 0x0231E               ) -- BOTTOM LEFT CORNER
             , ("llhard"                                 , Left 0x0296B               ) -- LEFTWARDS HARPOON WITH BARB DOWN BELOW LONG DASH
             , ("lltri"                                  , Left 0x025FA               ) -- LOWER LEFT TRIANGLE
             , ("lmidot"                                 , Left 0x00140               ) -- LATIN SMALL LETTER L WITH MIDDLE DOT
             , ("lmoust"                                 , Left 0x023B0               ) -- UPPER LEFT OR LOWER RIGHT CURLY BRACKET SECTION
             , ("lmoustache"                             , Left 0x023B0               ) -- UPPER LEFT OR LOWER RIGHT CURLY BRACKET SECTION
             , ("lnE"                                    , Left 0x02268               ) -- LESS-THAN BUT NOT EQUAL TO
             , ("lnap"                                   , Left 0x02A89               ) -- LESS-THAN AND NOT APPROXIMATE
             , ("lnapprox"                               , Left 0x02A89               ) -- LESS-THAN AND NOT APPROXIMATE
             , ("lne"                                    , Left 0x02A87               ) -- LESS-THAN AND SINGLE-LINE NOT EQUAL TO
             , ("lneq"                                   , Left 0x02A87               ) -- LESS-THAN AND SINGLE-LINE NOT EQUAL TO
             , ("lneqq"                                  , Left 0x02268               ) -- LESS-THAN BUT NOT EQUAL TO
             , ("lnsim"                                  , Left 0x022E6               ) -- LESS-THAN BUT NOT EQUIVALENT TO
             , ("loang"                                  , Left 0x027EC               ) -- MATHEMATICAL LEFT WHITE TORTOISE SHELL BRACKET
             , ("loarr"                                  , Left 0x021FD               ) -- LEFTWARDS OPEN-HEADED ARROW
             , ("lobrk"                                  , Left 0x027E6               ) -- MATHEMATICAL LEFT WHITE SQUARE BRACKET
             , ("longleftarrow"                          , Left 0x027F5               ) -- LONG LEFTWARDS ARROW
             , ("longleftrightarrow"                     , Left 0x027F7               ) -- LONG LEFT RIGHT ARROW
             , ("longmapsto"                             , Left 0x027FC               ) -- LONG RIGHTWARDS ARROW FROM BAR
             , ("longrightarrow"                         , Left 0x027F6               ) -- LONG RIGHTWARDS ARROW
             , ("looparrowleft"                          , Left 0x021AB               ) -- LEFTWARDS ARROW WITH LOOP
             , ("looparrowright"                         , Left 0x021AC               ) -- RIGHTWARDS ARROW WITH LOOP
             , ("lopar"                                  , Left 0x02985               ) -- LEFT WHITE PARENTHESIS
             , ("lopf"                                   , Left 0x1D55D               ) -- MATHEMATICAL DOUBLE-STRUCK SMALL L
             , ("loplus"                                 , Left 0x02A2D               ) -- PLUS SIGN IN LEFT HALF CIRCLE
             , ("lotimes"                                , Left 0x02A34               ) -- MULTIPLICATION SIGN IN LEFT HALF CIRCLE
             , ("lowast"                                 , Left 0x02217               ) -- ASTERISK OPERATOR
             , ("lowbar"                                 , Left 0x0005F               ) -- LOW LINE
             , ("loz"                                    , Left 0x025CA               ) -- LOZENGE
             , ("lozenge"                                , Left 0x025CA               ) -- LOZENGE
             , ("lozf"                                   , Left 0x029EB               ) -- BLACK LOZENGE
             , ("lpar"                                   , Left 0x00028               ) -- LEFT PARENTHESIS
             , ("lparlt"                                 , Left 0x02993               ) -- LEFT ARC LESS-THAN BRACKET
             , ("lrarr"                                  , Left 0x021C6               ) -- LEFTWARDS ARROW OVER RIGHTWARDS ARROW
             , ("lrcorner"                               , Left 0x0231F               ) -- BOTTOM RIGHT CORNER
             , ("lrhar"                                  , Left 0x021CB               ) -- LEFTWARDS HARPOON OVER RIGHTWARDS HARPOON
             , ("lrhard"                                 , Left 0x0296D               ) -- RIGHTWARDS HARPOON WITH BARB DOWN BELOW LONG DASH
             , ("lrm"                                    , Left 0x0200E               ) -- LEFT-TO-RIGHT MARK
             , ("lrtri"                                  , Left 0x022BF               ) -- RIGHT TRIANGLE
             , ("lsaquo"                                 , Left 0x02039               ) -- SINGLE LEFT-POINTING ANGLE QUOTATION MARK
             , ("lscr"                                   , Left 0x1D4C1               ) -- MATHEMATICAL SCRIPT SMALL L
             , ("lsh"                                    , Left 0x021B0               ) -- UPWARDS ARROW WITH TIP LEFTWARDS
             , ("lsim"                                   , Left 0x02272               ) -- LESS-THAN OR EQUIVALENT TO
             , ("lsime"                                  , Left 0x02A8D               ) -- LESS-THAN ABOVE SIMILAR OR EQUAL
             , ("lsimg"                                  , Left 0x02A8F               ) -- LESS-THAN ABOVE SIMILAR ABOVE GREATER-THAN
             , ("lsqb"                                   , Left 0x0005B               ) -- LEFT SQUARE BRACKET
             , ("lsquo"                                  , Left 0x02018               ) -- LEFT SINGLE QUOTATION MARK
             , ("lsquor"                                 , Left 0x0201A               ) -- SINGLE LOW-9 QUOTATION MARK
             , ("lstrok"                                 , Left 0x00142               ) -- LATIN SMALL LETTER L WITH STROKE
             , ("lt"                                     , Left 0x0003C               ) -- LESS-THAN SIGN
             , ("ltcc"                                   , Left 0x02AA6               ) -- LESS-THAN CLOSED BY CURVE
             , ("ltcir"                                  , Left 0x02A79               ) -- LESS-THAN WITH CIRCLE INSIDE
             , ("ltdot"                                  , Left 0x022D6               ) -- LESS-THAN WITH DOT
             , ("lthree"                                 , Left 0x022CB               ) -- LEFT SEMIDIRECT PRODUCT
             , ("ltimes"                                 , Left 0x022C9               ) -- LEFT NORMAL FACTOR SEMIDIRECT PRODUCT
             , ("ltlarr"                                 , Left 0x02976               ) -- LESS-THAN ABOVE LEFTWARDS ARROW
             , ("ltquest"                                , Left 0x02A7B               ) -- LESS-THAN WITH QUESTION MARK ABOVE
             , ("ltrPar"                                 , Left 0x02996               ) -- DOUBLE RIGHT ARC LESS-THAN BRACKET
             , ("ltri"                                   , Left 0x025C3               ) -- WHITE LEFT-POINTING SMALL TRIANGLE
             , ("ltrie"                                  , Left 0x022B4               ) -- NORMAL SUBGROUP OF OR EQUAL TO
             , ("ltrif"                                  , Left 0x025C2               ) -- BLACK LEFT-POINTING SMALL TRIANGLE
             , ("lurdshar"                               , Left 0x0294A               ) -- LEFT BARB UP RIGHT BARB DOWN HARPOON
             , ("luruhar"                                , Left 0x02966               ) -- LEFTWARDS HARPOON WITH BARB UP ABOVE RIGHTWARDS HARPOON WITH BARB UP
             , ("lvertneqq"                              , Right [ 0x02268, 0x0FE00 ] ) -- LESS-THAN BUT NOT EQUAL TO - with vertical stroke
             , ("lvnE"                                   , Right [ 0x02268, 0x0FE00 ] ) -- LESS-THAN BUT NOT EQUAL TO - with vertical stroke
             , ("mDDot"                                  , Left 0x0223A               ) -- GEOMETRIC PROPORTION
             , ("macr"                                   , Left 0x000AF               ) -- MACRON
             , ("male"                                   , Left 0x02642               ) -- MALE SIGN
             , ("malt"                                   , Left 0x02720               ) -- MALTESE CROSS
             , ("maltese"                                , Left 0x02720               ) -- MALTESE CROSS
             , ("map"                                    , Left 0x021A6               ) -- RIGHTWARDS ARROW FROM BAR
             , ("mapsto"                                 , Left 0x021A6               ) -- RIGHTWARDS ARROW FROM BAR
             , ("mapstodown"                             , Left 0x021A7               ) -- DOWNWARDS ARROW FROM BAR
             , ("mapstoleft"                             , Left 0x021A4               ) -- LEFTWARDS ARROW FROM BAR
             , ("mapstoup"                               , Left 0x021A5               ) -- UPWARDS ARROW FROM BAR
             , ("marker"                                 , Left 0x025AE               ) -- BLACK VERTICAL RECTANGLE
             , ("mcomma"                                 , Left 0x02A29               ) -- MINUS SIGN WITH COMMA ABOVE
             , ("mcy"                                    , Left 0x0043C               ) -- CYRILLIC SMALL LETTER EM
             , ("mdash"                                  , Left 0x02014               ) -- EM DASH
             , ("measuredangle"                          , Left 0x02221               ) -- MEASURED ANGLE
             , ("mfr"                                    , Left 0x1D52A               ) -- MATHEMATICAL FRAKTUR SMALL M
             , ("mgr"                                    , Left 0x003BC               ) -- GREEK SMALL LETTER MU
             , ("mho"                                    , Left 0x02127               ) -- INVERTED OHM SIGN
             , ("micro"                                  , Left 0x000B5               ) -- MICRO SIGN
             , ("mid"                                    , Left 0x02223               ) -- DIVIDES
             , ("midast"                                 , Left 0x0002A               ) -- ASTERISK
             , ("midcir"                                 , Left 0x02AF0               ) -- VERTICAL LINE WITH CIRCLE BELOW
             , ("middot"                                 , Left 0x000B7               ) -- MIDDLE DOT
             , ("minus"                                  , Left 0x02212               ) -- MINUS SIGN
             , ("minusb"                                 , Left 0x0229F               ) -- SQUARED MINUS
             , ("minusd"                                 , Left 0x02238               ) -- DOT MINUS
             , ("minusdu"                                , Left 0x02A2A               ) -- MINUS SIGN WITH DOT BELOW
             , ("mlcp"                                   , Left 0x02ADB               ) -- TRANSVERSAL INTERSECTION
             , ("mldr"                                   , Left 0x02026               ) -- HORIZONTAL ELLIPSIS
             , ("mnplus"                                 , Left 0x02213               ) -- MINUS-OR-PLUS SIGN
             , ("models"                                 , Left 0x022A7               ) -- MODELS
             , ("mopf"                                   , Left 0x1D55E               ) -- MATHEMATICAL DOUBLE-STRUCK SMALL M
             , ("mp"                                     , Left 0x02213               ) -- MINUS-OR-PLUS SIGN
             , ("mscr"                                   , Left 0x1D4C2               ) -- MATHEMATICAL SCRIPT SMALL M
             , ("mstpos"                                 , Left 0x0223E               ) -- INVERTED LAZY S
             , ("mu"                                     , Left 0x003BC               ) -- GREEK SMALL LETTER MU
             , ("multimap"                               , Left 0x022B8               ) -- MULTIMAP
             , ("mumap"                                  , Left 0x022B8               ) -- MULTIMAP
             , ("nGg"                                    , Right [ 0x022D9, 0x00338 ] ) -- VERY MUCH GREATER-THAN with slash
             , ("nGt"                                    , Right [ 0x0226B, 0x020D2 ] ) -- MUCH GREATER THAN with vertical line
             , ("nGtv"                                   , Right [ 0x0226B, 0x00338 ] ) -- MUCH GREATER THAN with slash
             , ("nLeftarrow"                             , Left 0x021CD               ) -- LEFTWARDS DOUBLE ARROW WITH STROKE
             , ("nLeftrightarrow"                        , Left 0x021CE               ) -- LEFT RIGHT DOUBLE ARROW WITH STROKE
             , ("nLl"                                    , Right [ 0x022D8, 0x00338 ] ) -- VERY MUCH LESS-THAN with slash
             , ("nLt"                                    , Right [ 0x0226A, 0x020D2 ] ) -- MUCH LESS THAN with vertical line
             , ("nLtv"                                   , Right [ 0x0226A, 0x00338 ] ) -- MUCH LESS THAN with slash
             , ("nRightarrow"                            , Left 0x021CF               ) -- RIGHTWARDS DOUBLE ARROW WITH STROKE
             , ("nVDash"                                 , Left 0x022AF               ) -- NEGATED DOUBLE VERTICAL BAR DOUBLE RIGHT TURNSTILE
             , ("nVdash"                                 , Left 0x022AE               ) -- DOES NOT FORCE
             , ("nabla"                                  , Left 0x02207               ) -- NABLA
             , ("nacute"                                 , Left 0x00144               ) -- LATIN SMALL LETTER N WITH ACUTE
             , ("nang"                                   , Right [ 0x02220, 0x020D2 ] ) -- ANGLE with vertical line
             , ("nap"                                    , Left 0x02249               ) -- NOT ALMOST EQUAL TO
             , ("napE"                                   , Right [ 0x02A70, 0x00338 ] ) -- APPROXIMATELY EQUAL OR EQUAL TO with slash
             , ("napid"                                  , Right [ 0x0224B, 0x00338 ] ) -- TRIPLE TILDE with slash
             , ("napos"                                  , Left 0x00149               ) -- LATIN SMALL LETTER N PRECEDED BY APOSTROPHE
             , ("napprox"                                , Left 0x02249               ) -- NOT ALMOST EQUAL TO
             , ("natur"                                  , Left 0x0266E               ) -- MUSIC NATURAL SIGN
             , ("natural"                                , Left 0x0266E               ) -- MUSIC NATURAL SIGN
             , ("naturals"                               , Left 0x02115               ) -- DOUBLE-STRUCK CAPITAL N
             , ("nbsp"                                   , Left 0x000A0               ) -- NO-BREAK SPACE
             , ("nbump"                                  , Right [ 0x0224E, 0x00338 ] ) -- GEOMETRICALLY EQUIVALENT TO with slash
             , ("nbumpe"                                 , Right [ 0x0224F, 0x00338 ] ) -- DIFFERENCE BETWEEN with slash
             , ("ncap"                                   , Left 0x02A43               ) -- INTERSECTION WITH OVERBAR
             , ("ncaron"                                 , Left 0x00148               ) -- LATIN SMALL LETTER N WITH CARON
             , ("ncedil"                                 , Left 0x00146               ) -- LATIN SMALL LETTER N WITH CEDILLA
             , ("ncong"                                  , Left 0x02247               ) -- NEITHER APPROXIMATELY NOR ACTUALLY EQUAL TO
             , ("ncongdot"                               , Right [ 0x02A6D, 0x00338 ] ) -- CONGRUENT WITH DOT ABOVE with slash
             , ("ncup"                                   , Left 0x02A42               ) -- UNION WITH OVERBAR
             , ("ncy"                                    , Left 0x0043D               ) -- CYRILLIC SMALL LETTER EN
             , ("ndash"                                  , Left 0x02013               ) -- EN DASH
             , ("ne"                                     , Left 0x02260               ) -- NOT EQUAL TO
             , ("neArr"                                  , Left 0x021D7               ) -- NORTH EAST DOUBLE ARROW
             , ("nearhk"                                 , Left 0x02924               ) -- NORTH EAST ARROW WITH HOOK
             , ("nearr"                                  , Left 0x02197               ) -- NORTH EAST ARROW
             , ("nearrow"                                , Left 0x02197               ) -- NORTH EAST ARROW
             , ("nedot"                                  , Right [ 0x02250, 0x00338 ] ) -- APPROACHES THE LIMIT with slash
             , ("nequiv"                                 , Left 0x02262               ) -- NOT IDENTICAL TO
             , ("nesear"                                 , Left 0x02928               ) -- NORTH EAST ARROW AND SOUTH EAST ARROW
             , ("nesim"                                  , Right [ 0x02242, 0x00338 ] ) -- MINUS TILDE with slash
             , ("nexist"                                 , Left 0x02204               ) -- THERE DOES NOT EXIST
             , ("nexists"                                , Left 0x02204               ) -- THERE DOES NOT EXIST
             , ("nfr"                                    , Left 0x1D52B               ) -- MATHEMATICAL FRAKTUR SMALL N
             , ("ngE"                                    , Right [ 0x02267, 0x00338 ] ) -- GREATER-THAN OVER EQUAL TO with slash
             , ("nge"                                    , Left 0x02271               ) -- NEITHER GREATER-THAN NOR EQUAL TO
             , ("ngeq"                                   , Left 0x02271               ) -- NEITHER GREATER-THAN NOR EQUAL TO
             , ("ngeqq"                                  , Right [ 0x02267, 0x00338 ] ) -- GREATER-THAN OVER EQUAL TO with slash
             , ("ngeqslant"                              , Right [ 0x02A7E, 0x00338 ] ) -- GREATER-THAN OR SLANTED EQUAL TO with slash
             , ("nges"                                   , Right [ 0x02A7E, 0x00338 ] ) -- GREATER-THAN OR SLANTED EQUAL TO with slash
             , ("ngr"                                    , Left 0x003BD               ) -- GREEK SMALL LETTER NU
             , ("ngsim"                                  , Left 0x02275               ) -- NEITHER GREATER-THAN NOR EQUIVALENT TO
             , ("ngt"                                    , Left 0x0226F               ) -- NOT GREATER-THAN
             , ("ngtr"                                   , Left 0x0226F               ) -- NOT GREATER-THAN
             , ("nhArr"                                  , Left 0x021CE               ) -- LEFT RIGHT DOUBLE ARROW WITH STROKE
             , ("nharr"                                  , Left 0x021AE               ) -- LEFT RIGHT ARROW WITH STROKE
             , ("nhpar"                                  , Left 0x02AF2               ) -- PARALLEL WITH HORIZONTAL STROKE
             , ("ni"                                     , Left 0x0220B               ) -- CONTAINS AS MEMBER
             , ("nis"                                    , Left 0x022FC               ) -- SMALL CONTAINS WITH VERTICAL BAR AT END OF HORIZONTAL STROKE
             , ("nisd"                                   , Left 0x022FA               ) -- CONTAINS WITH LONG HORIZONTAL STROKE
             , ("niv"                                    , Left 0x0220B               ) -- CONTAINS AS MEMBER
             , ("njcy"                                   , Left 0x0045A               ) -- CYRILLIC SMALL LETTER NJE
             , ("nlArr"                                  , Left 0x021CD               ) -- LEFTWARDS DOUBLE ARROW WITH STROKE
             , ("nlE"                                    , Right [ 0x02266, 0x00338 ] ) -- LESS-THAN OVER EQUAL TO with slash
             , ("nlarr"                                  , Left 0x0219A               ) -- LEFTWARDS ARROW WITH STROKE
             , ("nldr"                                   , Left 0x02025               ) -- TWO DOT LEADER
             , ("nle"                                    , Left 0x02270               ) -- NEITHER LESS-THAN NOR EQUAL TO
             , ("nleftarrow"                             , Left 0x0219A               ) -- LEFTWARDS ARROW WITH STROKE
             , ("nleftrightarrow"                        , Left 0x021AE               ) -- LEFT RIGHT ARROW WITH STROKE
             , ("nleq"                                   , Left 0x02270               ) -- NEITHER LESS-THAN NOR EQUAL TO
             , ("nleqq"                                  , Right [ 0x02266, 0x00338 ] ) -- LESS-THAN OVER EQUAL TO with slash
             , ("nleqslant"                              , Right [ 0x02A7D, 0x00338 ] ) -- LESS-THAN OR SLANTED EQUAL TO with slash
             , ("nles"                                   , Right [ 0x02A7D, 0x00338 ] ) -- LESS-THAN OR SLANTED EQUAL TO with slash
             , ("nless"                                  , Left 0x0226E               ) -- NOT LESS-THAN
             , ("nlsim"                                  , Left 0x02274               ) -- NEITHER LESS-THAN NOR EQUIVALENT TO
             , ("nlt"                                    , Left 0x0226E               ) -- NOT LESS-THAN
             , ("nltri"                                  , Left 0x022EA               ) -- NOT NORMAL SUBGROUP OF
             , ("nltrie"                                 , Left 0x022EC               ) -- NOT NORMAL SUBGROUP OF OR EQUAL TO
             , ("nmid"                                   , Left 0x02224               ) -- DOES NOT DIVIDE
             , ("nopf"                                   , Left 0x1D55F               ) -- MATHEMATICAL DOUBLE-STRUCK SMALL N
             , ("not"                                    , Left 0x000AC               ) -- NOT SIGN
             , ("notin"                                  , Left 0x02209               ) -- NOT AN ELEMENT OF
             , ("notinE"                                 , Right [ 0x022F9, 0x00338 ] ) -- ELEMENT OF WITH TWO HORIZONTAL STROKES with slash
             , ("notindot"                               , Right [ 0x022F5, 0x00338 ] ) -- ELEMENT OF WITH DOT ABOVE with slash
             , ("notinva"                                , Left 0x02209               ) -- NOT AN ELEMENT OF
             , ("notinvb"                                , Left 0x022F7               ) -- SMALL ELEMENT OF WITH OVERBAR
             , ("notinvc"                                , Left 0x022F6               ) -- ELEMENT OF WITH OVERBAR
             , ("notni"                                  , Left 0x0220C               ) -- DOES NOT CONTAIN AS MEMBER
             , ("notniva"                                , Left 0x0220C               ) -- DOES NOT CONTAIN AS MEMBER
             , ("notnivb"                                , Left 0x022FE               ) -- SMALL CONTAINS WITH OVERBAR
             , ("notnivc"                                , Left 0x022FD               ) -- CONTAINS WITH OVERBAR
             , ("npar"                                   , Left 0x02226               ) -- NOT PARALLEL TO
             , ("nparallel"                              , Left 0x02226               ) -- NOT PARALLEL TO
             , ("nparsl"                                 , Right [ 0x02AFD, 0x020E5 ] ) -- DOUBLE SOLIDUS OPERATOR with reverse slash
             , ("npart"                                  , Right [ 0x02202, 0x00338 ] ) -- PARTIAL DIFFERENTIAL with slash
             , ("npolint"                                , Left 0x02A14               ) -- LINE INTEGRATION NOT INCLUDING THE POLE
             , ("npr"                                    , Left 0x02280               ) -- DOES NOT PRECEDE
             , ("nprcue"                                 , Left 0x022E0               ) -- DOES NOT PRECEDE OR EQUAL
             , ("npre"                                   , Right [ 0x02AAF, 0x00338 ] ) -- PRECEDES ABOVE SINGLE-LINE EQUALS SIGN with slash
             , ("nprec"                                  , Left 0x02280               ) -- DOES NOT PRECEDE
             , ("npreceq"                                , Right [ 0x02AAF, 0x00338 ] ) -- PRECEDES ABOVE SINGLE-LINE EQUALS SIGN with slash
             , ("nrArr"                                  , Left 0x021CF               ) -- RIGHTWARDS DOUBLE ARROW WITH STROKE
             , ("nrarr"                                  , Left 0x0219B               ) -- RIGHTWARDS ARROW WITH STROKE
             , ("nrarrc"                                 , Right [ 0x02933, 0x00338 ] ) -- WAVE ARROW POINTING DIRECTLY RIGHT with slash
             , ("nrarrw"                                 , Right [ 0x0219D, 0x00338 ] ) -- RIGHTWARDS WAVE ARROW with slash
             , ("nrightarrow"                            , Left 0x0219B               ) -- RIGHTWARDS ARROW WITH STROKE
             , ("nrtri"                                  , Left 0x022EB               ) -- DOES NOT CONTAIN AS NORMAL SUBGROUP
             , ("nrtrie"                                 , Left 0x022ED               ) -- DOES NOT CONTAIN AS NORMAL SUBGROUP OR EQUAL
             , ("nsc"                                    , Left 0x02281               ) -- DOES NOT SUCCEED
             , ("nsccue"                                 , Left 0x022E1               ) -- DOES NOT SUCCEED OR EQUAL
             , ("nsce"                                   , Right [ 0x02AB0, 0x00338 ] ) -- SUCCEEDS ABOVE SINGLE-LINE EQUALS SIGN with slash
             , ("nscr"                                   , Left 0x1D4C3               ) -- MATHEMATICAL SCRIPT SMALL N
             , ("nshortmid"                              , Left 0x02224               ) -- DOES NOT DIVIDE
             , ("nshortparallel"                         , Left 0x02226               ) -- NOT PARALLEL TO
             , ("nsim"                                   , Left 0x02241               ) -- NOT TILDE
             , ("nsime"                                  , Left 0x02244               ) -- NOT ASYMPTOTICALLY EQUAL TO
             , ("nsimeq"                                 , Left 0x02244               ) -- NOT ASYMPTOTICALLY EQUAL TO
             , ("nsmid"                                  , Left 0x02224               ) -- DOES NOT DIVIDE
             , ("nspar"                                  , Left 0x02226               ) -- NOT PARALLEL TO
             , ("nsqsube"                                , Left 0x022E2               ) -- NOT SQUARE IMAGE OF OR EQUAL TO
             , ("nsqsupe"                                , Left 0x022E3               ) -- NOT SQUARE ORIGINAL OF OR EQUAL TO
             , ("nsub"                                   , Left 0x02284               ) -- NOT A SUBSET OF
             , ("nsubE"                                  , Right [ 0x02AC5, 0x00338 ] ) -- SUBSET OF ABOVE EQUALS SIGN with slash
             , ("nsube"                                  , Left 0x02288               ) -- NEITHER A SUBSET OF NOR EQUAL TO
             , ("nsubset"                                , Right [ 0x02282, 0x020D2 ] ) -- SUBSET OF with vertical line
             , ("nsubseteq"                              , Left 0x02288               ) -- NEITHER A SUBSET OF NOR EQUAL TO
             , ("nsubseteqq"                             , Right [ 0x02AC5, 0x00338 ] ) -- SUBSET OF ABOVE EQUALS SIGN with slash
             , ("nsucc"                                  , Left 0x02281               ) -- DOES NOT SUCCEED
             , ("nsucceq"                                , Right [ 0x02AB0, 0x00338 ] ) -- SUCCEEDS ABOVE SINGLE-LINE EQUALS SIGN with slash
             , ("nsup"                                   , Left 0x02285               ) -- NOT A SUPERSET OF
             , ("nsupE"                                  , Right [ 0x02AC6, 0x00338 ] ) -- SUPERSET OF ABOVE EQUALS SIGN with slash
             , ("nsupe"                                  , Left 0x02289               ) -- NEITHER A SUPERSET OF NOR EQUAL TO
             , ("nsupset"                                , Right [ 0x02283, 0x020D2 ] ) -- SUPERSET OF with vertical line
             , ("nsupseteq"                              , Left 0x02289               ) -- NEITHER A SUPERSET OF NOR EQUAL TO
             , ("nsupseteqq"                             , Right [ 0x02AC6, 0x00338 ] ) -- SUPERSET OF ABOVE EQUALS SIGN with slash
             , ("ntgl"                                   , Left 0x02279               ) -- NEITHER GREATER-THAN NOR LESS-THAN
             , ("ntilde"                                 , Left 0x000F1               ) -- LATIN SMALL LETTER N WITH TILDE
             , ("ntlg"                                   , Left 0x02278               ) -- NEITHER LESS-THAN NOR GREATER-THAN
             , ("ntriangleleft"                          , Left 0x022EA               ) -- NOT NORMAL SUBGROUP OF
             , ("ntrianglelefteq"                        , Left 0x022EC               ) -- NOT NORMAL SUBGROUP OF OR EQUAL TO
             , ("ntriangleright"                         , Left 0x022EB               ) -- DOES NOT CONTAIN AS NORMAL SUBGROUP
             , ("ntrianglerighteq"                       , Left 0x022ED               ) -- DOES NOT CONTAIN AS NORMAL SUBGROUP OR EQUAL
             , ("nu"                                     , Left 0x003BD               ) -- GREEK SMALL LETTER NU
             , ("num"                                    , Left 0x00023               ) -- NUMBER SIGN
             , ("numero"                                 , Left 0x02116               ) -- NUMERO SIGN
             , ("numsp"                                  , Left 0x02007               ) -- FIGURE SPACE
             , ("nvDash"                                 , Left 0x022AD               ) -- NOT TRUE
             , ("nvHarr"                                 , Left 0x02904               ) -- LEFT RIGHT DOUBLE ARROW WITH VERTICAL STROKE
             , ("nvap"                                   , Right [ 0x0224D, 0x020D2 ] ) -- EQUIVALENT TO with vertical line
             , ("nvdash"                                 , Left 0x022AC               ) -- DOES NOT PROVE
             , ("nvge"                                   , Right [ 0x02265, 0x020D2 ] ) -- GREATER-THAN OR EQUAL TO with vertical line
             , ("nvgt"                                   , Right [ 0x0003E, 0x020D2 ] ) -- GREATER-THAN SIGN with vertical line
             , ("nvinfin"                                , Left 0x029DE               ) -- INFINITY NEGATED WITH VERTICAL BAR
             , ("nvlArr"                                 , Left 0x02902               ) -- LEFTWARDS DOUBLE ARROW WITH VERTICAL STROKE
             , ("nvle"                                   , Right [ 0x02264, 0x020D2 ] ) -- LESS-THAN OR EQUAL TO with vertical line
             , ("nvlt"                                   , Right [ 0x0003C, 0x020D2 ] ) -- LESS-THAN SIGN with vertical line
             , ("nvltrie"                                , Right [ 0x022B4, 0x020D2 ] ) -- NORMAL SUBGROUP OF OR EQUAL TO with vertical line
             , ("nvrArr"                                 , Left 0x02903               ) -- RIGHTWARDS DOUBLE ARROW WITH VERTICAL STROKE
             , ("nvrtrie"                                , Right [ 0x022B5, 0x020D2 ] ) -- CONTAINS AS NORMAL SUBGROUP OR EQUAL TO with vertical line
             , ("nvsim"                                  , Right [ 0x0223C, 0x020D2 ] ) -- TILDE OPERATOR with vertical line
             , ("nwArr"                                  , Left 0x021D6               ) -- NORTH WEST DOUBLE ARROW
             , ("nwarhk"                                 , Left 0x02923               ) -- NORTH WEST ARROW WITH HOOK
             , ("nwarr"                                  , Left 0x02196               ) -- NORTH WEST ARROW
             , ("nwarrow"                                , Left 0x02196               ) -- NORTH WEST ARROW
             , ("nwnear"                                 , Left 0x02927               ) -- NORTH WEST ARROW AND NORTH EAST ARROW
             , ("oS"                                     , Left 0x024C8               ) -- CIRCLED LATIN CAPITAL LETTER S
             , ("oacgr"                                  , Left 0x003CC               ) -- GREEK SMALL LETTER OMICRON WITH TONOS
             , ("oacute"                                 , Left 0x000F3               ) -- LATIN SMALL LETTER O WITH ACUTE
             , ("oast"                                   , Left 0x0229B               ) -- CIRCLED ASTERISK OPERATOR
             , ("ocir"                                   , Left 0x0229A               ) -- CIRCLED RING OPERATOR
             , ("ocirc"                                  , Left 0x000F4               ) -- LATIN SMALL LETTER O WITH CIRCUMFLEX
             , ("ocy"                                    , Left 0x0043E               ) -- CYRILLIC SMALL LETTER O
             , ("odash"                                  , Left 0x0229D               ) -- CIRCLED DASH
             , ("odblac"                                 , Left 0x00151               ) -- LATIN SMALL LETTER O WITH DOUBLE ACUTE
             , ("odiv"                                   , Left 0x02A38               ) -- CIRCLED DIVISION SIGN
             , ("odot"                                   , Left 0x02299               ) -- CIRCLED DOT OPERATOR
             , ("odsold"                                 , Left 0x029BC               ) -- CIRCLED ANTICLOCKWISE-ROTATED DIVISION SIGN
             , ("oelig"                                  , Left 0x00153               ) -- LATIN SMALL LIGATURE OE
             , ("ofcir"                                  , Left 0x029BF               ) -- CIRCLED BULLET
             , ("ofr"                                    , Left 0x1D52C               ) -- MATHEMATICAL FRAKTUR SMALL O
             , ("ogon"                                   , Left 0x002DB               ) -- OGONEK
             , ("ogr"                                    , Left 0x003BF               ) -- GREEK SMALL LETTER OMICRON
             , ("ograve"                                 , Left 0x000F2               ) -- LATIN SMALL LETTER O WITH GRAVE
             , ("ogt"                                    , Left 0x029C1               ) -- CIRCLED GREATER-THAN
             , ("ohacgr"                                 , Left 0x003CE               ) -- GREEK SMALL LETTER OMEGA WITH TONOS
             , ("ohbar"                                  , Left 0x029B5               ) -- CIRCLE WITH HORIZONTAL BAR
             , ("ohgr"                                   , Left 0x003C9               ) -- GREEK SMALL LETTER OMEGA
             , ("ohm"                                    , Left 0x003A9               ) -- GREEK CAPITAL LETTER OMEGA
             , ("oint"                                   , Left 0x0222E               ) -- CONTOUR INTEGRAL
             , ("olarr"                                  , Left 0x021BA               ) -- ANTICLOCKWISE OPEN CIRCLE ARROW
             , ("olcir"                                  , Left 0x029BE               ) -- CIRCLED WHITE BULLET
             , ("olcross"                                , Left 0x029BB               ) -- CIRCLE WITH SUPERIMPOSED X
             , ("oline"                                  , Left 0x0203E               ) -- OVERLINE
             , ("olt"                                    , Left 0x029C0               ) -- CIRCLED LESS-THAN
             , ("omacr"                                  , Left 0x0014D               ) -- LATIN SMALL LETTER O WITH MACRON
             , ("omega"                                  , Left 0x003C9               ) -- GREEK SMALL LETTER OMEGA
             , ("omicron"                                , Left 0x003BF               ) -- GREEK SMALL LETTER OMICRON
             , ("omid"                                   , Left 0x029B6               ) -- CIRCLED VERTICAL BAR
             , ("ominus"                                 , Left 0x02296               ) -- CIRCLED MINUS
             , ("oopf"                                   , Left 0x1D560               ) -- MATHEMATICAL DOUBLE-STRUCK SMALL O
             , ("opar"                                   , Left 0x029B7               ) -- CIRCLED PARALLEL
             , ("operp"                                  , Left 0x029B9               ) -- CIRCLED PERPENDICULAR
             , ("oplus"                                  , Left 0x02295               ) -- CIRCLED PLUS
             , ("or"                                     , Left 0x02228               ) -- LOGICAL OR
             , ("orarr"                                  , Left 0x021BB               ) -- CLOCKWISE OPEN CIRCLE ARROW
             , ("ord"                                    , Left 0x02A5D               ) -- LOGICAL OR WITH HORIZONTAL DASH
             , ("order"                                  , Left 0x02134               ) -- SCRIPT SMALL O
             , ("orderof"                                , Left 0x02134               ) -- SCRIPT SMALL O
             , ("ordf"                                   , Left 0x000AA               ) -- FEMININE ORDINAL INDICATOR
             , ("ordm"                                   , Left 0x000BA               ) -- MASCULINE ORDINAL INDICATOR
             , ("origof"                                 , Left 0x022B6               ) -- ORIGINAL OF
             , ("oror"                                   , Left 0x02A56               ) -- TWO INTERSECTING LOGICAL OR
             , ("orslope"                                , Left 0x02A57               ) -- SLOPING LARGE OR
             , ("orv"                                    , Left 0x02A5B               ) -- LOGICAL OR WITH MIDDLE STEM
             , ("oscr"                                   , Left 0x02134               ) -- SCRIPT SMALL O
             , ("oslash"                                 , Left 0x000F8               ) -- LATIN SMALL LETTER O WITH STROKE
             , ("osol"                                   , Left 0x02298               ) -- CIRCLED DIVISION SLASH
             , ("otilde"                                 , Left 0x000F5               ) -- LATIN SMALL LETTER O WITH TILDE
             , ("otimes"                                 , Left 0x02297               ) -- CIRCLED TIMES
             , ("otimesas"                               , Left 0x02A36               ) -- CIRCLED MULTIPLICATION SIGN WITH CIRCUMFLEX ACCENT
             , ("ouml"                                   , Left 0x000F6               ) -- LATIN SMALL LETTER O WITH DIAERESIS
             , ("ovbar"                                  , Left 0x0233D               ) -- APL FUNCTIONAL SYMBOL CIRCLE STILE
             , ("par"                                    , Left 0x02225               ) -- PARALLEL TO
             , ("para"                                   , Left 0x000B6               ) -- PILCROW SIGN
             , ("parallel"                               , Left 0x02225               ) -- PARALLEL TO
             , ("parsim"                                 , Left 0x02AF3               ) -- PARALLEL WITH TILDE OPERATOR
             , ("parsl"                                  , Left 0x02AFD               ) -- DOUBLE SOLIDUS OPERATOR
             , ("part"                                   , Left 0x02202               ) -- PARTIAL DIFFERENTIAL
             , ("pcy"                                    , Left 0x0043F               ) -- CYRILLIC SMALL LETTER PE
             , ("percnt"                                 , Left 0x00025               ) -- PERCENT SIGN
             , ("period"                                 , Left 0x0002E               ) -- FULL STOP
             , ("permil"                                 , Left 0x02030               ) -- PER MILLE SIGN
             , ("perp"                                   , Left 0x022A5               ) -- UP TACK
             , ("pertenk"                                , Left 0x02031               ) -- PER TEN THOUSAND SIGN
             , ("pfr"                                    , Left 0x1D52D               ) -- MATHEMATICAL FRAKTUR SMALL P
             , ("pgr"                                    , Left 0x003C0               ) -- GREEK SMALL LETTER PI
             , ("phgr"                                   , Left 0x003C6               ) -- GREEK SMALL LETTER PHI
             , ("phi"                                    , Left 0x003C6               ) -- GREEK SMALL LETTER PHI
             , ("phiv"                                   , Left 0x003D5               ) -- GREEK PHI SYMBOL
             , ("phmmat"                                 , Left 0x02133               ) -- SCRIPT CAPITAL M
             , ("phone"                                  , Left 0x0260E               ) -- BLACK TELEPHONE
             , ("pi"                                     , Left 0x003C0               ) -- GREEK SMALL LETTER PI
             , ("pitchfork"                              , Left 0x022D4               ) -- PITCHFORK
             , ("piv"                                    , Left 0x003D6               ) -- GREEK PI SYMBOL
             , ("planck"                                 , Left 0x0210F               ) -- PLANCK CONSTANT OVER TWO PI
             , ("planckh"                                , Left 0x0210E               ) -- PLANCK CONSTANT
             , ("plankv"                                 , Left 0x0210F               ) -- PLANCK CONSTANT OVER TWO PI
             , ("plus"                                   , Left 0x0002B               ) -- PLUS SIGN
             , ("plusacir"                               , Left 0x02A23               ) -- PLUS SIGN WITH CIRCUMFLEX ACCENT ABOVE
             , ("plusb"                                  , Left 0x0229E               ) -- SQUARED PLUS
             , ("pluscir"                                , Left 0x02A22               ) -- PLUS SIGN WITH SMALL CIRCLE ABOVE
             , ("plusdo"                                 , Left 0x02214               ) -- DOT PLUS
             , ("plusdu"                                 , Left 0x02A25               ) -- PLUS SIGN WITH DOT BELOW
             , ("pluse"                                  , Left 0x02A72               ) -- PLUS SIGN ABOVE EQUALS SIGN
             , ("plusmn"                                 , Left 0x000B1               ) -- PLUS-MINUS SIGN
             , ("plussim"                                , Left 0x02A26               ) -- PLUS SIGN WITH TILDE BELOW
             , ("plustwo"                                , Left 0x02A27               ) -- PLUS SIGN WITH SUBSCRIPT TWO
             , ("pm"                                     , Left 0x000B1               ) -- PLUS-MINUS SIGN
             , ("pointint"                               , Left 0x02A15               ) -- INTEGRAL AROUND A POINT OPERATOR
             , ("popf"                                   , Left 0x1D561               ) -- MATHEMATICAL DOUBLE-STRUCK SMALL P
             , ("pound"                                  , Left 0x000A3               ) -- POUND SIGN
             , ("pr"                                     , Left 0x0227A               ) -- PRECEDES
             , ("prE"                                    , Left 0x02AB3               ) -- PRECEDES ABOVE EQUALS SIGN
             , ("prap"                                   , Left 0x02AB7               ) -- PRECEDES ABOVE ALMOST EQUAL TO
             , ("prcue"                                  , Left 0x0227C               ) -- PRECEDES OR EQUAL TO
             , ("pre"                                    , Left 0x02AAF               ) -- PRECEDES ABOVE SINGLE-LINE EQUALS SIGN
             , ("prec"                                   , Left 0x0227A               ) -- PRECEDES
             , ("precapprox"                             , Left 0x02AB7               ) -- PRECEDES ABOVE ALMOST EQUAL TO
             , ("preccurlyeq"                            , Left 0x0227C               ) -- PRECEDES OR EQUAL TO
             , ("preceq"                                 , Left 0x02AAF               ) -- PRECEDES ABOVE SINGLE-LINE EQUALS SIGN
             , ("precnapprox"                            , Left 0x02AB9               ) -- PRECEDES ABOVE NOT ALMOST EQUAL TO
             , ("precneqq"                               , Left 0x02AB5               ) -- PRECEDES ABOVE NOT EQUAL TO
             , ("precnsim"                               , Left 0x022E8               ) -- PRECEDES BUT NOT EQUIVALENT TO
             , ("precsim"                                , Left 0x0227E               ) -- PRECEDES OR EQUIVALENT TO
             , ("prime"                                  , Left 0x02032               ) -- PRIME
             , ("primes"                                 , Left 0x02119               ) -- DOUBLE-STRUCK CAPITAL P
             , ("prnE"                                   , Left 0x02AB5               ) -- PRECEDES ABOVE NOT EQUAL TO
             , ("prnap"                                  , Left 0x02AB9               ) -- PRECEDES ABOVE NOT ALMOST EQUAL TO
             , ("prnsim"                                 , Left 0x022E8               ) -- PRECEDES BUT NOT EQUIVALENT TO
             , ("prod"                                   , Left 0x0220F               ) -- N-ARY PRODUCT
             , ("profalar"                               , Left 0x0232E               ) -- ALL AROUND-PROFILE
             , ("profline"                               , Left 0x02312               ) -- ARC
             , ("profsurf"                               , Left 0x02313               ) -- SEGMENT
             , ("prop"                                   , Left 0x0221D               ) -- PROPORTIONAL TO
             , ("propto"                                 , Left 0x0221D               ) -- PROPORTIONAL TO
             , ("prsim"                                  , Left 0x0227E               ) -- PRECEDES OR EQUIVALENT TO
             , ("prurel"                                 , Left 0x022B0               ) -- PRECEDES UNDER RELATION
             , ("pscr"                                   , Left 0x1D4C5               ) -- MATHEMATICAL SCRIPT SMALL P
             , ("psgr"                                   , Left 0x003C8               ) -- GREEK SMALL LETTER PSI
             , ("psi"                                    , Left 0x003C8               ) -- GREEK SMALL LETTER PSI
             , ("puncsp"                                 , Left 0x02008               ) -- PUNCTUATION SPACE
             , ("qfr"                                    , Left 0x1D52E               ) -- MATHEMATICAL FRAKTUR SMALL Q
             , ("qint"                                   , Left 0x02A0C               ) -- QUADRUPLE INTEGRAL OPERATOR
             , ("qopf"                                   , Left 0x1D562               ) -- MATHEMATICAL DOUBLE-STRUCK SMALL Q
             , ("qprime"                                 , Left 0x02057               ) -- QUADRUPLE PRIME
             , ("qscr"                                   , Left 0x1D4C6               ) -- MATHEMATICAL SCRIPT SMALL Q
             , ("quaternions"                            , Left 0x0210D               ) -- DOUBLE-STRUCK CAPITAL H
             , ("quatint"                                , Left 0x02A16               ) -- QUATERNION INTEGRAL OPERATOR
             , ("quest"                                  , Left 0x0003F               ) -- QUESTION MARK
             , ("questeq"                                , Left 0x0225F               ) -- QUESTIONED EQUAL TO
             , ("quot"                                   , Left 0x00022               ) -- QUOTATION MARK
             , ("rAarr"                                  , Left 0x021DB               ) -- RIGHTWARDS TRIPLE ARROW
             , ("rArr"                                   , Left 0x021D2               ) -- RIGHTWARDS DOUBLE ARROW
             , ("rAtail"                                 , Left 0x0291C               ) -- RIGHTWARDS DOUBLE ARROW-TAIL
             , ("rBarr"                                  , Left 0x0290F               ) -- RIGHTWARDS TRIPLE DASH ARROW
             , ("rHar"                                   , Left 0x02964               ) -- RIGHTWARDS HARPOON WITH BARB UP ABOVE RIGHTWARDS HARPOON WITH BARB DOWN
             , ("race"                                   , Right [ 0x0223D, 0x00331 ] ) -- REVERSED TILDE with underline
             , ("racute"                                 , Left 0x00155               ) -- LATIN SMALL LETTER R WITH ACUTE
             , ("radic"                                  , Left 0x0221A               ) -- SQUARE ROOT
             , ("raemptyv"                               , Left 0x029B3               ) -- EMPTY SET WITH RIGHT ARROW ABOVE
             , ("rang"                                   , Left 0x027E9               ) -- MATHEMATICAL RIGHT ANGLE BRACKET
             , ("rangd"                                  , Left 0x02992               ) -- RIGHT ANGLE BRACKET WITH DOT
             , ("range"                                  , Left 0x029A5               ) -- REVERSED ANGLE WITH UNDERBAR
             , ("rangle"                                 , Left 0x027E9               ) -- MATHEMATICAL RIGHT ANGLE BRACKET
             , ("raquo"                                  , Left 0x000BB               ) -- RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
             , ("rarr"                                   , Left 0x02192               ) -- RIGHTWARDS ARROW
             , ("rarrap"                                 , Left 0x02975               ) -- RIGHTWARDS ARROW ABOVE ALMOST EQUAL TO
             , ("rarrb"                                  , Left 0x021E5               ) -- RIGHTWARDS ARROW TO BAR
             , ("rarrbfs"                                , Left 0x02920               ) -- RIGHTWARDS ARROW FROM BAR TO BLACK DIAMOND
             , ("rarrc"                                  , Left 0x02933               ) -- WAVE ARROW POINTING DIRECTLY RIGHT
             , ("rarrfs"                                 , Left 0x0291E               ) -- RIGHTWARDS ARROW TO BLACK DIAMOND
             , ("rarrhk"                                 , Left 0x021AA               ) -- RIGHTWARDS ARROW WITH HOOK
             , ("rarrlp"                                 , Left 0x021AC               ) -- RIGHTWARDS ARROW WITH LOOP
             , ("rarrpl"                                 , Left 0x02945               ) -- RIGHTWARDS ARROW WITH PLUS BELOW
             , ("rarrsim"                                , Left 0x02974               ) -- RIGHTWARDS ARROW ABOVE TILDE OPERATOR
             , ("rarrtl"                                 , Left 0x021A3               ) -- RIGHTWARDS ARROW WITH TAIL
             , ("rarrw"                                  , Left 0x0219D               ) -- RIGHTWARDS WAVE ARROW
             , ("ratail"                                 , Left 0x0291A               ) -- RIGHTWARDS ARROW-TAIL
             , ("ratio"                                  , Left 0x02236               ) -- RATIO
             , ("rationals"                              , Left 0x0211A               ) -- DOUBLE-STRUCK CAPITAL Q
             , ("rbarr"                                  , Left 0x0290D               ) -- RIGHTWARDS DOUBLE DASH ARROW
             , ("rbbrk"                                  , Left 0x02773               ) -- LIGHT RIGHT TORTOISE SHELL BRACKET ORNAMENT
             , ("rbrace"                                 , Left 0x0007D               ) -- RIGHT CURLY BRACKET
             , ("rbrack"                                 , Left 0x0005D               ) -- RIGHT SQUARE BRACKET
             , ("rbrke"                                  , Left 0x0298C               ) -- RIGHT SQUARE BRACKET WITH UNDERBAR
             , ("rbrksld"                                , Left 0x0298E               ) -- RIGHT SQUARE BRACKET WITH TICK IN BOTTOM CORNER
             , ("rbrkslu"                                , Left 0x02990               ) -- RIGHT SQUARE BRACKET WITH TICK IN TOP CORNER
             , ("rcaron"                                 , Left 0x00159               ) -- LATIN SMALL LETTER R WITH CARON
             , ("rcedil"                                 , Left 0x00157               ) -- LATIN SMALL LETTER R WITH CEDILLA
             , ("rceil"                                  , Left 0x02309               ) -- RIGHT CEILING
             , ("rcub"                                   , Left 0x0007D               ) -- RIGHT CURLY BRACKET
             , ("rcy"                                    , Left 0x00440               ) -- CYRILLIC SMALL LETTER ER
             , ("rdca"                                   , Left 0x02937               ) -- ARROW POINTING DOWNWARDS THEN CURVING RIGHTWARDS
             , ("rdldhar"                                , Left 0x02969               ) -- RIGHTWARDS HARPOON WITH BARB DOWN ABOVE LEFTWARDS HARPOON WITH BARB DOWN
             , ("rdquo"                                  , Left 0x0201D               ) -- RIGHT DOUBLE QUOTATION MARK
             , ("rdquor"                                 , Left 0x0201D               ) -- RIGHT DOUBLE QUOTATION MARK
             , ("rdsh"                                   , Left 0x021B3               ) -- DOWNWARDS ARROW WITH TIP RIGHTWARDS
             , ("real"                                   , Left 0x0211C               ) -- BLACK-LETTER CAPITAL R
             , ("realine"                                , Left 0x0211B               ) -- SCRIPT CAPITAL R
             , ("realpart"                               , Left 0x0211C               ) -- BLACK-LETTER CAPITAL R
             , ("reals"                                  , Left 0x0211D               ) -- DOUBLE-STRUCK CAPITAL R
             , ("rect"                                   , Left 0x025AD               ) -- WHITE RECTANGLE
             , ("reg"                                    , Left 0x000AE               ) -- REGISTERED SIGN
             , ("rfisht"                                 , Left 0x0297D               ) -- RIGHT FISH TAIL
             , ("rfloor"                                 , Left 0x0230B               ) -- RIGHT FLOOR
             , ("rfr"                                    , Left 0x1D52F               ) -- MATHEMATICAL FRAKTUR SMALL R
             , ("rgr"                                    , Left 0x003C1               ) -- GREEK SMALL LETTER RHO
             , ("rhard"                                  , Left 0x021C1               ) -- RIGHTWARDS HARPOON WITH BARB DOWNWARDS
             , ("rharu"                                  , Left 0x021C0               ) -- RIGHTWARDS HARPOON WITH BARB UPWARDS
             , ("rharul"                                 , Left 0x0296C               ) -- RIGHTWARDS HARPOON WITH BARB UP ABOVE LONG DASH
             , ("rho"                                    , Left 0x003C1               ) -- GREEK SMALL LETTER RHO
             , ("rhov"                                   , Left 0x003F1               ) -- GREEK RHO SYMBOL
             , ("rightarrow"                             , Left 0x02192               ) -- RIGHTWARDS ARROW
             , ("rightarrowtail"                         , Left 0x021A3               ) -- RIGHTWARDS ARROW WITH TAIL
             , ("rightharpoondown"                       , Left 0x021C1               ) -- RIGHTWARDS HARPOON WITH BARB DOWNWARDS
             , ("rightharpoonup"                         , Left 0x021C0               ) -- RIGHTWARDS HARPOON WITH BARB UPWARDS
             , ("rightleftarrows"                        , Left 0x021C4               ) -- RIGHTWARDS ARROW OVER LEFTWARDS ARROW
             , ("rightleftharpoons"                      , Left 0x021CC               ) -- RIGHTWARDS HARPOON OVER LEFTWARDS HARPOON
             , ("rightrightarrows"                       , Left 0x021C9               ) -- RIGHTWARDS PAIRED ARROWS
             , ("rightsquigarrow"                        , Left 0x0219D               ) -- RIGHTWARDS WAVE ARROW
             , ("rightthreetimes"                        , Left 0x022CC               ) -- RIGHT SEMIDIRECT PRODUCT
             , ("ring"                                   , Left 0x002DA               ) -- RING ABOVE
             , ("risingdotseq"                           , Left 0x02253               ) -- IMAGE OF OR APPROXIMATELY EQUAL TO
             , ("rlarr"                                  , Left 0x021C4               ) -- RIGHTWARDS ARROW OVER LEFTWARDS ARROW
             , ("rlhar"                                  , Left 0x021CC               ) -- RIGHTWARDS HARPOON OVER LEFTWARDS HARPOON
             , ("rlm"                                    , Left 0x0200F               ) -- RIGHT-TO-LEFT MARK
             , ("rmoust"                                 , Left 0x023B1               ) -- UPPER RIGHT OR LOWER LEFT CURLY BRACKET SECTION
             , ("rmoustache"                             , Left 0x023B1               ) -- UPPER RIGHT OR LOWER LEFT CURLY BRACKET SECTION
             , ("rnmid"                                  , Left 0x02AEE               ) -- DOES NOT DIVIDE WITH REVERSED NEGATION SLASH
             , ("roang"                                  , Left 0x027ED               ) -- MATHEMATICAL RIGHT WHITE TORTOISE SHELL BRACKET
             , ("roarr"                                  , Left 0x021FE               ) -- RIGHTWARDS OPEN-HEADED ARROW
             , ("robrk"                                  , Left 0x027E7               ) -- MATHEMATICAL RIGHT WHITE SQUARE BRACKET
             , ("ropar"                                  , Left 0x02986               ) -- RIGHT WHITE PARENTHESIS
             , ("ropf"                                   , Left 0x1D563               ) -- MATHEMATICAL DOUBLE-STRUCK SMALL R
             , ("roplus"                                 , Left 0x02A2E               ) -- PLUS SIGN IN RIGHT HALF CIRCLE
             , ("rotimes"                                , Left 0x02A35               ) -- MULTIPLICATION SIGN IN RIGHT HALF CIRCLE
             , ("rpar"                                   , Left 0x00029               ) -- RIGHT PARENTHESIS
             , ("rpargt"                                 , Left 0x02994               ) -- RIGHT ARC GREATER-THAN BRACKET
             , ("rppolint"                               , Left 0x02A12               ) -- LINE INTEGRATION WITH RECTANGULAR PATH AROUND POLE
             , ("rrarr"                                  , Left 0x021C9               ) -- RIGHTWARDS PAIRED ARROWS
             , ("rsaquo"                                 , Left 0x0203A               ) -- SINGLE RIGHT-POINTING ANGLE QUOTATION MARK
             , ("rscr"                                   , Left 0x1D4C7               ) -- MATHEMATICAL SCRIPT SMALL R
             , ("rsh"                                    , Left 0x021B1               ) -- UPWARDS ARROW WITH TIP RIGHTWARDS
             , ("rsqb"                                   , Left 0x0005D               ) -- RIGHT SQUARE BRACKET
             , ("rsquo"                                  , Left 0x02019               ) -- RIGHT SINGLE QUOTATION MARK
             , ("rsquor"                                 , Left 0x02019               ) -- RIGHT SINGLE QUOTATION MARK
             , ("rthree"                                 , Left 0x022CC               ) -- RIGHT SEMIDIRECT PRODUCT
             , ("rtimes"                                 , Left 0x022CA               ) -- RIGHT NORMAL FACTOR SEMIDIRECT PRODUCT
             , ("rtri"                                   , Left 0x025B9               ) -- WHITE RIGHT-POINTING SMALL TRIANGLE
             , ("rtrie"                                  , Left 0x022B5               ) -- CONTAINS AS NORMAL SUBGROUP OR EQUAL TO
             , ("rtrif"                                  , Left 0x025B8               ) -- BLACK RIGHT-POINTING SMALL TRIANGLE
             , ("rtriltri"                               , Left 0x029CE               ) -- RIGHT TRIANGLE ABOVE LEFT TRIANGLE
             , ("ruluhar"                                , Left 0x02968               ) -- RIGHTWARDS HARPOON WITH BARB UP ABOVE LEFTWARDS HARPOON WITH BARB UP
             , ("rx"                                     , Left 0x0211E               ) -- PRESCRIPTION TAKE
             , ("sacute"                                 , Left 0x0015B               ) -- LATIN SMALL LETTER S WITH ACUTE
             , ("sbquo"                                  , Left 0x0201A               ) -- SINGLE LOW-9 QUOTATION MARK
             , ("sc"                                     , Left 0x0227B               ) -- SUCCEEDS
             , ("scE"                                    , Left 0x02AB4               ) -- SUCCEEDS ABOVE EQUALS SIGN
             , ("scap"                                   , Left 0x02AB8               ) -- SUCCEEDS ABOVE ALMOST EQUAL TO
             , ("scaron"                                 , Left 0x00161               ) -- LATIN SMALL LETTER S WITH CARON
             , ("sccue"                                  , Left 0x0227D               ) -- SUCCEEDS OR EQUAL TO
             , ("sce"                                    , Left 0x02AB0               ) -- SUCCEEDS ABOVE SINGLE-LINE EQUALS SIGN
             , ("scedil"                                 , Left 0x0015F               ) -- LATIN SMALL LETTER S WITH CEDILLA
             , ("scirc"                                  , Left 0x0015D               ) -- LATIN SMALL LETTER S WITH CIRCUMFLEX
             , ("scnE"                                   , Left 0x02AB6               ) -- SUCCEEDS ABOVE NOT EQUAL TO
             , ("scnap"                                  , Left 0x02ABA               ) -- SUCCEEDS ABOVE NOT ALMOST EQUAL TO
             , ("scnsim"                                 , Left 0x022E9               ) -- SUCCEEDS BUT NOT EQUIVALENT TO
             , ("scpolint"                               , Left 0x02A13               ) -- LINE INTEGRATION WITH SEMICIRCULAR PATH AROUND POLE
             , ("scsim"                                  , Left 0x0227F               ) -- SUCCEEDS OR EQUIVALENT TO
             , ("scy"                                    , Left 0x00441               ) -- CYRILLIC SMALL LETTER ES
             , ("sdot"                                   , Left 0x022C5               ) -- DOT OPERATOR
             , ("sdotb"                                  , Left 0x022A1               ) -- SQUARED DOT OPERATOR
             , ("sdote"                                  , Left 0x02A66               ) -- EQUALS SIGN WITH DOT BELOW
             , ("seArr"                                  , Left 0x021D8               ) -- SOUTH EAST DOUBLE ARROW
             , ("searhk"                                 , Left 0x02925               ) -- SOUTH EAST ARROW WITH HOOK
             , ("searr"                                  , Left 0x02198               ) -- SOUTH EAST ARROW
             , ("searrow"                                , Left 0x02198               ) -- SOUTH EAST ARROW
             , ("sect"                                   , Left 0x000A7               ) -- SECTION SIGN
             , ("semi"                                   , Left 0x0003B               ) -- SEMICOLON
             , ("seswar"                                 , Left 0x02929               ) -- SOUTH EAST ARROW AND SOUTH WEST ARROW
             , ("setminus"                               , Left 0x02216               ) -- SET MINUS
             , ("setmn"                                  , Left 0x02216               ) -- SET MINUS
             , ("sext"                                   , Left 0x02736               ) -- SIX POINTED BLACK STAR
             , ("sfgr"                                   , Left 0x003C2               ) -- GREEK SMALL LETTER FINAL SIGMA
             , ("sfr"                                    , Left 0x1D530               ) -- MATHEMATICAL FRAKTUR SMALL S
             , ("sfrown"                                 , Left 0x02322               ) -- FROWN
             , ("sgr"                                    , Left 0x003C3               ) -- GREEK SMALL LETTER SIGMA
             , ("sharp"                                  , Left 0x0266F               ) -- MUSIC SHARP SIGN
             , ("shchcy"                                 , Left 0x00449               ) -- CYRILLIC SMALL LETTER SHCHA
             , ("shcy"                                   , Left 0x00448               ) -- CYRILLIC SMALL LETTER SHA
             , ("shortmid"                               , Left 0x02223               ) -- DIVIDES
             , ("shortparallel"                          , Left 0x02225               ) -- PARALLEL TO
             , ("shy"                                    , Left 0x000AD               ) -- SOFT HYPHEN
             , ("sigma"                                  , Left 0x003C3               ) -- GREEK SMALL LETTER SIGMA
             , ("sigmaf"                                 , Left 0x003C2               ) -- GREEK SMALL LETTER FINAL SIGMA
             , ("sigmav"                                 , Left 0x003C2               ) -- GREEK SMALL LETTER FINAL SIGMA
             , ("sim"                                    , Left 0x0223C               ) -- TILDE OPERATOR
             , ("simdot"                                 , Left 0x02A6A               ) -- TILDE OPERATOR WITH DOT ABOVE
             , ("sime"                                   , Left 0x02243               ) -- ASYMPTOTICALLY EQUAL TO
             , ("simeq"                                  , Left 0x02243               ) -- ASYMPTOTICALLY EQUAL TO
             , ("simg"                                   , Left 0x02A9E               ) -- SIMILAR OR GREATER-THAN
             , ("simgE"                                  , Left 0x02AA0               ) -- SIMILAR ABOVE GREATER-THAN ABOVE EQUALS SIGN
             , ("siml"                                   , Left 0x02A9D               ) -- SIMILAR OR LESS-THAN
             , ("simlE"                                  , Left 0x02A9F               ) -- SIMILAR ABOVE LESS-THAN ABOVE EQUALS SIGN
             , ("simne"                                  , Left 0x02246               ) -- APPROXIMATELY BUT NOT ACTUALLY EQUAL TO
             , ("simplus"                                , Left 0x02A24               ) -- PLUS SIGN WITH TILDE ABOVE
             , ("simrarr"                                , Left 0x02972               ) -- TILDE OPERATOR ABOVE RIGHTWARDS ARROW
             , ("slarr"                                  , Left 0x02190               ) -- LEFTWARDS ARROW
             , ("smallsetminus"                          , Left 0x02216               ) -- SET MINUS
             , ("smashp"                                 , Left 0x02A33               ) -- SMASH PRODUCT
             , ("smeparsl"                               , Left 0x029E4               ) -- EQUALS SIGN AND SLANTED PARALLEL WITH TILDE ABOVE
             , ("smid"                                   , Left 0x02223               ) -- DIVIDES
             , ("smile"                                  , Left 0x02323               ) -- SMILE
             , ("smt"                                    , Left 0x02AAA               ) -- SMALLER THAN
             , ("smte"                                   , Left 0x02AAC               ) -- SMALLER THAN OR EQUAL TO
             , ("smtes"                                  , Right [ 0x02AAC, 0x0FE00 ] ) -- SMALLER THAN OR slanted EQUAL
             , ("softcy"                                 , Left 0x0044C               ) -- CYRILLIC SMALL LETTER SOFT SIGN
             , ("sol"                                    , Left 0x0002F               ) -- SOLIDUS
             , ("solb"                                   , Left 0x029C4               ) -- SQUARED RISING DIAGONAL SLASH
             , ("solbar"                                 , Left 0x0233F               ) -- APL FUNCTIONAL SYMBOL SLASH BAR
             , ("sopf"                                   , Left 0x1D564               ) -- MATHEMATICAL DOUBLE-STRUCK SMALL S
             , ("spades"                                 , Left 0x02660               ) -- BLACK SPADE SUIT
             , ("spadesuit"                              , Left 0x02660               ) -- BLACK SPADE SUIT
             , ("spar"                                   , Left 0x02225               ) -- PARALLEL TO
             , ("sqcap"                                  , Left 0x02293               ) -- SQUARE CAP
             , ("sqcaps"                                 , Right [ 0x02293, 0x0FE00 ] ) -- SQUARE CAP with serifs
             , ("sqcup"                                  , Left 0x02294               ) -- SQUARE CUP
             , ("sqcups"                                 , Right [ 0x02294, 0x0FE00 ] ) -- SQUARE CUP with serifs
             , ("sqsub"                                  , Left 0x0228F               ) -- SQUARE IMAGE OF
             , ("sqsube"                                 , Left 0x02291               ) -- SQUARE IMAGE OF OR EQUAL TO
             , ("sqsubset"                               , Left 0x0228F               ) -- SQUARE IMAGE OF
             , ("sqsubseteq"                             , Left 0x02291               ) -- SQUARE IMAGE OF OR EQUAL TO
             , ("sqsup"                                  , Left 0x02290               ) -- SQUARE ORIGINAL OF
             , ("sqsupe"                                 , Left 0x02292               ) -- SQUARE ORIGINAL OF OR EQUAL TO
             , ("sqsupset"                               , Left 0x02290               ) -- SQUARE ORIGINAL OF
             , ("sqsupseteq"                             , Left 0x02292               ) -- SQUARE ORIGINAL OF OR EQUAL TO
             , ("squ"                                    , Left 0x025A1               ) -- WHITE SQUARE
             , ("square"                                 , Left 0x025A1               ) -- WHITE SQUARE
             , ("squarf"                                 , Left 0x025AA               ) -- BLACK SMALL SQUARE
             , ("squf"                                   , Left 0x025AA               ) -- BLACK SMALL SQUARE
             , ("srarr"                                  , Left 0x02192               ) -- RIGHTWARDS ARROW
             , ("sscr"                                   , Left 0x1D4C8               ) -- MATHEMATICAL SCRIPT SMALL S
             , ("ssetmn"                                 , Left 0x02216               ) -- SET MINUS
             , ("ssmile"                                 , Left 0x02323               ) -- SMILE
             , ("sstarf"                                 , Left 0x022C6               ) -- STAR OPERATOR
             , ("star"                                   , Left 0x02606               ) -- WHITE STAR
             , ("starf"                                  , Left 0x02605               ) -- BLACK STAR
             , ("straightepsilon"                        , Left 0x003F5               ) -- GREEK LUNATE EPSILON SYMBOL
             , ("straightphi"                            , Left 0x003D5               ) -- GREEK PHI SYMBOL
             , ("strns"                                  , Left 0x000AF               ) -- MACRON
             , ("sub"                                    , Left 0x02282               ) -- SUBSET OF
             , ("subE"                                   , Left 0x02AC5               ) -- SUBSET OF ABOVE EQUALS SIGN
             , ("subdot"                                 , Left 0x02ABD               ) -- SUBSET WITH DOT
             , ("sube"                                   , Left 0x02286               ) -- SUBSET OF OR EQUAL TO
             , ("subedot"                                , Left 0x02AC3               ) -- SUBSET OF OR EQUAL TO WITH DOT ABOVE
             , ("submult"                                , Left 0x02AC1               ) -- SUBSET WITH MULTIPLICATION SIGN BELOW
             , ("subnE"                                  , Left 0x02ACB               ) -- SUBSET OF ABOVE NOT EQUAL TO
             , ("subne"                                  , Left 0x0228A               ) -- SUBSET OF WITH NOT EQUAL TO
             , ("subplus"                                , Left 0x02ABF               ) -- SUBSET WITH PLUS SIGN BELOW
             , ("subrarr"                                , Left 0x02979               ) -- SUBSET ABOVE RIGHTWARDS ARROW
             , ("subset"                                 , Left 0x02282               ) -- SUBSET OF
             , ("subseteq"                               , Left 0x02286               ) -- SUBSET OF OR EQUAL TO
             , ("subseteqq"                              , Left 0x02AC5               ) -- SUBSET OF ABOVE EQUALS SIGN
             , ("subsetneq"                              , Left 0x0228A               ) -- SUBSET OF WITH NOT EQUAL TO
             , ("subsetneqq"                             , Left 0x02ACB               ) -- SUBSET OF ABOVE NOT EQUAL TO
             , ("subsim"                                 , Left 0x02AC7               ) -- SUBSET OF ABOVE TILDE OPERATOR
             , ("subsub"                                 , Left 0x02AD5               ) -- SUBSET ABOVE SUBSET
             , ("subsup"                                 , Left 0x02AD3               ) -- SUBSET ABOVE SUPERSET
             , ("succ"                                   , Left 0x0227B               ) -- SUCCEEDS
             , ("succapprox"                             , Left 0x02AB8               ) -- SUCCEEDS ABOVE ALMOST EQUAL TO
             , ("succcurlyeq"                            , Left 0x0227D               ) -- SUCCEEDS OR EQUAL TO
             , ("succeq"                                 , Left 0x02AB0               ) -- SUCCEEDS ABOVE SINGLE-LINE EQUALS SIGN
             , ("succnapprox"                            , Left 0x02ABA               ) -- SUCCEEDS ABOVE NOT ALMOST EQUAL TO
             , ("succneqq"                               , Left 0x02AB6               ) -- SUCCEEDS ABOVE NOT EQUAL TO
             , ("succnsim"                               , Left 0x022E9               ) -- SUCCEEDS BUT NOT EQUIVALENT TO
             , ("succsim"                                , Left 0x0227F               ) -- SUCCEEDS OR EQUIVALENT TO
             , ("sum"                                    , Left 0x02211               ) -- N-ARY SUMMATION
             , ("sung"                                   , Left 0x0266A               ) -- EIGHTH NOTE
             , ("sup"                                    , Left 0x02283               ) -- SUPERSET OF
             , ("sup1"                                   , Left 0x000B9               ) -- SUPERSCRIPT ONE
             , ("sup2"                                   , Left 0x000B2               ) -- SUPERSCRIPT TWO
             , ("sup3"                                   , Left 0x000B3               ) -- SUPERSCRIPT THREE
             , ("supE"                                   , Left 0x02AC6               ) -- SUPERSET OF ABOVE EQUALS SIGN
             , ("supdot"                                 , Left 0x02ABE               ) -- SUPERSET WITH DOT
             , ("supdsub"                                , Left 0x02AD8               ) -- SUPERSET BESIDE AND JOINED BY DASH WITH SUBSET
             , ("supe"                                   , Left 0x02287               ) -- SUPERSET OF OR EQUAL TO
             , ("supedot"                                , Left 0x02AC4               ) -- SUPERSET OF OR EQUAL TO WITH DOT ABOVE
             , ("suphsol"                                , Left 0x027C9               ) -- SUPERSET PRECEDING SOLIDUS
             , ("suphsub"                                , Left 0x02AD7               ) -- SUPERSET BESIDE SUBSET
             , ("suplarr"                                , Left 0x0297B               ) -- SUPERSET ABOVE LEFTWARDS ARROW
             , ("supmult"                                , Left 0x02AC2               ) -- SUPERSET WITH MULTIPLICATION SIGN BELOW
             , ("supnE"                                  , Left 0x02ACC               ) -- SUPERSET OF ABOVE NOT EQUAL TO
             , ("supne"                                  , Left 0x0228B               ) -- SUPERSET OF WITH NOT EQUAL TO
             , ("supplus"                                , Left 0x02AC0               ) -- SUPERSET WITH PLUS SIGN BELOW
             , ("supset"                                 , Left 0x02283               ) -- SUPERSET OF
             , ("supseteq"                               , Left 0x02287               ) -- SUPERSET OF OR EQUAL TO
             , ("supseteqq"                              , Left 0x02AC6               ) -- SUPERSET OF ABOVE EQUALS SIGN
             , ("supsetneq"                              , Left 0x0228B               ) -- SUPERSET OF WITH NOT EQUAL TO
             , ("supsetneqq"                             , Left 0x02ACC               ) -- SUPERSET OF ABOVE NOT EQUAL TO
             , ("supsim"                                 , Left 0x02AC8               ) -- SUPERSET OF ABOVE TILDE OPERATOR
             , ("supsub"                                 , Left 0x02AD4               ) -- SUPERSET ABOVE SUBSET
             , ("supsup"                                 , Left 0x02AD6               ) -- SUPERSET ABOVE SUPERSET
             , ("swArr"                                  , Left 0x021D9               ) -- SOUTH WEST DOUBLE ARROW
             , ("swarhk"                                 , Left 0x02926               ) -- SOUTH WEST ARROW WITH HOOK
             , ("swarr"                                  , Left 0x02199               ) -- SOUTH WEST ARROW
             , ("swarrow"                                , Left 0x02199               ) -- SOUTH WEST ARROW
             , ("swnwar"                                 , Left 0x0292A               ) -- SOUTH WEST ARROW AND NORTH WEST ARROW
             , ("szlig"                                  , Left 0x000DF               ) -- LATIN SMALL LETTER SHARP S
             , ("target"                                 , Left 0x02316               ) -- POSITION INDICATOR
             , ("tau"                                    , Left 0x003C4               ) -- GREEK SMALL LETTER TAU
             , ("tbrk"                                   , Left 0x023B4               ) -- TOP SQUARE BRACKET
             , ("tcaron"                                 , Left 0x00165               ) -- LATIN SMALL LETTER T WITH CARON
             , ("tcedil"                                 , Left 0x00163               ) -- LATIN SMALL LETTER T WITH CEDILLA
             , ("tcy"                                    , Left 0x00442               ) -- CYRILLIC SMALL LETTER TE
             , ("tdot"                                   , Left 0x020DB               ) -- COMBINING THREE DOTS ABOVE
             , ("telrec"                                 , Left 0x02315               ) -- TELEPHONE RECORDER
             , ("tfr"                                    , Left 0x1D531               ) -- MATHEMATICAL FRAKTUR SMALL T
             , ("tgr"                                    , Left 0x003C4               ) -- GREEK SMALL LETTER TAU
             , ("there4"                                 , Left 0x02234               ) -- THEREFORE
             , ("therefore"                              , Left 0x02234               ) -- THEREFORE
             , ("theta"                                  , Left 0x003B8               ) -- GREEK SMALL LETTER THETA
             , ("thetasym"                               , Left 0x003D1               ) -- GREEK THETA SYMBOL
             , ("thetav"                                 , Left 0x003D1               ) -- GREEK THETA SYMBOL
             , ("thgr"                                   , Left 0x003B8               ) -- GREEK SMALL LETTER THETA
             , ("thickapprox"                            , Left 0x02248               ) -- ALMOST EQUAL TO
             , ("thicksim"                               , Left 0x0223C               ) -- TILDE OPERATOR
             , ("thinsp"                                 , Left 0x02009               ) -- THIN SPACE
             , ("thkap"                                  , Left 0x02248               ) -- ALMOST EQUAL TO
             , ("thksim"                                 , Left 0x0223C               ) -- TILDE OPERATOR
             , ("thorn"                                  , Left 0x000FE               ) -- LATIN SMALL LETTER THORN
             , ("tilde"                                  , Left 0x002DC               ) -- SMALL TILDE
             , ("times"                                  , Left 0x000D7               ) -- MULTIPLICATION SIGN
             , ("timesb"                                 , Left 0x022A0               ) -- SQUARED TIMES
             , ("timesbar"                               , Left 0x02A31               ) -- MULTIPLICATION SIGN WITH UNDERBAR
             , ("timesd"                                 , Left 0x02A30               ) -- MULTIPLICATION SIGN WITH DOT ABOVE
             , ("tint"                                   , Left 0x0222D               ) -- TRIPLE INTEGRAL
             , ("toea"                                   , Left 0x02928               ) -- NORTH EAST ARROW AND SOUTH EAST ARROW
             , ("top"                                    , Left 0x022A4               ) -- DOWN TACK
             , ("topbot"                                 , Left 0x02336               ) -- APL FUNCTIONAL SYMBOL I-BEAM
             , ("topcir"                                 , Left 0x02AF1               ) -- DOWN TACK WITH CIRCLE BELOW
             , ("topf"                                   , Left 0x1D565               ) -- MATHEMATICAL DOUBLE-STRUCK SMALL T
             , ("topfork"                                , Left 0x02ADA               ) -- PITCHFORK WITH TEE TOP
             , ("tosa"                                   , Left 0x02929               ) -- SOUTH EAST ARROW AND SOUTH WEST ARROW
             , ("tprime"                                 , Left 0x02034               ) -- TRIPLE PRIME
             , ("trade"                                  , Left 0x02122               ) -- TRADE MARK SIGN
             , ("triangle"                               , Left 0x025B5               ) -- WHITE UP-POINTING SMALL TRIANGLE
             , ("triangledown"                           , Left 0x025BF               ) -- WHITE DOWN-POINTING SMALL TRIANGLE
             , ("triangleleft"                           , Left 0x025C3               ) -- WHITE LEFT-POINTING SMALL TRIANGLE
             , ("trianglelefteq"                         , Left 0x022B4               ) -- NORMAL SUBGROUP OF OR EQUAL TO
             , ("triangleq"                              , Left 0x0225C               ) -- DELTA EQUAL TO
             , ("triangleright"                          , Left 0x025B9               ) -- WHITE RIGHT-POINTING SMALL TRIANGLE
             , ("trianglerighteq"                        , Left 0x022B5               ) -- CONTAINS AS NORMAL SUBGROUP OR EQUAL TO
             , ("tridot"                                 , Left 0x025EC               ) -- WHITE UP-POINTING TRIANGLE WITH DOT
             , ("trie"                                   , Left 0x0225C               ) -- DELTA EQUAL TO
             , ("triminus"                               , Left 0x02A3A               ) -- MINUS SIGN IN TRIANGLE
             , ("triplus"                                , Left 0x02A39               ) -- PLUS SIGN IN TRIANGLE
             , ("trisb"                                  , Left 0x029CD               ) -- TRIANGLE WITH SERIFS AT BOTTOM
             , ("tritime"                                , Left 0x02A3B               ) -- MULTIPLICATION SIGN IN TRIANGLE
             , ("trpezium"                               , Left 0x023E2               ) -- WHITE TRAPEZIUM
             , ("tscr"                                   , Left 0x1D4C9               ) -- MATHEMATICAL SCRIPT SMALL T
             , ("tscy"                                   , Left 0x00446               ) -- CYRILLIC SMALL LETTER TSE
             , ("tshcy"                                  , Left 0x0045B               ) -- CYRILLIC SMALL LETTER TSHE
             , ("tstrok"                                 , Left 0x00167               ) -- LATIN SMALL LETTER T WITH STROKE
             , ("twixt"                                  , Left 0x0226C               ) -- BETWEEN
             , ("twoheadleftarrow"                       , Left 0x0219E               ) -- LEFTWARDS TWO HEADED ARROW
             , ("twoheadrightarrow"                      , Left 0x021A0               ) -- RIGHTWARDS TWO HEADED ARROW
             , ("uArr"                                   , Left 0x021D1               ) -- UPWARDS DOUBLE ARROW
             , ("uHar"                                   , Left 0x02963               ) -- UPWARDS HARPOON WITH BARB LEFT BESIDE UPWARDS HARPOON WITH BARB RIGHT
             , ("uacgr"                                  , Left 0x003CD               ) -- GREEK SMALL LETTER UPSILON WITH TONOS
             , ("uacute"                                 , Left 0x000FA               ) -- LATIN SMALL LETTER U WITH ACUTE
             , ("uarr"                                   , Left 0x02191               ) -- UPWARDS ARROW
             , ("ubrcy"                                  , Left 0x0045E               ) -- CYRILLIC SMALL LETTER SHORT U
             , ("ubreve"                                 , Left 0x0016D               ) -- LATIN SMALL LETTER U WITH BREVE
             , ("ucirc"                                  , Left 0x000FB               ) -- LATIN SMALL LETTER U WITH CIRCUMFLEX
             , ("ucy"                                    , Left 0x00443               ) -- CYRILLIC SMALL LETTER U
             , ("udarr"                                  , Left 0x021C5               ) -- UPWARDS ARROW LEFTWARDS OF DOWNWARDS ARROW
             , ("udblac"                                 , Left 0x00171               ) -- LATIN SMALL LETTER U WITH DOUBLE ACUTE
             , ("udhar"                                  , Left 0x0296E               ) -- UPWARDS HARPOON WITH BARB LEFT BESIDE DOWNWARDS HARPOON WITH BARB RIGHT
             , ("udiagr"                                 , Left 0x003B0               ) -- GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND TONOS
             , ("udigr"                                  , Left 0x003CB               ) -- GREEK SMALL LETTER UPSILON WITH DIALYTIKA
             , ("ufisht"                                 , Left 0x0297E               ) -- UP FISH TAIL
             , ("ufr"                                    , Left 0x1D532               ) -- MATHEMATICAL FRAKTUR SMALL U
             , ("ugr"                                    , Left 0x003C5               ) -- GREEK SMALL LETTER UPSILON
             , ("ugrave"                                 , Left 0x000F9               ) -- LATIN SMALL LETTER U WITH GRAVE
             , ("uharl"                                  , Left 0x021BF               ) -- UPWARDS HARPOON WITH BARB LEFTWARDS
             , ("uharr"                                  , Left 0x021BE               ) -- UPWARDS HARPOON WITH BARB RIGHTWARDS
             , ("uhblk"                                  , Left 0x02580               ) -- UPPER HALF BLOCK
             , ("ulcorn"                                 , Left 0x0231C               ) -- TOP LEFT CORNER
             , ("ulcorner"                               , Left 0x0231C               ) -- TOP LEFT CORNER
             , ("ulcrop"                                 , Left 0x0230F               ) -- TOP LEFT CROP
             , ("ultri"                                  , Left 0x025F8               ) -- UPPER LEFT TRIANGLE
             , ("umacr"                                  , Left 0x0016B               ) -- LATIN SMALL LETTER U WITH MACRON
             , ("uml"                                    , Left 0x000A8               ) -- DIAERESIS
             , ("uogon"                                  , Left 0x00173               ) -- LATIN SMALL LETTER U WITH OGONEK
             , ("uopf"                                   , Left 0x1D566               ) -- MATHEMATICAL DOUBLE-STRUCK SMALL U
             , ("uparrow"                                , Left 0x02191               ) -- UPWARDS ARROW
             , ("updownarrow"                            , Left 0x02195               ) -- UP DOWN ARROW
             , ("upharpoonleft"                          , Left 0x021BF               ) -- UPWARDS HARPOON WITH BARB LEFTWARDS
             , ("upharpoonright"                         , Left 0x021BE               ) -- UPWARDS HARPOON WITH BARB RIGHTWARDS
             , ("uplus"                                  , Left 0x0228E               ) -- MULTISET UNION
             , ("upsi"                                   , Left 0x003C5               ) -- GREEK SMALL LETTER UPSILON
             , ("upsih"                                  , Left 0x003D2               ) -- GREEK UPSILON WITH HOOK SYMBOL
             , ("upsilon"                                , Left 0x003C5               ) -- GREEK SMALL LETTER UPSILON
             , ("upuparrows"                             , Left 0x021C8               ) -- UPWARDS PAIRED ARROWS
             , ("urcorn"                                 , Left 0x0231D               ) -- TOP RIGHT CORNER
             , ("urcorner"                               , Left 0x0231D               ) -- TOP RIGHT CORNER
             , ("urcrop"                                 , Left 0x0230E               ) -- TOP RIGHT CROP
             , ("uring"                                  , Left 0x0016F               ) -- LATIN SMALL LETTER U WITH RING ABOVE
             , ("urtri"                                  , Left 0x025F9               ) -- UPPER RIGHT TRIANGLE
             , ("uscr"                                   , Left 0x1D4CA               ) -- MATHEMATICAL SCRIPT SMALL U
             , ("utdot"                                  , Left 0x022F0               ) -- UP RIGHT DIAGONAL ELLIPSIS
             , ("utilde"                                 , Left 0x00169               ) -- LATIN SMALL LETTER U WITH TILDE
             , ("utri"                                   , Left 0x025B5               ) -- WHITE UP-POINTING SMALL TRIANGLE
             , ("utrif"                                  , Left 0x025B4               ) -- BLACK UP-POINTING SMALL TRIANGLE
             , ("uuarr"                                  , Left 0x021C8               ) -- UPWARDS PAIRED ARROWS
             , ("uuml"                                   , Left 0x000FC               ) -- LATIN SMALL LETTER U WITH DIAERESIS
             , ("uwangle"                                , Left 0x029A7               ) -- OBLIQUE ANGLE OPENING DOWN
             , ("vArr"                                   , Left 0x021D5               ) -- UP DOWN DOUBLE ARROW
             , ("vBar"                                   , Left 0x02AE8               ) -- SHORT UP TACK WITH UNDERBAR
             , ("vBarv"                                  , Left 0x02AE9               ) -- SHORT UP TACK ABOVE SHORT DOWN TACK
             , ("vDash"                                  , Left 0x022A8               ) -- TRUE
             , ("vangrt"                                 , Left 0x0299C               ) -- RIGHT ANGLE VARIANT WITH SQUARE
             , ("varepsilon"                             , Left 0x003F5               ) -- GREEK LUNATE EPSILON SYMBOL
             , ("varkappa"                               , Left 0x003F0               ) -- GREEK KAPPA SYMBOL
             , ("varnothing"                             , Left 0x02205               ) -- EMPTY SET
             , ("varphi"                                 , Left 0x003D5               ) -- GREEK PHI SYMBOL
             , ("varpi"                                  , Left 0x003D6               ) -- GREEK PI SYMBOL
             , ("varpropto"                              , Left 0x0221D               ) -- PROPORTIONAL TO
             , ("varr"                                   , Left 0x02195               ) -- UP DOWN ARROW
             , ("varrho"                                 , Left 0x003F1               ) -- GREEK RHO SYMBOL
             , ("varsigma"                               , Left 0x003C2               ) -- GREEK SMALL LETTER FINAL SIGMA
             , ("varsubsetneq"                           , Right [ 0x0228A, 0x0FE00 ] ) -- SUBSET OF WITH NOT EQUAL TO - variant with stroke through bottom members
             , ("varsubsetneqq"                          , Right [ 0x02ACB, 0x0FE00 ] ) -- SUBSET OF ABOVE NOT EQUAL TO - variant with stroke through bottom members
             , ("varsupsetneq"                           , Right [ 0x0228B, 0x0FE00 ] ) -- SUPERSET OF WITH NOT EQUAL TO - variant with stroke through bottom members
             , ("varsupsetneqq"                          , Right [ 0x02ACC, 0x0FE00 ] ) -- SUPERSET OF ABOVE NOT EQUAL TO - variant with stroke through bottom members
             , ("vartheta"                               , Left 0x003D1               ) -- GREEK THETA SYMBOL
             , ("vartriangleleft"                        , Left 0x022B2               ) -- NORMAL SUBGROUP OF
             , ("vartriangleright"                       , Left 0x022B3               ) -- CONTAINS AS NORMAL SUBGROUP
             , ("vcy"                                    , Left 0x00432               ) -- CYRILLIC SMALL LETTER VE
             , ("vdash"                                  , Left 0x022A2               ) -- RIGHT TACK
             , ("vee"                                    , Left 0x02228               ) -- LOGICAL OR
             , ("veebar"                                 , Left 0x022BB               ) -- XOR
             , ("veeeq"                                  , Left 0x0225A               ) -- EQUIANGULAR TO
             , ("vellip"                                 , Left 0x022EE               ) -- VERTICAL ELLIPSIS
             , ("verbar"                                 , Left 0x0007C               ) -- VERTICAL LINE
             , ("vert"                                   , Left 0x0007C               ) -- VERTICAL LINE
             , ("vfr"                                    , Left 0x1D533               ) -- MATHEMATICAL FRAKTUR SMALL V
             , ("vltri"                                  , Left 0x022B2               ) -- NORMAL SUBGROUP OF
             , ("vnsub"                                  , Right [ 0x02282, 0x020D2 ] ) -- SUBSET OF with vertical line
             , ("vnsup"                                  , Right [ 0x02283, 0x020D2 ] ) -- SUPERSET OF with vertical line
             , ("vopf"                                   , Left 0x1D567               ) -- MATHEMATICAL DOUBLE-STRUCK SMALL V
             , ("vprop"                                  , Left 0x0221D               ) -- PROPORTIONAL TO
             , ("vrtri"                                  , Left 0x022B3               ) -- CONTAINS AS NORMAL SUBGROUP
             , ("vscr"                                   , Left 0x1D4CB               ) -- MATHEMATICAL SCRIPT SMALL V
             , ("vsubnE"                                 , Right [ 0x02ACB, 0x0FE00 ] ) -- SUBSET OF ABOVE NOT EQUAL TO - variant with stroke through bottom members
             , ("vsubne"                                 , Right [ 0x0228A, 0x0FE00 ] ) -- SUBSET OF WITH NOT EQUAL TO - variant with stroke through bottom members
             , ("vsupnE"                                 , Right [ 0x02ACC, 0x0FE00 ] ) -- SUPERSET OF ABOVE NOT EQUAL TO - variant with stroke through bottom members
             , ("vsupne"                                 , Right [ 0x0228B, 0x0FE00 ] ) -- SUPERSET OF WITH NOT EQUAL TO - variant with stroke through bottom members
             , ("vzigzag"                                , Left 0x0299A               ) -- VERTICAL ZIGZAG LINE
             , ("wcirc"                                  , Left 0x00175               ) -- LATIN SMALL LETTER W WITH CIRCUMFLEX
             , ("wedbar"                                 , Left 0x02A5F               ) -- LOGICAL AND WITH UNDERBAR
             , ("wedge"                                  , Left 0x02227               ) -- LOGICAL AND
             , ("wedgeq"                                 , Left 0x02259               ) -- ESTIMATES
             , ("weierp"                                 , Left 0x02118               ) -- SCRIPT CAPITAL P
             , ("wfr"                                    , Left 0x1D534               ) -- MATHEMATICAL FRAKTUR SMALL W
             , ("wopf"                                   , Left 0x1D568               ) -- MATHEMATICAL DOUBLE-STRUCK SMALL W
             , ("wp"                                     , Left 0x02118               ) -- SCRIPT CAPITAL P
             , ("wr"                                     , Left 0x02240               ) -- WREATH PRODUCT
             , ("wreath"                                 , Left 0x02240               ) -- WREATH PRODUCT
             , ("wscr"                                   , Left 0x1D4CC               ) -- MATHEMATICAL SCRIPT SMALL W
             , ("xcap"                                   , Left 0x022C2               ) -- N-ARY INTERSECTION
             , ("xcirc"                                  , Left 0x025EF               ) -- LARGE CIRCLE
             , ("xcup"                                   , Left 0x022C3               ) -- N-ARY UNION
             , ("xdtri"                                  , Left 0x025BD               ) -- WHITE DOWN-POINTING TRIANGLE
             , ("xfr"                                    , Left 0x1D535               ) -- MATHEMATICAL FRAKTUR SMALL X
             , ("xgr"                                    , Left 0x003BE               ) -- GREEK SMALL LETTER XI
             , ("xhArr"                                  , Left 0x027FA               ) -- LONG LEFT RIGHT DOUBLE ARROW
             , ("xharr"                                  , Left 0x027F7               ) -- LONG LEFT RIGHT ARROW
             , ("xi"                                     , Left 0x003BE               ) -- GREEK SMALL LETTER XI
             , ("xlArr"                                  , Left 0x027F8               ) -- LONG LEFTWARDS DOUBLE ARROW
             , ("xlarr"                                  , Left 0x027F5               ) -- LONG LEFTWARDS ARROW
             , ("xmap"                                   , Left 0x027FC               ) -- LONG RIGHTWARDS ARROW FROM BAR
             , ("xnis"                                   , Left 0x022FB               ) -- CONTAINS WITH VERTICAL BAR AT END OF HORIZONTAL STROKE
             , ("xodot"                                  , Left 0x02A00               ) -- N-ARY CIRCLED DOT OPERATOR
             , ("xopf"                                   , Left 0x1D569               ) -- MATHEMATICAL DOUBLE-STRUCK SMALL X
             , ("xoplus"                                 , Left 0x02A01               ) -- N-ARY CIRCLED PLUS OPERATOR
             , ("xotime"                                 , Left 0x02A02               ) -- N-ARY CIRCLED TIMES OPERATOR
             , ("xrArr"                                  , Left 0x027F9               ) -- LONG RIGHTWARDS DOUBLE ARROW
             , ("xrarr"                                  , Left 0x027F6               ) -- LONG RIGHTWARDS ARROW
             , ("xscr"                                   , Left 0x1D4CD               ) -- MATHEMATICAL SCRIPT SMALL X
             , ("xsqcup"                                 , Left 0x02A06               ) -- N-ARY SQUARE UNION OPERATOR
             , ("xuplus"                                 , Left 0x02A04               ) -- N-ARY UNION OPERATOR WITH PLUS
             , ("xutri"                                  , Left 0x025B3               ) -- WHITE UP-POINTING TRIANGLE
             , ("xvee"                                   , Left 0x022C1               ) -- N-ARY LOGICAL OR
             , ("xwedge"                                 , Left 0x022C0               ) -- N-ARY LOGICAL AND
             , ("yacute"                                 , Left 0x000FD               ) -- LATIN SMALL LETTER Y WITH ACUTE
             , ("yacy"                                   , Left 0x0044F               ) -- CYRILLIC SMALL LETTER YA
             , ("ycirc"                                  , Left 0x00177               ) -- LATIN SMALL LETTER Y WITH CIRCUMFLEX
             , ("ycy"                                    , Left 0x0044B               ) -- CYRILLIC SMALL LETTER YERU
             , ("yen"                                    , Left 0x000A5               ) -- YEN SIGN
             , ("yfr"                                    , Left 0x1D536               ) -- MATHEMATICAL FRAKTUR SMALL Y
             , ("yicy"                                   , Left 0x00457               ) -- CYRILLIC SMALL LETTER YI
             , ("yopf"                                   , Left 0x1D56A               ) -- MATHEMATICAL DOUBLE-STRUCK SMALL Y
             , ("yscr"                                   , Left 0x1D4CE               ) -- MATHEMATICAL SCRIPT SMALL Y
             , ("yucy"                                   , Left 0x0044E               ) -- CYRILLIC SMALL LETTER YU
             , ("yuml"                                   , Left 0x000FF               ) -- LATIN SMALL LETTER Y WITH DIAERESIS
             , ("zacute"                                 , Left 0x0017A               ) -- LATIN SMALL LETTER Z WITH ACUTE
             , ("zcaron"                                 , Left 0x0017E               ) -- LATIN SMALL LETTER Z WITH CARON
             , ("zcy"                                    , Left 0x00437               ) -- CYRILLIC SMALL LETTER ZE
             , ("zdot"                                   , Left 0x0017C               ) -- LATIN SMALL LETTER Z WITH DOT ABOVE
             , ("zeetrf"                                 , Left 0x02128               ) -- BLACK-LETTER CAPITAL Z
             , ("zeta"                                   , Left 0x003B6               ) -- GREEK SMALL LETTER ZETA
             , ("zfr"                                    , Left 0x1D537               ) -- MATHEMATICAL FRAKTUR SMALL Z
             , ("zgr"                                    , Left 0x003B6               ) -- GREEK SMALL LETTER ZETA
             , ("zhcy"                                   , Left 0x00436               ) -- CYRILLIC SMALL LETTER ZHE
             , ("zigrarr"                                , Left 0x021DD               ) -- RIGHTWARDS SQUIGGLE ARROW
             , ("zopf"                                   , Left 0x1D56B               ) -- MATHEMATICAL DOUBLE-STRUCK SMALL Z
             , ("zscr"                                   , Left 0x1D4CF               ) -- MATHEMATICAL SCRIPT SMALL Z
             , ("zwj"                                    , Left 0x0200D               ) -- ZERO WIDTH JOINER
             , ("zwnj"                                   , Left 0x0200C               ) -- ZERO WIDTH NON-JOINER
             ]







-- entityENum = [ ("Aacute"                          , Left 0x000C1)
             -- , ("aacute"                          , Left 0x000E1)
             -- , ("Abreve"                          , Left 0x00102)
             -- , ("abreve"                          , Left 0x00103)
             -- , ("acd"                             , Left 0x0223F)
             -- , ("Acirc"                           , Left 0x000C2)
             -- , ("acirc"                           , Left 0x000E2)
             -- , ("ac"                              , Left 0x0223E)
             -- , ("acute"                           , Left 0x000B4)
             -- , ("Acy"                             , Left 0x00410)
             -- , ("acy"                             , Left 0x00430)
             -- , ("AElig"                           , Left 0x000C6)
             -- , ("aelig"                           , Left 0x000E6)
             -- , ("af"                              , Left 0x02061)
             -- , ("Afr"                             , Left 0x1D504)
             -- , ("afr"                             , Left 0x1D51E)
             -- , ("Agrave"                          , Left 0x000C0)
             -- , ("agrave"                          , Left 0x000E0)
             -- , ("aleph"                           , Left 0x02135)
             -- , ("alpha"                           , Left 0x003B1)
             -- , ("Amacr"                           , Left 0x00100)
             -- , ("amacr"                           , Left 0x00101)
             -- , ("amalg"                           , Left 0x02A3F)
             -- , ("amp"                             , Left 0x00026)
             -- , ("andand"                          , Left 0x02A55)
             -- , ("andd"                            , Left 0x02A5C)
             -- , ("and"                             , Left 0x02227)
             -- , ("And"                             , Left 0x02A53)
             -- , ("andslope"                        , Left 0x02A58)
             -- , ("andv"                            , Left 0x02A5A)
             -- , ("ange"                            , Left 0x029A4)
             -- , ("ang"                             , Left 0x02220)
             -- , ("angle"                           , Left 0x02220)
             -- , ("angmsdaa"                        , Left 0x029A8)
             -- , ("angmsdab"                        , Left 0x029A9)
             -- , ("angmsdac"                        , Left 0x029AA)
             -- , ("angmsdad"                        , Left 0x029AB)
             -- , ("angmsdae"                        , Left 0x029AC)
             -- , ("angmsdaf"                        , Left 0x029AD)
             -- , ("angmsdag"                        , Left 0x029AE)
             -- , ("angmsdah"                        , Left 0x029AF)
             -- , ("angmsd"                          , Left 0x02221)
             -- , ("angrt"                           , Left 0x0221F)
             -- , ("angrtvbd"                        , Left 0x0299D)
             -- , ("angrtvb"                         , Left 0x022BE)
             -- , ("angsph"                          , Left 0x02222)
             -- , ("angst"                           , Left 0x0212B)
             -- , ("angzarr"                         , Left 0x0237C)
             -- , ("Aogon"                           , Left 0x00104)
             -- , ("aogon"                           , Left 0x00105)
             -- , ("Aopf"                            , Left 0x1D538)
             -- , ("aopf"                            , Left 0x1D552)
             -- , ("apacir"                          , Left 0x02A6F)
             -- , ("ape"                             , Left 0x0224A)
             -- , ("apE"                             , Left 0x02A70)
             -- , ("apid"                            , Left 0x0224B)
             -- , ("ap"                              , Left 0x02248)
             -- , ("apos"                            , Left 0x00027)
             -- , ("ApplyFunction"                   , Left 0x02061)
             -- , ("approxeq"                        , Left 0x0224A)
             -- , ("approx"                          , Left 0x02248)
             -- , ("Aring"                           , Left 0x000C5)
             -- , ("aring"                           , Left 0x000E5)
             -- , ("Ascr"                            , Left 0x1D49C)
             -- , ("ascr"                            , Left 0x1D4B6)
             -- , ("Assign"                          , Left 0x02254)
             -- , ("ast"                             , Left 0x0002A)
             -- , ("asympeq"                         , Left 0x0224D)
             -- , ("asymp"                           , Left 0x02248)
             -- , ("Atilde"                          , Left 0x000C3)
             -- , ("atilde"                          , Left 0x000E3)
             -- , ("Auml"                            , Left 0x000C4)
             -- , ("auml"                            , Left 0x000E4)
             -- , ("awconint"                        , Left 0x02233)
             -- , ("awint"                           , Left 0x02A11)
             -- , ("backcong"                        , Left 0x0224C)
             -- , ("backepsilon"                     , Left 0x003F6)
             -- , ("backprime"                       , Left 0x02035)
             -- , ("backsimeq"                       , Left 0x022CD)
             -- , ("backsim"                         , Left 0x0223D)
             -- , ("Backslash"                       , Left 0x02216)
             -- , ("barvee"                          , Left 0x022BD)
             -- , ("Barv"                            , Left 0x02AE7)
             -- , ("barwedge"                        , Left 0x02305)
             -- , ("barwed"                          , Left 0x02305)
             -- , ("Barwed"                          , Left 0x02306)
             -- , ("bbrk"                            , Left 0x023B5)
             -- , ("bbrktbrk"                        , Left 0x023B6)
             -- , ("bcong"                           , Left 0x0224C)
             -- , ("Bcy"                             , Left 0x00411)
             -- , ("bcy"                             , Left 0x00431)
             -- , ("because"                         , Left 0x02235)
             -- , ("Because"                         , Left 0x02235)
             -- , ("becaus"                          , Left 0x02235)
             -- , ("bemptyv"                         , Left 0x029B0)
             -- , ("bepsi"                           , Left 0x003F6)
             -- , ("bernou"                          , Left 0x0212C)
             -- , ("Bernoullis"                      , Left 0x0212C)
             -- , ("beta"                            , Left 0x003B2)
             -- , ("beth"                            , Left 0x02136)
             -- , ("between"                         , Left 0x0226C)
             -- , ("Bfr"                             , Left 0x1D505)
             -- , ("bfr"                             , Left 0x1D51F)
             -- , ("bigcap"                          , Left 0x022C2)
             -- , ("bigcirc"                         , Left 0x025EF)
             -- , ("bigcup"                          , Left 0x022C3)
             -- , ("bigodot"                         , Left 0x02A00)
             -- , ("bigoplus"                        , Left 0x02A01)
             -- , ("bigotimes"                       , Left 0x02A02)
             -- , ("bigsqcup"                        , Left 0x02A06)
             -- , ("bigstar"                         , Left 0x02605)
             -- , ("bigtriangledown"                 , Left 0x025BD)
             -- , ("bigtriangleup"                   , Left 0x025B3)
             -- , ("biguplus"                        , Left 0x02A04)
             -- , ("bigvee"                          , Left 0x022C1)
             -- , ("bigwedge"                        , Left 0x022C0)
             -- , ("bkarow"                          , Left 0x0290D)
             -- , ("blacklozenge"                    , Left 0x029EB)
             -- , ("blacksquare"                     , Left 0x025AA)
             -- , ("blacktriangledown"               , Left 0x025BE)
             -- , ("blacktriangle"                   , Left 0x025B4)
             -- , ("blacktriangleleft"               , Left 0x025C2)
             -- , ("blacktriangleright"              , Left 0x025B8)
             -- , ("blank"                           , Left 0x02423)
             -- , ("blk12"                           , Left 0x02592)
             -- , ("blk14"                           , Left 0x02591)
             -- , ("blk34"                           , Left 0x02593)
             -- , ("block"                           , Left 0x02588)
             -- , ("bnot"                            , Left 0x02310)
             -- , ("bNot"                            , Left 0x02AED)
             -- , ("Bopf"                            , Left 0x1D539)
             -- , ("bopf"                            , Left 0x1D553)
             -- , ("bot"                             , Left 0x022A5)
             -- , ("bottom"                          , Left 0x022A5)
             -- , ("bowtie"                          , Left 0x022C8)
             -- , ("boxbox"                          , Left 0x029C9)
             -- , ("boxdl"                           , Left 0x02510)
             -- , ("boxdL"                           , Left 0x02555)
             -- , ("boxDl"                           , Left 0x02556)
             -- , ("boxDL"                           , Left 0x02557)
             -- , ("boxdr"                           , Left 0x0250C)
             -- , ("boxdR"                           , Left 0x02552)
             -- , ("boxDr"                           , Left 0x02553)
             -- , ("boxDR"                           , Left 0x02554)
             -- , ("boxhd"                           , Left 0x0252C)
             -- , ("boxHd"                           , Left 0x02564)
             -- , ("boxhD"                           , Left 0x02565)
             -- , ("boxHD"                           , Left 0x02566)
             -- , ("boxh"                            , Left 0x02500)
             -- , ("boxH"                            , Left 0x02550)
             -- , ("boxhu"                           , Left 0x02534)
             -- , ("boxHu"                           , Left 0x02567)
             -- , ("boxhU"                           , Left 0x02568)
             -- , ("boxHU"                           , Left 0x02569)
             -- , ("boxminus"                        , Left 0x0229F)
             -- , ("boxplus"                         , Left 0x0229E)
             -- , ("boxtimes"                        , Left 0x022A0)
             -- , ("boxul"                           , Left 0x02518)
             -- , ("boxuL"                           , Left 0x0255B)
             -- , ("boxUl"                           , Left 0x0255C)
             -- , ("boxUL"                           , Left 0x0255D)
             -- , ("boxur"                           , Left 0x02514)
             -- , ("boxuR"                           , Left 0x02558)
             -- , ("boxUr"                           , Left 0x02559)
             -- , ("boxUR"                           , Left 0x0255A)
             -- , ("boxvh"                           , Left 0x0253C)
             -- , ("boxvH"                           , Left 0x0256A)
             -- , ("boxVh"                           , Left 0x0256B)
             -- , ("boxVH"                           , Left 0x0256C)
             -- , ("boxv"                            , Left 0x02502)
             -- , ("boxV"                            , Left 0x02551)
             -- , ("boxvl"                           , Left 0x02524)
             -- , ("boxvL"                           , Left 0x02561)
             -- , ("boxVl"                           , Left 0x02562)
             -- , ("boxVL"                           , Left 0x02563)
             -- , ("boxvr"                           , Left 0x0251C)
             -- , ("boxvR"                           , Left 0x0255E)
             -- , ("boxVr"                           , Left 0x0255F)
             -- , ("boxVR"                           , Left 0x02560)
             -- , ("bprime"                          , Left 0x02035)
             -- , ("breve"                           , Left 0x002D8)
             -- , ("Breve"                           , Left 0x002D8)
             -- , ("brvbar"                          , Left 0x000A6)
             -- , ("Bscr"                            , Left 0x0212C)
             -- , ("bscr"                            , Left 0x1D4B7)
             -- , ("bsemi"                           , Left 0x0204F)
             -- , ("bsime"                           , Left 0x022CD)
             -- , ("bsim"                            , Left 0x0223D)
             -- , ("bsolb"                           , Left 0x029C5)
             -- , ("bsol"                            , Left 0x0005C)
             -- , ("bullet"                          , Left 0x02022)
             -- , ("bull"                            , Left 0x02022)
             -- , ("bumpe"                           , Left 0x0224F)
             -- , ("bumpE"                           , Left 0x02AAE)
             -- , ("Bumpeq"                          , Left 0x0224E)
             -- , ("bumpeq"                          , Left 0x0224F)
             -- , ("bump"                            , Left 0x0224E)
             -- , ("Cacute"                          , Left 0x00106)
             -- , ("cacute"                          , Left 0x00107)
             -- , ("capand"                          , Left 0x02A44)
             -- , ("capbrcup"                        , Left 0x02A49)
             -- , ("capcap"                          , Left 0x02A4B)
             -- , ("capcup"                          , Left 0x02A47)
             -- , ("capdot"                          , Left 0x02A40)
             -- , ("CapitalDifferentialD"            , Left 0x02145)
             -- , ("cap"                             , Left 0x02229)
             -- , ("Cap"                             , Left 0x022D2)
             -- , ("caret"                           , Left 0x02041)
             -- , ("caron"                           , Left 0x002C7)
             -- , ("Cayleys"                         , Left 0x0212D)
             -- , ("ccaps"                           , Left 0x02A4D)
             -- , ("Ccaron"                          , Left 0x0010C)
             -- , ("ccaron"                          , Left 0x0010D)
             -- , ("Ccedil"                          , Left 0x000C7)
             -- , ("ccedil"                          , Left 0x000E7)
             -- , ("Ccirc"                           , Left 0x00108)
             -- , ("ccirc"                           , Left 0x00109)
             -- , ("Cconint"                         , Left 0x02230)
             -- , ("ccups"                           , Left 0x02A4C)
             -- , ("ccupssm"                         , Left 0x02A50)
             -- , ("Cdot"                            , Left 0x0010A)
             -- , ("cdot"                            , Left 0x0010B)
             -- , ("Cedilla"                         , Left 0x000B8)
             -- , ("cedil"                           , Left 0x000B8)
             -- , ("cemptyv"                         , Left 0x029B2)
             -- , ("centerdot"                       , Left 0x000B7)
             -- , ("CenterDot"                       , Left 0x000B7)
             -- , ("cent"                            , Left 0x000A2)
             -- , ("Cfr"                             , Left 0x0212D)
             -- , ("cfr"                             , Left 0x1D520)
             -- , ("CHcy"                            , Left 0x00427)
             -- , ("chcy"                            , Left 0x00447)
             -- , ("check"                           , Left 0x02713)
             -- , ("checkmark"                       , Left 0x02713)
             -- , ("chi"                             , Left 0x003C7)
             -- , ("circeq"                          , Left 0x02257)
             -- , ("circlearrowleft"                 , Left 0x021BA)
             -- , ("circlearrowright"                , Left 0x021BB)
             -- , ("circledast"                      , Left 0x0229B)
             -- , ("circledcirc"                     , Left 0x0229A)
             -- , ("circleddash"                     , Left 0x0229D)
             -- , ("CircleDot"                       , Left 0x02299)
             -- , ("circledR"                        , Left 0x000AE)
             -- , ("circledS"                        , Left 0x024C8)
             -- , ("circ"                            , Left 0x002C6)
             -- , ("CircleMinus"                     , Left 0x02296)
             -- , ("CirclePlus"                      , Left 0x02295)
             -- , ("CircleTimes"                     , Left 0x02297)
             -- , ("cire"                            , Left 0x02257)
             -- , ("cirE"                            , Left 0x029C3)
             -- , ("cirfnint"                        , Left 0x02A10)
             -- , ("cir"                             , Left 0x025CB)
             -- , ("cirmid"                          , Left 0x02AEF)
             -- , ("cirscir"                         , Left 0x029C2)
             -- , ("ClockwiseContourIntegral"        , Left 0x02232)
             -- , ("CloseCurlyDoubleQuote"           , Left 0x0201D)
             -- , ("CloseCurlyQuote"                 , Left 0x02019)
             -- , ("clubs"                           , Left 0x02663)
             -- , ("clubsuit"                        , Left 0x02663)
             -- , ("colone"                          , Left 0x02254)
             -- , ("Colone"                          , Left 0x02A74)
             -- , ("coloneq"                         , Left 0x02254)
             -- , ("colon"                           , Left 0x0003A)
             -- , ("Colon"                           , Left 0x02237)
             -- , ("comma"                           , Left 0x0002C)
             -- , ("commat"                          , Left 0x00040)
             -- , ("compfn"                          , Left 0x02218)
             -- , ("comp"                            , Left 0x02201)
             -- , ("complement"                      , Left 0x02201)
             -- , ("complexes"                       , Left 0x02102)
             -- , ("congdot"                         , Left 0x02A6D)
             -- , ("cong"                            , Left 0x02245)
             -- , ("Congruent"                       , Left 0x02261)
             -- , ("conint"                          , Left 0x0222E)
             -- , ("Conint"                          , Left 0x0222F)
             -- , ("ContourIntegral"                 , Left 0x0222E)
             -- , ("Copf"                            , Left 0x02102)
             -- , ("copf"                            , Left 0x1D554)
             -- , ("coprod"                          , Left 0x02210)
             -- , ("Coproduct"                       , Left 0x02210)
             -- , ("copy"                            , Left 0x000A9)
             -- , ("copysr"                          , Left 0x02117)
             -- , ("CounterClockwiseContourIntegral" , Left 0x02233)
             -- , ("cross"                           , Left 0x02717)
             -- , ("Cross"                           , Left 0x02A2F)
             -- , ("Cscr"                            , Left 0x1D49E)
             -- , ("cscr"                            , Left 0x1D4B8)
             -- , ("csube"                           , Left 0x02AD1)
             -- , ("csub"                            , Left 0x02ACF)
             -- , ("csupe"                           , Left 0x02AD2)
             -- , ("csup"                            , Left 0x02AD0)
             -- , ("ctdot"                           , Left 0x022EF)
             -- , ("cudarrl"                         , Left 0x02938)
             -- , ("cudarrr"                         , Left 0x02935)
             -- , ("cuepr"                           , Left 0x022DE)
             -- , ("cuesc"                           , Left 0x022DF)
             -- , ("cularr"                          , Left 0x021B6)
             -- , ("cularrp"                         , Left 0x0293D)
             -- , ("cupbrcap"                        , Left 0x02A48)
             -- , ("CupCap"                          , Left 0x0224D)
             -- , ("cupcap"                          , Left 0x02A46)
             -- , ("cupcup"                          , Left 0x02A4A)
             -- , ("cupdot"                          , Left 0x0228D)
             -- , ("cup"                             , Left 0x0222A)
             -- , ("Cup"                             , Left 0x022D3)
             -- , ("cupor"                           , Left 0x02A45)
             -- , ("curarr"                          , Left 0x021B7)
             -- , ("curarrm"                         , Left 0x0293C)
             -- , ("curlyeqprec"                     , Left 0x022DE)
             -- , ("curlyeqsucc"                     , Left 0x022DF)
             -- , ("curlyvee"                        , Left 0x022CE)
             -- , ("curlywedge"                      , Left 0x022CF)
             -- , ("curren"                          , Left 0x000A4)
             -- , ("curvearrowleft"                  , Left 0x021B6)
             -- , ("curvearrowright"                 , Left 0x021B7)
             -- , ("cuvee"                           , Left 0x022CE)
             -- , ("cuwed"                           , Left 0x022CF)
             -- , ("cwconint"                        , Left 0x02232)
             -- , ("cwint"                           , Left 0x02231)
             -- , ("cylcty"                          , Left 0x0232D)
             -- , ("dagger"                          , Left 0x02020)
             -- , ("Dagger"                          , Left 0x02021)
             -- , ("daleth"                          , Left 0x02138)
             -- , ("darr"                            , Left 0x02193)
             -- , ("Darr"                            , Left 0x021A1)
             -- , ("dArr"                            , Left 0x021D3)
             -- , ("dash"                            , Left 0x02010)
             -- , ("dashv"                           , Left 0x022A3)
             -- , ("Dashv"                           , Left 0x02AE4)
             -- , ("dbkarow"                         , Left 0x0290F)
             -- , ("dblac"                           , Left 0x002DD)
             -- , ("Dcaron"                          , Left 0x0010E)
             -- , ("dcaron"                          , Left 0x0010F)
             -- , ("Dcy"                             , Left 0x00414)
             -- , ("dcy"                             , Left 0x00434)
             -- , ("ddagger"                         , Left 0x02021)
             -- , ("ddarr"                           , Left 0x021CA)
             -- , ("DD"                              , Left 0x02145)
             -- , ("dd"                              , Left 0x02146)
             -- , ("DDotrahd"                        , Left 0x02911)
             -- , ("ddotseq"                         , Left 0x02A77)
             -- , ("deg"                             , Left 0x000B0)
             -- , ("Del"                             , Left 0x02207)
             -- , ("Delta"                           , Left 0x00394)
             -- , ("delta"                           , Left 0x003B4)
             -- , ("demptyv"                         , Left 0x029B1)
             -- , ("dfisht"                          , Left 0x0297F)
             -- , ("Dfr"                             , Left 0x1D507)
             -- , ("dfr"                             , Left 0x1D521)
             -- , ("dHar"                            , Left 0x02965)
             -- , ("dharl"                           , Left 0x021C3)
             -- , ("dharr"                           , Left 0x021C2)
             -- , ("DiacriticalAcute"                , Left 0x000B4)
             -- , ("DiacriticalDot"                  , Left 0x002D9)
             -- , ("DiacriticalDoubleAcute"          , Left 0x002DD)
             -- , ("DiacriticalGrave"                , Left 0x00060)
             -- , ("DiacriticalTilde"                , Left 0x002DC)
             -- , ("diam"                            , Left 0x022C4)
             -- , ("diamond"                         , Left 0x022C4)
             -- , ("Diamond"                         , Left 0x022C4)
             -- , ("diamondsuit"                     , Left 0x02666)
             -- , ("diams"                           , Left 0x02666)
             -- , ("die"                             , Left 0x000A8)
             -- , ("DifferentialD"                   , Left 0x02146)
             -- , ("digamma"                         , Left 0x003DD)
             -- , ("disin"                           , Left 0x022F2)
             -- , ("divide"                          , Left 0x000F7)
             -- , ("divideontimes"                   , Left 0x022C7)
             -- , ("div"                             , Left 0x000F7)
             -- , ("divonx"                          , Left 0x022C7)
             -- , ("DJcy"                            , Left 0x00402)
             -- , ("djcy"                            , Left 0x00452)
             -- , ("dlcorn"                          , Left 0x0231E)
             -- , ("dlcrop"                          , Left 0x0230D)
             -- , ("dollar"                          , Left 0x00024)
             -- , ("Dopf"                            , Left 0x1D53B)
             -- , ("dopf"                            , Left 0x1D555)
             -- , ("DotDot"                          , Left 0x020DC)
             -- , ("doteqdot"                        , Left 0x02251)
             -- , ("doteq"                           , Left 0x02250)
             -- , ("DotEqual"                        , Left 0x02250)
             -- , ("Dot"                             , Left 0x000A8)
             -- , ("dot"                             , Left 0x002D9)
             -- , ("dotminus"                        , Left 0x02238)
             -- , ("dotplus"                         , Left 0x02214)
             -- , ("dotsquare"                       , Left 0x022A1)
             -- , ("doublebarwedge"                  , Left 0x02306)
             -- , ("DoubleContourIntegral"           , Left 0x0222F)
             -- , ("DoubleDot"                       , Left 0x000A8)
             -- , ("DoubleDownArrow"                 , Left 0x021D3)
             -- , ("DoubleLeftArrow"                 , Left 0x021D0)
             -- , ("DoubleLeftRightArrow"            , Left 0x021D4)
             -- , ("DoubleLeftTee"                   , Left 0x02AE4)
             -- , ("DoubleLongLeftArrow"             , Left 0x027F8)
             -- , ("DoubleLongLeftRightArrow"        , Left 0x027FA)
             -- , ("DoubleLongRightArrow"            , Left 0x027F9)
             -- , ("DoubleRightArrow"                , Left 0x021D2)
             -- , ("DoubleRightTee"                  , Left 0x022A8)
             -- , ("DoubleUpArrow"                   , Left 0x021D1)
             -- , ("DoubleUpDownArrow"               , Left 0x021D5)
             -- , ("DoubleVerticalBar"               , Left 0x02225)
             -- , ("DownArrowBar"                    , Left 0x02913)
             -- , ("downarrow"                       , Left 0x02193)
             -- , ("DownArrow"                       , Left 0x02193)
             -- , ("Downarrow"                       , Left 0x021D3)
             -- , ("DownArrowUpArrow"                , Left 0x021F5)
             -- , ("DownBreve"                       , Left 0x00311)
             -- , ("downdownarrows"                  , Left 0x021CA)
             -- , ("downharpoonleft"                 , Left 0x021C3)
             -- , ("downharpoonright"                , Left 0x021C2)
             -- , ("DownLeftRightVector"             , Left 0x02950)
             -- , ("DownLeftTeeVector"               , Left 0x0295E)
             -- , ("DownLeftVectorBar"               , Left 0x02956)
             -- , ("DownLeftVector"                  , Left 0x021BD)
             -- , ("DownRightTeeVector"              , Left 0x0295F)
             -- , ("DownRightVectorBar"              , Left 0x02957)
             -- , ("DownRightVector"                 , Left 0x021C1)
             -- , ("DownTeeArrow"                    , Left 0x021A7)
             -- , ("DownTee"                         , Left 0x022A4)
             -- , ("drbkarow"                        , Left 0x02910)
             -- , ("drcorn"                          , Left 0x0231F)
             -- , ("drcrop"                          , Left 0x0230C)
             -- , ("Dscr"                            , Left 0x1D49F)
             -- , ("dscr"                            , Left 0x1D4B9)
             -- , ("DScy"                            , Left 0x00405)
             -- , ("dscy"                            , Left 0x00455)
             -- , ("dsol"                            , Left 0x029F6)
             -- , ("Dstrok"                          , Left 0x00110)
             -- , ("dstrok"                          , Left 0x00111)
             -- , ("dtdot"                           , Left 0x022F1)
             -- , ("dtrif"                           , Left 0x025BE)
             -- , ("dtri"                            , Left 0x025BF)
             -- , ("duarr"                           , Left 0x021F5)
             -- , ("duhar"                           , Left 0x0296F)
             -- , ("dwangle"                         , Left 0x029A6)
             -- , ("DZcy"                            , Left 0x0040F)
             -- , ("dzcy"                            , Left 0x0045F)
             -- , ("dzigrarr"                        , Left 0x027FF)
             -- , ("Eacute"                          , Left 0x000C9)
             -- , ("eacute"                          , Left 0x000E9)
             -- , ("easter"                          , Left 0x02A6E)
             -- , ("Ecaron"                          , Left 0x0011A)
             -- , ("ecaron"                          , Left 0x0011B)
             -- , ("Ecirc"                           , Left 0x000CA)
             -- , ("ecirc"                           , Left 0x000EA)
             -- , ("ecir"                            , Left 0x02256)
             -- , ("ecolon"                          , Left 0x02255)
             -- , ("Ecy"                             , Left 0x0042D)
             -- , ("ecy"                             , Left 0x0044D)
             -- , ("eDDot"                           , Left 0x02A77)
             -- , ("Edot"                            , Left 0x00116)
             -- , ("edot"                            , Left 0x00117)
             -- , ("eDot"                            , Left 0x02251)
             -- , ("ee"                              , Left 0x02147)
             -- , ("efDot"                           , Left 0x02252)
             -- , ("Efr"                             , Left 0x1D508)
             -- , ("efr"                             , Left 0x1D522)
             -- , ("eg"                              , Left 0x02A9A)
             -- , ("Egrave"                          , Left 0x000C8)
             -- , ("egrave"                          , Left 0x000E8)
             -- , ("egsdot"                          , Left 0x02A98)
             -- , ("egs"                             , Left 0x02A96)
             -- , ("Element"                         , Left 0x02208)
             -- , ("elinters"                        , Left 0x0FFFD)
             -- , ("el"                              , Left 0x02A99)
             -- , ("ell"                             , Left 0x02113)
             -- , ("elsdot"                          , Left 0x02A97)
             -- , ("els"                             , Left 0x02A95)
             -- , ("Emacr"                           , Left 0x00112)
             -- , ("emacr"                           , Left 0x00113)
             -- , ("empty"                           , Left 0x02205)
             -- , ("emptyset"                        , Left 0x02205)
             -- , ("EmptySmallSquare"                , Left 0x025FB)
             -- , ("EmptyVerySmallSquare"            , Left 0x025AB)
             -- , ("emptyv"                          , Left 0x02205)
             -- , ("emsp13"                          , Left 0x02004)
             -- , ("emsp14"                          , Left 0x02005)
             -- , ("emsp"                            , Left 0x02003)
             -- , ("ENG"                             , Left 0x0014A)
             -- , ("eng"                             , Left 0x0014B)
             -- , ("ensp"                            , Left 0x02002)
             -- , ("Eogon"                           , Left 0x00118)
             -- , ("eogon"                           , Left 0x00119)
             -- , ("Eopf"                            , Left 0x1D53C)
             -- , ("eopf"                            , Left 0x1D556)
             -- , ("epar"                            , Left 0x022D5)
             -- , ("eparsl"                          , Left 0x029E3)
             -- , ("eplus"                           , Left 0x02A71)
             -- , ("epsi"                            , Left 0x003F5)
             -- , ("epsiv"                           , Left 0x003B5)
             -- , ("eqcirc"                          , Left 0x02256)
             -- , ("eqcolon"                         , Left 0x02255)
             -- , ("eqsim"                           , Left 0x02242)
             -- , ("eqslantgtr"                      , Left 0x02A96)
             -- , ("eqslantless"                     , Left 0x02A95)
             -- , ("Equal"                           , Left 0x02A75)
             -- , ("equals"                          , Left 0x0003D)
             -- , ("EqualTilde"                      , Left 0x02242)
             -- , ("equest"                          , Left 0x0225F)
             -- , ("Equilibrium"                     , Left 0x021CC)
             -- , ("equivDD"                         , Left 0x02A78)
             -- , ("equiv"                           , Left 0x02261)
             -- , ("eqvparsl"                        , Left 0x029E5)
             -- , ("erarr"                           , Left 0x02971)
             -- , ("erDot"                           , Left 0x02253)
             -- , ("escr"                            , Left 0x0212F)
             -- , ("Escr"                            , Left 0x02130)
             -- , ("esdot"                           , Left 0x02250)
             -- , ("esim"                            , Left 0x02242)
             -- , ("Esim"                            , Left 0x02A73)
             -- , ("eta"                             , Left 0x003B7)
             -- , ("ETH"                             , Left 0x000D0)
             -- , ("eth"                             , Left 0x000F0)
             -- , ("Euml"                            , Left 0x000CB)
             -- , ("euml"                            , Left 0x000EB)
             -- , ("excl"                            , Left 0x00021)
             -- , ("exist"                           , Left 0x02203)
             -- , ("Exists"                          , Left 0x02203)
             -- , ("expectation"                     , Left 0x02130)
             -- , ("exponentiale"                    , Left 0x02147)
             -- , ("ExponentialE"                    , Left 0x02147)
             -- , ("fallingdotseq"                   , Left 0x02252)
             -- , ("Fcy"                             , Left 0x00424)
             -- , ("fcy"                             , Left 0x00444)
             -- , ("female"                          , Left 0x02640)
             -- , ("ffilig"                          , Left 0x0FB03)
             -- , ("fflig"                           , Left 0x0FB00)
             -- , ("ffllig"                          , Left 0x0FB04)
             -- , ("Ffr"                             , Left 0x1D509)
             -- , ("ffr"                             , Left 0x1D523)
             -- , ("filig"                           , Left 0x0FB01)
             -- , ("FilledSmallSquare"               , Left 0x025FC)
             -- , ("FilledVerySmallSquare"           , Left 0x025AA)
             -- , ("flat"                            , Left 0x0266D)
             -- , ("fllig"                           , Left 0x0FB02)
             -- , ("fltns"                           , Left 0x025B1)
             -- , ("fnof"                            , Left 0x00192)
             -- , ("Fopf"                            , Left 0x1D53D)
             -- , ("fopf"                            , Left 0x1D557)
             -- , ("forall"                          , Left 0x02200)
             -- , ("ForAll"                          , Left 0x02200)
             -- , ("fork"                            , Left 0x022D4)
             -- , ("forkv"                           , Left 0x02AD9)
             -- , ("Fouriertrf"                      , Left 0x02131)
             -- , ("fpartint"                        , Left 0x02A0D)
             -- , ("frac12"                          , Left 0x000BD)
             -- , ("frac13"                          , Left 0x02153)
             -- , ("frac14"                          , Left 0x000BC)
             -- , ("frac15"                          , Left 0x02155)
             -- , ("frac16"                          , Left 0x02159)
             -- , ("frac18"                          , Left 0x0215B)
             -- , ("frac23"                          , Left 0x02154)
             -- , ("frac25"                          , Left 0x02156)
             -- , ("frac34"                          , Left 0x000BE)
             -- , ("frac35"                          , Left 0x02157)
             -- , ("frac38"                          , Left 0x0215C)
             -- , ("frac45"                          , Left 0x02158)
             -- , ("frac56"                          , Left 0x0215A)
             -- , ("frac58"                          , Left 0x0215D)
             -- , ("frac78"                          , Left 0x0215E)
             -- , ("frown"                           , Left 0x02322)
             -- , ("Fscr"                            , Left 0x02131)
             -- , ("fscr"                            , Left 0x1D4BB)
             -- , ("gacute"                          , Left 0x001F5)
             -- , ("Gammad"                          , Left 0x003DC)
             -- , ("gammad"                          , Left 0x003DD)
             -- , ("Gamma"                           , Left 0x00393)
             -- , ("gamma"                           , Left 0x003B3)
             -- , ("gap"                             , Left 0x02A86)
             -- , ("Gbreve"                          , Left 0x0011E)
             -- , ("gbreve"                          , Left 0x0011F)
             -- , ("Gcedil"                          , Left 0x00122)
             -- , ("Gcirc"                           , Left 0x0011C)
             -- , ("gcirc"                           , Left 0x0011D)
             -- , ("Gcy"                             , Left 0x00413)
             -- , ("gcy"                             , Left 0x00433)
             -- , ("Gdot"                            , Left 0x00120)
             -- , ("gdot"                            , Left 0x00121)
             -- , ("ge"                              , Left 0x02265)
             -- , ("gE"                              , Left 0x02267)
             -- , ("gel"                             , Left 0x022DB)
             -- , ("gEl"                             , Left 0x02A8C)
             -- , ("geq"                             , Left 0x02265)
             -- , ("geqq"                            , Left 0x02267)
             -- , ("geqslant"                        , Left 0x02A7E)
             -- , ("gescc"                           , Left 0x02AA9)
             -- , ("gesdot"                          , Left 0x02A80)
             -- , ("gesdoto"                         , Left 0x02A82)
             -- , ("gesdotol"                        , Left 0x02A84)
             -- , ("ges"                             , Left 0x02A7E)
             -- , ("gesles"                          , Left 0x02A94)
             -- , ("Gfr"                             , Left 0x1D50A)
             -- , ("gfr"                             , Left 0x1D524)
             -- , ("ggg"                             , Left 0x022D9)
             -- , ("gg"                              , Left 0x0226B)
             -- , ("Gg"                              , Left 0x022D9)
             -- , ("gimel"                           , Left 0x02137)
             -- , ("GJcy"                            , Left 0x00403)
             -- , ("gjcy"                            , Left 0x00453)
             -- , ("gla"                             , Left 0x02AA5)
             -- , ("glE"                             , Left 0x02A92)
             -- , ("glj"                             , Left 0x02AA4)
             -- , ("gl"                              , Left 0x02277)
             -- , ("gnap"                            , Left 0x02A8A)
             -- , ("gnapprox"                        , Left 0x02A8A)
             -- , ("gnE"                             , Left 0x02269)
             -- , ("gne"                             , Left 0x02A88)
             -- , ("gneq"                            , Left 0x02A88)
             -- , ("gneqq"                           , Left 0x02269)
             -- , ("gnsim"                           , Left 0x022E7)
             -- , ("Gopf"                            , Left 0x1D53E)
             -- , ("gopf"                            , Left 0x1D558)
             -- , ("grave"                           , Left 0x00060)
             -- , ("GreaterEqual"                    , Left 0x02265)
             -- , ("GreaterEqualLess"                , Left 0x022DB)
             -- , ("GreaterFullEqual"                , Left 0x02267)
             -- , ("GreaterGreater"                  , Left 0x02AA2)
             -- , ("GreaterLess"                     , Left 0x02277)
             -- , ("GreaterSlantEqual"               , Left 0x02A7E)
             -- , ("GreaterTilde"                    , Left 0x02273)
             -- , ("gscr"                            , Left 0x0210A)
             -- , ("Gscr"                            , Left 0x1D4A2)
             -- , ("gsime"                           , Left 0x02A8E)
             -- , ("gsim"                            , Left 0x02273)
             -- , ("gsiml"                           , Left 0x02A90)
             -- , ("gtcc"                            , Left 0x02AA7)
             -- , ("gtcir"                           , Left 0x02A7A)
             -- , ("gtdot"                           , Left 0x022D7)
             -- , ("gt"                              , Left 0x0003E)
             -- , ("Gt"                              , Left 0x0226B)
             -- , ("gtlPar"                          , Left 0x02995)
             -- , ("gtquest"                         , Left 0x02A7C)
             -- , ("gtrapprox"                       , Left 0x02A86)
             -- , ("gtrarr"                          , Left 0x02978)
             -- , ("gtrdot"                          , Left 0x022D7)
             -- , ("gtreqless"                       , Left 0x022DB)
             -- , ("gtreqqless"                      , Left 0x02A8C)
             -- , ("gtrless"                         , Left 0x02277)
             -- , ("gtrsim"                          , Left 0x02273)
             -- , ("Hacek"                           , Left 0x002C7)
             -- , ("hairsp"                          , Left 0x0200A)
             -- , ("half"                            , Left 0x000BD)
             -- , ("hamilt"                          , Left 0x0210B)
             -- , ("HARDcy"                          , Left 0x0042A)
             -- , ("hardcy"                          , Left 0x0044A)
             -- , ("harrcir"                         , Left 0x02948)
             -- , ("harr"                            , Left 0x02194)
             -- , ("hArr"                            , Left 0x021D4)
             -- , ("harrw"                           , Left 0x021AD)
             -- , ("Hat"                             , Left 0x0005E)
             -- , ("hbar"                            , Left 0x0210F)
             -- , ("Hcirc"                           , Left 0x00124)
             -- , ("hcirc"                           , Left 0x00125)
             -- , ("hearts"                          , Left 0x02665)
             -- , ("heartsuit"                       , Left 0x02665)
             -- , ("hellip"                          , Left 0x02026)
             -- , ("hercon"                          , Left 0x022B9)
             -- , ("Hfr"                             , Left 0x0210C)
             -- , ("hfr"                             , Left 0x1D525)
             -- , ("HilbertSpace"                    , Left 0x0210B)
             -- , ("hksearow"                        , Left 0x02925)
             -- , ("hkswarow"                        , Left 0x02926)
             -- , ("hoarr"                           , Left 0x021FF)
             -- , ("homtht"                          , Left 0x0223B)
             -- , ("hookleftarrow"                   , Left 0x021A9)
             -- , ("hookrightarrow"                  , Left 0x021AA)
             -- , ("Hopf"                            , Left 0x0210D)
             -- , ("hopf"                            , Left 0x1D559)
             -- , ("horbar"                          , Left 0x02015)
             -- , ("HorizontalLine"                  , Left 0x02500)
             -- , ("Hscr"                            , Left 0x0210B)
             -- , ("hscr"                            , Left 0x1D4BD)
             -- , ("hslash"                          , Left 0x0210F)
             -- , ("Hstrok"                          , Left 0x00126)
             -- , ("hstrok"                          , Left 0x00127)
             -- , ("HumpDownHump"                    , Left 0x0224E)
             -- , ("HumpEqual"                       , Left 0x0224F)
             -- , ("hybull"                          , Left 0x02043)
             -- , ("hyphen"                          , Left 0x02010)
             -- , ("Iacute"                          , Left 0x000CD)
             -- , ("iacute"                          , Left 0x000ED)
             -- , ("Icirc"                           , Left 0x000CE)
             -- , ("icirc"                           , Left 0x000EE)
             -- , ("ic"                              , Left 0x02063)
             -- , ("Icy"                             , Left 0x00418)
             -- , ("icy"                             , Left 0x00438)
             -- , ("Idot"                            , Left 0x00130)
             -- , ("IEcy"                            , Left 0x00415)
             -- , ("iecy"                            , Left 0x00435)
             -- , ("iexcl"                           , Left 0x000A1)
             -- , ("iff"                             , Left 0x021D4)
             -- , ("Ifr"                             , Left 0x02111)
             -- , ("ifr"                             , Left 0x1D526)
             -- , ("Igrave"                          , Left 0x000CC)
             -- , ("igrave"                          , Left 0x000EC)
             -- , ("iiiint"                          , Left 0x02A0C)
             -- , ("iiint"                           , Left 0x0222D)
             -- , ("ii"                              , Left 0x02148)
             -- , ("iinfin"                          , Left 0x029DC)
             -- , ("iiota"                           , Left 0x02129)
             -- , ("IJlig"                           , Left 0x00132)
             -- , ("ijlig"                           , Left 0x00133)
             -- , ("Imacr"                           , Left 0x0012A)
             -- , ("imacr"                           , Left 0x0012B)
             -- , ("image"                           , Left 0x02111)
             -- , ("ImaginaryI"                      , Left 0x02148)
             -- , ("imagline"                        , Left 0x02110)
             -- , ("imagpart"                        , Left 0x02111)
             -- , ("imath"                           , Left 0x00131)
             -- , ("Im"                              , Left 0x02111)
             -- , ("imof"                            , Left 0x022B7)
             -- , ("imped"                           , Left 0x001B5)
             -- , ("Implies"                         , Left 0x021D2)
             -- , ("incare"                          , Left 0x02105)
             -- , ("infin"                           , Left 0x0221E)
             -- , ("infintie"                        , Left 0x029DD)
             -- , ("in"                              , Left 0x02208)
             -- , ("inodot"                          , Left 0x00131)
             -- , ("intcal"                          , Left 0x022BA)
             -- , ("integers"                        , Left 0x02124)
             -- , ("Integral"                        , Left 0x0222B)
             -- , ("intercal"                        , Left 0x022BA)
             -- , ("Intersection"                    , Left 0x022C2)
             -- , ("intlarhk"                        , Left 0x02A17)
             -- , ("int"                             , Left 0x0222B)
             -- , ("Int"                             , Left 0x0222C)
             -- , ("intprod"                         , Left 0x02A3C)
             -- , ("InvisibleComma"                  , Left 0x02063)
             -- , ("InvisibleTimes"                  , Left 0x02062)
             -- , ("IOcy"                            , Left 0x00401)
             -- , ("iocy"                            , Left 0x00451)
             -- , ("Iogon"                           , Left 0x0012E)
             -- , ("iogon"                           , Left 0x0012F)
             -- , ("Iopf"                            , Left 0x1D540)
             -- , ("iopf"                            , Left 0x1D55A)
             -- , ("iota"                            , Left 0x003B9)
             -- , ("iprod"                           , Left 0x02A3C)
             -- , ("iquest"                          , Left 0x000BF)
             -- , ("Iscr"                            , Left 0x02110)
             -- , ("iscr"                            , Left 0x1D4BE)
             -- , ("isindot"                         , Left 0x022F5)
             -- , ("isinE"                           , Left 0x022F9)
             -- , ("isin"                            , Left 0x02208)
             -- , ("isins"                           , Left 0x022F4)
             -- , ("isinsv"                          , Left 0x022F3)
             -- , ("isinv"                           , Left 0x02208)
             -- , ("Itilde"                          , Left 0x00128)
             -- , ("itilde"                          , Left 0x00129)
             -- , ("it"                              , Left 0x02062)
             -- , ("Iukcy"                           , Left 0x00406)
             -- , ("iukcy"                           , Left 0x00456)
             -- , ("Iuml"                            , Left 0x000CF)
             -- , ("iuml"                            , Left 0x000EF)
             -- , ("Jcirc"                           , Left 0x00134)
             -- , ("jcirc"                           , Left 0x00135)
             -- , ("Jcy"                             , Left 0x00419)
             -- , ("jcy"                             , Left 0x00439)
             -- , ("Jfr"                             , Left 0x1D50D)
             -- , ("jfr"                             , Left 0x1D527)
             -- , ("jmath"                           , Left 0x0006A)
             -- , ("Jopf"                            , Left 0x1D541)
             -- , ("jopf"                            , Left 0x1D55B)
             -- , ("Jscr"                            , Left 0x1D4A5)
             -- , ("jscr"                            , Left 0x1D4BF)
             -- , ("Jsercy"                          , Left 0x00408)
             -- , ("jsercy"                          , Left 0x00458)
             -- , ("Jukcy"                           , Left 0x00404)
             -- , ("jukcy"                           , Left 0x00454)
             -- , ("kappa"                           , Left 0x003BA)
             -- , ("kappav"                          , Left 0x003F0)
             -- , ("Kcedil"                          , Left 0x00136)
             -- , ("kcedil"                          , Left 0x00137)
             -- , ("Kcy"                             , Left 0x0041A)
             -- , ("kcy"                             , Left 0x0043A)
             -- , ("Kfr"                             , Left 0x1D50E)
             -- , ("kfr"                             , Left 0x1D528)
             -- , ("kgreen"                          , Left 0x00138)
             -- , ("KHcy"                            , Left 0x00425)
             -- , ("khcy"                            , Left 0x00445)
             -- , ("KJcy"                            , Left 0x0040C)
             -- , ("kjcy"                            , Left 0x0045C)
             -- , ("Kopf"                            , Left 0x1D542)
             -- , ("kopf"                            , Left 0x1D55C)
             -- , ("Kscr"                            , Left 0x1D4A6)
             -- , ("kscr"                            , Left 0x1D4C0)
             -- , ("lAarr"                           , Left 0x021DA)
             -- , ("Lacute"                          , Left 0x00139)
             -- , ("lacute"                          , Left 0x0013A)
             -- , ("laemptyv"                        , Left 0x029B4)
             -- , ("lagran"                          , Left 0x02112)
             -- , ("Lambda"                          , Left 0x0039B)
             -- , ("lambda"                          , Left 0x003BB)
             -- , ("langd"                           , Left 0x02991)
             -- , ("lang"                            , Left 0x02329)
             -- , ("Lang"                            , Left 0x0300A)
             -- , ("langle"                          , Left 0x02329)
             -- , ("Laplacetrf"                      , Left 0x02112)
             -- , ("lap"                             , Left 0x02A85)
             -- , ("laquo"                           , Left 0x000AB)
             -- , ("larrbfs"                         , Left 0x0291F)
             -- , ("larrb"                           , Left 0x021E4)
             -- , ("larrfs"                          , Left 0x0291D)
             -- , ("larrhk"                          , Left 0x021A9)
             -- , ("larr"                            , Left 0x02190)
             -- , ("Larr"                            , Left 0x0219E)
             -- , ("lArr"                            , Left 0x021D0)
             -- , ("larrlp"                          , Left 0x021AB)
             -- , ("larrpl"                          , Left 0x02939)
             -- , ("larrsim"                         , Left 0x02973)
             -- , ("larrtl"                          , Left 0x021A2)
             -- , ("latail"                          , Left 0x02919)
             -- , ("lAtail"                          , Left 0x0291B)
             -- , ("late"                            , Left 0x02AAD)
             -- , ("lat"                             , Left 0x02AAB)
             -- , ("lbarr"                           , Left 0x0290C)
             -- , ("lBarr"                           , Left 0x0290E)
             -- , ("lbbrk"                           , Left 0x03014)
             -- , ("lbrace"                          , Left 0x0007B)
             -- , ("lbrack"                          , Left 0x0005B)
             -- , ("lbrke"                           , Left 0x0298B)
             -- , ("lbrksld"                         , Left 0x0298F)
             -- , ("lbrkslu"                         , Left 0x0298D)
             -- , ("Lcaron"                          , Left 0x0013D)
             -- , ("lcaron"                          , Left 0x0013E)
             -- , ("Lcedil"                          , Left 0x0013B)
             -- , ("lcedil"                          , Left 0x0013C)
             -- , ("lceil"                           , Left 0x02308)
             -- , ("lcub"                            , Left 0x0007B)
             -- , ("Lcy"                             , Left 0x0041B)
             -- , ("lcy"                             , Left 0x0043B)
             -- , ("ldca"                            , Left 0x02936)
             -- , ("ldquo"                           , Left 0x0201C)
             -- , ("ldquor"                          , Left 0x0201E)
             -- , ("ldrdhar"                         , Left 0x02967)
             -- , ("ldrushar"                        , Left 0x0294B)
             -- , ("ldsh"                            , Left 0x021B2)
             -- , ("LeftAngleBracket"                , Left 0x02329)
             -- , ("LeftArrowBar"                    , Left 0x021E4)
             -- , ("leftarrow"                       , Left 0x02190)
             -- , ("LeftArrow"                       , Left 0x02190)
             -- , ("Leftarrow"                       , Left 0x021D0)
             -- , ("LeftArrowRightArrow"             , Left 0x021C6)
             -- , ("leftarrowtail"                   , Left 0x021A2)
             -- , ("LeftCeiling"                     , Left 0x02308)
             -- , ("LeftDoubleBracket"               , Left 0x0301A)
             -- , ("LeftDownTeeVector"               , Left 0x02961)
             -- , ("LeftDownVectorBar"               , Left 0x02959)
             -- , ("LeftDownVector"                  , Left 0x021C3)
             -- , ("LeftFloor"                       , Left 0x0230A)
             -- , ("leftharpoondown"                 , Left 0x021BD)
             -- , ("leftharpoonup"                   , Left 0x021BC)
             -- , ("leftleftarrows"                  , Left 0x021C7)
             -- , ("leftrightarrow"                  , Left 0x02194)
             -- , ("LeftRightArrow"                  , Left 0x02194)
             -- , ("Leftrightarrow"                  , Left 0x021D4)
             -- , ("leftrightarrows"                 , Left 0x021C6)
             -- , ("leftrightharpoons"               , Left 0x021CB)
             -- , ("leftrightsquigarrow"             , Left 0x021AD)
             -- , ("LeftRightVector"                 , Left 0x0294E)
             -- , ("LeftTeeArrow"                    , Left 0x021A4)
             -- , ("LeftTee"                         , Left 0x022A3)
             -- , ("LeftTeeVector"                   , Left 0x0295A)
             -- , ("leftthreetimes"                  , Left 0x022CB)
             -- , ("LeftTriangleBar"                 , Left 0x029CF)
             -- , ("LeftTriangleEqual"               , Left 0x022B4)
             -- , ("LeftTriangle"                    , Left 0x022B2)
             -- , ("LeftUpDownVector"                , Left 0x02951)
             -- , ("LeftUpTeeVector"                 , Left 0x02960)
             -- , ("LeftUpVectorBar"                 , Left 0x02958)
             -- , ("LeftUpVector"                    , Left 0x021BF)
             -- , ("LeftVectorBar"                   , Left 0x02952)
             -- , ("LeftVector"                      , Left 0x021BC)
             -- , ("leg"                             , Left 0x022DA)
             -- , ("lEg"                             , Left 0x02A8B)
             -- , ("le"                              , Left 0x02264)
             -- , ("lE"                              , Left 0x02266)
             -- , ("leq"                             , Left 0x02264)
             -- , ("leqq"                            , Left 0x02266)
             -- , ("leqslant"                        , Left 0x02A7D)
             -- , ("lescc"                           , Left 0x02AA8)
             -- , ("lesdot"                          , Left 0x02A7F)
             -- , ("lesdoto"                         , Left 0x02A81)
             -- , ("lesdotor"                        , Left 0x02A83)
             -- , ("lesges"                          , Left 0x02A93)
             -- , ("les"                             , Left 0x02A7D)
             -- , ("lessapprox"                      , Left 0x02A85)
             -- , ("lessdot"                         , Left 0x022D6)
             -- , ("lesseqgtr"                       , Left 0x022DA)
             -- , ("lesseqqgtr"                      , Left 0x02A8B)
             -- , ("LessEqualGreater"                , Left 0x022DA)
             -- , ("LessFullEqual"                   , Left 0x02266)
             -- , ("LessGreater"                     , Left 0x02276)
             -- , ("lessgtr"                         , Left 0x02276)
             -- , ("LessLess"                        , Left 0x02AA1)
             -- , ("lesssim"                         , Left 0x02272)
             -- , ("LessSlantEqual"                  , Left 0x02A7D)
             -- , ("LessTilde"                       , Left 0x02272)
             -- , ("lfisht"                          , Left 0x0297C)
             -- , ("lfloor"                          , Left 0x0230A)
             -- , ("Lfr"                             , Left 0x1D50F)
             -- , ("lfr"                             , Left 0x1D529)
             -- , ("lgE"                             , Left 0x02A91)
             -- , ("lg"                              , Left 0x02276)
             -- , ("lhard"                           , Left 0x021BD)
             -- , ("lHar"                            , Left 0x02962)
             -- , ("lharu"                           , Left 0x021BC)
             -- , ("lharul"                          , Left 0x0296A)
             -- , ("lhblk"                           , Left 0x02584)
             -- , ("LJcy"                            , Left 0x00409)
             -- , ("ljcy"                            , Left 0x00459)
             -- , ("llarr"                           , Left 0x021C7)
             -- , ("llcorner"                        , Left 0x0231E)
             -- , ("Lleftarrow"                      , Left 0x021DA)
             -- , ("llhard"                          , Left 0x0296B)
             -- , ("ll"                              , Left 0x0226A)
             -- , ("Ll"                              , Left 0x022D8)
             -- , ("lltri"                           , Left 0x025FA)
             -- , ("Lmidot"                          , Left 0x0013F)
             -- , ("lmidot"                          , Left 0x00140)
             -- , ("lmoustache"                      , Left 0x023B0)
             -- , ("lmoust"                          , Left 0x023B0)
             -- , ("lnap"                            , Left 0x02A89)
             -- , ("lnapprox"                        , Left 0x02A89)
             -- , ("lnE"                             , Left 0x02268)
             -- , ("lne"                             , Left 0x02A87)
             -- , ("lneq"                            , Left 0x02A87)
             -- , ("lneqq"                           , Left 0x02268)
             -- , ("lnsim"                           , Left 0x022E6)
             -- , ("loang"                           , Left 0x03018)
             -- , ("loarr"                           , Left 0x021FD)
             -- , ("lobrk"                           , Left 0x0301A)
             -- , ("longleftarrow"                   , Left 0x027F5)
             -- , ("LongLeftArrow"                   , Left 0x027F5)
             -- , ("Longleftarrow"                   , Left 0x027F8)
             -- , ("longleftrightarrow"              , Left 0x027F7)
             -- , ("LongLeftRightArrow"              , Left 0x027F7)
             -- , ("Longleftrightarrow"              , Left 0x027FA)
             -- , ("longmapsto"                      , Left 0x027FC)
             -- , ("longrightarrow"                  , Left 0x027F6)
             -- , ("LongRightArrow"                  , Left 0x027F6)
             -- , ("Longrightarrow"                  , Left 0x027F9)
             -- , ("looparrowleft"                   , Left 0x021AB)
             -- , ("looparrowright"                  , Left 0x021AC)
             -- , ("lopar"                           , Left 0x02985)
             -- , ("Lopf"                            , Left 0x1D543)
             -- , ("lopf"                            , Left 0x1D55D)
             -- , ("loplus"                          , Left 0x02A2D)
             -- , ("lotimes"                         , Left 0x02A34)
             -- , ("lowast"                          , Left 0x02217)
             -- , ("lowbar"                          , Left 0x0005F)
             -- , ("LowerLeftArrow"                  , Left 0x02199)
             -- , ("LowerRightArrow"                 , Left 0x02198)
             -- , ("lozenge"                         , Left 0x025CA)
             -- , ("lozf"                            , Left 0x029EB)
             -- , ("loz"                             , Left 0x025CA)
             -- , ("lpar"                            , Left 0x00028)
             -- , ("lparlt"                          , Left 0x02993)
             -- , ("lrarr"                           , Left 0x021C6)
             -- , ("lrcorner"                        , Left 0x0231F)
             -- , ("lrhard"                          , Left 0x0296D)
             -- , ("lrhar"                           , Left 0x021CB)
             -- , ("lrtri"                           , Left 0x022BF)
             -- , ("Lscr"                            , Left 0x02112)
             -- , ("lscr"                            , Left 0x1D4C1)
             -- , ("lsh"                             , Left 0x021B0)
             -- , ("Lsh"                             , Left 0x021B0)
             -- , ("lsime"                           , Left 0x02A8D)
             -- , ("lsimg"                           , Left 0x02A8F)
             -- , ("lsim"                            , Left 0x02272)
             -- , ("lsqb"                            , Left 0x0005B)
             -- , ("lsquo"                           , Left 0x02018)
             -- , ("lsquor"                          , Left 0x0201A)
             -- , ("Lstrok"                          , Left 0x00141)
             -- , ("lstrok"                          , Left 0x00142)
             -- , ("ltcc"                            , Left 0x02AA6)
             -- , ("ltcir"                           , Left 0x02A79)
             -- , ("ltdot"                           , Left 0x022D6)
             -- , ("lthree"                          , Left 0x022CB)
             -- , ("ltimes"                          , Left 0x022C9)
             -- , ("ltlarr"                          , Left 0x02976)
             -- , ("lt"                              , Left 0x0003C)
             -- , ("Lt"                              , Left 0x0226A)
             -- , ("ltquest"                         , Left 0x02A7B)
             -- , ("ltrie"                           , Left 0x022B4)
             -- , ("ltrif"                           , Left 0x025C2)
             -- , ("ltri"                            , Left 0x025C3)
             -- , ("ltrPar"                          , Left 0x02996)
             -- , ("lurdshar"                        , Left 0x0294A)
             -- , ("luruhar"                         , Left 0x02966)
             -- , ("macr"                            , Left 0x000AF)
             -- , ("male"                            , Left 0x02642)
             -- , ("maltese"                         , Left 0x02720)
             -- , ("malt"                            , Left 0x02720)
             -- , ("map"                             , Left 0x021A6)
             -- , ("Map"                             , Left 0x02905)
             -- , ("mapstodown"                      , Left 0x021A7)
             -- , ("mapsto"                          , Left 0x021A6)
             -- , ("mapstoleft"                      , Left 0x021A4)
             -- , ("mapstoup"                        , Left 0x021A5)
             -- , ("marker"                          , Left 0x025AE)
             -- , ("mcomma"                          , Left 0x02A29)
             -- , ("Mcy"                             , Left 0x0041C)
             -- , ("mcy"                             , Left 0x0043C)
             -- , ("mdash"                           , Left 0x02014)
             -- , ("mDDot"                           , Left 0x0223A)
             -- , ("measuredangle"                   , Left 0x02221)
             -- , ("MediumSpace"                     , Left 0x0205F)
             -- , ("Mellintrf"                       , Left 0x02133)
             -- , ("Mfr"                             , Left 0x1D510)
             -- , ("mfr"                             , Left 0x1D52A)
             -- , ("mho"                             , Left 0x02127)
             -- , ("micro"                           , Left 0x000B5)
             -- , ("midast"                          , Left 0x0002A)
             -- , ("midcir"                          , Left 0x02AF0)
             -- , ("middot"                          , Left 0x000B7)
             -- , ("mid"                             , Left 0x02223)
             -- , ("minusb"                          , Left 0x0229F)
             -- , ("minusd"                          , Left 0x02238)
             -- , ("minusdu"                         , Left 0x02A2A)
             -- , ("minus"                           , Left 0x02212)
             -- , ("MinusPlus"                       , Left 0x02213)
             -- , ("mlcp"                            , Left 0x02ADB)
             -- , ("mldr"                            , Left 0x02026)
             -- , ("mnplus"                          , Left 0x02213)
             -- , ("models"                          , Left 0x022A7)
             -- , ("Mopf"                            , Left 0x1D544)
             -- , ("mopf"                            , Left 0x1D55E)
             -- , ("mp"                              , Left 0x02213)
             -- , ("Mscr"                            , Left 0x02133)
             -- , ("mscr"                            , Left 0x1D4C2)
             -- , ("mstpos"                          , Left 0x0223E)
             -- , ("mu"                              , Left 0x003BC)
             -- , ("multimap"                        , Left 0x022B8)
             -- , ("mumap"                           , Left 0x022B8)
             -- , ("nabla"                           , Left 0x02207)
             -- , ("Nacute"                          , Left 0x00143)
             -- , ("nacute"                          , Left 0x00144)
             -- , ("nap"                             , Left 0x02249)
             -- , ("napos"                           , Left 0x00149)
             -- , ("napprox"                         , Left 0x02249)
             -- , ("natural"                         , Left 0x0266E)
             -- , ("naturals"                        , Left 0x02115)
             -- , ("natur"                           , Left 0x0266E)
             -- , ("nbsp"                            , Left 0x000A0)
             -- , ("ncap"                            , Left 0x02A43)
             -- , ("Ncaron"                          , Left 0x00147)
             -- , ("ncaron"                          , Left 0x00148)
             -- , ("Ncedil"                          , Left 0x00145)
             -- , ("ncedil"                          , Left 0x00146)
             -- , ("ncong"                           , Left 0x02247)
             -- , ("ncup"                            , Left 0x02A42)
             -- , ("Ncy"                             , Left 0x0041D)
             -- , ("ncy"                             , Left 0x0043D)
             -- , ("ndash"                           , Left 0x02013)
             -- , ("nearhk"                          , Left 0x02924)
             -- , ("nearr"                           , Left 0x02197)
             -- , ("neArr"                           , Left 0x021D7)
             -- , ("nearrow"                         , Left 0x02197)
             -- , ("NegativeMediumSpace"             , Left 0x0200B)
             -- , ("NegativeThickSpace"              , Left 0x0200B)
             -- , ("NegativeThinSpace"               , Left 0x0200B)
             -- , ("NegativeVeryThinSpace"           , Left 0x0200B)
             -- , ("ne"                              , Left 0x02260)
             -- , ("nequiv"                          , Left 0x02262)
             -- , ("nesear"                          , Left 0x02928)
             -- , ("NestedGreaterGreater"            , Left 0x0226B)
             -- , ("NestedLessLess"                  , Left 0x0226A)
             -- , ("NewLine"                         , Left 0x0000A)
             -- , ("nexist"                          , Left 0x02204)
             -- , ("nexists"                         , Left 0x02204)
             -- , ("Nfr"                             , Left 0x1D511)
             -- , ("nfr"                             , Left 0x1D52B)
             -- , ("nge"                             , Left 0x02271)
             -- , ("ngeq"                            , Left 0x02271)
             -- , ("ngsim"                           , Left 0x02275)
             -- , ("ngt"                             , Left 0x0226F)
             -- , ("ngtr"                            , Left 0x0226F)
             -- , ("nharr"                           , Left 0x021AE)
             -- , ("nhArr"                           , Left 0x021CE)
             -- , ("nhpar"                           , Left 0x02AF2)
             -- , ("ni"                              , Left 0x0220B)
             -- , ("nisd"                            , Left 0x022FA)
             -- , ("nis"                             , Left 0x022FC)
             -- , ("niv"                             , Left 0x0220B)
             -- , ("NJcy"                            , Left 0x0040A)
             -- , ("njcy"                            , Left 0x0045A)
             -- , ("nlarr"                           , Left 0x0219A)
             -- , ("nlArr"                           , Left 0x021CD)
             -- , ("nldr"                            , Left 0x02025)
             -- , ("nleftarrow"                      , Left 0x0219A)
             -- , ("nLeftarrow"                      , Left 0x021CD)
             -- , ("nleftrightarrow"                 , Left 0x021AE)
             -- , ("nLeftrightarrow"                 , Left 0x021CE)
             -- , ("nle"                             , Left 0x02270)
             -- , ("nleq"                            , Left 0x02270)
             -- , ("nless"                           , Left 0x0226E)
             -- , ("nlsim"                           , Left 0x02274)
             -- , ("nlt"                             , Left 0x0226E)
             -- , ("nltrie"                          , Left 0x022EC)
             -- , ("nltri"                           , Left 0x022EA)
             -- , ("nmid"                            , Left 0x02224)
             -- , ("NoBreak"                         , Left 0x02060)
             -- , ("NonBreakingSpace"                , Left 0x000A0)
             -- , ("Nopf"                            , Left 0x02115)
             -- , ("nopf"                            , Left 0x1D55F)
             -- , ("NotCongruent"                    , Left 0x02262)
             -- , ("NotCupCap"                       , Left 0x0226D)
             -- , ("NotDoubleVerticalBar"            , Left 0x02226)
             -- , ("NotElement"                      , Left 0x02209)
             -- , ("NotEqual"                        , Left 0x02260)
             -- , ("NotExists"                       , Left 0x02204)
             -- , ("NotGreaterEqual"                 , Left 0x02271)
             -- , ("NotGreater"                      , Left 0x0226F)
             -- , ("NotGreaterLess"                  , Left 0x02279)
             -- , ("NotGreaterTilde"                 , Left 0x02275)
             -- , ("notin"                           , Left 0x02209)
             -- , ("notinva"                         , Left 0x02209)
             -- , ("notinvb"                         , Left 0x022F7)
             -- , ("notinvc"                         , Left 0x022F6)
             -- , ("not"                             , Left 0x000AC)
             -- , ("Not"                             , Left 0x02AEC)
             -- , ("NotLeftTriangleEqual"            , Left 0x022EC)
             -- , ("NotLeftTriangle"                 , Left 0x022EA)
             -- , ("NotLessEqual"                    , Left 0x02270)
             -- , ("NotLessGreater"                  , Left 0x02278)
             -- , ("NotLess"                         , Left 0x0226E)
             -- , ("NotLessTilde"                    , Left 0x02274)
             -- , ("notni"                           , Left 0x0220C)
             -- , ("notniva"                         , Left 0x0220C)
             -- , ("notnivb"                         , Left 0x022FE)
             -- , ("notnivc"                         , Left 0x022FD)
             -- , ("NotPrecedes"                     , Left 0x02280)
             -- , ("NotPrecedesSlantEqual"           , Left 0x022E0)
             -- , ("NotReverseElement"               , Left 0x0220C)
             -- , ("NotRightTriangleEqual"           , Left 0x022ED)
             -- , ("NotRightTriangle"                , Left 0x022EB)
             -- , ("NotSquareSubsetEqual"            , Left 0x022E2)
             -- , ("NotSquareSupersetEqual"          , Left 0x022E3)
             -- , ("NotSubsetEqual"                  , Left 0x02288)
             -- , ("NotSucceeds"                     , Left 0x02281)
             -- , ("NotSucceedsSlantEqual"           , Left 0x022E1)
             -- , ("NotSupersetEqual"                , Left 0x02289)
             -- , ("NotTildeEqual"                   , Left 0x02244)
             -- , ("NotTildeFullEqual"               , Left 0x02247)
             -- , ("NotTilde"                        , Left 0x02241)
             -- , ("NotTildeTilde"                   , Left 0x02249)
             -- , ("NotVerticalBar"                  , Left 0x02224)
             -- , ("nparallel"                       , Left 0x02226)
             -- , ("npar"                            , Left 0x02226)
             -- , ("npolint"                         , Left 0x02A14)
             -- , ("nprcue"                          , Left 0x022E0)
             -- , ("nprec"                           , Left 0x02280)
             -- , ("npr"                             , Left 0x02280)
             -- , ("nrarr"                           , Left 0x0219B)
             -- , ("nrArr"                           , Left 0x021CF)
             -- , ("nrightarrow"                     , Left 0x0219B)
             -- , ("nRightarrow"                     , Left 0x021CF)
             -- , ("nrtrie"                          , Left 0x022ED)
             -- , ("nrtri"                           , Left 0x022EB)
             -- , ("nsccue"                          , Left 0x022E1)
             -- , ("nsc"                             , Left 0x02281)
             -- , ("Nscr"                            , Left 0x1D4A9)
             -- , ("nscr"                            , Left 0x1D4C3)
             -- , ("nshortmid"                       , Left 0x02224)
             -- , ("nshortparallel"                  , Left 0x02226)
             -- , ("nsime"                           , Left 0x02244)
             -- , ("nsimeq"                          , Left 0x02244)
             -- , ("nsim"                            , Left 0x02241)
             -- , ("nsmid"                           , Left 0x02224)
             -- , ("nspar"                           , Left 0x02226)
             -- , ("nsqsube"                         , Left 0x022E2)
             -- , ("nsqsupe"                         , Left 0x022E3)
             -- , ("nsube"                           , Left 0x02288)
             -- , ("nsub"                            , Left 0x02284)
             -- , ("nsubseteq"                       , Left 0x02288)
             -- , ("nsucc"                           , Left 0x02281)
             -- , ("nsupe"                           , Left 0x02289)
             -- , ("nsup"                            , Left 0x02285)
             -- , ("nsupseteq"                       , Left 0x02289)
             -- , ("ntgl"                            , Left 0x02279)
             -- , ("Ntilde"                          , Left 0x000D1)
             -- , ("ntilde"                          , Left 0x000F1)
             -- , ("ntlg"                            , Left 0x02278)
             -- , ("ntrianglelefteq"                 , Left 0x022EC)
             -- , ("ntriangleleft"                   , Left 0x022EA)
             -- , ("ntrianglerighteq"                , Left 0x022ED)
             -- , ("ntriangleright"                  , Left 0x022EB)
             -- , ("nu"                              , Left 0x003BD)
             -- , ("numero"                          , Left 0x02116)
             -- , ("num"                             , Left 0x00023)
             -- , ("numsp"                           , Left 0x02007)
             -- , ("nvdash"                          , Left 0x022AC)
             -- , ("nvDash"                          , Left 0x022AD)
             -- , ("nVdash"                          , Left 0x022AE)
             -- , ("nVDash"                          , Left 0x022AF)
             -- , ("nvHarr"                          , Left 0x02904)
             -- , ("nvinfin"                         , Left 0x029DE)
             -- , ("nvlArr"                          , Left 0x02902)
             -- , ("nvrArr"                          , Left 0x02903)
             -- , ("nwarhk"                          , Left 0x02923)
             -- , ("nwarr"                           , Left 0x02196)
             -- , ("nwArr"                           , Left 0x021D6)
             -- , ("nwarrow"                         , Left 0x02196)
             -- , ("nwnear"                          , Left 0x02927)
             -- , ("Oacute"                          , Left 0x000D3)
             -- , ("oacute"                          , Left 0x000F3)
             -- , ("oast"                            , Left 0x0229B)
             -- , ("Ocirc"                           , Left 0x000D4)
             -- , ("ocirc"                           , Left 0x000F4)
             -- , ("ocir"                            , Left 0x0229A)
             -- , ("Ocy"                             , Left 0x0041E)
             -- , ("ocy"                             , Left 0x0043E)
             -- , ("odash"                           , Left 0x0229D)
             -- , ("Odblac"                          , Left 0x00150)
             -- , ("odblac"                          , Left 0x00151)
             -- , ("odiv"                            , Left 0x02A38)
             -- , ("odot"                            , Left 0x02299)
             -- , ("odsold"                          , Left 0x029BC)
             -- , ("OElig"                           , Left 0x00152)
             -- , ("oelig"                           , Left 0x00153)
             -- , ("ofcir"                           , Left 0x029BF)
             -- , ("Ofr"                             , Left 0x1D512)
             -- , ("ofr"                             , Left 0x1D52C)
             -- , ("ogon"                            , Left 0x002DB)
             -- , ("Ograve"                          , Left 0x000D2)
             -- , ("ograve"                          , Left 0x000F2)
             -- , ("ogt"                             , Left 0x029C1)
             -- , ("ohbar"                           , Left 0x029B5)
             -- , ("ohm"                             , Left 0x02126)
             -- , ("oint"                            , Left 0x0222E)
             -- , ("olarr"                           , Left 0x021BA)
             -- , ("olcir"                           , Left 0x029BE)
             -- , ("olcross"                         , Left 0x029BB)
             -- , ("olt"                             , Left 0x029C0)
             -- , ("Omacr"                           , Left 0x0014C)
             -- , ("omacr"                           , Left 0x0014D)
             -- , ("Omega"                           , Left 0x003A9)
             -- , ("omega"                           , Left 0x003C9)
             -- , ("omid"                            , Left 0x029B6)
             -- , ("ominus"                          , Left 0x02296)
             -- , ("Oopf"                            , Left 0x1D546)
             -- , ("oopf"                            , Left 0x1D560)
             -- , ("opar"                            , Left 0x029B7)
             -- , ("OpenCurlyDoubleQuote"            , Left 0x0201C)
             -- , ("OpenCurlyQuote"                  , Left 0x02018)
             -- , ("operp"                           , Left 0x029B9)
             -- , ("oplus"                           , Left 0x02295)
             -- , ("orarr"                           , Left 0x021BB)
             -- , ("order"                           , Left 0x02134)
             -- , ("orderof"                         , Left 0x02134)
             -- , ("ordf"                            , Left 0x000AA)
             -- , ("ord"                             , Left 0x02A5D)
             -- , ("ordm"                            , Left 0x000BA)
             -- , ("origof"                          , Left 0x022B6)
             -- , ("or"                              , Left 0x02228)
             -- , ("Or"                              , Left 0x02A54)
             -- , ("oror"                            , Left 0x02A56)
             -- , ("orslope"                         , Left 0x02A57)
             -- , ("orv"                             , Left 0x02A5B)
             -- , ("oscr"                            , Left 0x02134)
             -- , ("Oscr"                            , Left 0x1D4AA)
             -- , ("Oslash"                          , Left 0x000D8)
             -- , ("oslash"                          , Left 0x000F8)
             -- , ("oS"                              , Left 0x024C8)
             -- , ("osol"                            , Left 0x02298)
             -- , ("Otilde"                          , Left 0x000D5)
             -- , ("otilde"                          , Left 0x000F5)
             -- , ("otimesas"                        , Left 0x02A36)
             -- , ("otimes"                          , Left 0x02297)
             -- , ("Otimes"                          , Left 0x02A37)
             -- , ("Ouml"                            , Left 0x000D6)
             -- , ("ouml"                            , Left 0x000F6)
             -- , ("ovbar"                           , Left 0x0233D)
             -- , ("OverBar"                         , Left 0x000AF)
             -- , ("OverBrace"                       , Left 0x0FE37)
             -- , ("OverBracket"                     , Left 0x023B4)
             -- , ("OverParenthesis"                 , Left 0x0FE35)
             -- , ("para"                            , Left 0x000B6)
             -- , ("parallel"                        , Left 0x02225)
             -- , ("par"                             , Left 0x02225)
             -- , ("parsim"                          , Left 0x02AF3)
             -- , ("parsl"                           , Left 0x02AFD)
             -- , ("PartialD"                        , Left 0x02202)
             -- , ("part"                            , Left 0x02202)
             -- , ("Pcy"                             , Left 0x0041F)
             -- , ("pcy"                             , Left 0x0043F)
             -- , ("percnt"                          , Left 0x00025)
             -- , ("period"                          , Left 0x0002E)
             -- , ("permil"                          , Left 0x02030)
             -- , ("perp"                            , Left 0x022A5)
             -- , ("pertenk"                         , Left 0x02031)
             -- , ("Pfr"                             , Left 0x1D513)
             -- , ("pfr"                             , Left 0x1D52D)
             -- , ("Phi"                             , Left 0x003A6)
             -- , ("phi"                             , Left 0x003D5)
             -- , ("phiv"                            , Left 0x003C6)
             -- , ("phmmat"                          , Left 0x02133)
             -- , ("phone"                           , Left 0x0260E)
             -- , ("Pi"                              , Left 0x003A0)
             -- , ("pi"                              , Left 0x003C0)
             -- , ("pitchfork"                       , Left 0x022D4)
             -- , ("piv"                             , Left 0x003D6)
             -- , ("planckh"                         , Left 0x0210E)
             -- , ("planck"                          , Left 0x0210F)
             -- , ("plankv"                          , Left 0x0210F)
             -- , ("plusacir"                        , Left 0x02A23)
             -- , ("plusb"                           , Left 0x0229E)
             -- , ("pluscir"                         , Left 0x02A22)
             -- , ("plusdo"                          , Left 0x02214)
             -- , ("plusdu"                          , Left 0x02A25)
             -- , ("pluse"                           , Left 0x02A72)
             -- , ("plus"                            , Left 0x0002B)
             -- , ("PlusMinus"                       , Left 0x000B1)
             -- , ("plusmn"                          , Left 0x000B1)
             -- , ("plussim"                         , Left 0x02A26)
             -- , ("plustwo"                         , Left 0x02A27)
             -- , ("pm"                              , Left 0x000B1)
             -- , ("Poincareplane"                   , Left 0x0210C)
             -- , ("pointint"                        , Left 0x02A15)
             -- , ("Popf"                            , Left 0x02119)
             -- , ("popf"                            , Left 0x1D561)
             -- , ("pound"                           , Left 0x000A3)
             -- , ("prap"                            , Left 0x02AB7)
             -- , ("prcue"                           , Left 0x0227C)
             -- , ("precapprox"                      , Left 0x02AB7)
             -- , ("preccurlyeq"                     , Left 0x0227C)
             -- , ("PrecedesEqual"                   , Left 0x02AAF)
             -- , ("Precedes"                        , Left 0x0227A)
             -- , ("PrecedesSlantEqual"              , Left 0x0227C)
             -- , ("PrecedesTilde"                   , Left 0x0227E)
             -- , ("preceq"                          , Left 0x02AAF)
             -- , ("prec"                            , Left 0x0227A)
             -- , ("precnapprox"                     , Left 0x02AB9)
             -- , ("precneqq"                        , Left 0x02AB5)
             -- , ("precnsim"                        , Left 0x022E8)
             -- , ("precsim"                         , Left 0x0227E)
             -- , ("pre"                             , Left 0x02AAF)
             -- , ("prE"                             , Left 0x02AB3)
             -- , ("prime"                           , Left 0x02032)
             -- , ("Prime"                           , Left 0x02033)
             -- , ("primes"                          , Left 0x02119)
             -- , ("pr"                              , Left 0x0227A)
             -- , ("Pr"                              , Left 0x02ABB)
             -- , ("prnap"                           , Left 0x02AB9)
             -- , ("prnE"                            , Left 0x02AB5)
             -- , ("prnsim"                          , Left 0x022E8)
             -- , ("prod"                            , Left 0x0220F)
             -- , ("Product"                         , Left 0x0220F)
             -- , ("profalar"                        , Left 0x0232E)
             -- , ("profline"                        , Left 0x02312)
             -- , ("profsurf"                        , Left 0x02313)
             -- , ("prop"                            , Left 0x0221D)
             -- , ("Proportional"                    , Left 0x0221D)
             -- , ("Proportion"                      , Left 0x02237)
             -- , ("propto"                          , Left 0x0221D)
             -- , ("prsim"                           , Left 0x0227E)
             -- , ("prurel"                          , Left 0x022B0)
             -- , ("Pscr"                            , Left 0x1D4AB)
             -- , ("pscr"                            , Left 0x1D4C5)
             -- , ("Psi"                             , Left 0x003A8)
             -- , ("psi"                             , Left 0x003C8)
             -- , ("puncsp"                          , Left 0x02008)
             -- , ("Qfr"                             , Left 0x1D514)
             -- , ("qfr"                             , Left 0x1D52E)
             -- , ("qint"                            , Left 0x02A0C)
             -- , ("Qopf"                            , Left 0x0211A)
             -- , ("qopf"                            , Left 0x1D562)
             -- , ("qprime"                          , Left 0x02057)
             -- , ("Qscr"                            , Left 0x1D4AC)
             -- , ("qscr"                            , Left 0x1D4C6)
             -- , ("quaternions"                     , Left 0x0210D)
             -- , ("quatint"                         , Left 0x02A16)
             -- , ("questeq"                         , Left 0x0225F)
             -- , ("quest"                           , Left 0x0003F)
             -- , ("quot"                            , Left 0x00022)
             -- , ("rAarr"                           , Left 0x021DB)
             -- , ("race"                            , Left 0x029DA)
             -- , ("Racute"                          , Left 0x00154)
             -- , ("racute"                          , Left 0x00155)
             -- , ("radic"                           , Left 0x0221A)
             -- , ("raemptyv"                        , Left 0x029B3)
             -- , ("rangd"                           , Left 0x02992)
             -- , ("range"                           , Left 0x029A5)
             -- , ("rang"                            , Left 0x0232A)
             -- , ("Rang"                            , Left 0x0300B)
             -- , ("rangle"                          , Left 0x0232A)
             -- , ("raquo"                           , Left 0x000BB)
             -- , ("rarrap"                          , Left 0x02975)
             -- , ("rarrbfs"                         , Left 0x02920)
             -- , ("rarrb"                           , Left 0x021E5)
             -- , ("rarrc"                           , Left 0x02933)
             -- , ("rarrfs"                          , Left 0x0291E)
             -- , ("rarrhk"                          , Left 0x021AA)
             -- , ("rarr"                            , Left 0x02192)
             -- , ("Rarr"                            , Left 0x021A0)
             -- , ("rArr"                            , Left 0x021D2)
             -- , ("rarrlp"                          , Left 0x021AC)
             -- , ("rarrpl"                          , Left 0x02945)
             -- , ("rarrsim"                         , Left 0x02974)
             -- , ("rarrtl"                          , Left 0x021A3)
             -- , ("Rarrtl"                          , Left 0x02916)
             -- , ("rarrw"                           , Left 0x0219D)
             -- , ("ratail"                          , Left 0x0291A)
             -- , ("rAtail"                          , Left 0x0291C)
             -- , ("ratio"                           , Left 0x02236)
             -- , ("rationals"                       , Left 0x0211A)
             -- , ("rbarr"                           , Left 0x0290D)
             -- , ("rBarr"                           , Left 0x0290F)
             -- , ("RBarr"                           , Left 0x02910)
             -- , ("rbbrk"                           , Left 0x03015)
             -- , ("rbrace"                          , Left 0x0007D)
             -- , ("rbrack"                          , Left 0x0005D)
             -- , ("rbrke"                           , Left 0x0298C)
             -- , ("rbrksld"                         , Left 0x0298E)
             -- , ("rbrkslu"                         , Left 0x02990)
             -- , ("Rcaron"                          , Left 0x00158)
             -- , ("rcaron"                          , Left 0x00159)
             -- , ("Rcedil"                          , Left 0x00156)
             -- , ("rcedil"                          , Left 0x00157)
             -- , ("rceil"                           , Left 0x02309)
             -- , ("rcub"                            , Left 0x0007D)
             -- , ("Rcy"                             , Left 0x00420)
             -- , ("rcy"                             , Left 0x00440)
             -- , ("rdca"                            , Left 0x02937)
             -- , ("rdldhar"                         , Left 0x02969)
             -- , ("rdquo"                           , Left 0x0201D)
             -- , ("rdquor"                          , Left 0x0201D)
             -- , ("rdsh"                            , Left 0x021B3)
             -- , ("realine"                         , Left 0x0211B)
             -- , ("real"                            , Left 0x0211C)
             -- , ("realpart"                        , Left 0x0211C)
             -- , ("reals"                           , Left 0x0211D)
             -- , ("rect"                            , Left 0x025AD)
             -- , ("reg"                             , Left 0x000AE)
             -- , ("Re"                              , Left 0x0211C)
             -- , ("ReverseElement"                  , Left 0x0220B)
             -- , ("ReverseEquilibrium"              , Left 0x021CB)
             -- , ("ReverseUpEquilibrium"            , Left 0x0296F)
             -- , ("rfisht"                          , Left 0x0297D)
             -- , ("rfloor"                          , Left 0x0230B)
             -- , ("Rfr"                             , Left 0x0211C)
             -- , ("rfr"                             , Left 0x1D52F)
             -- , ("rhard"                           , Left 0x021C1)
             -- , ("rHar"                            , Left 0x02964)
             -- , ("rharu"                           , Left 0x021C0)
             -- , ("rharul"                          , Left 0x0296C)
             -- , ("rho"                             , Left 0x003C1)
             -- , ("rhov"                            , Left 0x003F1)
             -- , ("RightAngleBracket"               , Left 0x0232A)
             -- , ("RightArrowBar"                   , Left 0x021E5)
             -- , ("rightarrow"                      , Left 0x02192)
             -- , ("RightArrow"                      , Left 0x02192)
             -- , ("Rightarrow"                      , Left 0x021D2)
             -- , ("RightArrowLeftArrow"             , Left 0x021C4)
             -- , ("rightarrowtail"                  , Left 0x021A3)
             -- , ("RightCeiling"                    , Left 0x02309)
             -- , ("RightDoubleBracket"              , Left 0x0301B)
             -- , ("RightDownTeeVector"              , Left 0x0295D)
             -- , ("RightDownVectorBar"              , Left 0x02955)
             -- , ("RightDownVector"                 , Left 0x021C2)
             -- , ("RightFloor"                      , Left 0x0230B)
             -- , ("rightharpoondown"                , Left 0x021C1)
             -- , ("rightharpoonup"                  , Left 0x021C0)
             -- , ("rightleftarrows"                 , Left 0x021C4)
             -- , ("rightleftharpoons"               , Left 0x021CC)
             -- , ("rightrightarrows"                , Left 0x021C9)
             -- , ("rightsquigarrow"                 , Left 0x0219D)
             -- , ("RightTeeArrow"                   , Left 0x021A6)
             -- , ("RightTee"                        , Left 0x022A2)
             -- , ("RightTeeVector"                  , Left 0x0295B)
             -- , ("rightthreetimes"                 , Left 0x022CC)
             -- , ("RightTriangleBar"                , Left 0x029D0)
             -- , ("RightTriangleEqual"              , Left 0x022B5)
             -- , ("RightTriangle"                   , Left 0x022B3)
             -- , ("RightUpDownVector"               , Left 0x0294F)
             -- , ("RightUpTeeVector"                , Left 0x0295C)
             -- , ("RightUpVectorBar"                , Left 0x02954)
             -- , ("RightUpVector"                   , Left 0x021BE)
             -- , ("RightVectorBar"                  , Left 0x02953)
             -- , ("RightVector"                     , Left 0x021C0)
             -- , ("ring"                            , Left 0x002DA)
             -- , ("risingdotseq"                    , Left 0x02253)
             -- , ("rlarr"                           , Left 0x021C4)
             -- , ("rlhar"                           , Left 0x021CC)
             -- , ("rmoustache"                      , Left 0x023B1)
             -- , ("rmoust"                          , Left 0x023B1)
             -- , ("rnmid"                           , Left 0x02AEE)
             -- , ("roang"                           , Left 0x03019)
             -- , ("roarr"                           , Left 0x021FE)
             -- , ("robrk"                           , Left 0x0301B)
             -- , ("ropar"                           , Left 0x02986)
             -- , ("Ropf"                            , Left 0x0211D)
             -- , ("ropf"                            , Left 0x1D563)
             -- , ("roplus"                          , Left 0x02A2E)
             -- , ("rotimes"                         , Left 0x02A35)
             -- , ("RoundImplies"                    , Left 0x02970)
             -- , ("rpargt"                          , Left 0x02994)
             -- , ("rpar"                            , Left 0x00029)
             -- , ("rppolint"                        , Left 0x02A12)
             -- , ("rrarr"                           , Left 0x021C9)
             -- , ("Rrightarrow"                     , Left 0x021DB)
             -- , ("Rscr"                            , Left 0x0211B)
             -- , ("rscr"                            , Left 0x1D4C7)
             -- , ("rsh"                             , Left 0x021B1)
             -- , ("Rsh"                             , Left 0x021B1)
             -- , ("rsqb"                            , Left 0x0005D)
             -- , ("rsquo"                           , Left 0x02019)
             -- , ("rsquor"                          , Left 0x02019)
             -- , ("rthree"                          , Left 0x022CC)
             -- , ("rtimes"                          , Left 0x022CA)
             -- , ("rtrie"                           , Left 0x022B5)
             -- , ("rtrif"                           , Left 0x025B8)
             -- , ("rtri"                            , Left 0x025B9)
             -- , ("rtriltri"                        , Left 0x029CE)
             -- , ("RuleDelayed"                     , Left 0x029F4)
             -- , ("ruluhar"                         , Left 0x02968)
             -- , ("rx"                              , Left 0x0211E)
             -- , ("Sacute"                          , Left 0x0015A)
             -- , ("sacute"                          , Left 0x0015B)
             -- , ("scap"                            , Left 0x02AB8)
             -- , ("Scaron"                          , Left 0x00160)
             -- , ("scaron"                          , Left 0x00161)
             -- , ("sccue"                           , Left 0x0227D)
             -- , ("Scedil"                          , Left 0x0015E)
             -- , ("scedil"                          , Left 0x0015F)
             -- , ("sce"                             , Left 0x02AB0)
             -- , ("scE"                             , Left 0x02AB4)
             -- , ("Scirc"                           , Left 0x0015C)
             -- , ("scirc"                           , Left 0x0015D)
             -- , ("sc"                              , Left 0x0227B)
             -- , ("Sc"                              , Left 0x02ABC)
             -- , ("scnap"                           , Left 0x02ABA)
             -- , ("scnE"                            , Left 0x02AB6)
             -- , ("scnsim"                          , Left 0x022E9)
             -- , ("scpolint"                        , Left 0x02A13)
             -- , ("scsim"                           , Left 0x0227F)
             -- , ("Scy"                             , Left 0x00421)
             -- , ("scy"                             , Left 0x00441)
             -- , ("sdotb"                           , Left 0x022A1)
             -- , ("sdote"                           , Left 0x02A66)
             -- , ("sdot"                            , Left 0x022C5)
             -- , ("searhk"                          , Left 0x02925)
             -- , ("searr"                           , Left 0x02198)
             -- , ("seArr"                           , Left 0x021D8)
             -- , ("searrow"                         , Left 0x02198)
             -- , ("sect"                            , Left 0x000A7)
             -- , ("semi"                            , Left 0x0003B)
             -- , ("seswar"                          , Left 0x02929)
             -- , ("setminus"                        , Left 0x02216)
             -- , ("setmn"                           , Left 0x02216)
             -- , ("sext"                            , Left 0x02736)
             -- , ("Sfr"                             , Left 0x1D516)
             -- , ("sfr"                             , Left 0x1D530)
             -- , ("sfrown"                          , Left 0x02322)
             -- , ("sharp"                           , Left 0x0266F)
             -- , ("SHCHcy"                          , Left 0x00429)
             -- , ("shchcy"                          , Left 0x00449)
             -- , ("SHcy"                            , Left 0x00428)
             -- , ("shcy"                            , Left 0x00448)
             -- , ("ShortDownArrow"                  , Left 0x02193)
             -- , ("ShortLeftArrow"                  , Left 0x02190)
             -- , ("shortmid"                        , Left 0x02223)
             -- , ("shortparallel"                   , Left 0x02225)
             -- , ("ShortRightArrow"                 , Left 0x02192)
             -- , ("ShortUpArrow"                    , Left 0x02191)
             -- , ("shy"                             , Left 0x000AD)
             -- , ("Sigma"                           , Left 0x003A3)
             -- , ("sigma"                           , Left 0x003C3)
             -- , ("sigmav"                          , Left 0x003C2)
             -- , ("simdot"                          , Left 0x02A6A)
             -- , ("sime"                            , Left 0x02243)
             -- , ("simeq"                           , Left 0x02243)
             -- , ("simgE"                           , Left 0x02AA0)
             -- , ("simg"                            , Left 0x02A9E)
             -- , ("sim"                             , Left 0x0223C)
             -- , ("simlE"                           , Left 0x02A9F)
             -- , ("siml"                            , Left 0x02A9D)
             -- , ("simne"                           , Left 0x02246)
             -- , ("simplus"                         , Left 0x02A24)
             -- , ("simrarr"                         , Left 0x02972)
             -- , ("slarr"                           , Left 0x02190)
             -- , ("SmallCircle"                     , Left 0x02218)
             -- , ("smallsetminus"                   , Left 0x02216)
             -- , ("smashp"                          , Left 0x02A33)
             -- , ("smeparsl"                        , Left 0x029E4)
             -- , ("smid"                            , Left 0x02223)
             -- , ("smile"                           , Left 0x02323)
             -- , ("smte"                            , Left 0x02AAC)
             -- , ("smt"                             , Left 0x02AAA)
             -- , ("SOFTcy"                          , Left 0x0042C)
             -- , ("softcy"                          , Left 0x0044C)
             -- , ("solbar"                          , Left 0x0233F)
             -- , ("solb"                            , Left 0x029C4)
             -- , ("sol"                             , Left 0x0002F)
             -- , ("Sopf"                            , Left 0x1D54A)
             -- , ("sopf"                            , Left 0x1D564)
             -- , ("spades"                          , Left 0x02660)
             -- , ("spadesuit"                       , Left 0x02660)
             -- , ("spar"                            , Left 0x02225)
             -- , ("sqcap"                           , Left 0x02293)
             -- , ("sqcup"                           , Left 0x02294)
             -- , ("Sqrt"                            , Left 0x0221A)
             -- , ("sqsube"                          , Left 0x02291)
             -- , ("sqsub"                           , Left 0x0228F)
             -- , ("sqsubseteq"                      , Left 0x02291)
             -- , ("sqsubset"                        , Left 0x0228F)
             -- , ("sqsupe"                          , Left 0x02292)
             -- , ("sqsup"                           , Left 0x02290)
             -- , ("sqsupseteq"                      , Left 0x02292)
             -- , ("sqsupset"                        , Left 0x02290)
             -- , ("SquareIntersection"              , Left 0x02293)
             -- , ("square"                          , Left 0x025A1)
             -- , ("Square"                          , Left 0x025A1)
             -- , ("SquareSubsetEqual"               , Left 0x02291)
             -- , ("SquareSubset"                    , Left 0x0228F)
             -- , ("SquareSupersetEqual"             , Left 0x02292)
             -- , ("SquareSuperset"                  , Left 0x02290)
             -- , ("SquareUnion"                     , Left 0x02294)
             -- , ("squarf"                          , Left 0x025AA)
             -- , ("squf"                            , Left 0x025AA)
             -- , ("squ"                             , Left 0x025A1)
             -- , ("srarr"                           , Left 0x02192)
             -- , ("Sscr"                            , Left 0x1D4AE)
             -- , ("sscr"                            , Left 0x1D4C8)
             -- , ("ssetmn"                          , Left 0x02216)
             -- , ("ssmile"                          , Left 0x02323)
             -- , ("sstarf"                          , Left 0x022C6)
             -- , ("starf"                           , Left 0x02605)
             -- , ("Star"                            , Left 0x022C6)
             -- , ("star"                            , Left 0x02606)
             -- , ("straightepsilon"                 , Left 0x003F5)
             -- , ("straightphi"                     , Left 0x003D5)
             -- , ("strns"                           , Left 0x000AF)
             -- , ("subdot"                          , Left 0x02ABD)
             -- , ("subedot"                         , Left 0x02AC3)
             -- , ("sube"                            , Left 0x02286)
             -- , ("subE"                            , Left 0x02AC5)
             -- , ("sub"                             , Left 0x02282)
             -- , ("Sub"                             , Left 0x022D0)
             -- , ("submult"                         , Left 0x02AC1)
             -- , ("subne"                           , Left 0x0228A)
             -- , ("subnE"                           , Left 0x02ACB)
             -- , ("subplus"                         , Left 0x02ABF)
             -- , ("subrarr"                         , Left 0x02979)
             -- , ("subseteq"                        , Left 0x02286)
             -- , ("subseteqq"                       , Left 0x02AC5)
             -- , ("SubsetEqual"                     , Left 0x02286)
             -- , ("subset"                          , Left 0x02282)
             -- , ("Subset"                          , Left 0x022D0)
             -- , ("subsetneq"                       , Left 0x0228A)
             -- , ("subsetneqq"                      , Left 0x02ACB)
             -- , ("subsim"                          , Left 0x02AC7)
             -- , ("subsub"                          , Left 0x02AD5)
             -- , ("subsup"                          , Left 0x02AD3)
             -- , ("succapprox"                      , Left 0x02AB8)
             -- , ("succcurlyeq"                     , Left 0x0227D)
             -- , ("SucceedsEqual"                   , Left 0x02AB0)
             -- , ("Succeeds"                        , Left 0x0227B)
             -- , ("SucceedsSlantEqual"              , Left 0x0227D)
             -- , ("SucceedsTilde"                   , Left 0x0227F)
             -- , ("succeq"                          , Left 0x02AB0)
             -- , ("succ"                            , Left 0x0227B)
             -- , ("succnapprox"                     , Left 0x02ABA)
             -- , ("succneqq"                        , Left 0x02AB6)
             -- , ("succnsim"                        , Left 0x022E9)
             -- , ("succsim"                         , Left 0x0227F)
             -- , ("SuchThat"                        , Left 0x0220B)
             -- , ("sum"                             , Left 0x02211)
             -- , ("Sum"                             , Left 0x02211)
             -- , ("sung"                            , Left 0x0266A)
             -- , ("sup1"                            , Left 0x000B9)
             -- , ("sup2"                            , Left 0x000B2)
             -- , ("sup3"                            , Left 0x000B3)
             -- , ("supdot"                          , Left 0x02ABE)
             -- , ("supdsub"                         , Left 0x02AD8)
             -- , ("supedot"                         , Left 0x02AC4)
             -- , ("supe"                            , Left 0x02287)
             -- , ("supE"                            , Left 0x02AC6)
             -- , ("SupersetEqual"                   , Left 0x02287)
             -- , ("Superset"                        , Left 0x02283)
             -- , ("suphsub"                         , Left 0x02AD7)
             -- , ("suplarr"                         , Left 0x0297B)
             -- , ("sup"                             , Left 0x02283)
             -- , ("Sup"                             , Left 0x022D1)
             -- , ("supmult"                         , Left 0x02AC2)
             -- , ("supne"                           , Left 0x0228B)
             -- , ("supnE"                           , Left 0x02ACC)
             -- , ("supplus"                         , Left 0x02AC0)
             -- , ("supseteq"                        , Left 0x02287)
             -- , ("supseteqq"                       , Left 0x02AC6)
             -- , ("supset"                          , Left 0x02283)
             -- , ("Supset"                          , Left 0x022D1)
             -- , ("supsetneq"                       , Left 0x0228B)
             -- , ("supsetneqq"                      , Left 0x02ACC)
             -- , ("supsim"                          , Left 0x02AC8)
             -- , ("supsub"                          , Left 0x02AD4)
             -- , ("supsup"                          , Left 0x02AD6)
             -- , ("swarhk"                          , Left 0x02926)
             -- , ("swarr"                           , Left 0x02199)
             -- , ("swArr"                           , Left 0x021D9)
             -- , ("swarrow"                         , Left 0x02199)
             -- , ("swnwar"                          , Left 0x0292A)
             -- , ("szlig"                           , Left 0x000DF)
             -- , ("Tab"                             , Left 0x00009)
             -- , ("target"                          , Left 0x02316)
             -- , ("tau"                             , Left 0x003C4)
             -- , ("tbrk"                            , Left 0x023B4)
             -- , ("Tcaron"                          , Left 0x00164)
             -- , ("tcaron"                          , Left 0x00165)
             -- , ("Tcedil"                          , Left 0x00162)
             -- , ("tcedil"                          , Left 0x00163)
             -- , ("Tcy"                             , Left 0x00422)
             -- , ("tcy"                             , Left 0x00442)
             -- , ("tdot"                            , Left 0x020DB)
             -- , ("telrec"                          , Left 0x02315)
             -- , ("Tfr"                             , Left 0x1D517)
             -- , ("tfr"                             , Left 0x1D531)
             -- , ("there4"                          , Left 0x02234)
             -- , ("therefore"                       , Left 0x02234)
             -- , ("Therefore"                       , Left 0x02234)
             -- , ("Theta"                           , Left 0x00398)
             -- , ("theta"                           , Left 0x003B8)
             -- , ("thetav"                          , Left 0x003D1)
             -- , ("thickapprox"                     , Left 0x02248)
             -- , ("thicksim"                        , Left 0x0223C)
             -- , ("ThinSpace"                       , Left 0x02009)
             -- , ("thinsp"                          , Left 0x02009)
             -- , ("thkap"                           , Left 0x02248)
             -- , ("thksim"                          , Left 0x0223C)
             -- , ("THORN"                           , Left 0x000DE)
             -- , ("thorn"                           , Left 0x000FE)
             -- , ("TildeEqual"                      , Left 0x02243)
             -- , ("TildeFullEqual"                  , Left 0x02245)
             -- , ("tilde"                           , Left 0x002DC)
             -- , ("Tilde"                           , Left 0x0223C)
             -- , ("TildeTilde"                      , Left 0x02248)
             -- , ("timesbar"                        , Left 0x02A31)
             -- , ("timesb"                          , Left 0x022A0)
             -- , ("timesd"                          , Left 0x02A30)
             -- , ("times"                           , Left 0x000D7)
             -- , ("tint"                            , Left 0x0222D)
             -- , ("toea"                            , Left 0x02928)
             -- , ("topbot"                          , Left 0x02336)
             -- , ("topcir"                          , Left 0x02AF1)
             -- , ("Topf"                            , Left 0x1D54B)
             -- , ("topf"                            , Left 0x1D565)
             -- , ("topfork"                         , Left 0x02ADA)
             -- , ("top"                             , Left 0x022A4)
             -- , ("tosa"                            , Left 0x02929)
             -- , ("tprime"                          , Left 0x02034)
             -- , ("trade"                           , Left 0x02122)
             -- , ("triangledown"                    , Left 0x025BF)
             -- , ("triangle"                        , Left 0x025B5)
             -- , ("trianglelefteq"                  , Left 0x022B4)
             -- , ("triangleleft"                    , Left 0x025C3)
             -- , ("triangleq"                       , Left 0x0225C)
             -- , ("trianglerighteq"                 , Left 0x022B5)
             -- , ("triangleright"                   , Left 0x025B9)
             -- , ("tridot"                          , Left 0x025EC)
             -- , ("trie"                            , Left 0x0225C)
             -- , ("triminus"                        , Left 0x02A3A)
             -- , ("TripleDot"                       , Left 0x020DB)
             -- , ("triplus"                         , Left 0x02A39)
             -- , ("trisb"                           , Left 0x029CD)
             -- , ("tritime"                         , Left 0x02A3B)
             -- , ("trpezium"                        , Left 0x0FFFD)
             -- , ("Tscr"                            , Left 0x1D4AF)
             -- , ("tscr"                            , Left 0x1D4C9)
             -- , ("TScy"                            , Left 0x00426)
             -- , ("tscy"                            , Left 0x00446)
             -- , ("TSHcy"                           , Left 0x0040B)
             -- , ("tshcy"                           , Left 0x0045B)
             -- , ("Tstrok"                          , Left 0x00166)
             -- , ("tstrok"                          , Left 0x00167)
             -- , ("twixt"                           , Left 0x0226C)
             -- , ("twoheadleftarrow"                , Left 0x0219E)
             -- , ("twoheadrightarrow"               , Left 0x021A0)
             -- , ("Uacute"                          , Left 0x000DA)
             -- , ("uacute"                          , Left 0x000FA)
             -- , ("uarr"                            , Left 0x02191)
             -- , ("Uarr"                            , Left 0x0219F)
             -- , ("uArr"                            , Left 0x021D1)
             -- , ("Uarrocir"                        , Left 0x02949)
             -- , ("Ubrcy"                           , Left 0x0040E)
             -- , ("ubrcy"                           , Left 0x0045E)
             -- , ("Ubreve"                          , Left 0x0016C)
             -- , ("ubreve"                          , Left 0x0016D)
             -- , ("Ucirc"                           , Left 0x000DB)
             -- , ("ucirc"                           , Left 0x000FB)
             -- , ("Ucy"                             , Left 0x00423)
             -- , ("ucy"                             , Left 0x00443)
             -- , ("udarr"                           , Left 0x021C5)
             -- , ("Udblac"                          , Left 0x00170)
             -- , ("udblac"                          , Left 0x00171)
             -- , ("udhar"                           , Left 0x0296E)
             -- , ("ufisht"                          , Left 0x0297E)
             -- , ("Ufr"                             , Left 0x1D518)
             -- , ("ufr"                             , Left 0x1D532)
             -- , ("Ugrave"                          , Left 0x000D9)
             -- , ("ugrave"                          , Left 0x000F9)
             -- , ("uHar"                            , Left 0x02963)
             -- , ("uharl"                           , Left 0x021BF)
             -- , ("uharr"                           , Left 0x021BE)
             -- , ("uhblk"                           , Left 0x02580)
             -- , ("ulcorner"                        , Left 0x0231C)
             -- , ("ulcorn"                          , Left 0x0231C)
             -- , ("ulcrop"                          , Left 0x0230F)
             -- , ("ultri"                           , Left 0x025F8)
             -- , ("Umacr"                           , Left 0x0016A)
             -- , ("umacr"                           , Left 0x0016B)
             -- , ("uml"                             , Left 0x000A8)
             -- , ("UnderBar"                        , Left 0x00332)
             -- , ("UnderBrace"                      , Left 0x0FE38)
             -- , ("UnderBracket"                    , Left 0x023B5)
             -- , ("UnderParenthesis"                , Left 0x0FE36)
             -- , ("Union"                           , Left 0x022C3)
             -- , ("UnionPlus"                       , Left 0x0228E)
             -- , ("Uogon"                           , Left 0x00172)
             -- , ("uogon"                           , Left 0x00173)
             -- , ("Uopf"                            , Left 0x1D54C)
             -- , ("uopf"                            , Left 0x1D566)
             -- , ("UpArrowBar"                      , Left 0x02912)
             -- , ("UpArrowDownArrow"                , Left 0x021C5)
             -- , ("uparrow"                         , Left 0x02191)
             -- , ("UpArrow"                         , Left 0x02191)
             -- , ("Uparrow"                         , Left 0x021D1)
             -- , ("updownarrow"                     , Left 0x02195)
             -- , ("UpDownArrow"                     , Left 0x02195)
             -- , ("Updownarrow"                     , Left 0x021D5)
             -- , ("UpEquilibrium"                   , Left 0x0296E)
             -- , ("upharpoonleft"                   , Left 0x021BF)
             -- , ("upharpoonright"                  , Left 0x021BE)
             -- , ("uplus"                           , Left 0x0228E)
             -- , ("UpperLeftArrow"                  , Left 0x02196)
             -- , ("UpperRightArrow"                 , Left 0x02197)
             -- , ("upsi"                            , Left 0x003C5)
             -- , ("Upsi"                            , Left 0x003D2)
             -- , ("Upsilon"                         , Left 0x003A5)
             -- , ("upsilon"                         , Left 0x003C5)
             -- , ("UpTeeArrow"                      , Left 0x021A5)
             -- , ("UpTee"                           , Left 0x022A5)
             -- , ("upuparrows"                      , Left 0x021C8)
             -- , ("urcorner"                        , Left 0x0231D)
             -- , ("urcorn"                          , Left 0x0231D)
             -- , ("urcrop"                          , Left 0x0230E)
             -- , ("Uring"                           , Left 0x0016E)
             -- , ("uring"                           , Left 0x0016F)
             -- , ("urtri"                           , Left 0x025F9)
             -- , ("Uscr"                            , Left 0x1D4B0)
             -- , ("uscr"                            , Left 0x1D4CA)
             -- , ("utdot"                           , Left 0x022F0)
             -- , ("Utilde"                          , Left 0x00168)
             -- , ("utilde"                          , Left 0x00169)
             -- , ("utrif"                           , Left 0x025B4)
             -- , ("utri"                            , Left 0x025B5)
             -- , ("uuarr"                           , Left 0x021C8)
             -- , ("Uuml"                            , Left 0x000DC)
             -- , ("uuml"                            , Left 0x000FC)
             -- , ("uwangle"                         , Left 0x029A7)
             -- , ("vangrt"                          , Left 0x0299C)
             -- , ("varepsilon"                      , Left 0x003B5)
             -- , ("varkappa"                        , Left 0x003F0)
             -- , ("varnothing"                      , Left 0x02205)
             -- , ("varphi"                          , Left 0x003C6)
             -- , ("varpi"                           , Left 0x003D6)
             -- , ("varpropto"                       , Left 0x0221D)
             -- , ("varrho"                          , Left 0x003F1)
             -- , ("varr"                            , Left 0x02195)
             -- , ("vArr"                            , Left 0x021D5)
             -- , ("varsigma"                        , Left 0x003C2)
             -- , ("vartheta"                        , Left 0x003D1)
             -- , ("vartriangleleft"                 , Left 0x022B2)
             -- , ("vartriangleright"                , Left 0x022B3)
             -- , ("vBar"                            , Left 0x02AE8)
             -- , ("Vbar"                            , Left 0x02AEB)
             -- , ("vBarv"                           , Left 0x02AE9)
             -- , ("Vcy"                             , Left 0x00412)
             -- , ("vcy"                             , Left 0x00432)
             -- , ("vdash"                           , Left 0x022A2)
             -- , ("vDash"                           , Left 0x022A8)
             -- , ("Vdash"                           , Left 0x022A9)
             -- , ("VDash"                           , Left 0x022AB)
             -- , ("Vdashl"                          , Left 0x02AE6)
             -- , ("veebar"                          , Left 0x022BB)
             -- , ("veeeq"                           , Left 0x0225A)
             -- , ("vee"                             , Left 0x02228)
             -- , ("Vee"                             , Left 0x022C1)
             -- , ("vellip"                          , Left 0x022EE)
             -- , ("verbar"                          , Left 0x0007C)
             -- , ("Verbar"                          , Left 0x02016)
             -- , ("VerticalBar"                     , Left 0x02223)
             -- , ("VerticalLine"                    , Left 0x0007C)
             -- , ("VerticalSeparator"               , Left 0x02758)
             -- , ("VerticalTilde"                   , Left 0x02240)
             -- , ("vert"                            , Left 0x0007C)
             -- , ("Vert"                            , Left 0x02016)
             -- , ("VeryThinSpace"                   , Left 0x0200A)
             -- , ("Vfr"                             , Left 0x1D519)
             -- , ("vfr"                             , Left 0x1D533)
             -- , ("vltri"                           , Left 0x022B2)
             -- , ("Vopf"                            , Left 0x1D54D)
             -- , ("vopf"                            , Left 0x1D567)
             -- , ("vprop"                           , Left 0x0221D)
             -- , ("vrtri"                           , Left 0x022B3)
             -- , ("Vscr"                            , Left 0x1D4B1)
             -- , ("vscr"                            , Left 0x1D4CB)
             -- , ("Vvdash"                          , Left 0x022AA)
             -- , ("vzigzag"                         , Left 0x0299A)
             -- , ("Wcirc"                           , Left 0x00174)
             -- , ("wcirc"                           , Left 0x00175)
             -- , ("wedbar"                          , Left 0x02A5F)
             -- , ("wedge"                           , Left 0x02227)
             -- , ("Wedge"                           , Left 0x022C0)
             -- , ("wedgeq"                          , Left 0x02259)
             -- , ("weierp"                          , Left 0x02118)
             -- , ("Wfr"                             , Left 0x1D51A)
             -- , ("wfr"                             , Left 0x1D534)
             -- , ("Wopf"                            , Left 0x1D54E)
             -- , ("wopf"                            , Left 0x1D568)
             -- , ("wp"                              , Left 0x02118)
             -- , ("wreath"                          , Left 0x02240)
             -- , ("wr"                              , Left 0x02240)
             -- , ("Wscr"                            , Left 0x1D4B2)
             -- , ("wscr"                            , Left 0x1D4CC)
             -- , ("xcap"                            , Left 0x022C2)
             -- , ("xcirc"                           , Left 0x025EF)
             -- , ("xcup"                            , Left 0x022C3)
             -- , ("xdtri"                           , Left 0x025BD)
             -- , ("Xfr"                             , Left 0x1D51B)
             -- , ("xfr"                             , Left 0x1D535)
             -- , ("xharr"                           , Left 0x027F7)
             -- , ("xhArr"                           , Left 0x027FA)
             -- , ("Xi"                              , Left 0x0039E)
             -- , ("xi"                              , Left 0x003BE)
             -- , ("xlarr"                           , Left 0x027F5)
             -- , ("xlArr"                           , Left 0x027F8)
             -- , ("xmap"                            , Left 0x027FC)
             -- , ("xnis"                            , Left 0x022FB)
             -- , ("xodot"                           , Left 0x02A00)
             -- , ("Xopf"                            , Left 0x1D54F)
             -- , ("xopf"                            , Left 0x1D569)
             -- , ("xoplus"                          , Left 0x02A01)
             -- , ("xotime"                          , Left 0x02A02)
             -- , ("xrarr"                           , Left 0x027F6)
             -- , ("xrArr"                           , Left 0x027F9)
             -- , ("Xscr"                            , Left 0x1D4B3)
             -- , ("xscr"                            , Left 0x1D4CD)
             -- , ("xsqcup"                          , Left 0x02A06)
             -- , ("xuplus"                          , Left 0x02A04)
             -- , ("xutri"                           , Left 0x025B3)
             -- , ("xvee"                            , Left 0x022C1)
             -- , ("xwedge"                          , Left 0x022C0)
             -- , ("Yacute"                          , Left 0x000DD)
             -- , ("yacute"                          , Left 0x000FD)
             -- , ("YAcy"                            , Left 0x0042F)
             -- , ("yacy"                            , Left 0x0044F)
             -- , ("Ycirc"                           , Left 0x00176)
             -- , ("ycirc"                           , Left 0x00177)
             -- , ("Ycy"                             , Left 0x0042B)
             -- , ("ycy"                             , Left 0x0044B)
             -- , ("yen"                             , Left 0x000A5)
             -- , ("Yfr"                             , Left 0x1D51C)
             -- , ("yfr"                             , Left 0x1D536)
             -- , ("YIcy"                            , Left 0x00407)
             -- , ("yicy"                            , Left 0x00457)
             -- , ("Yopf"                            , Left 0x1D550)
             -- , ("yopf"                            , Left 0x1D56A)
             -- , ("Yscr"                            , Left 0x1D4B4)
             -- , ("yscr"                            , Left 0x1D4CE)
             -- , ("YUcy"                            , Left 0x0042E)
             -- , ("yucy"                            , Left 0x0044E)
             -- , ("yuml"                            , Left 0x000FF)
             -- , ("Yuml"                            , Left 0x00178)
             -- , ("Zacute"                          , Left 0x00179)
             -- , ("zacute"                          , Left 0x0017A)
             -- , ("Zcaron"                          , Left 0x0017D)
             -- , ("zcaron"                          , Left 0x0017E)
             -- , ("Zcy"                             , Left 0x00417)
             -- , ("zcy"                             , Left 0x00437)
             -- , ("Zdot"                            , Left 0x0017B)
             -- , ("zdot"                            , Left 0x0017C)
             -- , ("zeetrf"                          , Left 0x02128)
             -- , ("ZeroWidthSpace"                  , Left 0x0200B)
             -- , ("zeta"                            , Left 0x003B6)
             -- , ("Zfr"                             , Left 0x02128)
             -- , ("zfr"                             , Left 0x1D537)
             -- , ("ZHcy"                            , Left 0x00416)
             -- , ("zhcy"                            , Left 0x00436)
             -- , ("zigrarr"                         , Left 0x021DD)
             -- , ("Zopf"                            , Left 0x02124)
             -- , ("zopf"                            , Left 0x1D56B)
             -- , ("Zscr"                            , Left 0x1D4B5)
             -- , ("zscr"                            , Left 0x1D4CF)
             -- , ("acE"                             , Right [ 0x0223E, 0x00333 ])
             -- , ("bnequiv"                         , Right [ 0x02261, 0x020E5 ])
             -- , ("bne"                             , Right [ 0x0003D, 0x020E5 ])
             -- , ("bsolhsub"                        , Right [ 0x0005C, 0x02282 ])
             -- , ("caps"                            , Right [ 0x02229, 0x0FE00 ])
             -- , ("cups"                            , Right [ 0x0222A, 0x0FE00 ])
             -- , ("gesl"                            , Right [ 0x022DB, 0x0FE00 ])
             -- , ("gvertneqq"                       , Right [ 0x02269, 0x0FE00 ])
             -- , ("gvnE"                            , Right [ 0x02269, 0x0FE00 ])
             -- , ("lates"                           , Right [ 0x02AAD, 0x0FE00 ])
             -- , ("lesg"                            , Right [ 0x022DA, 0x0FE00 ])
             -- , ("lvertneqq"                       , Right [ 0x02268, 0x0FE00 ])
             -- , ("lvnE"                            , Right [ 0x02268, 0x0FE00 ])
             -- , ("nang"                            , Right [ 0x02220, 0x020D2 ])
             -- , ("napE"                            , Right [ 0x02A70, 0x00338 ])
             -- , ("napid"                           , Right [ 0x0224B, 0x00338 ])
             -- , ("nbumpe"                          , Right [ 0x0224F, 0x00338 ])
             -- , ("nbump"                           , Right [ 0x0224E, 0x00338 ])
             -- , ("ncongdot"                        , Right [ 0x02A6D, 0x00338 ])
             -- , ("nedot"                           , Right [ 0x02250, 0x00338 ])
             -- , ("nesim"                           , Right [ 0x02242, 0x00338 ])
             -- , ("ngeqq"                           , Right [ 0x02267, 0x00338 ])
             -- , ("ngeqslant"                       , Right [ 0x02A7E, 0x00338 ])
             -- , ("ngE"                             , Right [ 0x02267, 0x00338 ])
             -- , ("nges"                            , Right [ 0x02A7E, 0x00338 ])
             -- , ("nGg"                             , Right [ 0x022D9, 0x00338 ])
             -- , ("nGt"                             , Right [ 0x0226B, 0x020D2 ])
             -- , ("nGtv"                            , Right [ 0x0226B, 0x00338 ])
             -- , ("nleqq"                           , Right [ 0x02266, 0x00338 ])
             -- , ("nleqslant"                       , Right [ 0x02A7D, 0x00338 ])
             -- , ("nlE"                             , Right [ 0x02266, 0x00338 ])
             -- , ("nles"                            , Right [ 0x02A7D, 0x00338 ])
             -- , ("nLl"                             , Right [ 0x022D8, 0x00338 ])
             -- , ("nLt"                             , Right [ 0x0226A, 0x020D2 ])
             -- , ("nLtv"                            , Right [ 0x0226A, 0x00338 ])
             -- , ("NotEqualTilde"                   , Right [ 0x02242, 0x00338 ])
             -- , ("NotGreaterFullEqual"             , Right [ 0x02266, 0x00338 ])
             -- , ("NotGreaterGreater"               , Right [ 0x0226B, 0x00338 ])
             -- , ("NotGreaterSlantEqual"            , Right [ 0x02A7E, 0x00338 ])
             -- , ("NotHumpDownHump"                 , Right [ 0x0224E, 0x00338 ])
             -- , ("NotHumpEqual"                    , Right [ 0x0224F, 0x00338 ])
             -- , ("notindot"                        , Right [ 0x022F5, 0x00338 ])
             -- , ("notinE"                          , Right [ 0x022F9, 0x00338 ])
             -- , ("NotLeftTriangleBar"              , Right [ 0x029CF, 0x00338 ])
             -- , ("NotLessLess"                     , Right [ 0x0226A, 0x00338 ])
             -- , ("NotLessSlantEqual"               , Right [ 0x02A7D, 0x00338 ])
             -- , ("NotNestedGreaterGreater"         , Right [ 0x02AA2, 0x00338 ])
             -- , ("NotNestedLessLess"               , Right [ 0x02AA1, 0x00338 ])
             -- , ("NotPrecedesEqual"                , Right [ 0x02AAF, 0x00338 ])
             -- , ("NotRightTriangleBar"             , Right [ 0x029D0, 0x00338 ])
             -- , ("NotSquareSubset"                 , Right [ 0x0228F, 0x00338 ])
             -- , ("NotSquareSuperset"               , Right [ 0x02290, 0x00338 ])
             -- , ("NotSubset"                       , Right [ 0x02282, 0x020D2 ])
             -- , ("NotSucceedsEqual"                , Right [ 0x02AB0, 0x00338 ])
             -- , ("NotSucceedsTilde"                , Right [ 0x0227F, 0x00338 ])
             -- , ("NotSuperset"                     , Right [ 0x02283, 0x020D2 ])
             -- , ("nparsl"                          , Right [ 0x02AFD, 0x020E5 ])
             -- , ("npart"                           , Right [ 0x02202, 0x00338 ])
             -- , ("npreceq"                         , Right [ 0x02AAF, 0x00338 ])
             -- , ("npre"                            , Right [ 0x02AAF, 0x00338 ])
             -- , ("nrarrc"                          , Right [ 0x02933, 0x00338 ])
             -- , ("nrarrw"                          , Right [ 0x0219D, 0x00338 ])
             -- , ("nsce"                            , Right [ 0x02AB0, 0x00338 ])
             -- , ("nsubE"                           , Right [ 0x02AC5, 0x00338 ])
             -- , ("nsubseteqq"                      , Right [ 0x02AC5, 0x00338 ])
             -- , ("nsubset"                         , Right [ 0x02282, 0x020D2 ])
             -- , ("nsucceq"                         , Right [ 0x02AB0, 0x00338 ])
             -- , ("nsupE"                           , Right [ 0x02AC6, 0x00338 ])
             -- , ("nsupseteqq"                      , Right [ 0x02AC6, 0x00338 ])
             -- , ("nsupset"                         , Right [ 0x02283, 0x020D2 ])
             -- , ("nvap"                            , Right [ 0x0224D, 0x020D2 ])
             -- , ("nvge"                            , Right [ 0x02265, 0x020D2 ])
             -- , ("nvgt"                            , Right [ 0x0003E, 0x020D2 ])
             -- , ("nvle"                            , Right [ 0x02264, 0x020D2 ])
             -- , ("nvltrie"                         , Right [ 0x022B4, 0x020D2 ])
             -- , ("nvlt"                            , Right [ 0x0003C, 0x020D2 ])
             -- , ("nvrtrie"                         , Right [ 0x022B5, 0x020D2 ])
             -- , ("nvsim"                           , Right [ 0x0223C, 0x020D2 ])
             -- , ("smtes"                           , Right [ 0x02AAC, 0x0FE00 ])
             -- , ("sqcaps"                          , Right [ 0x02293, 0x0FE00 ])
             -- , ("sqcups"                          , Right [ 0x02294, 0x0FE00 ])
             -- , ("suphsol"                         , Right [ 0x02283, 0x0002F ])
             -- , ("ThickSpace"                      , Right [ 0x02009, 0x0200A, 0x0200A ])
             -- , ("varsubsetneqq"                   , Right [ 0x02ACB, 0x0FE00 ])
             -- , ("varsubsetneq"                    , Right [ 0x0228A, 0x0FE00 ])
             -- , ("varsupsetneqq"                   , Right [ 0x02ACC, 0x0FE00 ])
             -- , ("varsupsetneq"                    , Right [ 0x0228B, 0x0FE00 ])
             -- , ("vnsub"                           , Right [ 0x02282, 0x020D2 ])
             -- , ("vnsup"                           , Right [ 0x02283, 0x020D2 ])
             -- , ("vsubne"                          , Right [ 0x0228A, 0x0FE00 ])
             -- , ("vsubnE"                          , Right [ 0x02ACB, 0x0FE00 ])
             -- , ("vsupne"                          , Right [ 0x0228B, 0x0FE00 ])
             -- , ("vsupnE"                          , Right [ 0x02ACC, 0x0FE00 ])
             -- ]

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
