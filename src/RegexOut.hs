{-# Language ExistentialQuantification, MultiParamTypeClasses
  , FlexibleInstances, GeneralizedNewtypeDeriving, NegativeLiterals, MultiWayIf #-}
{-| Time-stamp: <2018-07-03 21:30:09 CDT>

Module      : RegexOut
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

module RegexOut
where


-- Local Imports

import Lading
import Regex 

-- Explicit Imports

-- Qualified Imports

import qualified Data.Text as T

-- Undisciplined Imports

import ClassyPrelude

-- End of Imports
-- -----------------------------------------------------------------------------------------------------------------------------------------------------

-- Objectives
-- 2. Create XSD regex from AST     : Take Aeson Parser AST and produce XML Schema 1.1 regex string.

class Transformatio a where  -- Transformatio ➙ transform
  scribe :: a -> Text        -- scribe        ➙ write text

instance Transformatio RE
  where scribe (RE branches) = T.intercalate "|" $ map scribe branches
                
instance Transformatio Branch
  where scribe (Branch pieces) = T.concat $ map scribe pieces
                
instance Transformatio Piece
  where scribe (Piece atm mquant) = T.append (scribe atm) $ maybe T.empty scribe mquant
                
instance Transformatio Atom
  where scribe (AtomNormal    atm) = scribe atm
        scribe (AtomCharClass atm) = scribe atm
        scribe (AtomRE        atm) = T.concat ["(", scribe atm, ")"]
                
instance Transformatio Quantifier
  where scribe QuantifierMaybeOne         = T.singleton '?'
        scribe QuantifierMaybeMany        = T.singleton '*'
        scribe QuantifierMany             = T.singleton '+'
        scribe (QuintifierQuantity quant) = T.concat ["{", scribe quant, "}"]
                
instance Transformatio Quantity
  where scribe (QuantRange  qel qer) = T.concat [scribe qel, ",", scribe qer]
        scribe (QuantMin    qe) = T.append (scribe qe) ","
        scribe (QuantExactQ qe) = scribe qe
                
instance Transformatio QuantExact
  where scribe (QuantExact i) = tShow i

instance Transformatio NormalChar
  where scribe (NormalChar c) = T.singleton c
                
instance Transformatio CharClass
  where scribe (CharClassSingle cc) = scribe cc
        scribe (CharClassEscC   cc) = scribe cc
        scribe (CharClassExprC  cc) = scribe cc
        scribe (CharClassWild   cc) = scribe cc

instance Transformatio CharClassExpr
  where scribe (CharClassExpr cg) = T.concat ["[", scribe cg, "]"]

instance Transformatio CharGroup
  where scribe (CharGroup (Left cg)  mce) = T.append (scribe cg) $ maybe T.empty (T.cons '-' . scribe) mce
        scribe (CharGroup (Right cg) mce) = T.append (scribe cg) $ maybe T.empty (T.cons '-' . scribe) mce

instance Transformatio NegCharGroup
  where scribe (NegCharGroup pcg) = T.cons '^' $ scribe pcg

instance Transformatio PosCharGroup
  where scribe (PosCharGroup cgp) = T.concat $ map scribe cgp

instance Transformatio CharGroupPart
  where scribe (CharGroupPartSingle   cgp) = scribe cgp
        scribe (CharGroupPartRange    cgp) = scribe cgp
        scribe (CharGroupPartClassEsc cgp) = scribe cgp

instance Transformatio CharClassEsc
  where scribe (CharClassEscMultiCharEsc cesc) = scribe cesc
        scribe (CharClassEscCatEsc       cesc) = scribe cesc
        scribe (CharClassEscComplEsc     cesc) = scribe cesc

instance Transformatio CharRange
  where scribe (CharRange l r) = T.concat [scribe l, "-", scribe r]

instance Transformatio SingleChar
  where scribe (SingleChar (Left se)) = scribe se
        scribe (SingleChar (Right sne)) = scribe sne

instance Transformatio SingleCharNoEsc
  where scribe (SingleCharNoEsc c) = T.singleton c

instance Transformatio SingleCharEsc
  where scribe (SingleCharEsc c) = T.cons '\\' $ T.singleton c

instance Transformatio CatEsc
  where scribe (CatEsc cp) = T.concat ["\\p{", scribe cp, "}"]

instance Transformatio ComplEsc
  where scribe (ComplEsc cp) = T.concat ["\\P{", scribe cp, "}"]

instance Transformatio CharProp
  where scribe (CharProp (Left i)) = scribe i
        scribe (CharProp (Right b)) = scribe b

instance Transformatio IsCategory
  where scribe (LettersCat     cat) = scribe  cat
        scribe (MarksCat       cat) = scribe  cat
        scribe (NumbersCat     cat) = scribe  cat
        scribe (PunctuationCat cat) = scribe  cat
        scribe (SeparatorsCat  cat) = scribe  cat
        scribe (SymbolsCat     cat) = scribe  cat
        scribe (OthersCat      cat) = scribe  cat

instance Transformatio Letters
  where scribe = tShow

instance Transformatio Marks
  where scribe = tShow

instance Transformatio Numbers
  where scribe = tShow

instance Transformatio Punctuation
  where scribe = tShow

instance Transformatio Separators
  where scribe = tShow

instance Transformatio Symbols
  where scribe = tShow

instance Transformatio Others
  where scribe = tShow

instance Transformatio UnicodeBlockName
  where scribe = tShow

instance Transformatio IsBlock
  where scribe (IsBlock u) = scribe u

instance Transformatio MultiCharEsc
  where scribe (MultiCharEsc c) = T.cons '\\' $ T.singleton c

instance Transformatio WildcardEsc
  where scribe WildcardEsc = "."

