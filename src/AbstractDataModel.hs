{-# Language ExistentialQuantification, QuasiQuotes, TemplateHaskell #-}
{-| Time-stamp: <2019-11-04 14:39:24 CST>

Module      : AbstractDataModel
Copyright   : Robert Lee, © 2017-2019
License     : ISC

Maintainer  : robert.lee@chicago.vc
Stability   : None
Portability : non-portable (GHC extensions)

Contumacy   : Best viewed with unbroken/unwrapped 154 column display.

Description : Reify the abstract data model for XML Schema 1.1.

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

module AbstractDataModel where

-- Local Imports

-- import Builtin

-- Explicit Imports


-- Qualified Imports

import qualified Data.Set as S

-- Undisciplined Imports

import Text.Pretty.Simple

import Text.XML
import ClassyPrelude hiding (readFile)

-- End of Imports
-- -----------------------------------------------------------------------------------------------------------------------------------------------------
{-
  The name [Definition:]  Component covers all the different kinds of schema component defined in this specification.

  During ·validation·, [Definition:]  declaration components are associated by (qualified) name to information items being ·validated·.

  On the other hand, [Definition:]  definition components define internal schema components that can be used in other schema components.
-}

{-

data XSDSchema = SchemaComponents
  { simpleTypeDefinitions          :: S.Set SimpleTypeDefinition
  , complexTypeDefinitions         :: S.Set ComplexTypeDefinition
  , attributeDeclarations          :: S.Set AttributeDeclaration
  , elementDeclarations            :: S.Set ElementDeclaration
  , attributeGroupDefinitions      :: S.Set AttributeGroupDefinition
  , identityConstraintDefinitions  :: S.Set IdentityConstraintDefinition
  , typeAlternatives               :: S.Set TypeAlternatives
  , assertions                     :: S.Set Assertion
  , modelGroupDefinitions          :: S.Set ModelGroupDefinition
  , notationDeclarations           :: S.Set NotationDeclaration
  , annotations                    :: S.Set Annotation
  , modelGroups                    :: S.Set ModelGroup
  , particles                      :: S.Set Particle
  , wildcards                      :: S.Set Wildcard
  , attributeUses                  :: S.Set AttributeUse
  }

-}


processing :: IO ()
processing = do
  docx <- readFile def "/home/robert/Projects/XSD1.1/supporting_cast/shiporder.xsd"
  pPrint docx
  putStr "\n"


{-

2.2 XSD Abstract Data Model
  2.2.1 Type Definition Components
      2.2.1.1 Type Definition Hierarchy
      2.2.1.2 Simple Type Definition
      2.2.1.3 Complex Type Definition
  2.2.2 Declaration Components
      2.2.2.1 Element Declaration
      2.2.2.2 Element Substitution Group
      2.2.2.3 Attribute Declaration
      2.2.2.4 Notation Declaration
  2.2.3 Model Group Components
      2.2.3.1 Model Group
      2.2.3.2 Particle
      2.2.3.3 Attribute Use
      2.2.3.4 Wildcard
  2.2.4 Constraint Components
      2.2.4.1 Identity-constraint Definition
      2.2.4.2 Type Alternative
      2.2.4.3 Assertion
      2.2.4.4 Overlapping Functionality of Constraint Components
  2.2.5 Group Definition Components
      2.2.5.1 Model Group Definition
      2.2.5.2 Attribute Group Definition
  2.2.6 Annotation Components

-}





