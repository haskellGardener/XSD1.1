{-# Language ExistentialQuantification, QuasiQuotes, TemplateHaskell #-}
{-| Time-stamp: <2019-10-25 13:04:45 CDT>

Module      : ComplexTypes
Copyright   : Robert Lee, © 2017-2019
License     : ISC

Maintainer  : robert.lee@chicago.vc
Stability   : None
Portability : non-portable (GHC extensions)

Contumacy   : Best viewed with unbroken/unwrapped 154 column display.

Description : Complex type functions for XML Schema 1.1.

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

module ComplexTypes where

-- Local Imports

-- import Builtin

-- Explicit Imports

-- import Data.Char  ( chr,ord,isDigit                )
-- import Data.Int   ( Int16, Int8                    )
-- import Data.Ix    ( inRange, range                 )
-- import Data.List  ( head, length, maximum, minimum )
-- import Data.Maybe ( fromJust                       )
-- import Data.Word  ( Word16, Word8                  )

import qualified Data.Map as M

-- Qualified Imports

-- import qualified Data.List                 as DL
-- import qualified Data.Text                 as T
-- import qualified Data.Text.Encoding        as TE
-- import qualified Data.Ix                   as Ix

import qualified Data.Set as S

-- Undisciplined Imports

import Text.Hamlet.XML
import Text.XML

import ClassyPrelude

-- End of Imports
-- -----------------------------------------------------------------------------------------------------------------------------------------------------

-- test :: [] Node
-- test = [xml| <foo:with-prefix xmlns:foo="second-namespace" foo:second-attr="value2"/> |]

data AnnotationCompontent      = AnnotationCompontent      -- Placeholder, may become a typeclass.
data AssertionComponent        = AssertionComponent        -- Placeholder, may become a typeclass.
data AttributeUseCompontent    = AttributeUseCompontent    -- Placeholder, may become a typeclass.
data ComplexTypeDeclaration    = ComplexTypeDeclaration    -- Placeholder, may become a typeclass.
data ContentTypePropertyRecord = ContentTypePropertyRecord -- Placeholder, may become a typeclass.
data ElementDeclaration        = ElementDeclaration        -- Placeholder, may become a typeclass.
data Extension                 = Extension                 -- Placeholder, may become a typeclass.
data Restriction               = Restriction               -- Placeholder, may become a typeclass.
data TypeDefinitionComponent   = TypeDefinitionComponent   -- Placeholder, may become a typeclass.
data WildcardCompontent        = WildcardCompontent        -- Placeholder, may become a typeclass.

data ComplexTypeDefinition = ComplexTypeDefinition
  { annotations             :: [AnnotationCompontent]
  , name                    :: Maybe NCName
  , targetNamespace         :: Maybe AnyURI
  , baseTypeDefinition      :: TypeDefinitionComponent
  , final                   :: S.Set (Either Extension Restriction)                     -- Subset
  , context                 :: Maybe (Either ElementDeclaration ComplexTypeDeclaration) -- See rule below.
  , derivationMethod        :: Either Extension Restriction
  , abstract                :: Bool
  , attributeUses           :: S.Set AttributeUseCompontent
  , attributeWildcard       :: Maybe WildcardCompontent
  , contentType             :: ContentTypePropertyRecord
  , prohibitedSubstitutions :: S.Set (Either Extension Restriction)                     -- Subset
  , assertions              :: [AssertionComponent]
  }

data Variety = VarietyEmpty        -- Placeholder
             | VarietySimple       -- Placeholder
             | VarietyElementOnly  -- Placeholder
             | VarietyMixed        -- Placeholder

data Particle = Particle -- Placeholder, may become a typeclass.

data OpenContentPropertyRecord      = OpenContentPropertyRecord       -- Placeholder
data SimpleTypeDefinitionCompontent = SimpleTypeDefinitionCompontent  -- Placeholder


data ContentTypePropertyRecord = ContentTypePropertyRecord
  { variety              :: Variety
  , particle             :: Maybe Particle
  , openContent          :: Maybe OpenContentPropertyRecord
  , simpleTypeDefinition :: Maybe SimpleTypeDefinitionCompontent
  }


{-
See:
https://www.w3.org/TR/xmlschema11-1/#Complex_Type_Definitions

3.4.1 The Complex Type Definition Schema Component

A complex type definition schema component has the following properties:

    Schema Component: Complex Type Definition, a kind of Type Definition
    --------------------------------------------------------------------------------------
    {annotations}
        A sequence of Annotation components.
    {name}
        An xs:NCName value. Optional.
    {target namespace}
        An xs:anyURI value. Optional.
    {base type definition}
        A type definition component. Required.
    {final}
        A subset of {extension, restriction}.
    {context}
        Required if {name} is ·absent·, otherwise must be ·absent·.
        Either an Element Declaration or a Complex Type Definition.
    {derivation method}
        One of {extension, restriction}. Required.
    {abstract}
        An xs:boolean value. Required.
    {attribute uses}
        A set of Attribute Use components.
    {attribute wildcard}
        A Wildcard component. Optional.
    {content type}
        A Content Type property record. Required.
    {prohibited substitutions}
        A subset of {extension, restriction}.
    {assertions}
        A sequence of Assertion components.

    --------------------------------------------------------------------------------------
    Property Record: Content Type
    {variety}
        One of {empty, simple, element-only, mixed}. Required.
    {particle}
        A Particle component. Required if {variety} is element-only or mixed, otherwise must be ·absent·.
    {open content}
        An Open Content property record. Optional if {variety} is element-only or mixed, otherwise must be ·absent·.
    {simple type definition}
        A Simple Type Definition component. Required if {variety} is simple, otherwise must be ·absent·.

    --------------------------------------------------------------------------------------
    Property Record: Open Content
    {mode}
        One of {interleave, suffix}. Required.
    {wildcard}
        A Wildcard component. Required.

-}



{-
3.4.2 XML Representation of Complex Type Definition Schema Components

The XML representation for a complex type definition schema component is a <complexType>
element information item.

The XML representation for complex type definitions with a {content type} with {variety}
simple is significantly different from that of those with other {content type}s, and this
is reflected in the presentation below, which describes the mappings for the two cases in
separate subsections. Common mapping rules are factored out and given in separate
sections. As always, the mapping rules given here apply after, not before, the appropriate
·pre-processing·.

--------------------------------------------------------------------------------------
XML Representation Summary: complexType Element Information Item
<complexType
  abstract = boolean : false
  block = (#all | List of (extension | restriction))
  final = (#all | List of (extension | restriction))
  id = ID
  mixed = boolean
  name = NCName
  defaultAttributesApply = boolean : true
  {any attributes with non-schema namespace . . .}
  >

  Content:
  (annotation?, (simpleContent | complexContent
                               | (openContent?, (group | all | choice | sequence)?, ((attribute | attributeGroup)*, anyAttribute?), assert*)
                )
  )

</complexType>
--------------------------------------------------------------------------------------

Note: It is a consequence of the concrete syntax given above that a top-level type
definition need consist of no more than a name, i.e. that <complexType name="anyThing"/>
is allowed.

Note: Aside from the simple coherence requirements outlined below, the requirement that
type definitions identified as restrictions actually be restrictions — that is, the
requirement that they accept as valid only a subset of the items which are accepted as
valid by their base type definition — is enforced in Constraints on Complex Type
Definition Schema Components (§3.4.6).

-}

