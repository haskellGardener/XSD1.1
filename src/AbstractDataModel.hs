{-# Language ExistentialQuantification, QuasiQuotes, TemplateHaskell #-}
{-| Time-stamp: <2019-11-06 19:13:17 CST>

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


{- 

Some random project goals.

xsd to Haskell accurate internal (not exposed) reification.
Haskell accurate reification (not exposed) to exposed user types.
Generate exposed user types based on the accurate reification.
Provide the ability to attempt (Maybe) conversion of exposed user types to accurate reification.
Generate correct XML (initially Data.XML) based on Haskell accurate reification (not exposed).
Validation of xml based on xsd via Haskell accurate internal (not exposed) reification.

-}




module AbstractDataModel where

-- Local Imports

-- import Builtin

-- Explicit Imports

import Data.XML.Types --  (elementChildren, nodeChildren, Document (..))

-- Qualified Imports

import qualified Data.Text as T

-- Undisciplined Imports

import Text.Pretty.Simple (pPrint) -- , pPrintNoColor)

import Text.XML.Unresolved (def, psRetainNamespaces, readFile)

import Prelude hiding (readFile)

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

getXSD :: FilePath -> IO Document
getXSD path = readFile parseSettings path
  where parseSettings = def { psRetainNamespaces = True } -- This is important!                                                                      -- ⚡

cleanPrint :: Document -> IO ()
cleanPrint docx = do
  pPrint $ docx { documentRoot = (documentRoot docx) { elementNodes = newElementNodes } } -- pPrintNoColor
  putStr "\n"
  where newElementNodes = walkNodes . elementNodes $ documentRoot docx

walkNodes :: [] Node -> [] Node
walkNodes [] = []
walkNodes x = map (walkNode . cropEmptyContent) x

walkNode :: Node -> Node
walkNode (NodeElement element) = NodeElement element { elementNodes = walkNodes (elementNodes element) }
walkNode x = x

cropEmptyContent :: Node -> Node
cropEmptyContent (NodeContent (ContentText x)) = NodeContent . ContentText $ T.strip x
cropEmptyContent x = x

-- Get various xsd/XML files and convert to Haskell.

example3_20XSD :: IO ()
example3_20XSD = do
  xsd <- getXSD "/home/robert/Projects/XSD1.1/supporting_cast/example3_20.xsd"
  print $ show xsd
--  getXSD "/home/robert/Projects/XSD1.1/supporting_cast/example3_20.xsd" >>= cleanPrint

getXMLSchema_xsd :: IO ()
getXMLSchema_xsd = getXSD "/home/robert/Projects/XSD1.1/supporting_cast/XMLSchema.xsd" >>= cleanPrint

datatypes :: IO ()
datatypes = getXSD "/home/robert/Projects/XSD1.1/supporting_cast/datatypes.xsd" >>= cleanPrint
                     
shiporder :: IO ()
shiporder = getXSD "/home/robert/Projects/XSD1.1/supporting_cast/shiporder.xsd" >>= cleanPrint

rxmp :: IO ()
rxmp = getXSD "/home/robert/Projects/XSD1.1/supporting_cast/rxmp.xsd" >>= cleanPrint

purchaseOrder :: IO ()
purchaseOrder = getXSD "/home/robert/Projects/XSD1.1/supporting_cast/purchaseOrder.xsd" >>= cleanPrint

irsbaseFactaNotification :: IO ()
irsbaseFactaNotification = getXSD "/home/robert/Projects/XSD1.1/supporting_cast/BASE-FATCA-NOTIFICATION-2.4.xsd" >>= cleanPrint

irs_sdt :: IO ()
irs_sdt = getXSD "/home/robert/Projects/XSD1.1/supporting_cast/IRS-SDT.xsd" >>= cleanPrint

irs_cbc :: IO ()
irs_cbc = getXSD "/home/robert/Projects/XSD1.1/supporting_cast/IRS-CBC.xsd" >>= cleanPrint

{-

instance Default ParseSettings where
    def = ParseSettings
        { psDecodeEntities = decodeXmlEntities
        , psRetainNamespaces = False
        , psDecodeIllegalCharacters = const Nothing
        }


instance Default RenderSettings where
    def = RenderSettings
        { rsPretty = False
        , rsNamespaces = []
        , rsAttrOrder = const Map.toList
        , rsUseCDATA = const False
        , rsXMLDeclaration = True
        }
-}
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





