-- This file is a Haskell Text.XML reified version of shiporder.xsd

{-
import Data.XML.Types

data Document = Document { documentPrologue :: Prologue
	                 , documentRoot     :: Element
	                 , documentEpilogue :: [Miscellaneous]
	                 }
                deriving (Data, Eq, Ord, Show, Typeable)

data Prologue = Prologue { prologueBefore  :: [Miscellaneous]
	                 , prologueDoctype :: Maybe Doctype
	                 , prologueAfter   :: [Miscellaneous]
	                 }
	        deriving (Data, Eq, Ord, Show, Typeable)

data Doctype = Doctype { doctypeName :: Text
                       , doctypeID   :: Maybe ExternalID
                       }
               deriving (Data, Eq, Ord, Show, Typeable)

data Node = NodeElement     Element
          | NodeInstruction Instruction
          | NodeContent     Content
          | NodeComment     Text
            deriving (Data, Eq, Ord, Show, Typeable)

data Content = ContentText Text
             | ContentEntity Text -- ^ For pass-through parsing
               deriving (Data, Eq, Ord, Show, Typeable)

data Element = Element { elementName       :: Name
                       , elementAttributes :: Map.Map Name Text
                       , elementNodes      :: [Node]
                       }
               deriving (Data, Eq, Ord, Show, Typeable)

data Name = Name { nameLocalName :: Text
                 , nameNamespace :: Maybe Text
                 , namePrefix    :: Maybe Text
                 }
            deriving (Data, Show, Typeable)

data Instruction =
  Instruction { instructionTarget :: Text
              , instructionData   :: Text
              }
  deriving (Data, Eq, Ord, Show, Typeable)

data Miscellaneous
  = MiscInstruction Instruction
  | MiscComment     Text
    deriving (Data, Eq, Ord, Show, Typeable)


-}

Document -- :: Document
  { documentPrologue = Prologue -- :: Prologue
      { prologueBefore  = []      -- :: [] Miscellaneous
      , prologueDoctype = Nothing -- :: Maybe Doctype
      , prologueAfter   = []      -- :: [] Miscellaneous
      }
  , documentRoot = Element -- :: Element                                                       -- <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
      { elementName = Name -- :: Name
          { nameLocalName = "schema"                                -- :: Text
          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" -- :: Maybe Text
          , namePrefix    = Just "xs"                               -- :: Maybe Text
          }
      , elementAttributes = fromList -- :: Map Name Text
          [ 
           ( Name -- :: Name                                                                   -- xmlns:xs="http://www.w3.org/2001/XMLSchema"
             { nameLocalName = "xmlns:xs" -- :: Text      
             , nameNamespace = Nothing    -- :: Maybe Text
             , namePrefix    = Nothing    -- :: Maybe Text
             } 
           , "http://www.w3.org/2001/XMLSchema" -- :: Text
           ) 
          ]
      , elementNodes = -- :: [Node]
          [ NodeContent "" -- :: Text
          , NodeElement    -- :: Element
              ( Element -- :: Element                                                                                    -- <xs:element name="shiporder">
                  { elementName = Name -- :: Name
                      { nameLocalName = "element"                               -- :: Text
                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" -- :: Maybe Text
                      , namePrefix    = Just "xs"                               -- :: Maybe Text
                      }
                  , elementAttributes = fromList -- :: Map Name Text
                      [
                          ( Name -- :: Name
                              { nameLocalName = "name"  -- :: Text
                              , nameNamespace = Nothing -- :: Maybe Text
                              , namePrefix    = Nothing -- :: Maybe Text
                              }
                          , "shiporder" -- :: Text
                          )
                      ]
                  , elementNodes = -- :: [Node]
                      [ NodeContent "" -- :: Text
                      , NodeElement    -- :: Element
                          ( Element    -- :: Element                                                                     -- <xs:complexType>
                              { elementName = Name -- :: Name
                                  { nameLocalName = "complexType"                           -- :: Text      
                                  , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" -- :: Maybe Text
                                  , namePrefix    = Just "xs"                               -- :: Maybe Text
                                  }
                              , elementAttributes = fromList [] -- :: Map Name Text
                              , elementNodes = -- :: [Node]
                                  [ NodeContent ""
                                  , NodeElement
                                      ( Element                                                                          -- <xs:sequence>
                                          { elementName = Name
                                              { nameLocalName = "sequence"
                                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                              , namePrefix = Just "xs"
                                              }
                                          , elementAttributes = fromList []
                                          , elementNodes =
                                              [ NodeContent ""
                                              , NodeElement
                                                  ( Element                              -- <xs:element name="orderperson" type="xs:string"/>
                                                      { elementName = Name
                                                          { nameLocalName = "element"
                                                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                                          , namePrefix = Just "xs"
                                                          }
                                                      , elementAttributes = fromList
                                                          [
                                                              ( Name
                                                                  { nameLocalName = "name"
                                                                  , nameNamespace = Nothing
                                                                  , namePrefix = Nothing
                                                                  }
                                                              , "orderperson"
                                                              )
                                                          ,
                                                              ( Name
                                                                  { nameLocalName = "type"
                                                                  , nameNamespace = Nothing
                                                                  , namePrefix = Nothing
                                                                  }
                                                              , "xs:string"
                                                              )
                                                          ]
                                                      , elementNodes = []
                                                      }
                                                  )
                                              , NodeContent ""
                                              , NodeElement
                                                  ( Element                              -- <xs:element name="shipto">
                                                      { elementName = Name
                                                          { nameLocalName = "element"
                                                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                                          , namePrefix = Just "xs"
                                                          }
                                                      , elementAttributes = fromList
                                                          [
                                                              ( Name
                                                                  { nameLocalName = "name"
                                                                  , nameNamespace = Nothing
                                                                  , namePrefix = Nothing
                                                                  }
                                                              , "shipto"
                                                              )
                                                          ]
                                                      , elementNodes =
                                                          [ NodeContent ""
                                                          , NodeElement                  -- <xs:complexType>
                                                              ( Element
                                                                  { elementName = Name
                                                                      { nameLocalName = "complexType"
                                                                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                                                      , namePrefix = Just "xs"
                                                                      }
                                                                  , elementAttributes = fromList []
                                                                  , elementNodes =
                                                                      [ NodeContent ""
                                                                      , NodeElement      -- <xs:sequence>
                                                                          ( Element
                                                                              { elementName = Name
                                                                                  { nameLocalName = "sequence"
                                                                                  , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                                                                  , namePrefix = Just "xs"
                                                                                  }
                                                                              , elementAttributes = fromList []
                                                                              , elementNodes =
                                                                                  [ NodeContent ""
                                                                                  , NodeElement   -- <xs:element name="name" type="xs:string"/>
                                                                                      ( Element
                                                                                          { elementName = Name
                                                                                              { nameLocalName = "element"
                                                                                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                                                                              , namePrefix = Just "xs"
                                                                                              }
                                                                                          , elementAttributes = fromList
                                                                                              [
                                                                                                  ( Name
                                                                                                      { nameLocalName = "name"
                                                                                                      , nameNamespace = Nothing
                                                                                                      , namePrefix = Nothing
                                                                                                      }
                                                                                                  , "name"
                                                                                                  )
                                                                                              ,
                                                                                                  ( Name
                                                                                                      { nameLocalName = "type"
                                                                                                      , nameNamespace = Nothing
                                                                                                      , namePrefix = Nothing
                                                                                                      }
                                                                                                  , "xs:string"
                                                                                                  )
                                                                                              ]
                                                                                          , elementNodes = []
                                                                                          }
                                                                                      )
                                                                                  , NodeContent ""
                                                                                  , NodeElement   -- <xs:element name="address" type="xs:string"/>
                                                                                      ( Element
                                                                                          { elementName = Name
                                                                                              { nameLocalName = "element"
                                                                                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                                                                              , namePrefix = Just "xs"
                                                                                              }
                                                                                          , elementAttributes = fromList
                                                                                              [
                                                                                                  ( Name
                                                                                                      { nameLocalName = "name"
                                                                                                      , nameNamespace = Nothing
                                                                                                      , namePrefix = Nothing
                                                                                                      }
                                                                                                  , "address"
                                                                                                  )
                                                                                              ,
                                                                                                  ( Name
                                                                                                      { nameLocalName = "type"
                                                                                                      , nameNamespace = Nothing
                                                                                                      , namePrefix = Nothing
                                                                                                      }
                                                                                                  , "xs:string"
                                                                                                  )
                                                                                              ]
                                                                                          , elementNodes = []
                                                                                          }
                                                                                      )
                                                                                  , NodeContent ""
                                                                                  , NodeElement   -- <xs:element name="city" type="xs:string"/>
                                                                                      ( Element
                                                                                          { elementName = Name
                                                                                              { nameLocalName = "element"
                                                                                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                                                                              , namePrefix = Just "xs"
                                                                                              }
                                                                                          , elementAttributes = fromList
                                                                                              [
                                                                                                  ( Name
                                                                                                      { nameLocalName = "name"
                                                                                                      , nameNamespace = Nothing
                                                                                                      , namePrefix = Nothing
                                                                                                      }
                                                                                                  , "city"
                                                                                                  )
                                                                                              ,
                                                                                                  ( Name
                                                                                                      { nameLocalName = "type"
                                                                                                      , nameNamespace = Nothing
                                                                                                      , namePrefix = Nothing
                                                                                                      }
                                                                                                  , "xs:string"
                                                                                                  )
                                                                                              ]
                                                                                          , elementNodes = []
                                                                                          }
                                                                                      )
                                                                                  , NodeContent ""
                                                                                  , NodeElement   -- <xs:element name="country" type="xs:string"/>
                                                                                      ( Element
                                                                                          { elementName = Name
                                                                                              { nameLocalName = "element"
                                                                                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                                                                              , namePrefix = Just "xs"
                                                                                              }
                                                                                          , elementAttributes = fromList
                                                                                              [
                                                                                                  ( Name
                                                                                                      { nameLocalName = "name"
                                                                                                      , nameNamespace = Nothing
                                                                                                      , namePrefix = Nothing
                                                                                                      }
                                                                                                  , "country"
                                                                                                  )
                                                                                              ,
                                                                                                  ( Name
                                                                                                      { nameLocalName = "type"
                                                                                                      , nameNamespace = Nothing
                                                                                                      , namePrefix = Nothing
                                                                                                      }
                                                                                                  , "xs:string"
                                                                                                  )
                                                                                              ]
                                                                                          , elementNodes = []
                                                                                          }
                                                                                      )
                                                                                  , NodeContent ""
                                                                                  ]
                                                                              }
                                                                          )
                                                                      , NodeContent ""
                                                                      ]
                                                                  }
                                                              )
                                                          , NodeContent ""
                                                          ]
                                                      }
                                                  )
                                              , NodeContent ""
                                              , NodeElement                              -- <xs:element name="item" maxOccurs="unbounded">
                                                  ( Element
                                                      { elementName = Name
                                                          { nameLocalName = "element"
                                                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                                          , namePrefix = Just "xs"
                                                          }
                                                      , elementAttributes = fromList
                                                          [
                                                              ( Name
                                                                  { nameLocalName = "maxOccurs"
                                                                  , nameNamespace = Nothing
                                                                  , namePrefix = Nothing
                                                                  }
                                                              , "unbounded"
                                                              )
                                                          ,
                                                              ( Name
                                                                  { nameLocalName = "name"
                                                                  , nameNamespace = Nothing
                                                                  , namePrefix = Nothing
                                                                  }
                                                              , "item"
                                                              )
                                                          ]
                                                      , elementNodes =
                                                          [ NodeContent ""
                                                          , NodeElement                  -- <xs:complexType>
                                                              ( Element
                                                                  { elementName = Name
                                                                      { nameLocalName = "complexType"
                                                                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                                                      , namePrefix = Just "xs"
                                                                      }
                                                                  , elementAttributes = fromList []
                                                                  , elementNodes =
                                                                      [ NodeContent ""
                                                                      , NodeElement      -- <xs:sequence>
                                                                          ( Element
                                                                              { elementName = Name
                                                                                  { nameLocalName = "sequence"
                                                                                  , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                                                                  , namePrefix = Just "xs"
                                                                                  }
                                                                              , elementAttributes = fromList []
                                                                              , elementNodes =
                                                                                  [ NodeContent ""
                                                                                  , NodeElement -- <xs:element name="title" type="xs:string"/>
                                                                                      ( Element
                                                                                          { elementName = Name
                                                                                              { nameLocalName = "element"
                                                                                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                                                                              , namePrefix = Just "xs"
                                                                                              }
                                                                                          , elementAttributes = fromList
                                                                                              [
                                                                                                  ( Name
                                                                                                      { nameLocalName = "name"
                                                                                                      , nameNamespace = Nothing
                                                                                                      , namePrefix = Nothing
                                                                                                      }
                                                                                                  , "title"
                                                                                                  )
                                                                                              ,
                                                                                                  ( Name
                                                                                                      { nameLocalName = "type"
                                                                                                      , nameNamespace = Nothing
                                                                                                      , namePrefix = Nothing
                                                                                                      }
                                                                                                  , "xs:string"
                                                                                                  )
                                                                                              ]
                                                                                          , elementNodes = []
                                                                                          }
                                                                                      )
                                                                                  , NodeContent ""
                                                                                  , NodeElement -- <xs:element name="note" type="xs:string" minOccurs="0" />
                                                                                      ( Element
                                                                                          { elementName = Name
                                                                                              { nameLocalName = "element"
                                                                                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                                                                              , namePrefix = Just "xs"
                                                                                              }
                                                                                          , elementAttributes = fromList
                                                                                              [
                                                                                                  ( Name
                                                                                                      { nameLocalName = "minOccurs"
                                                                                                      , nameNamespace = Nothing
                                                                                                      , namePrefix = Nothing
                                                                                                      }
                                                                                                  , "0"
                                                                                                  )
                                                                                              ,
                                                                                                  ( Name
                                                                                                      { nameLocalName = "name"
                                                                                                      , nameNamespace = Nothing
                                                                                                      , namePrefix = Nothing
                                                                                                      }
                                                                                                  , "note"
                                                                                                  )
                                                                                              ,
                                                                                                  ( Name
                                                                                                      { nameLocalName = "type"
                                                                                                      , nameNamespace = Nothing
                                                                                                      , namePrefix = Nothing
                                                                                                      }
                                                                                                  , "xs:string"
                                                                                                  )
                                                                                              ]
                                                                                          , elementNodes = []
                                                                                          }
                                                                                      )
                                                                                  , NodeContent ""
                                                                                  , NodeElement -- <xs:element name="quantity" type="xs:positiveInteger" />
                                                                                      ( Element
                                                                                          { elementName = Name
                                                                                              { nameLocalName = "element"
                                                                                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                                                                              , namePrefix = Just "xs"
                                                                                              }
                                                                                          , elementAttributes = fromList
                                                                                              [
                                                                                                  ( Name
                                                                                                      { nameLocalName = "name"
                                                                                                      , nameNamespace = Nothing
                                                                                                      , namePrefix = Nothing
                                                                                                      }
                                                                                                  , "quantity"
                                                                                                  )
                                                                                              ,
                                                                                                  ( Name
                                                                                                      { nameLocalName = "type"
                                                                                                      , nameNamespace = Nothing
                                                                                                      , namePrefix = Nothing
                                                                                                      }
                                                                                                  , "xs:positiveInteger"
                                                                                                  )
                                                                                              ]
                                                                                          , elementNodes = []
                                                                                          }
                                                                                      )
                                                                                  , NodeContent ""
                                                                                  , NodeElement -- <xs:element name="price" type="xs:decimal" />
                                                                                      ( Element
                                                                                          { elementName = Name
                                                                                              { nameLocalName = "element"
                                                                                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                                                                              , namePrefix = Just "xs"
                                                                                              }
                                                                                          , elementAttributes = fromList
                                                                                              [
                                                                                                  ( Name
                                                                                                      { nameLocalName = "name"
                                                                                                      , nameNamespace = Nothing
                                                                                                      , namePrefix = Nothing
                                                                                                      }
                                                                                                  , "price"
                                                                                                  )
                                                                                              ,
                                                                                                  ( Name
                                                                                                      { nameLocalName = "type"
                                                                                                      , nameNamespace = Nothing
                                                                                                      , namePrefix = Nothing
                                                                                                      }
                                                                                                  , "xs:decimal"
                                                                                                  )
                                                                                              ]
                                                                                          , elementNodes = []
                                                                                          }
                                                                                      )
                                                                                  , NodeContent ""
                                                                                  ]
                                                                              }
                                                                          )
                                                                      , NodeContent ""
                                                                      ]
                                                                  }
                                                              )
                                                          , NodeContent ""
                                                          ]
                                                      }
                                                  )
                                              , NodeContent ""
                                              ]
                                          }
                                      )
                                  , NodeContent ""
                                  , NodeElement         -- <xs:attribute name="orderid" type="xs:string" use="required"/>
                                      ( Element
                                          { elementName = Name
                                              { nameLocalName = "attribute"
                                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                              , namePrefix = Just "xs"
                                              }
                                          , elementAttributes = fromList
                                              [
                                                  ( Name
                                                      { nameLocalName = "name"
                                                      , nameNamespace = Nothing
                                                      , namePrefix = Nothing
                                                      }
                                                  , "orderid"
                                                  )
                                              ,
                                                  ( Name
                                                      { nameLocalName = "type"
                                                      , nameNamespace = Nothing
                                                      , namePrefix = Nothing
                                                      }
                                                  , "xs:string"
                                                  )
                                              ,
                                                  ( Name
                                                      { nameLocalName = "use"
                                                      , nameNamespace = Nothing
                                                      , namePrefix = Nothing
                                                      }
                                                  , "required"
                                                  )
                                              ]
                                          , elementNodes = []
                                          }
                                      )
                                  , NodeContent ""
                                  ]
                              }
                          )
                      , NodeContent ""
                      ]
                  }
              )
          , NodeContent ""
          ]
      }
  , documentEpilogue = [] -- :: [Miscellaneous]
  }
