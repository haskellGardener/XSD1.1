Document
  { documentPrologue = Prologue
    { prologueBefore = []
    , prologueDoctype = Nothing
    , prologueAfter = []
    }
  , documentRoot = Element
    { elementName = Name
      { nameLocalName = "schema"
      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
      , namePrefix = Just "xs"
      }
    , elementAttributes =
      [
       ( Name
         { nameLocalName = "targetNamespace"
         , nameNamespace = Nothing
         , namePrefix = Nothing
         }
       , [ ContentText "http://datypic.com/prod" ]
       )
      ,
       ( Name
         { nameLocalName = "xmlns:prod"
         , nameNamespace = Nothing
         , namePrefix = Nothing
         }
       , [ ContentText "http://datypic.com/prod" ]
       )
      ,
       ( Name
         { nameLocalName = "xmlns:xs"
         , nameNamespace = Nothing
         , namePrefix = Nothing
         }
       , [ ContentText "http://www.w3.org/2001/XMLSchema" ]
       )
      ]
    , elementNodes =
      [ NodeContent ( ContentText "" )
      , NodeElement
        ( Element
          { elementName = Name
            { nameLocalName = "element"
            , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
            , namePrefix = Just "xs"
            }
          , elementAttributes =
            [
             ( Name
               { nameLocalName = "type"
               , nameNamespace = Nothing
               , namePrefix = Nothing
               }
             , [ ContentText "xs:integer" ]
             )
            ,
             ( Name
               { nameLocalName = "name"
               , nameNamespace = Nothing
               , namePrefix = Nothing
               }
             , [ ContentText "number" ]
             )
            ]
          , elementNodes = []
          }
        )
      , NodeContent ( ContentText "" )
      , NodeElement
        ( Element
          { elementName = Name
            { nameLocalName = "element"
            , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
            , namePrefix = Just "xs"
            }
          , elementAttributes =
            [
             ( Name
               { nameLocalName = "type"
               , nameNamespace = Nothing
               , namePrefix = Nothing
               }
             , [ ContentText "prod:SizeType" ]
             )
            ,
             ( Name
               { nameLocalName = "name"
               , nameNamespace = Nothing
               , namePrefix = Nothing
               }
             , [ ContentText "size" ]
             )
            ]
          , elementNodes = []
          }
        )
      , NodeContent ( ContentText "" )
      , NodeElement
        ( Element
          { elementName = Name
            { nameLocalName = "simpleType"
            , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
            , namePrefix = Just "xs"
            }
          , elementAttributes =
            [
             ( Name
               { nameLocalName = "name"
               , nameNamespace = Nothing
               , namePrefix = Nothing
               }
             , [ ContentText "SizeType" ]
             )
            ]
          , elementNodes =
            [ NodeContent ( ContentText "" )
            , NodeElement
              ( Element
                { elementName = Name
                  { nameLocalName = "restriction"
                  , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                  , namePrefix = Just "xs"
                  }
                , elementAttributes =
                  [
                   ( Name
                     { nameLocalName = "base"
                     , nameNamespace = Nothing
                     , namePrefix = Nothing
                     }
                   , [ ContentText "xs:integer" ]
                   )
                  ]
                , elementNodes =
                  [ NodeContent ( ContentText "" )
                  , NodeComment "See http://www.datypic.com/books/defxmlschema/chapter03.html#EX35"
                  , NodeContent ( ContentText "" )
                  ]
                }
              )
            , NodeContent ( ContentText "" )
            ]
          }
        )
      , NodeContent ( ContentText "" )
      ]
    }
  , documentEpilogue = []
  }

