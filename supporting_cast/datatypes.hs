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
         { nameLocalName = "version"
         , nameNamespace = Nothing
         , namePrefix = Nothing
         }
       , [ ContentText "Id: datatypes.xsd,v 1.4 2004/05/29 10:26:33 ht Exp " ]
       )
      ,
       ( Name
         { nameLocalName = "targetNamespace"
         , nameNamespace = Nothing
         , namePrefix = Nothing
         }
       , [ ContentText "http://www.w3.org/2001/XMLSchema" ]
       )
      ,
       ( Name
         { nameLocalName = "lang"
         , nameNamespace = Just "http://www.w3.org/XML/1998/namespace"
         , namePrefix = Just "xml"
         }
       , [ ContentText "en" ]
       )
      ,
       ( Name
         { nameLocalName = "elementFormDefault"
         , nameNamespace = Nothing
         , namePrefix = Nothing
         }
       , [ ContentText "qualified" ]
       )
      ,
       ( Name
         { nameLocalName = "blockDefault"
         , nameNamespace = Nothing
         , namePrefix = Nothing
         }
       , [ ContentText "#all" ]
       )
      ,
       ( Name
         { nameLocalName = "xmlns:xs"
         , nameNamespace = Nothing
         , namePrefix = Nothing
         }
       , [ ContentText "http://www.w3.org/2001/XMLSchema" ]
       )
      ,
       ( Name
         { nameLocalName = "xmlns:hfp"
         , nameNamespace = Nothing
         , namePrefix = Nothing
         }
       , [ ContentText "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty" ]
       )
      ]
    , elementNodes =
      [ NodeContent ( ContentText "" )
      , NodeElement
        ( Element
          { elementName = Name
            { nameLocalName = "annotation"
            , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
            , namePrefix = Just "xs"
            }
          , elementAttributes = []
          , elementNodes =
            [ NodeContent ( ContentText "" )
            , NodeElement
              ( Element
                { elementName = Name
                  { nameLocalName = "documentation"
                  , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                  , namePrefix = Just "xs"
                  }
                , elementAttributes =
                  [
                   ( Name
                     { nameLocalName = "source"
                     , nameNamespace = Nothing
                     , namePrefix = Nothing
                     }
                   , [ ContentText "../datatypes/datatypes-with-errata.html" ]
                   )
                  ]
                , elementNodes =
                  [ NodeContent ( ContentText
                                  "The schema corresponding to this document is normative,"
                                \ "with respect to the syntactic constraints it expresses in the"
                                \ "XML Schema language.  The documentation (within <documentation>"
                                \ "elements) below, is not normative, but rather highlights important"
                                \ "aspects of the W3C Recommendation of which this is a part"
                                )
                  ]
                }
              )
            , NodeContent ( ContentText "" )
            ]
          }
        )
      , NodeContent ( ContentText "" )
      , NodeElement
        ( Element
          { elementName = Name
            { nameLocalName = "annotation"
            , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
            , namePrefix = Just "xs"
            }
          , elementAttributes = []
          , elementNodes =
            [ NodeContent ( ContentText "" )
            , NodeElement
              ( Element
                { elementName = Name
                  { nameLocalName = "documentation"
                  , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                  , namePrefix = Just "xs"
                  }
                , elementAttributes = []
                , elementNodes =
                  [ NodeContent ( ContentText
                                  "First the built-in primitive datatypes.  These definitions are for"
                                \ "information only, the real built-in definitions are magic."
                                )
                  ]
                }
              )
            , NodeContent ( ContentText "" )
            , NodeElement
              ( Element
                { elementName = Name
                  { nameLocalName = "documentation"
                  , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                  , namePrefix = Just "xs"
                  }
                , elementAttributes = []
                , elementNodes =
                  [ NodeContent
                    ( ContentText
                      "For each built-in datatype in this schema (both primitive and"
                      \ "derived) can be uniquely addressed via a URI constructed"
                      \ "as follows:"
                      \ "1) the base URI is the URI of the XML Schema namespace"
                      \ "2) the fragment identifier is the name of the datatype"
                      \ "For example, to address the int datatype, the URI is:"

                      \ "http://www.w3.org/2001/XMLSchema#int"

                      \ "Additionally, each facet definition element can be uniquely"
                      \ "addressed via a URI constructed as follows:"
                      \ "1) the base URI is the URI of the XML Schema namespace"
                      \ "2) the fragment identifier is the name of the facet"

                      \ "For example, to address the maxInclusive facet, the URI is:"

                      \ "http://www.w3.org/2001/XMLSchema#maxInclusive"

                      \ "Additionally, each facet usage in a built-in datatype definition"
                      \ "can be uniquely addressed via a URI constructed as follows:"
                      \ "1) the base URI is the URI of the XML Schema namespace"
                      \ "2) the fragment identifier is the name of the datatype, followed"
                      \ "by a period (\".\") followed by the name of the facet"

                      \ "For example, to address the usage of the maxInclusive facet in"
                      \ "the definition of int, the URI is:"

                      \ "http://www.w3.org/2001/XMLSchema#int.maxInclusive"
                    )
                  ]
                }
              )
            , NodeContent ( ContentText "" )
            ]
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
               { nameLocalName = "id"
               , nameNamespace = Nothing
               , namePrefix = Nothing
               }
             , [ ContentText "string" ]
             )
            ,
             ( Name
               { nameLocalName = "name"
               , nameNamespace = Nothing
               , namePrefix = Nothing
               }
             , [ ContentText "string" ]
             )
            ]
          , elementNodes =
            [ NodeContent ( ContentText "" )
            , NodeElement
              ( Element
                { elementName = Name
                  { nameLocalName = "annotation"
                  , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                  , namePrefix = Just "xs"
                  }
                , elementAttributes = []
                , elementNodes =
                  [ NodeContent ( ContentText "" )
                  , NodeElement
                    ( Element
                      { elementName = Name
                        { nameLocalName = "appinfo"
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                        , namePrefix = Just "xs"
                        }
                      , elementAttributes = []
                      , elementNodes =
                        [ NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "length" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "minLength" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "maxLength" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "pattern" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "enumeration" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "whiteSpace" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasProperty"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "value"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "false" ]
                               )
                              ,
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "ordered" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasProperty"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "value"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "false" ]
                               )
                              ,
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "bounded" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasProperty"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "value"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "countably infinite" ]
                               )
                              ,
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "cardinality" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasProperty"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "value"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "false" ]
                               )
                              ,
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "numeric" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        ]
                      }
                    )
                  , NodeContent ( ContentText "" )
                  , NodeElement
                    ( Element
                      { elementName = Name
                        { nameLocalName = "documentation"
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                        , namePrefix = Just "xs"
                        }
                      , elementAttributes =
                        [
                         ( Name
                           { nameLocalName = "source"
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           }
                         , [ ContentText "http://www.w3.org/TR/xmlschema-2/#string" ]
                         )
                        ]
                      , elementNodes = []
                      }
                    )
                  , NodeContent ( ContentText "" )
                  ]
                }
              )
            , NodeContent ( ContentText "" )
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
                   , [ ContentText "xs:anySimpleType" ]
                   )
                  ]
                , elementNodes =
                  [ NodeContent ( ContentText "" )
                  , NodeElement
                    ( Element
                      { elementName = Name
                        { nameLocalName = "whiteSpace"
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                        , namePrefix = Just "xs"
                        }
                      , elementAttributes =
                        [
                         ( Name
                           { nameLocalName = "id"
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           }
                         , [ ContentText "string.preserve" ]
                         )
                        ,
                         ( Name
                           { nameLocalName = "value"
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           }
                         , [ ContentText "preserve" ]
                         )
                        ]
                      , elementNodes = []
                      }
                    )
                  , NodeContent ( ContentText "" )
                  ]
                }
              )
            , NodeContent ( ContentText "" )
            ]
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
               { nameLocalName = "id"
               , nameNamespace = Nothing
               , namePrefix = Nothing
               }
             , [ ContentText "boolean" ]
             )
            ,
             ( Name
               { nameLocalName = "name"
               , nameNamespace = Nothing
               , namePrefix = Nothing
               }
             , [ ContentText "boolean" ]
             )
            ]
          , elementNodes =
            [ NodeContent ( ContentText "" )
            , NodeElement
              ( Element
                { elementName = Name
                  { nameLocalName = "annotation"
                  , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                  , namePrefix = Just "xs"
                  }
                , elementAttributes = []
                , elementNodes =
                  [ NodeContent ( ContentText "" )
                  , NodeElement
                    ( Element
                      { elementName = Name
                        { nameLocalName = "appinfo"
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                        , namePrefix = Just "xs"
                        }
                      , elementAttributes = []
                      , elementNodes =
                        [ NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "pattern" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "whiteSpace" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasProperty"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "value"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "false" ]
                               )
                              ,
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "ordered" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasProperty"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "value"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "false" ]
                               )
                              ,
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "bounded" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasProperty"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "value"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "finite" ]
                               )
                              ,
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "cardinality" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasProperty"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "value"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "false" ]
                               )
                              ,
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "numeric" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        ]
                      }
                    )
                  , NodeContent ( ContentText "" )
                  , NodeElement
                    ( Element
                      { elementName = Name
                        { nameLocalName = "documentation"
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                        , namePrefix = Just "xs"
                        }
                      , elementAttributes =
                        [
                         ( Name
                           { nameLocalName = "source"
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           }
                         , [ ContentText "http://www.w3.org/TR/xmlschema-2/#boolean" ]
                         )
                        ]
                      , elementNodes = []
                      }
                    )
                  , NodeContent ( ContentText "" )
                  ]
                }
              )
            , NodeContent ( ContentText "" )
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
                   , [ ContentText "xs:anySimpleType" ]
                   )
                  ]
                , elementNodes =
                  [ NodeContent ( ContentText "" )
                  , NodeElement
                    ( Element
                      { elementName = Name
                        { nameLocalName = "whiteSpace"
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                        , namePrefix = Just "xs"
                        }
                      , elementAttributes =
                        [
                         ( Name
                           { nameLocalName = "id"
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           }
                         , [ ContentText "boolean.whiteSpace" ]
                         )
                        ,
                         ( Name
                           { nameLocalName = "value"
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           }
                         , [ ContentText "collapse" ]
                         )
                        ,
                         ( Name
                           { nameLocalName = "fixed"
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           }
                         , [ ContentText "true" ]
                         )
                        ]
                      , elementNodes = []
                      }
                    )
                  , NodeContent ( ContentText "" )
                  ]
                }
              )
            , NodeContent ( ContentText "" )
            ]
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
               { nameLocalName = "id"
               , nameNamespace = Nothing
               , namePrefix = Nothing
               }
             , [ ContentText "float" ]
             )
            ,
             ( Name
               { nameLocalName = "name"
               , nameNamespace = Nothing
               , namePrefix = Nothing
               }
             , [ ContentText "float" ]
             )
            ]
          , elementNodes =
            [ NodeContent ( ContentText "" )
            , NodeElement
              ( Element
                { elementName = Name
                  { nameLocalName = "annotation"
                  , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                  , namePrefix = Just "xs"
                  }
                , elementAttributes = []
                , elementNodes =
                  [ NodeContent ( ContentText "" )
                  , NodeElement
                    ( Element
                      { elementName = Name
                        { nameLocalName = "appinfo"
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                        , namePrefix = Just "xs"
                        }
                      , elementAttributes = []
                      , elementNodes =
                        [ NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "pattern" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "enumeration" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "whiteSpace" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "maxInclusive" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "maxExclusive" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "minInclusive" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "minExclusive" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasProperty"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "value"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "partial" ]
                               )
                              ,
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "ordered" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasProperty"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "value"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "true" ]
                               )
                              ,
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "bounded" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasProperty"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "value"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "finite" ]
                               )
                              ,
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "cardinality" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasProperty"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "value"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "true" ]
                               )
                              ,
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "numeric" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        ]
                      }
                    )
                  , NodeContent ( ContentText "" )
                  , NodeElement
                    ( Element
                      { elementName = Name
                        { nameLocalName = "documentation"
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                        , namePrefix = Just "xs"
                        }
                      , elementAttributes =
                        [
                         ( Name
                           { nameLocalName = "source"
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           }
                         , [ ContentText "http://www.w3.org/TR/xmlschema-2/#float" ]
                         )
                        ]
                      , elementNodes = []
                      }
                    )
                  , NodeContent ( ContentText "" )
                  ]
                }
              )
            , NodeContent ( ContentText "" )
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
                   , [ ContentText "xs:anySimpleType" ]
                   )
                  ]
                , elementNodes =
                  [ NodeContent ( ContentText "" )
                  , NodeElement
                    ( Element
                      { elementName = Name
                        { nameLocalName = "whiteSpace"
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                        , namePrefix = Just "xs"
                        }
                      , elementAttributes =
                        [
                         ( Name
                           { nameLocalName = "id"
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           }
                         , [ ContentText "float.whiteSpace" ]
                         )
                        ,
                         ( Name
                           { nameLocalName = "value"
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           }
                         , [ ContentText "collapse" ]
                         )
                        ,
                         ( Name
                           { nameLocalName = "fixed"
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           }
                         , [ ContentText "true" ]
                         )
                        ]
                      , elementNodes = []
                      }
                    )
                  , NodeContent ( ContentText "" )
                  ]
                }
              )
            , NodeContent ( ContentText "" )
            ]
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
               { nameLocalName = "id"
               , nameNamespace = Nothing
               , namePrefix = Nothing
               }
             , [ ContentText "double" ]
             )
            ,
             ( Name
               { nameLocalName = "name"
               , nameNamespace = Nothing
               , namePrefix = Nothing
               }
             , [ ContentText "double" ]
             )
            ]
          , elementNodes =
            [ NodeContent ( ContentText "" )
            , NodeElement
              ( Element
                { elementName = Name
                  { nameLocalName = "annotation"
                  , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                  , namePrefix = Just "xs"
                  }
                , elementAttributes = []
                , elementNodes =
                  [ NodeContent ( ContentText "" )
                  , NodeElement
                    ( Element
                      { elementName = Name
                        { nameLocalName = "appinfo"
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                        , namePrefix = Just "xs"
                        }
                      , elementAttributes = []
                      , elementNodes =
                        [ NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "pattern" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "enumeration" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "whiteSpace" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "maxInclusive" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "maxExclusive" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "minInclusive" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "minExclusive" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasProperty"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "value"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "partial" ]
                               )
                              ,
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "ordered" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasProperty"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "value"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "true" ]
                               )
                              ,
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "bounded" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasProperty"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "value"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "finite" ]
                               )
                              ,
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "cardinality" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasProperty"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "value"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "true" ]
                               )
                              ,
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "numeric" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        ]
                      }
                    )
                  , NodeContent ( ContentText "" )
                  , NodeElement
                    ( Element
                      { elementName = Name
                        { nameLocalName = "documentation"
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                        , namePrefix = Just "xs"
                        }
                      , elementAttributes =
                        [
                         ( Name
                           { nameLocalName = "source"
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           }
                         , [ ContentText "http://www.w3.org/TR/xmlschema-2/#double" ]
                         )
                        ]
                      , elementNodes = []
                      }
                    )
                  , NodeContent ( ContentText "" )
                  ]
                }
              )
            , NodeContent ( ContentText "" )
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
                   , [ ContentText "xs:anySimpleType" ]
                   )
                  ]
                , elementNodes =
                  [ NodeContent ( ContentText "" )
                  , NodeElement
                    ( Element
                      { elementName = Name
                        { nameLocalName = "whiteSpace"
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                        , namePrefix = Just "xs"
                        }
                      , elementAttributes =
                        [
                         ( Name
                           { nameLocalName = "id"
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           }
                         , [ ContentText "double.whiteSpace" ]
                         )
                        ,
                         ( Name
                           { nameLocalName = "value"
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           }
                         , [ ContentText "collapse" ]
                         )
                        ,
                         ( Name
                           { nameLocalName = "fixed"
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           }
                         , [ ContentText "true" ]
                         )
                        ]
                      , elementNodes = []
                      }
                    )
                  , NodeContent ( ContentText "" )
                  ]
                }
              )
            , NodeContent ( ContentText "" )
            ]
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
               { nameLocalName = "id"
               , nameNamespace = Nothing
               , namePrefix = Nothing
               }
             , [ ContentText "decimal" ]
             )
            ,
             ( Name
               { nameLocalName = "name"
               , nameNamespace = Nothing
               , namePrefix = Nothing
               }
             , [ ContentText "decimal" ]
             )
            ]
          , elementNodes =
            [ NodeContent ( ContentText "" )
            , NodeElement
              ( Element
                { elementName = Name
                  { nameLocalName = "annotation"
                  , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                  , namePrefix = Just "xs"
                  }
                , elementAttributes = []
                , elementNodes =
                  [ NodeContent ( ContentText "" )
                  , NodeElement
                    ( Element
                      { elementName = Name
                        { nameLocalName = "appinfo"
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                        , namePrefix = Just "xs"
                        }
                      , elementAttributes = []
                      , elementNodes =
                        [ NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "totalDigits" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "fractionDigits" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "pattern" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "whiteSpace" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "enumeration" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "maxInclusive" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "maxExclusive" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "minInclusive" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "minExclusive" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasProperty"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "value"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "total" ]
                               )
                              ,
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "ordered" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasProperty"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "value"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "false" ]
                               )
                              ,
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "bounded" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasProperty"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "value"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "countably infinite" ]
                               )
                              ,
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "cardinality" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasProperty"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "value"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "true" ]
                               )
                              ,
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "numeric" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        ]
                      }
                    )
                  , NodeContent ( ContentText "" )
                  , NodeElement
                    ( Element
                      { elementName = Name
                        { nameLocalName = "documentation"
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                        , namePrefix = Just "xs"
                        }
                      , elementAttributes =
                        [
                         ( Name
                           { nameLocalName = "source"
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           }
                         , [ ContentText "http://www.w3.org/TR/xmlschema-2/#decimal" ]
                         )
                        ]
                      , elementNodes = []
                      }
                    )
                  , NodeContent ( ContentText "" )
                  ]
                }
              )
            , NodeContent ( ContentText "" )
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
                   , [ ContentText "xs:anySimpleType" ]
                   )
                  ]
                , elementNodes =
                  [ NodeContent ( ContentText "" )
                  , NodeElement
                    ( Element
                      { elementName = Name
                        { nameLocalName = "whiteSpace"
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                        , namePrefix = Just "xs"
                        }
                      , elementAttributes =
                        [
                         ( Name
                           { nameLocalName = "id"
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           }
                         , [ ContentText "decimal.whiteSpace" ]
                         )
                        ,
                         ( Name
                           { nameLocalName = "value"
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           }
                         , [ ContentText "collapse" ]
                         )
                        ,
                         ( Name
                           { nameLocalName = "fixed"
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           }
                         , [ ContentText "true" ]
                         )
                        ]
                      , elementNodes = []
                      }
                    )
                  , NodeContent ( ContentText "" )
                  ]
                }
              )
            , NodeContent ( ContentText "" )
            ]
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
               { nameLocalName = "id"
               , nameNamespace = Nothing
               , namePrefix = Nothing
               }
             , [ ContentText "duration" ]
             )
            ,
             ( Name
               { nameLocalName = "name"
               , nameNamespace = Nothing
               , namePrefix = Nothing
               }
             , [ ContentText "duration" ]
             )
            ]
          , elementNodes =
            [ NodeContent ( ContentText "" )
            , NodeElement
              ( Element
                { elementName = Name
                  { nameLocalName = "annotation"
                  , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                  , namePrefix = Just "xs"
                  }
                , elementAttributes = []
                , elementNodes =
                  [ NodeContent ( ContentText "" )
                  , NodeElement
                    ( Element
                      { elementName = Name
                        { nameLocalName = "appinfo"
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                        , namePrefix = Just "xs"
                        }
                      , elementAttributes = []
                      , elementNodes =
                        [ NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "pattern" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "enumeration" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "whiteSpace" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "maxInclusive" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "maxExclusive" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "minInclusive" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "minExclusive" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasProperty"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "value"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "partial" ]
                               )
                              ,
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "ordered" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasProperty"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "value"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "false" ]
                               )
                              ,
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "bounded" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasProperty"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "value"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "countably infinite" ]
                               )
                              ,
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "cardinality" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasProperty"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "value"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "false" ]
                               )
                              ,
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "numeric" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        ]
                      }
                    )
                  , NodeContent ( ContentText "" )
                  , NodeElement
                    ( Element
                      { elementName = Name
                        { nameLocalName = "documentation"
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                        , namePrefix = Just "xs"
                        }
                      , elementAttributes =
                        [
                         ( Name
                           { nameLocalName = "source"
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           }
                         , [ ContentText "http://www.w3.org/TR/xmlschema-2/#duration" ]
                         )
                        ]
                      , elementNodes = []
                      }
                    )
                  , NodeContent ( ContentText "" )
                  ]
                }
              )
            , NodeContent ( ContentText "" )
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
                   , [ ContentText "xs:anySimpleType" ]
                   )
                  ]
                , elementNodes =
                  [ NodeContent ( ContentText "" )
                  , NodeElement
                    ( Element
                      { elementName = Name
                        { nameLocalName = "whiteSpace"
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                        , namePrefix = Just "xs"
                        }
                      , elementAttributes =
                        [
                         ( Name
                           { nameLocalName = "id"
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           }
                         , [ ContentText "duration.whiteSpace" ]
                         )
                        ,
                         ( Name
                           { nameLocalName = "value"
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           }
                         , [ ContentText "collapse" ]
                         )
                        ,
                         ( Name
                           { nameLocalName = "fixed"
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           }
                         , [ ContentText "true" ]
                         )
                        ]
                      , elementNodes = []
                      }
                    )
                  , NodeContent ( ContentText "" )
                  ]
                }
              )
            , NodeContent ( ContentText "" )
            ]
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
               { nameLocalName = "id"
               , nameNamespace = Nothing
               , namePrefix = Nothing
               }
             , [ ContentText "dateTime" ]
             )
            ,
             ( Name
               { nameLocalName = "name"
               , nameNamespace = Nothing
               , namePrefix = Nothing
               }
             , [ ContentText "dateTime" ]
             )
            ]
          , elementNodes =
            [ NodeContent ( ContentText "" )
            , NodeElement
              ( Element
                { elementName = Name
                  { nameLocalName = "annotation"
                  , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                  , namePrefix = Just "xs"
                  }
                , elementAttributes = []
                , elementNodes =
                  [ NodeContent ( ContentText "" )
                  , NodeElement
                    ( Element
                      { elementName = Name
                        { nameLocalName = "appinfo"
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                        , namePrefix = Just "xs"
                        }
                      , elementAttributes = []
                      , elementNodes =
                        [ NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "pattern" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "enumeration" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "whiteSpace" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "maxInclusive" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "maxExclusive" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "minInclusive" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "minExclusive" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasProperty"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "value"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "partial" ]
                               )
                              ,
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "ordered" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasProperty"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "value"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "false" ]
                               )
                              ,
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "bounded" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasProperty"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "value"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "countably infinite" ]
                               )
                              ,
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "cardinality" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasProperty"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "value"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "false" ]
                               )
                              ,
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "numeric" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        ]
                      }
                    )
                  , NodeContent ( ContentText "" )
                  , NodeElement
                    ( Element
                      { elementName = Name
                        { nameLocalName = "documentation"
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                        , namePrefix = Just "xs"
                        }
                      , elementAttributes =
                        [
                         ( Name
                           { nameLocalName = "source"
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           }
                         , [ ContentText "http://www.w3.org/TR/xmlschema-2/#dateTime" ]
                         )
                        ]
                      , elementNodes = []
                      }
                    )
                  , NodeContent ( ContentText "" )
                  ]
                }
              )
            , NodeContent ( ContentText "" )
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
                   , [ ContentText "xs:anySimpleType" ]
                   )
                  ]
                , elementNodes =
                  [ NodeContent ( ContentText "" )
                  , NodeElement
                    ( Element
                      { elementName = Name
                        { nameLocalName = "whiteSpace"
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                        , namePrefix = Just "xs"
                        }
                      , elementAttributes =
                        [
                         ( Name
                           { nameLocalName = "id"
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           }
                         , [ ContentText "dateTime.whiteSpace" ]
                         )
                        ,
                         ( Name
                           { nameLocalName = "value"
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           }
                         , [ ContentText "collapse" ]
                         )
                        ,
                         ( Name
                           { nameLocalName = "fixed"
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           }
                         , [ ContentText "true" ]
                         )
                        ]
                      , elementNodes = []
                      }
                    )
                  , NodeContent ( ContentText "" )
                  ]
                }
              )
            , NodeContent ( ContentText "" )
            ]
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
               { nameLocalName = "id"
               , nameNamespace = Nothing
               , namePrefix = Nothing
               }
             , [ ContentText "time" ]
             )
            ,
             ( Name
               { nameLocalName = "name"
               , nameNamespace = Nothing
               , namePrefix = Nothing
               }
             , [ ContentText "time" ]
             )
            ]
          , elementNodes =
            [ NodeContent ( ContentText "" )
            , NodeElement
              ( Element
                { elementName = Name
                  { nameLocalName = "annotation"
                  , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                  , namePrefix = Just "xs"
                  }
                , elementAttributes = []
                , elementNodes =
                  [ NodeContent ( ContentText "" )
                  , NodeElement
                    ( Element
                      { elementName = Name
                        { nameLocalName = "appinfo"
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                        , namePrefix = Just "xs"
                        }
                      , elementAttributes = []
                      , elementNodes =
                        [ NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "pattern" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "enumeration" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "whiteSpace" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "maxInclusive" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "maxExclusive" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "minInclusive" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "minExclusive" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasProperty"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "value"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "partial" ]
                               )
                              ,
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "ordered" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasProperty"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "value"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "false" ]
                               )
                              ,
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "bounded" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasProperty"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "value"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "countably infinite" ]
                               )
                              ,
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "cardinality" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasProperty"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "value"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "false" ]
                               )
                              ,
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "numeric" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        ]
                      }
                    )
                  , NodeContent ( ContentText "" )
                  , NodeElement
                    ( Element
                      { elementName = Name
                        { nameLocalName = "documentation"
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                        , namePrefix = Just "xs"
                        }
                      , elementAttributes =
                        [
                         ( Name
                           { nameLocalName = "source"
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           }
                         , [ ContentText "http://www.w3.org/TR/xmlschema-2/#time" ]
                         )
                        ]
                      , elementNodes = []
                      }
                    )
                  , NodeContent ( ContentText "" )
                  ]
                }
              )
            , NodeContent ( ContentText "" )
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
                   , [ ContentText "xs:anySimpleType" ]
                   )
                  ]
                , elementNodes =
                  [ NodeContent ( ContentText "" )
                  , NodeElement
                    ( Element
                      { elementName = Name
                        { nameLocalName = "whiteSpace"
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                        , namePrefix = Just "xs"
                        }
                      , elementAttributes =
                        [
                         ( Name
                           { nameLocalName = "id"
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           }
                         , [ ContentText "time.whiteSpace" ]
                         )
                        ,
                         ( Name
                           { nameLocalName = "value"
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           }
                         , [ ContentText "collapse" ]
                         )
                        ,
                         ( Name
                           { nameLocalName = "fixed"
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           }
                         , [ ContentText "true" ]
                         )
                        ]
                      , elementNodes = []
                      }
                    )
                  , NodeContent ( ContentText "" )
                  ]
                }
              )
            , NodeContent ( ContentText "" )
            ]
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
               { nameLocalName = "id"
               , nameNamespace = Nothing
               , namePrefix = Nothing
               }
             , [ ContentText "date" ]
             )
            ,
             ( Name
               { nameLocalName = "name"
               , nameNamespace = Nothing
               , namePrefix = Nothing
               }
             , [ ContentText "date" ]
             )
            ]
          , elementNodes =
            [ NodeContent ( ContentText "" )
            , NodeElement
              ( Element
                { elementName = Name
                  { nameLocalName = "annotation"
                  , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                  , namePrefix = Just "xs"
                  }
                , elementAttributes = []
                , elementNodes =
                  [ NodeContent ( ContentText "" )
                  , NodeElement
                    ( Element
                      { elementName = Name
                        { nameLocalName = "appinfo"
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                        , namePrefix = Just "xs"
                        }
                      , elementAttributes = []
                      , elementNodes =
                        [ NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "pattern" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "enumeration" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "whiteSpace" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "maxInclusive" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "maxExclusive" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "minInclusive" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "minExclusive" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasProperty"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "value"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "partial" ]
                               )
                              ,
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "ordered" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasProperty"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "value"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "false" ]
                               )
                              ,
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "bounded" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasProperty"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "value"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "countably infinite" ]
                               )
                              ,
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "cardinality" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasProperty"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "value"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "false" ]
                               )
                              ,
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "numeric" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        ]
                      }
                    )
                  , NodeContent ( ContentText "" )
                  , NodeElement
                    ( Element
                      { elementName = Name
                        { nameLocalName = "documentation"
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                        , namePrefix = Just "xs"
                        }
                      , elementAttributes =
                        [
                         ( Name
                           { nameLocalName = "source"
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           }
                         , [ ContentText "http://www.w3.org/TR/xmlschema-2/#date" ]
                         )
                        ]
                      , elementNodes = []
                      }
                    )
                  , NodeContent ( ContentText "" )
                  ]
                }
              )
            , NodeContent ( ContentText "" )
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
                   , [ ContentText "xs:anySimpleType" ]
                   )
                  ]
                , elementNodes =
                  [ NodeContent ( ContentText "" )
                  , NodeElement
                    ( Element
                      { elementName = Name
                        { nameLocalName = "whiteSpace"
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                        , namePrefix = Just "xs"
                        }
                      , elementAttributes =
                        [
                         ( Name
                           { nameLocalName = "id"
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           }
                         , [ ContentText "date.whiteSpace" ]
                         )
                        ,
                         ( Name
                           { nameLocalName = "value"
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           }
                         , [ ContentText "collapse" ]
                         )
                        ,
                         ( Name
                           { nameLocalName = "fixed"
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           }
                         , [ ContentText "true" ]
                         )
                        ]
                      , elementNodes = []
                      }
                    )
                  , NodeContent ( ContentText "" )
                  ]
                }
              )
            , NodeContent ( ContentText "" )
            ]
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
               { nameLocalName = "id"
               , nameNamespace = Nothing
               , namePrefix = Nothing
               }
             , [ ContentText "gYearMonth" ]
             )
            ,
             ( Name
               { nameLocalName = "name"
               , nameNamespace = Nothing
               , namePrefix = Nothing
               }
             , [ ContentText "gYearMonth" ]
             )
            ]
          , elementNodes =
            [ NodeContent ( ContentText "" )
            , NodeElement
              ( Element
                { elementName = Name
                  { nameLocalName = "annotation"
                  , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                  , namePrefix = Just "xs"
                  }
                , elementAttributes = []
                , elementNodes =
                  [ NodeContent ( ContentText "" )
                  , NodeElement
                    ( Element
                      { elementName = Name
                        { nameLocalName = "appinfo"
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                        , namePrefix = Just "xs"
                        }
                      , elementAttributes = []
                      , elementNodes =
                        [ NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "pattern" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "enumeration" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "whiteSpace" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "maxInclusive" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "maxExclusive" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "minInclusive" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "minExclusive" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasProperty"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "value"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "partial" ]
                               )
                              ,
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "ordered" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasProperty"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "value"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "false" ]
                               )
                              ,
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "bounded" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasProperty"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "value"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "countably infinite" ]
                               )
                              ,
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "cardinality" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasProperty"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "value"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "false" ]
                               )
                              ,
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "numeric" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        ]
                      }
                    )
                  , NodeContent ( ContentText "" )
                  , NodeElement
                    ( Element
                      { elementName = Name
                        { nameLocalName = "documentation"
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                        , namePrefix = Just "xs"
                        }
                      , elementAttributes =
                        [
                         ( Name
                           { nameLocalName = "source"
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           }
                         , [ ContentText "http://www.w3.org/TR/xmlschema-2/#gYearMonth" ]
                         )
                        ]
                      , elementNodes = []
                      }
                    )
                  , NodeContent ( ContentText "" )
                  ]
                }
              )
            , NodeContent ( ContentText "" )
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
                   , [ ContentText "xs:anySimpleType" ]
                   )
                  ]
                , elementNodes =
                  [ NodeContent ( ContentText "" )
                  , NodeElement
                    ( Element
                      { elementName = Name
                        { nameLocalName = "whiteSpace"
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                        , namePrefix = Just "xs"
                        }
                      , elementAttributes =
                        [
                         ( Name
                           { nameLocalName = "id"
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           }
                         , [ ContentText "gYearMonth.whiteSpace" ]
                         )
                        ,
                         ( Name
                           { nameLocalName = "value"
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           }
                         , [ ContentText "collapse" ]
                         )
                        ,
                         ( Name
                           { nameLocalName = "fixed"
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           }
                         , [ ContentText "true" ]
                         )
                        ]
                      , elementNodes = []
                      }
                    )
                  , NodeContent ( ContentText "" )
                  ]
                }
              )
            , NodeContent ( ContentText "" )
            ]
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
               { nameLocalName = "id"
               , nameNamespace = Nothing
               , namePrefix = Nothing
               }
             , [ ContentText "gYear" ]
             )
            ,
             ( Name
               { nameLocalName = "name"
               , nameNamespace = Nothing
               , namePrefix = Nothing
               }
             , [ ContentText "gYear" ]
             )
            ]
          , elementNodes =
            [ NodeContent ( ContentText "" )
            , NodeElement
              ( Element
                { elementName = Name
                  { nameLocalName = "annotation"
                  , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                  , namePrefix = Just "xs"
                  }
                , elementAttributes = []
                , elementNodes =
                  [ NodeContent ( ContentText "" )
                  , NodeElement
                    ( Element
                      { elementName = Name
                        { nameLocalName = "appinfo"
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                        , namePrefix = Just "xs"
                        }
                      , elementAttributes = []
                      , elementNodes =
                        [ NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "pattern" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "enumeration" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "whiteSpace" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "maxInclusive" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "maxExclusive" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "minInclusive" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "minExclusive" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasProperty"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "value"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "partial" ]
                               )
                              ,
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "ordered" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasProperty"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "value"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "false" ]
                               )
                              ,
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "bounded" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasProperty"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "value"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "countably infinite" ]
                               )
                              ,
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "cardinality" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasProperty"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "value"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "false" ]
                               )
                              ,
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "numeric" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        ]
                      }
                    )
                  , NodeContent ( ContentText "" )
                  , NodeElement
                    ( Element
                      { elementName = Name
                        { nameLocalName = "documentation"
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                        , namePrefix = Just "xs"
                        }
                      , elementAttributes =
                        [
                         ( Name
                           { nameLocalName = "source"
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           }
                         , [ ContentText "http://www.w3.org/TR/xmlschema-2/#gYear" ]
                         )
                        ]
                      , elementNodes = []
                      }
                    )
                  , NodeContent ( ContentText "" )
                  ]
                }
              )
            , NodeContent ( ContentText "" )
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
                   , [ ContentText "xs:anySimpleType" ]
                   )
                  ]
                , elementNodes =
                  [ NodeContent ( ContentText "" )
                  , NodeElement
                    ( Element
                      { elementName = Name
                        { nameLocalName = "whiteSpace"
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                        , namePrefix = Just "xs"
                        }
                      , elementAttributes =
                        [
                         ( Name
                           { nameLocalName = "id"
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           }
                         , [ ContentText "gYear.whiteSpace" ]
                         )
                        ,
                         ( Name
                           { nameLocalName = "value"
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           }
                         , [ ContentText "collapse" ]
                         )
                        ,
                         ( Name
                           { nameLocalName = "fixed"
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           }
                         , [ ContentText "true" ]
                         )
                        ]
                      , elementNodes = []
                      }
                    )
                  , NodeContent ( ContentText "" )
                  ]
                }
              )
            , NodeContent ( ContentText "" )
            ]
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
               { nameLocalName = "id"
               , nameNamespace = Nothing
               , namePrefix = Nothing
               }
             , [ ContentText "gMonthDay" ]
             )
            ,
             ( Name
               { nameLocalName = "name"
               , nameNamespace = Nothing
               , namePrefix = Nothing
               }
             , [ ContentText "gMonthDay" ]
             )
            ]
          , elementNodes =
            [ NodeContent ( ContentText "" )
            , NodeElement
              ( Element
                { elementName = Name
                  { nameLocalName = "annotation"
                  , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                  , namePrefix = Just "xs"
                  }
                , elementAttributes = []
                , elementNodes =
                  [ NodeContent ( ContentText "" )
                  , NodeElement
                    ( Element
                      { elementName = Name
                        { nameLocalName = "appinfo"
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                        , namePrefix = Just "xs"
                        }
                      , elementAttributes = []
                      , elementNodes =
                        [ NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "pattern" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "enumeration" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "whiteSpace" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "maxInclusive" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "maxExclusive" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "minInclusive" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "minExclusive" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasProperty"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "value"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "partial" ]
                               )
                              ,
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "ordered" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasProperty"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "value"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "false" ]
                               )
                              ,
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "bounded" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasProperty"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "value"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "countably infinite" ]
                               )
                              ,
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "cardinality" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasProperty"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "value"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "false" ]
                               )
                              ,
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "numeric" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        ]
                      }
                    )
                  , NodeContent ( ContentText "" )
                  , NodeElement
                    ( Element
                      { elementName = Name
                        { nameLocalName = "documentation"
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                        , namePrefix = Just "xs"
                        }
                      , elementAttributes =
                        [
                         ( Name
                           { nameLocalName = "source"
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           }
                         , [ ContentText "http://www.w3.org/TR/xmlschema-2/#gMonthDay" ]
                         )
                        ]
                      , elementNodes = []
                      }
                    )
                  , NodeContent ( ContentText "" )
                  ]
                }
              )
            , NodeContent ( ContentText "" )
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
                   , [ ContentText "xs:anySimpleType" ]
                   )
                  ]
                , elementNodes =
                  [ NodeContent ( ContentText "" )
                  , NodeElement
                    ( Element
                      { elementName = Name
                        { nameLocalName = "whiteSpace"
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                        , namePrefix = Just "xs"
                        }
                      , elementAttributes =
                        [
                         ( Name
                           { nameLocalName = "id"
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           }
                         , [ ContentText "gMonthDay.whiteSpace" ]
                         )
                        ,
                         ( Name
                           { nameLocalName = "value"
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           }
                         , [ ContentText "collapse" ]
                         )
                        ,
                         ( Name
                           { nameLocalName = "fixed"
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           }
                         , [ ContentText "true" ]
                         )
                        ]
                      , elementNodes = []
                      }
                    )
                  , NodeContent ( ContentText "" )
                  ]
                }
              )
            , NodeContent ( ContentText "" )
            ]
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
               { nameLocalName = "id"
               , nameNamespace = Nothing
               , namePrefix = Nothing
               }
             , [ ContentText "gDay" ]
             )
            ,
             ( Name
               { nameLocalName = "name"
               , nameNamespace = Nothing
               , namePrefix = Nothing
               }
             , [ ContentText "gDay" ]
             )
            ]
          , elementNodes =
            [ NodeContent ( ContentText "" )
            , NodeElement
              ( Element
                { elementName = Name
                  { nameLocalName = "annotation"
                  , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                  , namePrefix = Just "xs"
                  }
                , elementAttributes = []
                , elementNodes =
                  [ NodeContent ( ContentText "" )
                  , NodeElement
                    ( Element
                      { elementName = Name
                        { nameLocalName = "appinfo"
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                        , namePrefix = Just "xs"
                        }
                      , elementAttributes = []
                      , elementNodes =
                        [ NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "pattern" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "enumeration" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "whiteSpace" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "maxInclusive" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "maxExclusive" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "minInclusive" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "minExclusive" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasProperty"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "value"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "partial" ]
                               )
                              ,
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "ordered" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasProperty"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "value"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "false" ]
                               )
                              ,
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "bounded" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasProperty"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "value"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "countably infinite" ]
                               )
                              ,
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "cardinality" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasProperty"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "value"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "false" ]
                               )
                              ,
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "numeric" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        ]
                      }
                    )
                  , NodeContent ( ContentText "" )
                  , NodeElement
                    ( Element
                      { elementName = Name
                        { nameLocalName = "documentation"
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                        , namePrefix = Just "xs"
                        }
                      , elementAttributes =
                        [
                         ( Name
                           { nameLocalName = "source"
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           }
                         , [ ContentText "http://www.w3.org/TR/xmlschema-2/#gDay" ]
                         )
                        ]
                      , elementNodes = []
                      }
                    )
                  , NodeContent ( ContentText "" )
                  ]
                }
              )
            , NodeContent ( ContentText "" )
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
                   , [ ContentText "xs:anySimpleType" ]
                   )
                  ]
                , elementNodes =
                  [ NodeContent ( ContentText "" )
                  , NodeElement
                    ( Element
                      { elementName = Name
                        { nameLocalName = "whiteSpace"
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                        , namePrefix = Just "xs"
                        }
                      , elementAttributes =
                        [
                         ( Name
                           { nameLocalName = "id"
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           }
                         , [ ContentText "gDay.whiteSpace" ]
                         )
                        ,
                         ( Name
                           { nameLocalName = "value"
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           }
                         , [ ContentText "collapse" ]
                         )
                        ,
                         ( Name
                           { nameLocalName = "fixed"
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           }
                         , [ ContentText "true" ]
                         )
                        ]
                      , elementNodes = []
                      }
                    )
                  , NodeContent ( ContentText "" )
                  ]
                }
              )
            , NodeContent ( ContentText "" )
            ]
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
               { nameLocalName = "id"
               , nameNamespace = Nothing
               , namePrefix = Nothing
               }
             , [ ContentText "gMonth" ]
             )
            ,
             ( Name
               { nameLocalName = "name"
               , nameNamespace = Nothing
               , namePrefix = Nothing
               }
             , [ ContentText "gMonth" ]
             )
            ]
          , elementNodes =
            [ NodeContent ( ContentText "" )
            , NodeElement
              ( Element
                { elementName = Name
                  { nameLocalName = "annotation"
                  , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                  , namePrefix = Just "xs"
                  }
                , elementAttributes = []
                , elementNodes =
                  [ NodeContent ( ContentText "" )
                  , NodeElement
                    ( Element
                      { elementName = Name
                        { nameLocalName = "appinfo"
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                        , namePrefix = Just "xs"
                        }
                      , elementAttributes = []
                      , elementNodes =
                        [ NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "pattern" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "enumeration" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "whiteSpace" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "maxInclusive" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "maxExclusive" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "minInclusive" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "minExclusive" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasProperty"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "value"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "partial" ]
                               )
                              ,
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "ordered" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasProperty"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "value"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "false" ]
                               )
                              ,
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "bounded" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasProperty"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "value"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "countably infinite" ]
                               )
                              ,
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "cardinality" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasProperty"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "value"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "false" ]
                               )
                              ,
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "numeric" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        ]
                      }
                    )
                  , NodeContent ( ContentText "" )
                  , NodeElement
                    ( Element
                      { elementName = Name
                        { nameLocalName = "documentation"
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                        , namePrefix = Just "xs"
                        }
                      , elementAttributes =
                        [
                         ( Name
                           { nameLocalName = "source"
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           }
                         , [ ContentText "http://www.w3.org/TR/xmlschema-2/#gMonth" ]
                         )
                        ]
                      , elementNodes = []
                      }
                    )
                  , NodeContent ( ContentText "" )
                  ]
                }
              )
            , NodeContent ( ContentText "" )
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
                   , [ ContentText "xs:anySimpleType" ]
                   )
                  ]
                , elementNodes =
                  [ NodeContent ( ContentText "" )
                  , NodeElement
                    ( Element
                      { elementName = Name
                        { nameLocalName = "whiteSpace"
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                        , namePrefix = Just "xs"
                        }
                      , elementAttributes =
                        [
                         ( Name
                           { nameLocalName = "id"
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           }
                         , [ ContentText "gMonth.whiteSpace" ]
                         )
                        ,
                         ( Name
                           { nameLocalName = "value"
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           }
                         , [ ContentText "collapse" ]
                         )
                        ,
                         ( Name
                           { nameLocalName = "fixed"
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           }
                         , [ ContentText "true" ]
                         )
                        ]
                      , elementNodes = []
                      }
                    )
                  , NodeContent ( ContentText "" )
                  ]
                }
              )
            , NodeContent ( ContentText "" )
            ]
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
               { nameLocalName = "id"
               , nameNamespace = Nothing
               , namePrefix = Nothing
               }
             , [ ContentText "hexBinary" ]
             )
            ,
             ( Name
               { nameLocalName = "name"
               , nameNamespace = Nothing
               , namePrefix = Nothing
               }
             , [ ContentText "hexBinary" ]
             )
            ]
          , elementNodes =
            [ NodeContent ( ContentText "" )
            , NodeElement
              ( Element
                { elementName = Name
                  { nameLocalName = "annotation"
                  , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                  , namePrefix = Just "xs"
                  }
                , elementAttributes = []
                , elementNodes =
                  [ NodeContent ( ContentText "" )
                  , NodeElement
                    ( Element
                      { elementName = Name
                        { nameLocalName = "appinfo"
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                        , namePrefix = Just "xs"
                        }
                      , elementAttributes = []
                      , elementNodes =
                        [ NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "length" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "minLength" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "maxLength" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "pattern" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "enumeration" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "whiteSpace" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasProperty"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "value"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "false" ]
                               )
                              ,
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "ordered" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasProperty"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "value"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "false" ]
                               )
                              ,
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "bounded" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasProperty"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "value"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "countably infinite" ]
                               )
                              ,
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "cardinality" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasProperty"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "value"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "false" ]
                               )
                              ,
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "numeric" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        ]
                      }
                    )
                  , NodeContent ( ContentText "" )
                  , NodeElement
                    ( Element
                      { elementName = Name
                        { nameLocalName = "documentation"
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                        , namePrefix = Just "xs"
                        }
                      , elementAttributes =
                        [
                         ( Name
                           { nameLocalName = "source"
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           }
                         , [ ContentText "http://www.w3.org/TR/xmlschema-2/#binary" ]
                         )
                        ]
                      , elementNodes = []
                      }
                    )
                  , NodeContent ( ContentText "" )
                  ]
                }
              )
            , NodeContent ( ContentText "" )
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
                   , [ ContentText "xs:anySimpleType" ]
                   )
                  ]
                , elementNodes =
                  [ NodeContent ( ContentText "" )
                  , NodeElement
                    ( Element
                      { elementName = Name
                        { nameLocalName = "whiteSpace"
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                        , namePrefix = Just "xs"
                        }
                      , elementAttributes =
                        [
                         ( Name
                           { nameLocalName = "id"
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           }
                         , [ ContentText "hexBinary.whiteSpace" ]
                         )
                        ,
                         ( Name
                           { nameLocalName = "value"
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           }
                         , [ ContentText "collapse" ]
                         )
                        ,
                         ( Name
                           { nameLocalName = "fixed"
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           }
                         , [ ContentText "true" ]
                         )
                        ]
                      , elementNodes = []
                      }
                    )
                  , NodeContent ( ContentText "" )
                  ]
                }
              )
            , NodeContent ( ContentText "" )
            ]
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
               { nameLocalName = "id"
               , nameNamespace = Nothing
               , namePrefix = Nothing
               }
             , [ ContentText "base64Binary" ]
             )
            ,
             ( Name
               { nameLocalName = "name"
               , nameNamespace = Nothing
               , namePrefix = Nothing
               }
             , [ ContentText "base64Binary" ]
             )
            ]
          , elementNodes =
            [ NodeContent ( ContentText "" )
            , NodeElement
              ( Element
                { elementName = Name
                  { nameLocalName = "annotation"
                  , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                  , namePrefix = Just "xs"
                  }
                , elementAttributes = []
                , elementNodes =
                  [ NodeContent ( ContentText "" )
                  , NodeElement
                    ( Element
                      { elementName = Name
                        { nameLocalName = "appinfo"
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                        , namePrefix = Just "xs"
                        }
                      , elementAttributes = []
                      , elementNodes =
                        [ NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "length" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "minLength" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "maxLength" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "pattern" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "enumeration" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "whiteSpace" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasProperty"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "value"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "false" ]
                               )
                              ,
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "ordered" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasProperty"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "value"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "false" ]
                               )
                              ,
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "bounded" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasProperty"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "value"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "countably infinite" ]
                               )
                              ,
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "cardinality" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasProperty"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "value"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "false" ]
                               )
                              ,
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "numeric" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        ]
                      }
                    )
                  , NodeContent ( ContentText "" )
                  , NodeElement
                    ( Element
                      { elementName = Name
                        { nameLocalName = "documentation"
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                        , namePrefix = Just "xs"
                        }
                      , elementAttributes =
                        [
                         ( Name
                           { nameLocalName = "source"
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           }
                         , [ ContentText "http://www.w3.org/TR/xmlschema-2/#base64Binary" ]
                         )
                        ]
                      , elementNodes = []
                      }
                    )
                  , NodeContent ( ContentText "" )
                  ]
                }
              )
            , NodeContent ( ContentText "" )
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
                   , [ ContentText "xs:anySimpleType" ]
                   )
                  ]
                , elementNodes =
                  [ NodeContent ( ContentText "" )
                  , NodeElement
                    ( Element
                      { elementName = Name
                        { nameLocalName = "whiteSpace"
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                        , namePrefix = Just "xs"
                        }
                      , elementAttributes =
                        [
                         ( Name
                           { nameLocalName = "id"
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           }
                         , [ ContentText "base64Binary.whiteSpace" ]
                         )
                        ,
                         ( Name
                           { nameLocalName = "value"
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           }
                         , [ ContentText "collapse" ]
                         )
                        ,
                         ( Name
                           { nameLocalName = "fixed"
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           }
                         , [ ContentText "true" ]
                         )
                        ]
                      , elementNodes = []
                      }
                    )
                  , NodeContent ( ContentText "" )
                  ]
                }
              )
            , NodeContent ( ContentText "" )
            ]
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
               { nameLocalName = "id"
               , nameNamespace = Nothing
               , namePrefix = Nothing
               }
             , [ ContentText "anyURI" ]
             )
            ,
             ( Name
               { nameLocalName = "name"
               , nameNamespace = Nothing
               , namePrefix = Nothing
               }
             , [ ContentText "anyURI" ]
             )
            ]
          , elementNodes =
            [ NodeContent ( ContentText "" )
            , NodeElement
              ( Element
                { elementName = Name
                  { nameLocalName = "annotation"
                  , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                  , namePrefix = Just "xs"
                  }
                , elementAttributes = []
                , elementNodes =
                  [ NodeContent ( ContentText "" )
                  , NodeElement
                    ( Element
                      { elementName = Name
                        { nameLocalName = "appinfo"
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                        , namePrefix = Just "xs"
                        }
                      , elementAttributes = []
                      , elementNodes =
                        [ NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "length" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "minLength" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "maxLength" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "pattern" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "enumeration" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "whiteSpace" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasProperty"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "value"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "false" ]
                               )
                              ,
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "ordered" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasProperty"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "value"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "false" ]
                               )
                              ,
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "bounded" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasProperty"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "value"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "countably infinite" ]
                               )
                              ,
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "cardinality" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasProperty"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "value"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "false" ]
                               )
                              ,
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "numeric" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        ]
                      }
                    )
                  , NodeContent ( ContentText "" )
                  , NodeElement
                    ( Element
                      { elementName = Name
                        { nameLocalName = "documentation"
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                        , namePrefix = Just "xs"
                        }
                      , elementAttributes =
                        [
                         ( Name
                           { nameLocalName = "source"
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           }
                         , [ ContentText "http://www.w3.org/TR/xmlschema-2/#anyURI" ]
                         )
                        ]
                      , elementNodes = []
                      }
                    )
                  , NodeContent ( ContentText "" )
                  ]
                }
              )
            , NodeContent ( ContentText "" )
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
                   , [ ContentText "xs:anySimpleType" ]
                   )
                  ]
                , elementNodes =
                  [ NodeContent ( ContentText "" )
                  , NodeElement
                    ( Element
                      { elementName = Name
                        { nameLocalName = "whiteSpace"
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                        , namePrefix = Just "xs"
                        }
                      , elementAttributes =
                        [
                         ( Name
                           { nameLocalName = "id"
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           }
                         , [ ContentText "anyURI.whiteSpace" ]
                         )
                        ,
                         ( Name
                           { nameLocalName = "value"
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           }
                         , [ ContentText "collapse" ]
                         )
                        ,
                         ( Name
                           { nameLocalName = "fixed"
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           }
                         , [ ContentText "true" ]
                         )
                        ]
                      , elementNodes = []
                      }
                    )
                  , NodeContent ( ContentText "" )
                  ]
                }
              )
            , NodeContent ( ContentText "" )
            ]
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
               { nameLocalName = "id"
               , nameNamespace = Nothing
               , namePrefix = Nothing
               }
             , [ ContentText "QName" ]
             )
            ,
             ( Name
               { nameLocalName = "name"
               , nameNamespace = Nothing
               , namePrefix = Nothing
               }
             , [ ContentText "QName" ]
             )
            ]
          , elementNodes =
            [ NodeContent ( ContentText "" )
            , NodeElement
              ( Element
                { elementName = Name
                  { nameLocalName = "annotation"
                  , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                  , namePrefix = Just "xs"
                  }
                , elementAttributes = []
                , elementNodes =
                  [ NodeContent ( ContentText "" )
                  , NodeElement
                    ( Element
                      { elementName = Name
                        { nameLocalName = "appinfo"
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                        , namePrefix = Just "xs"
                        }
                      , elementAttributes = []
                      , elementNodes =
                        [ NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "length" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "minLength" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "maxLength" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "pattern" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "enumeration" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "whiteSpace" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasProperty"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "value"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "false" ]
                               )
                              ,
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "ordered" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasProperty"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "value"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "false" ]
                               )
                              ,
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "bounded" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasProperty"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "value"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "countably infinite" ]
                               )
                              ,
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "cardinality" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasProperty"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "value"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "false" ]
                               )
                              ,
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "numeric" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        ]
                      }
                    )
                  , NodeContent ( ContentText "" )
                  , NodeElement
                    ( Element
                      { elementName = Name
                        { nameLocalName = "documentation"
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                        , namePrefix = Just "xs"
                        }
                      , elementAttributes =
                        [
                         ( Name
                           { nameLocalName = "source"
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           }
                         , [ ContentText "http://www.w3.org/TR/xmlschema-2/#QName" ]
                         )
                        ]
                      , elementNodes = []
                      }
                    )
                  , NodeContent ( ContentText "" )
                  ]
                }
              )
            , NodeContent ( ContentText "" )
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
                   , [ ContentText "xs:anySimpleType" ]
                   )
                  ]
                , elementNodes =
                  [ NodeContent ( ContentText "" )
                  , NodeElement
                    ( Element
                      { elementName = Name
                        { nameLocalName = "whiteSpace"
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                        , namePrefix = Just "xs"
                        }
                      , elementAttributes =
                        [
                         ( Name
                           { nameLocalName = "id"
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           }
                         , [ ContentText "QName.whiteSpace" ]
                         )
                        ,
                         ( Name
                           { nameLocalName = "value"
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           }
                         , [ ContentText "collapse" ]
                         )
                        ,
                         ( Name
                           { nameLocalName = "fixed"
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           }
                         , [ ContentText "true" ]
                         )
                        ]
                      , elementNodes = []
                      }
                    )
                  , NodeContent ( ContentText "" )
                  ]
                }
              )
            , NodeContent ( ContentText "" )
            ]
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
               { nameLocalName = "id"
               , nameNamespace = Nothing
               , namePrefix = Nothing
               }
             , [ ContentText "NOTATION" ]
             )
            ,
             ( Name
               { nameLocalName = "name"
               , nameNamespace = Nothing
               , namePrefix = Nothing
               }
             , [ ContentText "NOTATION" ]
             )
            ]
          , elementNodes =
            [ NodeContent ( ContentText "" )
            , NodeElement
              ( Element
                { elementName = Name
                  { nameLocalName = "annotation"
                  , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                  , namePrefix = Just "xs"
                  }
                , elementAttributes = []
                , elementNodes =
                  [ NodeContent ( ContentText "" )
                  , NodeElement
                    ( Element
                      { elementName = Name
                        { nameLocalName = "appinfo"
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                        , namePrefix = Just "xs"
                        }
                      , elementAttributes = []
                      , elementNodes =
                        [ NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "length" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "minLength" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "maxLength" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "pattern" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "enumeration" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasFacet"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "whiteSpace" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasProperty"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "value"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "false" ]
                               )
                              ,
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "ordered" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasProperty"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "value"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "false" ]
                               )
                              ,
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "bounded" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasProperty"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "value"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "countably infinite" ]
                               )
                              ,
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "cardinality" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "hasProperty"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                              , namePrefix = Just "hfp"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "value"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "false" ]
                               )
                              ,
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "numeric" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        ]
                      }
                    )
                  , NodeContent ( ContentText "" )
                  , NodeElement
                    ( Element
                      { elementName = Name
                        { nameLocalName = "documentation"
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                        , namePrefix = Just "xs"
                        }
                      , elementAttributes =
                        [
                         ( Name
                           { nameLocalName = "source"
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           }
                         , [ ContentText "http://www.w3.org/TR/xmlschema-2/#NOTATION" ]
                         )
                        ]
                      , elementNodes = []
                      }
                    )
                  , NodeContent ( ContentText "" )
                  , NodeElement
                    ( Element
                      { elementName = Name
                        { nameLocalName = "documentation"
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                        , namePrefix = Just "xs"
                        }
                      , elementAttributes = []
                      , elementNodes =
                        [ NodeContent ( ContentText "NOTATION cannot be used directly in a schema; rather a type"
                                      \ "must be derived from it by specifying at least one enumeration"
                                      \ "facet whose value is the name of a NOTATION declared in the"
                                      \ "schema."
                                      )
                        ]
                      }
                    )
                  , NodeContent ( ContentText "" )
                  ]
                }
              )
            , NodeContent ( ContentText "" )
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
                       , [ ContentText "xs:anySimpleType" ]
                       )
                      ]
                    , elementNodes =
                      [ NodeContent ( ContentText "" )
                      , NodeElement
                        ( Element
                          { elementName = Name
                            { nameLocalName = "whiteSpace"
                            , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                            , namePrefix = Just "xs"
                            }
                          , elementAttributes =
                            [
                             ( Name
                               { nameLocalName = "id"
                               , nameNamespace = Nothing
                               , namePrefix = Nothing
                               }
                             , [ ContentText "NOTATION.whiteSpace" ]
                             )
                            ,
                             ( Name
                               { nameLocalName = "value"
                               , nameNamespace = Nothing
                               , namePrefix = Nothing
                               }
                             , [ ContentText "collapse" ]
                             )
                            ,
                             ( Name
                               { nameLocalName = "fixed"
                               , nameNamespace = Nothing
                               , namePrefix = Nothing
                               }
                             , [ ContentText "true" ]
                             )
                            ]
                          , elementNodes = []
                          }
                        )
                      , NodeContent ( ContentText "" )
                      ]
                    }
                  )
                , NodeContent ( ContentText "" )
                ]
              }
            )
          , NodeContent ( ContentText "" )
          , NodeElement
            ( Element
              { elementName = Name
                { nameLocalName = "annotation"
                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                , namePrefix = Just "xs"
                }
              , elementAttributes = []
              , elementNodes =
                [ NodeContent ( ContentText "" )
                , NodeElement
                  ( Element
                    { elementName = Name
                      { nameLocalName = "documentation"
                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                      , namePrefix = Just "xs"
                      }
                    , elementAttributes = []
                    , elementNodes =
                      [ NodeContent ( ContentText "Now the derived primitive types" ) ]
                    }
                  )
                , NodeContent ( ContentText "" )
                ]
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
                   { nameLocalName = "id"
                   , nameNamespace = Nothing
                   , namePrefix = Nothing
                   }
                 , [ ContentText "normalizedString" ]
                 )
                ,
                 ( Name
                   { nameLocalName = "name"
                   , nameNamespace = Nothing
                   , namePrefix = Nothing
                   }
                 , [ ContentText "normalizedString" ]
                 )
                ]
              , elementNodes =
                [ NodeContent ( ContentText "" )
                , NodeElement
                  ( Element
                    { elementName = Name
                      { nameLocalName = "annotation"
                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                      , namePrefix = Just "xs"
                      }
                    , elementAttributes = []
                    , elementNodes =
                      [ NodeContent ( ContentText "" )
                      , NodeElement
                        ( Element
                          { elementName = Name
                            { nameLocalName = "documentation"
                            , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                            , namePrefix = Just "xs"
                            }
                          , elementAttributes =
                            [
                             ( Name
                               { nameLocalName = "source"
                               , nameNamespace = Nothing
                               , namePrefix = Nothing
                               }
                             , [ ContentText "http://www.w3.org/TR/xmlschema-2/#normalizedString" ]
                             )
                            ]
                          , elementNodes = []
                          }
                        )
                      , NodeContent ( ContentText "" )
                      ]
                    }
                  )
                , NodeContent ( ContentText "" )
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
                       , [ ContentText "xs:string" ]
                       )
                      ]
                    , elementNodes =
                      [ NodeContent ( ContentText "" )
                      , NodeElement
                        ( Element
                          { elementName = Name
                            { nameLocalName = "whiteSpace"
                            , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                            , namePrefix = Just "xs"
                            }
                          , elementAttributes =
                            [
                             ( Name
                               { nameLocalName = "id"
                               , nameNamespace = Nothing
                               , namePrefix = Nothing
                               }
                             , [ ContentText "normalizedString.whiteSpace" ]
                             )
                            ,
                             ( Name
                               { nameLocalName = "value"
                               , nameNamespace = Nothing
                               , namePrefix = Nothing
                               }
                             , [ ContentText "replace" ]
                             )
                            ]
                          , elementNodes = []
                          }
                        )
                      , NodeContent ( ContentText "" )
                      ]
                    }
                  )
                , NodeContent ( ContentText "" )
                ]
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
                   { nameLocalName = "id"
                   , nameNamespace = Nothing
                   , namePrefix = Nothing
                   }
                 , [ ContentText "token" ]
                 )
                ,
                 ( Name
                   { nameLocalName = "name"
                   , nameNamespace = Nothing
                   , namePrefix = Nothing
                   }
                 , [ ContentText "token" ]
                 )
                ]
              , elementNodes =
                [ NodeContent ( ContentText "" )
                , NodeElement
                  ( Element
                    { elementName = Name
                      { nameLocalName = "annotation"
                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                      , namePrefix = Just "xs"
                      }
                    , elementAttributes = []
                    , elementNodes =
                      [ NodeContent ( ContentText "" )
                      , NodeElement
                        ( Element
                          { elementName = Name
                            { nameLocalName = "documentation"
                            , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                            , namePrefix = Just "xs"
                            }
                          , elementAttributes =
                            [
                             ( Name
                               { nameLocalName = "source"
                               , nameNamespace = Nothing
                               , namePrefix = Nothing
                               }
                             , [ ContentText "http://www.w3.org/TR/xmlschema-2/#token" ]
                             )
                            ]
                          , elementNodes = []
                          }
                        )
                      , NodeContent ( ContentText "" )
                      ]
                    }
                  )
                , NodeContent ( ContentText "" )
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
                       , [ ContentText "xs:normalizedString" ]
                       )
                      ]
                    , elementNodes =
                      [ NodeContent ( ContentText "" )
                      , NodeElement
                        ( Element
                          { elementName = Name
                            { nameLocalName = "whiteSpace"
                            , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                            , namePrefix = Just "xs"
                            }
                          , elementAttributes =
                            [
                             ( Name
                               { nameLocalName = "id"
                               , nameNamespace = Nothing
                               , namePrefix = Nothing
                               }
                             , [ ContentText "token.whiteSpace" ]
                             )
                            ,
                             ( Name
                               { nameLocalName = "value"
                               , nameNamespace = Nothing
                               , namePrefix = Nothing
                               }
                             , [ ContentText "collapse" ]
                             )
                            ]
                          , elementNodes = []
                          }
                        )
                      , NodeContent ( ContentText "" )
                      ]
                    }
                  )
                , NodeContent ( ContentText "" )
                ]
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
                   { nameLocalName = "id"
                   , nameNamespace = Nothing
                   , namePrefix = Nothing
                   }
                 , [ ContentText "language" ]
                 )
                ,
                 ( Name
                   { nameLocalName = "name"
                   , nameNamespace = Nothing
                   , namePrefix = Nothing
                   }
                 , [ ContentText "language" ]
                 )
                ]
              , elementNodes =
                [ NodeContent ( ContentText "" )
                , NodeElement
                  ( Element
                    { elementName = Name
                      { nameLocalName = "annotation"
                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                      , namePrefix = Just "xs"
                      }
                    , elementAttributes = []
                    , elementNodes =
                      [ NodeContent ( ContentText "" )
                      , NodeElement
                        ( Element
                          { elementName = Name
                            { nameLocalName = "documentation"
                            , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                            , namePrefix = Just "xs"
                            }
                          , elementAttributes =
                            [
                             ( Name
                               { nameLocalName = "source"
                               , nameNamespace = Nothing
                               , namePrefix = Nothing
                               }
                             , [ ContentText "http://www.w3.org/TR/xmlschema-2/#language" ]
                             )
                            ]
                          , elementNodes = []
                          }
                        )
                      , NodeContent ( ContentText "" )
                      ]
                    }
                  )
                , NodeContent ( ContentText "" )
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
                       , [ ContentText "xs:token" ]
                       )
                      ]
                    , elementNodes =
                      [ NodeContent ( ContentText "" )
                      , NodeElement
                        ( Element
                          { elementName = Name
                            { nameLocalName = "pattern"
                            , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                            , namePrefix = Just "xs"
                            }
                          , elementAttributes =
                            [
                             ( Name
                               { nameLocalName = "id"
                               , nameNamespace = Nothing
                               , namePrefix = Nothing
                               }
                             , [ ContentText "language.pattern" ]
                             )
                            ,
                             ( Name
                               { nameLocalName = "value"
                               , nameNamespace = Nothing
                               , namePrefix = Nothing
                               }
                             , [ ContentText "[a-zA-Z]{1,8}(-[a-zA-Z0-9]{1,8})*" ]
                             )
                            ]
                          , elementNodes =
                            [ NodeContent ( ContentText "" )
                            , NodeElement
                              ( Element
                                { elementName = Name
                                  { nameLocalName = "annotation"
                                  , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                  , namePrefix = Just "xs"
                                  }
                                , elementAttributes = []
                                , elementNodes =
                                  [ NodeContent ( ContentText "" )
                                  , NodeElement
                                    ( Element
                                      { elementName = Name
                                        { nameLocalName = "documentation"
                                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                        , namePrefix = Just "xs"
                                        }
                                      , elementAttributes =
                                        [
                                         ( Name
                                           { nameLocalName = "source"
                                           , nameNamespace = Nothing
                                           , namePrefix = Nothing
                                           }
                                         , [ ContentText "http://www.ietf.org/rfc/rfc3066.txt" ]
                                         )
                                        ]
                                      , elementNodes =
                                        [ NodeContent
                                          ( ContentText
                                            "pattern specifies the content of section 2.12 of XML 1.0e2"
                                          \ "and RFC 3066 (Revised version of RFC 1766)."
                                          )
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
                        )
                      , NodeContent ( ContentText "" )
                      ]
                    }
                  )
                , NodeContent ( ContentText "" )
                ]
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
                       { nameLocalName = "id"
                       , nameNamespace = Nothing
                       , namePrefix = Nothing
                       }
                     , [ ContentText "IDREFS" ]
                     )
                    ,
                     ( Name
                       { nameLocalName = "name"
                       , nameNamespace = Nothing
                       , namePrefix = Nothing
                       }
                     , [ ContentText "IDREFS" ]
                     )
                    ]
                  , elementNodes =
                    [ NodeContent ( ContentText "" )
                    , NodeElement
                      ( Element
                        { elementName = Name
                          { nameLocalName = "annotation"
                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                          , namePrefix = Just "xs"
                          }
                        , elementAttributes = []
                        , elementNodes =
                          [ NodeContent ( ContentText "" )
                          , NodeElement
                            ( Element
                              { elementName = Name
                                { nameLocalName = "appinfo"
                                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                , namePrefix = Just "xs"
                                }
                              , elementAttributes = []
                              , elementNodes =
                                [ NodeContent ( ContentText "" )
                                , NodeElement
                                  ( Element
                                    { elementName = Name
                                      { nameLocalName = "hasFacet"
                                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                                      , namePrefix = Just "hfp"
                                      }
                                    , elementAttributes =
                                      [
                                       ( Name
                                         { nameLocalName = "name"
                                         , nameNamespace = Nothing
                                         , namePrefix = Nothing
                                         }
                                       , [ ContentText "length" ]
                                       )
                                      ]
                                    , elementNodes = []
                                    }
                                  )
                                , NodeContent ( ContentText "" )
                                , NodeElement
                                  ( Element
                                    { elementName = Name
                                      { nameLocalName = "hasFacet"
                                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                                      , namePrefix = Just "hfp"
                                      }
                                    , elementAttributes =
                                      [
                                       ( Name
                                         { nameLocalName = "name"
                                         , nameNamespace = Nothing
                                         , namePrefix = Nothing
                                         }
                                       , [ ContentText "minLength" ]
                                       )
                                      ]
                                    , elementNodes = []
                                    }
                                  )
                                , NodeContent ( ContentText "" )
                                , NodeElement
                                  ( Element
                                    { elementName = Name
                                      { nameLocalName = "hasFacet"
                                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                                      , namePrefix = Just "hfp"
                                      }
                                    , elementAttributes =
                                      [
                                       ( Name
                                         { nameLocalName = "name"
                                         , nameNamespace = Nothing
                                         , namePrefix = Nothing
                                         }
                                       , [ ContentText "maxLength" ]
                                       )
                                      ]
                                    , elementNodes = []
                                    }
                                  )
                                , NodeContent ( ContentText "" )
                                , NodeElement
                                  ( Element
                                    { elementName = Name
                                      { nameLocalName = "hasFacet"
                                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                                      , namePrefix = Just "hfp"
                                      }
                                    , elementAttributes =
                                      [
                                       ( Name
                                         { nameLocalName = "name"
                                         , nameNamespace = Nothing
                                         , namePrefix = Nothing
                                         }
                                       , [ ContentText "enumeration" ]
                                       )
                                      ]
                                    , elementNodes = []
                                    }
                                  )
                                , NodeContent ( ContentText "" )
                                , NodeElement
                                  ( Element
                                    { elementName = Name
                                      { nameLocalName = "hasFacet"
                                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                                      , namePrefix = Just "hfp"
                                      }
                                    , elementAttributes =
                                      [
                                       ( Name
                                         { nameLocalName = "name"
                                         , nameNamespace = Nothing
                                         , namePrefix = Nothing
                                         }
                                       , [ ContentText "whiteSpace" ]
                                       )
                                      ]
                                    , elementNodes = []
                                    }
                                  )
                                , NodeContent ( ContentText "" )
                                , NodeElement
                                  ( Element
                                    { elementName = Name
                                      { nameLocalName = "hasFacet"
                                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                                      , namePrefix = Just "hfp"
                                      }
                                    , elementAttributes =
                                      [
                                       ( Name
                                         { nameLocalName = "name"
                                         , nameNamespace = Nothing
                                         , namePrefix = Nothing
                                         }
                                       , [ ContentText "pattern" ]
                                       )
                                      ]
                                    , elementNodes = []
                                    }
                                  )
                                , NodeContent ( ContentText "" )
                                , NodeElement
                                  ( Element
                                    { elementName = Name
                                      { nameLocalName = "hasProperty"
                                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                                      , namePrefix = Just "hfp"
                                      }
                                    , elementAttributes =
                                      [
                                       ( Name
                                         { nameLocalName = "value"
                                         , nameNamespace = Nothing
                                         , namePrefix = Nothing
                                         }
                                       , [ ContentText "false" ]
                                       )
                                      ,
                                       ( Name
                                         { nameLocalName = "name"
                                         , nameNamespace = Nothing
                                         , namePrefix = Nothing
                                         }
                                       , [ ContentText "ordered" ]
                                       )
                                      ]
                                    , elementNodes = []
                                    }
                                  )
                                , NodeContent ( ContentText "" )
                                , NodeElement
                                  ( Element
                                    { elementName = Name
                                      { nameLocalName = "hasProperty"
                                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                                      , namePrefix = Just "hfp"
                                      }
                                    , elementAttributes =
                                      [
                                       ( Name
                                         { nameLocalName = "value"
                                         , nameNamespace = Nothing
                                         , namePrefix = Nothing
                                         }
                                       , [ ContentText "false" ]
                                       )
                                      ,
                                       ( Name
                                         { nameLocalName = "name"
                                         , nameNamespace = Nothing
                                         , namePrefix = Nothing
                                         }
                                       , [ ContentText "bounded" ]
                                       )
                                      ]
                                    , elementNodes = []
                                    }
                                  )
                                , NodeContent ( ContentText "" )
                                , NodeElement
                                  ( Element
                                    { elementName = Name
                                      { nameLocalName = "hasProperty"
                                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                                      , namePrefix = Just "hfp"
                                      }
                                    , elementAttributes =
                                      [
                                       ( Name
                                         { nameLocalName = "value"
                                         , nameNamespace = Nothing
                                         , namePrefix = Nothing
                                         }
                                       , [ ContentText "countably infinite" ]
                                       )
                                      ,
                                       ( Name
                                         { nameLocalName = "name"
                                         , nameNamespace = Nothing
                                         , namePrefix = Nothing
                                         }
                                       , [ ContentText "cardinality" ]
                                       )
                                      ]
                                    , elementNodes = []
                                    }
                                  )
                                , NodeContent ( ContentText "" )
                                , NodeElement
                                  ( Element
                                    { elementName = Name
                                      { nameLocalName = "hasProperty"
                                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                                      , namePrefix = Just "hfp"
                                      }
                                    , elementAttributes =
                                      [
                                       ( Name
                                         { nameLocalName = "value"
                                         , nameNamespace = Nothing
                                         , namePrefix = Nothing
                                         }
                                       , [ ContentText "false" ]
                                       )
                                      ,
                                       ( Name
                                         { nameLocalName = "name"
                                         , nameNamespace = Nothing
                                         , namePrefix = Nothing
                                         }
                                       , [ ContentText "numeric" ]
                                       )
                                      ]
                                    , elementNodes = []
                                    }
                                  )
                                , NodeContent ( ContentText "" )
                                ]
                              }
                            )
                          , NodeContent ( ContentText "" )
                          , NodeElement
                            ( Element
                              { elementName = Name
                                { nameLocalName = "documentation"
                                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                , namePrefix = Just "xs"
                                }
                              , elementAttributes =
                                [
                                 ( Name
                                   { nameLocalName = "source"
                                   , nameNamespace = Nothing
                                   , namePrefix = Nothing
                                   }
                                 , [ ContentText "http://www.w3.org/TR/xmlschema-2/#IDREFS" ]
                                 )
                                ]
                              , elementNodes = []
                              }
                            )
                          , NodeContent ( ContentText "" )
                          ]
                        }
                      )
                    , NodeContent ( ContentText "" )
                    , NodeElement
                      ( Element
                        { elementName = Name
                          { nameLocalName = "restriction"
                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                          , namePrefix = Just "xs"
                          }
                        , elementAttributes = []
                        , elementNodes =
                          [ NodeContent ( ContentText "" )
                          , NodeElement
                            ( Element
                              { elementName = Name
                                { nameLocalName = "simpleType"
                                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                , namePrefix = Just "xs"
                                }
                              , elementAttributes = []
                              , elementNodes =
                                [ NodeContent ( ContentText "" )
                                , NodeElement
                                  ( Element
                                    { elementName = Name
                                      { nameLocalName = "list"
                                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                      , namePrefix = Just "xs"
                                      }
                                    , elementAttributes =
                                      [
                                       ( Name
                                         { nameLocalName = "itemType"
                                         , nameNamespace = Nothing
                                         , namePrefix = Nothing
                                         }
                                       , [ ContentText "xs:IDREF" ]
                                       )
                                      ]
                                    , elementNodes = []
                                    }
                                  )
                                , NodeContent ( ContentText "" )
                                ]
                              }
                            )
                          , NodeContent ( ContentText "" )
                          , NodeElement
                            ( Element
                              { elementName = Name
                                { nameLocalName = "minLength"
                                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                , namePrefix = Just "xs"
                                }
                              , elementAttributes =
                                [
                                 ( Name
                                   { nameLocalName = "id"
                                   , nameNamespace = Nothing
                                   , namePrefix = Nothing
                                   }
                                 , [ ContentText "IDREFS.minLength" ]
                                 )
                                ,
                                 ( Name
                                   { nameLocalName = "value"
                                   , nameNamespace = Nothing
                                   , namePrefix = Nothing
                                   }
                                 , [ ContentText "1" ]
                                 )
                                ]
                              , elementNodes = []
                              }
                            )
                          , NodeContent ( ContentText "" )
                          ]
                        }
                      )
                    , NodeContent ( ContentText "" )
                    ]
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
                       { nameLocalName = "id"
                       , nameNamespace = Nothing
                       , namePrefix = Nothing
                       }
                     , [ ContentText "ENTITIES" ]
                     )
                    ,
                     ( Name
                       { nameLocalName = "name"
                       , nameNamespace = Nothing
                       , namePrefix = Nothing
                       }
                     , [ ContentText "ENTITIES" ]
                     )
                    ]
                  , elementNodes =
                    [ NodeContent ( ContentText "" )
                    , NodeElement
                      ( Element
                        { elementName = Name
                          { nameLocalName = "annotation"
                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                          , namePrefix = Just "xs"
                          }
                        , elementAttributes = []
                        , elementNodes =
                          [ NodeContent ( ContentText "" )
                          , NodeElement
                            ( Element
                              { elementName = Name
                                { nameLocalName = "appinfo"
                                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                , namePrefix = Just "xs"
                                }
                              , elementAttributes = []
                              , elementNodes =
                                [ NodeContent ( ContentText "" )
                                , NodeElement
                                  ( Element
                                    { elementName = Name
                                      { nameLocalName = "hasFacet"
                                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                                      , namePrefix = Just "hfp"
                                      }
                                    , elementAttributes =
                                      [
                                       ( Name
                                         { nameLocalName = "name"
                                         , nameNamespace = Nothing
                                         , namePrefix = Nothing
                                         }
                                       , [ ContentText "length" ]
                                       )
                                      ]
                                    , elementNodes = []
                                    }
                                  )
                                , NodeContent ( ContentText "" )
                                , NodeElement
                                  ( Element
                                    { elementName = Name
                                      { nameLocalName = "hasFacet"
                                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                                      , namePrefix = Just "hfp"
                                      }
                                    , elementAttributes =
                                      [
                                       ( Name
                                         { nameLocalName = "name"
                                         , nameNamespace = Nothing
                                         , namePrefix = Nothing
                                         }
                                       , [ ContentText "minLength" ]
                                       )
                                      ]
                                    , elementNodes = []
                                    }
                                  )
                                , NodeContent ( ContentText "" )
                                , NodeElement
                                  ( Element
                                    { elementName = Name
                                      { nameLocalName = "hasFacet"
                                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                                      , namePrefix = Just "hfp"
                                      }
                                    , elementAttributes =
                                      [
                                       ( Name
                                         { nameLocalName = "name"
                                         , nameNamespace = Nothing
                                         , namePrefix = Nothing
                                         }
                                       , [ ContentText "maxLength" ]
                                       )
                                      ]
                                    , elementNodes = []
                                    }
                                  )
                                , NodeContent ( ContentText "" )
                                , NodeElement
                                  ( Element
                                    { elementName = Name
                                      { nameLocalName = "hasFacet"
                                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                                      , namePrefix = Just "hfp"
                                      }
                                    , elementAttributes =
                                      [
                                       ( Name
                                         { nameLocalName = "name"
                                         , nameNamespace = Nothing
                                         , namePrefix = Nothing
                                         }
                                       , [ ContentText "enumeration" ]
                                       )
                                      ]
                                    , elementNodes = []
                                    }
                                  )
                                , NodeContent ( ContentText "" )
                                , NodeElement
                                  ( Element
                                    { elementName = Name
                                      { nameLocalName = "hasFacet"
                                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                                      , namePrefix = Just "hfp"
                                      }
                                    , elementAttributes =
                                      [
                                       ( Name
                                         { nameLocalName = "name"
                                         , nameNamespace = Nothing
                                         , namePrefix = Nothing
                                         }
                                       , [ ContentText "whiteSpace" ]
                                       )
                                      ]
                                    , elementNodes = []
                                    }
                                  )
                                , NodeContent ( ContentText "" )
                                , NodeElement
                                  ( Element
                                    { elementName = Name
                                      { nameLocalName = "hasFacet"
                                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                                      , namePrefix = Just "hfp"
                                      }
                                    , elementAttributes =
                                      [
                                       ( Name
                                         { nameLocalName = "name"
                                         , nameNamespace = Nothing
                                         , namePrefix = Nothing
                                         }
                                       , [ ContentText "pattern" ]
                                       )
                                      ]
                                    , elementNodes = []
                                    }
                                  )
                                , NodeContent ( ContentText "" )
                                , NodeElement
                                  ( Element
                                    { elementName = Name
                                      { nameLocalName = "hasProperty"
                                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                                      , namePrefix = Just "hfp"
                                      }
                                    , elementAttributes =
                                      [
                                       ( Name
                                         { nameLocalName = "value"
                                         , nameNamespace = Nothing
                                         , namePrefix = Nothing
                                         }
                                       , [ ContentText "false" ]
                                       )
                                      ,
                                       ( Name
                                         { nameLocalName = "name"
                                         , nameNamespace = Nothing
                                         , namePrefix = Nothing
                                         }
                                       , [ ContentText "ordered" ]
                                       )
                                      ]
                                    , elementNodes = []
                                    }
                                  )
                                , NodeContent ( ContentText "" )
                                , NodeElement
                                  ( Element
                                    { elementName = Name
                                      { nameLocalName = "hasProperty"
                                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                                      , namePrefix = Just "hfp"
                                      }
                                    , elementAttributes =
                                      [
                                       ( Name
                                         { nameLocalName = "value"
                                         , nameNamespace = Nothing
                                         , namePrefix = Nothing
                                         }
                                       , [ ContentText "false" ]
                                       )
                                      ,
                                       ( Name
                                         { nameLocalName = "name"
                                         , nameNamespace = Nothing
                                         , namePrefix = Nothing
                                         }
                                       , [ ContentText "bounded" ]
                                       )
                                      ]
                                    , elementNodes = []
                                    }
                                  )
                                , NodeContent ( ContentText "" )
                                , NodeElement
                                  ( Element
                                    { elementName = Name
                                      { nameLocalName = "hasProperty"
                                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                                      , namePrefix = Just "hfp"
                                      }
                                    , elementAttributes =
                                      [
                                       ( Name
                                         { nameLocalName = "value"
                                         , nameNamespace = Nothing
                                         , namePrefix = Nothing
                                         }
                                       , [ ContentText "countably infinite" ]
                                       )
                                      ,
                                       ( Name
                                         { nameLocalName = "name"
                                         , nameNamespace = Nothing
                                         , namePrefix = Nothing
                                         }
                                       , [ ContentText "cardinality" ]
                                       )
                                      ]
                                    , elementNodes = []
                                    }
                                  )
                                , NodeContent ( ContentText "" )
                                , NodeElement
                                  ( Element
                                    { elementName = Name
                                      { nameLocalName = "hasProperty"
                                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                                      , namePrefix = Just "hfp"
                                      }
                                    , elementAttributes =
                                      [
                                       ( Name
                                         { nameLocalName = "value"
                                         , nameNamespace = Nothing
                                         , namePrefix = Nothing
                                         }
                                       , [ ContentText "false" ]
                                       )
                                      ,
                                       ( Name
                                         { nameLocalName = "name"
                                         , nameNamespace = Nothing
                                         , namePrefix = Nothing
                                         }
                                       , [ ContentText "numeric" ]
                                       )
                                      ]
                                    , elementNodes = []
                                    }
                                  )
                                , NodeContent ( ContentText "" )
                                ]
                              }
                            )
                          , NodeContent ( ContentText "" )
                          , NodeElement
                            ( Element
                              { elementName = Name
                                { nameLocalName = "documentation"
                                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                , namePrefix = Just "xs"
                                }
                              , elementAttributes =
                                [
                                 ( Name
                                   { nameLocalName = "source"
                                   , nameNamespace = Nothing
                                   , namePrefix = Nothing
                                   }
                                 , [ ContentText "http://www.w3.org/TR/xmlschema-2/#ENTITIES" ]
                                 )
                                ]
                              , elementNodes = []
                              }
                            )
                          , NodeContent ( ContentText "" )
                          ]
                        }
                      )
                    , NodeContent ( ContentText "" )
                    , NodeElement
                      ( Element
                        { elementName = Name
                          { nameLocalName = "restriction"
                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                          , namePrefix = Just "xs"
                          }
                        , elementAttributes = []
                        , elementNodes =
                          [ NodeContent ( ContentText "" )
                          , NodeElement
                            ( Element
                              { elementName = Name
                                { nameLocalName = "simpleType"
                                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                , namePrefix = Just "xs"
                                }
                              , elementAttributes = []
                              , elementNodes =
                                [ NodeContent ( ContentText "" )
                                , NodeElement
                                  ( Element
                                    { elementName = Name
                                      { nameLocalName = "list"
                                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                      , namePrefix = Just "xs"
                                      }
                                    , elementAttributes =
                                      [
                                       ( Name
                                         { nameLocalName = "itemType"
                                         , nameNamespace = Nothing
                                         , namePrefix = Nothing
                                         }
                                       , [ ContentText "xs:ENTITY" ]
                                       )
                                      ]
                                    , elementNodes = []
                                    }
                                  )
                                , NodeContent ( ContentText "" )
                                ]
                              }
                            )
                          , NodeContent ( ContentText "" )
                          , NodeElement
                            ( Element
                              { elementName = Name
                                { nameLocalName = "minLength"
                                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                , namePrefix = Just "xs"
                                }
                              , elementAttributes =
                                [
                                 ( Name
                                   { nameLocalName = "id"
                                   , nameNamespace = Nothing
                                   , namePrefix = Nothing
                                   }
                                 , [ ContentText "ENTITIES.minLength" ]
                                 )
                                ,
                                 ( Name
                                   { nameLocalName = "value"
                                   , nameNamespace = Nothing
                                   , namePrefix = Nothing
                                   }
                                 , [ ContentText "1" ]
                                 )
                                ]
                              , elementNodes = []
                              }
                            )
                          , NodeContent ( ContentText "" )
                          ]
                        }
                      )
                    , NodeContent ( ContentText "" )
                    ]
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
                       { nameLocalName = "id"
                       , nameNamespace = Nothing
                       , namePrefix = Nothing
                       }
                     , [ ContentText "NMTOKEN" ]
                     )
                    ,
                     ( Name
                       { nameLocalName = "name"
                       , nameNamespace = Nothing
                       , namePrefix = Nothing
                       }
                     , [ ContentText "NMTOKEN" ]
                     )
                    ]
                  , elementNodes =
                    [ NodeContent ( ContentText "" )
                    , NodeElement
                      ( Element
                        { elementName = Name
                          { nameLocalName = "annotation"
                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                          , namePrefix = Just "xs"
                          }
                        , elementAttributes = []
                        , elementNodes =
                          [ NodeContent ( ContentText "" )
                          , NodeElement
                            ( Element
                              { elementName = Name
                                { nameLocalName = "documentation"
                                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                , namePrefix = Just "xs"
                                }
                              , elementAttributes =
                                [
                                 ( Name
                                   { nameLocalName = "source"
                                   , nameNamespace = Nothing
                                   , namePrefix = Nothing
                                   }
                                 , [ ContentText "http://www.w3.org/TR/xmlschema-2/#NMTOKEN" ]
                                 )
                                ]
                              , elementNodes = []
                              }
                            )
                          , NodeContent ( ContentText "" )
                          ]
                        }
                      )
                    , NodeContent ( ContentText "" )
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
                           , [ ContentText "xs:token" ]
                           )
                          ]
                        , elementNodes =
                          [ NodeContent ( ContentText "" )
                          , NodeElement
                            ( Element
                              { elementName = Name
                                { nameLocalName = "pattern"
                                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                , namePrefix = Just "xs"
                                }
                              , elementAttributes =
                                [
                                 ( Name
                                   { nameLocalName = "id"
                                   , nameNamespace = Nothing
                                   , namePrefix = Nothing
                                   }
                                 , [ ContentText "NMTOKEN.pattern" ]
                                 )
                                ,
                                 ( Name
                                   { nameLocalName = "value"
                                   , nameNamespace = Nothing
                                   , namePrefix = Nothing
                                   }
                                 , [ ContentText "\c+" ]
                                 )
                                ]
                              , elementNodes =
                                [ NodeContent ( ContentText "" )
                                , NodeElement
                                  ( Element
                                    { elementName = Name
                                      { nameLocalName = "annotation"
                                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                      , namePrefix = Just "xs"
                                      }
                                    , elementAttributes = []
                                    , elementNodes =
                                      [ NodeContent ( ContentText "" )
                                      , NodeElement
                                        ( Element
                                          { elementName = Name
                                            { nameLocalName = "documentation"
                                            , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                            , namePrefix = Just "xs"
                                            }
                                          , elementAttributes =
                                            [
                                             ( Name
                                               { nameLocalName = "source"
                                               , nameNamespace = Nothing
                                               , namePrefix = Nothing
                                               }
                                             , [ ContentText "http://www.w3.org/TR/REC-xml#NT-Nmtoken" ]
                                             )
                                            ]
                                          , elementNodes =
                                            [ NodeContent ( ContentText "pattern matches production 7 from the XML spec" ) ]
                                          }
                                        )
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
                      )
                    , NodeContent ( ContentText "" )
                    ]
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
                       { nameLocalName = "id"
                       , nameNamespace = Nothing
                       , namePrefix = Nothing
                       }
                     , [ ContentText "NMTOKENS" ]
                     )
                    ,
                     ( Name
                       { nameLocalName = "name"
                       , nameNamespace = Nothing
                       , namePrefix = Nothing
                       }
                     , [ ContentText "NMTOKENS" ]
                     )
                    ]
                  , elementNodes =
                    [ NodeContent ( ContentText "" )
                    , NodeElement
                      ( Element
                        { elementName = Name
                          { nameLocalName = "annotation"
                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                          , namePrefix = Just "xs"
                          }
                        , elementAttributes = []
                        , elementNodes =
                          [ NodeContent ( ContentText "" )
                          , NodeElement
                            ( Element
                              { elementName = Name
                                { nameLocalName = "appinfo"
                                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                , namePrefix = Just "xs"
                                }
                              , elementAttributes = []
                              , elementNodes =
                                [ NodeContent ( ContentText "" )
                                , NodeElement
                                  ( Element
                                    { elementName = Name
                                      { nameLocalName = "hasFacet"
                                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                                      , namePrefix = Just "hfp"
                                      }
                                    , elementAttributes =
                                      [
                                       ( Name
                                         { nameLocalName = "name"
                                         , nameNamespace = Nothing
                                         , namePrefix = Nothing
                                         }
                                       , [ ContentText "length" ]
                                       )
                                      ]
                                    , elementNodes = []
                                    }
                                  )
                                , NodeContent ( ContentText "" )
                                , NodeElement
                                  ( Element
                                    { elementName = Name
                                      { nameLocalName = "hasFacet"
                                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                                      , namePrefix = Just "hfp"
                                      }
                                    , elementAttributes =
                                      [
                                       ( Name
                                         { nameLocalName = "name"
                                         , nameNamespace = Nothing
                                         , namePrefix = Nothing
                                         }
                                       , [ ContentText "minLength" ]
                                       )
                                      ]
                                    , elementNodes = []
                                    }
                                  )
                                , NodeContent ( ContentText "" )
                                , NodeElement
                                  ( Element
                                    { elementName = Name
                                      { nameLocalName = "hasFacet"
                                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                                      , namePrefix = Just "hfp"
                                      }
                                    , elementAttributes =
                                      [
                                       ( Name
                                         { nameLocalName = "name"
                                         , nameNamespace = Nothing
                                         , namePrefix = Nothing
                                         }
                                       , [ ContentText "maxLength" ]
                                       )
                                      ]
                                    , elementNodes = []
                                    }
                                  )
                                , NodeContent ( ContentText "" )
                                , NodeElement
                                  ( Element
                                    { elementName = Name
                                      { nameLocalName = "hasFacet"
                                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                                      , namePrefix = Just "hfp"
                                      }
                                    , elementAttributes =
                                      [
                                       ( Name
                                         { nameLocalName = "name"
                                         , nameNamespace = Nothing
                                         , namePrefix = Nothing
                                         }
                                       , [ ContentText "enumeration" ]
                                       )
                                      ]
                                    , elementNodes = []
                                    }
                                  )
                                , NodeContent ( ContentText "" )
                                , NodeElement
                                  ( Element
                                    { elementName = Name
                                      { nameLocalName = "hasFacet"
                                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                                      , namePrefix = Just "hfp"
                                      }
                                    , elementAttributes =
                                      [
                                       ( Name
                                         { nameLocalName = "name"
                                         , nameNamespace = Nothing
                                         , namePrefix = Nothing
                                         }
                                       , [ ContentText "whiteSpace" ]
                                       )
                                      ]
                                    , elementNodes = []
                                    }
                                  )
                                , NodeContent ( ContentText "" )
                                , NodeElement
                                  ( Element
                                    { elementName = Name
                                      { nameLocalName = "hasFacet"
                                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                                      , namePrefix = Just "hfp"
                                      }
                                    , elementAttributes =
                                      [
                                       ( Name
                                         { nameLocalName = "name"
                                         , nameNamespace = Nothing
                                         , namePrefix = Nothing
                                         }
                                       , [ ContentText "pattern" ]
                                       )
                                      ]
                                    , elementNodes = []
                                    }
                                  )
                                , NodeContent ( ContentText "" )
                                , NodeElement
                                  ( Element
                                    { elementName = Name
                                      { nameLocalName = "hasProperty"
                                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                                      , namePrefix = Just "hfp"
                                      }
                                    , elementAttributes =
                                      [
                                       ( Name
                                         { nameLocalName = "value"
                                         , nameNamespace = Nothing
                                         , namePrefix = Nothing
                                         }
                                       , [ ContentText "false" ]
                                       )
                                      ,
                                       ( Name
                                         { nameLocalName = "name"
                                         , nameNamespace = Nothing
                                         , namePrefix = Nothing
                                         }
                                       , [ ContentText "ordered" ]
                                       )
                                      ]
                                    , elementNodes = []
                                    }
                                  )
                                , NodeContent ( ContentText "" )
                                , NodeElement
                                  ( Element
                                    { elementName = Name
                                      { nameLocalName = "hasProperty"
                                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                                      , namePrefix = Just "hfp"
                                      }
                                    , elementAttributes =
                                      [
                                       ( Name
                                         { nameLocalName = "value"
                                         , nameNamespace = Nothing
                                         , namePrefix = Nothing
                                         }
                                       , [ ContentText "false" ]
                                       )
                                      ,
                                       ( Name
                                         { nameLocalName = "name"
                                         , nameNamespace = Nothing
                                         , namePrefix = Nothing
                                         }
                                       , [ ContentText "bounded" ]
                                       )
                                      ]
                                    , elementNodes = []
                                    }
                                  )
                                , NodeContent ( ContentText "" )
                                , NodeElement
                                  ( Element
                                    { elementName = Name
                                      { nameLocalName = "hasProperty"
                                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                                      , namePrefix = Just "hfp"
                                      }
                                    , elementAttributes =
                                      [
                                       ( Name
                                         { nameLocalName = "value"
                                         , nameNamespace = Nothing
                                         , namePrefix = Nothing
                                         }
                                       , [ ContentText "countably infinite" ]
                                       )
                                      ,
                                       ( Name
                                         { nameLocalName = "name"
                                         , nameNamespace = Nothing
                                         , namePrefix = Nothing
                                         }
                                       , [ ContentText "cardinality" ]
                                       )
                                      ]
                                    , elementNodes = []
                                    }
                                  )
                                , NodeContent ( ContentText "" )
                                , NodeElement
                                  ( Element
                                    { elementName = Name
                                      { nameLocalName = "hasProperty"
                                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                                      , namePrefix = Just "hfp"
                                      }
                                    , elementAttributes =
                                      [
                                       ( Name
                                         { nameLocalName = "value"
                                         , nameNamespace = Nothing
                                         , namePrefix = Nothing
                                         }
                                       , [ ContentText "false" ]
                                       )
                                      ,
                                       ( Name
                                         { nameLocalName = "name"
                                         , nameNamespace = Nothing
                                         , namePrefix = Nothing
                                         }
                                       , [ ContentText "numeric" ]
                                       )
                                      ]
                                    , elementNodes = []
                                    }
                                  )
                                , NodeContent ( ContentText "" )
                                ]
                              }
                            )
                          , NodeContent ( ContentText "" )
                          , NodeElement
                            ( Element
                              { elementName = Name
                                { nameLocalName = "documentation"
                                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                , namePrefix = Just "xs"
                                }
                              , elementAttributes =
                                [
                                 ( Name
                                   { nameLocalName = "source"
                                   , nameNamespace = Nothing
                                   , namePrefix = Nothing
                                   }
                                 , [ ContentText "http://www.w3.org/TR/xmlschema-2/#NMTOKENS" ]
                                 )
                                ]
                              , elementNodes = []
                              }
                            )
                          , NodeContent ( ContentText "" )
                          ]
                        }
                      )
                    , NodeContent ( ContentText "" )
                    , NodeElement
                      ( Element
                        { elementName = Name
                          { nameLocalName = "restriction"
                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                          , namePrefix = Just "xs"
                          }
                        , elementAttributes = []
                        , elementNodes =
                          [ NodeContent ( ContentText "" )
                          , NodeElement
                            ( Element
                              { elementName = Name
                                { nameLocalName = "simpleType"
                                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                , namePrefix = Just "xs"
                                }
                              , elementAttributes = []
                              , elementNodes =
                                [ NodeContent ( ContentText "" )
                                , NodeElement
                                  ( Element
                                    { elementName = Name
                                      { nameLocalName = "list"
                                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                      , namePrefix = Just "xs"
                                      }
                                    , elementAttributes =
                                      [
                                       ( Name
                                         { nameLocalName = "itemType"
                                         , nameNamespace = Nothing
                                         , namePrefix = Nothing
                                         }
                                       , [ ContentText "xs:NMTOKEN" ]
                                       )
                                      ]
                                    , elementNodes = []
                                    }
                                  )
                                , NodeContent ( ContentText "" )
                                ]
                              }
                            )
                          , NodeContent ( ContentText "" )
                          , NodeElement
                            ( Element
                              { elementName = Name
                                { nameLocalName = "minLength"
                                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                , namePrefix = Just "xs"
                                }
                              , elementAttributes =
                                [
                                 ( Name
                                   { nameLocalName = "id"
                                   , nameNamespace = Nothing
                                   , namePrefix = Nothing
                                   }
                                 , [ ContentText "NMTOKENS.minLength" ]
                                 )
                                ,
                                 ( Name
                                   { nameLocalName = "value"
                                   , nameNamespace = Nothing
                                   , namePrefix = Nothing
                                   }
                                 , [ ContentText "1" ]
                                 )
                                ]
                              , elementNodes = []
                              }
                            )
                          , NodeContent ( ContentText "" )
                          ]
                        }
                      )
                    , NodeContent ( ContentText "" )
                    ]
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
                       { nameLocalName = "id"
                       , nameNamespace = Nothing
                       , namePrefix = Nothing
                       }
                     , [ ContentText "Name" ]
                     )
                    ,
                     ( Name
                       { nameLocalName = "name"
                       , nameNamespace = Nothing
                       , namePrefix = Nothing
                       }
                     , [ ContentText "Name" ]
                     )
                    ]
                  , elementNodes =
                    [ NodeContent ( ContentText "" )
                    , NodeElement
                      ( Element
                        { elementName = Name
                          { nameLocalName = "annotation"
                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                          , namePrefix = Just "xs"
                          }
                        , elementAttributes = []
                        , elementNodes =
                          [ NodeContent ( ContentText "" )
                          , NodeElement
                            ( Element
                              { elementName = Name
                                { nameLocalName = "documentation"
                                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                , namePrefix = Just "xs"
                                }
                              , elementAttributes =
                                [
                                 ( Name
                                   { nameLocalName = "source"
                                   , nameNamespace = Nothing
                                   , namePrefix = Nothing
                                   }
                                 , [ ContentText "http://www.w3.org/TR/xmlschema-2/#Name" ]
                                 )
                                ]
                              , elementNodes = []
                              }
                            )
                          , NodeContent ( ContentText "" )
                          ]
                        }
                      )
                    , NodeContent ( ContentText "" )
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
                           , [ ContentText "xs:token" ]
                           )
                          ]
                        , elementNodes =
                          [ NodeContent ( ContentText "" )
                          , NodeElement
                            ( Element
                              { elementName = Name
                                { nameLocalName = "pattern"
                                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                , namePrefix = Just "xs"
                                }
                              , elementAttributes =
                                [
                                 ( Name
                                   { nameLocalName = "id"
                                   , nameNamespace = Nothing
                                   , namePrefix = Nothing
                                   }
                                 , [ ContentText "Name.pattern" ]
                                 )
                                ,
                                 ( Name
                                   { nameLocalName = "value"
                                   , nameNamespace = Nothing
                                   , namePrefix = Nothing
                                   }
                                 , [ ContentText "\i\c*" ]
                                 )
                                ]
                              , elementNodes =
                                [ NodeContent ( ContentText "" )
                                , NodeElement
                                  ( Element
                                    { elementName = Name
                                      { nameLocalName = "annotation"
                                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                      , namePrefix = Just "xs"
                                      }
                                    , elementAttributes = []
                                    , elementNodes =
                                      [ NodeContent ( ContentText "" )
                                      , NodeElement
                                        ( Element
                                          { elementName = Name
                                            { nameLocalName = "documentation"
                                            , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                            , namePrefix = Just "xs"
                                            }
                                          , elementAttributes =
                                            [
                                             ( Name
                                               { nameLocalName = "source"
                                               , nameNamespace = Nothing
                                               , namePrefix = Nothing
                                               }
                                             , [ ContentText "http://www.w3.org/TR/REC-xml#NT-Name" ]
                                             )
                                            ]
                                          , elementNodes =
                                            [ NodeContent ( ContentText "pattern matches production 5 from the XML spec" ) ]
                                          }
                                        )
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
                      )
                    , NodeContent ( ContentText "" )
                    ]
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
                       { nameLocalName = "id"
                       , nameNamespace = Nothing
                       , namePrefix = Nothing
                       }
                     , [ ContentText "NCName" ]
                     )
                    ,
                     ( Name
                       { nameLocalName = "name"
                       , nameNamespace = Nothing
                       , namePrefix = Nothing
                       }
                     , [ ContentText "NCName" ]
                     )
                    ]
                  , elementNodes =
                    [ NodeContent ( ContentText "" )
                    , NodeElement
                      ( Element
                        { elementName = Name
                          { nameLocalName = "annotation"
                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                          , namePrefix = Just "xs"
                          }
                        , elementAttributes = []
                        , elementNodes =
                          [ NodeContent ( ContentText "" )
                          , NodeElement
                            ( Element
                              { elementName = Name
                                { nameLocalName = "documentation"
                                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                , namePrefix = Just "xs"
                                }
                              , elementAttributes =
                                [
                                 ( Name
                                   { nameLocalName = "source"
                                   , nameNamespace = Nothing
                                   , namePrefix = Nothing
                                   }
                                 , [ ContentText "http://www.w3.org/TR/xmlschema-2/#NCName" ]
                                 )
                                ]
                              , elementNodes = []
                              }
                            )
                          , NodeContent ( ContentText "" )
                          ]
                        }
                      )
                    , NodeContent ( ContentText "" )
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
                           , [ ContentText "xs:Name" ]
                           )
                          ]
                        , elementNodes =
                          [ NodeContent ( ContentText "" )
                          , NodeElement
                            ( Element
                              { elementName = Name
                                { nameLocalName = "pattern"
                                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                , namePrefix = Just "xs"
                                }
                              , elementAttributes =
                                [
                                 ( Name
                                   { nameLocalName = "id"
                                   , nameNamespace = Nothing
                                   , namePrefix = Nothing
                                   }
                                 , [ ContentText "NCName.pattern" ]
                                 )
                                ,
                                 ( Name
                                   { nameLocalName = "value"
                                   , nameNamespace = Nothing
                                   , namePrefix = Nothing
                                   }
                                 , [ ContentText "[\i-[:]][\c-[:]]*" ]
                                 )
                                ]
                              , elementNodes =
                                [ NodeContent ( ContentText "" )
                                , NodeElement
                                  ( Element
                                    { elementName = Name
                                      { nameLocalName = "annotation"
                                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                      , namePrefix = Just "xs"
                                      }
                                    , elementAttributes = []
                                    , elementNodes =
                                      [ NodeContent ( ContentText "" )
                                      , NodeElement
                                        ( Element
                                          { elementName = Name
                                            { nameLocalName = "documentation"
                                            , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                            , namePrefix = Just "xs"
                                            }
                                          , elementAttributes =
                                            [
                                             ( Name
                                               { nameLocalName = "source"
                                               , nameNamespace = Nothing
                                               , namePrefix = Nothing
                                               }
                                             , [ ContentText "http://www.w3.org/TR/REC-xml-names/#NT-NCName" ]
                                             )
                                            ]
                                          , elementNodes =
                                            [ NodeContent ( ContentText "pattern matches production 4 from the Namespaces in XML spec" ) ]
                                          }
                                        )
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
                      )
                    , NodeContent ( ContentText "" )
                    ]
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
                       { nameLocalName = "id"
                       , nameNamespace = Nothing
                       , namePrefix = Nothing
                       }
                     , [ ContentText "ID" ]
                     )
                    ,
                     ( Name
                       { nameLocalName = "name"
                       , nameNamespace = Nothing
                       , namePrefix = Nothing
                       }
                     , [ ContentText "ID" ]
                     )
                    ]
                  , elementNodes =
                    [ NodeContent ( ContentText "" )
                    , NodeElement
                      ( Element
                        { elementName = Name
                          { nameLocalName = "annotation"
                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                          , namePrefix = Just "xs"
                          }
                        , elementAttributes = []
                        , elementNodes =
                          [ NodeContent ( ContentText "" )
                          , NodeElement
                            ( Element
                              { elementName = Name
                                { nameLocalName = "documentation"
                                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                , namePrefix = Just "xs"
                                }
                              , elementAttributes =
                                [
                                 ( Name
                                   { nameLocalName = "source"
                                   , nameNamespace = Nothing
                                   , namePrefix = Nothing
                                   }
                                 , [ ContentText "http://www.w3.org/TR/xmlschema-2/#ID" ]
                                 )
                                ]
                              , elementNodes = []
                              }
                            )
                          , NodeContent ( ContentText "" )
                          ]
                        }
                      )
                    , NodeContent ( ContentText "" )
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
                           , [ ContentText "xs:NCName" ]
                           )
                          ]
                        , elementNodes = []
                        }
                      )
                    , NodeContent ( ContentText "" )
                    ]
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
                       { nameLocalName = "id"
                       , nameNamespace = Nothing
                       , namePrefix = Nothing
                       }
                     , [ ContentText "IDREF" ]
                     )
                    ,
                     ( Name
                       { nameLocalName = "name"
                       , nameNamespace = Nothing
                       , namePrefix = Nothing
                       }
                     , [ ContentText "IDREF" ]
                     )
                    ]
                  , elementNodes =
                    [ NodeContent ( ContentText "" )
                    , NodeElement
                      ( Element
                        { elementName = Name
                          { nameLocalName = "annotation"
                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                          , namePrefix = Just "xs"
                          }
                        , elementAttributes = []
                        , elementNodes =
                          [ NodeContent ( ContentText "" )
                          , NodeElement
                            ( Element
                              { elementName = Name
                                { nameLocalName = "documentation"
                                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                , namePrefix = Just "xs"
                                }
                              , elementAttributes =
                                [
                                 ( Name
                                   { nameLocalName = "source"
                                   , nameNamespace = Nothing
                                   , namePrefix = Nothing
                                   }
                                 , [ ContentText "http://www.w3.org/TR/xmlschema-2/#IDREF" ]
                                 )
                                ]
                              , elementNodes = []
                              }
                            )
                          , NodeContent ( ContentText "" )
                          ]
                        }
                      )
                    , NodeContent ( ContentText "" )
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
                           , [ ContentText "xs:NCName" ]
                           )
                          ]
                        , elementNodes = []
                        }
                      )
                    , NodeContent ( ContentText "" )
                    ]
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
                       { nameLocalName = "id"
                       , nameNamespace = Nothing
                       , namePrefix = Nothing
                       }
                     , [ ContentText "ENTITY" ]
                     )
                    ,
                     ( Name
                       { nameLocalName = "name"
                       , nameNamespace = Nothing
                       , namePrefix = Nothing
                       }
                     , [ ContentText "ENTITY" ]
                     )
                    ]
                  , elementNodes =
                    [ NodeContent ( ContentText "" )
                    , NodeElement
                      ( Element
                        { elementName = Name
                          { nameLocalName = "annotation"
                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                          , namePrefix = Just "xs"
                          }
                        , elementAttributes = []
                        , elementNodes =
                          [ NodeContent ( ContentText "" )
                          , NodeElement
                            ( Element
                              { elementName = Name
                                { nameLocalName = "documentation"
                                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                , namePrefix = Just "xs"
                                }
                              , elementAttributes =
                                [
                                 ( Name
                                   { nameLocalName = "source"
                                   , nameNamespace = Nothing
                                   , namePrefix = Nothing
                                   }
                                 , [ ContentText "http://www.w3.org/TR/xmlschema-2/#ENTITY" ]
                                 )
                                ]
                              , elementNodes = []
                              }
                            )
                          , NodeContent ( ContentText "" )
                          ]
                        }
                      )
                    , NodeContent ( ContentText "" )
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
                           , [ ContentText "xs:NCName" ]
                           )
                          ]
                        , elementNodes = []
                        }
                      )
                    , NodeContent ( ContentText "" )
                    ]
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
                       { nameLocalName = "id"
                       , nameNamespace = Nothing
                       , namePrefix = Nothing
                       }
                     , [ ContentText "integer" ]
                     )
                    ,
                     ( Name
                       { nameLocalName = "name"
                       , nameNamespace = Nothing
                       , namePrefix = Nothing
                       }
                     , [ ContentText "integer" ]
                     )
                    ]
                  , elementNodes =
                    [ NodeContent ( ContentText "" )
                    , NodeElement
                      ( Element
                        { elementName = Name
                          { nameLocalName = "annotation"
                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                          , namePrefix = Just "xs"
                          }
                        , elementAttributes = []
                        , elementNodes =
                          [ NodeContent ( ContentText "" )
                          , NodeElement
                            ( Element
                              { elementName = Name
                                { nameLocalName = "documentation"
                                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                , namePrefix = Just "xs"
                                }
                              , elementAttributes =
                                [
                                 ( Name
                                   { nameLocalName = "source"
                                   , nameNamespace = Nothing
                                   , namePrefix = Nothing
                                   }
                                 , [ ContentText "http://www.w3.org/TR/xmlschema-2/#integer" ]
                                 )
                                ]
                              , elementNodes = []
                              }
                            )
                          , NodeContent ( ContentText "" )
                          ]
                        }
                      )
                    , NodeContent ( ContentText "" )
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
                           , [ ContentText "xs:decimal" ]
                           )
                          ]
                        , elementNodes =
                          [ NodeContent ( ContentText "" )
                          , NodeElement
                            ( Element
                              { elementName = Name
                                { nameLocalName = "fractionDigits"
                                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                , namePrefix = Just "xs"
                                }
                              , elementAttributes =
                                [
                                 ( Name
                                   { nameLocalName = "id"
                                   , nameNamespace = Nothing
                                   , namePrefix = Nothing
                                   }
                                 , [ ContentText "integer.fractionDigits" ]
                                 )
                                ,
                                 ( Name
                                   { nameLocalName = "value"
                                   , nameNamespace = Nothing
                                   , namePrefix = Nothing
                                   }
                                 , [ ContentText "0" ]
                                 )
                                ,
                                 ( Name
                                   { nameLocalName = "fixed"
                                   , nameNamespace = Nothing
                                   , namePrefix = Nothing
                                   }
                                 , [ ContentText "true" ]
                                 )
                                ]
                              , elementNodes = []
                              }
                            )
                          , NodeContent ( ContentText "" )
                          , NodeElement
                            ( Element
                              { elementName = Name
                                { nameLocalName = "pattern"
                                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                , namePrefix = Just "xs"
                                }
                              , elementAttributes =
                                [
                                 ( Name
                                   { nameLocalName = "value"
                                   , nameNamespace = Nothing
                                   , namePrefix = Nothing
                                   }
                                 , [ ContentText "[\-+]?[0-9]+" ]
                                 )
                                ]
                              , elementNodes = []
                              }
                            )
                          , NodeContent ( ContentText "" )
                          ]
                        }
                      )
                    , NodeContent ( ContentText "" )
                    ]
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
                       { nameLocalName = "id"
                       , nameNamespace = Nothing
                       , namePrefix = Nothing
                       }
                     , [ ContentText "nonPositiveInteger" ]
                     )
                    ,
                     ( Name
                       { nameLocalName = "name"
                       , nameNamespace = Nothing
                       , namePrefix = Nothing
                       }
                     , [ ContentText "nonPositiveInteger" ]
                     )
                    ]
                  , elementNodes =
                    [ NodeContent ( ContentText "" )
                    , NodeElement
                      ( Element
                        { elementName = Name
                          { nameLocalName = "annotation"
                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                          , namePrefix = Just "xs"
                          }
                        , elementAttributes = []
                        , elementNodes =
                          [ NodeContent ( ContentText "" )
                          , NodeElement
                            ( Element
                              { elementName = Name
                                { nameLocalName = "documentation"
                                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                , namePrefix = Just "xs"
                                }
                              , elementAttributes =
                                [
                                 ( Name
                                   { nameLocalName = "source"
                                   , nameNamespace = Nothing
                                   , namePrefix = Nothing
                                   }
                                 , [ ContentText "http://www.w3.org/TR/xmlschema-2/#nonPositiveInteger" ]
                                 )
                                ]
                              , elementNodes = []
                              }
                            )
                          , NodeContent ( ContentText "" )
                          ]
                        }
                      )
                    , NodeContent ( ContentText "" )
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
                          , NodeElement
                            ( Element
                              { elementName = Name
                                { nameLocalName = "maxInclusive"
                                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                , namePrefix = Just "xs"
                                }
                              , elementAttributes =
                                [
                                 ( Name
                                   { nameLocalName = "id"
                                   , nameNamespace = Nothing
                                   , namePrefix = Nothing
                                   }
                                 , [ ContentText "nonPositiveInteger.maxInclusive" ]
                                 )
                                ,
                                 ( Name
                                   { nameLocalName = "value"
                                   , nameNamespace = Nothing
                                   , namePrefix = Nothing
                                   }
                                 , [ ContentText "0" ]
                                 )
                                ]
                              , elementNodes = []
                              }
                            )
                          , NodeContent ( ContentText "" )
                          ]
                        }
                      )
                    , NodeContent ( ContentText "" )
                    ]
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
                       { nameLocalName = "id"
                       , nameNamespace = Nothing
                       , namePrefix = Nothing
                       }
                     , [ ContentText "negativeInteger" ]
                     )
                    ,
                     ( Name
                       { nameLocalName = "name"
                       , nameNamespace = Nothing
                       , namePrefix = Nothing
                       }
                     , [ ContentText "negativeInteger" ]
                     )
                    ]
                  , elementNodes =
                    [ NodeContent ( ContentText "" )
                    , NodeElement
                      ( Element
                        { elementName = Name
                          { nameLocalName = "annotation"
                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                          , namePrefix = Just "xs"
                          }
                        , elementAttributes = []
                        , elementNodes =
                          [ NodeContent ( ContentText "" )
                          , NodeElement
                            ( Element
                              { elementName = Name
                                { nameLocalName = "documentation"
                                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                , namePrefix = Just "xs"
                                }
                              , elementAttributes =
                                [
                                 ( Name
                                   { nameLocalName = "source"
                                   , nameNamespace = Nothing
                                   , namePrefix = Nothing
                                   }
                                 , [ ContentText "http://www.w3.org/TR/xmlschema-2/#negativeInteger" ]
                                 )
                                ]
                              , elementNodes = []
                              }
                            )
                          , NodeContent ( ContentText "" )
                          ]
                        }
                      )
                    , NodeContent ( ContentText "" )
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
                           , [ ContentText "xs:nonPositiveInteger" ]
                           )
                          ]
                        , elementNodes =
                          [ NodeContent ( ContentText "" )
                          , NodeElement
                            ( Element
                              { elementName = Name
                                { nameLocalName = "maxInclusive"
                                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                , namePrefix = Just "xs"
                                }
                              , elementAttributes =
                                [
                                 ( Name
                                   { nameLocalName = "id"
                                   , nameNamespace = Nothing
                                   , namePrefix = Nothing
                                   }
                                 , [ ContentText "negativeInteger.maxInclusive" ]
                                 )
                                ,
                                 ( Name
                                   { nameLocalName = "value"
                                   , nameNamespace = Nothing
                                   , namePrefix = Nothing
                                   }
                                 , [ ContentText "-1" ]
                                 )
                                ]
                              , elementNodes = []
                              }
                            )
                          , NodeContent ( ContentText "" )
                          ]
                        }
                      )
                    , NodeContent ( ContentText "" )
                    ]
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
                       { nameLocalName = "id"
                       , nameNamespace = Nothing
                       , namePrefix = Nothing
                       }
                     , [ ContentText "long" ]
                     )
                    ,
                     ( Name
                       { nameLocalName = "name"
                       , nameNamespace = Nothing
                       , namePrefix = Nothing
                       }
                     , [ ContentText "long" ]
                     )
                    ]
                  , elementNodes =
                    [ NodeContent ( ContentText "" )
                    , NodeElement
                      ( Element
                        { elementName = Name
                          { nameLocalName = "annotation"
                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                          , namePrefix = Just "xs"
                          }
                        , elementAttributes = []
                        , elementNodes =
                          [ NodeContent ( ContentText "" )
                          , NodeElement
                            ( Element
                              { elementName = Name
                                { nameLocalName = "appinfo"
                                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                , namePrefix = Just "xs"
                                }
                              , elementAttributes = []
                              , elementNodes =
                                [ NodeContent ( ContentText "" )
                                , NodeElement
                                  ( Element
                                    { elementName = Name
                                      { nameLocalName = "hasProperty"
                                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                                      , namePrefix = Just "hfp"
                                      }
                                    , elementAttributes =
                                      [
                                       ( Name
                                         { nameLocalName = "value"
                                         , nameNamespace = Nothing
                                         , namePrefix = Nothing
                                         }
                                       , [ ContentText "true" ]
                                       )
                                      ,
                                       ( Name
                                         { nameLocalName = "name"
                                         , nameNamespace = Nothing
                                         , namePrefix = Nothing
                                         }
                                       , [ ContentText "bounded" ]
                                       )
                                      ]
                                    , elementNodes = []
                                    }
                                  )
                                , NodeContent ( ContentText "" )
                                , NodeElement
                                  ( Element
                                    { elementName = Name
                                      { nameLocalName = "hasProperty"
                                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                                      , namePrefix = Just "hfp"
                                      }
                                    , elementAttributes =
                                      [
                                       ( Name
                                         { nameLocalName = "value"
                                         , nameNamespace = Nothing
                                         , namePrefix = Nothing
                                         }
                                       , [ ContentText "finite" ]
                                       )
                                      ,
                                       ( Name
                                         { nameLocalName = "name"
                                         , nameNamespace = Nothing
                                         , namePrefix = Nothing
                                         }
                                       , [ ContentText "cardinality" ]
                                       )
                                      ]
                                    , elementNodes = []
                                    }
                                  )
                                , NodeContent ( ContentText "" )
                                ]
                              }
                            )
                          , NodeContent ( ContentText "" )
                          , NodeElement
                            ( Element
                              { elementName = Name
                                { nameLocalName = "documentation"
                                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                , namePrefix = Just "xs"
                                }
                              , elementAttributes =
                                [
                                 ( Name
                                   { nameLocalName = "source"
                                   , nameNamespace = Nothing
                                   , namePrefix = Nothing
                                   }
                                 , [ ContentText "http://www.w3.org/TR/xmlschema-2/#long" ]
                                 )
                                ]
                              , elementNodes = []
                              }
                            )
                          , NodeContent ( ContentText "" )
                          ]
                        }
                      )
                    , NodeContent ( ContentText "" )
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
                          , NodeElement
                            ( Element
                              { elementName = Name
                                { nameLocalName = "minInclusive"
                                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                , namePrefix = Just "xs"
                                }
                              , elementAttributes =
                                [
                                 ( Name
                                   { nameLocalName = "id"
                                   , nameNamespace = Nothing
                                   , namePrefix = Nothing
                                   }
                                 , [ ContentText "long.minInclusive" ]
                                 )
                                ,
                                 ( Name
                                   { nameLocalName = "value"
                                   , nameNamespace = Nothing
                                   , namePrefix = Nothing
                                   }
                                 , [ ContentText "-9223372036854775808" ]
                                 )
                                ]
                              , elementNodes = []
                              }
                            )
                          , NodeContent ( ContentText "" )
                          , NodeElement
                            ( Element
                              { elementName = Name
                                { nameLocalName = "maxInclusive"
                                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                , namePrefix = Just "xs"
                                }
                              , elementAttributes =
                                [
                                 ( Name
                                   { nameLocalName = "id"
                                   , nameNamespace = Nothing
                                   , namePrefix = Nothing
                                   }
                                 , [ ContentText "long.maxInclusive" ]
                                 )
                                ,
                                 ( Name
                                   { nameLocalName = "value"
                                   , nameNamespace = Nothing
                                   , namePrefix = Nothing
                                   }
                                 , [ ContentText "9223372036854775807" ]
                                 )
                                ]
                              , elementNodes = []
                              }
                            )
                          , NodeContent ( ContentText "" )
                          ]
                        }
                      )
                    , NodeContent ( ContentText "" )
                    ]
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
                       { nameLocalName = "id"
                       , nameNamespace = Nothing
                       , namePrefix = Nothing
                       }
                     , [ ContentText "int" ]
                     )
                    ,
                     ( Name
                       { nameLocalName = "name"
                       , nameNamespace = Nothing
                       , namePrefix = Nothing
                       }
                     , [ ContentText "int" ]
                     )
                    ]
                  , elementNodes =
                    [ NodeContent ( ContentText "" )
                    , NodeElement
                      ( Element
                        { elementName = Name
                          { nameLocalName = "annotation"
                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                          , namePrefix = Just "xs"
                          }
                        , elementAttributes = []
                        , elementNodes =
                          [ NodeContent ( ContentText "" )
                          , NodeElement
                            ( Element
                              { elementName = Name
                                { nameLocalName = "documentation"
                                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                , namePrefix = Just "xs"
                                }
                              , elementAttributes =
                                [
                                 ( Name
                                   { nameLocalName = "source"
                                   , nameNamespace = Nothing
                                   , namePrefix = Nothing
                                   }
                                 , [ ContentText "http://www.w3.org/TR/xmlschema-2/#int" ]
                                 )
                                ]
                              , elementNodes = []
                              }
                            )
                          , NodeContent ( ContentText "" )
                          ]
                        }
                      )
                    , NodeContent ( ContentText "" )
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
                           , [ ContentText "xs:long" ]
                           )
                          ]
                        , elementNodes =
                          [ NodeContent ( ContentText "" )
                          , NodeElement
                            ( Element
                              { elementName = Name
                                { nameLocalName = "minInclusive"
                                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                , namePrefix = Just "xs"
                                }
                              , elementAttributes =
                                [
                                 ( Name
                                   { nameLocalName = "id"
                                   , nameNamespace = Nothing
                                   , namePrefix = Nothing
                                   }
                                 , [ ContentText "int.minInclusive" ]
                                 )
                                ,
                                 ( Name
                                   { nameLocalName = "value"
                                   , nameNamespace = Nothing
                                   , namePrefix = Nothing
                                   }
                                 , [ ContentText "-2147483648" ]
                                 )
                                ]
                              , elementNodes = []
                              }
                            )
                          , NodeContent ( ContentText "" )
                          , NodeElement
                            ( Element
                              { elementName = Name
                                { nameLocalName = "maxInclusive"
                                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                , namePrefix = Just "xs"
                                }
                              , elementAttributes =
                                [
                                 ( Name
                                   { nameLocalName = "id"
                                   , nameNamespace = Nothing
                                   , namePrefix = Nothing
                                   }
                                 , [ ContentText "int.maxInclusive" ]
                                 )
                                ,
                                 ( Name
                                   { nameLocalName = "value"
                                   , nameNamespace = Nothing
                                   , namePrefix = Nothing
                                   }
                                 , [ ContentText "2147483647" ]
                                 )
                                ]
                              , elementNodes = []
                              }
                            )
                          , NodeContent ( ContentText "" )
                          ]
                        }
                      )
                    , NodeContent ( ContentText "" )
                    ]
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
                       { nameLocalName = "id"
                       , nameNamespace = Nothing
                       , namePrefix = Nothing
                       }
                     , [ ContentText "short" ]
                     )
                    ,
                     ( Name
                       { nameLocalName = "name"
                       , nameNamespace = Nothing
                       , namePrefix = Nothing
                       }
                     , [ ContentText "short" ]
                     )
                    ]
                  , elementNodes =
                    [ NodeContent ( ContentText "" )
                    , NodeElement
                      ( Element
                        { elementName = Name
                          { nameLocalName = "annotation"
                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                          , namePrefix = Just "xs"
                          }
                        , elementAttributes = []
                        , elementNodes =
                          [ NodeContent ( ContentText "" )
                          , NodeElement
                            ( Element
                              { elementName = Name
                                { nameLocalName = "documentation"
                                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                , namePrefix = Just "xs"
                                }
                              , elementAttributes =
                                [
                                 ( Name
                                   { nameLocalName = "source"
                                   , nameNamespace = Nothing
                                   , namePrefix = Nothing
                                   }
                                 , [ ContentText "http://www.w3.org/TR/xmlschema-2/#short" ]
                                 )
                                ]
                              , elementNodes = []
                              }
                            )
                          , NodeContent ( ContentText "" )
                          ]
                        }
                      )
                    , NodeContent ( ContentText "" )
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
                           , [ ContentText "xs:int" ]
                           )
                          ]
                        , elementNodes =
                          [ NodeContent ( ContentText "" )
                          , NodeElement
                            ( Element
                              { elementName = Name
                                { nameLocalName = "minInclusive"
                                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                , namePrefix = Just "xs"
                                }
                              , elementAttributes =
                                [
                                 ( Name
                                   { nameLocalName = "id"
                                   , nameNamespace = Nothing
                                   , namePrefix = Nothing
                                   }
                                 , [ ContentText "short.minInclusive" ]
                                 )
                                ,
                                 ( Name
                                   { nameLocalName = "value"
                                   , nameNamespace = Nothing
                                   , namePrefix = Nothing
                                   }
                                 , [ ContentText "-32768" ]
                                 )
                                ]
                              , elementNodes = []
                              }
                            )
                          , NodeContent ( ContentText "" )
                          , NodeElement
                            ( Element
                              { elementName = Name
                                { nameLocalName = "maxInclusive"
                                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                , namePrefix = Just "xs"
                                }
                              , elementAttributes =
                                [
                                 ( Name
                                   { nameLocalName = "id"
                                   , nameNamespace = Nothing
                                   , namePrefix = Nothing
                                   }
                                 , [ ContentText "short.maxInclusive" ]
                                 )
                                ,
                                 ( Name
                                   { nameLocalName = "value"
                                   , nameNamespace = Nothing
                                   , namePrefix = Nothing
                                   }
                                 , [ ContentText "32767" ]
                                 )
                                ]
                              , elementNodes = []
                              }
                            )
                          , NodeContent ( ContentText "" )
                          ]
                        }
                      )
                    , NodeContent ( ContentText "" )
                    ]
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
                       { nameLocalName = "id"
                       , nameNamespace = Nothing
                       , namePrefix = Nothing
                       }
                     , [ ContentText "byte" ]
                     )
                    ,
                     ( Name
                       { nameLocalName = "name"
                       , nameNamespace = Nothing
                       , namePrefix = Nothing
                       }
                     , [ ContentText "byte" ]
                     )
                    ]
                  , elementNodes =
                    [ NodeContent ( ContentText "" )
                    , NodeElement
                      ( Element
                        { elementName = Name
                          { nameLocalName = "annotation"
                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                          , namePrefix = Just "xs"
                          }
                        , elementAttributes = []
                        , elementNodes =
                          [ NodeContent ( ContentText "" )
                          , NodeElement
                            ( Element
                              { elementName = Name
                                { nameLocalName = "documentation"
                                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                , namePrefix = Just "xs"
                                }
                              , elementAttributes =
                                [
                                 ( Name
                                   { nameLocalName = "source"
                                   , nameNamespace = Nothing
                                   , namePrefix = Nothing
                                   }
                                 , [ ContentText "http://www.w3.org/TR/xmlschema-2/#byte" ]
                                 )
                                ]
                              , elementNodes = []
                              }
                            )
                          , NodeContent ( ContentText "" )
                          ]
                        }
                      )
                    , NodeContent ( ContentText "" )
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
                           , [ ContentText "xs:short" ]
                           )
                          ]
                        , elementNodes =
                          [ NodeContent ( ContentText "" )
                          , NodeElement
                            ( Element
                              { elementName = Name
                                { nameLocalName = "minInclusive"
                                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                , namePrefix = Just "xs"
                                }
                              , elementAttributes =
                                [
                                 ( Name
                                   { nameLocalName = "id"
                                   , nameNamespace = Nothing
                                   , namePrefix = Nothing
                                   }
                                 , [ ContentText "byte.minInclusive" ]
                                 )
                                ,
                                 ( Name
                                   { nameLocalName = "value"
                                   , nameNamespace = Nothing
                                   , namePrefix = Nothing
                                   }
                                 , [ ContentText "-128" ]
                                 )
                                ]
                              , elementNodes = []
                              }
                            )
                          , NodeContent ( ContentText "" )
                          , NodeElement
                            ( Element
                              { elementName = Name
                                { nameLocalName = "maxInclusive"
                                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                , namePrefix = Just "xs"
                                }
                              , elementAttributes =
                                [
                                 ( Name
                                   { nameLocalName = "id"
                                   , nameNamespace = Nothing
                                   , namePrefix = Nothing
                                   }
                                 , [ ContentText "byte.maxInclusive" ]
                                 )
                                ,
                                 ( Name
                                   { nameLocalName = "value"
                                   , nameNamespace = Nothing
                                   , namePrefix = Nothing
                                   }
                                 , [ ContentText "127" ]
                                 )
                                ]
                              , elementNodes = []
                              }
                            )
                          , NodeContent ( ContentText "" )
                          ]
                        }
                      )
                    , NodeContent ( ContentText "" )
                    ]
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
                       { nameLocalName = "id"
                       , nameNamespace = Nothing
                       , namePrefix = Nothing
                       }
                     , [ ContentText "nonNegativeInteger" ]
                     )
                    ,
                     ( Name
                       { nameLocalName = "name"
                       , nameNamespace = Nothing
                       , namePrefix = Nothing
                       }
                     , [ ContentText "nonNegativeInteger" ]
                     )
                    ]
                  , elementNodes =
                    [ NodeContent ( ContentText "" )
                    , NodeElement
                      ( Element
                        { elementName = Name
                          { nameLocalName = "annotation"
                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                          , namePrefix = Just "xs"
                          }
                        , elementAttributes = []
                        , elementNodes =
                          [ NodeContent ( ContentText "" )
                          , NodeElement
                            ( Element
                              { elementName = Name
                                { nameLocalName = "documentation"
                                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                , namePrefix = Just "xs"
                                }
                              , elementAttributes =
                                [
                                 ( Name
                                   { nameLocalName = "source"
                                   , nameNamespace = Nothing
                                   , namePrefix = Nothing
                                   }
                                 , [ ContentText "http://www.w3.org/TR/xmlschema-2/#nonNegativeInteger" ]
                                 )
                                ]
                              , elementNodes = []
                              }
                            )
                          , NodeContent ( ContentText "" )
                          ]
                        }
                      )
                    , NodeContent ( ContentText "" )
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
                          , NodeElement
                            ( Element
                              { elementName = Name
                                { nameLocalName = "minInclusive"
                                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                , namePrefix = Just "xs"
                                }
                              , elementAttributes =
                                [
                                 ( Name
                                   { nameLocalName = "id"
                                   , nameNamespace = Nothing
                                   , namePrefix = Nothing
                                   }
                                 , [ ContentText "nonNegativeInteger.minInclusive" ]
                                 )
                                ,
                                 ( Name
                                   { nameLocalName = "value"
                                   , nameNamespace = Nothing
                                   , namePrefix = Nothing
                                   }
                                 , [ ContentText "0" ]
                                 )
                                ]
                              , elementNodes = []
                              }
                            )
                          , NodeContent ( ContentText "" )
                          ]
                        }
                      )
                    , NodeContent ( ContentText "" )
                    ]
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
                       { nameLocalName = "id"
                       , nameNamespace = Nothing
                       , namePrefix = Nothing
                       }
                     , [ ContentText "unsignedLong" ]
                     )
                    ,
                     ( Name
                       { nameLocalName = "name"
                       , nameNamespace = Nothing
                       , namePrefix = Nothing
                       }
                     , [ ContentText "unsignedLong" ]
                     )
                    ]
                  , elementNodes =
                    [ NodeContent ( ContentText "" )
                    , NodeElement
                      ( Element
                        { elementName = Name
                          { nameLocalName = "annotation"
                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                          , namePrefix = Just "xs"
                          }
                        , elementAttributes = []
                        , elementNodes =
                          [ NodeContent ( ContentText "" )
                          , NodeElement
                            ( Element
                              { elementName = Name
                                { nameLocalName = "appinfo"
                                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                , namePrefix = Just "xs"
                                }
                              , elementAttributes = []
                              , elementNodes =
                                [ NodeContent ( ContentText "" )
                                , NodeElement
                                  ( Element
                                    { elementName = Name
                                      { nameLocalName = "hasProperty"
                                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                                      , namePrefix = Just "hfp"
                                      }
                                    , elementAttributes =
                                      [
                                       ( Name
                                         { nameLocalName = "value"
                                         , nameNamespace = Nothing
                                         , namePrefix = Nothing
                                         }
                                       , [ ContentText "true" ]
                                       )
                                      ,
                                       ( Name
                                         { nameLocalName = "name"
                                         , nameNamespace = Nothing
                                         , namePrefix = Nothing
                                         }
                                       , [ ContentText "bounded" ]
                                       )
                                      ]
                                    , elementNodes = []
                                    }
                                  )
                                , NodeContent ( ContentText "" )
                                , NodeElement
                                  ( Element
                                    { elementName = Name
                                      { nameLocalName = "hasProperty"
                                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema-hasFacetAndProperty"
                                      , namePrefix = Just "hfp"
                                      }
                                    , elementAttributes =
                                      [
                                       ( Name
                                         { nameLocalName = "value"
                                         , nameNamespace = Nothing
                                         , namePrefix = Nothing
                                         }
                                       , [ ContentText "finite" ]
                                       )
                                      ,
                                       ( Name
                                         { nameLocalName = "name"
                                         , nameNamespace = Nothing
                                         , namePrefix = Nothing
                                         }
                                       , [ ContentText "cardinality" ]
                                       )
                                      ]
                                    , elementNodes = []
                                    }
                                  )
                                , NodeContent ( ContentText "" )
                                ]
                              }
                            )
                          , NodeContent ( ContentText "" )
                          , NodeElement
                            ( Element
                              { elementName = Name
                                { nameLocalName = "documentation"
                                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                , namePrefix = Just "xs"
                                }
                              , elementAttributes =
                                [
                                 ( Name
                                   { nameLocalName = "source"
                                   , nameNamespace = Nothing
                                   , namePrefix = Nothing
                                   }
                                 , [ ContentText "http://www.w3.org/TR/xmlschema-2/#unsignedLong" ]
                                 )
                                ]
                              , elementNodes = []
                              }
                            )
                          , NodeContent ( ContentText "" )
                          ]
                        }
                      )
                    , NodeContent ( ContentText "" )
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
                           , [ ContentText "xs:nonNegativeInteger" ]
                           )
                          ]
                        , elementNodes =
                          [ NodeContent ( ContentText "" )
                          , NodeElement
                            ( Element
                              { elementName = Name
                                { nameLocalName = "maxInclusive"
                                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                , namePrefix = Just "xs"
                                }
                              , elementAttributes =
                                [
                                 ( Name
                                   { nameLocalName = "id"
                                   , nameNamespace = Nothing
                                   , namePrefix = Nothing
                                   }
                                 , [ ContentText "unsignedLong.maxInclusive" ]
                                 )
                                ,
                                 ( Name
                                   { nameLocalName = "value"
                                   , nameNamespace = Nothing
                                   , namePrefix = Nothing
                                   }
                                 , [ ContentText "18446744073709551615" ]
                                 )
                                ]
                              , elementNodes = []
                              }
                            )
                          , NodeContent ( ContentText "" )
                          ]
                        }
                      )
                    , NodeContent ( ContentText "" )
                    ]
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
                       { nameLocalName = "id"
                       , nameNamespace = Nothing
                       , namePrefix = Nothing
                       }
                     , [ ContentText "unsignedInt" ]
                     )
                    ,
                     ( Name
                       { nameLocalName = "name"
                       , nameNamespace = Nothing
                       , namePrefix = Nothing
                       }
                     , [ ContentText "unsignedInt" ]
                     )
                    ]
                  , elementNodes =
                    [ NodeContent ( ContentText "" )
                    , NodeElement
                      ( Element
                        { elementName = Name
                          { nameLocalName = "annotation"
                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                          , namePrefix = Just "xs"
                          }
                        , elementAttributes = []
                        , elementNodes =
                          [ NodeContent ( ContentText "" )
                          , NodeElement
                            ( Element
                              { elementName = Name
                                { nameLocalName = "documentation"
                                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                , namePrefix = Just "xs"
                                }
                              , elementAttributes =
                                [
                                 ( Name
                                   { nameLocalName = "source"
                                   , nameNamespace = Nothing
                                   , namePrefix = Nothing
                                   }
                                 , [ ContentText "http://www.w3.org/TR/xmlschema-2/#unsignedInt" ]
                                 )
                                ]
                              , elementNodes = []
                              }
                            )
                          , NodeContent ( ContentText "" )
                          ]
                        }
                      )
                    , NodeContent ( ContentText "" )
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
                           , [ ContentText "xs:unsignedLong" ]
                           )
                          ]
                        , elementNodes =
                          [ NodeContent ( ContentText "" )
                          , NodeElement
                            ( Element
                              { elementName = Name
                                { nameLocalName = "maxInclusive"
                                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                , namePrefix = Just "xs"
                                }
                              , elementAttributes =
                                [
                                 ( Name
                                   { nameLocalName = "id"
                                   , nameNamespace = Nothing
                                   , namePrefix = Nothing
                                   }
                                 , [ ContentText "unsignedInt.maxInclusive" ]
                                 )
                                ,
                                 ( Name
                                   { nameLocalName = "value"
                                   , nameNamespace = Nothing
                                   , namePrefix = Nothing
                                   }
                                 , [ ContentText "4294967295" ]
                                 )
                                ]
                              , elementNodes = []
                              }
                            )
                          , NodeContent ( ContentText "" )
                          ]
                        }
                      )
                    , NodeContent ( ContentText "" )
                    ]
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
                       { nameLocalName = "id"
                       , nameNamespace = Nothing
                       , namePrefix = Nothing
                       }
                     , [ ContentText "unsignedShort" ]
                     )
                    ,
                     ( Name
                       { nameLocalName = "name"
                       , nameNamespace = Nothing
                       , namePrefix = Nothing
                       }
                     , [ ContentText "unsignedShort" ]
                     )
                    ]
                  , elementNodes =
                    [ NodeContent ( ContentText "" )
                    , NodeElement
                      ( Element
                        { elementName = Name
                          { nameLocalName = "annotation"
                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                          , namePrefix = Just "xs"
                          }
                        , elementAttributes = []
                        , elementNodes =
                          [ NodeContent ( ContentText "" )
                          , NodeElement
                            ( Element
                              { elementName = Name
                                { nameLocalName = "documentation"
                                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                , namePrefix = Just "xs"
                                }
                              , elementAttributes =
                                [
                                 ( Name
                                   { nameLocalName = "source"
                                   , nameNamespace = Nothing
                                   , namePrefix = Nothing
                                   }
                                 , [ ContentText "http://www.w3.org/TR/xmlschema-2/#unsignedShort" ]
                                 )
                                ]
                              , elementNodes = []
                              }
                            )
                          , NodeContent ( ContentText "" )
                          ]
                        }
                      )
                    , NodeContent ( ContentText "" )
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
                           , [ ContentText "xs:unsignedInt" ]
                           )
                          ]
                        , elementNodes =
                          [ NodeContent ( ContentText "" )
                          , NodeElement
                            ( Element
                              { elementName = Name
                                { nameLocalName = "maxInclusive"
                                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                , namePrefix = Just "xs"
                                }
                              , elementAttributes =
                                [
                                 ( Name
                                   { nameLocalName = "id"
                                   , nameNamespace = Nothing
                                   , namePrefix = Nothing
                                   }
                                 , [ ContentText "unsignedShort.maxInclusive" ]
                                 )
                                ,
                                 ( Name
                                   { nameLocalName = "value"
                                   , nameNamespace = Nothing
                                   , namePrefix = Nothing
                                   }
                                 , [ ContentText "65535" ]
                                 )
                                ]
                              , elementNodes = []
                              }
                            )
                          , NodeContent ( ContentText "" )
                          ]
                        }
                      )
                    , NodeContent ( ContentText "" )
                    ]
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
                       { nameLocalName = "id"
                       , nameNamespace = Nothing
                       , namePrefix = Nothing
                       }
                     , [ ContentText "unsignedByte" ]
                     )
                    ,
                     ( Name
                       { nameLocalName = "name"
                       , nameNamespace = Nothing
                       , namePrefix = Nothing
                       }
                     , [ ContentText "unsignedByte" ]
                     )
                    ]
                  , elementNodes =
                    [ NodeContent ( ContentText "" )
                    , NodeElement
                      ( Element
                        { elementName = Name
                          { nameLocalName = "annotation"
                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                          , namePrefix = Just "xs"
                          }
                        , elementAttributes = []
                        , elementNodes =
                          [ NodeContent ( ContentText "" )
                          , NodeElement
                            ( Element
                              { elementName = Name
                                { nameLocalName = "documentation"
                                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                , namePrefix = Just "xs"
                                }
                              , elementAttributes =
                                [
                                 ( Name
                                   { nameLocalName = "source"
                                   , nameNamespace = Nothing
                                   , namePrefix = Nothing
                                   }
                                 , [ ContentText "http://www.w3.org/TR/xmlschema-2/#unsignedByte" ]
                                 )
                                ]
                              , elementNodes = []
                              }
                            )
                          , NodeContent ( ContentText "" )
                          ]
                        }
                      )
                    , NodeContent ( ContentText "" )
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
                           , [ ContentText "xs:unsignedShort" ]
                           )
                          ]
                        , elementNodes =
                          [ NodeContent ( ContentText "" )
                          , NodeElement
                            ( Element
                              { elementName = Name
                                { nameLocalName = "maxInclusive"
                                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                , namePrefix = Just "xs"
                                }
                              , elementAttributes =
                                [
                                 ( Name
                                   { nameLocalName = "id"
                                   , nameNamespace = Nothing
                                   , namePrefix = Nothing
                                   }
                                 , [ ContentText "unsignedByte.maxInclusive" ]
                                 )
                                ,
                                 ( Name
                                   { nameLocalName = "value"
                                   , nameNamespace = Nothing
                                   , namePrefix = Nothing
                                   }
                                 , [ ContentText "255" ]
                                 )
                                ]
                              , elementNodes = []
                              }
                            )
                          , NodeContent ( ContentText "" )
                          ]
                        }
                      )
                    , NodeContent ( ContentText "" )
                    ]
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
                       { nameLocalName = "id"
                       , nameNamespace = Nothing
                       , namePrefix = Nothing
                       }
                     , [ ContentText "positiveInteger" ]
                     )
                    ,
                     ( Name
                       { nameLocalName = "name"
                       , nameNamespace = Nothing
                       , namePrefix = Nothing
                       }
                     , [ ContentText "positiveInteger" ]
                     )
                    ]
                  , elementNodes =
                    [ NodeContent ( ContentText "" )
                    , NodeElement
                      ( Element
                        { elementName = Name
                          { nameLocalName = "annotation"
                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                          , namePrefix = Just "xs"
                          }
                        , elementAttributes = []
                        , elementNodes =
                          [ NodeContent ( ContentText "" )
                          , NodeElement
                            ( Element
                              { elementName = Name
                                { nameLocalName = "documentation"
                                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                , namePrefix = Just "xs"
                                }
                              , elementAttributes =
                                [
                                 ( Name
                                   { nameLocalName = "source"
                                   , nameNamespace = Nothing
                                   , namePrefix = Nothing
                                   }
                                 , [ ContentText "http://www.w3.org/TR/xmlschema-2/#positiveInteger" ]
                                 )
                                ]
                              , elementNodes = []
                              }
                            )
                          , NodeContent ( ContentText "" )
                          ]
                        }
                      )
                    , NodeContent ( ContentText "" )
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
                           , [ ContentText "xs:nonNegativeInteger" ]
                           )
                          ]
                        , elementNodes =
                          [ NodeContent ( ContentText "" )
                          , NodeElement
                            ( Element
                              { elementName = Name
                                { nameLocalName = "minInclusive"
                                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                , namePrefix = Just "xs"
                                }
                              , elementAttributes =
                                [
                                 ( Name
                                   { nameLocalName = "id"
                                   , nameNamespace = Nothing
                                   , namePrefix = Nothing
                                   }
                                 , [ ContentText "positiveInteger.minInclusive" ]
                                 )
                                ,
                                 ( Name
                                   { nameLocalName = "value"
                                   , nameNamespace = Nothing
                                   , namePrefix = Nothing
                                   }
                                 , [ ContentText "1" ]
                                 )
                                ]
                              , elementNodes = []
                              }
                            )
                          , NodeContent ( ContentText "" )
                          ]
                        }
                      )
                    , NodeContent ( ContentText "" )
                    ]
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
                     , [ ContentText "derivationControl" ]
                     )
                    ]
                  , elementNodes =
                    [ NodeContent ( ContentText "" )
                    , NodeElement
                      ( Element
                        { elementName = Name
                          { nameLocalName = "annotation"
                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                          , namePrefix = Just "xs"
                          }
                        , elementAttributes = []
                        , elementNodes =
                          [ NodeContent ( ContentText "" )
                          , NodeElement
                            ( Element
                              { elementName = Name
                                { nameLocalName = "documentation"
                                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                , namePrefix = Just "xs"
                                }
                              , elementAttributes = []
                              , elementNodes =
                                [ NodeContent ( ContentText "A utility type, not for public use" ) ]
                              }
                            )
                          , NodeContent ( ContentText "" )
                          ]
                        }
                      )
                    , NodeContent ( ContentText "" )
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
                           , [ ContentText "xs:NMTOKEN" ]
                           )
                          ]
                        , elementNodes =
                          [ NodeContent ( ContentText "" )
                          , NodeElement
                            ( Element
                              { elementName = Name
                                { nameLocalName = "enumeration"
                                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                , namePrefix = Just "xs"
                                }
                              , elementAttributes =
                                [
                                 ( Name
                                   { nameLocalName = "value"
                                   , nameNamespace = Nothing
                                   , namePrefix = Nothing
                                   }
                                 , [ ContentText "substitution" ]
                                 )
                                ]
                              , elementNodes = []
                              }
                            )
                          , NodeContent ( ContentText "" )
                          , NodeElement
                            ( Element
                              { elementName = Name
                                { nameLocalName = "enumeration"
                                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                , namePrefix = Just "xs"
                                }
                              , elementAttributes =
                                [
                                 ( Name
                                   { nameLocalName = "value"
                                   , nameNamespace = Nothing
                                   , namePrefix = Nothing
                                   }
                                 , [ ContentText "extension" ]
                                 )
                                ]
                              , elementNodes = []
                              }
                            )
                          , NodeContent ( ContentText "" )
                          , NodeElement
                            ( Element
                              { elementName = Name
                                { nameLocalName = "enumeration"
                                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                , namePrefix = Just "xs"
                                }
                              , elementAttributes =
                                [
                                 ( Name
                                   { nameLocalName = "value"
                                   , nameNamespace = Nothing
                                   , namePrefix = Nothing
                                   }
                                 , [ ContentText "restriction" ]
                                 )
                                ]
                              , elementNodes = []
                              }
                            )
                          , NodeContent ( ContentText "" )
                          , NodeElement
                            ( Element
                              { elementName = Name
                                { nameLocalName = "enumeration"
                                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                , namePrefix = Just "xs"
                                }
                              , elementAttributes =
                                [
                                 ( Name
                                   { nameLocalName = "value"
                                   , nameNamespace = Nothing
                                   , namePrefix = Nothing
                                   }
                                 , [ ContentText "list" ]
                                 )
                                ]
                              , elementNodes = []
                              }
                            )
                          , NodeContent ( ContentText "" )
                          , NodeElement
                            ( Element
                              { elementName = Name
                                { nameLocalName = "enumeration"
                                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                , namePrefix = Just "xs"
                                }
                              , elementAttributes =
                                [
                                 ( Name
                                   { nameLocalName = "value"
                                   , nameNamespace = Nothing
                                   , namePrefix = Nothing
                                   }
                                 , [ ContentText "union" ]
                                 )
                                ]
                              , elementNodes = []
                              }
                            )
                          , NodeContent ( ContentText "" )
                          ]
                        }
                      )
                    , NodeContent ( ContentText "" )
                    ]
                  }
                )
              , NodeContent ( ContentText "" )
              , NodeElement
                ( Element
                  { elementName = Name
                    { nameLocalName = "group"
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
                     , [ ContentText "simpleDerivation" ]
                     )
                    ]
                  , elementNodes =
                    [ NodeContent ( ContentText "" )
                    , NodeElement
                      ( Element
                        { elementName = Name
                          { nameLocalName = "choice"
                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                          , namePrefix = Just "xs"
                          }
                        , elementAttributes = []
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
                                   { nameLocalName = "ref"
                                   , nameNamespace = Nothing
                                   , namePrefix = Nothing
                                   }
                                 , [ ContentText "xs:restriction" ]
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
                                   { nameLocalName = "ref"
                                   , nameNamespace = Nothing
                                   , namePrefix = Nothing
                                   }
                                 , [ ContentText "xs:list" ]
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
                                   { nameLocalName = "ref"
                                   , nameNamespace = Nothing
                                   , namePrefix = Nothing
                                   }
                                 , [ ContentText "xs:union" ]
                                 )
                                ]
                              , elementNodes = []
                              }
                            )
                          , NodeContent ( ContentText "" )
                          ]
                        }
                      )
                    , NodeContent ( ContentText "" )
                    ]
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
                     , [ ContentText "simpleDerivationSet" ]
                     )
                    ]
                  , elementNodes =
                    [ NodeContent ( ContentText "" )
                    , NodeElement
                      ( Element
                        { elementName = Name
                          { nameLocalName = "annotation"
                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                          , namePrefix = Just "xs"
                          }
                        , elementAttributes = []
                        , elementNodes =
                          [ NodeContent ( ContentText "" )
                          , NodeElement
                            ( Element
                              { elementName = Name
                                { nameLocalName = "documentation"
                                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                , namePrefix = Just "xs"
                                }
                              , elementAttributes = []
                              , elementNodes =
                                [ NodeContent ( ContentText "#all or (possibly empty) subset of {restriction, union, list}" ) ]
                              }
                            )
                          , NodeContent ( ContentText "" )
                          , NodeElement
                            ( Element
                              { elementName = Name
                                { nameLocalName = "documentation"
                                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                , namePrefix = Just "xs"
                                }
                              , elementAttributes = []
                              , elementNodes =
                                [ NodeContent ( ContentText "A utility type, not for public use" ) ]
                              }
                            )
                          , NodeContent ( ContentText "" )
                          ]
                        }
                      )
                    , NodeContent ( ContentText "" )
                    , NodeElement
                      ( Element
                        { elementName = Name
                          { nameLocalName = "union"
                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                          , namePrefix = Just "xs"
                          }
                        , elementAttributes = []
                        , elementNodes =
                          [ NodeContent ( ContentText "" )
                          , NodeElement
                            ( Element
                              { elementName = Name
                                { nameLocalName = "simpleType"
                                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                , namePrefix = Just "xs"
                                }
                              , elementAttributes = []
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
                                       , [ ContentText "xs:token" ]
                                       )
                                      ]
                                    , elementNodes =
                                      [ NodeContent ( ContentText "" )
                                      , NodeElement
                                        ( Element
                                          { elementName = Name
                                            { nameLocalName = "enumeration"
                                            , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                            , namePrefix = Just "xs"
                                            }
                                          , elementAttributes =
                                            [
                                             ( Name
                                               { nameLocalName = "value"
                                               , nameNamespace = Nothing
                                               , namePrefix = Nothing
                                               }
                                             , [ ContentText "#all" ]
                                             )
                                            ]
                                          , elementNodes = []
                                          }
                                        )
                                      , NodeContent ( ContentText "" )
                                      ]
                                    }
                                  )
                                , NodeContent ( ContentText "" )
                                ]
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
                              , elementAttributes = []
                              , elementNodes =
                                [ NodeContent ( ContentText "" )
                                , NodeElement
                                  ( Element
                                    { elementName = Name
                                      { nameLocalName = "list"
                                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                      , namePrefix = Just "xs"
                                      }
                                    , elementAttributes = []
                                    , elementNodes =
                                      [ NodeContent ( ContentText "" )
                                      , NodeElement
                                        ( Element
                                          { elementName = Name
                                            { nameLocalName = "simpleType"
                                            , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                            , namePrefix = Just "xs"
                                            }
                                          , elementAttributes = []
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
                                                   , [ ContentText "xs:derivationControl" ]
                                                   )
                                                  ]
                                                , elementNodes =
                                                  [ NodeContent ( ContentText "" )
                                                  , NodeElement
                                                    ( Element
                                                      { elementName = Name
                                                        { nameLocalName = "enumeration"
                                                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                                        , namePrefix = Just "xs"
                                                        }
                                                      , elementAttributes =
                                                        [
                                                         ( Name
                                                           { nameLocalName = "value"
                                                           , nameNamespace = Nothing
                                                           , namePrefix = Nothing
                                                           }
                                                         , [ ContentText "list" ]
                                                         )
                                                        ]
                                                      , elementNodes = []
                                                      }
                                                    )
                                                  , NodeContent ( ContentText "" )
                                                  , NodeElement
                                                    ( Element
                                                      { elementName = Name
                                                        { nameLocalName = "enumeration"
                                                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                                        , namePrefix = Just "xs"
                                                        }
                                                      , elementAttributes =
                                                        [
                                                         ( Name
                                                           { nameLocalName = "value"
                                                           , nameNamespace = Nothing
                                                           , namePrefix = Nothing
                                                           }
                                                         , [ ContentText "union" ]
                                                         )
                                                        ]
                                                      , elementNodes = []
                                                      }
                                                    )
                                                  , NodeContent ( ContentText "" )
                                                  , NodeElement
                                                    ( Element
                                                      { elementName = Name
                                                        { nameLocalName = "enumeration"
                                                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                                        , namePrefix = Just "xs"
                                                        }
                                                      , elementAttributes =
                                                        [
                                                         ( Name
                                                           { nameLocalName = "value"
                                                           , nameNamespace = Nothing
                                                           , namePrefix = Nothing
                                                           }
                                                         , [ ContentText "restriction" ]
                                                         )
                                                        ]
                                                      , elementNodes = []
                                                      }
                                                    )
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
                                  )
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
                )
              , NodeContent ( ContentText "" )
              , NodeElement
                ( Element
                  { elementName = Name
                    { nameLocalName = "complexType"
                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                    , namePrefix = Just "xs"
                    }
                  , elementAttributes =
                    [
                     ( Name
                       { nameLocalName = "abstract"
                       , nameNamespace = Nothing
                       , namePrefix = Nothing
                       }
                     , [ ContentText "true" ]
                     )
                    ,
                     ( Name
                       { nameLocalName = "name"
                       , nameNamespace = Nothing
                       , namePrefix = Nothing
                       }
                     , [ ContentText "simpleType" ]
                     )
                    ]
                  , elementNodes =
                    [ NodeContent ( ContentText "" )
                    , NodeElement
                      ( Element
                        { elementName = Name
                          { nameLocalName = "complexContent"
                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                          , namePrefix = Just "xs"
                          }
                        , elementAttributes = []
                        , elementNodes =
                          [ NodeContent ( ContentText "" )
                          , NodeElement
                            ( Element
                              { elementName = Name
                                { nameLocalName = "extension"
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
                                 , [ ContentText "xs:annotated" ]
                                 )
                                ]
                              , elementNodes =
                                [ NodeContent ( ContentText "" )
                                , NodeElement
                                  ( Element
                                    { elementName = Name
                                      { nameLocalName = "group"
                                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                      , namePrefix = Just "xs"
                                      }
                                    , elementAttributes =
                                      [
                                       ( Name
                                         { nameLocalName = "ref"
                                         , nameNamespace = Nothing
                                         , namePrefix = Nothing
                                         }
                                       , [ ContentText "xs:simpleDerivation" ]
                                       )
                                      ]
                                    , elementNodes = []
                                    }
                                  )
                                , NodeContent ( ContentText "" )
                                , NodeElement
                                  ( Element
                                    { elementName = Name
                                      { nameLocalName = "attribute"
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
                                       , [ ContentText "xs:simpleDerivationSet" ]
                                       )
                                      ,
                                       ( Name
                                         { nameLocalName = "name"
                                         , nameNamespace = Nothing
                                         , namePrefix = Nothing
                                         }
                                       , [ ContentText "final" ]
                                       )
                                      ]
                                    , elementNodes = []
                                    }
                                  )
                                , NodeContent ( ContentText "" )
                                , NodeElement
                                  ( Element
                                    { elementName = Name
                                      { nameLocalName = "attribute"
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
                                       , [ ContentText "xs:NCName" ]
                                       )
                                      ,
                                       ( Name
                                         { nameLocalName = "name"
                                         , nameNamespace = Nothing
                                         , namePrefix = Nothing
                                         }
                                       , [ ContentText "name" ]
                                       )
                                      ]
                                    , elementNodes =
                                      [ NodeContent ( ContentText "" )
                                      , NodeElement
                                        ( Element
                                          { elementName = Name
                                            { nameLocalName = "annotation"
                                            , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                            , namePrefix = Just "xs"
                                            }
                                          , elementAttributes = []
                                          , elementNodes =
                                            [ NodeContent ( ContentText "" )
                                            , NodeElement
                                              ( Element
                                                { elementName = Name
                                                  { nameLocalName = "documentation"
                                                  , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                                  , namePrefix = Just "xs"
                                                  }
                                                , elementAttributes = []
                                                , elementNodes =
                                                  [ NodeContent ( ContentText "Can be restricted to required or forbidden" ) ]
                                                }
                                              )
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
                            )
                          , NodeContent ( ContentText "" )
                          ]
                        }
                      )
                    , NodeContent ( ContentText "" )
                    ]
                  }
                )
              , NodeContent ( ContentText "" )
              , NodeElement
                ( Element
                  { elementName = Name
                    { nameLocalName = "complexType"
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
                     , [ ContentText "topLevelSimpleType" ]
                     )
                    ]
                  , elementNodes =
                    [ NodeContent ( ContentText "" )
                    , NodeElement
                      ( Element
                        { elementName = Name
                          { nameLocalName = "complexContent"
                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                          , namePrefix = Just "xs"
                          }
                        , elementAttributes = []
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
                                 , [ ContentText "xs:simpleType" ]
                                 )
                                ]
                              , elementNodes =
                                [ NodeContent ( ContentText "" )
                                , NodeElement
                                  ( Element
                                    { elementName = Name
                                      { nameLocalName = "sequence"
                                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                      , namePrefix = Just "xs"
                                      }
                                    , elementAttributes = []
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
                                               { nameLocalName = "minOccurs"
                                               , nameNamespace = Nothing
                                               , namePrefix = Nothing
                                               }
                                             , [ ContentText "0" ]
                                             )
                                            ,
                                             ( Name
                                               { nameLocalName = "ref"
                                               , nameNamespace = Nothing
                                               , namePrefix = Nothing
                                               }
                                             , [ ContentText "xs:annotation" ]
                                             )
                                            ]
                                          , elementNodes = []
                                          }
                                        )
                                      , NodeContent ( ContentText "" )
                                      , NodeElement
                                        ( Element
                                          { elementName = Name
                                            { nameLocalName = "group"
                                            , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                            , namePrefix = Just "xs"
                                            }
                                          , elementAttributes =
                                            [
                                             ( Name
                                               { nameLocalName = "ref"
                                               , nameNamespace = Nothing
                                               , namePrefix = Nothing
                                               }
                                             , [ ContentText "xs:simpleDerivation" ]
                                             )
                                            ]
                                          , elementNodes = []
                                          }
                                        )
                                      , NodeContent ( ContentText "" )
                                      ]
                                    }
                                  )
                                , NodeContent ( ContentText "" )
                                , NodeElement
                                  ( Element
                                    { elementName = Name
                                      { nameLocalName = "attribute"
                                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                      , namePrefix = Just "xs"
                                      }
                                    , elementAttributes =
                                      [
                                       ( Name
                                         { nameLocalName = "use"
                                         , nameNamespace = Nothing
                                         , namePrefix = Nothing
                                         }
                                       , [ ContentText "required" ]
                                       )
                                      ,
                                       ( Name
                                         { nameLocalName = "type"
                                         , nameNamespace = Nothing
                                         , namePrefix = Nothing
                                         }
                                       , [ ContentText "xs:NCName" ]
                                       )
                                      ,
                                       ( Name
                                         { nameLocalName = "name"
                                         , nameNamespace = Nothing
                                         , namePrefix = Nothing
                                         }
                                       , [ ContentText "name" ]
                                       )
                                      ]
                                    , elementNodes =
                                      [ NodeContent ( ContentText "" )
                                      , NodeElement
                                        ( Element
                                          { elementName = Name
                                            { nameLocalName = "annotation"
                                            , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                            , namePrefix = Just "xs"
                                            }
                                          , elementAttributes = []
                                          , elementNodes =
                                            [ NodeContent ( ContentText "" )
                                            , NodeElement
                                              ( Element
                                                { elementName = Name
                                                  { nameLocalName = "documentation"
                                                  , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                                  , namePrefix = Just "xs"
                                                  }
                                                , elementAttributes = []
                                                , elementNodes =
                                                  [ NodeContent ( ContentText "Required at the top level" ) ]
                                                }
                                              )
                                            , NodeContent ( ContentText "" )
                                            ]
                                          }
                                        )
                                      , NodeContent ( ContentText "" )
                                      ]
                                    }
                                  )
                                , NodeContent ( ContentText "" )
                                , NodeElement
                                  ( Element
                                    { elementName = Name
                                      { nameLocalName = "anyAttribute"
                                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                      , namePrefix = Just "xs"
                                      }
                                    , elementAttributes =
                                      [
                                       ( Name
                                         { nameLocalName = "processContents"
                                         , nameNamespace = Nothing
                                         , namePrefix = Nothing
                                         }
                                       , [ ContentText "lax" ]
                                       )
                                      ,
                                       ( Name
                                         { nameLocalName = "namespace"
                                         , nameNamespace = Nothing
                                         , namePrefix = Nothing
                                         }
                                       , [ ContentText "##other" ]
                                       )
                                      ]
                                    , elementNodes = []
                                    }
                                  )
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
                )
              , NodeContent ( ContentText "" )
              , NodeElement
                ( Element
                  { elementName = Name
                    { nameLocalName = "complexType"
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
                     , [ ContentText "localSimpleType" ]
                     )
                    ]
                  , elementNodes =
                    [ NodeContent ( ContentText "" )
                    , NodeElement
                      ( Element
                        { elementName = Name
                          { nameLocalName = "complexContent"
                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                          , namePrefix = Just "xs"
                          }
                        , elementAttributes = []
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
                                 , [ ContentText "xs:simpleType" ]
                                 )
                                ]
                              , elementNodes =
                                [ NodeContent ( ContentText "" )
                                , NodeElement
                                  ( Element
                                    { elementName = Name
                                      { nameLocalName = "sequence"
                                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                      , namePrefix = Just "xs"
                                      }
                                    , elementAttributes = []
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
                                               { nameLocalName = "minOccurs"
                                               , nameNamespace = Nothing
                                               , namePrefix = Nothing
                                               }
                                             , [ ContentText "0" ]
                                             )
                                            ,
                                             ( Name
                                               { nameLocalName = "ref"
                                               , nameNamespace = Nothing
                                               , namePrefix = Nothing
                                               }
                                             , [ ContentText "xs:annotation" ]
                                             )
                                            ]
                                          , elementNodes = []
                                          }
                                        )
                                      , NodeContent ( ContentText "" )
                                      , NodeElement
                                        ( Element
                                          { elementName = Name
                                            { nameLocalName = "group"
                                            , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                            , namePrefix = Just "xs"
                                            }
                                          , elementAttributes =
                                            [
                                             ( Name
                                               { nameLocalName = "ref"
                                               , nameNamespace = Nothing
                                               , namePrefix = Nothing
                                               }
                                             , [ ContentText "xs:simpleDerivation" ]
                                             )
                                            ]
                                          , elementNodes = []
                                          }
                                        )
                                      , NodeContent ( ContentText "" )
                                      ]
                                    }
                                  )
                                , NodeContent ( ContentText "" )
                                , NodeElement
                                  ( Element
                                    { elementName = Name
                                      { nameLocalName = "attribute"
                                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                      , namePrefix = Just "xs"
                                      }
                                    , elementAttributes =
                                      [
                                       ( Name
                                         { nameLocalName = "use"
                                         , nameNamespace = Nothing
                                         , namePrefix = Nothing
                                         }
                                       , [ ContentText "prohibited" ]
                                       )
                                      ,
                                       ( Name
                                         { nameLocalName = "name"
                                         , nameNamespace = Nothing
                                         , namePrefix = Nothing
                                         }
                                       , [ ContentText "name" ]
                                       )
                                      ]
                                    , elementNodes =
                                      [ NodeContent ( ContentText "" )
                                      , NodeElement
                                        ( Element
                                          { elementName = Name
                                            { nameLocalName = "annotation"
                                            , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                            , namePrefix = Just "xs"
                                            }
                                          , elementAttributes = []
                                          , elementNodes =
                                            [ NodeContent ( ContentText "" )
                                            , NodeElement
                                              ( Element
                                                { elementName = Name
                                                  { nameLocalName = "documentation"
                                                  , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                                  , namePrefix = Just "xs"
                                                  }
                                                , elementAttributes = []
                                                , elementNodes =
                                                  [ NodeContent ( ContentText "Forbidden when nested" ) ]
                                                }
                                              )
                                            , NodeContent ( ContentText "" )
                                            ]
                                          }
                                        )
                                      , NodeContent ( ContentText "" )
                                      ]
                                    }
                                  )
                                , NodeContent ( ContentText "" )
                                , NodeElement
                                  ( Element
                                    { elementName = Name
                                      { nameLocalName = "attribute"
                                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                      , namePrefix = Just "xs"
                                      }
                                    , elementAttributes =
                                      [
                                       ( Name
                                         { nameLocalName = "use"
                                         , nameNamespace = Nothing
                                         , namePrefix = Nothing
                                         }
                                       , [ ContentText "prohibited" ]
                                       )
                                      ,
                                       ( Name
                                         { nameLocalName = "name"
                                         , nameNamespace = Nothing
                                         , namePrefix = Nothing
                                         }
                                       , [ ContentText "final" ]
                                       )
                                      ]
                                    , elementNodes = []
                                    }
                                  )
                                , NodeContent ( ContentText "" )
                                , NodeElement
                                  ( Element
                                    { elementName = Name
                                      { nameLocalName = "anyAttribute"
                                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                      , namePrefix = Just "xs"
                                      }
                                    , elementAttributes =
                                      [
                                       ( Name
                                         { nameLocalName = "processContents"
                                         , nameNamespace = Nothing
                                         , namePrefix = Nothing
                                         }
                                       , [ ContentText "lax" ]
                                       )
                                      ,
                                       ( Name
                                         { nameLocalName = "namespace"
                                         , nameNamespace = Nothing
                                         , namePrefix = Nothing
                                         }
                                       , [ ContentText "##other" ]
                                       )
                                      ]
                                    , elementNodes = []
                                    }
                                  )
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
                       { nameLocalName = "id"
                       , nameNamespace = Nothing
                       , namePrefix = Nothing
                       }
                     , [ ContentText "simpleType" ]
                     )
                    ,
                     ( Name
                       { nameLocalName = "type"
                       , nameNamespace = Nothing
                       , namePrefix = Nothing
                       }
                     , [ ContentText "xs:topLevelSimpleType" ]
                     )
                    ,
                     ( Name
                       { nameLocalName = "name"
                       , nameNamespace = Nothing
                       , namePrefix = Nothing
                       }
                     , [ ContentText "simpleType" ]
                     )
                    ]
                  , elementNodes =
                    [ NodeContent ( ContentText "" )
                    , NodeElement
                      ( Element
                        { elementName = Name
                          { nameLocalName = "annotation"
                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                          , namePrefix = Just "xs"
                          }
                        , elementAttributes = []
                        , elementNodes =
                          [ NodeContent ( ContentText "" )
                          , NodeElement
                            ( Element
                              { elementName = Name
                                { nameLocalName = "documentation"
                                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                , namePrefix = Just "xs"
                                }
                              , elementAttributes =
                                [
                                 ( Name
                                   { nameLocalName = "source"
                                   , nameNamespace = Nothing
                                   , namePrefix = Nothing
                                   }
                                 , [ ContentText "http://www.w3.org/TR/xmlschema-2/#element-simpleType" ]
                                 )
                                ]
                              , elementNodes = []
                              }
                            )
                          , NodeContent ( ContentText "" )
                          ]
                        }
                      )
                    , NodeContent ( ContentText "" )
                    ]
                  }
                )
              , NodeContent ( ContentText "" )
              , NodeElement
                ( Element
                  { elementName = Name
                    { nameLocalName = "group"
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
                     , [ ContentText "facets" ]
                     )
                    ]
                  , elementNodes =
                    [ NodeContent ( ContentText "" )
                    , NodeElement
                      ( Element
                        { elementName = Name
                          { nameLocalName = "annotation"
                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                          , namePrefix = Just "xs"
                          }
                        , elementAttributes = []
                        , elementNodes =
                          [ NodeContent ( ContentText "" )
                          , NodeElement
                            ( Element
                              { elementName = Name
                                { nameLocalName = "documentation"
                                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                , namePrefix = Just "xs"
                                }
                              , elementAttributes = []
                              , elementNodes =
                                [ NodeContent
                                  ( ContentText
                                    "We should use a substitution group for facets, but"
                                  \ "that's ruled out because it would allow users to"
                                  \ "add their own, which we're not ready for yet."
                                  )
                                ]
                              }
                            )
                          , NodeContent ( ContentText "" )
                          ]
                        }
                      )
                    , NodeContent ( ContentText "" )
                    , NodeElement
                      ( Element
                        { elementName = Name
                          { nameLocalName = "choice"
                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                          , namePrefix = Just "xs"
                          }
                        , elementAttributes = []
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
                                   { nameLocalName = "ref"
                                   , nameNamespace = Nothing
                                   , namePrefix = Nothing
                                   }
                                 , [ ContentText "xs:minExclusive" ]
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
                                   { nameLocalName = "ref"
                                   , nameNamespace = Nothing
                                   , namePrefix = Nothing
                                   }
                                 , [ ContentText "xs:minInclusive" ]
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
                                   { nameLocalName = "ref"
                                   , nameNamespace = Nothing
                                   , namePrefix = Nothing
                                   }
                                 , [ ContentText "xs:maxExclusive" ]
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
                                   { nameLocalName = "ref"
                                   , nameNamespace = Nothing
                                   , namePrefix = Nothing
                                   }
                                 , [ ContentText "xs:maxInclusive" ]
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
                                   { nameLocalName = "ref"
                                   , nameNamespace = Nothing
                                   , namePrefix = Nothing
                                   }
                                 , [ ContentText "xs:totalDigits" ]
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
                                   { nameLocalName = "ref"
                                   , nameNamespace = Nothing
                                   , namePrefix = Nothing
                                   }
                                 , [ ContentText "xs:fractionDigits" ]
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
                                   { nameLocalName = "ref"
                                   , nameNamespace = Nothing
                                   , namePrefix = Nothing
                                   }
                                 , [ ContentText "xs:length" ]
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
                                   { nameLocalName = "ref"
                                   , nameNamespace = Nothing
                                   , namePrefix = Nothing
                                   }
                                 , [ ContentText "xs:minLength" ]
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
                                   { nameLocalName = "ref"
                                   , nameNamespace = Nothing
                                   , namePrefix = Nothing
                                   }
                                 , [ ContentText "xs:maxLength" ]
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
                                   { nameLocalName = "ref"
                                   , nameNamespace = Nothing
                                   , namePrefix = Nothing
                                   }
                                 , [ ContentText "xs:enumeration" ]
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
                                   { nameLocalName = "ref"
                                   , nameNamespace = Nothing
                                   , namePrefix = Nothing
                                   }
                                 , [ ContentText "xs:whiteSpace" ]
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
                                   { nameLocalName = "ref"
                                   , nameNamespace = Nothing
                                   , namePrefix = Nothing
                                   }
                                 , [ ContentText "xs:pattern" ]
                                 )
                                ]
                              , elementNodes = []
                              }
                            )
                          , NodeContent ( ContentText "" )
                          ]
                        }
                      )
                    , NodeContent ( ContentText "" )
                    ]
                  }
                )
              , NodeContent ( ContentText "" )
              , NodeElement
                ( Element
                  { elementName = Name
                    { nameLocalName = "group"
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
                     , [ ContentText "simpleRestrictionModel" ]
                     )
                    ]
                  , elementNodes =
                    [ NodeContent ( ContentText "" )
                    , NodeElement
                      ( Element
                        { elementName = Name
                          { nameLocalName = "sequence"
                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                          , namePrefix = Just "xs"
                          }
                        , elementAttributes = []
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
                                   { nameLocalName = "minOccurs"
                                   , nameNamespace = Nothing
                                   , namePrefix = Nothing
                                   }
                                 , [ ContentText "0" ]
                                 )
                                ,
                                 ( Name
                                   { nameLocalName = "type"
                                   , nameNamespace = Nothing
                                   , namePrefix = Nothing
                                   }
                                 , [ ContentText "xs:localSimpleType" ]
                                 )
                                ,
                                 ( Name
                                   { nameLocalName = "name"
                                   , nameNamespace = Nothing
                                   , namePrefix = Nothing
                                   }
                                 , [ ContentText "simpleType" ]
                                 )
                                ]
                              , elementNodes = []
                              }
                            )
                          , NodeContent ( ContentText "" )
                          , NodeElement
                            ( Element
                              { elementName = Name
                                { nameLocalName = "group"
                                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                , namePrefix = Just "xs"
                                }
                              , elementAttributes =
                                [
                                 ( Name
                                   { nameLocalName = "maxOccurs"
                                   , nameNamespace = Nothing
                                   , namePrefix = Nothing
                                   }
                                 , [ ContentText "unbounded" ]
                                 )
                                ,
                                 ( Name
                                   { nameLocalName = "minOccurs"
                                   , nameNamespace = Nothing
                                   , namePrefix = Nothing
                                   }
                                 , [ ContentText "0" ]
                                 )
                                ,
                                 ( Name
                                   { nameLocalName = "ref"
                                   , nameNamespace = Nothing
                                   , namePrefix = Nothing
                                   }
                                 , [ ContentText "xs:facets" ]
                                 )
                                ]
                              , elementNodes = []
                              }
                            )
                          , NodeContent ( ContentText "" )
                          ]
                        }
                      )
                    , NodeContent ( ContentText "" )
                    ]
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
                       { nameLocalName = "id"
                       , nameNamespace = Nothing
                       , namePrefix = Nothing
                       }
                     , [ ContentText "restriction" ]
                     )
                    ,
                     ( Name
                       { nameLocalName = "name"
                       , nameNamespace = Nothing
                       , namePrefix = Nothing
                       }
                     , [ ContentText "restriction" ]
                     )
                    ]
                  , elementNodes =
                    [ NodeContent ( ContentText "" )
                    , NodeElement
                      ( Element
                        { elementName = Name
                          { nameLocalName = "complexType"
                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                          , namePrefix = Just "xs"
                          }
                        , elementAttributes = []
                        , elementNodes =
                          [ NodeContent ( ContentText "" )
                          , NodeElement
                            ( Element
                              { elementName = Name
                                { nameLocalName = "annotation"
                                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                , namePrefix = Just "xs"
                                }
                              , elementAttributes = []
                              , elementNodes =
                                [ NodeContent ( ContentText "" )
                                , NodeElement
                                  ( Element
                                    { elementName = Name
                                      { nameLocalName = "documentation"
                                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                      , namePrefix = Just "xs"
                                      }
                                    , elementAttributes =
                                      [
                                       ( Name
                                         { nameLocalName = "source"
                                         , nameNamespace = Nothing
                                         , namePrefix = Nothing
                                         }
                                       , [ ContentText "http://www.w3.org/TR/xmlschema-2/#element-restriction" ]
                                       )
                                      ]
                                    , elementNodes =
                                      [ NodeContent ( ContentText "base attribute and simpleType child are mutually"
                                                      \ "exclusive, but one or other is required"
                                                    )
                                      ]
                                    }
                                  )
                                , NodeContent ( ContentText "" )
                                ]
                              }
                            )
                          , NodeContent ( ContentText "" )
                          , NodeElement
                            ( Element
                              { elementName = Name
                                { nameLocalName = "complexContent"
                                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                , namePrefix = Just "xs"
                                }
                              , elementAttributes = []
                              , elementNodes =
                                [ NodeContent ( ContentText "" )
                                , NodeElement
                                  ( Element
                                    { elementName = Name
                                      { nameLocalName = "extension"
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
                                       , [ ContentText "xs:annotated" ]
                                       )
                                      ]
                                    , elementNodes =
                                      [ NodeContent ( ContentText "" )
                                      , NodeElement
                                        ( Element
                                          { elementName = Name
                                            { nameLocalName = "group"
                                            , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                            , namePrefix = Just "xs"
                                            }
                                          , elementAttributes =
                                            [
                                             ( Name
                                               { nameLocalName = "ref"
                                               , nameNamespace = Nothing
                                               , namePrefix = Nothing
                                               }
                                             , [ ContentText "xs:simpleRestrictionModel" ]
                                             )
                                            ]
                                          , elementNodes = []
                                          }
                                        )
                                      , NodeContent ( ContentText "" )
                                      , NodeElement
                                        ( Element
                                          { elementName = Name
                                            { nameLocalName = "attribute"
                                            , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                            , namePrefix = Just "xs"
                                            }
                                          , elementAttributes =
                                            [
                                             ( Name
                                               { nameLocalName = "use"
                                               , nameNamespace = Nothing
                                               , namePrefix = Nothing
                                               }
                                             , [ ContentText "optional" ]
                                             )
                                            ,
                                             ( Name
                                               { nameLocalName = "type"
                                               , nameNamespace = Nothing
                                               , namePrefix = Nothing
                                               }
                                             , [ ContentText "xs:QName" ]
                                             )
                                            ,
                                             ( Name
                                               { nameLocalName = "name"
                                               , nameNamespace = Nothing
                                               , namePrefix = Nothing
                                               }
                                             , [ ContentText "base" ]
                                             )
                                            ]
                                          , elementNodes = []
                                          }
                                        )
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
                      )
                    , NodeContent ( ContentText "" )
                    ]
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
               { nameLocalName = "id"
               , nameNamespace = Nothing
               , namePrefix = Nothing
               }
             , [ ContentText "list" ]
             )
            ,
             ( Name
               { nameLocalName = "name"
               , nameNamespace = Nothing
               , namePrefix = Nothing
               }
             , [ ContentText "list" ]
             )
            ]
          , elementNodes =
            [ NodeContent ( ContentText "" )
            , NodeElement
              ( Element
                { elementName = Name
                  { nameLocalName = "complexType"
                  , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                  , namePrefix = Just "xs"
                  }
                , elementAttributes = []
                , elementNodes =
                  [ NodeContent ( ContentText "" )
                  , NodeElement
                    ( Element
                      { elementName = Name
                        { nameLocalName = "annotation"
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                        , namePrefix = Just "xs"
                        }
                      , elementAttributes = []
                      , elementNodes =
                        [ NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "documentation"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                              , namePrefix = Just "xs"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "source"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "http://www.w3.org/TR/xmlschema-2/#element-list" ]
                               )
                              ]
                            , elementNodes =
                              [ NodeContent ( ContentText
                                              "itemType attribute and simpleType child are mutually"
                                              "exclusive, but one or other is required"
                                            )
                              ]
                            }
                          )
                        , NodeContent ( ContentText "" )
                        ]
                      }
                    )
                  , NodeContent ( ContentText "" )
                  , NodeElement
                    ( Element
                      { elementName = Name
                        { nameLocalName = "complexContent"
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                        , namePrefix = Just "xs"
                        }
                      , elementAttributes = []
                      , elementNodes =
                        [ NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "extension"
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
                               , [ ContentText "xs:annotated" ]
                               )
                              ]
                            , elementNodes =
                              [ NodeContent ( ContentText "" )
                              , NodeElement
                                ( Element
                                  { elementName = Name
                                    { nameLocalName = "sequence"
                                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                    , namePrefix = Just "xs"
                                    }
                                  , elementAttributes = []
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
                                             { nameLocalName = "minOccurs"
                                             , nameNamespace = Nothing
                                             , namePrefix = Nothing
                                             }
                                           , [ ContentText "0" ]
                                           )
                                          ,
                                           ( Name
                                             { nameLocalName = "type"
                                             , nameNamespace = Nothing
                                             , namePrefix = Nothing
                                             }
                                           , [ ContentText "xs:localSimpleType" ]
                                           )
                                          ,
                                           ( Name
                                             { nameLocalName = "name"
                                             , nameNamespace = Nothing
                                             , namePrefix = Nothing
                                             }
                                           , [ ContentText "simpleType" ]
                                           )
                                          ]
                                        , elementNodes = []
                                        }
                                      )
                                    , NodeContent ( ContentText "" )
                                    ]
                                  }
                                )
                              , NodeContent ( ContentText "" )
                              , NodeElement
                                ( Element
                                  { elementName = Name
                                    { nameLocalName = "attribute"
                                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                    , namePrefix = Just "xs"
                                    }
                                  , elementAttributes =
                                    [
                                     ( Name
                                       { nameLocalName = "use"
                                       , nameNamespace = Nothing
                                       , namePrefix = Nothing
                                       }
                                     , [ ContentText "optional" ]
                                     )
                                    ,
                                     ( Name
                                       { nameLocalName = "type"
                                       , nameNamespace = Nothing
                                       , namePrefix = Nothing
                                       }
                                     , [ ContentText "xs:QName" ]
                                     )
                                    ,
                                     ( Name
                                       { nameLocalName = "name"
                                       , nameNamespace = Nothing
                                       , namePrefix = Nothing
                                       }
                                     , [ ContentText "itemType" ]
                                     )
                                    ]
                                  , elementNodes = []
                                  }
                                )
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
              )
            , NodeContent ( ContentText "" )
            ]
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
               { nameLocalName = "id"
               , nameNamespace = Nothing
               , namePrefix = Nothing
               }
             , [ ContentText "union" ]
             )
            ,
             ( Name
               { nameLocalName = "name"
               , nameNamespace = Nothing
               , namePrefix = Nothing
               }
             , [ ContentText "union" ]
             )
            ]
          , elementNodes =
            [ NodeContent ( ContentText "" )
            , NodeElement
              ( Element
                { elementName = Name
                  { nameLocalName = "complexType"
                  , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                  , namePrefix = Just "xs"
                  }
                , elementAttributes = []
                , elementNodes =
                  [ NodeContent ( ContentText "" )
                  , NodeElement
                    ( Element
                      { elementName = Name
                        { nameLocalName = "annotation"
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                        , namePrefix = Just "xs"
                        }
                      , elementAttributes = []
                      , elementNodes =
                        [ NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "documentation"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                              , namePrefix = Just "xs"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "source"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "http://www.w3.org/TR/xmlschema-2/#element-union" ]
                               )
                              ]
                            , elementNodes =
                              [ NodeContent
                                (
                                 ContentText
                                 "memberTypes attribute must be non-empty or there must be"
                                \ "at least one simpleType child"
                                )
                              ]
                            }
                          )
                        , NodeContent ( ContentText "" )
                        ]
                      }
                    )
                  , NodeContent ( ContentText "" )
                  , NodeElement
                    ( Element
                      { elementName = Name
                        { nameLocalName = "complexContent"
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                        , namePrefix = Just "xs"
                        }
                      , elementAttributes = []
                      , elementNodes =
                        [ NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "extension"
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
                               , [ ContentText "xs:annotated" ]
                               )
                              ]
                            , elementNodes =
                              [ NodeContent ( ContentText "" )
                              , NodeElement
                                ( Element
                                  { elementName = Name
                                    { nameLocalName = "sequence"
                                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                    , namePrefix = Just "xs"
                                    }
                                  , elementAttributes = []
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
                                             { nameLocalName = "maxOccurs"
                                             , nameNamespace = Nothing
                                             , namePrefix = Nothing
                                             }
                                           , [ ContentText "unbounded" ]
                                           )
                                          ,
                                           ( Name
                                             { nameLocalName = "minOccurs"
                                             , nameNamespace = Nothing
                                             , namePrefix = Nothing
                                             }
                                           , [ ContentText "0" ]
                                           )
                                          ,
                                           ( Name
                                             { nameLocalName = "type"
                                             , nameNamespace = Nothing
                                             , namePrefix = Nothing
                                             }
                                           , [ ContentText "xs:localSimpleType" ]
                                           )
                                          ,
                                           ( Name
                                             { nameLocalName = "name"
                                             , nameNamespace = Nothing
                                             , namePrefix = Nothing
                                             }
                                           , [ ContentText "simpleType" ]
                                           )
                                          ]
                                        , elementNodes = []
                                        }
                                      )
                                    , NodeContent ( ContentText "" )
                                    ]
                                  }
                                )
                              , NodeContent ( ContentText "" )
                              , NodeElement
                                ( Element
                                  { elementName = Name
                                    { nameLocalName = "attribute"
                                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                    , namePrefix = Just "xs"
                                    }
                                  , elementAttributes =
                                    [
                                     ( Name
                                       { nameLocalName = "use"
                                       , nameNamespace = Nothing
                                       , namePrefix = Nothing
                                       }
                                     , [ ContentText "optional" ]
                                     )
                                    ,
                                     ( Name
                                       { nameLocalName = "name"
                                       , nameNamespace = Nothing
                                       , namePrefix = Nothing
                                       }
                                     , [ ContentText "memberTypes" ]
                                     )
                                    ]
                                  , elementNodes =
                                    [ NodeContent ( ContentText "" )
                                    , NodeElement
                                      ( Element
                                        { elementName = Name
                                          { nameLocalName = "simpleType"
                                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                          , namePrefix = Just "xs"
                                          }
                                        , elementAttributes = []
                                        , elementNodes =
                                          [ NodeContent ( ContentText "" )
                                          , NodeElement
                                            ( Element
                                              { elementName = Name
                                                { nameLocalName = "list"
                                                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                                , namePrefix = Just "xs"
                                                }
                                              , elementAttributes =
                                                [
                                                 ( Name
                                                   { nameLocalName = "itemType"
                                                   , nameNamespace = Nothing
                                                   , namePrefix = Nothing
                                                   }
                                                 , [ ContentText "xs:QName" ]
                                                 )
                                                ]
                                              , elementNodes = []
                                              }
                                            )
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
                          )
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
        )
      , NodeContent ( ContentText "" )
      , NodeElement
        ( Element
          { elementName = Name
            { nameLocalName = "complexType"
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
             , [ ContentText "facet" ]
             )
            ]
          , elementNodes =
            [ NodeContent ( ContentText "" )
            , NodeElement
              ( Element
                { elementName = Name
                  { nameLocalName = "complexContent"
                  , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                  , namePrefix = Just "xs"
                  }
                , elementAttributes = []
                , elementNodes =
                  [ NodeContent ( ContentText "" )
                  , NodeElement
                    ( Element
                      { elementName = Name
                        { nameLocalName = "extension"
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
                         , [ ContentText "xs:annotated" ]
                         )
                        ]
                      , elementNodes =
                        [ NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "attribute"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                              , namePrefix = Just "xs"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "use"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "required" ]
                               )
                              ,
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "value" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "attribute"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                              , namePrefix = Just "xs"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "use"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "optional" ]
                               )
                              ,
                               ( Name
                                 { nameLocalName = "default"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "false" ]
                               )
                              ,
                               ( Name
                                 { nameLocalName = "type"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "xs:boolean" ]
                               )
                              ,
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "fixed" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
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
        )
      , NodeContent ( ContentText "" )
      , NodeElement
        ( Element
          { elementName = Name
            { nameLocalName = "complexType"
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
             , [ ContentText "noFixedFacet" ]
             )
            ]
          , elementNodes =
            [ NodeContent ( ContentText "" )
            , NodeElement
              ( Element
                { elementName = Name
                  { nameLocalName = "complexContent"
                  , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                  , namePrefix = Just "xs"
                  }
                , elementAttributes = []
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
                         , [ ContentText "xs:facet" ]
                         )
                        ]
                      , elementNodes =
                        [ NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "sequence"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                              , namePrefix = Just "xs"
                              }
                            , elementAttributes = []
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
                                       { nameLocalName = "minOccurs"
                                       , nameNamespace = Nothing
                                       , namePrefix = Nothing
                                       }
                                     , [ ContentText "0" ]
                                     )
                                    ,
                                     ( Name
                                       { nameLocalName = "ref"
                                       , nameNamespace = Nothing
                                       , namePrefix = Nothing
                                       }
                                     , [ ContentText "xs:annotation" ]
                                     )
                                    ]
                                  , elementNodes = []
                                  }
                                )
                              , NodeContent ( ContentText "" )
                              ]
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "attribute"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                              , namePrefix = Just "xs"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "use"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "prohibited" ]
                               )
                              ,
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "fixed" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "anyAttribute"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                              , namePrefix = Just "xs"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "processContents"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "lax" ]
                               )
                              ,
                               ( Name
                                 { nameLocalName = "namespace"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "##other" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
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
               { nameLocalName = "id"
               , nameNamespace = Nothing
               , namePrefix = Nothing
               }
             , [ ContentText "minExclusive" ]
             )
            ,
             ( Name
               { nameLocalName = "type"
               , nameNamespace = Nothing
               , namePrefix = Nothing
               }
             , [ ContentText "xs:facet" ]
             )
            ,
             ( Name
               { nameLocalName = "name"
               , nameNamespace = Nothing
               , namePrefix = Nothing
               }
             , [ ContentText "minExclusive" ]
             )
            ]
          , elementNodes =
            [ NodeContent ( ContentText "" )
            , NodeElement
              ( Element
                { elementName = Name
                  { nameLocalName = "annotation"
                  , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                  , namePrefix = Just "xs"
                  }
                , elementAttributes = []
                , elementNodes =
                  [ NodeContent ( ContentText "" )
                  , NodeElement
                    ( Element
                      { elementName = Name
                        { nameLocalName = "documentation"
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                        , namePrefix = Just "xs"
                        }
                      , elementAttributes =
                        [
                         ( Name
                           { nameLocalName = "source"
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           }
                         , [ ContentText "http://www.w3.org/TR/xmlschema-2/#element-minExclusive" ]
                         )
                        ]
                      , elementNodes = []
                      }
                    )
                  , NodeContent ( ContentText "" )
                  ]
                }
              )
            , NodeContent ( ContentText "" )
            ]
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
               { nameLocalName = "id"
               , nameNamespace = Nothing
               , namePrefix = Nothing
               }
             , [ ContentText "minInclusive" ]
             )
            ,
             ( Name
               { nameLocalName = "type"
               , nameNamespace = Nothing
               , namePrefix = Nothing
               }
             , [ ContentText "xs:facet" ]
             )
            ,
             ( Name
               { nameLocalName = "name"
               , nameNamespace = Nothing
               , namePrefix = Nothing
               }
             , [ ContentText "minInclusive" ]
             )
            ]
          , elementNodes =
            [ NodeContent ( ContentText "" )
            , NodeElement
              ( Element
                { elementName = Name
                  { nameLocalName = "annotation"
                  , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                  , namePrefix = Just "xs"
                  }
                , elementAttributes = []
                , elementNodes =
                  [ NodeContent ( ContentText "" )
                  , NodeElement
                    ( Element
                      { elementName = Name
                        { nameLocalName = "documentation"
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                        , namePrefix = Just "xs"
                        }
                      , elementAttributes =
                        [
                         ( Name
                           { nameLocalName = "source"
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           }
                         , [ ContentText "http://www.w3.org/TR/xmlschema-2/#element-minInclusive" ]
                         )
                        ]
                      , elementNodes = []
                      }
                    )
                  , NodeContent ( ContentText "" )
                  ]
                }
              )
            , NodeContent ( ContentText "" )
            ]
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
               { nameLocalName = "id"
               , nameNamespace = Nothing
               , namePrefix = Nothing
               }
             , [ ContentText "maxExclusive" ]
             )
            ,
             ( Name
               { nameLocalName = "type"
               , nameNamespace = Nothing
               , namePrefix = Nothing
               }
             , [ ContentText "xs:facet" ]
             )
            ,
             ( Name
               { nameLocalName = "name"
               , nameNamespace = Nothing
               , namePrefix = Nothing
               }
             , [ ContentText "maxExclusive" ]
             )
            ]
          , elementNodes =
            [ NodeContent ( ContentText "" )
            , NodeElement
              ( Element
                { elementName = Name
                  { nameLocalName = "annotation"
                  , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                  , namePrefix = Just "xs"
                  }
                , elementAttributes = []
                , elementNodes =
                  [ NodeContent ( ContentText "" )
                  , NodeElement
                    ( Element
                      { elementName = Name
                        { nameLocalName = "documentation"
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                        , namePrefix = Just "xs"
                        }
                      , elementAttributes =
                        [
                         ( Name
                           { nameLocalName = "source"
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           }
                         , [ ContentText "http://www.w3.org/TR/xmlschema-2/#element-maxExclusive" ]
                         )
                        ]
                      , elementNodes = []
                      }
                    )
                  , NodeContent ( ContentText "" )
                  ]
                }
              )
            , NodeContent ( ContentText "" )
            ]
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
               { nameLocalName = "id"
               , nameNamespace = Nothing
               , namePrefix = Nothing
               }
             , [ ContentText "maxInclusive" ]
             )
            ,
             ( Name
               { nameLocalName = "type"
               , nameNamespace = Nothing
               , namePrefix = Nothing
               }
             , [ ContentText "xs:facet" ]
             )
            ,
             ( Name
               { nameLocalName = "name"
               , nameNamespace = Nothing
               , namePrefix = Nothing
               }
             , [ ContentText "maxInclusive" ]
             )
            ]
          , elementNodes =
            [ NodeContent ( ContentText "" )
            , NodeElement
              ( Element
                { elementName = Name
                  { nameLocalName = "annotation"
                  , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                  , namePrefix = Just "xs"
                  }
                , elementAttributes = []
                , elementNodes =
                  [ NodeContent ( ContentText "" )
                  , NodeElement
                    ( Element
                      { elementName = Name
                        { nameLocalName = "documentation"
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                        , namePrefix = Just "xs"
                        }
                      , elementAttributes =
                        [
                         ( Name
                           { nameLocalName = "source"
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           }
                         , [ ContentText "http://www.w3.org/TR/xmlschema-2/#element-maxInclusive" ]
                         )
                        ]
                      , elementNodes = []
                      }
                    )
                  , NodeContent ( ContentText "" )
                  ]
                }
              )
            , NodeContent ( ContentText "" )
            ]
          }
        )
      , NodeContent ( ContentText "" )
      , NodeElement
        ( Element
          { elementName = Name
            { nameLocalName = "complexType"
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
             , [ ContentText "numFacet" ]
             )
            ]
          , elementNodes =
            [ NodeContent ( ContentText "" )
            , NodeElement
              ( Element
                { elementName = Name
                  { nameLocalName = "complexContent"
                  , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                  , namePrefix = Just "xs"
                  }
                , elementAttributes = []
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
                         , [ ContentText "xs:facet" ]
                         )
                        ]
                      , elementNodes =
                        [ NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "sequence"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                              , namePrefix = Just "xs"
                              }
                            , elementAttributes = []
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
                                       { nameLocalName = "minOccurs"
                                       , nameNamespace = Nothing
                                       , namePrefix = Nothing
                                       }
                                     , [ ContentText "0" ]
                                     )
                                    ,
                                     ( Name
                                       { nameLocalName = "ref"
                                       , nameNamespace = Nothing
                                       , namePrefix = Nothing
                                       }
                                     , [ ContentText "xs:annotation" ]
                                     )
                                    ]
                                  , elementNodes = []
                                  }
                                )
                              , NodeContent ( ContentText "" )
                              ]
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "attribute"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                              , namePrefix = Just "xs"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "use"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "required" ]
                               )
                              ,
                               ( Name
                                 { nameLocalName = "type"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "xs:nonNegativeInteger" ]
                               )
                              ,
                               ( Name
                                 { nameLocalName = "name"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "value" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement
                          ( Element
                            { elementName = Name
                              { nameLocalName = "anyAttribute"
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                              , namePrefix = Just "xs"
                              }
                            , elementAttributes =
                              [
                               ( Name
                                 { nameLocalName = "processContents"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "lax" ]
                               )
                              ,
                               ( Name
                                 { nameLocalName = "namespace"
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 }
                               , [ ContentText "##other" ]
                               )
                              ]
                            , elementNodes = []
                            }
                          )
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
               { nameLocalName = "id"
               , nameNamespace = Nothing
               , namePrefix = Nothing
               }
             , [ ContentText "totalDigits" ]
             )
            ,
             ( Name
               { nameLocalName = "name"
               , nameNamespace = Nothing
               , namePrefix = Nothing
               }
             , [ ContentText "totalDigits" ]
             )
            ]
          , elementNodes =
            [ NodeContent ( ContentText "" )
            , NodeElement
              ( Element
                { elementName = Name
                  { nameLocalName = "annotation"
                  , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                  , namePrefix = Just "xs"
                  }
                , elementAttributes = []
                , elementNodes =
                  [ NodeContent ( ContentText "" )
                  , NodeElement
                    ( Element
                      { elementName = Name
                        { nameLocalName = "documentation"
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                        , namePrefix = Just "xs"
                        }
                      , elementAttributes =
                        [
                         ( Name
                           { nameLocalName = "source"
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           }
                         , [ ContentText "http://www.w3.org/TR/xmlschema-2/#element-totalDigits" ]
                         )
                        ]
                      , elementNodes = []
                      }
                    )
                  , NodeContent ( ContentText "" )
                  ]
                }
              )
            , NodeContent ( ContentText "" )
            , NodeElement
              ( Element
                { elementName = Name
                  { nameLocalName = "complexType"
                  , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                  , namePrefix = Just "xs"
                  }
                , elementAttributes = []
                , elementNodes =
                  [ NodeContent ( ContentText "" )
                  , NodeElement
                    ( Element
                      { elementName = Name
                        { nameLocalName = "complexContent"
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                        , namePrefix = Just "xs"
                        }
                      , elementAttributes = []
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
                               , [ ContentText "xs:numFacet" ]
                               )
                              ]
                            , elementNodes =
                              [ NodeContent ( ContentText "" )
                              , NodeElement
                                ( Element
                                  { elementName = Name
                                    { nameLocalName = "sequence"
                                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                    , namePrefix = Just "xs"
                                    }
                                  , elementAttributes = []
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
                                             { nameLocalName = "minOccurs"
                                             , nameNamespace = Nothing
                                             , namePrefix = Nothing
                                             }
                                           , [ ContentText "0" ]
                                           )
                                          ,
                                           ( Name
                                             { nameLocalName = "ref"
                                             , nameNamespace = Nothing
                                             , namePrefix = Nothing
                                             }
                                           , [ ContentText "xs:annotation" ]
                                           )
                                          ]
                                        , elementNodes = []
                                        }
                                      )
                                    , NodeContent ( ContentText "" )
                                    ]
                                  }
                                )
                              , NodeContent ( ContentText "" )
                              , NodeElement
                                ( Element
                                  { elementName = Name
                                    { nameLocalName = "attribute"
                                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                    , namePrefix = Just "xs"
                                    }
                                  , elementAttributes =
                                    [
                                     ( Name
                                       { nameLocalName = "use"
                                       , nameNamespace = Nothing
                                       , namePrefix = Nothing
                                       }
                                     , [ ContentText "required" ]
                                     )
                                    ,
                                     ( Name
                                       { nameLocalName = "type"
                                       , nameNamespace = Nothing
                                       , namePrefix = Nothing
                                       }
                                     , [ ContentText "xs:positiveInteger" ]
                                     )
                                    ,
                                     ( Name
                                       { nameLocalName = "name"
                                       , nameNamespace = Nothing
                                       , namePrefix = Nothing
                                       }
                                     , [ ContentText "value" ]
                                     )
                                    ]
                                  , elementNodes = []
                                  }
                                )
                              , NodeContent ( ContentText "" )
                              , NodeElement
                                ( Element
                                  { elementName = Name
                                    { nameLocalName = "anyAttribute"
                                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                    , namePrefix = Just "xs"
                                    }
                                  , elementAttributes =
                                    [
                                     ( Name
                                       { nameLocalName = "processContents"
                                       , nameNamespace = Nothing
                                       , namePrefix = Nothing
                                       }
                                     , [ ContentText "lax" ]
                                     )
                                    ,
                                     ( Name
                                       { nameLocalName = "namespace"
                                       , nameNamespace = Nothing
                                       , namePrefix = Nothing
                                       }
                                     , [ ContentText "##other" ]
                                     )
                                    ]
                                  , elementNodes = []
                                  }
                                )
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
              )
            , NodeContent ( ContentText "" )
            ]
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
               { nameLocalName = "id"
               , nameNamespace = Nothing
               , namePrefix = Nothing
               }
             , [ ContentText "fractionDigits" ]
             )
            ,
             ( Name
               { nameLocalName = "type"
               , nameNamespace = Nothing
               , namePrefix = Nothing
               }
             , [ ContentText "xs:numFacet" ]
             )
            ,
             ( Name
               { nameLocalName = "name"
               , nameNamespace = Nothing
               , namePrefix = Nothing
               }
             , [ ContentText "fractionDigits" ]
             )
            ]
          , elementNodes =
            [ NodeContent ( ContentText "" )
            , NodeElement
              ( Element
                { elementName = Name
                  { nameLocalName = "annotation"
                  , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                  , namePrefix = Just "xs"
                  }
                , elementAttributes = []
                , elementNodes =
                  [ NodeContent ( ContentText "" )
                  , NodeElement
                    ( Element
                      { elementName = Name
                        { nameLocalName = "documentation"
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                        , namePrefix = Just "xs"
                        }
                      , elementAttributes =
                        [
                         ( Name
                           { nameLocalName = "source"
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           }
                         , [ ContentText "http://www.w3.org/TR/xmlschema-2/#element-fractionDigits" ]
                         )
                        ]
                      , elementNodes = []
                      }
                    )
                  , NodeContent ( ContentText "" )
                  ]
                }
              )
            , NodeContent ( ContentText "" )
            ]
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
               { nameLocalName = "id"
               , nameNamespace = Nothing
               , namePrefix = Nothing
               }
             , [ ContentText "length" ]
             )
            ,
             ( Name
               { nameLocalName = "type"
               , nameNamespace = Nothing
               , namePrefix = Nothing
               }
             , [ ContentText "xs:numFacet" ]
             )
            ,
             ( Name
               { nameLocalName = "name"
               , nameNamespace = Nothing
               , namePrefix = Nothing
               }
             , [ ContentText "length" ]
             )
            ]
          , elementNodes =
            [ NodeContent ( ContentText "" )
            , NodeElement
              ( Element
                { elementName = Name
                  { nameLocalName = "annotation"
                  , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                  , namePrefix = Just "xs"
                  }
                , elementAttributes = []
                , elementNodes =
                  [ NodeContent ( ContentText "" )
                  , NodeElement
                    ( Element
                      { elementName = Name
                        { nameLocalName = "documentation"
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                        , namePrefix = Just "xs"
                        }
                      , elementAttributes =
                        [
                         ( Name
                           { nameLocalName = "source"
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           }
                         , [ ContentText "http://www.w3.org/TR/xmlschema-2/#element-length" ]
                         )
                        ]
                      , elementNodes = []
                      }
                    )
                  , NodeContent ( ContentText "" )
                  ]
                }
              )
            , NodeContent ( ContentText "" )
            ]
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
               { nameLocalName = "id"
               , nameNamespace = Nothing
               , namePrefix = Nothing
               }
             , [ ContentText "minLength" ]
             )
            ,
             ( Name
               { nameLocalName = "type"
               , nameNamespace = Nothing
               , namePrefix = Nothing
               }
             , [ ContentText "xs:numFacet" ]
             )
            ,
             ( Name
               { nameLocalName = "name"
               , nameNamespace = Nothing
               , namePrefix = Nothing
               }
             , [ ContentText "minLength" ]
             )
            ]
          , elementNodes =
            [ NodeContent ( ContentText "" )
            , NodeElement
              ( Element
                { elementName = Name
                  { nameLocalName = "annotation"
                  , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                  , namePrefix = Just "xs"
                  }
                , elementAttributes = []
                , elementNodes =
                  [ NodeContent ( ContentText "" )
                  , NodeElement
                    ( Element
                      { elementName = Name
                        { nameLocalName = "documentation"
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                        , namePrefix = Just "xs"
                        }
                      , elementAttributes =
                        [
                         ( Name
                           { nameLocalName = "source"
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           }
                         , [ ContentText "http://www.w3.org/TR/xmlschema-2/#element-minLength" ]
                         )
                        ]
                      , elementNodes = []
                      }
                    )
                  , NodeContent ( ContentText "" )
                  ]
                }
              )
            , NodeContent ( ContentText "" )
            ]
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
               { nameLocalName = "id"
               , nameNamespace = Nothing
               , namePrefix = Nothing
               }
             , [ ContentText "maxLength" ]
             )
            ,
             ( Name
               { nameLocalName = "type"
               , nameNamespace = Nothing
               , namePrefix = Nothing
               }
             , [ ContentText "xs:numFacet" ]
             )
            ,
             ( Name
               { nameLocalName = "name"
               , nameNamespace = Nothing
               , namePrefix = Nothing
               }
             , [ ContentText "maxLength" ]
             )
            ]
          , elementNodes =
            [ NodeContent ( ContentText "" )
            , NodeElement
              ( Element
                { elementName = Name
                  { nameLocalName = "annotation"
                  , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                  , namePrefix = Just "xs"
                  }
                , elementAttributes = []
                , elementNodes =
                  [ NodeContent ( ContentText "" )
                  , NodeElement
                    ( Element
                      { elementName = Name
                        { nameLocalName = "documentation"
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                        , namePrefix = Just "xs"
                        }
                      , elementAttributes =
                        [
                         ( Name
                           { nameLocalName = "source"
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           }
                         , [ ContentText "http://www.w3.org/TR/xmlschema-2/#element-maxLength" ]
                         )
                        ]
                      , elementNodes = []
                      }
                    )
                  , NodeContent ( ContentText "" )
                  ]
                }
              )
            , NodeContent ( ContentText "" )
            ]
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
               { nameLocalName = "id"
               , nameNamespace = Nothing
               , namePrefix = Nothing
               }
             , [ ContentText "enumeration" ]
             )
            ,
             ( Name
               { nameLocalName = "type"
               , nameNamespace = Nothing
               , namePrefix = Nothing
               }
             , [ ContentText "xs:noFixedFacet" ]
             )
            ,
             ( Name
               { nameLocalName = "name"
               , nameNamespace = Nothing
               , namePrefix = Nothing
               }
             , [ ContentText "enumeration" ]
             )
            ]
          , elementNodes =
            [ NodeContent ( ContentText "" )
            , NodeElement
              ( Element
                { elementName = Name
                  { nameLocalName = "annotation"
                  , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                  , namePrefix = Just "xs"
                  }
                , elementAttributes = []
                , elementNodes =
                  [ NodeContent ( ContentText "" )
                  , NodeElement
                    ( Element
                      { elementName = Name
                        { nameLocalName = "documentation"
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                        , namePrefix = Just "xs"
                        }
                      , elementAttributes =
                        [
                         ( Name
                           { nameLocalName = "source"
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           }
                         , [ ContentText "http://www.w3.org/TR/xmlschema-2/#element-enumeration" ]
                         )
                        ]
                      , elementNodes = []
                      }
                    )
                  , NodeContent ( ContentText "" )
                  ]
                }
              )
            , NodeContent ( ContentText "" )
            ]
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
               { nameLocalName = "id"
               , nameNamespace = Nothing
               , namePrefix = Nothing
               }
             , [ ContentText "whiteSpace" ]
             )
            ,
             ( Name
               { nameLocalName = "name"
               , nameNamespace = Nothing
               , namePrefix = Nothing
               }
             , [ ContentText "whiteSpace" ]
             )
            ]
          , elementNodes =
            [ NodeContent ( ContentText "" )
            , NodeElement
              ( Element
                { elementName = Name
                  { nameLocalName = "annotation"
                  , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                  , namePrefix = Just "xs"
                  }
                , elementAttributes = []
                , elementNodes =
                  [ NodeContent ( ContentText "" )
                  , NodeElement
                    ( Element
                      { elementName = Name
                        { nameLocalName = "documentation"
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                        , namePrefix = Just "xs"
                        }
                      , elementAttributes =
                        [
                         ( Name
                           { nameLocalName = "source"
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           }
                         , [ ContentText "http://www.w3.org/TR/xmlschema-2/#element-whiteSpace" ]
                         )
                        ]
                      , elementNodes = []
                      }
                    )
                  , NodeContent ( ContentText "" )
                  ]
                }
              )
            , NodeContent ( ContentText "" )
            , NodeElement
              ( Element
                { elementName = Name
                  { nameLocalName = "complexType"
                  , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                  , namePrefix = Just "xs"
                  }
                , elementAttributes = []
                , elementNodes =
                  [ NodeContent ( ContentText "" )
                  , NodeElement
                    ( Element
                      { elementName = Name
                        { nameLocalName = "complexContent"
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                        , namePrefix = Just "xs"
                        }
                      , elementAttributes = []
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
                               , [ ContentText "xs:facet" ]
                               )
                              ]
                            , elementNodes =
                              [ NodeContent ( ContentText "" )
                              , NodeElement
                                ( Element
                                  { elementName = Name
                                    { nameLocalName = "sequence"
                                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                    , namePrefix = Just "xs"
                                    }
                                  , elementAttributes = []
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
                                             { nameLocalName = "minOccurs"
                                             , nameNamespace = Nothing
                                             , namePrefix = Nothing
                                             }
                                           , [ ContentText "0" ]
                                           )
                                          ,
                                           ( Name
                                             { nameLocalName = "ref"
                                             , nameNamespace = Nothing
                                             , namePrefix = Nothing
                                             }
                                           , [ ContentText "xs:annotation" ]
                                           )
                                          ]
                                        , elementNodes = []
                                        }
                                      )
                                    , NodeContent ( ContentText "" )
                                    ]
                                  }
                                )
                              , NodeContent ( ContentText "" )
                              , NodeElement
                                ( Element
                                  { elementName = Name
                                    { nameLocalName = "attribute"
                                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                    , namePrefix = Just "xs"
                                    }
                                  , elementAttributes =
                                    [
                                     ( Name
                                       { nameLocalName = "use"
                                       , nameNamespace = Nothing
                                       , namePrefix = Nothing
                                       }
                                     , [ ContentText "required" ]
                                     )
                                    ,
                                     ( Name
                                       { nameLocalName = "name"
                                       , nameNamespace = Nothing
                                       , namePrefix = Nothing
                                       }
                                     , [ ContentText "value" ]
                                     )
                                    ]
                                  , elementNodes =
                                    [ NodeContent ( ContentText "" )
                                    , NodeElement
                                      ( Element
                                        { elementName = Name
                                          { nameLocalName = "simpleType"
                                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                          , namePrefix = Just "xs"
                                          }
                                        , elementAttributes = []
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
                                                 , [ ContentText "xs:NMTOKEN" ]
                                                 )
                                                ]
                                              , elementNodes =
                                                [ NodeContent ( ContentText "" )
                                                , NodeElement
                                                  ( Element
                                                    { elementName = Name
                                                      { nameLocalName = "enumeration"
                                                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                                      , namePrefix = Just "xs"
                                                      }
                                                    , elementAttributes =
                                                      [
                                                       ( Name
                                                         { nameLocalName = "value"
                                                         , nameNamespace = Nothing
                                                         , namePrefix = Nothing
                                                         }
                                                       , [ ContentText "preserve" ]
                                                       )
                                                      ]
                                                    , elementNodes = []
                                                    }
                                                  )
                                                , NodeContent ( ContentText "" )
                                                , NodeElement
                                                  ( Element
                                                    { elementName = Name
                                                      { nameLocalName = "enumeration"
                                                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                                      , namePrefix = Just "xs"
                                                      }
                                                    , elementAttributes =
                                                      [
                                                       ( Name
                                                         { nameLocalName = "value"
                                                         , nameNamespace = Nothing
                                                         , namePrefix = Nothing
                                                         }
                                                       , [ ContentText "replace" ]
                                                       )
                                                      ]
                                                    , elementNodes = []
                                                    }
                                                  )
                                                , NodeContent ( ContentText "" )
                                                , NodeElement
                                                  ( Element
                                                    { elementName = Name
                                                      { nameLocalName = "enumeration"
                                                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                                      , namePrefix = Just "xs"
                                                      }
                                                    , elementAttributes =
                                                      [
                                                       ( Name
                                                         { nameLocalName = "value"
                                                         , nameNamespace = Nothing
                                                         , namePrefix = Nothing
                                                         }
                                                       , [ ContentText "collapse" ]
                                                       )
                                                      ]
                                                    , elementNodes = []
                                                    }
                                                  )
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
                                )
                              , NodeContent ( ContentText "" )
                              , NodeElement
                                ( Element
                                  { elementName = Name
                                    { nameLocalName = "anyAttribute"
                                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                    , namePrefix = Just "xs"
                                    }
                                  , elementAttributes =
                                    [
                                     ( Name
                                       { nameLocalName = "processContents"
                                       , nameNamespace = Nothing
                                       , namePrefix = Nothing
                                       }
                                     , [ ContentText "lax" ]
                                     )
                                    ,
                                     ( Name
                                       { nameLocalName = "namespace"
                                       , nameNamespace = Nothing
                                       , namePrefix = Nothing
                                       }
                                     , [ ContentText "##other" ]
                                     )
                                    ]
                                  , elementNodes = []
                                  }
                                )
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
              )
            , NodeContent ( ContentText "" )
            ]
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
               { nameLocalName = "id"
               , nameNamespace = Nothing
               , namePrefix = Nothing
               }
             , [ ContentText "pattern" ]
             )
            ,
             ( Name
               { nameLocalName = "name"
               , nameNamespace = Nothing
               , namePrefix = Nothing
               }
             , [ ContentText "pattern" ]
             )
            ]
          , elementNodes =
            [ NodeContent ( ContentText "" )
            , NodeElement
              ( Element
                { elementName = Name
                  { nameLocalName = "annotation"
                  , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                  , namePrefix = Just "xs"
                  }
                , elementAttributes = []
                , elementNodes =
                  [ NodeContent ( ContentText "" )
                  , NodeElement
                    ( Element
                      { elementName = Name
                        { nameLocalName = "documentation"
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                        , namePrefix = Just "xs"
                        }
                      , elementAttributes =
                        [
                         ( Name
                           { nameLocalName = "source"
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           }
                         , [ ContentText "http://www.w3.org/TR/xmlschema-2/#element-pattern" ]
                         )
                        ]
                      , elementNodes = []
                      }
                    )
                  , NodeContent ( ContentText "" )
                  ]
                }
              )
            , NodeContent ( ContentText "" )
            , NodeElement
              ( Element
                { elementName = Name
                  { nameLocalName = "complexType"
                  , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                  , namePrefix = Just "xs"
                  }
                , elementAttributes = []
                , elementNodes =
                  [ NodeContent ( ContentText "" )
                  , NodeElement
                    ( Element
                      { elementName = Name
                        { nameLocalName = "complexContent"
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                        , namePrefix = Just "xs"
                        }
                      , elementAttributes = []
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
                               , [ ContentText "xs:noFixedFacet" ]
                               )
                              ]
                            , elementNodes =
                              [ NodeContent ( ContentText "" )
                              , NodeElement
                                ( Element
                                  { elementName = Name
                                    { nameLocalName = "sequence"
                                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                    , namePrefix = Just "xs"
                                    }
                                  , elementAttributes = []
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
                                             { nameLocalName = "minOccurs"
                                             , nameNamespace = Nothing
                                             , namePrefix = Nothing
                                             }
                                           , [ ContentText "0" ]
                                           )
                                          ,
                                           ( Name
                                             { nameLocalName = "ref"
                                             , nameNamespace = Nothing
                                             , namePrefix = Nothing
                                             }
                                           , [ ContentText "xs:annotation" ]
                                           )
                                          ]
                                        , elementNodes = []
                                        }
                                      )
                                    , NodeContent ( ContentText "" )
                                    ]
                                  }
                                )
                              , NodeContent ( ContentText "" )
                              , NodeElement
                                ( Element
                                  { elementName = Name
                                    { nameLocalName = "attribute"
                                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                    , namePrefix = Just "xs"
                                    }
                                  , elementAttributes =
                                    [
                                     ( Name
                                       { nameLocalName = "use"
                                       , nameNamespace = Nothing
                                       , namePrefix = Nothing
                                       }
                                     , [ ContentText "required" ]
                                     )
                                    ,
                                     ( Name
                                       { nameLocalName = "type"
                                       , nameNamespace = Nothing
                                       , namePrefix = Nothing
                                       }
                                     , [ ContentText "xs:string" ]
                                     )
                                    ,
                                     ( Name
                                       { nameLocalName = "name"
                                       , nameNamespace = Nothing
                                       , namePrefix = Nothing
                                       }
                                     , [ ContentText "value" ]
                                     )
                                    ]
                                  , elementNodes = []
                                  }
                                )
                              , NodeContent ( ContentText "" )
                              , NodeElement
                                ( Element
                                  { elementName = Name
                                    { nameLocalName = "anyAttribute"
                                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                    , namePrefix = Just "xs"
                                    }
                                  , elementAttributes =
                                    [
                                     ( Name
                                       { nameLocalName = "processContents"
                                       , nameNamespace = Nothing
                                       , namePrefix = Nothing
                                       }
                                     , [ ContentText "lax" ]
                                     )
                                    ,
                                     ( Name
                                       { nameLocalName = "namespace"
                                       , nameNamespace = Nothing
                                       , namePrefix = Nothing
                                       }
                                     , [ ContentText "##other" ]
                                     )
                                    ]
                                  , elementNodes = []
                                  }
                                )
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
