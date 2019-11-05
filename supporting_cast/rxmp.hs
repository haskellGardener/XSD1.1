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
    , namePrefix = Just "xsd"
    }
  , elementAttributes = fromList
    [
     ( Name
       { nameLocalName = "targetNamespace"
       , nameNamespace = Nothing
       , namePrefix = Nothing
       }
     , "http://com.ibm.wbit.comptest.controller"
     )
    ,
     ( Name
       { nameLocalName = "xmlns:Q1"
       , nameNamespace = Nothing
       , namePrefix = Nothing
       }
     , "http://com.ibm.wbit.comptest.controller"
     )
    ,
     ( Name
       { nameLocalName = "xmlns:xsd"
       , nameNamespace = Nothing
       , namePrefix = Nothing
       }
     , "http://www.w3.org/2001/XMLSchema"
     )
    ]
  , elementNodes =
    [ NodeContent ""
    , NodeElement
        ( Element
          { elementName = Name
            { nameLocalName = "complexType"
            , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
            , namePrefix = Just "xsd"
            }
          , elementAttributes = fromList
            [
             ( Name
               { nameLocalName = "name"
               , nameNamespace = Nothing
               , namePrefix = Nothing
               }
             , "TestResults"
             )
            ]
          , elementNodes =
            [ NodeContent ""
            , NodeElement
                ( Element
                  { elementName = Name
                    { nameLocalName = "sequence"
                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                    , namePrefix = Just "xsd"
                    }
                  , elementAttributes = fromList []
                  , elementNodes =
                    [ NodeContent ""
                    , NodeElement
                        ( Element
                          { elementName = Name
                            { nameLocalName = "element"
                            , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                            , namePrefix = Just "xsd"
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
                             , "testSuites"
                             )
                            ,
                             ( Name
                               { nameLocalName = "type"
                               , nameNamespace = Nothing
                               , namePrefix = Nothing
                               }
                             , "Q1:TestSuiteRun"
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
            , NodeElement
                ( Element
                  { elementName = Name
                    { nameLocalName = "attribute"
                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                    , namePrefix = Just "xsd"
                    }
                  , elementAttributes = fromList
                    [
                     ( Name
                       { nameLocalName = "name"
                       , nameNamespace = Nothing
                       , namePrefix = Nothing
                       }
                     , "testProject"
                     )
                    ,
                     ( Name
                       { nameLocalName = "type"
                       , nameNamespace = Nothing
                       , namePrefix = Nothing
                       }
                     , "xsd:string"
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
    , NodeElement
        ( Element
          { elementName = Name
            { nameLocalName = "complexType"
            , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
            , namePrefix = Just "xsd"
            }
          , elementAttributes = fromList
            [
             ( Name
               { nameLocalName = "name"
               , nameNamespace = Nothing
               , namePrefix = Nothing
               }
             , "TestSuiteRun"
             )
            ]
          , elementNodes =
            [ NodeContent ""
            , NodeElement
                ( Element
                  { elementName = Name
                    { nameLocalName = "complexContent"
                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                    , namePrefix = Just "xsd"
                    }
                  , elementAttributes = fromList []
                  , elementNodes =
                    [ NodeContent ""
                    , NodeElement
                        ( Element
                          { elementName = Name
                            { nameLocalName = "extension"
                            , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                            , namePrefix = Just "xsd"
                            }
                          , elementAttributes = fromList
                            [
                             ( Name
                               { nameLocalName = "base"
                               , nameNamespace = Nothing
                               , namePrefix = Nothing
                               }
                             , "Q1:TestRun"
                             )
                            ]
                          , elementNodes =
                            [ NodeContent ""
                            , NodeElement
                                ( Element
                                  { elementName = Name
                                    { nameLocalName = "sequence"
                                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                    , namePrefix = Just "xsd"
                                    }
                                  , elementAttributes = fromList []
                                  , elementNodes =
                                    [ NodeContent ""
                                    , NodeElement
                                        ( Element
                                          { elementName = Name
                                            { nameLocalName = "element"
                                            , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                            , namePrefix = Just "xsd"
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
                                             , "testCases"
                                             )
                                            ,
                                             ( Name
                                               { nameLocalName = "type"
                                               , nameNamespace = Nothing
                                               , namePrefix = Nothing
                                               }
                                             , "Q1:TestCaseRun"
                                             )
                                            ]
                                          , elementNodes = [ NodeContent "" ]
                                          }
                                        )
                                    , NodeContent ""
                                    ]
                                  }
                                )
                            , NodeContent ""
                            , NodeElement
                                ( Element
                                  { elementName = Name
                                    { nameLocalName = "attribute"
                                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                    , namePrefix = Just "xsd"
                                    }
                                  , elementAttributes = fromList
                                    [
                                     ( Name
                                       { nameLocalName = "name"
                                       , nameNamespace = Nothing
                                       , namePrefix = Nothing
                                       }
                                     , "tests"
                                     )
                                    ,
                                     ( Name
                                       { nameLocalName = "type"
                                       , nameNamespace = Nothing
                                       , namePrefix = Nothing
                                       }
                                     , "xsd:int"
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
    , NodeElement
        ( Element
          { elementName = Name
            { nameLocalName = "complexType"
            , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
            , namePrefix = Just "xsd"
            }
          , elementAttributes = fromList
            [
             ( Name
               { nameLocalName = "name"
               , nameNamespace = Nothing
               , namePrefix = Nothing
               }
             , "TestCaseRun"
             )
            ]
          , elementNodes =
            [ NodeContent ""
            , NodeElement
                ( Element
                  { elementName = Name
                    { nameLocalName = "complexContent"
                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                    , namePrefix = Just "xsd"
                    }
                  , elementAttributes = fromList []
                  , elementNodes =
                    [ NodeContent ""
                    , NodeElement
                        ( Element
                          { elementName = Name
                            { nameLocalName = "extension"
                            , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                            , namePrefix = Just "xsd"
                            }
                          , elementAttributes = fromList
                            [
                             ( Name
                               { nameLocalName = "base"
                               , nameNamespace = Nothing
                               , namePrefix = Nothing
                               }
                             , "Q1:TestRun"
                             )
                            ]
                          , elementNodes =
                            [ NodeContent ""
                            , NodeElement
                                ( Element
                                  { elementName = Name
                                    { nameLocalName = "sequence"
                                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                    , namePrefix = Just "xsd"
                                    }
                                  , elementAttributes = fromList []
                                  , elementNodes =
                                    [ NodeContent ""
                                    , NodeElement
                                        ( Element
                                          { elementName = Name
                                            { nameLocalName = "element"
                                            , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                            , namePrefix = Just "xsd"
                                            }
                                          , elementAttributes = fromList
                                            [
                                             ( Name
                                               { nameLocalName = "name"
                                               , nameNamespace = Nothing
                                               , namePrefix = Nothing
                                               }
                                             , "result"
                                             )
                                            ,
                                             ( Name
                                               { nameLocalName = "type"
                                               , nameNamespace = Nothing
                                               , namePrefix = Nothing
                                               }
                                             , "Q1:Severity"
                                             )
                                            ]
                                          , elementNodes = []
                                          }
                                        )
                                    , NodeContent ""
                                    , NodeElement
                                        ( Element
                                          { elementName = Name
                                            { nameLocalName = "element"
                                            , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                            , namePrefix = Just "xsd"
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
                                             , "variations"
                                             )
                                            ,
                                             ( Name
                                               { nameLocalName = "type"
                                               , nameNamespace = Nothing
                                               , namePrefix = Nothing
                                               }
                                             , "Q1:VariationRun"
                                             )
                                            ]
                                          , elementNodes = [ NodeContent "" ]
                                          }
                                        )
                                    , NodeContent ""
                                    ]
                                  }
                                )
                            , NodeContent ""
                            , NodeElement
                                ( Element
                                  { elementName = Name
                                    { nameLocalName = "attribute"
                                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                    , namePrefix = Just "xsd"
                                    }
                                  , elementAttributes = fromList
                                    [
                                     ( Name
                                       { nameLocalName = "name"
                                       , nameNamespace = Nothing
                                       , namePrefix = Nothing
                                       }
                                     , "variationCount"
                                     )
                                    ,
                                     ( Name
                                       { nameLocalName = "type"
                                       , nameNamespace = Nothing
                                       , namePrefix = Nothing
                                       }
                                     , "xsd:int"
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
    , NodeElement
        ( Element
          { elementName = Name
            { nameLocalName = "complexType"
            , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
            , namePrefix = Just "xsd"
            }
          , elementAttributes = fromList
            [
             ( Name
               { nameLocalName = "name"
               , nameNamespace = Nothing
               , namePrefix = Nothing
               }
             , "VariationRun"
             )
            ]
          , elementNodes =
            [ NodeContent ""
            , NodeElement
                ( Element
                  { elementName = Name
                    { nameLocalName = "complexContent"
                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                    , namePrefix = Just "xsd"
                    }
                  , elementAttributes = fromList []
                  , elementNodes =
                    [ NodeContent ""
                    , NodeElement
                        ( Element
                          { elementName = Name
                            { nameLocalName = "extension"
                            , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                            , namePrefix = Just "xsd"
                            }
                          , elementAttributes = fromList
                            [
                             ( Name
                               { nameLocalName = "base"
                               , nameNamespace = Nothing
                               , namePrefix = Nothing
                               }
                             , "Q1:TestRun"
                             )
                            ]
                          , elementNodes =
                            [ NodeContent ""
                            , NodeElement
                                ( Element
                                  { elementName = Name
                                    { nameLocalName = "sequence"
                                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                    , namePrefix = Just "xsd"
                                    }
                                  , elementAttributes = fromList []
                                  , elementNodes =
                                    [ NodeContent ""
                                    , NodeElement
                                        ( Element
                                          { elementName = Name
                                            { nameLocalName = "element"
                                            , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                            , namePrefix = Just "xsd"
                                            }
                                          , elementAttributes = fromList
                                            [
                                             ( Name
                                               { nameLocalName = "name"
                                               , nameNamespace = Nothing
                                               , namePrefix = Nothing
                                               }
                                             , "result"
                                             )
                                            ,
                                             ( Name
                                               { nameLocalName = "type"
                                               , nameNamespace = Nothing
                                               , namePrefix = Nothing
                                               }
                                             , "Q1:Severity"
                                             )
                                            ]
                                          , elementNodes = []
                                          }
                                        )
                                    , NodeContent ""
                                    , NodeElement
                                        ( Element
                                          { elementName = Name
                                            { nameLocalName = "element"
                                            , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                            , namePrefix = Just "xsd"
                                            }
                                          , elementAttributes = fromList
                                            [
                                             ( Name
                                               { nameLocalName = "name"
                                               , nameNamespace = Nothing
                                               , namePrefix = Nothing
                                               }
                                             , "exception"
                                             )
                                            ,
                                             ( Name
                                               { nameLocalName = "nillable"
                                               , nameNamespace = Nothing
                                               , namePrefix = Nothing
                                               }
                                             , "true"
                                             )
                                            ,
                                             ( Name
                                               { nameLocalName = "type"
                                               , nameNamespace = Nothing
                                               , namePrefix = Nothing
                                               }
                                             , "xsd:string"
                                             )
                                            ]
                                          , elementNodes =
                                            [ NodeContent ""
                                            , NodeElement
                                                ( Element
                                                  { elementName = Name
                                                    { nameLocalName = "annotation"
                                                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                                    , namePrefix = Just "xsd"
                                                    }
                                                  , elementAttributes = fromList []
                                                  , elementNodes =
                                                    [ NodeContent ""
                                                    , NodeElement
                                                        ( Element
                                                          { elementName = Name
                                                            { nameLocalName = "documentation"
                                                            , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                                            , namePrefix = Just "xsd"
                                                            }
                                                          , elementAttributes = fromList []
                                                          , elementNodes = [
                                                             NodeContent "This element is used to display the exception of a failure or error."
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
    , NodeElement
        ( Element
          { elementName = Name
            { nameLocalName = "simpleType"
            , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
            , namePrefix = Just "xsd"
            }
          , elementAttributes = fromList
            [
             ( Name
               { nameLocalName = "name"
               , nameNamespace = Nothing
               , namePrefix = Nothing
               }
             , "Severity"
             )
            ]
          , elementNodes =
            [ NodeContent ""
            , NodeElement
                ( Element
                  { elementName = Name
                    { nameLocalName = "restriction"
                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                    , namePrefix = Just "xsd"
                    }
                  , elementAttributes = fromList
                    [
                     ( Name
                       { nameLocalName = "base"
                       , nameNamespace = Nothing
                       , namePrefix = Nothing
                       }
                     , "xsd:string"
                     )
                    ]
                  , elementNodes =
                    [ NodeContent ""
                    , NodeElement
                        ( Element
                          { elementName = Name
                            { nameLocalName = "enumeration"
                            , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                            , namePrefix = Just "xsd"
                            }
                          , elementAttributes = fromList
                            [
                             ( Name
                               { nameLocalName = "value"
                               , nameNamespace = Nothing
                               , namePrefix = Nothing
                               }
                             , "pass"
                             )
                            ]
                          , elementNodes = []
                          }
                        )
                    , NodeContent ""
                    , NodeElement
                        ( Element
                          { elementName = Name
                            { nameLocalName = "enumeration"
                            , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                            , namePrefix = Just "xsd"
                            }
                          , elementAttributes = fromList
                            [
                             ( Name
                               { nameLocalName = "value"
                               , nameNamespace = Nothing
                               , namePrefix = Nothing
                               }
                             , "fail"
                             )
                            ]
                          , elementNodes = []
                          }
                        )
                    , NodeContent ""
                    , NodeElement
                        ( Element
                          { elementName = Name
                            { nameLocalName = "enumeration"
                            , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                            , namePrefix = Just "xsd"
                            }
                          , elementAttributes = fromList
                            [
                             ( Name
                               { nameLocalName = "value"
                               , nameNamespace = Nothing
                               , namePrefix = Nothing
                               }
                             , "error"
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
    , NodeElement
        ( Element
          { elementName = Name
            { nameLocalName = "complexType"
            , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
            , namePrefix = Just "xsd"
            }
          , elementAttributes = fromList
            [
             ( Name
               { nameLocalName = "name"
               , nameNamespace = Nothing
               , namePrefix = Nothing
               }
             , "TestRun"
             )
            ]
          , elementNodes =
            [ NodeContent ""
            , NodeElement
                ( Element
                  { elementName = Name
                    { nameLocalName = "attribute"
                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                    , namePrefix = Just "xsd"
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
                     , "xsd:string"
                     )
                    ]
                  , elementNodes = []
                  }
                )
            , NodeContent ""
            , NodeElement
                ( Element
                  { elementName = Name
                    { nameLocalName = "attribute"
                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                    , namePrefix = Just "xsd"
                    }
                  , elementAttributes = fromList
                    [
                     ( Name
                       { nameLocalName = "name"
                       , nameNamespace = Nothing
                       , namePrefix = Nothing
                       }
                     , "startTime"
                     )
                    ,
                     ( Name
                       { nameLocalName = "type"
                       , nameNamespace = Nothing
                       , namePrefix = Nothing
                       }
                     , "xsd:dateTime"
                     )
                    ]
                  , elementNodes = []
                  }
                )
            , NodeContent ""
            , NodeElement
                ( Element
                  { elementName = Name
                    { nameLocalName = "attribute"
                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                    , namePrefix = Just "xsd"
                    }
                  , elementAttributes = fromList
                    [
                     ( Name
                       { nameLocalName = "name"
                       , nameNamespace = Nothing
                       , namePrefix = Nothing
                       }
                     , "endTime"
                     )
                    ,
                     ( Name
                       { nameLocalName = "type"
                       , nameNamespace = Nothing
                       , namePrefix = Nothing
                       }
                     , "xsd:dateTime"
                     )
                    ]
                  , elementNodes = []
                  }
                )
            , NodeContent ""
            , NodeElement
                ( Element
                  { elementName = Name
                    { nameLocalName = "attribute"
                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                    , namePrefix = Just "xsd"
                    }
                  , elementAttributes = fromList
                    [
                     ( Name
                       { nameLocalName = "name"
                       , nameNamespace = Nothing
                       , namePrefix = Nothing
                       }
                     , "result"
                     )
                    ,
                     ( Name
                       { nameLocalName = "type"
                       , nameNamespace = Nothing
                       , namePrefix = Nothing
                       }
                     , "xsd:string"
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
, documentEpilogue = []
}
                     
