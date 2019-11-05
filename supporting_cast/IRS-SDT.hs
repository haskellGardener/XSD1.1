irs_sdt
Document 
{ documentPrologue = Prologue 
    { prologueBefore = [ MiscComment " edited with XMLSpy v2013 (http://www.altova.com) by Internal Revenue Service (Internal Revenue Service) " ]
    , prologueDoctype = Nothing
    , prologueAfter = []
    } 
, documentRoot = Element 
  { elementName = Name 
    { nameLocalName = "schema" 
    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
    , namePrefix = Just "xsd" 
    } 
  , elementAttributes = 
    [ 
     ( Name 
       { nameLocalName = "version" 
       , nameNamespace = Nothing
       , namePrefix = Nothing
       } 
     , [ ContentText "8.2" ]
     ) 
    , 
     ( Name 
       { nameLocalName = "attributeFormDefault" 
       , nameNamespace = Nothing
       , namePrefix = Nothing
       } 
     , [ ContentText "unqualified" ]
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
       { nameLocalName = "targetNamespace" 
       , nameNamespace = Nothing
       , namePrefix = Nothing
       } 
     , [ ContentText "urn:us:gov:treasury:irs:common" ]
     ) 
    , 
     ( Name 
       { nameLocalName = "xmlns:xsd" 
       , nameNamespace = Nothing
       , namePrefix = Nothing
       } 
     , [ ContentText "http://www.w3.org/2001/XMLSchema" ]
     ) 
    , 
     ( Name 
       { nameLocalName = "xmlns" 
       , nameNamespace = Nothing
       , namePrefix = Nothing
       } 
     , [ ContentText "urn:us:gov:treasury:irs:common" ]
     ) 
    ] 
  , elementNodes = 
    [ NodeContent ( ContentText "" )
    , NodeElement 
      ( Element 
        { elementName = Name 
          { nameLocalName = "annotation" 
          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
          , namePrefix = Just "xsd" 
          } 
        , elementAttributes = []
        , elementNodes = 
          [ NodeContent ( ContentText "" )
          , NodeElement 
            ( Element 
              { elementName = Name 
                { nameLocalName = "documentation" 
                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                , namePrefix = Just "xsd" 
                } 
              , elementAttributes = []
              , elementNodes = 
                [ NodeContent ( ContentText "" )
                , NodeElement 
                  ( Element 
                    { elementName = Name 
                      { nameLocalName = "Component" 
                      , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                      , namePrefix = Nothing
                      } 
                    , elementAttributes = []
                    , elementNodes = 
                      [ NodeContent ( ContentText "" )
                      , NodeElement 
                        ( Element 
                          { elementName = Name 
                            { nameLocalName = "DictionaryEntryNm" 
                            , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                            , namePrefix = Nothing
                            } 
                          , elementAttributes = []
                          , elementNodes = 
                            [ NodeContent ( ContentText "IRS Specialized Data Types" ) ]
                          } 
                        )
                      , NodeContent ( ContentText "" )
                      , NodeElement 
                        ( Element 
                          { elementName = Name 
                            { nameLocalName = "MajorVersionNum" 
                            , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                            , namePrefix = Nothing
                            } 
                          , elementAttributes = []
                          , elementNodes = 
                            [ NodeContent ( ContentText "8" ) ]
                          } 
                        )
                      , NodeContent ( ContentText "" )
                      , NodeElement 
                        ( Element 
                          { elementName = Name 
                            { nameLocalName = "MinorVersionNum" 
                            , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                            , namePrefix = Nothing
                            } 
                          , elementAttributes = []
                          , elementNodes = 
                            [ NodeContent ( ContentText "2" ) ]
                          } 
                        )
                      , NodeContent ( ContentText "" )
                      , NodeElement 
                        ( Element 
                          { elementName = Name 
                            { nameLocalName = "VersionEffectiveBeginDt" 
                            , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                            , namePrefix = Nothing
                            } 
                          , elementAttributes = []
                          , elementNodes = 
                            [ NodeContent ( ContentText "2014-07-14" ) ]
                          } 
                        )
                      , NodeContent ( ContentText "" )
                      , NodeElement 
                        ( Element 
                          { elementName = Name 
                            { nameLocalName = "VersionDescriptionTxt" 
                            , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                            , namePrefix = Nothing
                            } 
                          , elementAttributes = []
                          , elementNodes = 
                            [ NodeContent ( ContentText "Upgrade to current BASELINE 8.2" ) ]
                          } 
                        )
                      , NodeContent ( ContentText "" )
                      , NodeElement 
                        ( Element 
                          { elementName = Name 
                            { nameLocalName = "DescriptionTxt" 
                            , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                            , namePrefix = Nothing
                            } 
                          , elementAttributes = []
                          , elementNodes = 
                            [ NodeContent ( ContentText "Specialized data types created with IRS design and validation principles" ) ]
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
    , NodeComment " ===== Imports ===== " 
    , NodeContent ( ContentText "" )
    , NodeComment " ===== Attribute Group Definitions ===== " 
    , NodeContent ( ContentText "" )
    , NodeComment " ===== Type Definitions ===== " 
    , NodeContent ( ContentText "" )
    , NodeElement 
      ( Element 
        { elementName = Name 
          { nameLocalName = "simpleType" 
          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
          , namePrefix = Just "xsd" 
          } 
        , elementAttributes = 
          [ 
           ( Name 
             { nameLocalName = "name" 
             , nameNamespace = Nothing
             , namePrefix = Nothing
             } 
           , [ ContentText "ACABusinessCorrelationIdType" ]
           ) 
          ]
        , elementNodes = 
          [ NodeContent ( ContentText "" )
          , NodeElement 
            ( Element 
              { elementName = Name 
                { nameLocalName = "annotation" 
                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                , namePrefix = Just "xsd" 
                } 
              , elementAttributes = []
              , elementNodes = 
                [ NodeContent ( ContentText "" )
                , NodeElement 
                  ( Element 
                    { elementName = Name 
                      { nameLocalName = "documentation" 
                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                      , namePrefix = Just "xsd" 
                      } 
                    , elementAttributes = []
                    , elementNodes = 
                      [ NodeContent ( ContentText "" )
                      , NodeElement 
                        ( Element 
                          { elementName = Name 
                            { nameLocalName = "Component" 
                            , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                            , namePrefix = Nothing
                            } 
                          , elementAttributes = []
                          , elementNodes = 
                            [ NodeContent ( ContentText "" )
                            , NodeElement 
                              ( Element 
                                { elementName = Name 
                                  { nameLocalName = "DictionaryEntryNm" 
                                  , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                  , namePrefix = Nothing
                                  } 
                                , elementAttributes = []
                                , elementNodes = 
                                  [ NodeContent ( ContentText "ACA (Affordable Care Act) Business Correlation Identification Type" ) ]
                                } 
                              )
                            , NodeContent ( ContentText "" )
                            , NodeElement 
                              ( Element 
                                { elementName = Name 
                                  { nameLocalName = "MajorVersionNum" 
                                  , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                  , namePrefix = Nothing
                                  } 
                                , elementAttributes = []
                                , elementNodes = 
                                  [ NodeContent ( ContentText "1" ) ]
                                } 
                              )
                            , NodeContent ( ContentText "" )
                            , NodeElement 
                              ( Element 
                                { elementName = Name 
                                  { nameLocalName = "MinorVersionNum" 
                                  , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                  , namePrefix = Nothing
                                  } 
                                , elementAttributes = []
                                , elementNodes = 
                                  [ NodeContent ( ContentText "0" ) ]
                                } 
                              )
                            , NodeContent ( ContentText "" )
                            , NodeElement 
                              ( Element 
                                { elementName = Name 
                                  { nameLocalName = "VersionEffectiveBeginDt" 
                                  , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                  , namePrefix = Nothing
                                  } 
                                , elementAttributes = []
                                , elementNodes = 
                                  [ NodeContent ( ContentText "2012-07-11" ) ]
                                } 
                              )
                            , NodeContent ( ContentText "" )
                            , NodeElement 
                              ( Element 
                                { elementName = Name 
                                  { nameLocalName = "VersionDescriptionTxt" 
                                  , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                  , namePrefix = Nothing
                                  } 
                                , elementAttributes = []
                                , elementNodes = 
                                  [ NodeContent ( ContentText "Initial version" ) ]
                                } 
                              )
                            , NodeContent ( ContentText "" )
                            , NodeElement 
                              ( Element 
                                { elementName = Name 
                                  { nameLocalName = "DescriptionTxt" 
                                  , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                  , namePrefix = Nothing
                                  } 
                                , elementAttributes = []
                                , elementNodes = 
                                  [ NodeContent ( ContentText "A business correlation identifier used for all request and response messages in ACA messages exchanges" ) ]
                                } 
                              )
                            , NodeContent ( ContentText "" )
                            , NodeElement 
                              ( Element 
                                { elementName = Name 
                                  { nameLocalName = "DataElementId" 
                                  , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                  , namePrefix = Nothing
                                  } 
                                , elementAttributes = []
                                , elementNodes = 
                                  [ NodeContent ( ContentText "163258" ) ]
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
                { nameLocalName = "restriction" 
                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                , namePrefix = Just "xsd" 
                } 
              , elementAttributes = 
                [ 
                 ( Name 
                   { nameLocalName = "base" 
                   , nameNamespace = Nothing
                   , namePrefix = Nothing
                   } 
                 , [ ContentText "xsd:token" ]
                 ) 
                ]
              , elementNodes = 
                [ NodeContent ( ContentText "" )
                , NodeElement 
                  ( Element 
                    { elementName = Name 
                      { nameLocalName = "pattern" 
                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                      , namePrefix = Just "xsd" 
                      } 
                    , elementAttributes = 
                      [ 
                       ( Name 
                         { nameLocalName = "value" 
                         , nameNamespace = Nothing
                         , namePrefix = Nothing
                         } 
                       , [ ContentText "[0-9a-zA-Z]{8}-[0-9a-zA-Z]{4}-[0-9a-zA-Z]{4}-[0-9a-zA-Z]{4}-[0-9a-zA-Z]{12}:[a-zA-Z0]{5}::[0-9]{2}\.[a-zA-Z]{2}[a-zA-Z*]{1}\.[a-zA-Z0-9]{3}\.[0-9]{3}\.[0-9]{3}:(T|B|O)" ]
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
          , namePrefix = Just "xsd" 
          } 
        , elementAttributes = 
          [ 
           ( Name 
             { nameLocalName = "name" 
             , nameNamespace = Nothing
             , namePrefix = Nothing
             } 
           , [ ContentText "AcknowledgementStatusCodeType" ]
           ) 
          ]
        , elementNodes = 
          [ NodeContent ( ContentText "" )
          , NodeElement 
            ( Element 
              { elementName = Name 
                { nameLocalName = "annotation" 
                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                , namePrefix = Just "xsd" 
                } 
              , elementAttributes = []
              , elementNodes = 
                [ NodeContent ( ContentText "" )
                , NodeElement 
                  ( Element 
                    { elementName = Name 
                      { nameLocalName = "documentation" 
                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                      , namePrefix = Just "xsd" 
                      } 
                    , elementAttributes = []
                    , elementNodes = 
                      [ NodeContent ( ContentText "" )
                      , NodeElement 
                        ( Element 
                          { elementName = Name 
                            { nameLocalName = "Component" 
                            , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                            , namePrefix = Nothing
                            } 
                          , elementAttributes = []
                          , elementNodes = 
                            [ NodeContent ( ContentText "" )
                            , NodeElement 
                              ( Element 
                                { elementName = Name 
                                  { nameLocalName = "DictionaryEntryNm" 
                                  , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                  , namePrefix = Nothing
                                  } 
                                , elementAttributes = []
                                , elementNodes = 
                                  [ NodeContent ( ContentText "Acknowledgement Status Code Type" ) ]
                                } 
                              )
                            , NodeContent ( ContentText "" )
                            , NodeElement 
                              ( Element 
                                { elementName = Name 
                                  { nameLocalName = "MajorVersionNum" 
                                  , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                  , namePrefix = Nothing
                                  } 
                                , elementAttributes = []
                                , elementNodes = 
                                  [ NodeContent ( ContentText "1" ) ]
                                } 
                              )
                            , NodeContent ( ContentText "" )
                            , NodeElement 
                              ( Element 
                                { elementName = Name 
                                  { nameLocalName = "MinorVersionNum" 
                                  , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                  , namePrefix = Nothing
                                  } 
                                , elementAttributes = []
                                , elementNodes = 
                                  [ NodeContent ( ContentText "0" ) ]
                                } 
                              )
                            , NodeContent ( ContentText "" )
                            , NodeElement 
                              ( Element 
                                { elementName = Name 
                                  { nameLocalName = "VersionEffectiveBeginDt" 
                                  , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                  , namePrefix = Nothing
                                  } 
                                , elementAttributes = []
                                , elementNodes = 
                                  [ NodeContent ( ContentText "2012-07-11" ) ]
                                } 
                              )
                            , NodeContent ( ContentText "" )
                            , NodeElement 
                              ( Element 
                                { elementName = Name 
                                  { nameLocalName = "VersionDescriptionTxt" 
                                  , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                  , namePrefix = Nothing
                                  } 
                                , elementAttributes = []
                                , elementNodes = 
                                  [ NodeContent ( ContentText "Initial version" ) ]
                                } 
                              )
                            , NodeContent ( ContentText "" )
                            , NodeElement 
                              ( Element 
                                { elementName = Name 
                                  { nameLocalName = "DescriptionTxt" 
                                  , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                  , namePrefix = Nothing
                                  } 
                                , elementAttributes = []
                                , elementNodes = 
                                  [ NodeContent ( ContentText "A code used to indicate the status of an acknowledgement." ) ]
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
                { nameLocalName = "restriction" 
                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                , namePrefix = Just "xsd" 
                } 
              , elementAttributes = 
                [ 
                 ( Name 
                   { nameLocalName = "base" 
                   , nameNamespace = Nothing
                   , namePrefix = Nothing
                   } 
                 , [ ContentText "xsd:string" ]
                 ) 
                ]
              , elementNodes = 
                [ NodeContent ( ContentText "" )
                , NodeElement 
                  ( Element 
                    { elementName = Name 
                      { nameLocalName = "enumeration" 
                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                      , namePrefix = Just "xsd" 
                      } 
                    , elementAttributes = 
                      [ 
                       ( Name 
                         { nameLocalName = "value" 
                         , nameNamespace = Nothing
                         , namePrefix = Nothing
                         } 
                       , [ ContentText "SUCCESS" ]
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
                      , namePrefix = Just "xsd" 
                      } 
                    , elementAttributes = 
                      [ 
                       ( Name 
                         { nameLocalName = "value" 
                         , nameNamespace = Nothing
                         , namePrefix = Nothing
                         } 
                       , [ ContentText "FAILURE" ]
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
          , namePrefix = Just "xsd" 
          } 
        , elementAttributes = 
          [ 
           ( Name 
             { nameLocalName = "name" 
             , nameNamespace = Nothing
             , namePrefix = Nothing
             } 
           , [ ContentText "AfaStatusCodeType" ]
           ) 
          ]
        , elementNodes = 
          [ NodeContent ( ContentText "" )
          , NodeElement 
            ( Element 
              { elementName = Name 
                { nameLocalName = "annotation" 
                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                , namePrefix = Just "xsd" 
                } 
              , elementAttributes = []
              , elementNodes = 
                [ NodeContent ( ContentText "" )
                , NodeElement 
                  ( Element 
                    { elementName = Name 
                      { nameLocalName = "documentation" 
                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                      , namePrefix = Just "xsd" 
                      } 
                    , elementAttributes = []
                    , elementNodes = 
                      [ NodeContent ( ContentText "" )
                      , NodeElement 
                        ( Element 
                          { elementName = Name 
                            { nameLocalName = "Component" 
                            , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                            , namePrefix = Nothing
                            } 
                          , elementAttributes = []
                          , elementNodes = 
                            [ NodeContent ( ContentText "" )
                            , NodeElement 
                              ( Element 
                                { elementName = Name 
                                  { nameLocalName = "DictionaryEntryNm" 
                                  , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                  , namePrefix = Nothing
                                  } 
                                , elementAttributes = []
                                , elementNodes = 
                                  [ NodeContent ( ContentText "AFA Submissions Status Code Type" ) ]
                                } 
                              )
                            , NodeContent ( ContentText "" )
                            , NodeElement 
                              ( Element 
                                { elementName = Name 
                                  { nameLocalName = "MajorVersionNum" 
                                  , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                  , namePrefix = Nothing
                                  } 
                                , elementAttributes = []
                                , elementNodes = 
                                  [ NodeContent ( ContentText "1" ) ]
                                } 
                              )
                            , NodeContent ( ContentText "" )
                            , NodeElement 
                              ( Element 
                                { elementName = Name 
                                  { nameLocalName = "MinorVersionNum" 
                                  , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                  , namePrefix = Nothing
                                  } 
                                , elementAttributes = []
                                , elementNodes = 
                                  [ NodeContent ( ContentText "0" ) ]
                                } 
                              )
                            , NodeContent ( ContentText "" )
                            , NodeElement 
                              ( Element 
                                { elementName = Name 
                                  { nameLocalName = "VersionEffectiveBeginDt" 
                                  , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                  , namePrefix = Nothing
                                  } 
                                , elementAttributes = []
                                , elementNodes = 
                                  [ NodeContent ( ContentText "2013-10-22" ) ]
                                } 
                              )
                            , NodeContent ( ContentText "" )
                            , NodeElement 
                              ( Element 
                                { elementName = Name 
                                  { nameLocalName = "VersionDescriptionTxt" 
                                  , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                  , namePrefix = Nothing
                                  } 
                                , elementAttributes = []
                                , elementNodes = 
                                  [ NodeContent ( ContentText "Initial version" ) ]
                                } 
                              )
                            , NodeContent ( ContentText "" )
                            , NodeElement 
                              ( Element 
                                { elementName = Name 
                                  { nameLocalName = "DescriptionTxt" 
                                  , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                  , namePrefix = Nothing
                                  } 
                                , elementAttributes = []
                                , elementNodes = 
                                  [ NodeContent ( ContentText "The AFA status submission status type code" ) ]
                                } 
                              )
                            , NodeContent ( ContentText "" )
                            , NodeElement 
                              ( Element 
                                { elementName = Name 
                                  { nameLocalName = "DataElementId" 
                                  , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                  , namePrefix = Nothing
                                  } 
                                , elementAttributes = []
                                , elementNodes = 
                                  [ NodeContent ( ContentText "" ) ]
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
                { nameLocalName = "restriction" 
                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                , namePrefix = Just "xsd" 
                } 
              , elementAttributes = 
                [ 
                 ( Name 
                   { nameLocalName = "base" 
                   , nameNamespace = Nothing
                   , namePrefix = Nothing
                   } 
                 , [ ContentText "xsd:string" ]
                 ) 
                ]
              , elementNodes = 
                [ NodeContent ( ContentText "" )
                , NodeElement 
                  ( Element 
                    { elementName = Name 
                      { nameLocalName = "enumeration" 
                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                      , namePrefix = Just "xsd" 
                      } 
                    , elementAttributes = 
                      [ 
                       ( Name 
                         { nameLocalName = "value" 
                         , nameNamespace = Nothing
                         , namePrefix = Nothing
                         } 
                       , [ ContentText "Success" ]
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
                      , namePrefix = Just "xsd" 
                      } 
                    , elementAttributes = 
                      [ 
                       ( Name 
                         { nameLocalName = "value" 
                         , nameNamespace = Nothing
                         , namePrefix = Nothing
                         } 
                       , [ ContentText "Failure" ]
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
                      , namePrefix = Just "xsd" 
                      } 
                    , elementAttributes = 
                      [ 
                       ( Name 
                         { nameLocalName = "value" 
                         , nameNamespace = Nothing
                         , namePrefix = Nothing
                         } 
                       , [ ContentText "Error" ]
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
          , namePrefix = Just "xsd" 
          } 
        , elementAttributes = 
          [ 
           ( Name 
             { nameLocalName = "name" 
             , nameNamespace = Nothing
             , namePrefix = Nothing
             } 
           , [ ContentText "AmountType" ]
           ) 
          ]
        , elementNodes = 
          [ NodeContent ( ContentText "" )
          , NodeElement 
            ( Element 
              { elementName = Name 
                { nameLocalName = "annotation" 
                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                , namePrefix = Just "xsd" 
                } 
              , elementAttributes = []
              , elementNodes = 
                [ NodeContent ( ContentText "" )
                , NodeElement 
                  ( Element 
                    { elementName = Name 
                      { nameLocalName = "documentation" 
                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                      , namePrefix = Just "xsd" 
                      } 
                    , elementAttributes = []
                    , elementNodes = 
                      [ NodeContent ( ContentText "" )
                      , NodeElement 
                        ( Element 
                          { elementName = Name 
                            { nameLocalName = "Component" 
                            , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                            , namePrefix = Nothing
                            } 
                          , elementAttributes = []
                          , elementNodes = 
                            [ NodeContent ( ContentText "" )
                            , NodeElement 
                              ( Element 
                                { elementName = Name 
                                  { nameLocalName = "DictionaryEntryNm" 
                                  , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                  , namePrefix = Nothing
                                  } 
                                , elementAttributes = []
                                , elementNodes = 
                                  [ NodeContent ( ContentText "Amount Type" ) ]
                                } 
                              )
                            , NodeContent ( ContentText "" )
                            , NodeElement 
                              ( Element 
                                { elementName = Name 
                                  { nameLocalName = "MajorVersionNum" 
                                  , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                  , namePrefix = Nothing
                                  } 
                                , elementAttributes = []
                                , elementNodes = 
                                  [ NodeContent ( ContentText "2" ) ]
                                } 
                              )
                            , NodeContent ( ContentText "" )
                            , NodeElement 
                              ( Element 
                                { elementName = Name 
                                  { nameLocalName = "MinorVersionNum" 
                                  , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                  , namePrefix = Nothing
                                  } 
                                , elementAttributes = []
                                , elementNodes = 
                                  [ NodeContent ( ContentText "0" ) ]
                                } 
                              )
                            , NodeContent ( ContentText "" )
                            , NodeElement 
                              ( Element 
                                { elementName = Name 
                                  { nameLocalName = "VersionEffectiveBeginDt" 
                                  , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                  , namePrefix = Nothing
                                  } 
                                , elementAttributes = []
                                , elementNodes = 
                                  [ NodeContent ( ContentText "2005-06-06" ) ]
                                } 
                              )
                            , NodeContent ( ContentText "" )
                            , NodeElement 
                              ( Element 
                                { elementName = Name 
                                  { nameLocalName = "VersionDescriptionTxt" 
                                  , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                  , namePrefix = Nothing
                                  } 
                                , elementAttributes = []
                                , elementNodes = 
                                  [ NodeContent ( ContentText "Initial version" ) ]
                                } 
                              )
                            , NodeContent ( ContentText "" )
                            , NodeElement 
                              ( Element 
                                { elementName = Name 
                                  { nameLocalName = "DescriptionTxt" 
                                  , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                  , namePrefix = Nothing
                                  } 
                                , elementAttributes = []
                                , elementNodes = 
                                  [ NodeContent ( ContentText "Base type for US currency amount" ) ]
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
                { nameLocalName = "restriction" 
                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                , namePrefix = Just "xsd" 
                } 
              , elementAttributes = 
                [ 
                 ( Name 
                   { nameLocalName = "base" 
                   , nameNamespace = Nothing
                   , namePrefix = Nothing
                   } 
                 , [ ContentText "xsd:decimal" ]
                 ) 
                ]
              , elementNodes = 
                [ NodeContent ( ContentText "" )
                , NodeElement 
                  ( Element 
                    { elementName = Name 
                      { nameLocalName = "totalDigits" 
                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                      , namePrefix = Just "xsd" 
                      } 
                    , elementAttributes = 
                      [ 
                       ( Name 
                         { nameLocalName = "value" 
                         , nameNamespace = Nothing
                         , namePrefix = Nothing
                         } 
                       , [ ContentText "19" ]
                       ) 
                      ]
                    , elementNodes = []
                    } 
                  )
                , NodeContent ( ContentText "" )
                , NodeElement 
                  ( Element 
                    { elementName = Name 
                      { nameLocalName = "fractionDigits" 
                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                      , namePrefix = Just "xsd" 
                      } 
                    , elementAttributes = 
                      [ 
                       ( Name 
                         { nameLocalName = "value" 
                         , nameNamespace = Nothing
                         , namePrefix = Nothing
                         } 
                       , [ ContentText "2" ]
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
          , namePrefix = Just "xsd" 
          } 
        , elementAttributes = 
          [ 
           ( Name 
             { nameLocalName = "name" 
             , nameNamespace = Nothing
             , namePrefix = Nothing
             } 
           , [ ContentText "AppealReasonCdType" ]
           ) 
          ]
        , elementNodes = 
          [ NodeContent ( ContentText "" )
          , NodeElement 
            ( Element 
              { elementName = Name 
                { nameLocalName = "annotation" 
                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                , namePrefix = Just "xsd" 
                } 
              , elementAttributes = []
              , elementNodes = 
                [ NodeContent ( ContentText "" )
                , NodeElement 
                  ( Element 
                    { elementName = Name 
                      { nameLocalName = "documentation" 
                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                      , namePrefix = Just "xsd" 
                      } 
                    , elementAttributes = []
                    , elementNodes = 
                      [ NodeContent ( ContentText "" )
                      , NodeElement 
                        ( Element 
                          { elementName = Name 
                            { nameLocalName = "Component" 
                            , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                            , namePrefix = Nothing
                            } 
                          , elementAttributes = []
                          , elementNodes = 
                            [ NodeContent ( ContentText "" )
                            , NodeElement 
                              ( Element 
                                { elementName = Name 
                                  { nameLocalName = "DictionaryEntryNm" 
                                  , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                  , namePrefix = Nothing
                                  } 
                                , elementAttributes = []
                                , elementNodes = 
                                  [ NodeContent ( ContentText "AppealReasonCodeType" ) ]
                                } 
                              )
                            , NodeContent ( ContentText "" )
                            , NodeElement 
                              ( Element 
                                { elementName = Name 
                                  { nameLocalName = "MajorVersionNum" 
                                  , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                  , namePrefix = Nothing
                                  } 
                                , elementAttributes = []
                                , elementNodes = 
                                  [ NodeContent ( ContentText "1" ) ]
                                } 
                              )
                            , NodeContent ( ContentText "" )
                            , NodeElement 
                              ( Element 
                                { elementName = Name 
                                  { nameLocalName = "MinorVersionNum" 
                                  , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                  , namePrefix = Nothing
                                  } 
                                , elementAttributes = []
                                , elementNodes = 
                                  [ NodeContent ( ContentText "0" ) ]
                                } 
                              )
                            , NodeContent ( ContentText "" )
                            , NodeElement 
                              ( Element 
                                { elementName = Name 
                                  { nameLocalName = "VersionEffectiveBeginDt" 
                                  , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                  , namePrefix = Nothing
                                  } 
                                , elementAttributes = []
                                , elementNodes = 
                                  [ NodeContent ( ContentText "2012-08-18" ) ]
                                } 
                              )
                            , NodeContent ( ContentText "" )
                            , NodeElement 
                              ( Element 
                                { elementName = Name 
                                  { nameLocalName = "VersionDescriptionTxt" 
                                  , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                  , namePrefix = Nothing
                                  } 
                                , elementAttributes = []
                                , elementNodes = 
                                  [ NodeContent ( ContentText "Initial version" ) ]
                                } 
                              )
                            , NodeContent ( ContentText "" )
                            , NodeElement 
                              ( Element 
                                { elementName = Name 
                                  { nameLocalName = "DescriptionTxt" 
                                  , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                  , namePrefix = Nothing
                                  } 
                                , elementAttributes = []
                                , elementNodes = 
                                  [ NodeContent ( ContentText "The value options related to a appeal reason" ) ]
                                } 
                              )
                            , NodeContent ( ContentText "" )
                            , NodeElement 
                              ( Element 
                                { elementName = Name 
                                  { nameLocalName = "DataElementId" 
                                  , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                  , namePrefix = Nothing
                                  } 
                                , elementAttributes = []
                                , elementNodes = 
                                  [ NodeContent ( ContentText "" ) ]
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
                { nameLocalName = "restriction" 
                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                , namePrefix = Just "xsd" 
                } 
              , elementAttributes = 
                [ 
                 ( Name 
                   { nameLocalName = "base" 
                   , nameNamespace = Nothing
                   , namePrefix = Nothing
                   } 
                 , [ ContentText "xsd:string" ]
                 ) 
                ]
              , elementNodes = 
                [ NodeContent ( ContentText "" )
                , NodeElement 
                  ( Element 
                    { elementName = Name 
                      { nameLocalName = "enumeration" 
                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                      , namePrefix = Just "xsd" 
                      } 
                    , elementAttributes = 
                      [ 
                       ( Name 
                         { nameLocalName = "value" 
                         , nameNamespace = Nothing
                         , namePrefix = Nothing
                         } 
                       , [ ContentText "Individual eligibility for coverage" ]
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
                      , namePrefix = Just "xsd" 
                      } 
                    , elementAttributes = 
                      [ 
                       ( Name 
                         { nameLocalName = "value" 
                         , nameNamespace = Nothing
                         , namePrefix = Nothing
                         } 
                       , [ ContentText "Individual eligibility for exception" ]
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
                      , namePrefix = Just "xsd" 
                      } 
                    , elementAttributes = 
                      [ 
                       ( Name 
                         { nameLocalName = "value" 
                         , nameNamespace = Nothing
                         , namePrefix = Nothing
                         } 
                       , [ ContentText "Individual eligibility for APTC" ]
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
                      , namePrefix = Just "xsd" 
                      } 
                    , elementAttributes = 
                      [ 
                       ( Name 
                         { nameLocalName = "value" 
                         , nameNamespace = Nothing
                         , namePrefix = Nothing
                         } 
                       , [ ContentText "Employer does not offer MEC" ]
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
                      , namePrefix = Just "xsd" 
                      } 
                    , elementAttributes = 
                      [ 
                       ( Name 
                         { nameLocalName = "value" 
                         , nameNamespace = Nothing
                         , namePrefix = Nothing
                         } 
                       , [ ContentText "Employer MEC is unaffordable" ]
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
          , namePrefix = Just "xsd" 
          } 
        , elementAttributes = 
          [ 
           ( Name 
             { nameLocalName = "name" 
             , nameNamespace = Nothing
             , namePrefix = Nothing
             } 
           , [ ContentText "BatchCategoryCodeType" ]
           ) 
          ]
        , elementNodes = 
          [ NodeContent ( ContentText "" )
          , NodeElement 
            ( Element 
              { elementName = Name 
                { nameLocalName = "annotation" 
                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                , namePrefix = Just "xsd" 
                } 
              , elementAttributes = []
              , elementNodes = 
                [ NodeContent ( ContentText "" )
                , NodeElement 
                  ( Element 
                    { elementName = Name 
                      { nameLocalName = "documentation" 
                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                      , namePrefix = Just "xsd" 
                      } 
                    , elementAttributes = []
                    , elementNodes = 
                      [ NodeContent ( ContentText "" )
                      , NodeElement 
                        ( Element 
                          { elementName = Name 
                            { nameLocalName = "Component" 
                            , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                            , namePrefix = Nothing
                            } 
                          , elementAttributes = []
                          , elementNodes = 
                            [ NodeContent ( ContentText "" )
                            , NodeElement 
                              ( Element 
                                { elementName = Name 
                                  { nameLocalName = "DictionaryEntryNm" 
                                  , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                  , namePrefix = Nothing
                                  } 
                                , elementAttributes = []
                                , elementNodes = 
                                  [ NodeContent ( ContentText "Batch Category Code Type" ) ]
                                } 
                              )
                            , NodeContent ( ContentText "" )
                            , NodeElement 
                              ( Element 
                                { elementName = Name 
                                  { nameLocalName = "MajorVersionNum" 
                                  , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                  , namePrefix = Nothing
                                  } 
                                , elementAttributes = []
                                , elementNodes = 
                                  [ NodeContent ( ContentText "1" ) ]
                                } 
                              )
                            , NodeContent ( ContentText "" )
                            , NodeElement 
                              ( Element 
                                { elementName = Name 
                                  { nameLocalName = "MinorVersionNum" 
                                  , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                  , namePrefix = Nothing
                                  } 
                                , elementAttributes = []
                                , elementNodes = 
                                  [ NodeContent ( ContentText "0" ) ]
                                } 
                              )
                            , NodeContent ( ContentText "" )
                            , NodeElement 
                              ( Element 
                                { elementName = Name 
                                  { nameLocalName = "VersionEffectiveBeginDt" 
                                  , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                  , namePrefix = Nothing
                                  } 
                                , elementAttributes = []
                                , elementNodes = 
                                  [ NodeContent ( ContentText "2012-09-05" ) ]
                                } 
                              )
                            , NodeContent ( ContentText "" )
                            , NodeElement 
                              ( Element 
                                { elementName = Name 
                                  { nameLocalName = "VersionDescriptionTxt" 
                                  , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                  , namePrefix = Nothing
                                  } 
                                , elementAttributes = []
                                , elementNodes = 
                                  [ NodeContent ( ContentText "Initial version" ) ]
                                } 
                              )
                            , NodeContent ( ContentText "" )
                            , NodeElement 
                              ( Element 
                                { elementName = Name 
                                  { nameLocalName = "DescriptionTxt" 
                                  , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                  , namePrefix = Nothing
                                  } 
                                , elementAttributes = []
                                , elementNodes = 
                                  [ NodeContent ( ContentText "This is a cummulative list of predetermined values identifying each possible kind of batch processing (List will be updated as new bulk services are implemented)." ) ]
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
                { nameLocalName = "restriction" 
                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                , namePrefix = Just "xsd" 
                } 
              , elementAttributes = 
                [ 
                 ( Name 
                   { nameLocalName = "base" 
                   , nameNamespace = Nothing
                   , namePrefix = Nothing
                   } 
                 , [ ContentText "xsd:string" ]
                 ) 
                ]
              , elementNodes = 
                [ NodeContent ( ContentText "" )
                , NodeElement 
                  ( Element 
                    { elementName = Name 
                      { nameLocalName = "enumeration" 
                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                      , namePrefix = Just "xsd" 
                      } 
                    , elementAttributes = 
                      [ 
                       ( Name 
                         { nameLocalName = "value" 
                         , nameNamespace = Nothing
                         , namePrefix = Nothing
                         } 
                       , [ ContentText "IRS_EOM_REQ" ]
                       ) 
                      ]
                    , elementNodes = 
                      [ NodeContent ( ContentText "" )
                      , NodeElement 
                        ( Element 
                          { elementName = Name 
                            { nameLocalName = "annotation" 
                            , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                            , namePrefix = Just "xsd" 
                            } 
                          , elementAttributes = []
                          , elementNodes = 
                            [ NodeContent ( ContentText "" )
                            , NodeElement 
                              ( Element 
                                { elementName = Name 
                                  { nameLocalName = "documentation" 
                                  , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                  , namePrefix = Just "xsd" 
                                  } 
                                , elementAttributes = []
                                , elementNodes = 
                                  [ NodeContent ( ContentText "Identifies End of Month Request processing" ) ]
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
                      { nameLocalName = "enumeration" 
                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                      , namePrefix = Just "xsd" 
                      } 
                    , elementAttributes = 
                      [ 
                       ( Name 
                         { nameLocalName = "value" 
                         , nameNamespace = Nothing
                         , namePrefix = Nothing
                         } 
                       , [ ContentText "IRS_EOY_REQ" ]
                       ) 
                      ]
                    , elementNodes = 
                      [ NodeContent ( ContentText "" )
                      , NodeElement 
                        ( Element 
                          { elementName = Name 
                            { nameLocalName = "annotation" 
                            , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                            , namePrefix = Just "xsd" 
                            } 
                          , elementAttributes = []
                          , elementNodes = 
                            [ NodeContent ( ContentText "" )
                            , NodeElement 
                              ( Element 
                                { elementName = Name 
                                  { nameLocalName = "documentation" 
                                  , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                  , namePrefix = Just "xsd" 
                                  } 
                                , elementAttributes = []
                                , elementNodes = 
                                  [ NodeContent ( ContentText "Identifies End of year Request processing" ) ]
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
                      { nameLocalName = "enumeration" 
                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                      , namePrefix = Just "xsd" 
                      } 
                    , elementAttributes = 
                      [ 
                       ( Name 
                         { nameLocalName = "value" 
                         , nameNamespace = Nothing
                         , namePrefix = Nothing
                         } 
                       , [ ContentText "IRS_APTC_REQ" ]
                       ) 
                      ]
                    , elementNodes = 
                      [ NodeContent ( ContentText "" )
                      , NodeElement 
                        ( Element 
                          { elementName = Name 
                            { nameLocalName = "annotation" 
                            , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                            , namePrefix = Just "xsd" 
                            } 
                          , elementAttributes = []
                          , elementNodes = 
                            [ NodeContent ( ContentText "" )
                            , NodeElement 
                              ( Element 
                                { elementName = Name 
                                  { nameLocalName = "documentation" 
                                  , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                  , namePrefix = Just "xsd" 
                                  } 
                                , elementAttributes = []
                                , elementNodes = 
                                  [ NodeContent ( ContentText "Identifies APTC Request processing" ) ]
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
                      { nameLocalName = "enumeration" 
                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                      , namePrefix = Just "xsd" 
                      } 
                    , elementAttributes = 
                      [ 
                       ( Name 
                         { nameLocalName = "value" 
                         , nameNamespace = Nothing
                         , namePrefix = Nothing
                         } 
                       , [ ContentText "IRS_IFSV_REQ" ]
                       ) 
                      ]
                    , elementNodes = 
                      [ NodeContent ( ContentText "" )
                      , NodeElement 
                        ( Element 
                          { elementName = Name 
                            { nameLocalName = "annotation" 
                            , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                            , namePrefix = Just "xsd" 
                            } 
                          , elementAttributes = []
                          , elementNodes = 
                            [ NodeContent ( ContentText "" )
                            , NodeElement 
                              ( Element 
                                { elementName = Name 
                                  { nameLocalName = "documentation" 
                                  , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                  , namePrefix = Just "xsd" 
                                  } 
                                , elementAttributes = []
                                , elementNodes = 
                                  [ NodeContent ( ContentText "Identifies IFSV Request processing" ) ]
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
                      { nameLocalName = "enumeration" 
                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                      , namePrefix = Just "xsd" 
                      } 
                    , elementAttributes = 
                      [ 
                       ( Name 
                         { nameLocalName = "value" 
                         , nameNamespace = Nothing
                         , namePrefix = Nothing
                         } 
                       , [ ContentText "IRS_EOM_RESUBMIT_FILE_REQ" ]
                       ) 
                      ]
                    , elementNodes = 
                      [ NodeContent ( ContentText "" )
                      , NodeElement 
                        ( Element 
                          { elementName = Name 
                            { nameLocalName = "annotation" 
                            , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                            , namePrefix = Just "xsd" 
                            } 
                          , elementAttributes = []
                          , elementNodes = 
                            [ NodeContent ( ContentText "" )
                            , NodeElement 
                              ( Element 
                                { elementName = Name 
                                  { nameLocalName = "documentation" 
                                  , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                  , namePrefix = Just "xsd" 
                                  } 
                                , elementAttributes = []
                                , elementNodes = 
                                  [ NodeContent ( ContentText "Identifies End of Month File Resubmit Request processing" ) ]
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
                      { nameLocalName = "enumeration" 
                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                      , namePrefix = Just "xsd" 
                      } 
                    , elementAttributes = 
                      [ 
                       ( Name 
                         { nameLocalName = "value" 
                         , nameNamespace = Nothing
                         , namePrefix = Nothing
                         } 
                       , [ ContentText "IRS_EOM_RESUBMIT_BATCH_REQ" ]
                       ) 
                      ]
                    , elementNodes = 
                      [ NodeContent ( ContentText "" )
                      , NodeElement 
                        ( Element 
                          { elementName = Name 
                            { nameLocalName = "annotation" 
                            , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                            , namePrefix = Just "xsd" 
                            } 
                          , elementAttributes = []
                          , elementNodes = 
                            [ NodeContent ( ContentText "" )
                            , NodeElement 
                              ( Element 
                                { elementName = Name 
                                  { nameLocalName = "documentation" 
                                  , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                  , namePrefix = Just "xsd" 
                                  } 
                                , elementAttributes = []
                                , elementNodes = 
                                  [ NodeContent ( ContentText "Identifies End of Month Batch Resubmit Request processing" ) ]
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
                      { nameLocalName = "enumeration" 
                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                      , namePrefix = Just "xsd" 
                      } 
                    , elementAttributes = 
                      [ 
                       ( Name 
                         { nameLocalName = "value" 
                         , nameNamespace = Nothing
                         , namePrefix = Nothing
                         } 
                       , [ ContentText "IRS_EOM_RESUBMIT_MISSING_FILE_REQ" ]
                       ) 
                      ]
                    , elementNodes = 
                      [ NodeContent ( ContentText "" )
                      , NodeElement 
                        ( Element 
                          { elementName = Name 
                            { nameLocalName = "annotation" 
                            , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                            , namePrefix = Just "xsd" 
                            } 
                          , elementAttributes = []
                          , elementNodes = 
                            [ NodeContent ( ContentText "" )
                            , NodeElement 
                              ( Element 
                                { elementName = Name 
                                  { nameLocalName = "documentation" 
                                  , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                  , namePrefix = Just "xsd" 
                                  } 
                                , elementAttributes = []
                                , elementNodes = 
                                  [ NodeContent ( ContentText "Identifies End of Month Missing File Resubmit Request processing" ) ]
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
                      { nameLocalName = "enumeration" 
                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                      , namePrefix = Just "xsd" 
                      } 
                    , elementAttributes = 
                      [ 
                       ( Name 
                         { nameLocalName = "value" 
                         , nameNamespace = Nothing
                         , namePrefix = Nothing
                         } 
                       , [ ContentText "IRS_EOY_RESUBMIT_FILE_REQ" ]
                       ) 
                      ]
                    , elementNodes = 
                      [ NodeContent ( ContentText "" )
                      , NodeElement 
                        ( Element 
                          { elementName = Name 
                            { nameLocalName = "annotation" 
                            , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                            , namePrefix = Just "xsd" 
                            } 
                          , elementAttributes = []
                          , elementNodes = 
                            [ NodeContent ( ContentText "" )
                            , NodeElement 
                              ( Element 
                                { elementName = Name 
                                  { nameLocalName = "documentation" 
                                  , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                  , namePrefix = Just "xsd" 
                                  } 
                                , elementAttributes = []
                                , elementNodes = 
                                  [ NodeContent ( ContentText "Identifies End of Year File Resubmit Request processing" ) ]
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
                      { nameLocalName = "enumeration" 
                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                      , namePrefix = Just "xsd" 
                      } 
                    , elementAttributes = 
                      [ 
                       ( Name 
                         { nameLocalName = "value" 
                         , nameNamespace = Nothing
                         , namePrefix = Nothing
                         } 
                       , [ ContentText "IRS_EOY_RESUBMIT_BATCH_REQ" ]
                       ) 
                      ]
                    , elementNodes = 
                      [ NodeContent ( ContentText "" )
                      , NodeElement 
                        ( Element 
                          { elementName = Name 
                            { nameLocalName = "annotation" 
                            , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                            , namePrefix = Just "xsd" 
                            } 
                          , elementAttributes = []
                          , elementNodes = 
                            [ NodeContent ( ContentText "" )
                            , NodeElement 
                              ( Element 
                                { elementName = Name 
                                  { nameLocalName = "documentation" 
                                  , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                  , namePrefix = Just "xsd" 
                                  } 
                                , elementAttributes = []
                                , elementNodes = 
                                  [ NodeContent ( ContentText "Identifies End of Year Batch Resubmit Request processing" ) ]
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
                      { nameLocalName = "enumeration" 
                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                      , namePrefix = Just "xsd" 
                      } 
                    , elementAttributes = 
                      [ 
                       ( Name 
                         { nameLocalName = "value" 
                         , nameNamespace = Nothing
                         , namePrefix = Nothing
                         } 
                       , [ ContentText "IRS_EOY_RESUBMIT_MISSING_FILE_REQ" ]
                       ) 
                      ]
                    , elementNodes = 
                      [ NodeContent ( ContentText "" )
                      , NodeElement 
                        ( Element 
                          { elementName = Name 
                            { nameLocalName = "annotation" 
                            , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                            , namePrefix = Just "xsd" 
                            } 
                          , elementAttributes = []
                          , elementNodes = 
                            [ NodeContent ( ContentText "" )
                            , NodeElement 
                              ( Element 
                                { elementName = Name 
                                  { nameLocalName = "documentation" 
                                  , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                  , namePrefix = Just "xsd" 
                                  } 
                                , elementAttributes = []
                                , elementNodes = 
                                  [ NodeContent ( ContentText "Identifies End of Year Missing File Resubmit Request processing" ) ]
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
          , namePrefix = Just "xsd" 
          } 
        , elementAttributes = 
          [ 
           ( Name 
             { nameLocalName = "name" 
             , nameNamespace = Nothing
             , namePrefix = Nothing
             } 
           , [ ContentText "BinaryFormatCodeType" ]
           ) 
          ]
        , elementNodes = 
          [ NodeContent ( ContentText "" )
          , NodeElement 
            ( Element 
              { elementName = Name 
                { nameLocalName = "annotation" 
                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                , namePrefix = Just "xsd" 
                } 
              , elementAttributes = []
              , elementNodes = 
                [ NodeContent ( ContentText "" )
                , NodeElement 
                  ( Element 
                    { elementName = Name 
                      { nameLocalName = "documentation" 
                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                      , namePrefix = Just "xsd" 
                      } 
                    , elementAttributes = []
                    , elementNodes = 
                      [ NodeContent ( ContentText "" )
                      , NodeElement 
                        ( Element 
                          { elementName = Name 
                            { nameLocalName = "Component" 
                            , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                            , namePrefix = Nothing
                            } 
                          , elementAttributes = []
                          , elementNodes = 
                            [ NodeContent ( ContentText "" )
                            , NodeElement 
                              ( Element 
                                { elementName = Name 
                                  { nameLocalName = "DictionaryEntryNm" 
                                  , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                  , namePrefix = Nothing
                                  } 
                                , elementAttributes = []
                                , elementNodes = 
                                  [ NodeContent ( ContentText "Binary Format Code Type" ) ]
                                } 
                              )
                            , NodeContent ( ContentText "" )
                            , NodeElement 
                              ( Element 
                                { elementName = Name 
                                  { nameLocalName = "MajorVersionNum" 
                                  , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                  , namePrefix = Nothing
                                  } 
                                , elementAttributes = []
                                , elementNodes = 
                                  [ NodeContent ( ContentText "1" ) ]
                                } 
                              )
                            , NodeContent ( ContentText "" )
                            , NodeElement 
                              ( Element 
                                { elementName = Name 
                                  { nameLocalName = "MinorVersionNum" 
                                  , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                  , namePrefix = Nothing
                                  } 
                                , elementAttributes = []
                                , elementNodes = 
                                  [ NodeContent ( ContentText "0" ) ]
                                } 
                              )
                            , NodeContent ( ContentText "" )
                            , NodeElement 
                              ( Element 
                                { elementName = Name 
                                  { nameLocalName = "VersionEffectiveBeginDt" 
                                  , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                  , namePrefix = Nothing
                                  } 
                                , elementAttributes = []
                                , elementNodes = 
                                  [ NodeContent ( ContentText "2012-09-05" ) ]
                                } 
                              )
                            , NodeContent ( ContentText "" )
                            , NodeElement 
                              ( Element 
                                { elementName = Name 
                                  { nameLocalName = "VersionDescriptionTxt" 
                                  , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                  , namePrefix = Nothing
                                  } 
                                , elementAttributes = []
                                , elementNodes = 
                                  [ NodeContent ( ContentText "Initial version" ) ]
                                } 
                              )
                            , NodeContent ( ContentText "" )
                            , NodeElement 
                              ( Element 
                                { elementName = Name 
                                  { nameLocalName = "DescriptionTxt" 
                                  , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                  , namePrefix = Nothing
                                  } 
                                , elementAttributes = []
                                , elementNodes = 
                                  [ NodeContent ( ContentText "A code for the file format of content type of a binary object. Extensible Markup Language (XML) is the only accepted type at this time." ) ]
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
                { nameLocalName = "restriction" 
                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                , namePrefix = Just "xsd" 
                } 
              , elementAttributes = 
                [ 
                 ( Name 
                   { nameLocalName = "base" 
                   , nameNamespace = Nothing
                   , namePrefix = Nothing
                   } 
                 , [ ContentText "xsd:string" ]
                 ) 
                ]
              , elementNodes = 
                [ NodeContent ( ContentText "" )
                , NodeElement 
                  ( Element 
                    { elementName = Name 
                      { nameLocalName = "enumeration" 
                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                      , namePrefix = Just "xsd" 
                      } 
                    , elementAttributes = 
                      [ 
                       ( Name 
                         { nameLocalName = "value" 
                         , nameNamespace = Nothing
                         , namePrefix = Nothing
                         } 
                       , [ ContentText "application/xml" ]
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
          , namePrefix = Just "xsd" 
          } 
        , elementAttributes = 
          [ 
           ( Name 
             { nameLocalName = "name" 
             , nameNamespace = Nothing
             , namePrefix = Nothing
             } 
           , [ ContentText "BooleanStringType" ]
           ) 
          ]
        , elementNodes = 
          [ NodeContent ( ContentText "" )
          , NodeElement 
            ( Element 
              { elementName = Name 
                { nameLocalName = "annotation" 
                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                , namePrefix = Just "xsd" 
                } 
              , elementAttributes = []
              , elementNodes = 
                [ NodeContent ( ContentText "" )
                , NodeElement 
                  ( Element 
                    { elementName = Name 
                      { nameLocalName = "documentation" 
                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                      , namePrefix = Just "xsd" 
                      } 
                    , elementAttributes = []
                    , elementNodes = 
                      [ NodeContent ( ContentText "" )
                      , NodeElement 
                        ( Element 
                          { elementName = Name 
                            { nameLocalName = "Component" 
                            , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                            , namePrefix = Nothing
                            } 
                          , elementAttributes = []
                          , elementNodes = 
                            [ NodeContent ( ContentText "" )
                            , NodeElement 
                              ( Element 
                                { elementName = Name 
                                  { nameLocalName = "DictionaryEntryNm" 
                                  , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                  , namePrefix = Nothing
                                  } 
                                , elementAttributes = []
                                , elementNodes = 
                                  [ NodeContent ( ContentText "Boolean String Type" ) ]
                                } 
                              )
                            , NodeContent ( ContentText "" )
                            , NodeElement 
                              ( Element 
                                { elementName = Name 
                                  { nameLocalName = "MajorVersionNum" 
                                  , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                  , namePrefix = Nothing
                                  } 
                                , elementAttributes = []
                                , elementNodes = 
                                  [ NodeContent ( ContentText "1" ) ]
                                } 
                              )
                            , NodeContent ( ContentText "" )
                            , NodeElement 
                              ( Element 
                                { elementName = Name 
                                  { nameLocalName = "MinorVersionNum" 
                                  , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                  , namePrefix = Nothing
                                  } 
                                , elementAttributes = []
                                , elementNodes = 
                                  [ NodeContent ( ContentText "0" ) ]
                                } 
                              )
                            , NodeContent ( ContentText "" )
                            , NodeElement 
                              ( Element 
                                { elementName = Name 
                                  { nameLocalName = "VersionEffectiveBeginDt" 
                                  , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                  , namePrefix = Nothing
                                  } 
                                , elementAttributes = []
                                , elementNodes = 
                                  [ NodeContent ( ContentText "2012-03-141" ) ]
                                } 
                              )
                            , NodeContent ( ContentText "" )
                            , NodeElement 
                              ( Element 
                                { elementName = Name 
                                  { nameLocalName = "VersionDescriptionTxt" 
                                  , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                  , namePrefix = Nothing
                                  } 
                                , elementAttributes = []
                                , elementNodes = 
                                  [ NodeContent ( ContentText "Initial version" ) ]
                                } 
                              )
                            , NodeContent ( ContentText "" )
                            , NodeElement 
                              ( Element 
                                { elementName = Name 
                                  { nameLocalName = "DescriptionTxt" 
                                  , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                  , namePrefix = Nothing
                                  } 
                                , elementAttributes = []
                                , elementNodes = 
                                  [ NodeContent ( ContentText "The code options related to a boolean string" ) ]
                                } 
                              )
                            , NodeContent ( ContentText "" )
                            , NodeElement 
                              ( Element 
                                { elementName = Name 
                                  { nameLocalName = "DataElementId" 
                                  , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                  , namePrefix = Nothing
                                  } 
                                , elementAttributes = []
                                , elementNodes = 
                                  [ NodeContent ( ContentText "" ) ]
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
                { nameLocalName = "restriction" 
                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                , namePrefix = Just "xsd" 
                } 
              , elementAttributes = 
                [ 
                 ( Name 
                   { nameLocalName = "base" 
                   , nameNamespace = Nothing
                   , namePrefix = Nothing
                   } 
                 , [ ContentText "xsd:string" ]
                 ) 
                ]
              , elementNodes = 
                [ NodeContent ( ContentText "" )
                , NodeElement 
                  ( Element 
                    { elementName = Name 
                      { nameLocalName = "enumeration" 
                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                      , namePrefix = Just "xsd" 
                      } 
                    , elementAttributes = 
                      [ 
                       ( Name 
                         { nameLocalName = "value" 
                         , nameNamespace = Nothing
                         , namePrefix = Nothing
                         } 
                       , [ ContentText "Y" ]
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
                      , namePrefix = Just "xsd" 
                      } 
                    , elementAttributes = 
                      [ 
                       ( Name 
                         { nameLocalName = "value" 
                         , nameNamespace = Nothing
                         , namePrefix = Nothing
                         } 
                       , [ ContentText "N" ]
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
          , namePrefix = Just "xsd" 
          } 
        , elementAttributes = 
          [ 
           ( Name 
             { nameLocalName = "name" 
             , nameNamespace = Nothing
             , namePrefix = Nothing
             } 
           , [ ContentText "BooleanType" ]
           ) 
          ]
        , elementNodes = 
          [ NodeContent ( ContentText "" )
          , NodeElement 
            ( Element 
              { elementName = Name 
                { nameLocalName = "annotation" 
                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                , namePrefix = Just "xsd" 
                } 
              , elementAttributes = []
              , elementNodes = 
                [ NodeContent ( ContentText "" )
                , NodeElement 
                  ( Element 
                    { elementName = Name 
                      { nameLocalName = "documentation" 
                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                      , namePrefix = Just "xsd" 
                      } 
                    , elementAttributes = []
                    , elementNodes = 
                      [ NodeContent ( ContentText "" )
                      , NodeElement 
                        ( Element 
                          { elementName = Name 
                            { nameLocalName = "Component" 
                            , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                            , namePrefix = Nothing
                            } 
                          , elementAttributes = []
                          , elementNodes = 
                            [ NodeContent ( ContentText "" )
                            , NodeElement 
                              ( Element 
                                { elementName = Name 
                                  { nameLocalName = "DictionaryEntryNm" 
                                  , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                  , namePrefix = Nothing
                                  } 
                                , elementAttributes = []
                                , elementNodes = 
                                  [ NodeContent ( ContentText "Boolean Type" ) ]
                                } 
                              )
                            , NodeContent ( ContentText "" )
                            , NodeElement 
                              ( Element 
                                { elementName = Name 
                                  { nameLocalName = "MajorVersionNum" 
                                  , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                  , namePrefix = Nothing
                                  } 
                                , elementAttributes = []
                                , elementNodes = 
                                  [ NodeContent ( ContentText "2" ) ]
                                } 
                              )
                            , NodeContent ( ContentText "" )
                            , NodeElement 
                              ( Element 
                                { elementName = Name 
                                  { nameLocalName = "MinorVersionNum" 
                                  , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                  , namePrefix = Nothing
                                  } 
                                , elementAttributes = []
                                , elementNodes = 
                                  [ NodeContent ( ContentText "0" ) ]
                                } 
                              )
                            , NodeContent ( ContentText "" )
                            , NodeElement 
                              ( Element 
                                { elementName = Name 
                                  { nameLocalName = "VersionEffectiveBeginDt" 
                                  , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                  , namePrefix = Nothing
                                  } 
                                , elementAttributes = []
                                , elementNodes = 
                                  [ NodeContent ( ContentText "2008-01-08" ) ]
                                } 
                              )
                            , NodeContent ( ContentText "" )
                            , NodeElement 
                              ( Element 
                                { elementName = Name 
                                  { nameLocalName = "VersionDescriptionTxt" 
                                  , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                  , namePrefix = Nothing
                                  } 
                                , elementAttributes = []
                                , elementNodes = 
                                  [ NodeContent ( ContentText "Added defaults of 0/1 EFileTypes, December 14, 2007" ) ]
                                } 
                              )
                            , NodeContent ( ContentText "" )
                            , NodeElement 
                              ( Element 
                                { elementName = Name 
                                  { nameLocalName = "DescriptionTxt" 
                                  , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                  , namePrefix = Nothing
                                  } 
                                , elementAttributes = []
                                , elementNodes = 
                                  [ NodeContent ( ContentText "Base type for a boolean. 0= False, 1=True" ) ]
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
                { nameLocalName = "restriction" 
                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                , namePrefix = Just "xsd" 
                } 
              , elementAttributes = 
                [ 
                 ( Name 
                   { nameLocalName = "base" 
                   , nameNamespace = Nothing
                   , namePrefix = Nothing
                   } 
                 , [ ContentText "xsd:boolean" ]
                 ) 
                ]
              , elementNodes = 
                [ NodeContent ( ContentText "" )
                , NodeElement 
                  ( Element 
                    { elementName = Name 
                      { nameLocalName = "pattern" 
                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                      , namePrefix = Just "xsd" 
                      } 
                    , elementAttributes = 
                      [ 
                       ( Name 
                         { nameLocalName = "value" 
                         , nameNamespace = Nothing
                         , namePrefix = Nothing
                         } 
                       , [ ContentText "[01]" ]
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
          , namePrefix = Just "xsd" 
          } 
        , elementAttributes = 
          [ 
           ( Name 
             { nameLocalName = "name" 
             , nameNamespace = Nothing
             , namePrefix = Nothing
             } 
           , [ ContentText "BRMGatewayStatusCodeType" ]
           ) 
          ]
        , elementNodes = 
          [ NodeContent ( ContentText "" )
          , NodeElement 
            ( Element 
              { elementName = Name 
                { nameLocalName = "annotation" 
                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                , namePrefix = Just "xsd" 
                } 
              , elementAttributes = []
              , elementNodes = 
                [ NodeContent ( ContentText "" )
                , NodeElement 
                  ( Element 
                    { elementName = Name 
                      { nameLocalName = "documentation" 
                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                      , namePrefix = Just "xsd" 
                      } 
                    , elementAttributes = []
                    , elementNodes = 
                      [ NodeContent ( ContentText "" )
                      , NodeElement 
                        ( Element 
                          { elementName = Name 
                            { nameLocalName = "Component" 
                            , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                            , namePrefix = Nothing
                            } 
                          , elementAttributes = []
                          , elementNodes = 
                            [ NodeContent ( ContentText "" )
                            , NodeElement 
                              ( Element 
                                { elementName = Name 
                                  { nameLocalName = "DictionaryEntryNm" 
                                  , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                  , namePrefix = Nothing
                                  } 
                                , elementAttributes = []
                                , elementNodes = 
                                  [ NodeContent ( ContentText "BRM Gateway Status Code Type" ) ]
                                } 
                              )
                            , NodeContent ( ContentText "" )
                            , NodeElement 
                              ( Element 
                                { elementName = Name 
                                  { nameLocalName = "MajorVersionNum" 
                                  , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                  , namePrefix = Nothing
                                  } 
                                , elementAttributes = []
                                , elementNodes = 
                                  [ NodeContent ( ContentText "1" ) ]
                                } 
                              )
                            , NodeContent ( ContentText "" )
                            , NodeElement 
                              ( Element 
                                { elementName = Name 
                                  { nameLocalName = "MinorVersionNum" 
                                  , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                  , namePrefix = Nothing
                                  } 
                                , elementAttributes = []
                                , elementNodes = 
                                  [ NodeContent ( ContentText "0" ) ]
                                } 
                              )
                            , NodeContent ( ContentText "" )
                            , NodeElement 
                              ( Element 
                                { elementName = Name 
                                  { nameLocalName = "VersionEffectiveBeginDt" 
                                  , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                  , namePrefix = Nothing
                                  } 
                                , elementAttributes = []
                                , elementNodes = 
                                  [ NodeContent ( ContentText "2012-09-05" ) ]
                                } 
                              )
                            , NodeContent ( ContentText "" )
                            , NodeElement 
                              ( Element 
                                { elementName = Name 
                                  { nameLocalName = "VersionDescriptionTxt" 
                                  , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                  , namePrefix = Nothing
                                  } 
                                , elementAttributes = []
                                , elementNodes = 
                                  [ NodeContent ( ContentText "Initial version" ) ]
                                } 
                              )
                            , NodeContent ( ContentText "" )
                            , NodeElement 
                              ( Element 
                                { elementName = Name 
                                  { nameLocalName = "DescriptionTxt" 
                                  , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                  , namePrefix = Nothing
                                  } 
                                , elementAttributes = []
                                , elementNodes = 
                                  [ NodeContent ( ContentText "This is a code used to indicate the status of the BRM Gateway." ) ]
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
                { nameLocalName = "restriction" 
                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                , namePrefix = Just "xsd" 
                } 
              , elementAttributes = 
                [ 
                 ( Name 
                   { nameLocalName = "base" 
                   , nameNamespace = Nothing
                   , namePrefix = Nothing
                   } 
                 , [ ContentText "xsd:string" ]
                 ) 
                ]
              , elementNodes = 
                [ NodeContent ( ContentText "" )
                , NodeElement 
                  ( Element 
                    { elementName = Name 
                      { nameLocalName = "enumeration" 
                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                      , namePrefix = Just "xsd" 
                      } 
                    , elementAttributes = 
                      [ 
                       ( Name 
                         { nameLocalName = "value" 
                         , nameNamespace = Nothing
                         , namePrefix = Nothing
                         } 
                       , [ ContentText "NEW" ]
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
                      , namePrefix = Just "xsd" 
                      } 
                    , elementAttributes = 
                      [ 
                       ( Name 
                         { nameLocalName = "value" 
                         , nameNamespace = Nothing
                         , namePrefix = Nothing
                         } 
                       , [ ContentText "INPROCESS" ]
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
                      , namePrefix = Just "xsd" 
                      } 
                    , elementAttributes = 
                      [ 
                       ( Name 
                         { nameLocalName = "value" 
                         , nameNamespace = Nothing
                         , namePrefix = Nothing
                         } 
                       , [ ContentText "CHUNKED" ]
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
                      , namePrefix = Just "xsd" 
                      } 
                    , elementAttributes = 
                      [ 
                       ( Name 
                         { nameLocalName = "value" 
                         , nameNamespace = Nothing
                         , namePrefix = Nothing
                         } 
                       , [ ContentText "ERROR" ]
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
                      , namePrefix = Just "xsd" 
                      } 
                    , elementAttributes = 
                      [ 
                       ( Name 
                         { nameLocalName = "value" 
                         , nameNamespace = Nothing
                         , namePrefix = Nothing
                         } 
                       , [ ContentText "CHUNKS RECEIVED" ]
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
                      , namePrefix = Just "xsd" 
                      } 
                    , elementAttributes = 
                      [ 
                       ( Name 
                         { nameLocalName = "value" 
                         , nameNamespace = Nothing
                         , namePrefix = Nothing
                         } 
                       , [ ContentText "RESPONSE FILE CONSOLIDATED" ]
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
                      , namePrefix = Just "xsd" 
                      } 
                    , elementAttributes = 
                      [ 
                       ( Name 
                         { nameLocalName = "value" 
                         , nameNamespace = Nothing
                         , namePrefix = Nothing
                         } 
                       , [ ContentText "BRM GATEWAY NOTIFIED" ]
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
                      , namePrefix = Just "xsd" 
                      } 
                    , elementAttributes = 
                      [ 
                       ( Name 
                         { nameLocalName = "value" 
                         , nameNamespace = Nothing
                         , namePrefix = Nothing
                         } 
                       , [ ContentText "RESPONSE SUBMITTED TO CMS" ]
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
                      , namePrefix = Just "xsd" 
                      } 
                    , elementAttributes = 
                      [ 
                       ( Name 
                         { nameLocalName = "value" 
                         , nameNamespace = Nothing
                         , namePrefix = Nothing
                         } 
                       , [ ContentText "COMPLETED" ]
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
          , namePrefix = Just "xsd" 
          } 
        , elementAttributes = 
          [ 
           ( Name 
             { nameLocalName = "name" 
             , nameNamespace = Nothing
             , namePrefix = Nothing
             } 
           , [ ContentText "BusinessNameControlType" ]
           ) 
          ]
        , elementNodes = 
          [ NodeContent ( ContentText "" )
          , NodeElement 
            ( Element 
              { elementName = Name 
                { nameLocalName = "annotation" 
                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                , namePrefix = Just "xsd" 
                } 
              , elementAttributes = []
              , elementNodes = 
                [ NodeContent ( ContentText "" )
                , NodeElement 
                  ( Element 
                    { elementName = Name 
                      { nameLocalName = "documentation" 
                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                      , namePrefix = Just "xsd" 
                      } 
                    , elementAttributes = []
                    , elementNodes = 
                      [ NodeContent ( ContentText "" )
                      , NodeElement 
                        ( Element 
                          { elementName = Name 
                            { nameLocalName = "Component" 
                            , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                            , namePrefix = Nothing
                            } 
                          , elementAttributes = []
                          , elementNodes = 
                            [ NodeContent ( ContentText "" )
                            , NodeElement 
                              ( Element 
                                { elementName = Name 
                                  { nameLocalName = "DictionaryEntryNm" 
                                  , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                  , namePrefix = Nothing
                                  } 
                                , elementAttributes = []
                                , elementNodes = 
                                  [ NodeContent ( ContentText "Business Name Control Type" ) ]
                                } 
                              )
                            , NodeContent ( ContentText "" )
                            , NodeElement 
                              ( Element 
                                { elementName = Name 
                                  { nameLocalName = "MajorVersionNum" 
                                  , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                  , namePrefix = Nothing
                                  } 
                                , elementAttributes = []
                                , elementNodes = 
                                  [ NodeContent ( ContentText "1" ) ]
                                } 
                              )
                            , NodeContent ( ContentText "" )
                            , NodeElement 
                              ( Element 
                                { elementName = Name 
                                  { nameLocalName = "MinorVersionNum" 
                                  , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                  , namePrefix = Nothing
                                  } 
                                , elementAttributes = []
                                , elementNodes = 
                                  [ NodeContent ( ContentText "0" ) ]
                                } 
                              )
                            , NodeContent ( ContentText "" )
                            , NodeElement 
                              ( Element 
                                { elementName = Name 
                                  { nameLocalName = "VersionEffectiveBeginDt" 
                                  , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                  , namePrefix = Nothing
                                  } 
                                , elementAttributes = []
                                , elementNodes = 
                                  [ NodeContent ( ContentText "2008-01-08" ) ]
                                } 
                              )
                            , NodeContent ( ContentText "" )
                            , NodeElement 
                              ( Element 
                                { elementName = Name 
                                  { nameLocalName = "VersionDescriptionTxt" 
                                  , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                  , namePrefix = Nothing
                                  } 
                                , elementAttributes = []
                                , elementNodes = 
                                  [ NodeContent ( ContentText "EFileTypes, December 14, 2007" ) ]
                                } 
                              )
                            , NodeContent ( ContentText "" )
                            , NodeElement 
                              ( Element 
                                { elementName = Name 
                                  { nameLocalName = "DescriptionTxt" 
                                  , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                  , namePrefix = Nothing
                                  } 
                                , elementAttributes = []
                                , elementNodes = 
                                  [ NodeContent ( ContentText "Used for a Name Control. Legal Characters: A-Z,
                                                  0-9, hyphen and ampersand. Illegal Character: spaces
                                                  and symbols." ) ]
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
                    { nameLocalName = "restriction" 
                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                    , namePrefix = Just "xsd" 
                    } 
                  , elementAttributes = 
                    [ 
                     ( Name 
                       { nameLocalName = "base" 
                       , nameNamespace = Nothing
                       , namePrefix = Nothing
                       } 
                     , [ ContentText "xsd:string" ]
                     ) 
                    ]
                  , elementNodes = 
                    [ NodeContent ( ContentText "" )
                    , NodeElement 
                      ( Element 
                        { elementName = Name 
                          { nameLocalName = "pattern" 
                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                          , namePrefix = Just "xsd" 
                          } 
                        , elementAttributes = 
                          [ 
                           ( Name 
                             { nameLocalName = "value" 
                             , nameNamespace = Nothing
                             , namePrefix = Nothing
                             } 
                           , 
                             [ ContentText "([A-Z0-9\-]|" 
                             , ContentText "&" 
                             , ContentText "){1,4}" 
                             ] 
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
              , namePrefix = Just "xsd" 
              } 
            , elementAttributes = 
              [ 
               ( Name 
                 { nameLocalName = "name" 
                 , nameNamespace = Nothing
                 , namePrefix = Nothing
                 } 
               , [ ContentText "BusinessNameLine1Type" ]
               ) 
              ]
            , elementNodes = 
              [ NodeContent ( ContentText "" )
              , NodeElement 
                ( Element 
                  { elementName = Name 
                    { nameLocalName = "annotation" 
                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                    , namePrefix = Just "xsd" 
                    } 
                  , elementAttributes = []
                  , elementNodes = 
                    [ NodeContent ( ContentText "" )
                    , NodeElement 
                      ( Element 
                        { elementName = Name 
                          { nameLocalName = "documentation" 
                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                          , namePrefix = Just "xsd" 
                          } 
                        , elementAttributes = []
                        , elementNodes = 
                          [ NodeContent ( ContentText "" )
                          , NodeElement 
                            ( Element 
                              { elementName = Name 
                                { nameLocalName = "Component" 
                                , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                , namePrefix = Nothing
                                } 
                              , elementAttributes = []
                              , elementNodes = 
                                [ NodeContent ( ContentText "" )
                                , NodeElement 
                                  ( Element 
                                    { elementName = Name 
                                      { nameLocalName = "DictionaryEntryNm" 
                                      , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                      , namePrefix = Nothing
                                      } 
                                    , elementAttributes = []
                                    , elementNodes = 
                                      [ NodeContent ( ContentText "Business Name Line 1 Type" ) ]
                                    } 
                                  )
                                , NodeContent ( ContentText "" )
                                , NodeElement 
                                  ( Element 
                                    { elementName = Name 
                                      { nameLocalName = "MajorVersionNum" 
                                      , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                      , namePrefix = Nothing
                                      } 
                                    , elementAttributes = []
                                    , elementNodes = 
                                      [ NodeContent ( ContentText "1" ) ]
                                    } 
                                  )
                                , NodeContent ( ContentText "" )
                                , NodeElement 
                                  ( Element 
                                    { elementName = Name 
                                      { nameLocalName = "MinorVersionNum" 
                                      , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                      , namePrefix = Nothing
                                      } 
                                    , elementAttributes = []
                                    , elementNodes = 
                                      [ NodeContent ( ContentText "0" ) ]
                                    } 
                                  )
                                , NodeContent ( ContentText "" )
                                , NodeElement 
                                  ( Element 
                                    { elementName = Name 
                                      { nameLocalName = "VersionEffectiveBeginDt" 
                                      , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                      , namePrefix = Nothing
                                      } 
                                    , elementAttributes = []
                                    , elementNodes = 
                                      [ NodeContent ( ContentText "2008-01-08" ) ]
                                    } 
                                  )
                                , NodeContent ( ContentText "" )
                                , NodeElement 
                                  ( Element 
                                    { elementName = Name 
                                      { nameLocalName = "VersionDescriptionTxt" 
                                      , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                      , namePrefix = Nothing
                                      } 
                                    , elementAttributes = []
                                    , elementNodes = 
                                      [ NodeContent ( ContentText "EFileTypes, December 14, 2007" ) ]
                                    } 
                                  )
                                , NodeContent ( ContentText "" )
                                , NodeElement 
                                  ( Element 
                                    { elementName = Name 
                                      { nameLocalName = "DescriptionTxt" 
                                      , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                      , namePrefix = Nothing
                                      } 
                                    , elementAttributes = []
                                    , elementNodes = 
                                      [ NodeContent ( ContentText "Typically used for a business name. Legal Characters: A-Z, a-z, 0-9, hash, hyphen, parentheses, ampersand, apostrophe and single space. Illegal Character: leading space, trailing space, adjacent spaces, and other symbols" ) ]
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
                    { nameLocalName = "restriction" 
                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                    , namePrefix = Just "xsd" 
                    } 
                  , elementAttributes = 
                    [ 
                     ( Name 
                       { nameLocalName = "base" 
                       , nameNamespace = Nothing
                       , namePrefix = Nothing
                       } 
                     , [ ContentText "xsd:string" ]
                     ) 
                    ]
                  , elementNodes = 
                    [ NodeContent ( ContentText "" )
                    , NodeElement 
                      ( Element 
                        { elementName = Name 
                          { nameLocalName = "maxLength" 
                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                          , namePrefix = Just "xsd" 
                          } 
                        , elementAttributes = 
                          [ 
                           ( Name 
                             { nameLocalName = "value" 
                             , nameNamespace = Nothing
                             , namePrefix = Nothing
                             } 
                           , [ ContentText "75" ]
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
                          , namePrefix = Just "xsd" 
                          } 
                        , elementAttributes = 
                          [ 
                           ( Name 
                             { nameLocalName = "value" 
                             , nameNamespace = Nothing
                             , namePrefix = Nothing
                             } 
                           , 
                             [ ContentText "(([A-Za-z0-9#\-\(\)]|" 
                             , ContentText "&" 
                             , ContentText "|" 
                             , ContentText "'" 
                             , ContentText ") ?)*([A-Za-z0-9#\-\(\)]|" 
                             , ContentText "&" 
                             , ContentText "|" 
                             , ContentText "'" 
                             , ContentText ")" 
                             ] 
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
              , namePrefix = Just "xsd" 
              } 
            , elementAttributes = 
              [ 
               ( Name 
                 { nameLocalName = "name" 
                 , nameNamespace = Nothing
                 , namePrefix = Nothing
                 } 
               , [ ContentText "BusinessNameLine2Type" ]
               ) 
              ]
            , elementNodes = 
              [ NodeContent ( ContentText "" )
              , NodeElement 
                ( Element 
                  { elementName = Name 
                    { nameLocalName = "annotation" 
                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                    , namePrefix = Just "xsd" 
                    } 
                  , elementAttributes = []
                  , elementNodes = 
                    [ NodeContent ( ContentText "" )
                    , NodeElement 
                      ( Element 
                        { elementName = Name 
                          { nameLocalName = "documentation" 
                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                          , namePrefix = Just "xsd" 
                          } 
                        , elementAttributes = []
                        , elementNodes = 
                          [ NodeContent ( ContentText "" )
                          , NodeElement 
                            ( Element 
                              { elementName = Name 
                                { nameLocalName = "Component" 
                                , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                , namePrefix = Nothing
                                } 
                              , elementAttributes = []
                              , elementNodes = 
                                [ NodeContent ( ContentText "" )
                                , NodeElement 
                                  ( Element 
                                    { elementName = Name 
                                      { nameLocalName = "DictionaryEntryNm" 
                                      , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                      , namePrefix = Nothing
                                      } 
                                    , elementAttributes = []
                                    , elementNodes = 
                                      [ NodeContent ( ContentText "Business Name Line 2 Type" ) ]
                                    } 
                                  )
                                , NodeContent ( ContentText "" )
                                , NodeElement 
                                  ( Element 
                                    { elementName = Name 
                                      { nameLocalName = "MajorVersionNum" 
                                      , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                      , namePrefix = Nothing
                                      } 
                                    , elementAttributes = []
                                    , elementNodes = 
                                      [ NodeContent ( ContentText "1" ) ]
                                    } 
                                  )
                                , NodeContent ( ContentText "" )
                                , NodeElement 
                                  ( Element 
                                    { elementName = Name 
                                      { nameLocalName = "MinorVersionNum" 
                                      , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                      , namePrefix = Nothing
                                      } 
                                    , elementAttributes = []
                                    , elementNodes = 
                                      [ NodeContent ( ContentText "0" ) ]
                                    } 
                                  )
                                , NodeContent ( ContentText "" )
                                , NodeElement 
                                  ( Element 
                                    { elementName = Name 
                                      { nameLocalName = "VersionEffectiveBeginDt" 
                                      , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                      , namePrefix = Nothing
                                      } 
                                    , elementAttributes = []
                                    , elementNodes = 
                                      [ NodeContent ( ContentText "2008-01-08" ) ]
                                    } 
                                  )
                                , NodeContent ( ContentText "" )
                                , NodeElement 
                                  ( Element 
                                    { elementName = Name 
                                      { nameLocalName = "VersionDescriptionTxt" 
                                      , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                      , namePrefix = Nothing
                                      } 
                                    , elementAttributes = []
                                    , elementNodes = 
                                      [ NodeContent ( ContentText "EFileTypes, December 14, 2007" ) ]
                                    } 
                                  )
                                , NodeContent ( ContentText "" )
                                , NodeElement 
                                  ( Element 
                                    { elementName = Name 
                                      { nameLocalName = "DescriptionTxt" 
                                      , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                      , namePrefix = Nothing
                                      } 
                                    , elementAttributes = []
                                    , elementNodes = 
                                      [ NodeContent ( ContentText "Typically used for line 2 of a business name. Legal Characters: A-Z, a-z, 0-9, hash, slash, percent, hyphen, parentheses, ampersand, apostrophe and single space. Illegal Character: leading space, trailing space, adjacent spaces, and other symbols." ) ]
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
                    { nameLocalName = "restriction" 
                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                    , namePrefix = Just "xsd" 
                    } 
                  , elementAttributes = 
                    [ 
                     ( Name 
                       { nameLocalName = "base" 
                       , nameNamespace = Nothing
                       , namePrefix = Nothing
                       } 
                     , [ ContentText "xsd:string" ]
                     ) 
                    ]
                  , elementNodes = 
                    [ NodeContent ( ContentText "" )
                    , NodeElement 
                      ( Element 
                        { elementName = Name 
                          { nameLocalName = "maxLength" 
                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                          , namePrefix = Just "xsd" 
                          } 
                        , elementAttributes = 
                          [ 
                           ( Name 
                             { nameLocalName = "value" 
                             , nameNamespace = Nothing
                             , namePrefix = Nothing
                             } 
                           , [ ContentText "75" ]
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
                          , namePrefix = Just "xsd" 
                          } 
                        , elementAttributes = 
                          [ 
                           ( Name 
                             { nameLocalName = "value" 
                             , nameNamespace = Nothing
                             , namePrefix = Nothing
                             } 
                           , 
                             [ ContentText "(([A-Za-z0-9#/%\-\(\)]|" 
                             , ContentText "&" 
                             , ContentText "|" 
                             , ContentText "'" 
                             , ContentText ") ?)*([A-Za-z0-9#/%\-\(\)]|" 
                             , ContentText "&" 
                             , ContentText "|" 
                             , ContentText "'" 
                             , ContentText ")" 
                             ] 
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
              , namePrefix = Just "xsd" 
              } 
            , elementAttributes = 
              [ 
               ( Name 
                 { nameLocalName = "name" 
                 , nameNamespace = Nothing
                 , namePrefix = Nothing
                 } 
               , [ ContentText "CheckboxType" ]
               ) 
              ]
            , elementNodes = 
              [ NodeContent ( ContentText "" )
              , NodeElement 
                ( Element 
                  { elementName = Name 
                    { nameLocalName = "annotation" 
                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                    , namePrefix = Just "xsd" 
                    } 
                  , elementAttributes = []
                  , elementNodes = 
                    [ NodeContent ( ContentText "" )
                    , NodeElement 
                      ( Element 
                        { elementName = Name 
                          { nameLocalName = "documentation" 
                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                          , namePrefix = Just "xsd" 
                          } 
                        , elementAttributes = []
                        , elementNodes = 
                          [ NodeContent ( ContentText "" )
                          , NodeElement 
                            ( Element 
                              { elementName = Name 
                                { nameLocalName = "Component" 
                                , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                , namePrefix = Nothing
                                } 
                              , elementAttributes = []
                              , elementNodes = 
                                [ NodeContent ( ContentText "" )
                                , NodeElement 
                                  ( Element 
                                    { elementName = Name 
                                      { nameLocalName = "DictionaryEntryNm" 
                                      , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                      , namePrefix = Nothing
                                      } 
                                    , elementAttributes = []
                                    , elementNodes = 
                                      [ NodeContent ( ContentText "Checkbox Type" ) ]
                                    } 
                                  )
                                , NodeContent ( ContentText "" )
                                , NodeElement 
                                  ( Element 
                                    { elementName = Name 
                                      { nameLocalName = "MajorVersionNum" 
                                      , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                      , namePrefix = Nothing
                                      } 
                                    , elementAttributes = []
                                    , elementNodes = 
                                      [ NodeContent ( ContentText "1" ) ]
                                    } 
                                  )
                                , NodeContent ( ContentText "" )
                                , NodeElement 
                                  ( Element 
                                    { elementName = Name 
                                      { nameLocalName = "MinorVersionNum" 
                                      , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                      , namePrefix = Nothing
                                      } 
                                    , elementAttributes = []
                                    , elementNodes = 
                                      [ NodeContent ( ContentText "0" ) ]
                                    } 
                                  )
                                , NodeContent ( ContentText "" )
                                , NodeElement 
                                  ( Element 
                                    { elementName = Name 
                                      { nameLocalName = "VersionEffectiveBeginDt" 
                                      , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                      , namePrefix = Nothing
                                      } 
                                    , elementAttributes = []
                                    , elementNodes = 
                                      [ NodeContent ( ContentText "2008-01-08" ) ]
                                    } 
                                  )
                                , NodeContent ( ContentText "" )
                                , NodeElement 
                                  ( Element 
                                    { elementName = Name 
                                      { nameLocalName = "VersionDescriptionTxt" 
                                      , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                      , namePrefix = Nothing
                                      } 
                                    , elementAttributes = []
                                    , elementNodes = 
                                      [ NodeContent ( ContentText "EFileTypes, December 14, 2007" ) ]
                                    } 
                                  )
                                , NodeContent ( ContentText "" )
                                , NodeElement 
                                  ( Element 
                                    { elementName = Name 
                                      { nameLocalName = "DescriptionTxt" 
                                      , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                      , namePrefix = Nothing
                                      } 
                                    , elementAttributes = []
                                    , elementNodes = 
                                      [ NodeContent ( ContentText "Typically used by an optional checkbox." ) ]
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
                    { nameLocalName = "restriction" 
                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                    , namePrefix = Just "xsd" 
                    } 
                  , elementAttributes = 
                    [ 
                     ( Name 
                       { nameLocalName = "base" 
                       , nameNamespace = Nothing
                       , namePrefix = Nothing
                       } 
                     , [ ContentText "xsd:string" ]
                     ) 
                    ]
                  , elementNodes = 
                    [ NodeContent ( ContentText "" )
                    , NodeElement 
                      ( Element 
                        { elementName = Name 
                          { nameLocalName = "enumeration" 
                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                          , namePrefix = Just "xsd" 
                          } 
                        , elementAttributes = 
                          [ 
                           ( Name 
                             { nameLocalName = "value" 
                             , nameNamespace = Nothing
                             , namePrefix = Nothing
                             } 
                           , [ ContentText "X" ]
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
              , namePrefix = Just "xsd" 
              } 
            , elementAttributes = 
              [ 
               ( Name 
                 { nameLocalName = "name" 
                 , nameNamespace = Nothing
                 , namePrefix = Nothing
                 } 
               , [ ContentText "CityType" ]
               ) 
              ]
            , elementNodes = 
              [ NodeContent ( ContentText "" )
              , NodeElement 
                ( Element 
                  { elementName = Name 
                    { nameLocalName = "annotation" 
                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                    , namePrefix = Just "xsd" 
                    } 
                  , elementAttributes = []
                  , elementNodes = 
                    [ NodeContent ( ContentText "" )
                    , NodeElement 
                      ( Element 
                        { elementName = Name 
                          { nameLocalName = "documentation" 
                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                          , namePrefix = Just "xsd" 
                          } 
                        , elementAttributes = []
                        , elementNodes = 
                          [ NodeContent ( ContentText "" )
                          , NodeElement 
                            ( Element 
                              { elementName = Name 
                                { nameLocalName = "Component" 
                                , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                , namePrefix = Nothing
                                } 
                              , elementAttributes = []
                              , elementNodes = 
                                [ NodeContent ( ContentText "" )
                                , NodeElement 
                                  ( Element 
                                    { elementName = Name 
                                      { nameLocalName = "DictionaryEntryNm" 
                                      , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                      , namePrefix = Nothing
                                      } 
                                    , elementAttributes = []
                                    , elementNodes = 
                                      [ NodeContent ( ContentText "City Type" ) ]
                                    } 
                                  )
                                , NodeContent ( ContentText "" )
                                , NodeElement 
                                  ( Element 
                                    { elementName = Name 
                                      { nameLocalName = "MajorVersionNum" 
                                      , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                      , namePrefix = Nothing
                                      } 
                                    , elementAttributes = []
                                    , elementNodes = 
                                      [ NodeContent ( ContentText "1" ) ]
                                    } 
                                  )
                                , NodeContent ( ContentText "" )
                                , NodeElement 
                                  ( Element 
                                    { elementName = Name 
                                      { nameLocalName = "MinorVersionNum" 
                                      , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                      , namePrefix = Nothing
                                      } 
                                    , elementAttributes = []
                                    , elementNodes = 
                                      [ NodeContent ( ContentText "0" ) ]
                                    } 
                                  )
                                , NodeContent ( ContentText "" )
                                , NodeElement 
                                  ( Element 
                                    { elementName = Name 
                                      { nameLocalName = "VersionEffectiveBeginDt" 
                                      , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                      , namePrefix = Nothing
                                      } 
                                    , elementAttributes = []
                                    , elementNodes = 
                                      [ NodeContent ( ContentText "2008-01-08" ) ]
                                    } 
                                  )
                                , NodeContent ( ContentText "" )
                                , NodeElement 
                                  ( Element 
                                    { elementName = Name 
                                      { nameLocalName = "VersionDescriptionTxt" 
                                      , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                      , namePrefix = Nothing
                                      } 
                                    , elementAttributes = []
                                    , elementNodes = 
                                      [ NodeContent ( ContentText "EFileTypes, December 14, 2007" ) ]
                                    } 
                                  )
                                , NodeContent ( ContentText "" )
                                , NodeElement 
                                  ( Element 
                                    { elementName = Name 
                                      { nameLocalName = "DescriptionTxt" 
                                      , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                      , namePrefix = Nothing
                                      } 
                                    , elementAttributes = []
                                    , elementNodes = 
                                      [ NodeContent ( ContentText "City Name" ) ]
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
                    { nameLocalName = "restriction" 
                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                    , namePrefix = Just "xsd" 
                    } 
                  , elementAttributes = 
                    [ 
                     ( Name 
                       { nameLocalName = "base" 
                       , nameNamespace = Nothing
                       , namePrefix = Nothing
                       } 
                     , [ ContentText "xsd:string" ]
                     ) 
                    ]
                  , elementNodes = 
                    [ NodeContent ( ContentText "" )
                    , NodeElement 
                      ( Element 
                        { elementName = Name 
                          { nameLocalName = "maxLength" 
                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                          , namePrefix = Just "xsd" 
                          } 
                        , elementAttributes = 
                          [ 
                           ( Name 
                             { nameLocalName = "value" 
                             , nameNamespace = Nothing
                             , namePrefix = Nothing
                             } 
                           , [ ContentText "22" ]
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
                          , namePrefix = Just "xsd" 
                          } 
                        , elementAttributes = 
                          [ 
                           ( Name 
                             { nameLocalName = "value" 
                             , nameNamespace = Nothing
                             , namePrefix = Nothing
                             } 
                           , [ ContentText "([A-Za-z] ?)*[A-Za-z]" ]
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
              , namePrefix = Just "xsd" 
              } 
            , elementAttributes = 
              [ 
               ( Name 
                 { nameLocalName = "name" 
                 , nameNamespace = Nothing
                 , namePrefix = Nothing
                 } 
               , [ ContentText "CountryCodeType" ]
               ) 
              ]
            , elementNodes = 
              [ NodeContent ( ContentText "" )
              , NodeElement 
                ( Element 
                  { elementName = Name 
                    { nameLocalName = "annotation" 
                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                    , namePrefix = Just "xsd" 
                    } 
                  , elementAttributes = []
                  , elementNodes = 
                    [ NodeContent ( ContentText "" )
                    , NodeElement 
                      ( Element 
                        { elementName = Name 
                          { nameLocalName = "documentation" 
                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                          , namePrefix = Just "xsd" 
                          } 
                        , elementAttributes = []
                        , elementNodes = 
                          [ NodeContent ( ContentText "" )
                          , NodeElement 
                            ( Element 
                              { elementName = Name 
                                { nameLocalName = "Component" 
                                , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                , namePrefix = Nothing
                                } 
                              , elementAttributes = []
                              , elementNodes = 
                                [ NodeContent ( ContentText "" )
                                , NodeElement 
                                  ( Element 
                                    { elementName = Name 
                                      { nameLocalName = "DictionaryEntryNm" 
                                      , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                      , namePrefix = Nothing
                                      } 
                                    , elementAttributes = []
                                    , elementNodes = 
                                      [ NodeContent ( ContentText "Country Code Type" ) ]
                                    } 
                                  )
                                , NodeContent ( ContentText "" )
                                , NodeElement 
                                  ( Element 
                                    { elementName = Name 
                                      { nameLocalName = "MajorVersionNum" 
                                      , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                      , namePrefix = Nothing
                                      } 
                                    , elementAttributes = []
                                    , elementNodes = 
                                      [ NodeContent ( ContentText "1" ) ]
                                    } 
                                  )
                                , NodeContent ( ContentText "" )
                                , NodeElement 
                                  ( Element 
                                    { elementName = Name 
                                      { nameLocalName = "MinorVersionNum" 
                                      , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                      , namePrefix = Nothing
                                      } 
                                    , elementAttributes = []
                                    , elementNodes = 
                                      [ NodeContent ( ContentText "0" ) ]
                                    } 
                                  )
                                , NodeContent ( ContentText "" )
                                , NodeElement 
                                  ( Element 
                                    { elementName = Name 
                                      { nameLocalName = "VersionEffectiveBeginDt" 
                                      , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                      , namePrefix = Nothing
                                      } 
                                    , elementAttributes = []
                                    , elementNodes = 
                                      [ NodeContent ( ContentText "2006-03-31" ) ]
                                    } 
                                  )
                                , NodeContent ( ContentText "" )
                                , NodeElement 
                                  ( Element 
                                    { elementName = Name 
                                      { nameLocalName = "VersionDescriptionTxt" 
                                      , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                      , namePrefix = Nothing
                                      } 
                                    , elementAttributes = []
                                    , elementNodes = 
                                      [ NodeContent ( ContentText "ELDM 3.1  (mapped to ELDM 5.1, 11/27/2007)" ) ]
                                    } 
                                  )
                                , NodeContent ( ContentText "" )
                                , NodeElement 
                                  ( Element 
                                    { elementName = Name 
                                      { nameLocalName = "DescriptionTxt" 
                                      , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                      , namePrefix = Nothing
                                      } 
                                    , elementAttributes = []
                                    , elementNodes = 
                                      [ NodeContent ( ContentText "The name of a nation or state recognized by international postal authorities for delivery of mail." ) ]
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
                    { nameLocalName = "restriction" 
                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                    , namePrefix = Just "xsd" 
                    } 
                  , elementAttributes = 
                    [ 
                     ( Name 
                       { nameLocalName = "base" 
                       , nameNamespace = Nothing
                       , namePrefix = Nothing
                       } 
                     , [ ContentText "xsd:string" ]
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
              , namePrefix = Just "xsd" 
              } 
            , elementAttributes = 
              [ 
               ( Name 
                 { nameLocalName = "name" 
                 , nameNamespace = Nothing
                 , namePrefix = Nothing
                 } 
               , [ ContentText "DateType" ]
               ) 
              ]
            , elementNodes = 
              [ NodeContent ( ContentText "" )
              , NodeElement 
                ( Element 
                  { elementName = Name 
                    { nameLocalName = "annotation" 
                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                    , namePrefix = Just "xsd" 
                    } 
                  , elementAttributes = []
                  , elementNodes = 
                    [ NodeContent ( ContentText "" )
                    , NodeElement 
                      ( Element 
                        { elementName = Name 
                          { nameLocalName = "documentation" 
                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                          , namePrefix = Just "xsd" 
                          } 
                        , elementAttributes = []
                        , elementNodes = 
                          [ NodeContent ( ContentText "" )
                          , NodeElement 
                            ( Element 
                              { elementName = Name 
                                { nameLocalName = "Component" 
                                , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                , namePrefix = Nothing
                                } 
                              , elementAttributes = []
                              , elementNodes = 
                                [ NodeContent ( ContentText "" )
                                , NodeElement 
                                  ( Element 
                                    { elementName = Name 
                                      { nameLocalName = "DictionaryEntryNm" 
                                      , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                      , namePrefix = Nothing
                                      } 
                                    , elementAttributes = []
                                    , elementNodes = 
                                      [ NodeContent ( ContentText "Date Type" ) ]
                                    } 
                                  )
                                , NodeContent ( ContentText "" )
                                , NodeElement 
                                  ( Element 
                                    { elementName = Name 
                                      { nameLocalName = "MajorVersionNum" 
                                      , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                      , namePrefix = Nothing
                                      } 
                                    , elementAttributes = []
                                    , elementNodes = 
                                      [ NodeContent ( ContentText "1" ) ]
                                    } 
                                  )
                                , NodeContent ( ContentText "" )
                                , NodeElement 
                                  ( Element 
                                    { elementName = Name 
                                      { nameLocalName = "MinorVersionNum" 
                                      , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                      , namePrefix = Nothing
                                      } 
                                    , elementAttributes = []
                                    , elementNodes = 
                                      [ NodeContent ( ContentText "0" ) ]
                                    } 
                                  )
                                , NodeContent ( ContentText "" )
                                , NodeElement 
                                  ( Element 
                                    { elementName = Name 
                                      { nameLocalName = "VersionEffectiveBeginDt" 
                                      , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                      , namePrefix = Nothing
                                      } 
                                    , elementAttributes = []
                                    , elementNodes = 
                                      [ NodeContent ( ContentText "2008-01-08" ) ]
                                    } 
                                  )
                                , NodeContent ( ContentText "" )
                                , NodeElement 
                                  ( Element 
                                    { elementName = Name 
                                      { nameLocalName = "VersionDescriptionTxt" 
                                      , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                      , namePrefix = Nothing
                                      } 
                                    , elementAttributes = []
                                    , elementNodes = 
                                      [ NodeContent ( ContentText "EFileTypes, December 14, 2007" ) ]
                                    } 
                                  )
                                , NodeContent ( ContentText "" )
                                , NodeElement 
                                  ( Element 
                                    { elementName = Name 
                                      { nameLocalName = "DescriptionTxt" 
                                      , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                      , namePrefix = Nothing
                                      } 
                                    , elementAttributes = []
                                    , elementNodes = 
                                      [ NodeContent ( ContentText "Base type for a date; base type definition used for MeF shared forms only" ) ]
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
                    { nameLocalName = "restriction" 
                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                    , namePrefix = Just "xsd" 
                    } 
                  , elementAttributes = 
                    [ 
                     ( Name 
                       { nameLocalName = "base" 
                       , nameNamespace = Nothing
                       , namePrefix = Nothing
                       } 
                     , [ ContentText "xsd:date" ]
                     ) 
                    ]
                  , elementNodes = 
                    [ NodeContent ( ContentText "" )
                    , NodeElement 
                      ( Element 
                        { elementName = Name 
                          { nameLocalName = "pattern" 
                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                          , namePrefix = Just "xsd" 
                          } 
                        , elementAttributes = 
                          [ 
                           ( Name 
                             { nameLocalName = "value" 
                             , nameNamespace = Nothing
                             , namePrefix = Nothing
                             } 
                           , [ ContentText "[1-9][0-9]{3}\-.*" ]
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
              , namePrefix = Just "xsd" 
              } 
            , elementAttributes = 
              [ 
               ( Name 
                 { nameLocalName = "name" 
                 , nameNamespace = Nothing
                 , namePrefix = Nothing
                 } 
               , [ ContentText "DeliveryPointBarCodeType" ]
               ) 
              ]
            , elementNodes = 
              [ NodeContent ( ContentText "" )
              , NodeElement 
                ( Element 
                  { elementName = Name 
                    { nameLocalName = "annotation" 
                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                    , namePrefix = Just "xsd" 
                    } 
                  , elementAttributes = []
                  , elementNodes = 
                    [ NodeContent ( ContentText "" )
                    , NodeElement 
                      ( Element 
                        { elementName = Name 
                          { nameLocalName = "documentation" 
                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                          , namePrefix = Just "xsd" 
                          } 
                        , elementAttributes = []
                        , elementNodes = 
                          [ NodeContent ( ContentText "" )
                          , NodeElement 
                            ( Element 
                              { elementName = Name 
                                { nameLocalName = "Component" 
                                , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                , namePrefix = Nothing
                                } 
                              , elementAttributes = []
                              , elementNodes = 
                                [ NodeContent ( ContentText "" )
                                , NodeElement 
                                  ( Element 
                                    { elementName = Name 
                                      { nameLocalName = "DictionaryEntryNm" 
                                      , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                      , namePrefix = Nothing
                                      } 
                                    , elementAttributes = []
                                    , elementNodes = 
                                      [ NodeContent ( ContentText "DeliveryPointBarCode Type" ) ]
                                    } 
                                  )
                                , NodeContent ( ContentText "" )
                                , NodeElement 
                                  ( Element 
                                    { elementName = Name 
                                      { nameLocalName = "MajorVersionNum" 
                                      , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                      , namePrefix = Nothing
                                      } 
                                    , elementAttributes = []
                                    , elementNodes = 
                                      [ NodeContent ( ContentText "1" ) ]
                                    } 
                                  )
                                , NodeContent ( ContentText "" )
                                , NodeElement 
                                  ( Element 
                                    { elementName = Name 
                                      { nameLocalName = "MinorVersionNum" 
                                      , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                      , namePrefix = Nothing
                                      } 
                                    , elementAttributes = []
                                    , elementNodes = 
                                      [ NodeContent ( ContentText "0" ) ]
                                    } 
                                  )
                                , NodeContent ( ContentText "" )
                                , NodeElement 
                                  ( Element 
                                    { elementName = Name 
                                      { nameLocalName = "VersionEffectiveBeginDt" 
                                      , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                      , namePrefix = Nothing
                                      } 
                                    , elementAttributes = []
                                    , elementNodes = 
                                      [ NodeContent ( ContentText "2008-01-08" ) ]
                                    } 
                                  )
                                , NodeContent ( ContentText "" )
                                , NodeElement 
                                  ( Element 
                                    { elementName = Name 
                                      { nameLocalName = "VersionDescriptionTxt" 
                                      , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                      , namePrefix = Nothing
                                      } 
                                    , elementAttributes = []
                                    , elementNodes = []
                                    } 
                                  )
                                , NodeContent ( ContentText "" )
                                , NodeElement 
                                  ( Element 
                                    { elementName = Name 
                                      { nameLocalName = "DescriptionTxt" 
                                      , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                      , namePrefix = Nothing
                                      } 
                                    , elementAttributes = []
                                    , elementNodes = 
                                      [ NodeContent ( ContentText "Additional 3 digits after the zipPluscode" ) ]
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
                    { nameLocalName = "restriction" 
                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                    , namePrefix = Just "xsd" 
                    } 
                  , elementAttributes = 
                    [ 
                     ( Name 
                       { nameLocalName = "base" 
                       , nameNamespace = Nothing
                       , namePrefix = Nothing
                       } 
                     , [ ContentText "xsd:string" ]
                     ) 
                    ]
                  , elementNodes = 
                    [ NodeContent ( ContentText "" )
                    , NodeElement 
                      ( Element 
                        { elementName = Name 
                          { nameLocalName = "pattern" 
                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                          , namePrefix = Just "xsd" 
                          } 
                        , elementAttributes = 
                          [ 
                           ( Name 
                             { nameLocalName = "value" 
                             , nameNamespace = Nothing
                             , namePrefix = Nothing
                             } 
                           , [ ContentText "[0-9]{3}" ]
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
              , namePrefix = Just "xsd" 
              } 
            , elementAttributes = 
              [ 
               ( Name 
                 { nameLocalName = "name" 
                 , nameNamespace = Nothing
                 , namePrefix = Nothing
                 } 
               , [ ContentText "DocumentSequenceNumType" ]
               ) 
              ]
            , elementNodes = 
              [ NodeContent ( ContentText "" )
              , NodeElement 
                ( Element 
                  { elementName = Name 
                    { nameLocalName = "annotation" 
                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                    , namePrefix = Just "xsd" 
                    } 
                  , elementAttributes = []
                  , elementNodes = 
                    [ NodeContent ( ContentText "" )
                    , NodeElement 
                      ( Element 
                        { elementName = Name 
                          { nameLocalName = "documentation" 
                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                          , namePrefix = Just "xsd" 
                          } 
                        , elementAttributes = []
                        , elementNodes = 
                          [ NodeContent ( ContentText "" )
                          , NodeElement 
                            ( Element 
                              { elementName = Name 
                                { nameLocalName = "Component" 
                                , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                , namePrefix = Nothing
                                } 
                              , elementAttributes = []
                              , elementNodes = 
                                [ NodeContent ( ContentText "" )
                                , NodeElement 
                                  ( Element 
                                    { elementName = Name 
                                      { nameLocalName = "DictionaryEntryNm" 
                                      , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                      , namePrefix = Nothing
                                      } 
                                    , elementAttributes = []
                                    , elementNodes = 
                                      [ NodeContent ( ContentText "Document Sequence Number Type" ) ]
                                    } 
                                  )
                                , NodeContent ( ContentText "" )
                                , NodeElement 
                                  ( Element 
                                    { elementName = Name 
                                      { nameLocalName = "MajorVersionNum" 
                                      , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                      , namePrefix = Nothing
                                      } 
                                    , elementAttributes = []
                                    , elementNodes = 
                                      [ NodeContent ( ContentText "1" ) ]
                                    } 
                                  )
                                , NodeContent ( ContentText "" )
                                , NodeElement 
                                  ( Element 
                                    { elementName = Name 
                                      { nameLocalName = "MinorVersionNum" 
                                      , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                      , namePrefix = Nothing
                                      } 
                                    , elementAttributes = []
                                    , elementNodes = 
                                      [ NodeContent ( ContentText "0" ) ]
                                    } 
                                  )
                                , NodeContent ( ContentText "" )
                                , NodeElement 
                                  ( Element 
                                    { elementName = Name 
                                      { nameLocalName = "VersionEffectiveBeginDt" 
                                      , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                      , namePrefix = Nothing
                                      } 
                                    , elementAttributes = []
                                    , elementNodes = 
                                      [ NodeContent ( ContentText "2013-10-30" ) ]
                                    } 
                                  )
                                , NodeContent ( ContentText "" )
                                , NodeElement 
                                  ( Element 
                                    { elementName = Name 
                                      { nameLocalName = "VersionDescriptionTxt" 
                                      , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                      , namePrefix = Nothing
                                      } 
                                    , elementAttributes = []
                                    , elementNodes = 
                                      [ NodeContent ( ContentText "Baseline 7.0" ) ]
                                    } 
                                  )
                                , NodeContent ( ContentText "" )
                                , NodeElement 
                                  ( Element 
                                    { elementName = Name 
                                      { nameLocalName = "DescriptionTxt" 
                                      , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                      , namePrefix = Nothing
                                      } 
                                    , elementAttributes = []
                                    , elementNodes = 
                                      [ NodeContent ( ContentText "Developed a generic sequence number (range 00001-99999) for documents" ) ]
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
                    { nameLocalName = "restriction" 
                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                    , namePrefix = Just "xsd" 
                    } 
                  , elementAttributes = 
                    [ 
                     ( Name 
                       { nameLocalName = "base" 
                       , nameNamespace = Nothing
                       , namePrefix = Nothing
                       } 
                     , [ ContentText "xsd:integer" ]
                     ) 
                    ]
                  , elementNodes = 
                    [ NodeContent ( ContentText "" )
                    , NodeElement 
                      ( Element 
                        { elementName = Name 
                          { nameLocalName = "pattern" 
                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                          , namePrefix = Just "xsd" 
                          } 
                        , elementAttributes = 
                          [ 
                           ( Name 
                             { nameLocalName = "value" 
                             , nameNamespace = Nothing
                             , namePrefix = Nothing
                             } 
                           , [ ContentText "(0000[1-9]|000[1-9][0-9]|00[1-9][0-9][0-9]|0[1-9][0-9][0-9][0-9]|[1-9][0-9][0-9][0-9][0-9])" ]
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
              , namePrefix = Just "xsd" 
              } 
            , elementAttributes = 
              [ 
               ( Name 
                 { nameLocalName = "name" 
                 , nameNamespace = Nothing
                 , namePrefix = Nothing
                 } 
               , [ ContentText "EINType" ]
               ) 
              ]
            , elementNodes = 
              [ NodeContent ( ContentText "" )
              , NodeElement 
                ( Element 
                  { elementName = Name 
                    { nameLocalName = "annotation" 
                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                    , namePrefix = Just "xsd" 
                    } 
                  , elementAttributes = []
                  , elementNodes = 
                    [ NodeContent ( ContentText "" )
                    , NodeElement 
                      ( Element 
                        { elementName = Name 
                          { nameLocalName = "documentation" 
                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                          , namePrefix = Just "xsd" 
                          } 
                        , elementAttributes = []
                        , elementNodes = 
                          [ NodeContent ( ContentText "" )
                          , NodeElement 
                            ( Element 
                              { elementName = Name 
                                { nameLocalName = "Component" 
                                , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                , namePrefix = Nothing
                                } 
                              , elementAttributes = []
                              , elementNodes = 
                                [ NodeContent ( ContentText "" )
                                , NodeElement 
                                  ( Element 
                                    { elementName = Name 
                                      { nameLocalName = "DictionaryEntryNm" 
                                      , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                      , namePrefix = Nothing
                                      } 
                                    , elementAttributes = []
                                    , elementNodes = 
                                      [ NodeContent ( ContentText "EIN Type" ) ]
                                    } 
                                  )
                                , NodeContent ( ContentText "" )
                                , NodeElement 
                                  ( Element 
                                    { elementName = Name 
                                      { nameLocalName = "MajorVersionNum" 
                                      , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                      , namePrefix = Nothing
                                      } 
                                    , elementAttributes = []
                                    , elementNodes = 
                                      [ NodeContent ( ContentText "1" ) ]
                                    } 
                                  )
                                , NodeContent ( ContentText "" )
                                , NodeElement 
                                  ( Element 
                                    { elementName = Name 
                                      { nameLocalName = "MinorVersionNum" 
                                      , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                      , namePrefix = Nothing
                                      } 
                                    , elementAttributes = []
                                    , elementNodes = 
                                      [ NodeContent ( ContentText "0" ) ]
                                    } 
                                  )
                                , NodeContent ( ContentText "" )
                                , NodeElement 
                                  ( Element 
                                    { elementName = Name 
                                      { nameLocalName = "VersionEffectiveBeginDt" 
                                      , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                      , namePrefix = Nothing
                                      } 
                                    , elementAttributes = []
                                    , elementNodes = 
                                      [ NodeContent ( ContentText "2008-01-08" ) ]
                                    } 
                                  )
                                , NodeContent ( ContentText "" )
                                , NodeElement 
                                  ( Element 
                                    { elementName = Name 
                                      { nameLocalName = "VersionDescriptionTxt" 
                                      , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                      , namePrefix = Nothing
                                      } 
                                    , elementAttributes = []
                                    , elementNodes = 
                                      [ NodeContent ( ContentText "EFileTypes, December 14, 2007" ) ]
                                    } 
                                  )
                                , NodeContent ( ContentText "" )
                                , NodeElement 
                                  ( Element 
                                    { elementName = Name 
                                      { nameLocalName = "DescriptionTxt" 
                                      , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                      , namePrefix = Nothing
                                      } 
                                    , elementAttributes = []
                                    , elementNodes = 
                                      [ NodeContent ( ContentText "Base type for an employer Identification number" ) ]
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
                    { nameLocalName = "restriction" 
                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                    , namePrefix = Just "xsd" 
                    } 
                  , elementAttributes = 
                    [ 
                     ( Name 
                       { nameLocalName = "base" 
                       , nameNamespace = Nothing
                       , namePrefix = Nothing
                       } 
                     , [ ContentText "xsd:string" ]
                     ) 
                    ]
                  , elementNodes = 
                    [ NodeContent ( ContentText "" )
                    , NodeElement 
                      ( Element 
                        { elementName = Name 
                          { nameLocalName = "pattern" 
                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                          , namePrefix = Just "xsd" 
                          } 
                        , elementAttributes = 
                          [ 
                           ( Name 
                             { nameLocalName = "value" 
                             , nameNamespace = Nothing
                             , namePrefix = Nothing
                             } 
                           , [ ContentText "[0-9]{9}" ]
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
              , namePrefix = Just "xsd" 
              } 
            , elementAttributes = 
              [ 
               ( Name 
                 { nameLocalName = "name" 
                 , nameNamespace = Nothing
                 , namePrefix = Nothing
                 } 
               , [ ContentText "EPDSubmissionSourceCdType" ]
               ) 
              ]
            , elementNodes = 
              [ NodeContent ( ContentText "" )
              , NodeElement 
                ( Element 
                  { elementName = Name 
                    { nameLocalName = "annotation" 
                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                    , namePrefix = Just "xsd" 
                    } 
                  , elementAttributes = []
                  , elementNodes = 
                    [ NodeContent ( ContentText "" )
                    , NodeElement 
                      ( Element 
                        { elementName = Name 
                          { nameLocalName = "documentation" 
                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                          , namePrefix = Just "xsd" 
                          } 
                        , elementAttributes = []
                        , elementNodes = 
                          [ NodeContent ( ContentText "" )
                          , NodeElement 
                            ( Element 
                              { elementName = Name 
                                { nameLocalName = "Component" 
                                , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                , namePrefix = Nothing
                                } 
                              , elementAttributes = []
                              , elementNodes = 
                                [ NodeContent ( ContentText "" )
                                , NodeElement 
                                  ( Element 
                                    { elementName = Name 
                                      { nameLocalName = "DictionaryEntryNm" 
                                      , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                      , namePrefix = Nothing
                                      } 
                                    , elementAttributes = []
                                    , elementNodes = 
                                      [ NodeContent ( ContentText "EPDSubmission Source Code Type" ) ]
                                    } 
                                  )
                                , NodeContent ( ContentText "" )
                                , NodeElement 
                                  ( Element 
                                    { elementName = Name 
                                      { nameLocalName = "MajorVersionNum" 
                                      , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                      , namePrefix = Nothing
                                      } 
                                    , elementAttributes = []
                                    , elementNodes = 
                                      [ NodeContent ( ContentText "1" ) ]
                                    } 
                                  )
                                , NodeContent ( ContentText "" )
                                , NodeElement 
                                  ( Element 
                                    { elementName = Name 
                                      { nameLocalName = "MinorVersionNum" 
                                      , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                      , namePrefix = Nothing
                                      } 
                                    , elementAttributes = []
                                    , elementNodes = 
                                      [ NodeContent ( ContentText "0" ) ]
                                    } 
                                  )
                                , NodeContent ( ContentText "" )
                                , NodeElement 
                                  ( Element 
                                    { elementName = Name 
                                      { nameLocalName = "VersionEffectiveBeginDt" 
                                      , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                      , namePrefix = Nothing
                                      } 
                                    , elementAttributes = []
                                    , elementNodes = 
                                      [ NodeContent ( ContentText "2013-07-08" ) ]
                                    } 
                                  )
                                , NodeContent ( ContentText "" )
                                , NodeElement 
                                  ( Element 
                                    { elementName = Name 
                                      { nameLocalName = "VersionDescriptionTxt" 
                                      , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                      , namePrefix = Nothing
                                      } 
                                    , elementAttributes = []
                                    , elementNodes = 
                                      [ NodeContent ( ContentText "Initial version" ) ]
                                    } 
                                  )
                                , NodeContent ( ContentText "" )
                                , NodeElement 
                                  ( Element 
                                    { elementName = Name 
                                      { nameLocalName = "DescriptionTxt" 
                                      , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                      , namePrefix = Nothing
                                      } 
                                    , elementAttributes = []
                                    , elementNodes = 
                                      [ NodeContent ( ContentText "A code used to indicate the source of the EPD data" ) ]
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
                    { nameLocalName = "restriction" 
                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                    , namePrefix = Just "xsd" 
                    } 
                  , elementAttributes = 
                    [ 
                     ( Name 
                       { nameLocalName = "base" 
                       , nameNamespace = Nothing
                       , namePrefix = Nothing
                       } 
                     , [ ContentText "xsd:string" ]
                     ) 
                    ]
                  , elementNodes = 
                    [ NodeContent ( ContentText "" )
                    , NodeElement 
                      ( Element 
                        { elementName = Name 
                          { nameLocalName = "enumeration" 
                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                          , namePrefix = Just "xsd" 
                          } 
                        , elementAttributes = 
                          [ 
                           ( Name 
                             { nameLocalName = "value" 
                             , nameNamespace = Nothing
                             , namePrefix = Nothing
                             } 
                           , [ ContentText "Individual" ]
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
                          , namePrefix = Just "xsd" 
                          } 
                        , elementAttributes = 
                          [ 
                           ( Name 
                             { nameLocalName = "value" 
                             , nameNamespace = Nothing
                             , namePrefix = Nothing
                             } 
                           , [ ContentText "SHOP" ]
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
              , namePrefix = Just "xsd" 
              } 
            , elementAttributes = 
              [ 
               ( Name 
                 { nameLocalName = "name" 
                 , nameNamespace = Nothing
                 , namePrefix = Nothing
                 } 
               , [ ContentText "ExchangeIdType" ]
               ) 
              ]
            , elementNodes = 
              [ NodeContent ( ContentText "" )
              , NodeElement 
                ( Element 
                  { elementName = Name 
                    { nameLocalName = "annotation" 
                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                    , namePrefix = Just "xsd" 
                    } 
                  , elementAttributes = []
                  , elementNodes = 
                    [ NodeContent ( ContentText "" )
                    , NodeElement 
                      ( Element 
                        { elementName = Name 
                          { nameLocalName = "documentation" 
                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                          , namePrefix = Just "xsd" 
                          } 
                        , elementAttributes = []
                        , elementNodes = 
                          [ NodeContent ( ContentText "" )
                          , NodeElement 
                            ( Element 
                              { elementName = Name 
                                { nameLocalName = "Component" 
                                , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                , namePrefix = Nothing
                                } 
                              , elementAttributes = []
                              , elementNodes = 
                                [ NodeContent ( ContentText "" )
                                , NodeElement 
                                  ( Element 
                                    { elementName = Name 
                                      { nameLocalName = "DictionaryEntryNm" 
                                      , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                      , namePrefix = Nothing
                                      } 
                                    , elementAttributes = []
                                    , elementNodes = 
                                      [ NodeContent ( ContentText "Exchange Identification Type" ) ]
                                    } 
                                  )
                                , NodeContent ( ContentText "" )
                                , NodeElement 
                                  ( Element 
                                    { elementName = Name 
                                      { nameLocalName = "MajorVersionNum" 
                                      , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                      , namePrefix = Nothing
                                      } 
                                    , elementAttributes = []
                                    , elementNodes = 
                                      [ NodeContent ( ContentText "1" ) ]
                                    } 
                                  )
                                , NodeContent ( ContentText "" )
                                , NodeElement 
                                  ( Element 
                                    { elementName = Name 
                                      { nameLocalName = "MinorVersionNum" 
                                      , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                      , namePrefix = Nothing
                                      } 
                                    , elementAttributes = []
                                    , elementNodes = 
                                      [ NodeContent ( ContentText "0" ) ]
                                    } 
                                  )
                                , NodeContent ( ContentText "" )
                                , NodeElement 
                                  ( Element 
                                    { elementName = Name 
                                      { nameLocalName = "VersionEffectiveBeginDt" 
                                      , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                      , namePrefix = Nothing
                                      } 
                                    , elementAttributes = []
                                    , elementNodes = 
                                      [ NodeContent ( ContentText "2012-01-08" ) ]
                                    } 
                                  )
                                , NodeContent ( ContentText "" )
                                , NodeElement 
                                  ( Element 
                                    { elementName = Name 
                                      { nameLocalName = "VersionDescriptionTxt" 
                                      , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                      , namePrefix = Nothing
                                      } 
                                    , elementAttributes = []
                                    , elementNodes = 
                                      [ NodeContent ( ContentText "Initial Version" ) ]
                                    } 
                                  )
                                , NodeContent ( ContentText "" )
                                , NodeElement 
                                  ( Element 
                                    { elementName = Name 
                                      { nameLocalName = "DescriptionTxt" 
                                      , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                      , namePrefix = Nothing
                                      } 
                                    , elementAttributes = []
                                    , elementNodes = 
                                      [ NodeContent ( ContentText "A exchange identifier used for all request and response messages in ACA messages exchanges.The CMS Exchange ID will be a part of the MessageID in the SOAP header" ) ]
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
                    { nameLocalName = "restriction" 
                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                    , namePrefix = Just "xsd" 
                    } 
                  , elementAttributes = 
                    [ 
                     ( Name 
                       { nameLocalName = "base" 
                       , nameNamespace = Nothing
                       , namePrefix = Nothing
                       } 
                     , [ ContentText "xsd:string" ]
                     ) 
                    ]
                  , elementNodes = 
                    [ NodeContent ( ContentText "" )
                    , NodeElement 
                      ( Element 
                        { elementName = Name 
                          { nameLocalName = "pattern" 
                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                          , namePrefix = Just "xsd" 
                          } 
                        , elementAttributes = 
                          [ 
                           ( Name 
                             { nameLocalName = "value" 
                             , nameNamespace = Nothing
                             , namePrefix = Nothing
                             } 
                           , [ ContentText "[0-9]{2}\.[a-zA-Z]{2}[a-zA-Z*]{1}\.[a-zA-Z0-9]{3}\.[0-9]{3}\.[0-9]{3}" ]
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
              , namePrefix = Just "xsd" 
              } 
            , elementAttributes = 
              [ 
               ( Name 
                 { nameLocalName = "name" 
                 , nameNamespace = Nothing
                 , namePrefix = Nothing
                 } 
               , [ ContentText "ExemptionCertificateNumType" ]
               ) 
              ]
            , elementNodes = 
              [ NodeContent ( ContentText "" )
              , NodeElement 
                ( Element 
                  { elementName = Name 
                    { nameLocalName = "annotation" 
                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                    , namePrefix = Just "xsd" 
                    } 
                  , elementAttributes = []
                  , elementNodes = 
                    [ NodeContent ( ContentText "" )
                    , NodeElement 
                      ( Element 
                        { elementName = Name 
                          { nameLocalName = "documentation" 
                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                          , namePrefix = Just "xsd" 
                          } 
                        , elementAttributes = []
                        , elementNodes = 
                          [ NodeContent ( ContentText "" )
                          , NodeElement 
                            ( Element 
                              { elementName = Name 
                                { nameLocalName = "Component" 
                                , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                , namePrefix = Nothing
                                } 
                              , elementAttributes = []
                              , elementNodes = 
                                [ NodeContent ( ContentText "" )
                                , NodeElement 
                                  ( Element 
                                    { elementName = Name 
                                      { nameLocalName = "DictionaryEntryNm" 
                                      , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                      , namePrefix = Nothing
                                      } 
                                    , elementAttributes = []
                                    , elementNodes = 
                                      [ NodeContent ( ContentText "Exemption Certificate Number Type" ) ]
                                    } 
                                  )
                                , NodeContent ( ContentText "" )
                                , NodeElement 
                                  ( Element 
                                    { elementName = Name 
                                      { nameLocalName = "MajorVersionNum" 
                                      , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                      , namePrefix = Nothing
                                      } 
                                    , elementAttributes = []
                                    , elementNodes = 
                                      [ NodeContent ( ContentText "1" ) ]
                                    } 
                                  )
                                , NodeContent ( ContentText "" )
                                , NodeElement 
                                  ( Element 
                                    { elementName = Name 
                                      { nameLocalName = "MinorVersionNum" 
                                      , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                      , namePrefix = Nothing
                                      } 
                                    , elementAttributes = []
                                    , elementNodes = 
                                      [ NodeContent ( ContentText "0" ) ]
                                    } 
                                  )
                                , NodeContent ( ContentText "" )
                                , NodeElement 
                                  ( Element 
                                    { elementName = Name 
                                      { nameLocalName = "VersionEffectiveBeginDt" 
                                      , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                      , namePrefix = Nothing
                                      } 
                                    , elementAttributes = []
                                    , elementNodes = 
                                      [ NodeContent ( ContentText "2013-03-25" ) ]
                                    } 
                                  )
                                , NodeContent ( ContentText "" )
                                , NodeElement 
                                  ( Element 
                                    { elementName = Name 
                                      { nameLocalName = "VersionDescriptionTxt" 
                                      , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                      , namePrefix = Nothing
                                      } 
                                    , elementAttributes = []
                                    , elementNodes = 
                                      [ NodeContent ( ContentText "Initial Version" ) ]
                                    } 
                                  )
                                , NodeContent ( ContentText "" )
                                , NodeElement 
                                  ( Element 
                                    { elementName = Name 
                                      { nameLocalName = "DescriptionTxt" 
                                      , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                      , namePrefix = Nothing
                                      } 
                                    , elementAttributes = []
                                    , elementNodes = 
                                      [ NodeContent ( ContentText "Certificate Number needs to be a smart code which may include information on the following values(AA A -NN NN - NNNN - NN) as part of its numbering sequence: Combination of  StateCode  ExemptionType -  Beginning Month of Exemption Ending Month of Exemption - Unique ECNs created - Year for which Exemption is applicable" ) ]
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
                    { nameLocalName = "restriction" 
                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                    , namePrefix = Just "xsd" 
                    } 
                  , elementAttributes = 
                    [ 
                     ( Name 
                       { nameLocalName = "base" 
                       , nameNamespace = Nothing
                       , namePrefix = Nothing
                       } 
                     , [ ContentText "xsd:string" ]
                     ) 
                    ]
                  , elementNodes = 
                    [ NodeContent ( ContentText "" )
                    , NodeElement 
                      ( Element 
                        { elementName = Name 
                          { nameLocalName = "pattern" 
                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                          , namePrefix = Just "xsd" 
                          } 
                        , elementAttributes = 
                          [ 
                           ( Name 
                             { nameLocalName = "value" 
                             , nameNamespace = Nothing
                             , namePrefix = Nothing
                             } 
                           , [ ContentText "[a-hA-H]{1}[a-zA-Z0-9]{5}[NYny]{1}" ]
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
              , namePrefix = Just "xsd" 
              } 
            , elementAttributes = 
              [ 
               ( Name 
                 { nameLocalName = "name" 
                 , nameNamespace = Nothing
                 , namePrefix = Nothing
                 } 
               , [ ContentText "FileSourceCodeType" ]
               ) 
              ]
            , elementNodes = 
              [ NodeContent ( ContentText "" )
              , NodeElement 
                ( Element 
                  { elementName = Name 
                    { nameLocalName = "annotation" 
                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                    , namePrefix = Just "xsd" 
                    } 
                  , elementAttributes = []
                  , elementNodes = 
                    [ NodeContent ( ContentText "" )
                    , NodeElement 
                      ( Element 
                        { elementName = Name 
                          { nameLocalName = "documentation" 
                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                          , namePrefix = Just "xsd" 
                          } 
                        , elementAttributes = []
                        , elementNodes = 
                          [ NodeContent ( ContentText "" )
                          , NodeElement 
                            ( Element 
                              { elementName = Name 
                                { nameLocalName = "Component" 
                                , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                , namePrefix = Nothing
                                } 
                              , elementAttributes = []
                              , elementNodes = 
                                [ NodeContent ( ContentText "" )
                                , NodeElement 
                                  ( Element 
                                    { elementName = Name 
                                      { nameLocalName = "DictionaryEntryNm" 
                                      , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                      , namePrefix = Nothing
                                      } 
                                    , elementAttributes = []
                                    , elementNodes = 
                                      [ NodeContent ( ContentText "File Source Code Type" ) ]
                                    } 
                                  )
                                , NodeContent ( ContentText "" )
                                , NodeElement 
                                  ( Element 
                                    { elementName = Name 
                                      { nameLocalName = "MajorVersionNum" 
                                      , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                      , namePrefix = Nothing
                                      } 
                                    , elementAttributes = []
                                    , elementNodes = 
                                      [ NodeContent ( ContentText "2" ) ]
                                    } 
                                  )
                                , NodeContent ( ContentText "" )
                                , NodeElement 
                                  ( Element 
                                    { elementName = Name 
                                      { nameLocalName = "MinorVersionNum" 
                                      , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                      , namePrefix = Nothing
                                      } 
                                    , elementAttributes = []
                                    , elementNodes = 
                                      [ NodeContent ( ContentText "0" ) ]
                                    } 
                                  )
                                , NodeContent ( ContentText "" )
                                , NodeElement 
                                  ( Element 
                                    { elementName = Name 
                                      { nameLocalName = "VersionEffectiveBeginDt" 
                                      , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                      , namePrefix = Nothing
                                      } 
                                    , elementAttributes = []
                                    , elementNodes = 
                                      [ NodeContent ( ContentText "2007-10-15" ) ]
                                    } 
                                  )
                                , NodeContent ( ContentText "" )
                                , NodeElement 
                                  ( Element 
                                    { elementName = Name 
                                      { nameLocalName = "VersionDescriptionTxt" 
                                      , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                      , namePrefix = Nothing
                                      } 
                                    , elementAttributes = []
                                    , elementNodes = 
                                      [ NodeContent ( ContentText "From the old CODELIST schema, moved enumeration values in this CBC  (mapped to ELDM 5.1, 11/27/2007)" ) ]
                                    } 
                                  )
                                , NodeContent ( ContentText "" )
                                , NodeElement 
                                  ( Element 
                                    { elementName = Name 
                                      { nameLocalName = "DescriptionTxt" 
                                      , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                      , namePrefix = Nothing
                                      } 
                                    , elementAttributes = []
                                    , elementNodes = 
                                      [ NodeContent ( ContentText "A current systems data source for a Taxpayer obligation line item.  The sources of transactions (with their current legacy codes) include:
                                                      Individual Retirement Account File (IRAF),   Business Master File (BMF),   Individual Master File (IMF),   Non-Master File (NMF),   Financial Reporting (FR) processing, EFTPS,   FTD,   DOJ,   SITLP,   ISRP,   RPS,   IDRS" ) ]
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
                        { nameLocalName = "restriction" 
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                        , namePrefix = Just "xsd" 
                        } 
                      , elementAttributes = 
                        [ 
                         ( Name 
                           { nameLocalName = "base" 
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           } 
                         , [ ContentText "xsd:string" ]
                         ) 
                        ]
                      , elementNodes = 
                        [ NodeContent ( ContentText "" )
                        , NodeElement 
                          ( Element 
                            { elementName = Name 
                              { nameLocalName = "maxLength" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
                               ( Name 
                                 { nameLocalName = "value" 
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 } 
                               , [ ContentText "6" ]
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
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
                               ( Name 
                                 { nameLocalName = "value" 
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 } 
                               , [ ContentText "1" ]
                               ) 
                              ]
                            , elementNodes = 
                              [ NodeContent ( ContentText "" )
                              , NodeElement 
                                ( Element 
                                  { elementName = Name 
                                    { nameLocalName = "annotation" 
                                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                    , namePrefix = Just "xsd" 
                                    } 
                                  , elementAttributes = []
                                  , elementNodes = 
                                    [ NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "documentation" 
                                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                          , namePrefix = Just "xsd" 
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "IMF" ) ]
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
                              { nameLocalName = "enumeration" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
                               ( Name 
                                 { nameLocalName = "value" 
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 } 
                               , [ ContentText "2" ]
                               ) 
                              ]
                            , elementNodes = 
                              [ NodeContent ( ContentText "" )
                              , NodeElement 
                                ( Element 
                                  { elementName = Name 
                                    { nameLocalName = "annotation" 
                                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                    , namePrefix = Just "xsd" 
                                    } 
                                  , elementAttributes = []
                                  , elementNodes = 
                                    [ NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "documentation" 
                                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                          , namePrefix = Just "xsd" 
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "BMF" ) ]
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
                              { nameLocalName = "enumeration" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
                               ( Name 
                                 { nameLocalName = "value" 
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 } 
                               , [ ContentText "3" ]
                               ) 
                              ]
                            , elementNodes = 
                              [ NodeContent ( ContentText "" )
                              , NodeElement 
                                ( Element 
                                  { elementName = Name 
                                    { nameLocalName = "annotation" 
                                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                    , namePrefix = Just "xsd" 
                                    } 
                                  , elementAttributes = []
                                  , elementNodes = 
                                    [ NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "documentation" 
                                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                          , namePrefix = Just "xsd" 
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "EPMF" ) ]
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
                              { nameLocalName = "enumeration" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
                               ( Name 
                                 { nameLocalName = "value" 
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 } 
                               , [ ContentText "4" ]
                               ) 
                              ]
                            , elementNodes = 
                              [ NodeContent ( ContentText "" )
                              , NodeElement 
                                ( Element 
                                  { elementName = Name 
                                    { nameLocalName = "annotation" 
                                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                    , namePrefix = Just "xsd" 
                                    } 
                                  , elementAttributes = []
                                  , elementNodes = 
                                    [ NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "documentation" 
                                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                          , namePrefix = Just "xsd" 
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "IRAF" ) ]
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
                              { nameLocalName = "enumeration" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
                               ( Name 
                                 { nameLocalName = "value" 
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 } 
                               , [ ContentText "5" ]
                               ) 
                              ]
                            , elementNodes = 
                              [ NodeContent ( ContentText "" )
                              , NodeElement 
                                ( Element 
                                  { elementName = Name 
                                    { nameLocalName = "annotation" 
                                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                    , namePrefix = Just "xsd" 
                                    } 
                                  , elementAttributes = []
                                  , elementNodes = 
                                    [ NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "documentation" 
                                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                          , namePrefix = Just "xsd" 
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "Unknown" ) ]
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
                              { nameLocalName = "enumeration" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
                               ( Name 
                                 { nameLocalName = "value" 
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 } 
                               , [ ContentText "6" ]
                               ) 
                              ]
                            , elementNodes = 
                              [ NodeContent ( ContentText "" )
                              , NodeElement 
                                ( Element 
                                  { elementName = Name 
                                    { nameLocalName = "annotation" 
                                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                    , namePrefix = Just "xsd" 
                                    } 
                                  , elementAttributes = []
                                  , elementNodes = 
                                    [ NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "documentation" 
                                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                          , namePrefix = Just "xsd" 
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "NMF" ) ]
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
                              { nameLocalName = "enumeration" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
                               ( Name 
                                 { nameLocalName = "value" 
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 } 
                               , [ ContentText "7" ]
                               ) 
                              ]
                            , elementNodes = 
                              [ NodeContent ( ContentText "" )
                              , NodeElement 
                                ( Element 
                                  { elementName = Name 
                                    { nameLocalName = "annotation" 
                                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                    , namePrefix = Just "xsd" 
                                    } 
                                  , elementAttributes = []
                                  , elementNodes = 
                                    [ NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "documentation" 
                                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                          , namePrefix = Just "xsd" 
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "Unknown" ) ]
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
                              { nameLocalName = "enumeration" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
                               ( Name 
                                 { nameLocalName = "value" 
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 } 
                               , [ ContentText "8" ]
                               ) 
                              ]
                            , elementNodes = 
                              [ NodeContent ( ContentText "" )
                              , NodeElement 
                                ( Element 
                                  { elementName = Name 
                                    { nameLocalName = "annotation" 
                                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                    , namePrefix = Just "xsd" 
                                    } 
                                  , elementAttributes = []
                                  , elementNodes = 
                                    [ NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "documentation" 
                                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                          , namePrefix = Just "xsd" 
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "Unknown" ) ]
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
                              { nameLocalName = "enumeration" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
                               ( Name 
                                 { nameLocalName = "value" 
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 } 
                               , [ ContentText "9" ]
                               ) 
                              ]
                            , elementNodes = 
                              [ NodeContent ( ContentText "" )
                              , NodeElement 
                                ( Element 
                                  { elementName = Name 
                                    { nameLocalName = "annotation" 
                                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                    , namePrefix = Just "xsd" 
                                    } 
                                  , elementAttributes = []
                                  , elementNodes = 
                                    [ NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "documentation" 
                                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                          , namePrefix = Just "xsd" 
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "CAF" ) ]
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
                              { nameLocalName = "enumeration" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
                               ( Name 
                                 { nameLocalName = "value" 
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 } 
                               , [ ContentText "0" ]
                               ) 
                              ]
                            , elementNodes = 
                              [ NodeContent ( ContentText "" )
                              , NodeElement 
                                ( Element 
                                  { elementName = Name 
                                    { nameLocalName = "annotation" 
                                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                    , namePrefix = Just "xsd" 
                                    } 
                                  , elementAttributes = []
                                  , elementNodes = 
                                    [ NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "documentation" 
                                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                          , namePrefix = Just "xsd" 
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "AIMS" ) ]
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
                  , namePrefix = Just "xsd" 
                  } 
                , elementAttributes = 
                  [ 
                   ( Name 
                     { nameLocalName = "name" 
                     , nameNamespace = Nothing
                     , namePrefix = Nothing
                     } 
                   , [ ContentText "FileStatusCdType" ]
                   ) 
                  ]
                , elementNodes = 
                  [ NodeContent ( ContentText "" )
                  , NodeElement 
                    ( Element 
                      { elementName = Name 
                        { nameLocalName = "annotation" 
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                        , namePrefix = Just "xsd" 
                        } 
                      , elementAttributes = []
                      , elementNodes = 
                        [ NodeContent ( ContentText "" )
                        , NodeElement 
                          ( Element 
                            { elementName = Name 
                              { nameLocalName = "documentation" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = []
                            , elementNodes = 
                              [ NodeContent ( ContentText "" )
                              , NodeElement 
                                ( Element 
                                  { elementName = Name 
                                    { nameLocalName = "Component" 
                                    , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                    , namePrefix = Nothing
                                    } 
                                  , elementAttributes = []
                                  , elementNodes = 
                                    [ NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "DictionaryEntryNm" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "File Status Code Type" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "MajorVersionNum" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "1" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "MinorVersionNum" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "0" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "VersionEffectiveBeginDt" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "2013-10-28" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "VersionDescriptionTxt" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "Initial version" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "DescriptionTxt" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "A code used to indicate the file receving status" ) ]
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
                        { nameLocalName = "restriction" 
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                        , namePrefix = Just "xsd" 
                        } 
                      , elementAttributes = 
                        [ 
                         ( Name 
                           { nameLocalName = "base" 
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           } 
                         , [ ContentText "xsd:string" ]
                         ) 
                        ]
                      , elementNodes = 
                        [ NodeContent ( ContentText "" )
                        , NodeElement 
                          ( Element 
                            { elementName = Name 
                              { nameLocalName = "enumeration" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
                               ( Name 
                                 { nameLocalName = "value" 
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 } 
                               , [ ContentText "Rejected" ]
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
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
                               ( Name 
                                 { nameLocalName = "value" 
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 } 
                               , [ ContentText "Delivered-Success" ]
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
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
                               ( Name 
                                 { nameLocalName = "value" 
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 } 
                               , [ ContentText "Delivered-Exception" ]
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
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
                               ( Name 
                                 { nameLocalName = "value" 
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 } 
                               , [ ContentText "Delivery-Failed" ]
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
                  , namePrefix = Just "xsd" 
                  } 
                , elementAttributes = 
                  [ 
                   ( Name 
                     { nameLocalName = "name" 
                     , nameNamespace = Nothing
                     , namePrefix = Nothing
                     } 
                   , [ ContentText "IdentifierType" ]
                   ) 
                  ]
                , elementNodes = 
                  [ NodeContent ( ContentText "" )
                  , NodeElement 
                    ( Element 
                      { elementName = Name 
                        { nameLocalName = "annotation" 
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                        , namePrefix = Just "xsd" 
                        } 
                      , elementAttributes = []
                      , elementNodes = 
                        [ NodeContent ( ContentText "" )
                        , NodeElement 
                          ( Element 
                            { elementName = Name 
                              { nameLocalName = "documentation" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = []
                            , elementNodes = 
                              [ NodeContent ( ContentText "" )
                              , NodeElement 
                                ( Element 
                                  { elementName = Name 
                                    { nameLocalName = "Component" 
                                    , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                    , namePrefix = Nothing
                                    } 
                                  , elementAttributes = []
                                  , elementNodes = 
                                    [ NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "DictionaryEntryNm" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "Identification Type" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "MajorVersionNum" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "1" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "MinorVersionNum" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "0" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "VersionEffectiveBeginDt" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "2013-04-26" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "VersionDescriptionTxt" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "Initial version" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "DescriptionTxt" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "Base type for a 5 character formatted number." ) ]
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
                        { nameLocalName = "restriction" 
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                        , namePrefix = Just "xsd" 
                        } 
                      , elementAttributes = 
                        [ 
                         ( Name 
                           { nameLocalName = "base" 
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           } 
                         , [ ContentText "xsd:integer" ]
                         ) 
                        ]
                      , elementNodes = 
                        [ NodeContent ( ContentText "" )
                        , NodeElement 
                          ( Element 
                            { elementName = Name 
                              { nameLocalName = "totalDigits" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
                               ( Name 
                                 { nameLocalName = "value" 
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 } 
                               , [ ContentText "16" ]
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
                  , namePrefix = Just "xsd" 
                  } 
                , elementAttributes = 
                  [ 
                   ( Name 
                     { nameLocalName = "name" 
                     , nameNamespace = Nothing
                     , namePrefix = Nothing
                     } 
                   , [ ContentText "IndicatorCodeType" ]
                   ) 
                  ]
                , elementNodes = 
                  [ NodeContent ( ContentText "" )
                  , NodeElement 
                    ( Element 
                      { elementName = Name 
                        { nameLocalName = "annotation" 
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                        , namePrefix = Just "xsd" 
                        } 
                      , elementAttributes = []
                      , elementNodes = 
                        [ NodeContent ( ContentText "" )
                        , NodeElement 
                          ( Element 
                            { elementName = Name 
                              { nameLocalName = "documentation" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = []
                            , elementNodes = 
                              [ NodeContent ( ContentText "" )
                              , NodeElement 
                                ( Element 
                                  { elementName = Name 
                                    { nameLocalName = "Component" 
                                    , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                    , namePrefix = Nothing
                                    } 
                                  , elementAttributes = []
                                  , elementNodes = 
                                    [ NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "DictionaryEntryNm" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "Indicator Code Type" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "MajorVersionNum" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "1" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "MinorVersionNum" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "0" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "VersionEffectiveBeginDt" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "2006-03-06" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "VersionDescriptionTxt" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "Original DescriptionTxt" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "DescriptionTxt" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "Indicator for flags that must b e of type=string. Y= yes, N= no, not required" ) ]
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
                        { nameLocalName = "restriction" 
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                        , namePrefix = Just "xsd" 
                        } 
                      , elementAttributes = 
                        [ 
                         ( Name 
                           { nameLocalName = "base" 
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           } 
                         , [ ContentText "xsd:string" ]
                         ) 
                        ]
                      , elementNodes = 
                        [ NodeContent ( ContentText "" )
                        , NodeElement 
                          ( Element 
                            { elementName = Name 
                              { nameLocalName = "enumeration" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
                               ( Name 
                                 { nameLocalName = "value" 
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 } 
                               , [ ContentText "Y" ]
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
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
                               ( Name 
                                 { nameLocalName = "value" 
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 } 
                               , [ ContentText "N" ]
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
                  , namePrefix = Just "xsd" 
                  } 
                , elementAttributes = 
                  [ 
                   ( Name 
                     { nameLocalName = "name" 
                     , nameNamespace = Nothing
                     , namePrefix = Nothing
                     } 
                   , [ ContentText "IntegerNNType" ]
                   ) 
                  ]
                , elementNodes = 
                  [ NodeContent ( ContentText "" )
                  , NodeElement 
                    ( Element 
                      { elementName = Name 
                        { nameLocalName = "annotation" 
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                        , namePrefix = Just "xsd" 
                        } 
                      , elementAttributes = []
                      , elementNodes = 
                        [ NodeContent ( ContentText "" )
                        , NodeElement 
                          ( Element 
                            { elementName = Name 
                              { nameLocalName = "documentation" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = []
                            , elementNodes = 
                              [ NodeContent ( ContentText "" )
                              , NodeElement 
                                ( Element 
                                  { elementName = Name 
                                    { nameLocalName = "Component" 
                                    , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                    , namePrefix = Nothing
                                    } 
                                  , elementAttributes = []
                                  , elementNodes = 
                                    [ NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "DictionaryEntryNm" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "Integer Non NegativeType" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "MajorVersionNum" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "1" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "MinorVersionNum" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "0" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "VersionEffectiveBeginDt" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "2008-01-08" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "VersionDescriptionTxt" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "EFileTypes, December 14, 2007" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "DescriptionTxt" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "Base type for a non-negative integer" ) ]
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
                        { nameLocalName = "restriction" 
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                        , namePrefix = Just "xsd" 
                        } 
                      , elementAttributes = 
                        [ 
                         ( Name 
                           { nameLocalName = "base" 
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           } 
                         , [ ContentText "xsd:nonNegativeInteger" ]
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
                  , namePrefix = Just "xsd" 
                  } 
                , elementAttributes = 
                  [ 
                   ( Name 
                     { nameLocalName = "name" 
                     , nameNamespace = Nothing
                     , namePrefix = Nothing
                     } 
                   , [ ContentText "InternalDocumentSystemFileNameType" ]
                   ) 
                  ]
                , elementNodes = 
                  [ NodeContent ( ContentText "" )
                  , NodeElement 
                    ( Element 
                      { elementName = Name 
                        { nameLocalName = "annotation" 
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                        , namePrefix = Just "xsd" 
                        } 
                      , elementAttributes = []
                      , elementNodes = 
                        [ NodeContent ( ContentText "" )
                        , NodeElement 
                          ( Element 
                            { elementName = Name 
                              { nameLocalName = "documentation" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = []
                            , elementNodes = 
                              [ NodeContent ( ContentText "" )
                              , NodeElement 
                                ( Element 
                                  { elementName = Name 
                                    { nameLocalName = "Component" 
                                    , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                    , namePrefix = Nothing
                                    } 
                                  , elementAttributes = []
                                  , elementNodes = 
                                    [ NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "DictionaryEntryNm" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "Internal Document System File Name Type" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "MajorVersionNum" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "1" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "MinorVersionNum" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "0" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "VersionEffectiveBeginDt" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "2013-10-31" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "VersionDescriptionTxt" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "Initial Version" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "DescriptionTxt" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "A internal document system file name pattern definition representing 'yyyyMMdd'T'HHmmssSSS'Z." ) ]
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
                        { nameLocalName = "restriction" 
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                        , namePrefix = Just "xsd" 
                        } 
                      , elementAttributes = 
                        [ 
                         ( Name 
                           { nameLocalName = "base" 
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           } 
                         , [ ContentText "xsd:string" ]
                         ) 
                        ]
                      , elementNodes = 
                        [ NodeContent ( ContentText "" )
                        , NodeElement 
                          ( Element 
                            { elementName = Name 
                              { nameLocalName = "pattern" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
                               ( Name 
                                 { nameLocalName = "value" 
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 } 
                               , [ ContentText "(EOM|EOY)_(Request|Response)_(0000[1-9]|000[1-9][0-9]|00[1-9][0-9][0-9]|0[1-9][0-9][0-9][0-9]|[1-9][0-9][0-9][0-9][0-9])_[1-9][0-9]{3}(0[1-9]|1[0-2])(0[1-9]|[1-2][0-9]|3[0-1])T(0[1-9]|1[0-9]|2[0-3])(0[1-9]|[1-5][0-9])(0[1-9]|[1-5][0-9])[0-9]{3}Z_[1-9][0-9]{3}\-.+T[^\.]+(Z|[\+\-].+)_[0-9]{8}\.xml" ]
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
                  , namePrefix = Just "xsd" 
                  } 
                , elementAttributes = 
                  [ 
                   ( Name 
                     { nameLocalName = "name" 
                     , nameNamespace = Nothing
                     , namePrefix = Nothing
                     } 
                   , [ ContentText "MD5Type" ]
                   ) 
                  ]
                , elementNodes = 
                  [ NodeContent ( ContentText "" )
                  , NodeElement 
                    ( Element 
                      { elementName = Name 
                        { nameLocalName = "annotation" 
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                        , namePrefix = Just "xsd" 
                        } 
                      , elementAttributes = []
                      , elementNodes = 
                        [ NodeContent ( ContentText "" )
                        , NodeElement 
                          ( Element 
                            { elementName = Name 
                              { nameLocalName = "documentation" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = []
                            , elementNodes = 
                              [ NodeContent ( ContentText "" )
                              , NodeElement 
                                ( Element 
                                  { elementName = Name 
                                    { nameLocalName = "Component" 
                                    , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                    , namePrefix = Nothing
                                    } 
                                  , elementAttributes = []
                                  , elementNodes = 
                                    [ NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "DictionaryEntryNm" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "MD5 Checksum Type" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "MajorVersionNum" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "1" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "MinorVersionNum" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "0" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "VersionEffectiveBeginDt" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "2013-05-01" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "VersionDescriptionTxt" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "Initial Version" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "DescriptionTxt" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "The MD5 Checksum Type (64 character hex value)." ) ]
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
                        { nameLocalName = "restriction" 
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                        , namePrefix = Just "xsd" 
                        } 
                      , elementAttributes = 
                        [ 
                         ( Name 
                           { nameLocalName = "base" 
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           } 
                         , [ ContentText "xsd:string" ]
                         ) 
                        ]
                      , elementNodes = 
                        [ NodeContent ( ContentText "" )
                        , NodeElement 
                          ( Element 
                            { elementName = Name 
                              { nameLocalName = "pattern" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
                               ( Name 
                                 { nameLocalName = "value" 
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 } 
                               , [ ContentText "[0-9A-Fa-f]{64}" ]
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
                  , namePrefix = Just "xsd" 
                  } 
                , elementAttributes = 
                  [ 
                   ( Name 
                     { nameLocalName = "name" 
                     , nameNamespace = Nothing
                     , namePrefix = Nothing
                     } 
                   , [ ContentText "SHA256Type" ]
                   ) 
                  ]
                , elementNodes = 
                  [ NodeContent ( ContentText "" )
                  , NodeElement 
                    ( Element 
                      { elementName = Name 
                        { nameLocalName = "annotation" 
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                        , namePrefix = Just "xsd" 
                        } 
                      , elementAttributes = []
                      , elementNodes = 
                        [ NodeContent ( ContentText "" )
                        , NodeElement 
                          ( Element 
                            { elementName = Name 
                              { nameLocalName = "documentation" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = []
                            , elementNodes = 
                              [ NodeContent ( ContentText "" )
                              , NodeElement 
                                ( Element 
                                  { elementName = Name 
                                    { nameLocalName = "Component" 
                                    , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                    , namePrefix = Nothing
                                    } 
                                  , elementAttributes = []
                                  , elementNodes = 
                                    [ NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "DictionaryEntryNm" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "SHA256 Checksum Type" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "MajorVersionNum" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "1" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "MinorVersionNum" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "0" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "VersionEffectiveBeginDt" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "2017-05-01" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "VersionDescriptionTxt" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "Initial Version" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "DescriptionTxt" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "The SHA256 Checksum Type 256-bit signature hex key value." ) ]
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
                        { nameLocalName = "restriction" 
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                        , namePrefix = Just "xsd" 
                        } 
                      , elementAttributes = 
                        [ 
                         ( Name 
                           { nameLocalName = "base" 
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           } 
                         , [ ContentText "xsd:string" ]
                         ) 
                        ]
                      , elementNodes = 
                        [ NodeContent ( ContentText "" )
                        , NodeElement 
                          ( Element 
                            { elementName = Name 
                              { nameLocalName = "pattern" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
                               ( Name 
                                 { nameLocalName = "value" 
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 } 
                               , [ ContentText "[0-9A-Fa-f]{64}" ]
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
                  , namePrefix = Just "xsd" 
                  } 
                , elementAttributes = 
                  [ 
                   ( Name 
                     { nameLocalName = "name" 
                     , nameNamespace = Nothing
                     , namePrefix = Nothing
                     } 
                   , [ ContentText "MonthNumberType" ]
                   ) 
                  ]
                , elementNodes = 
                  [ NodeContent ( ContentText "" )
                  , NodeElement 
                    ( Element 
                      { elementName = Name 
                        { nameLocalName = "annotation" 
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                        , namePrefix = Just "xsd" 
                        } 
                      , elementAttributes = []
                      , elementNodes = 
                        [ NodeContent ( ContentText "" )
                        , NodeElement 
                          ( Element 
                            { elementName = Name 
                              { nameLocalName = "documentation" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = []
                            , elementNodes = 
                              [ NodeContent ( ContentText "" )
                              , NodeElement 
                                ( Element 
                                  { elementName = Name 
                                    { nameLocalName = "Component" 
                                    , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                    , namePrefix = Nothing
                                    } 
                                  , elementAttributes = []
                                  , elementNodes = 
                                    [ NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "DictionaryEntryNm" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "Month Number Type" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "MajorVersionNum" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "1" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "MinorVersionNum" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "0" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "VersionEffectiveBeginDt" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "2013-03-05" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "VersionDescriptionTxt" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "Inital Version" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "DescriptionTxt" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "Represents the month number in a year.  Range (01-12)" ) ]
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
                        { nameLocalName = "restriction" 
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                        , namePrefix = Just "xsd" 
                        } 
                      , elementAttributes = 
                        [ 
                         ( Name 
                           { nameLocalName = "base" 
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           } 
                         , [ ContentText "IntegerNNType" ]
                         ) 
                        ]
                      , elementNodes = 
                        [ NodeContent ( ContentText "" )
                        , NodeElement 
                          ( Element 
                            { elementName = Name 
                              { nameLocalName = "minInclusive" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
                               ( Name 
                                 { nameLocalName = "value" 
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 } 
                               , [ ContentText "01" ]
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
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
                               ( Name 
                                 { nameLocalName = "value" 
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 } 
                               , [ ContentText "12" ]
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
                  , namePrefix = Just "xsd" 
                  } 
                , elementAttributes = 
                  [ 
                   ( Name 
                     { nameLocalName = "name" 
                     , nameNamespace = Nothing
                     , namePrefix = Nothing
                     } 
                   , [ ContentText "NameControlTypeCodeType" ]
                   ) 
                  ]
                , elementNodes = 
                  [ NodeContent ( ContentText "" )
                  , NodeElement 
                    ( Element 
                      { elementName = Name 
                        { nameLocalName = "annotation" 
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                        , namePrefix = Just "xsd" 
                        } 
                      , elementAttributes = []
                      , elementNodes = 
                        [ NodeContent ( ContentText "" )
                        , NodeElement 
                          ( Element 
                            { elementName = Name 
                              { nameLocalName = "documentation" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = []
                            , elementNodes = 
                              [ NodeContent ( ContentText "" )
                              , NodeElement 
                                ( Element 
                                  { elementName = Name 
                                    { nameLocalName = "Component" 
                                    , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                    , namePrefix = Nothing
                                    } 
                                  , elementAttributes = []
                                  , elementNodes = 
                                    [ NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "DictionaryEntryNm" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "Name Control Type Code" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "MajorVersionNum" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "1" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "MinorVersionNum" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "0" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "VersionEffectiveBeginDt" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "2012-11-05" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "VersionDescriptionTxt" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "Initial version" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "DescriptionTxt" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "A code used to identify the name control code type." ) ]
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
                        { nameLocalName = "restriction" 
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                        , namePrefix = Just "xsd" 
                        } 
                      , elementAttributes = 
                        [ 
                         ( Name 
                           { nameLocalName = "base" 
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           } 
                         , [ ContentText "xsd:string" ]
                         ) 
                        ]
                      , elementNodes = 
                        [ NodeContent ( ContentText "" )
                        , NodeElement 
                          ( Element 
                            { elementName = Name 
                              { nameLocalName = "enumeration" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
                               ( Name 
                                 { nameLocalName = "value" 
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 } 
                               , [ ContentText "CURRENT" ]
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
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
                               ( Name 
                                 { nameLocalName = "value" 
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 } 
                               , [ ContentText "PAST" ]
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
                  , namePrefix = Just "xsd" 
                  } 
                , elementAttributes = 
                  [ 
                   ( Name 
                     { nameLocalName = "name" 
                     , nameNamespace = Nothing
                     , namePrefix = Nothing
                     } 
                   , [ ContentText "NameControlValResultCodeType" ]
                   ) 
                  ]
                , elementNodes = 
                  [ NodeContent ( ContentText "" )
                  , NodeElement 
                    ( Element 
                      { elementName = Name 
                        { nameLocalName = "annotation" 
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                        , namePrefix = Just "xsd" 
                        } 
                      , elementAttributes = []
                      , elementNodes = 
                        [ NodeContent ( ContentText "" )
                        , NodeElement 
                          ( Element 
                            { elementName = Name 
                              { nameLocalName = "documentation" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = []
                            , elementNodes = 
                              [ NodeContent ( ContentText "" )
                              , NodeElement 
                                ( Element 
                                  { elementName = Name 
                                    { nameLocalName = "Component" 
                                    , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                    , namePrefix = Nothing
                                    } 
                                  , elementAttributes = []
                                  , elementNodes = 
                                    [ NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "DictionaryEntryNm" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "Name Control Validation Result Code Type" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "MajorVersionNum" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "1" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "MinorVersionNum" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "0" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "VersionEffectiveBeginDt" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "2012-11-05" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "VersionDescriptionTxt" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "Initial version" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "DescriptionTxt" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "A code used to identify the Name Control Validation Result Code type." ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "DataElementId" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
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
                        { nameLocalName = "restriction" 
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                        , namePrefix = Just "xsd" 
                        } 
                      , elementAttributes = 
                        [ 
                         ( Name 
                           { nameLocalName = "base" 
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           } 
                         , [ ContentText "xsd:string" ]
                         ) 
                        ]
                      , elementNodes = 
                        [ NodeContent ( ContentText "" )
                        , NodeElement 
                          ( Element 
                            { elementName = Name 
                              { nameLocalName = "enumeration" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
                               ( Name 
                                 { nameLocalName = "value" 
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 } 
                               , [ ContentText "EXACT_MATCH" ]
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
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
                               ( Name 
                                 { nameLocalName = "value" 
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 } 
                               , [ ContentText "PROXIMAL_MATCH" ]
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
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
                               ( Name 
                                 { nameLocalName = "value" 
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 } 
                               , [ ContentText "NO_MATCH" ]
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
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
                               ( Name 
                                 { nameLocalName = "value" 
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 } 
                               , [ ContentText "TIN_NOT_FOUND" ]
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
                  , namePrefix = Just "xsd" 
                  } 
                , elementAttributes = 
                  [ 
                   ( Name 
                     { nameLocalName = "name" 
                     , nameNamespace = Nothing
                     , namePrefix = Nothing
                     } 
                   , [ ContentText "NullBooleanType" ]
                   ) 
                  ]
                , elementNodes = 
                  [ NodeContent ( ContentText "" )
                  , NodeElement 
                    ( Element 
                      { elementName = Name 
                        { nameLocalName = "annotation" 
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                        , namePrefix = Just "xsd" 
                        } 
                      , elementAttributes = []
                      , elementNodes = 
                        [ NodeContent ( ContentText "" )
                        , NodeElement 
                          ( Element 
                            { elementName = Name 
                              { nameLocalName = "documentation" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = []
                            , elementNodes = 
                              [ NodeContent ( ContentText "" )
                              , NodeElement 
                                ( Element 
                                  { elementName = Name 
                                    { nameLocalName = "Component" 
                                    , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                    , namePrefix = Nothing
                                    } 
                                  , elementAttributes = []
                                  , elementNodes = 
                                    [ NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "DictionaryEntryNm" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "Null Boolean Type" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "MajorVersionNum" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "1" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "MinorVersionNum" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "0" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "VersionEffectiveBeginDt" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "2013-05-01" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "VersionDescriptionTxt" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "Initial Version" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "DescriptionTxt" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "Type for nullable boolean values" ) ]
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
                        { nameLocalName = "restriction" 
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                        , namePrefix = Just "xsd" 
                        } 
                      , elementAttributes = 
                        [ 
                         ( Name 
                           { nameLocalName = "base" 
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           } 
                         , [ ContentText "xsd:string" ]
                         ) 
                        ]
                      , elementNodes = 
                        [ NodeContent ( ContentText "" )
                        , NodeElement 
                          ( Element 
                            { elementName = Name 
                              { nameLocalName = "enumeration" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
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
                              , namePrefix = Just "xsd" 
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
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
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
                        , NodeElement 
                          ( Element 
                            { elementName = Name 
                              { nameLocalName = "enumeration" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
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
                        , NodeElement 
                          ( Element 
                            { elementName = Name 
                              { nameLocalName = "enumeration" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
                               ( Name 
                                 { nameLocalName = "value" 
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 } 
                               , [ ContentText "null" ]
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
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
                               ( Name 
                                 { nameLocalName = "value" 
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 } 
                               , []
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
                  , namePrefix = Just "xsd" 
                  } 
                , elementAttributes = 
                  [ 
                   ( Name 
                     { nameLocalName = "name" 
                     , nameNamespace = Nothing
                     , namePrefix = Nothing
                     } 
                   , [ ContentText "Percentage100Type" ]
                   ) 
                  ]
                , elementNodes = 
                  [ NodeContent ( ContentText "" )
                  , NodeElement 
                    ( Element 
                      { elementName = Name 
                        { nameLocalName = "annotation" 
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                        , namePrefix = Just "xsd" 
                        } 
                      , elementAttributes = []
                      , elementNodes = 
                        [ NodeContent ( ContentText "" )
                        , NodeElement 
                          ( Element 
                            { elementName = Name 
                              { nameLocalName = "documentation" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = []
                            , elementNodes = 
                              [ NodeContent ( ContentText "" )
                              , NodeElement 
                                ( Element 
                                  { elementName = Name 
                                    { nameLocalName = "Component" 
                                    , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                    , namePrefix = Nothing
                                    } 
                                  , elementAttributes = []
                                  , elementNodes = 
                                    [ NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "DictionaryEntryNm" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "Percentage 100 Type" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "MajorVersionNum" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "1" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "MinorVersionNum" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "0" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "VersionEffectiveBeginDt" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "2013-03-25" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "VersionDescriptionTxt" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "Initial version" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "DescriptionTxt" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "Base type for a percentage formatted as 5 integers and 2 decimals" ) ]
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
                        { nameLocalName = "restriction" 
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                        , namePrefix = Just "xsd" 
                        } 
                      , elementAttributes = 
                        [ 
                         ( Name 
                           { nameLocalName = "base" 
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           } 
                         , [ ContentText "xsd:decimal" ]
                         ) 
                        ]
                      , elementNodes = 
                        [ NodeContent ( ContentText "" )
                        , NodeElement 
                          ( Element 
                            { elementName = Name 
                              { nameLocalName = "totalDigits" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
                               ( Name 
                                 { nameLocalName = "value" 
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 } 
                               , [ ContentText "5" ]
                               ) 
                              ]
                            , elementNodes = []
                            } 
                          )
                        , NodeContent ( ContentText "" )
                        , NodeElement 
                          ( Element 
                            { elementName = Name 
                              { nameLocalName = "fractionDigits" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
                               ( Name 
                                 { nameLocalName = "value" 
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 } 
                               , [ ContentText "2" ]
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
                  , namePrefix = Just "xsd" 
                  } 
                , elementAttributes = 
                  [ 
                   ( Name 
                     { nameLocalName = "name" 
                     , nameNamespace = Nothing
                     , namePrefix = Nothing
                     } 
                   , [ ContentText "PersonNameControlType" ]
                   ) 
                  ]
                , elementNodes = 
                  [ NodeContent ( ContentText "" )
                  , NodeElement 
                    ( Element 
                      { elementName = Name 
                        { nameLocalName = "annotation" 
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                        , namePrefix = Just "xsd" 
                        } 
                      , elementAttributes = []
                      , elementNodes = 
                        [ NodeContent ( ContentText "" )
                        , NodeElement 
                          ( Element 
                            { elementName = Name 
                              { nameLocalName = "documentation" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = []
                            , elementNodes = 
                              [ NodeContent ( ContentText "" )
                              , NodeElement 
                                ( Element 
                                  { elementName = Name 
                                    { nameLocalName = "Component" 
                                    , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                    , namePrefix = Nothing
                                    } 
                                  , elementAttributes = []
                                  , elementNodes = 
                                    [ NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "DictionaryEntryNm" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "Person Name Control Type" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "MajorVersionNum" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "2" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "MinorVersionNum" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "0" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "VersionEffectiveBeginDt" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "2009-03-16" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "VersionDescriptionTxt" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "Initial version" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "DescriptionTxt" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "4 characters generated as a person's name control" ) ]
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
                        { nameLocalName = "restriction" 
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                        , namePrefix = Just "xsd" 
                        } 
                      , elementAttributes = 
                        [ 
                         ( Name 
                           { nameLocalName = "base" 
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           } 
                         , [ ContentText "xsd:string" ]
                         ) 
                        ]
                      , elementNodes = 
                        [ NodeContent ( ContentText "" )
                        , NodeElement 
                          ( Element 
                            { elementName = Name 
                              { nameLocalName = "length" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
                               ( Name 
                                 { nameLocalName = "value" 
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 } 
                               , [ ContentText "4" ]
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
                  , namePrefix = Just "xsd" 
                  } 
                , elementAttributes = 
                  [ 
                   ( Name 
                     { nameLocalName = "name" 
                     , nameNamespace = Nothing
                     , namePrefix = Nothing
                     } 
                   , [ ContentText "QHPIssuerIdType" ]
                   ) 
                  ]
                , elementNodes = 
                  [ NodeContent ( ContentText "" )
                  , NodeElement 
                    ( Element 
                      { elementName = Name 
                        { nameLocalName = "annotation" 
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                        , namePrefix = Just "xsd" 
                        } 
                      , elementAttributes = []
                      , elementNodes = 
                        [ NodeContent ( ContentText "" )
                        , NodeElement 
                          ( Element 
                            { elementName = Name 
                              { nameLocalName = "documentation" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = []
                            , elementNodes = 
                              [ NodeContent ( ContentText "" )
                              , NodeElement 
                                ( Element 
                                  { elementName = Name 
                                    { nameLocalName = "Component" 
                                    , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                    , namePrefix = Nothing
                                    } 
                                  , elementAttributes = []
                                  , elementNodes = 
                                    [ NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "DictionaryEntryNm" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "QHP Issuer Identification Type" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "MajorVersionNum" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "1" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "MinorVersionNum" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "0" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "VersionEffectiveBeginDt" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "2013-01-25" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "VersionDescriptionTxt" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "Initial version" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "DescriptionTxt" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "Base type for a 5 character formatted number." ) ]
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
                        { nameLocalName = "restriction" 
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                        , namePrefix = Just "xsd" 
                        } 
                      , elementAttributes = 
                        [ 
                         ( Name 
                           { nameLocalName = "base" 
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           } 
                         , [ ContentText "xsd:integer" ]
                         ) 
                        ]
                      , elementNodes = 
                        [ NodeContent ( ContentText "" )
                        , NodeElement 
                          ( Element 
                            { elementName = Name 
                              { nameLocalName = "pattern" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
                               ( Name 
                                 { nameLocalName = "value" 
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 } 
                               , [ ContentText "[0-9]{5}" ]
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
                  , namePrefix = Just "xsd" 
                  } 
                , elementAttributes = 
                  [ 
                   ( Name 
                     { nameLocalName = "name" 
                     , nameNamespace = Nothing
                     , namePrefix = Nothing
                     } 
                   , [ ContentText "RequestIdType" ]
                   ) 
                  ]
                , elementNodes = 
                  [ NodeContent ( ContentText "" )
                  , NodeElement 
                    ( Element 
                      { elementName = Name 
                        { nameLocalName = "annotation" 
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                        , namePrefix = Just "xsd" 
                        } 
                      , elementAttributes = []
                      , elementNodes = 
                        [ NodeContent ( ContentText "" )
                        , NodeElement 
                          ( Element 
                            { elementName = Name 
                              { nameLocalName = "documentation" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = []
                            , elementNodes = 
                              [ NodeContent ( ContentText "" )
                              , NodeElement 
                                ( Element 
                                  { elementName = Name 
                                    { nameLocalName = "Component" 
                                    , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                    , namePrefix = Nothing
                                    } 
                                  , elementAttributes = []
                                  , elementNodes = 
                                    [ NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "DictionaryEntryNm" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "Request Id Type" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "MajorVersionNum" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "1" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "MinorVersionNum" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "0" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "VersionEffectiveBeginDt" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "2015-02-11" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "VersionDescriptionTxt" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "Initial version" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "DescriptionTxt" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "Base type for a social security number - 9 digits" ) ]
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
                        { nameLocalName = "restriction" 
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                        , namePrefix = Just "xsd" 
                        } 
                      , elementAttributes = 
                        [ 
                         ( Name 
                           { nameLocalName = "base" 
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           } 
                         , [ ContentText "xsd:string" ]
                         ) 
                        ]
                      , elementNodes = 
                        [ NodeContent ( ContentText "" )
                        , NodeElement 
                          ( Element 
                            { elementName = Name 
                              { nameLocalName = "pattern" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
                               ( Name 
                                 { nameLocalName = "value" 
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 } 
                               , [ ContentText "([0-9]{1,9})" ]
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
                  , namePrefix = Just "xsd" 
                  } 
                , elementAttributes = 
                  [ 
                   ( Name 
                     { nameLocalName = "name" 
                     , nameNamespace = Nothing
                     , namePrefix = Nothing
                     } 
                   , [ ContentText "SequenceNumberType" ]
                   ) 
                  ]
                , elementNodes = 
                  [ NodeContent ( ContentText "" )
                  , NodeElement 
                    ( Element 
                      { elementName = Name 
                        { nameLocalName = "annotation" 
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                        , namePrefix = Just "xsd" 
                        } 
                      , elementAttributes = []
                      , elementNodes = 
                        [ NodeContent ( ContentText "" )
                        , NodeElement 
                          ( Element 
                            { elementName = Name 
                              { nameLocalName = "documentation" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = []
                            , elementNodes = 
                              [ NodeContent ( ContentText "" )
                              , NodeElement 
                                ( Element 
                                  { elementName = Name 
                                    { nameLocalName = "Component" 
                                    , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                    , namePrefix = Nothing
                                    } 
                                  , elementAttributes = []
                                  , elementNodes = 
                                    [ NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "DictionaryEntryNm" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "Sequence Number Type" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "MajorVersionNum" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "1" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "MinorVersionNum" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "0" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "VersionEffectiveBeginDt" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "2013-10-30" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "VersionDescriptionTxt" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "ACA" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "DescriptionTxt" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "Type for a Sequence Number" ) ]
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
                        { nameLocalName = "restriction" 
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                        , namePrefix = Just "xsd" 
                        } 
                      , elementAttributes = 
                        [ 
                         ( Name 
                           { nameLocalName = "base" 
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           } 
                         , [ ContentText "xsd:integer" ]
                         ) 
                        ]
                      , elementNodes = 
                        [ NodeContent ( ContentText "" )
                        , NodeElement 
                          ( Element 
                            { elementName = Name 
                              { nameLocalName = "totalDigits" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
                               ( Name 
                                 { nameLocalName = "value" 
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 } 
                               , [ ContentText "10" ]
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
                  , namePrefix = Just "xsd" 
                  } 
                , elementAttributes = 
                  [ 
                   ( Name 
                     { nameLocalName = "name" 
                     , nameNamespace = Nothing
                     , namePrefix = Nothing
                     } 
                   , [ ContentText "SSNType" ]
                   ) 
                  ]
                , elementNodes = 
                  [ NodeContent ( ContentText "" )
                  , NodeElement 
                    ( Element 
                      { elementName = Name 
                        { nameLocalName = "annotation" 
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                        , namePrefix = Just "xsd" 
                        } 
                      , elementAttributes = []
                      , elementNodes = 
                        [ NodeContent ( ContentText "" )
                        , NodeElement 
                          ( Element 
                            { elementName = Name 
                              { nameLocalName = "documentation" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = []
                            , elementNodes = 
                              [ NodeContent ( ContentText "" )
                              , NodeElement 
                                ( Element 
                                  { elementName = Name 
                                    { nameLocalName = "Component" 
                                    , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                    , namePrefix = Nothing
                                    } 
                                  , elementAttributes = []
                                  , elementNodes = 
                                    [ NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "DictionaryEntryNm" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "SSN Type" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "MajorVersionNum" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "1" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "MinorVersionNum" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "0" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "VersionEffectiveBeginDt" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "2008-01-08" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "VersionDescriptionTxt" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "Initial version" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "DescriptionTxt" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "Base type for a social security number - 9 digits" ) ]
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
                        { nameLocalName = "restriction" 
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                        , namePrefix = Just "xsd" 
                        } 
                      , elementAttributes = 
                        [ 
                         ( Name 
                           { nameLocalName = "base" 
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           } 
                         , [ ContentText "xsd:string" ]
                         ) 
                        ]
                      , elementNodes = 
                        [ NodeContent ( ContentText "" )
                        , NodeElement 
                          ( Element 
                            { elementName = Name 
                              { nameLocalName = "pattern" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
                               ( Name 
                                 { nameLocalName = "value" 
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 } 
                               , [ ContentText "[0-9]{9}" ]
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
                  , namePrefix = Just "xsd" 
                  } 
                , elementAttributes = 
                  [ 
                   ( Name 
                     { nameLocalName = "name" 
                     , nameNamespace = Nothing
                     , namePrefix = Nothing
                     } 
                   , [ ContentText "String16Type" ]
                   ) 
                  ]
                , elementNodes = 
                  [ NodeContent ( ContentText "" )
                  , NodeElement 
                    ( Element 
                      { elementName = Name 
                        { nameLocalName = "annotation" 
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                        , namePrefix = Just "xsd" 
                        } 
                      , elementAttributes = []
                      , elementNodes = 
                        [ NodeContent ( ContentText "" )
                        , NodeElement 
                          ( Element 
                            { elementName = Name 
                              { nameLocalName = "documentation" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = []
                            , elementNodes = 
                              [ NodeContent ( ContentText "" )
                              , NodeElement 
                                ( Element 
                                  { elementName = Name 
                                    { nameLocalName = "Component" 
                                    , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                    , namePrefix = Nothing
                                    } 
                                  , elementAttributes = []
                                  , elementNodes = 
                                    [ NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "DictionaryEntryNm" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "String 16 Text Type" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "MajorVersionNum" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "1" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "MinorVersionNum" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "0" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "VersionEffectiveBeginDt" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "2013-05-17" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "VersionDescriptionTxt" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "Initial version" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "DescriptionTxt" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "A string limited to only 16 characters" ) ]
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
                        { nameLocalName = "restriction" 
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                        , namePrefix = Just "xsd" 
                        } 
                      , elementAttributes = 
                        [ 
                         ( Name 
                           { nameLocalName = "base" 
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           } 
                         , [ ContentText "xsd:string" ]
                         ) 
                        ]
                      , elementNodes = 
                        [ NodeContent ( ContentText "" )
                        , NodeElement 
                          ( Element 
                            { elementName = Name 
                              { nameLocalName = "maxLength" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
                               ( Name 
                                 { nameLocalName = "value" 
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 } 
                               , [ ContentText "16" ]
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
                  , namePrefix = Just "xsd" 
                  } 
                , elementAttributes = 
                  [ 
                   ( Name 
                     { nameLocalName = "name" 
                     , nameNamespace = Nothing
                     , namePrefix = Nothing
                     } 
                   , [ ContentText "String50Type" ]
                   ) 
                  ]
                , elementNodes = 
                  [ NodeContent ( ContentText "" )
                  , NodeElement 
                    ( Element 
                      { elementName = Name 
                        { nameLocalName = "annotation" 
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                        , namePrefix = Just "xsd" 
                        } 
                      , elementAttributes = []
                      , elementNodes = 
                        [ NodeContent ( ContentText "" )
                        , NodeElement 
                          ( Element 
                            { elementName = Name 
                              { nameLocalName = "documentation" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = []
                            , elementNodes = 
                              [ NodeContent ( ContentText "" )
                              , NodeElement 
                                ( Element 
                                  { elementName = Name 
                                    { nameLocalName = "Component" 
                                    , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                    , namePrefix = Nothing
                                    } 
                                  , elementAttributes = []
                                  , elementNodes = 
                                    [ NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "DictionaryEntryNm" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "String 50 Text Type" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "MajorVersionNum" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "1" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "MinorVersionNum" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "0" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "VersionEffectiveBeginDt" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "2013-04-15" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "VersionDescriptionTxt" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "Initial version" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "DescriptionTxt" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "A string limited to only 50 characters" ) ]
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
                        { nameLocalName = "restriction" 
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                        , namePrefix = Just "xsd" 
                        } 
                      , elementAttributes = 
                        [ 
                         ( Name 
                           { nameLocalName = "base" 
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           } 
                         , [ ContentText "xsd:string" ]
                         ) 
                        ]
                      , elementNodes = 
                        [ NodeContent ( ContentText "" )
                        , NodeElement 
                          ( Element 
                            { elementName = Name 
                              { nameLocalName = "maxLength" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
                               ( Name 
                                 { nameLocalName = "value" 
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 } 
                               , [ ContentText "50" ]
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
                  , namePrefix = Just "xsd" 
                  } 
                , elementAttributes = 
                  [ 
                   ( Name 
                     { nameLocalName = "name" 
                     , nameNamespace = Nothing
                     , namePrefix = Nothing
                     } 
                   , [ ContentText "SubmissionStatusCodeType" ]
                   ) 
                  ]
                , elementNodes = 
                  [ NodeContent ( ContentText "" )
                  , NodeElement 
                    ( Element 
                      { elementName = Name 
                        { nameLocalName = "annotation" 
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                        , namePrefix = Just "xsd" 
                        } 
                      , elementAttributes = []
                      , elementNodes = 
                        [ NodeContent ( ContentText "" )
                        , NodeElement 
                          ( Element 
                            { elementName = Name 
                              { nameLocalName = "documentation" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = []
                            , elementNodes = 
                              [ NodeContent ( ContentText "" )
                              , NodeElement 
                                ( Element 
                                  { elementName = Name 
                                    { nameLocalName = "Component" 
                                    , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                    , namePrefix = Nothing
                                    } 
                                  , elementAttributes = []
                                  , elementNodes = 
                                    [ NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "DictionaryEntryNm" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "Submissions Status Type" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "MajorVersionNum" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "2" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "MinorVersionNum" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "0" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "VersionEffectiveBeginDt" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "2012-09-01" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "VersionDescriptionTxt" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "Initial version" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "DescriptionTxt" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "The form(s) submission status type code" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "DataElementId" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "" ) ]
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
                        { nameLocalName = "restriction" 
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                        , namePrefix = Just "xsd" 
                        } 
                      , elementAttributes = 
                        [ 
                         ( Name 
                           { nameLocalName = "base" 
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           } 
                         , [ ContentText "xsd:string" ]
                         ) 
                        ]
                      , elementNodes = 
                        [ NodeContent ( ContentText "" )
                        , NodeElement 
                          ( Element 
                            { elementName = Name 
                              { nameLocalName = "enumeration" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
                               ( Name 
                                 { nameLocalName = "value" 
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 } 
                               , [ ContentText "Accepted" ]
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
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
                               ( Name 
                                 { nameLocalName = "value" 
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 } 
                               , [ ContentText "Accepted with Errors" ]
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
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
                               ( Name 
                                 { nameLocalName = "value" 
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 } 
                               , [ ContentText "Rejected" ]
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
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
                               ( Name 
                                 { nameLocalName = "value" 
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 } 
                               , [ ContentText "Processing" ]
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
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
                               ( Name 
                                 { nameLocalName = "value" 
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 } 
                               , [ ContentText "Not Found" ]
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
                  , namePrefix = Just "xsd" 
                  } 
                , elementAttributes = 
                  [ 
                   ( Name 
                     { nameLocalName = "name" 
                     , nameNamespace = Nothing
                     , namePrefix = Nothing
                     } 
                   , [ ContentText "SubmissionValidationCdType" ]
                   ) 
                  ]
                , elementNodes = 
                  [ NodeContent ( ContentText "" )
                  , NodeElement 
                    ( Element 
                      { elementName = Name 
                        { nameLocalName = "annotation" 
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                        , namePrefix = Just "xsd" 
                        } 
                      , elementAttributes = []
                      , elementNodes = 
                        [ NodeContent ( ContentText "" )
                        , NodeElement 
                          ( Element 
                            { elementName = Name 
                              { nameLocalName = "documentation" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = []
                            , elementNodes = 
                              [ NodeContent ( ContentText "" )
                              , NodeElement 
                                ( Element 
                                  { elementName = Name 
                                    { nameLocalName = "Component" 
                                    , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                    , namePrefix = Nothing
                                    } 
                                  , elementAttributes = []
                                  , elementNodes = 
                                    [ NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "DictionaryEntryNm" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "Submissions Validation Code Type" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "MajorVersionNum" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "1" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "MinorVersionNum" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "0" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "VersionEffectiveBeginDt" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "2012-10-29" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "VersionDescriptionTxt" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "Initial version" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "DescriptionTxt" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "The form(s) submission validation status type code" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "DataElementId" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "" ) ]
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
                        { nameLocalName = "restriction" 
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                        , namePrefix = Just "xsd" 
                        } 
                      , elementAttributes = 
                        [ 
                         ( Name 
                           { nameLocalName = "base" 
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           } 
                         , [ ContentText "xsd:string" ]
                         ) 
                        ]
                      , elementNodes = 
                        [ NodeContent ( ContentText "" )
                        , NodeElement 
                          ( Element 
                            { elementName = Name 
                              { nameLocalName = "enumeration" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
                               ( Name 
                                 { nameLocalName = "value" 
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 } 
                               , [ ContentText "Valid" ]
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
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
                               ( Name 
                                 { nameLocalName = "value" 
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 } 
                               , [ ContentText "Invalid" ]
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
                  , namePrefix = Just "xsd" 
                  } 
                , elementAttributes = 
                  [ 
                   ( Name 
                     { nameLocalName = "name" 
                     , nameNamespace = Nothing
                     , namePrefix = Nothing
                     } 
                   , [ ContentText "SystemIdType" ]
                   ) 
                  ]
                , elementNodes = 
                  [ NodeContent ( ContentText "" )
                  , NodeElement 
                    ( Element 
                      { elementName = Name 
                        { nameLocalName = "annotation" 
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                        , namePrefix = Just "xsd" 
                        } 
                      , elementAttributes = []
                      , elementNodes = 
                        [ NodeContent ( ContentText "" )
                        , NodeElement 
                          ( Element 
                            { elementName = Name 
                              { nameLocalName = "documentation" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = []
                            , elementNodes = 
                              [ NodeContent ( ContentText "" )
                              , NodeElement 
                                ( Element 
                                  { elementName = Name 
                                    { nameLocalName = "Component" 
                                    , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                    , namePrefix = Nothing
                                    } 
                                  , elementAttributes = []
                                  , elementNodes = 
                                    [ NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "DictionaryEntryNm" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "System Id Type" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "MajorVersionNum" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "1" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "MinorVersionNum" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "0" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "VersionEffectiveBeginDt" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "2005-06-06" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "VersionDescriptionTxt" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "ELDM 3.0" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "DescriptionTxt" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "Type for a System Id" ) ]
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
                        { nameLocalName = "restriction" 
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                        , namePrefix = Just "xsd" 
                        } 
                      , elementAttributes = 
                        [ 
                         ( Name 
                           { nameLocalName = "base" 
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           } 
                         , [ ContentText "xsd:string" ]
                         ) 
                        ]
                      , elementNodes = 
                        [ NodeContent ( ContentText "" )
                        , NodeElement 
                          ( Element 
                            { elementName = Name 
                              { nameLocalName = "maxLength" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
                               ( Name 
                                 { nameLocalName = "value" 
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 } 
                               , [ ContentText "16" ]
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
                  , namePrefix = Just "xsd" 
                  } 
                , elementAttributes = 
                  [ 
                   ( Name 
                     { nameLocalName = "name" 
                     , nameNamespace = Nothing
                     , namePrefix = Nothing
                     } 
                   , [ ContentText "TaxpayerValidityCodeType" ]
                   ) 
                  ]
                , elementNodes = 
                  [ NodeContent ( ContentText "" )
                  , NodeElement 
                    ( Element 
                      { elementName = Name 
                        { nameLocalName = "annotation" 
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                        , namePrefix = Just "xsd" 
                        } 
                      , elementAttributes = []
                      , elementNodes = 
                        [ NodeContent ( ContentText "" )
                        , NodeElement 
                          ( Element 
                            { elementName = Name 
                              { nameLocalName = "documentation" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = []
                            , elementNodes = 
                              [ NodeContent ( ContentText "" )
                              , NodeElement 
                                ( Element 
                                  { elementName = Name 
                                    { nameLocalName = "Component" 
                                    , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                    , namePrefix = Nothing
                                    } 
                                  , elementAttributes = []
                                  , elementNodes = 
                                    [ NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "DictionaryEntryNm" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "Taxpayer Validity Code Type" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "MajorVersionNum" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "1" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "MinorVersionNum" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "0" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "VersionEffectiveBeginDt" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "2008-06-06" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "VersionDescriptionTxt" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "AMS Questions - newly mapped to ELDM" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "DescriptionTxt" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "Indicates the validity status of the TIN" ) ]
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
                        { nameLocalName = "restriction" 
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                        , namePrefix = Just "xsd" 
                        } 
                      , elementAttributes = 
                        [ 
                         ( Name 
                           { nameLocalName = "base" 
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           } 
                         , [ ContentText "xsd:integer" ]
                         ) 
                        ]
                      , elementNodes = 
                        [ NodeContent ( ContentText "" )
                        , NodeElement 
                          ( Element 
                            { elementName = Name 
                              { nameLocalName = "enumeration" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
                               ( Name 
                                 { nameLocalName = "value" 
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 } 
                               , [ ContentText "0" ]
                               ) 
                              ]
                            , elementNodes = 
                              [ NodeContent ( ContentText "" )
                              , NodeElement 
                                ( Element 
                                  { elementName = Name 
                                    { nameLocalName = "annotation" 
                                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                    , namePrefix = Just "xsd" 
                                    } 
                                  , elementAttributes = []
                                  , elementNodes = 
                                    [ NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "documentation" 
                                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                          , namePrefix = Just "xsd" 
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "SSN - VALID" ) ]
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
                              { nameLocalName = "enumeration" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
                               ( Name 
                                 { nameLocalName = "value" 
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 } 
                               , [ ContentText "1" ]
                               ) 
                              ]
                            , elementNodes = 
                              [ NodeContent ( ContentText "" )
                              , NodeElement 
                                ( Element 
                                  { elementName = Name 
                                    { nameLocalName = "annotation" 
                                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                    , namePrefix = Just "xsd" 
                                    } 
                                  , elementAttributes = []
                                  , elementNodes = 
                                    [ NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "documentation" 
                                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                          , namePrefix = Just "xsd" 
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "SSN - INVALID" ) ]
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
                              { nameLocalName = "enumeration" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
                               ( Name 
                                 { nameLocalName = "value" 
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 } 
                               , [ ContentText "2" ]
                               ) 
                              ]
                            , elementNodes = 
                              [ NodeContent ( ContentText "" )
                              , NodeElement 
                                ( Element 
                                  { elementName = Name 
                                    { nameLocalName = "annotation" 
                                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                    , namePrefix = Just "xsd" 
                                    } 
                                  , elementAttributes = []
                                  , elementNodes = 
                                    [ NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "documentation" 
                                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                          , namePrefix = Just "xsd" 
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "EIN (no status code assigned)" ) ]
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
                              { nameLocalName = "enumeration" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
                               ( Name 
                                 { nameLocalName = "value" 
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 } 
                               , [ ContentText "3" ]
                               ) 
                              ]
                            , elementNodes = 
                              [ NodeContent ( ContentText "" )
                              , NodeElement 
                                ( Element 
                                  { elementName = Name 
                                    { nameLocalName = "annotation" 
                                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                    , namePrefix = Just "xsd" 
                                    } 
                                  , elementAttributes = []
                                  , elementNodes = 
                                    [ NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "documentation" 
                                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                          , namePrefix = Just "xsd" 
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "a TIN stored on AIMS (no status code assigned)" ) ]
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
                              { nameLocalName = "enumeration" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
                               ( Name 
                                 { nameLocalName = "value" 
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 } 
                               , [ ContentText "4" ]
                               ) 
                              ]
                            , elementNodes = 
                              [ NodeContent ( ContentText "" )
                              , NodeElement 
                                ( Element 
                                  { elementName = Name 
                                    { nameLocalName = "annotation" 
                                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                    , namePrefix = Just "xsd" 
                                    } 
                                  , elementAttributes = []
                                  , elementNodes = 
                                    [ NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "documentation" 
                                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                          , namePrefix = Just "xsd" 
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "CAF (non-EIN) Representative Number (no status code assigned)" ) ]
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
                  , namePrefix = Just "xsd" 
                  } 
                , elementAttributes = 
                  [ 
                   ( Name 
                     { nameLocalName = "name" 
                     , nameNamespace = Nothing
                     , namePrefix = Nothing
                     } 
                   , [ ContentText "TestIndicatorType" ]
                   ) 
                  ]
                , elementNodes = 
                  [ NodeContent ( ContentText "" )
                  , NodeElement 
                    ( Element 
                      { elementName = Name 
                        { nameLocalName = "annotation" 
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                        , namePrefix = Just "xsd" 
                        } 
                      , elementAttributes = []
                      , elementNodes = 
                        [ NodeContent ( ContentText "" )
                        , NodeElement 
                          ( Element 
                            { elementName = Name 
                              { nameLocalName = "documentation" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = []
                            , elementNodes = 
                              [ NodeContent ( ContentText "" )
                              , NodeElement 
                                ( Element 
                                  { elementName = Name 
                                    { nameLocalName = "Component" 
                                    , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                    , namePrefix = Nothing
                                    } 
                                  , elementAttributes = []
                                  , elementNodes = 
                                    [ NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "DictionaryEntryNm" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "Test Indicator Type" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "MajorVersionNum" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "1" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "MinorVersionNum" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "0" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "VersionEffectiveBeginDt" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "2012-05-23" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "VersionDescriptionTxt" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "Initial version" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "DescriptionTxt" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "The test indicator type code" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "DataElementId" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "" ) ]
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
                        { nameLocalName = "restriction" 
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                        , namePrefix = Just "xsd" 
                        } 
                      , elementAttributes = 
                        [ 
                         ( Name 
                           { nameLocalName = "base" 
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           } 
                         , [ ContentText "xsd:string" ]
                         ) 
                        ]
                      , elementNodes = 
                        [ NodeContent ( ContentText "" )
                        , NodeElement 
                          ( Element 
                            { elementName = Name 
                              { nameLocalName = "enumeration" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
                               ( Name 
                                 { nameLocalName = "value" 
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 } 
                               , [ ContentText "P" ]
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
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
                               ( Name 
                                 { nameLocalName = "value" 
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 } 
                               , [ ContentText "T" ]
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
                  , namePrefix = Just "xsd" 
                  } 
                , elementAttributes = 
                  [ 
                   ( Name 
                     { nameLocalName = "name" 
                     , nameNamespace = Nothing
                     , namePrefix = Nothing
                     } 
                   , [ ContentText "TimestampType" ]
                   ) 
                  ]
                , elementNodes = 
                  [ NodeContent ( ContentText "" )
                  , NodeElement 
                    ( Element 
                      { elementName = Name 
                        { nameLocalName = "annotation" 
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                        , namePrefix = Just "xsd" 
                        } 
                      , elementAttributes = []
                      , elementNodes = 
                        [ NodeContent ( ContentText "" )
                        , NodeElement 
                          ( Element 
                            { elementName = Name 
                              { nameLocalName = "documentation" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = []
                            , elementNodes = 
                              [ NodeContent ( ContentText "" )
                              , NodeElement 
                                ( Element 
                                  { elementName = Name 
                                    { nameLocalName = "Component" 
                                    , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                    , namePrefix = Nothing
                                    } 
                                  , elementAttributes = []
                                  , elementNodes = 
                                    [ NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "DictionaryEntryNm" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "Timestamp Type" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "MajorVersionNum" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "1" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "MinorVersionNum" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "0" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "VersionEffectiveBeginDt" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "2008-01-08" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "VersionDescriptionTxt" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "EFileTypes, December 14, 2007" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "DescriptionTxt" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "Base type for a date and time stamp - Timezone portion is required and fractional seconds are prohibited" ) ]
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
                        { nameLocalName = "restriction" 
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                        , namePrefix = Just "xsd" 
                        } 
                      , elementAttributes = 
                        [ 
                         ( Name 
                           { nameLocalName = "base" 
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           } 
                         , [ ContentText "xsd:dateTime" ]
                         ) 
                        ]
                      , elementNodes = 
                        [ NodeContent ( ContentText "" )
                        , NodeElement 
                          ( Element 
                            { elementName = Name 
                              { nameLocalName = "pattern" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
                               ( Name 
                                 { nameLocalName = "value" 
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 } 
                               , [ ContentText "[1-9][0-9]{3}\-.+T[^\.]+(Z|[\+\-].+)" ]
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
                  , namePrefix = Just "xsd" 
                  } 
                , elementAttributes = 
                  [ 
                   ( Name 
                     { nameLocalName = "name" 
                     , nameNamespace = Nothing
                     , namePrefix = Nothing
                     } 
                   , [ ContentText "TimestampWithMillisecondsType" ]
                   ) 
                  ]
                , elementNodes = 
                  [ NodeContent ( ContentText "" )
                  , NodeElement 
                    ( Element 
                      { elementName = Name 
                        { nameLocalName = "annotation" 
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                        , namePrefix = Just "xsd" 
                        } 
                      , elementAttributes = []
                      , elementNodes = 
                        [ NodeContent ( ContentText "" )
                        , NodeElement 
                          ( Element 
                            { elementName = Name 
                              { nameLocalName = "documentation" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = []
                            , elementNodes = 
                              [ NodeContent ( ContentText "" )
                              , NodeElement 
                                ( Element 
                                  { elementName = Name 
                                    { nameLocalName = "Component" 
                                    , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                    , namePrefix = Nothing
                                    } 
                                  , elementAttributes = []
                                  , elementNodes = 
                                    [ NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "DictionaryEntryNm" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "Timestamp With Milliseconds Type" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "MajorVersionNum" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "1" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "MinorVersionNum" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "0" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "VersionEffectiveBeginDt" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "2009-03-11" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "VersionDescriptionTxt" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "New for AMS" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "DescriptionTxt" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "timestamp for AMS" ) ]
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
                        { nameLocalName = "restriction" 
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                        , namePrefix = Just "xsd" 
                        } 
                      , elementAttributes = 
                        [ 
                         ( Name 
                           { nameLocalName = "base" 
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           } 
                         , [ ContentText "xsd:string" ]
                         ) 
                        ]
                      , elementNodes = 
                        [ NodeContent ( ContentText "" )
                        , NodeElement 
                          ( Element 
                            { elementName = Name 
                              { nameLocalName = "pattern" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
                               ( Name 
                                 { nameLocalName = "value" 
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 } 
                               , [ ContentText "[1-9][0-9]{3}(\-[0-9]{2}){2}T([0-9]{2}:){2}[0-9]{2}(\.[0-9]{1,6}){0,1}(Z|[\+\-][0-9]{2}:{0,1}[0-9]{2})" ]
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
                  , namePrefix = Just "xsd" 
                  } 
                , elementAttributes = 
                  [ 
                   ( Name 
                     { nameLocalName = "name" 
                     , nameNamespace = Nothing
                     , namePrefix = Nothing
                     } 
                   , [ ContentText "TINRequestTypeCodeType" ]
                   ) 
                  ]
                , elementNodes = 
                  [ NodeContent ( ContentText "" )
                  , NodeElement 
                    ( Element 
                      { elementName = Name 
                        { nameLocalName = "annotation" 
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                        , namePrefix = Just "xsd" 
                        } 
                      , elementAttributes = []
                      , elementNodes = 
                        [ NodeContent ( ContentText "" )
                        , NodeElement 
                          ( Element 
                            { elementName = Name 
                              { nameLocalName = "documentation" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = []
                            , elementNodes = 
                              [ NodeContent ( ContentText "" )
                              , NodeElement 
                                ( Element 
                                  { elementName = Name 
                                    { nameLocalName = "Component" 
                                    , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                    , namePrefix = Nothing
                                    } 
                                  , elementAttributes = []
                                  , elementNodes = 
                                    [ NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "DictionaryEntryNm" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "TIN Request Type Code Type" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "MajorVersionNum" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "1" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "MinorVersionNum" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "0" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "VersionEffectiveBeginDt" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "2012-11-01" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "VersionDescriptionTxt" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "Initial Version" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "DescriptionTxt" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "A code type used to identify the TIN request type." ) ]
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
                        { nameLocalName = "restriction" 
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                        , namePrefix = Just "xsd" 
                        } 
                      , elementAttributes = 
                        [ 
                         ( Name 
                           { nameLocalName = "base" 
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           } 
                         , [ ContentText "xsd:string" ]
                         ) 
                        ]
                      , elementNodes = 
                        [ NodeContent ( ContentText "" )
                        , NodeElement 
                          ( Element 
                            { elementName = Name 
                              { nameLocalName = "enumeration" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
                               ( Name 
                                 { nameLocalName = "value" 
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 } 
                               , [ ContentText "INDIVIDUAL_TIN" ]
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
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
                               ( Name 
                                 { nameLocalName = "value" 
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 } 
                               , [ ContentText "BUSINESS_TIN" ]
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
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
                               ( Name 
                                 { nameLocalName = "value" 
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 } 
                               , [ ContentText "UNKNOWN" ]
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
                  , namePrefix = Just "xsd" 
                  } 
                , elementAttributes = 
                  [ 
                   ( Name 
                     { nameLocalName = "name" 
                     , nameNamespace = Nothing
                     , namePrefix = Nothing
                     } 
                   , [ ContentText "TINType" ]
                   ) 
                  ]
                , elementNodes = 
                  [ NodeContent ( ContentText "" )
                  , NodeElement 
                    ( Element 
                      { elementName = Name 
                        { nameLocalName = "annotation" 
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                        , namePrefix = Just "xsd" 
                        } 
                      , elementAttributes = []
                      , elementNodes = 
                        [ NodeContent ( ContentText "" )
                        , NodeElement 
                          ( Element 
                            { elementName = Name 
                              { nameLocalName = "documentation" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = []
                            , elementNodes = 
                              [ NodeContent ( ContentText "" )
                              , NodeElement 
                                ( Element 
                                  { elementName = Name 
                                    { nameLocalName = "Component" 
                                    , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                    , namePrefix = Nothing
                                    } 
                                  , elementAttributes = []
                                  , elementNodes = 
                                    [ NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "DictionaryEntryNm" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "TIN Type" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "MajorVersionNum" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "1" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "MinorVersionNum" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "0" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "VersionEffectiveBeginDt" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "2011-03-21" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "VersionDescriptionTxt" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "Initial version" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "DescriptionTxt" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "Type for Taxpayer Identification Number - restricted to 9 digits" ) ]
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
                        { nameLocalName = "restriction" 
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                        , namePrefix = Just "xsd" 
                        } 
                      , elementAttributes = 
                        [ 
                         ( Name 
                           { nameLocalName = "base" 
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           } 
                         , [ ContentText "xsd:string" ]
                         ) 
                        ]
                      , elementNodes = 
                        [ NodeContent ( ContentText "" )
                        , NodeElement 
                          ( Element 
                            { elementName = Name 
                              { nameLocalName = "length" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
                               ( Name 
                                 { nameLocalName = "value" 
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 } 
                               , [ ContentText "9" ]
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
                  , namePrefix = Just "xsd" 
                  } 
                , elementAttributes = 
                  [ 
                   ( Name 
                     { nameLocalName = "name" 
                     , nameNamespace = Nothing
                     , namePrefix = Nothing
                     } 
                   , [ ContentText "TINValidationIndicatorType" ]
                   ) 
                  ]
                , elementNodes = 
                  [ NodeContent ( ContentText "" )
                  , NodeElement 
                    ( Element 
                      { elementName = Name 
                        { nameLocalName = "annotation" 
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                        , namePrefix = Just "xsd" 
                        } 
                      , elementAttributes = []
                      , elementNodes = 
                        [ NodeContent ( ContentText "" )
                        , NodeElement 
                          ( Element 
                            { elementName = Name 
                              { nameLocalName = "documentation" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = []
                            , elementNodes = 
                              [ NodeContent ( ContentText "" )
                              , NodeElement 
                                ( Element 
                                  { elementName = Name 
                                    { nameLocalName = "Component" 
                                    , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                    , namePrefix = Nothing
                                    } 
                                  , elementAttributes = []
                                  , elementNodes = 
                                    [ NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "DictionaryEntryNm" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "Taxpayer Identification Number Validation Indicator Type" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "MajorVersionNum" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "1" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "MinorVersionNum" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "0" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "VersionEffectiveBeginDt" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "2013-03-05" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "VersionDescriptionTxt" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "Initial version" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "DescriptionTxt" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "defines the values for Taxpayer Identification Number Validation" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "DataElementId" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "" ) ]
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
                        { nameLocalName = "restriction" 
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                        , namePrefix = Just "xsd" 
                        } 
                      , elementAttributes = 
                        [ 
                         ( Name 
                           { nameLocalName = "base" 
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           } 
                         , [ ContentText "xsd:string" ]
                         ) 
                        ]
                      , elementNodes = 
                        [ NodeContent ( ContentText "" )
                        , NodeElement 
                          ( Element 
                            { elementName = Name 
                              { nameLocalName = "enumeration" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
                               ( Name 
                                 { nameLocalName = "value" 
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 } 
                               , [ ContentText "Pass" ]
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
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
                               ( Name 
                                 { nameLocalName = "value" 
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 } 
                               , [ ContentText "Fail" ]
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
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
                               ( Name 
                                 { nameLocalName = "value" 
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 } 
                               , [ ContentText "Not Executed" ]
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
                  , namePrefix = Just "xsd" 
                  } 
                , elementAttributes = 
                  [ 
                   ( Name 
                     { nameLocalName = "name" 
                     , nameNamespace = Nothing
                     , namePrefix = Nothing
                     } 
                   , [ ContentText "USStateCdType" ]
                   ) 
                  ]
                , elementNodes = 
                  [ NodeContent ( ContentText "" )
                  , NodeElement 
                    ( Element 
                      { elementName = Name 
                        { nameLocalName = "annotation" 
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                        , namePrefix = Just "xsd" 
                        } 
                      , elementAttributes = []
                      , elementNodes = 
                        [ NodeContent ( ContentText "" )
                        , NodeElement 
                          ( Element 
                            { elementName = Name 
                              { nameLocalName = "documentation" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = []
                            , elementNodes = 
                              [ NodeContent ( ContentText "" )
                              , NodeElement 
                                ( Element 
                                  { elementName = Name 
                                    { nameLocalName = "Component" 
                                    , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                    , namePrefix = Nothing
                                    } 
                                  , elementAttributes = []
                                  , elementNodes = 
                                    [ NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "DictionaryEntryNm" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "United States State Code Type" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "MajorVersionNum" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "1" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "MinorVersionNum" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "0" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "VersionEffectiveBeginDt" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "2013-01-15" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "VersionDescriptionTxt" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "Initial Version" ) ]
                                        } 
                                      )
                                    , NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "DescriptionTxt" 
                                          , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                          , namePrefix = Nothing
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "One of the states of the United States" ) ]
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
                        { nameLocalName = "restriction" 
                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                        , namePrefix = Just "xsd" 
                        } 
                      , elementAttributes = 
                        [ 
                         ( Name 
                           { nameLocalName = "base" 
                           , nameNamespace = Nothing
                           , namePrefix = Nothing
                           } 
                         , [ ContentText "xsd:string" ]
                         ) 
                        ]
                      , elementNodes = 
                        [ NodeContent ( ContentText "" )
                        , NodeElement 
                          ( Element 
                            { elementName = Name 
                              { nameLocalName = "enumeration" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
                               ( Name 
                                 { nameLocalName = "value" 
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 } 
                               , [ ContentText "AL" ]
                               ) 
                              ]
                            , elementNodes = 
                              [ NodeContent ( ContentText "" )
                              , NodeElement 
                                ( Element 
                                  { elementName = Name 
                                    { nameLocalName = "annotation" 
                                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                    , namePrefix = Just "xsd" 
                                    } 
                                  , elementAttributes = []
                                  , elementNodes = 
                                    [ NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "documentation" 
                                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                          , namePrefix = Just "xsd" 
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "Alabama" ) ]
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
                              { nameLocalName = "enumeration" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
                               ( Name 
                                 { nameLocalName = "value" 
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 } 
                               , [ ContentText "AK" ]
                               ) 
                              ]
                            , elementNodes = 
                              [ NodeContent ( ContentText "" )
                              , NodeElement 
                                ( Element 
                                  { elementName = Name 
                                    { nameLocalName = "annotation" 
                                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                    , namePrefix = Just "xsd" 
                                    } 
                                  , elementAttributes = []
                                  , elementNodes = 
                                    [ NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "documentation" 
                                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                          , namePrefix = Just "xsd" 
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "Alaska" ) ]
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
                              { nameLocalName = "enumeration" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
                               ( Name 
                                 { nameLocalName = "value" 
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 } 
                               , [ ContentText "AZ" ]
                               ) 
                              ]
                            , elementNodes = 
                              [ NodeContent ( ContentText "" )
                              , NodeElement 
                                ( Element 
                                  { elementName = Name 
                                    { nameLocalName = "annotation" 
                                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                    , namePrefix = Just "xsd" 
                                    } 
                                  , elementAttributes = []
                                  , elementNodes = 
                                    [ NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "documentation" 
                                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                          , namePrefix = Just "xsd" 
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "Arizona" ) ]
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
                              { nameLocalName = "enumeration" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
                               ( Name 
                                 { nameLocalName = "value" 
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 } 
                               , [ ContentText "AR" ]
                               ) 
                              ]
                            , elementNodes = 
                              [ NodeContent ( ContentText "" )
                              , NodeElement 
                                ( Element 
                                  { elementName = Name 
                                    { nameLocalName = "annotation" 
                                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                    , namePrefix = Just "xsd" 
                                    } 
                                  , elementAttributes = []
                                  , elementNodes = 
                                    [ NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "documentation" 
                                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                          , namePrefix = Just "xsd" 
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "Arkansas" ) ]
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
                              { nameLocalName = "enumeration" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
                               ( Name 
                                 { nameLocalName = "value" 
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 } 
                               , [ ContentText "CA" ]
                               ) 
                              ]
                            , elementNodes = 
                              [ NodeContent ( ContentText "" )
                              , NodeElement 
                                ( Element 
                                  { elementName = Name 
                                    { nameLocalName = "annotation" 
                                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                    , namePrefix = Just "xsd" 
                                    } 
                                  , elementAttributes = []
                                  , elementNodes = 
                                    [ NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "documentation" 
                                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                          , namePrefix = Just "xsd" 
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "California" ) ]
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
                              { nameLocalName = "enumeration" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
                               ( Name 
                                 { nameLocalName = "value" 
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 } 
                               , [ ContentText "CO" ]
                               ) 
                              ]
                            , elementNodes = 
                              [ NodeContent ( ContentText "" )
                              , NodeElement 
                                ( Element 
                                  { elementName = Name 
                                    { nameLocalName = "annotation" 
                                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                    , namePrefix = Just "xsd" 
                                    } 
                                  , elementAttributes = []
                                  , elementNodes = 
                                    [ NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "documentation" 
                                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                          , namePrefix = Just "xsd" 
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "Colorado" ) ]
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
                              { nameLocalName = "enumeration" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
                               ( Name 
                                 { nameLocalName = "value" 
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 } 
                               , [ ContentText "CT" ]
                               ) 
                              ]
                            , elementNodes = 
                              [ NodeContent ( ContentText "" )
                              , NodeElement 
                                ( Element 
                                  { elementName = Name 
                                    { nameLocalName = "annotation" 
                                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                    , namePrefix = Just "xsd" 
                                    } 
                                  , elementAttributes = []
                                  , elementNodes = 
                                    [ NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "documentation" 
                                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                          , namePrefix = Just "xsd" 
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "Connecticut" ) ]
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
                              { nameLocalName = "enumeration" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
                               ( Name 
                                 { nameLocalName = "value" 
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 } 
                               , [ ContentText "DE" ]
                               ) 
                              ]
                            , elementNodes = 
                              [ NodeContent ( ContentText "" )
                              , NodeElement 
                                ( Element 
                                  { elementName = Name 
                                    { nameLocalName = "annotation" 
                                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                    , namePrefix = Just "xsd" 
                                    } 
                                  , elementAttributes = []
                                  , elementNodes = 
                                    [ NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "documentation" 
                                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                          , namePrefix = Just "xsd" 
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "Delaware" ) ]
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
                              { nameLocalName = "enumeration" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
                               ( Name 
                                 { nameLocalName = "value" 
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 } 
                               , [ ContentText "DC" ]
                               ) 
                              ]
                            , elementNodes = 
                              [ NodeContent ( ContentText "" )
                              , NodeElement 
                                ( Element 
                                  { elementName = Name 
                                    { nameLocalName = "annotation" 
                                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                    , namePrefix = Just "xsd" 
                                    } 
                                  , elementAttributes = []
                                  , elementNodes = 
                                    [ NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "documentation" 
                                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                          , namePrefix = Just "xsd" 
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "District of Columbia" ) ]
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
                              { nameLocalName = "enumeration" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
                               ( Name 
                                 { nameLocalName = "value" 
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 } 
                               , [ ContentText "FL" ]
                               ) 
                              ]
                            , elementNodes = 
                              [ NodeContent ( ContentText "" )
                              , NodeElement 
                                ( Element 
                                  { elementName = Name 
                                    { nameLocalName = "annotation" 
                                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                    , namePrefix = Just "xsd" 
                                    } 
                                  , elementAttributes = []
                                  , elementNodes = 
                                    [ NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "documentation" 
                                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                          , namePrefix = Just "xsd" 
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "Florida" ) ]
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
                              { nameLocalName = "enumeration" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
                               ( Name 
                                 { nameLocalName = "value" 
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 } 
                               , [ ContentText "GA" ]
                               ) 
                              ]
                            , elementNodes = 
                              [ NodeContent ( ContentText "" )
                              , NodeElement 
                                ( Element 
                                  { elementName = Name 
                                    { nameLocalName = "annotation" 
                                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                    , namePrefix = Just "xsd" 
                                    } 
                                  , elementAttributes = []
                                  , elementNodes = 
                                    [ NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "documentation" 
                                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                          , namePrefix = Just "xsd" 
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "Georgia" ) ]
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
                              { nameLocalName = "enumeration" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
                               ( Name 
                                 { nameLocalName = "value" 
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 } 
                               , [ ContentText "HI" ]
                               ) 
                              ]
                            , elementNodes = 
                              [ NodeContent ( ContentText "" )
                              , NodeElement 
                                ( Element 
                                  { elementName = Name 
                                    { nameLocalName = "annotation" 
                                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                    , namePrefix = Just "xsd" 
                                    } 
                                  , elementAttributes = []
                                  , elementNodes = 
                                    [ NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "documentation" 
                                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                          , namePrefix = Just "xsd" 
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "Hawaii" ) ]
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
                              { nameLocalName = "enumeration" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
                               ( Name 
                                 { nameLocalName = "value" 
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
                                    , namePrefix = Just "xsd" 
                                    } 
                                  , elementAttributes = []
                                  , elementNodes = 
                                    [ NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "documentation" 
                                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                          , namePrefix = Just "xsd" 
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "Idaho" ) ]
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
                              { nameLocalName = "enumeration" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
                               ( Name 
                                 { nameLocalName = "value" 
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 } 
                               , [ ContentText "IL" ]
                               ) 
                              ]
                            , elementNodes = 
                              [ NodeContent ( ContentText "" )
                              , NodeElement 
                                ( Element 
                                  { elementName = Name 
                                    { nameLocalName = "annotation" 
                                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                    , namePrefix = Just "xsd" 
                                    } 
                                  , elementAttributes = []
                                  , elementNodes = 
                                    [ NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "documentation" 
                                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                          , namePrefix = Just "xsd" 
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "Illinois" ) ]
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
                              { nameLocalName = "enumeration" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
                               ( Name 
                                 { nameLocalName = "value" 
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 } 
                               , [ ContentText "IN" ]
                               ) 
                              ]
                            , elementNodes = 
                              [ NodeContent ( ContentText "" )
                              , NodeElement 
                                ( Element 
                                  { elementName = Name 
                                    { nameLocalName = "annotation" 
                                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                    , namePrefix = Just "xsd" 
                                    } 
                                  , elementAttributes = []
                                  , elementNodes = 
                                    [ NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "documentation" 
                                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                          , namePrefix = Just "xsd" 
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "Indiana" ) ]
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
                              { nameLocalName = "enumeration" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
                               ( Name 
                                 { nameLocalName = "value" 
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 } 
                               , [ ContentText "IA" ]
                               ) 
                              ]
                            , elementNodes = 
                              [ NodeContent ( ContentText "" )
                              , NodeElement 
                                ( Element 
                                  { elementName = Name 
                                    { nameLocalName = "annotation" 
                                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                    , namePrefix = Just "xsd" 
                                    } 
                                  , elementAttributes = []
                                  , elementNodes = 
                                    [ NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "documentation" 
                                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                          , namePrefix = Just "xsd" 
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "Iowa" ) ]
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
                              { nameLocalName = "enumeration" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
                               ( Name 
                                 { nameLocalName = "value" 
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 } 
                               , [ ContentText "KS" ]
                               ) 
                              ]
                            , elementNodes = 
                              [ NodeContent ( ContentText "" )
                              , NodeElement 
                                ( Element 
                                  { elementName = Name 
                                    { nameLocalName = "annotation" 
                                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                    , namePrefix = Just "xsd" 
                                    } 
                                  , elementAttributes = []
                                  , elementNodes = 
                                    [ NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "documentation" 
                                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                          , namePrefix = Just "xsd" 
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "Kansas" ) ]
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
                              { nameLocalName = "enumeration" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
                               ( Name 
                                 { nameLocalName = "value" 
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 } 
                               , [ ContentText "KY" ]
                               ) 
                              ]
                            , elementNodes = 
                              [ NodeContent ( ContentText "" )
                              , NodeElement 
                                ( Element 
                                  { elementName = Name 
                                    { nameLocalName = "annotation" 
                                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                    , namePrefix = Just "xsd" 
                                    } 
                                  , elementAttributes = []
                                  , elementNodes = 
                                    [ NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "documentation" 
                                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                          , namePrefix = Just "xsd" 
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "Kentucky" ) ]
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
                              { nameLocalName = "enumeration" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
                               ( Name 
                                 { nameLocalName = "value" 
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 } 
                               , [ ContentText "LA" ]
                               ) 
                              ]
                            , elementNodes = 
                              [ NodeContent ( ContentText "" )
                              , NodeElement 
                                ( Element 
                                  { elementName = Name 
                                    { nameLocalName = "annotation" 
                                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                    , namePrefix = Just "xsd" 
                                    } 
                                  , elementAttributes = []
                                  , elementNodes = 
                                    [ NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "documentation" 
                                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                          , namePrefix = Just "xsd" 
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "Louisiana" ) ]
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
                              { nameLocalName = "enumeration" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
                               ( Name 
                                 { nameLocalName = "value" 
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 } 
                               , [ ContentText "ME" ]
                               ) 
                              ]
                            , elementNodes = 
                              [ NodeContent ( ContentText "" )
                              , NodeElement 
                                ( Element 
                                  { elementName = Name 
                                    { nameLocalName = "annotation" 
                                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                    , namePrefix = Just "xsd" 
                                    } 
                                  , elementAttributes = []
                                  , elementNodes = 
                                    [ NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "documentation" 
                                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                          , namePrefix = Just "xsd" 
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "Maine" ) ]
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
                              { nameLocalName = "enumeration" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
                               ( Name 
                                 { nameLocalName = "value" 
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 } 
                               , [ ContentText "MD" ]
                               ) 
                              ]
                            , elementNodes = 
                              [ NodeContent ( ContentText "" )
                              , NodeElement 
                                ( Element 
                                  { elementName = Name 
                                    { nameLocalName = "annotation" 
                                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                    , namePrefix = Just "xsd" 
                                    } 
                                  , elementAttributes = []
                                  , elementNodes = 
                                    [ NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "documentation" 
                                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                          , namePrefix = Just "xsd" 
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "Maryland" ) ]
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
                              { nameLocalName = "enumeration" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
                               ( Name 
                                 { nameLocalName = "value" 
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 } 
                               , [ ContentText "MA" ]
                               ) 
                              ]
                            , elementNodes = 
                              [ NodeContent ( ContentText "" )
                              , NodeElement 
                                ( Element 
                                  { elementName = Name 
                                    { nameLocalName = "annotation" 
                                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                    , namePrefix = Just "xsd" 
                                    } 
                                  , elementAttributes = []
                                  , elementNodes = 
                                    [ NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "documentation" 
                                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                          , namePrefix = Just "xsd" 
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "Massachusetts" ) ]
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
                              { nameLocalName = "enumeration" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
                               ( Name 
                                 { nameLocalName = "value" 
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 } 
                               , [ ContentText "MI" ]
                               ) 
                              ]
                            , elementNodes = 
                              [ NodeContent ( ContentText "" )
                              , NodeElement 
                                ( Element 
                                  { elementName = Name 
                                    { nameLocalName = "annotation" 
                                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                    , namePrefix = Just "xsd" 
                                    } 
                                  , elementAttributes = []
                                  , elementNodes = 
                                    [ NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "documentation" 
                                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                          , namePrefix = Just "xsd" 
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "Michigan" ) ]
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
                              { nameLocalName = "enumeration" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
                               ( Name 
                                 { nameLocalName = "value" 
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 } 
                               , [ ContentText "MN" ]
                               ) 
                              ]
                            , elementNodes = 
                              [ NodeContent ( ContentText "" )
                              , NodeElement 
                                ( Element 
                                  { elementName = Name 
                                    { nameLocalName = "annotation" 
                                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                    , namePrefix = Just "xsd" 
                                    } 
                                  , elementAttributes = []
                                  , elementNodes = 
                                    [ NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "documentation" 
                                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                          , namePrefix = Just "xsd" 
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "Minnesota" ) ]
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
                              { nameLocalName = "enumeration" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
                               ( Name 
                                 { nameLocalName = "value" 
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 } 
                               , [ ContentText "MS" ]
                               ) 
                              ]
                            , elementNodes = 
                              [ NodeContent ( ContentText "" )
                              , NodeElement 
                                ( Element 
                                  { elementName = Name 
                                    { nameLocalName = "annotation" 
                                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                    , namePrefix = Just "xsd" 
                                    } 
                                  , elementAttributes = []
                                  , elementNodes = 
                                    [ NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "documentation" 
                                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                          , namePrefix = Just "xsd" 
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "Mississippi" ) ]
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
                              { nameLocalName = "enumeration" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
                               ( Name 
                                 { nameLocalName = "value" 
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 } 
                               , [ ContentText "MO" ]
                               ) 
                              ]
                            , elementNodes = 
                              [ NodeContent ( ContentText "" )
                              , NodeElement 
                                ( Element 
                                  { elementName = Name 
                                    { nameLocalName = "annotation" 
                                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                    , namePrefix = Just "xsd" 
                                    } 
                                  , elementAttributes = []
                                  , elementNodes = 
                                    [ NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "documentation" 
                                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                          , namePrefix = Just "xsd" 
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "Missouri" ) ]
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
                              { nameLocalName = "enumeration" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
                               ( Name 
                                 { nameLocalName = "value" 
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 } 
                               , [ ContentText "MT" ]
                               ) 
                              ]
                            , elementNodes = 
                              [ NodeContent ( ContentText "" )
                              , NodeElement 
                                ( Element 
                                  { elementName = Name 
                                    { nameLocalName = "annotation" 
                                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                    , namePrefix = Just "xsd" 
                                    } 
                                  , elementAttributes = []
                                  , elementNodes = 
                                    [ NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "documentation" 
                                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                          , namePrefix = Just "xsd" 
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "Montana" ) ]
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
                              { nameLocalName = "enumeration" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
                               ( Name 
                                 { nameLocalName = "value" 
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 } 
                               , [ ContentText "NE" ]
                               ) 
                              ]
                            , elementNodes = 
                              [ NodeContent ( ContentText "" )
                              , NodeElement 
                                ( Element 
                                  { elementName = Name 
                                    { nameLocalName = "annotation" 
                                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                    , namePrefix = Just "xsd" 
                                    } 
                                  , elementAttributes = []
                                  , elementNodes = 
                                    [ NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "documentation" 
                                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                          , namePrefix = Just "xsd" 
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "Nebraska" ) ]
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
                              { nameLocalName = "enumeration" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
                               ( Name 
                                 { nameLocalName = "value" 
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 } 
                               , [ ContentText "NV" ]
                               ) 
                              ]
                            , elementNodes = 
                              [ NodeContent ( ContentText "" )
                              , NodeElement 
                                ( Element 
                                  { elementName = Name 
                                    { nameLocalName = "annotation" 
                                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                    , namePrefix = Just "xsd" 
                                    } 
                                  , elementAttributes = []
                                  , elementNodes = 
                                    [ NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "documentation" 
                                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                          , namePrefix = Just "xsd" 
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "Nevada" ) ]
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
                              { nameLocalName = "enumeration" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
                               ( Name 
                                 { nameLocalName = "value" 
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 } 
                               , [ ContentText "NH" ]
                               ) 
                              ]
                            , elementNodes = 
                              [ NodeContent ( ContentText "" )
                              , NodeElement 
                                ( Element 
                                  { elementName = Name 
                                    { nameLocalName = "annotation" 
                                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                    , namePrefix = Just "xsd" 
                                    } 
                                  , elementAttributes = []
                                  , elementNodes = 
                                    [ NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "documentation" 
                                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                          , namePrefix = Just "xsd" 
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "New Hampshire" ) ]
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
                              { nameLocalName = "enumeration" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
                               ( Name 
                                 { nameLocalName = "value" 
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 } 
                               , [ ContentText "NJ" ]
                               ) 
                              ]
                            , elementNodes = 
                              [ NodeContent ( ContentText "" )
                              , NodeElement 
                                ( Element 
                                  { elementName = Name 
                                    { nameLocalName = "annotation" 
                                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                    , namePrefix = Just "xsd" 
                                    } 
                                  , elementAttributes = []
                                  , elementNodes = 
                                    [ NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "documentation" 
                                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                          , namePrefix = Just "xsd" 
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "New Jersey" ) ]
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
                              { nameLocalName = "enumeration" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
                               ( Name 
                                 { nameLocalName = "value" 
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 } 
                               , [ ContentText "NM" ]
                               ) 
                              ]
                            , elementNodes = 
                              [ NodeContent ( ContentText "" )
                              , NodeElement 
                                ( Element 
                                  { elementName = Name 
                                    { nameLocalName = "annotation" 
                                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                    , namePrefix = Just "xsd" 
                                    } 
                                  , elementAttributes = []
                                  , elementNodes = 
                                    [ NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "documentation" 
                                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                          , namePrefix = Just "xsd" 
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "New Mexico" ) ]
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
                              { nameLocalName = "enumeration" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
                               ( Name 
                                 { nameLocalName = "value" 
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 } 
                               , [ ContentText "NY" ]
                               ) 
                              ]
                            , elementNodes = 
                              [ NodeContent ( ContentText "" )
                              , NodeElement 
                                ( Element 
                                  { elementName = Name 
                                    { nameLocalName = "annotation" 
                                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                    , namePrefix = Just "xsd" 
                                    } 
                                  , elementAttributes = []
                                  , elementNodes = 
                                    [ NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "documentation" 
                                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                          , namePrefix = Just "xsd" 
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "New York" ) ]
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
                              { nameLocalName = "enumeration" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
                               ( Name 
                                 { nameLocalName = "value" 
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 } 
                               , [ ContentText "NC" ]
                               ) 
                              ]
                            , elementNodes = 
                              [ NodeContent ( ContentText "" )
                              , NodeElement 
                                ( Element 
                                  { elementName = Name 
                                    { nameLocalName = "annotation" 
                                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                    , namePrefix = Just "xsd" 
                                    } 
                                  , elementAttributes = []
                                  , elementNodes = 
                                    [ NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "documentation" 
                                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                          , namePrefix = Just "xsd" 
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "North Carolina" ) ]
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
                              { nameLocalName = "enumeration" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
                               ( Name 
                                 { nameLocalName = "value" 
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 } 
                               , [ ContentText "ND" ]
                               ) 
                              ]
                            , elementNodes = 
                              [ NodeContent ( ContentText "" )
                              , NodeElement 
                                ( Element 
                                  { elementName = Name 
                                    { nameLocalName = "annotation" 
                                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                    , namePrefix = Just "xsd" 
                                    } 
                                  , elementAttributes = []
                                  , elementNodes = 
                                    [ NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "documentation" 
                                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                          , namePrefix = Just "xsd" 
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "North Dakota" ) ]
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
                              { nameLocalName = "enumeration" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
                               ( Name 
                                 { nameLocalName = "value" 
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 } 
                               , [ ContentText "OH" ]
                               ) 
                              ]
                            , elementNodes = 
                              [ NodeContent ( ContentText "" )
                              , NodeElement 
                                ( Element 
                                  { elementName = Name 
                                    { nameLocalName = "annotation" 
                                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                    , namePrefix = Just "xsd" 
                                    } 
                                  , elementAttributes = []
                                  , elementNodes = 
                                    [ NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "documentation" 
                                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                          , namePrefix = Just "xsd" 
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "Ohio" ) ]
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
                              { nameLocalName = "enumeration" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
                               ( Name 
                                 { nameLocalName = "value" 
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 } 
                               , [ ContentText "OK" ]
                               ) 
                              ]
                            , elementNodes = 
                              [ NodeContent ( ContentText "" )
                              , NodeElement 
                                ( Element 
                                  { elementName = Name 
                                    { nameLocalName = "annotation" 
                                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                    , namePrefix = Just "xsd" 
                                    } 
                                  , elementAttributes = []
                                  , elementNodes = 
                                    [ NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "documentation" 
                                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                          , namePrefix = Just "xsd" 
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "Oklahoma" ) ]
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
                              { nameLocalName = "enumeration" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
                               ( Name 
                                 { nameLocalName = "value" 
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 } 
                               , [ ContentText "OR" ]
                               ) 
                              ]
                            , elementNodes = 
                              [ NodeContent ( ContentText "" )
                              , NodeElement 
                                ( Element 
                                  { elementName = Name 
                                    { nameLocalName = "annotation" 
                                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                    , namePrefix = Just "xsd" 
                                    } 
                                  , elementAttributes = []
                                  , elementNodes = 
                                    [ NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "documentation" 
                                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                          , namePrefix = Just "xsd" 
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "Oregon" ) ]
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
                              { nameLocalName = "enumeration" 
                              , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                              , namePrefix = Just "xsd" 
                              } 
                            , elementAttributes = 
                              [ 
                               ( Name 
                                 { nameLocalName = "value" 
                                 , nameNamespace = Nothing
                                 , namePrefix = Nothing
                                 } 
                               , [ ContentText "PA" ]
                               ) 
                              ]
                            , elementNodes = 
                              [ NodeContent ( ContentText "" )
                              , NodeElement 
                                ( Element 
                                  { elementName = Name 
                                    { nameLocalName = "annotation" 
                                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                    , namePrefix = Just "xsd" 
                                    } 
                                  , elementAttributes = []
                                  , elementNodes = 
                                    [ NodeContent ( ContentText "" )
                                    , NodeElement 
                                      ( Element 
                                        { elementName = Name 
                                          { nameLocalName = "documentation" 
                                          , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                          , namePrefix = Just "xsd" 
                                          } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                          [ NodeContent ( ContentText "Pennsylvania" ) ]
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
                                            { nameLocalName = "enumeration" 
                                            , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                            , namePrefix = Just "xsd" 
                                            } 
                                        , elementAttributes = 
                                            [ 
                                                ( Name 
                                                    { nameLocalName = "value" 
                                                    , nameNamespace = Nothing
                                                    , namePrefix = Nothing
                                                    } 
                                                , [ ContentText "RI" ]
                                                ) 
                                            ]
                                        , elementNodes = 
                                            [ NodeContent ( ContentText "" )
                                            , NodeElement 
                                                ( Element 
                                                    { elementName = Name 
                                                        { nameLocalName = "annotation" 
                                                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                                        , namePrefix = Just "xsd" 
                                                        } 
                                                    , elementAttributes = []
                                                    , elementNodes = 
                                                        [ NodeContent ( ContentText "" )
                                                        , NodeElement 
                                                            ( Element 
                                                                { elementName = Name 
                                                                    { nameLocalName = "documentation" 
                                                                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                                                    , namePrefix = Just "xsd" 
                                                                    } 
                                                                , elementAttributes = []
                                                                , elementNodes = 
                                                                    [ NodeContent ( ContentText "Rhode Island" ) ]
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
                                            { nameLocalName = "enumeration" 
                                            , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                            , namePrefix = Just "xsd" 
                                            } 
                                        , elementAttributes = 
                                            [ 
                                                ( Name 
                                                    { nameLocalName = "value" 
                                                    , nameNamespace = Nothing
                                                    , namePrefix = Nothing
                                                    } 
                                                , [ ContentText "SC" ]
                                                ) 
                                            ]
                                        , elementNodes = 
                                            [ NodeContent ( ContentText "" )
                                            , NodeElement 
                                                ( Element 
                                                    { elementName = Name 
                                                        { nameLocalName = "annotation" 
                                                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                                        , namePrefix = Just "xsd" 
                                                        } 
                                                    , elementAttributes = []
                                                    , elementNodes = 
                                                        [ NodeContent ( ContentText "" )
                                                        , NodeElement 
                                                            ( Element 
                                                                { elementName = Name 
                                                                    { nameLocalName = "documentation" 
                                                                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                                                    , namePrefix = Just "xsd" 
                                                                    } 
                                                                , elementAttributes = []
                                                                , elementNodes = 
                                                                    [ NodeContent ( ContentText "South Carolina" ) ]
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
                                            { nameLocalName = "enumeration" 
                                            , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                            , namePrefix = Just "xsd" 
                                            } 
                                        , elementAttributes = 
                                            [ 
                                                ( Name 
                                                    { nameLocalName = "value" 
                                                    , nameNamespace = Nothing
                                                    , namePrefix = Nothing
                                                    } 
                                                , [ ContentText "SD" ]
                                                ) 
                                            ]
                                        , elementNodes = 
                                            [ NodeContent ( ContentText "" )
                                            , NodeElement 
                                                ( Element 
                                                    { elementName = Name 
                                                        { nameLocalName = "annotation" 
                                                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                                        , namePrefix = Just "xsd" 
                                                        } 
                                                    , elementAttributes = []
                                                    , elementNodes = 
                                                        [ NodeContent ( ContentText "" )
                                                        , NodeElement 
                                                            ( Element 
                                                                { elementName = Name 
                                                                    { nameLocalName = "documentation" 
                                                                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                                                    , namePrefix = Just "xsd" 
                                                                    } 
                                                                , elementAttributes = []
                                                                , elementNodes = 
                                                                    [ NodeContent ( ContentText "South Dakota" ) ]
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
                                            { nameLocalName = "enumeration" 
                                            , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                            , namePrefix = Just "xsd" 
                                            } 
                                        , elementAttributes = 
                                            [ 
                                                ( Name 
                                                    { nameLocalName = "value" 
                                                    , nameNamespace = Nothing
                                                    , namePrefix = Nothing
                                                    } 
                                                , [ ContentText "TN" ]
                                                ) 
                                            ]
                                        , elementNodes = 
                                            [ NodeContent ( ContentText "" )
                                            , NodeElement 
                                                ( Element 
                                                    { elementName = Name 
                                                        { nameLocalName = "annotation" 
                                                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                                        , namePrefix = Just "xsd" 
                                                        } 
                                                    , elementAttributes = []
                                                    , elementNodes = 
                                                        [ NodeContent ( ContentText "" )
                                                        , NodeElement 
                                                            ( Element 
                                                                { elementName = Name 
                                                                    { nameLocalName = "documentation" 
                                                                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                                                    , namePrefix = Just "xsd" 
                                                                    } 
                                                                , elementAttributes = []
                                                                , elementNodes = 
                                                                    [ NodeContent ( ContentText "Tennessee" ) ]
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
                                            { nameLocalName = "enumeration" 
                                            , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                            , namePrefix = Just "xsd" 
                                            } 
                                        , elementAttributes = 
                                            [ 
                                                ( Name 
                                                    { nameLocalName = "value" 
                                                    , nameNamespace = Nothing
                                                    , namePrefix = Nothing
                                                    } 
                                                , [ ContentText "TX" ]
                                                ) 
                                            ]
                                        , elementNodes = 
                                            [ NodeContent ( ContentText "" )
                                            , NodeElement 
                                                ( Element 
                                                    { elementName = Name 
                                                        { nameLocalName = "annotation" 
                                                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                                        , namePrefix = Just "xsd" 
                                                        } 
                                                    , elementAttributes = []
                                                    , elementNodes = 
                                                        [ NodeContent ( ContentText "" )
                                                        , NodeElement 
                                                            ( Element 
                                                                { elementName = Name 
                                                                    { nameLocalName = "documentation" 
                                                                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                                                    , namePrefix = Just "xsd" 
                                                                    } 
                                                                , elementAttributes = []
                                                                , elementNodes = 
                                                                    [ NodeContent ( ContentText "Texas" ) ]
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
                                            { nameLocalName = "enumeration" 
                                            , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                            , namePrefix = Just "xsd" 
                                            } 
                                        , elementAttributes = 
                                            [ 
                                                ( Name 
                                                    { nameLocalName = "value" 
                                                    , nameNamespace = Nothing
                                                    , namePrefix = Nothing
                                                    } 
                                                , [ ContentText "UT" ]
                                                ) 
                                            ]
                                        , elementNodes = 
                                            [ NodeContent ( ContentText "" )
                                            , NodeElement 
                                                ( Element 
                                                    { elementName = Name 
                                                        { nameLocalName = "annotation" 
                                                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                                        , namePrefix = Just "xsd" 
                                                        } 
                                                    , elementAttributes = []
                                                    , elementNodes = 
                                                        [ NodeContent ( ContentText "" )
                                                        , NodeElement 
                                                            ( Element 
                                                                { elementName = Name 
                                                                    { nameLocalName = "documentation" 
                                                                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                                                    , namePrefix = Just "xsd" 
                                                                    } 
                                                                , elementAttributes = []
                                                                , elementNodes = 
                                                                    [ NodeContent ( ContentText "Utah" ) ]
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
                                            { nameLocalName = "enumeration" 
                                            , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                            , namePrefix = Just "xsd" 
                                            } 
                                        , elementAttributes = 
                                            [ 
                                                ( Name 
                                                    { nameLocalName = "value" 
                                                    , nameNamespace = Nothing
                                                    , namePrefix = Nothing
                                                    } 
                                                , [ ContentText "VT" ]
                                                ) 
                                            ]
                                        , elementNodes = 
                                            [ NodeContent ( ContentText "" )
                                            , NodeElement 
                                                ( Element 
                                                    { elementName = Name 
                                                        { nameLocalName = "annotation" 
                                                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                                        , namePrefix = Just "xsd" 
                                                        } 
                                                    , elementAttributes = []
                                                    , elementNodes = 
                                                        [ NodeContent ( ContentText "" )
                                                        , NodeElement 
                                                            ( Element 
                                                                { elementName = Name 
                                                                    { nameLocalName = "documentation" 
                                                                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                                                    , namePrefix = Just "xsd" 
                                                                    } 
                                                                , elementAttributes = []
                                                                , elementNodes = 
                                                                    [ NodeContent ( ContentText "Vermont" ) ]
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
                                            { nameLocalName = "enumeration" 
                                            , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                            , namePrefix = Just "xsd" 
                                            } 
                                        , elementAttributes = 
                                            [ 
                                                ( Name 
                                                    { nameLocalName = "value" 
                                                    , nameNamespace = Nothing
                                                    , namePrefix = Nothing
                                                    } 
                                                , [ ContentText "VA" ]
                                                ) 
                                            ]
                                        , elementNodes = 
                                            [ NodeContent ( ContentText "" )
                                            , NodeElement 
                                                ( Element 
                                                    { elementName = Name 
                                                        { nameLocalName = "annotation" 
                                                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                                        , namePrefix = Just "xsd" 
                                                        } 
                                                    , elementAttributes = []
                                                    , elementNodes = 
                                                        [ NodeContent ( ContentText "" )
                                                        , NodeElement 
                                                            ( Element 
                                                                { elementName = Name 
                                                                    { nameLocalName = "documentation" 
                                                                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                                                    , namePrefix = Just "xsd" 
                                                                    } 
                                                                , elementAttributes = []
                                                                , elementNodes = 
                                                                    [ NodeContent ( ContentText "Virginia" ) ]
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
                                            { nameLocalName = "enumeration" 
                                            , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                            , namePrefix = Just "xsd" 
                                            } 
                                        , elementAttributes = 
                                            [ 
                                                ( Name 
                                                    { nameLocalName = "value" 
                                                    , nameNamespace = Nothing
                                                    , namePrefix = Nothing
                                                    } 
                                                , [ ContentText "WA" ]
                                                ) 
                                            ]
                                        , elementNodes = 
                                            [ NodeContent ( ContentText "" )
                                            , NodeElement 
                                                ( Element 
                                                    { elementName = Name 
                                                        { nameLocalName = "annotation" 
                                                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                                        , namePrefix = Just "xsd" 
                                                        } 
                                                    , elementAttributes = []
                                                    , elementNodes = 
                                                        [ NodeContent ( ContentText "" )
                                                        , NodeElement 
                                                            ( Element 
                                                                { elementName = Name 
                                                                    { nameLocalName = "documentation" 
                                                                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                                                    , namePrefix = Just "xsd" 
                                                                    } 
                                                                , elementAttributes = []
                                                                , elementNodes = 
                                                                    [ NodeContent ( ContentText "Washington" ) ]
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
                                            { nameLocalName = "enumeration" 
                                            , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                            , namePrefix = Just "xsd" 
                                            } 
                                        , elementAttributes = 
                                            [ 
                                                ( Name 
                                                    { nameLocalName = "value" 
                                                    , nameNamespace = Nothing
                                                    , namePrefix = Nothing
                                                    } 
                                                , [ ContentText "WV" ]
                                                ) 
                                            ]
                                        , elementNodes = 
                                            [ NodeContent ( ContentText "" )
                                            , NodeElement 
                                                ( Element 
                                                    { elementName = Name 
                                                        { nameLocalName = "annotation" 
                                                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                                        , namePrefix = Just "xsd" 
                                                        } 
                                                    , elementAttributes = []
                                                    , elementNodes = 
                                                        [ NodeContent ( ContentText "" )
                                                        , NodeElement 
                                                            ( Element 
                                                                { elementName = Name 
                                                                    { nameLocalName = "documentation" 
                                                                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                                                    , namePrefix = Just "xsd" 
                                                                    } 
                                                                , elementAttributes = []
                                                                , elementNodes = 
                                                                    [ NodeContent ( ContentText "West Virginia" ) ]
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
                                            { nameLocalName = "enumeration" 
                                            , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                            , namePrefix = Just "xsd" 
                                            } 
                                        , elementAttributes = 
                                            [ 
                                                ( Name 
                                                    { nameLocalName = "value" 
                                                    , nameNamespace = Nothing
                                                    , namePrefix = Nothing
                                                    } 
                                                , [ ContentText "WI" ]
                                                ) 
                                            ]
                                        , elementNodes = 
                                            [ NodeContent ( ContentText "" )
                                            , NodeElement 
                                                ( Element 
                                                    { elementName = Name 
                                                        { nameLocalName = "annotation" 
                                                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                                        , namePrefix = Just "xsd" 
                                                        } 
                                                    , elementAttributes = []
                                                    , elementNodes = 
                                                        [ NodeContent ( ContentText "" )
                                                        , NodeElement 
                                                            ( Element 
                                                                { elementName = Name 
                                                                    { nameLocalName = "documentation" 
                                                                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                                                    , namePrefix = Just "xsd" 
                                                                    } 
                                                                , elementAttributes = []
                                                                , elementNodes = 
                                                                    [ NodeContent ( ContentText "Wisconsin" ) ]
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
                                            { nameLocalName = "enumeration" 
                                            , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                            , namePrefix = Just "xsd" 
                                            } 
                                        , elementAttributes = 
                                            [ 
                                                ( Name 
                                                    { nameLocalName = "value" 
                                                    , nameNamespace = Nothing
                                                    , namePrefix = Nothing
                                                    } 
                                                , [ ContentText "WY" ]
                                                ) 
                                            ]
                                        , elementNodes = 
                                            [ NodeContent ( ContentText "" )
                                            , NodeElement 
                                                ( Element 
                                                    { elementName = Name 
                                                        { nameLocalName = "annotation" 
                                                        , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                                        , namePrefix = Just "xsd" 
                                                        } 
                                                    , elementAttributes = []
                                                    , elementNodes = 
                                                        [ NodeContent ( ContentText "" )
                                                        , NodeElement 
                                                            ( Element 
                                                                { elementName = Name 
                                                                    { nameLocalName = "documentation" 
                                                                    , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                                                    , namePrefix = Just "xsd" 
                                                                    } 
                                                                , elementAttributes = []
                                                                , elementNodes = 
                                                                    [ NodeContent ( ContentText "Wyoming" ) ]
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
                    , namePrefix = Just "xsd" 
                    } 
                , elementAttributes = 
                    [ 
                        ( Name 
                            { nameLocalName = "name" 
                            , nameNamespace = Nothing
                            , namePrefix = Nothing
                            } 
                        , [ ContentText "USZIPCdType" ]
                        ) 
                    ]
                , elementNodes = 
                    [ NodeContent ( ContentText "" )
                    , NodeElement 
                        ( Element 
                            { elementName = Name 
                                { nameLocalName = "annotation" 
                                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                , namePrefix = Just "xsd" 
                                } 
                            , elementAttributes = []
                            , elementNodes = 
                                [ NodeContent ( ContentText "" )
                                , NodeElement 
                                    ( Element 
                                        { elementName = Name 
                                            { nameLocalName = "documentation" 
                                            , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                            , namePrefix = Just "xsd" 
                                            } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                            [ NodeContent ( ContentText "" )
                                            , NodeElement 
                                                ( Element 
                                                    { elementName = Name 
                                                        { nameLocalName = "Component" 
                                                        , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                                        , namePrefix = Nothing
                                                        } 
                                                    , elementAttributes = []
                                                    , elementNodes = 
                                                        [ NodeContent ( ContentText "" )
                                                        , NodeElement 
                                                            ( Element 
                                                                { elementName = Name 
                                                                    { nameLocalName = "DictionaryEntryNm" 
                                                                    , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                                                    , namePrefix = Nothing
                                                                    } 
                                                                , elementAttributes = []
                                                                , elementNodes = 
                                                                    [ NodeContent ( ContentText "United States Zip Code Type" ) ]
                                                                } 
                                                            )
                                                        , NodeContent ( ContentText "" )
                                                        , NodeElement 
                                                            ( Element 
                                                                { elementName = Name 
                                                                    { nameLocalName = "MajorVersionNum" 
                                                                    , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                                                    , namePrefix = Nothing
                                                                    } 
                                                                , elementAttributes = []
                                                                , elementNodes = 
                                                                    [ NodeContent ( ContentText "1" ) ]
                                                                } 
                                                            )
                                                        , NodeContent ( ContentText "" )
                                                        , NodeElement 
                                                            ( Element 
                                                                { elementName = Name 
                                                                    { nameLocalName = "MinorVersionNum" 
                                                                    , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                                                    , namePrefix = Nothing
                                                                    } 
                                                                , elementAttributes = []
                                                                , elementNodes = 
                                                                    [ NodeContent ( ContentText "0" ) ]
                                                                } 
                                                            )
                                                        , NodeContent ( ContentText "" )
                                                        , NodeElement 
                                                            ( Element 
                                                                { elementName = Name 
                                                                    { nameLocalName = "VersionEffectiveBeginDt" 
                                                                    , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                                                    , namePrefix = Nothing
                                                                    } 
                                                                , elementAttributes = []
                                                                , elementNodes = 
                                                                    [ NodeContent ( ContentText "2013-03-05" ) ]
                                                                } 
                                                            )
                                                        , NodeContent ( ContentText "" )
                                                        , NodeElement 
                                                            ( Element 
                                                                { elementName = Name 
                                                                    { nameLocalName = "VersionDescriptionTxt" 
                                                                    , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                                                    , namePrefix = Nothing
                                                                    } 
                                                                , elementAttributes = []
                                                                , elementNodes = 
                                                                    [ NodeContent ( ContentText "Initial Version" ) ]
                                                                } 
                                                            )
                                                        , NodeContent ( ContentText "" )
                                                        , NodeElement 
                                                            ( Element 
                                                                { elementName = Name 
                                                                    { nameLocalName = "DescriptionTxt" 
                                                                    , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                                                    , namePrefix = Nothing
                                                                    } 
                                                                , elementAttributes = []
                                                                , elementNodes = 
                                                                    [ NodeContent ( ContentText "ZIP Code - 5 digits plus" ) ]
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
                                { nameLocalName = "restriction" 
                                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                , namePrefix = Just "xsd" 
                                } 
                            , elementAttributes = 
                                [ 
                                    ( Name 
                                        { nameLocalName = "base" 
                                        , nameNamespace = Nothing
                                        , namePrefix = Nothing
                                        } 
                                    , [ ContentText "xsd:string" ]
                                    ) 
                                ]
                            , elementNodes = 
                                [ NodeContent ( ContentText "" )
                                , NodeElement 
                                    ( Element 
                                        { elementName = Name 
                                            { nameLocalName = "pattern" 
                                            , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                            , namePrefix = Just "xsd" 
                                            } 
                                        , elementAttributes = 
                                            [ 
                                                ( Name 
                                                    { nameLocalName = "value" 
                                                    , nameNamespace = Nothing
                                                    , namePrefix = Nothing
                                                    } 
                                                , [ ContentText "[0-9]{5}" ]
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
                    , namePrefix = Just "xsd" 
                    } 
                , elementAttributes = 
                    [ 
                        ( Name 
                            { nameLocalName = "name" 
                            , nameNamespace = Nothing
                            , namePrefix = Nothing
                            } 
                        , [ ContentText "USZIPExtensionCdType" ]
                        ) 
                    ]
                , elementNodes = 
                    [ NodeContent ( ContentText "" )
                    , NodeElement 
                        ( Element 
                            { elementName = Name 
                                { nameLocalName = "annotation" 
                                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                , namePrefix = Just "xsd" 
                                } 
                            , elementAttributes = []
                            , elementNodes = 
                                [ NodeContent ( ContentText "" )
                                , NodeElement 
                                    ( Element 
                                        { elementName = Name 
                                            { nameLocalName = "documentation" 
                                            , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                            , namePrefix = Just "xsd" 
                                            } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                            [ NodeContent ( ContentText "" )
                                            , NodeElement 
                                                ( Element 
                                                    { elementName = Name 
                                                        { nameLocalName = "Component" 
                                                        , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                                        , namePrefix = Nothing
                                                        } 
                                                    , elementAttributes = []
                                                    , elementNodes = 
                                                        [ NodeContent ( ContentText "" )
                                                        , NodeElement 
                                                            ( Element 
                                                                { elementName = Name 
                                                                    { nameLocalName = "DictionaryEntryNm" 
                                                                    , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                                                    , namePrefix = Nothing
                                                                    } 
                                                                , elementAttributes = []
                                                                , elementNodes = 
                                                                    [ NodeContent ( ContentText "United States Zip Extension Code Type" ) ]
                                                                } 
                                                            )
                                                        , NodeContent ( ContentText "" )
                                                        , NodeElement 
                                                            ( Element 
                                                                { elementName = Name 
                                                                    { nameLocalName = "MajorVersionNum" 
                                                                    , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                                                    , namePrefix = Nothing
                                                                    } 
                                                                , elementAttributes = []
                                                                , elementNodes = 
                                                                    [ NodeContent ( ContentText "1" ) ]
                                                                } 
                                                            )
                                                        , NodeContent ( ContentText "" )
                                                        , NodeElement 
                                                            ( Element 
                                                                { elementName = Name 
                                                                    { nameLocalName = "MinorVersionNum" 
                                                                    , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                                                    , namePrefix = Nothing
                                                                    } 
                                                                , elementAttributes = []
                                                                , elementNodes = 
                                                                    [ NodeContent ( ContentText "0" ) ]
                                                                } 
                                                            )
                                                        , NodeContent ( ContentText "" )
                                                        , NodeElement 
                                                            ( Element 
                                                                { elementName = Name 
                                                                    { nameLocalName = "VersionEffectiveBeginDt" 
                                                                    , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                                                    , namePrefix = Nothing
                                                                    } 
                                                                , elementAttributes = []
                                                                , elementNodes = 
                                                                    [ NodeContent ( ContentText "2013-03-05" ) ]
                                                                } 
                                                            )
                                                        , NodeContent ( ContentText "" )
                                                        , NodeElement 
                                                            ( Element 
                                                                { elementName = Name 
                                                                    { nameLocalName = "VersionDescriptionTxt" 
                                                                    , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                                                    , namePrefix = Nothing
                                                                    } 
                                                                , elementAttributes = []
                                                                , elementNodes = 
                                                                    [ NodeContent ( ContentText "Initial Version" ) ]
                                                                } 
                                                            )
                                                        , NodeContent ( ContentText "" )
                                                        , NodeElement 
                                                            ( Element 
                                                                { elementName = Name 
                                                                    { nameLocalName = "DescriptionTxt" 
                                                                    , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                                                    , namePrefix = Nothing
                                                                    } 
                                                                , elementAttributes = []
                                                                , elementNodes = 
                                                                    [ NodeContent ( ContentText "Four digit ZIP extention Code" ) ]
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
                                { nameLocalName = "restriction" 
                                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                , namePrefix = Just "xsd" 
                                } 
                            , elementAttributes = 
                                [ 
                                    ( Name 
                                        { nameLocalName = "base" 
                                        , nameNamespace = Nothing
                                        , namePrefix = Nothing
                                        } 
                                    , [ ContentText "xsd:string" ]
                                    ) 
                                ]
                            , elementNodes = 
                                [ NodeContent ( ContentText "" )
                                , NodeElement 
                                    ( Element 
                                        { elementName = Name 
                                            { nameLocalName = "pattern" 
                                            , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                            , namePrefix = Just "xsd" 
                                            } 
                                        , elementAttributes = 
                                            [ 
                                                ( Name 
                                                    { nameLocalName = "value" 
                                                    , nameNamespace = Nothing
                                                    , namePrefix = Nothing
                                                    } 
                                                , [ ContentText "[0-9]{4}" ]
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
                    , namePrefix = Just "xsd" 
                    } 
                , elementAttributes = 
                    [ 
                        ( Name 
                            { nameLocalName = "name" 
                            , nameNamespace = Nothing
                            , namePrefix = Nothing
                            } 
                        , [ ContentText "YearType" ]
                        ) 
                    ]
                , elementNodes = 
                    [ NodeContent ( ContentText "" )
                    , NodeElement 
                        ( Element 
                            { elementName = Name 
                                { nameLocalName = "annotation" 
                                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                , namePrefix = Just "xsd" 
                                } 
                            , elementAttributes = []
                            , elementNodes = 
                                [ NodeContent ( ContentText "" )
                                , NodeElement 
                                    ( Element 
                                        { elementName = Name 
                                            { nameLocalName = "documentation" 
                                            , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                            , namePrefix = Just "xsd" 
                                            } 
                                        , elementAttributes = []
                                        , elementNodes = 
                                            [ NodeContent ( ContentText "" )
                                            , NodeElement 
                                                ( Element 
                                                    { elementName = Name 
                                                        { nameLocalName = "Component" 
                                                        , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                                        , namePrefix = Nothing
                                                        } 
                                                    , elementAttributes = []
                                                    , elementNodes = 
                                                        [ NodeContent ( ContentText "" )
                                                        , NodeElement 
                                                            ( Element 
                                                                { elementName = Name 
                                                                    { nameLocalName = "DictionaryEntryNm" 
                                                                    , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                                                    , namePrefix = Nothing
                                                                    } 
                                                                , elementAttributes = []
                                                                , elementNodes = 
                                                                    [ NodeContent ( ContentText "Year Type" ) ]
                                                                } 
                                                            )
                                                        , NodeContent ( ContentText "" )
                                                        , NodeElement 
                                                            ( Element 
                                                                { elementName = Name 
                                                                    { nameLocalName = "MajorVersionNum" 
                                                                    , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                                                    , namePrefix = Nothing
                                                                    } 
                                                                , elementAttributes = []
                                                                , elementNodes = 
                                                                    [ NodeContent ( ContentText "1" ) ]
                                                                } 
                                                            )
                                                        , NodeContent ( ContentText "" )
                                                        , NodeElement 
                                                            ( Element 
                                                                { elementName = Name 
                                                                    { nameLocalName = "MinorVersionNum" 
                                                                    , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                                                    , namePrefix = Nothing
                                                                    } 
                                                                , elementAttributes = []
                                                                , elementNodes = 
                                                                    [ NodeContent ( ContentText "0" ) ]
                                                                } 
                                                            )
                                                        , NodeContent ( ContentText "" )
                                                        , NodeElement 
                                                            ( Element 
                                                                { elementName = Name 
                                                                    { nameLocalName = "VersionEffectiveBeginDt" 
                                                                    , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                                                    , namePrefix = Nothing
                                                                    } 
                                                                , elementAttributes = []
                                                                , elementNodes = 
                                                                    [ NodeContent ( ContentText "2008-01-08" ) ]
                                                                } 
                                                            )
                                                        , NodeContent ( ContentText "" )
                                                        , NodeElement 
                                                            ( Element 
                                                                { elementName = Name 
                                                                    { nameLocalName = "VersionDescriptionTxt" 
                                                                    , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                                                    , namePrefix = Nothing
                                                                    } 
                                                                , elementAttributes = []
                                                                , elementNodes = 
                                                                    [ NodeContent ( ContentText "Initial version" ) ]
                                                                } 
                                                            )
                                                        , NodeContent ( ContentText "" )
                                                        , NodeElement 
                                                            ( Element 
                                                                { elementName = Name 
                                                                    { nameLocalName = "DescriptionTxt" 
                                                                    , nameNamespace = Just "urn:us:gov:treasury:irs:common" 
                                                                    , namePrefix = Nothing
                                                                    } 
                                                                , elementAttributes = []
                                                                , elementNodes = 
                                                                    [ NodeContent ( ContentText "Base type for a year in the format of YYYY" ) ]
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
                                { nameLocalName = "restriction" 
                                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                , namePrefix = Just "xsd" 
                                } 
                            , elementAttributes = 
                                [ 
                                    ( Name 
                                        { nameLocalName = "base" 
                                        , nameNamespace = Nothing
                                        , namePrefix = Nothing
                                        } 
                                    , [ ContentText "xsd:gYear" ]
                                    ) 
                                ]
                            , elementNodes = 
                                [ NodeContent ( ContentText "" )
                                , NodeElement 
                                    ( Element 
                                        { elementName = Name 
                                            { nameLocalName = "minInclusive" 
                                            , nameNamespace = Just "http://www.w3.org/2001/XMLSchema" 
                                            , namePrefix = Just "xsd" 
                                            } 
                                        , elementAttributes = 
                                            [ 
                                                ( Name 
                                                    { nameLocalName = "value" 
                                                    , nameNamespace = Nothing
                                                    , namePrefix = Nothing
                                                    } 
                                                , [ ContentText "1000" ]
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
                                            , namePrefix = Just "xsd" 
                                            } 
                                        , elementAttributes = 
                                            [ 
                                                ( Name 
                                                    { nameLocalName = "value" 
                                                    , nameNamespace = Nothing
                                                    , namePrefix = Nothing
                                                    } 
                                                , [ ContentText "9999" ]
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
, documentEpilogue = []
} 
