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
  , elementAttributes = fromList
    [
     ( Name
       { nameLocalName = "version"
       , nameNamespace = Nothing
       , namePrefix = Nothing
       }
     , "1.0"
     )
    ,
     ( Name
       { nameLocalName = "xmlns:xdb"
       , nameNamespace = Nothing
       , namePrefix = Nothing
       }
     , "http://xmlns.oracle.com/xdb"
     )
    ,
     ( Name
       { nameLocalName = "xmlns:xs"
       , nameNamespace = Nothing
       , namePrefix = Nothing
       }
     , "http://www.w3.org/2001/XMLSchema"
     )
    ,
     ( Name
       { nameLocalName = "storeVarrayAsTable"
       , nameNamespace = Just "http://xmlns.oracle.com/xdb"
       , namePrefix = Just "xdb"
       }
     , "true"
     )
    ]
  , elementNodes =
    [ NodeContent ""
    , NodeElement
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
           , "PurchaseOrder"
           )
          ,
           ( Name
             { nameLocalName = "type"
             , nameNamespace = Nothing
             , namePrefix = Nothing
             }
           , "PurchaseOrderType"
           )
          ,
           ( Name
             { nameLocalName = "defaultTable"
             , nameNamespace = Just "http://xmlns.oracle.com/xdb"
             , namePrefix = Just "xdb"
             }
           , "PURCHASEORDER"
           )
          ]
        , elementNodes = []
        }
      )
    , NodeContent ""
    , NodeElement
      ( Element
        { elementName = Name
          { nameLocalName = "complexType"
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
           , "PurchaseOrderType"
           )
          ,
           ( Name
             { nameLocalName = "SQLType"
             , nameNamespace = Just "http://xmlns.oracle.com/xdb"
             , namePrefix = Just "xdb"
             }
           , "PURCHASEORDER_T"
           )
          ]
        , elementNodes =
          [ NodeContent ""
          , NodeElement
            ( Element
              { elementName = Name
                { nameLocalName = "sequence"
                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                , namePrefix = Just "xs"
                }
              , elementAttributes = fromList []
              , elementNodes =
                [ NodeContent ""
                , NodeElement
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
                       , "1"
                       )
                      ,
                       ( Name
                         { nameLocalName = "name"
                         , nameNamespace = Nothing
                         , namePrefix = Nothing
                         }
                       , "Reference"
                       )
                      ,
                       ( Name
                         { nameLocalName = "type"
                         , nameNamespace = Nothing
                         , namePrefix = Nothing
                         }
                       , "ReferenceType"
                       )
                      ,
                       ( Name
                         { nameLocalName = "SQLName"
                         , nameNamespace = Just "http://xmlns.oracle.com/xdb"
                         , namePrefix = Just "xdb"
                         }
                       , "REFERENCE"
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
                      , namePrefix = Just "xs"
                      }
                    , elementAttributes = fromList
                      [
                       ( Name
                         { nameLocalName = "name"
                         , nameNamespace = Nothing
                         , namePrefix = Nothing
                         }
                       , "Actions"
                       )
                      ,
                       ( Name
                         { nameLocalName = "type"
                         , nameNamespace = Nothing
                         , namePrefix = Nothing
                         }
                       , "ActionsType"
                       )
                      ,
                       ( Name
                         { nameLocalName = "SQLName"
                         , nameNamespace = Just "http://xmlns.oracle.com/xdb"
                         , namePrefix = Just "xdb"
                         }
                       , "ACTIONS"
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
                       , "Reject"
                       )
                      ,
                       ( Name
                         { nameLocalName = "type"
                         , nameNamespace = Nothing
                         , namePrefix = Nothing
                         }
                       , "RejectionType"
                       )
                      ,
                       ( Name
                         { nameLocalName = "SQLName"
                         , nameNamespace = Just "http://xmlns.oracle.com/xdb"
                         , namePrefix = Just "xdb"
                         }
                       , "REJECTION"
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
                      , namePrefix = Just "xs"
                      }
                    , elementAttributes = fromList
                      [
                       ( Name
                         { nameLocalName = "name"
                         , nameNamespace = Nothing
                         , namePrefix = Nothing
                         }
                       , "Requestor"
                       )
                      ,
                       ( Name
                         { nameLocalName = "type"
                         , nameNamespace = Nothing
                         , namePrefix = Nothing
                         }
                       , "RequestorType"
                       )
                      ,
                       ( Name
                         { nameLocalName = "SQLName"
                         , nameNamespace = Just "http://xmlns.oracle.com/xdb"
                         , namePrefix = Just "xdb"
                         }
                       , "REQUESTOR"
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
                      , namePrefix = Just "xs"
                      }
                    , elementAttributes = fromList
                      [
                       ( Name
                         { nameLocalName = "minOccurs"
                         , nameNamespace = Nothing
                         , namePrefix = Nothing
                         }
                       , "1"
                       )
                      ,
                       ( Name
                         { nameLocalName = "name"
                         , nameNamespace = Nothing
                         , namePrefix = Nothing
                         }
                       , "User"
                       )
                      ,
                       ( Name
                         { nameLocalName = "type"
                         , nameNamespace = Nothing
                         , namePrefix = Nothing
                         }
                       , "UserType"
                       )
                      ,
                       ( Name
                         { nameLocalName = "SQLName"
                         , nameNamespace = Just "http://xmlns.oracle.com/xdb"
                         , namePrefix = Just "xdb"
                         }
                       , "USERID"
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
                      , namePrefix = Just "xs"
                      }
                    , elementAttributes = fromList
                      [
                       ( Name
                         { nameLocalName = "name"
                         , nameNamespace = Nothing
                         , namePrefix = Nothing
                         }
                       , "CostCenter"
                       )
                      ,
                       ( Name
                         { nameLocalName = "type"
                         , nameNamespace = Nothing
                         , namePrefix = Nothing
                         }
                       , "CostCenterType"
                       )
                      ,
                       ( Name
                         { nameLocalName = "SQLName"
                         , nameNamespace = Just "http://xmlns.oracle.com/xdb"
                         , namePrefix = Just "xdb"
                         }
                       , "COST_CENTER"
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
                      , namePrefix = Just "xs"
                      }
                    , elementAttributes = fromList
                      [
                       ( Name
                         { nameLocalName = "name"
                         , nameNamespace = Nothing
                         , namePrefix = Nothing
                         }
                       , "ShippingInstructions"
                       )
                      ,
                       ( Name
                         { nameLocalName = "type"
                         , nameNamespace = Nothing
                         , namePrefix = Nothing
                         }
                       , "ShippingInstructionsType"
                       )
                      ,
                       ( Name
                         { nameLocalName = "SQLName"
                         , nameNamespace = Just "http://xmlns.oracle.com/xdb"
                         , namePrefix = Just "xdb"
                         }
                       , "SHIPPING_INSTRUCTIONS"
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
                      , namePrefix = Just "xs"
                      }
                    , elementAttributes = fromList
                      [
                       ( Name
                         { nameLocalName = "name"
                         , nameNamespace = Nothing
                         , namePrefix = Nothing
                         }
                       , "SpecialInstructions"
                       )
                      ,
                       ( Name
                         { nameLocalName = "type"
                         , nameNamespace = Nothing
                         , namePrefix = Nothing
                         }
                       , "SpecialInstructionsType"
                       )
                      ,
                       ( Name
                         { nameLocalName = "SQLName"
                         , nameNamespace = Just "http://xmlns.oracle.com/xdb"
                         , namePrefix = Just "xdb"
                         }
                       , "SPECIAL_INSTRUCTIONS"
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
                      , namePrefix = Just "xs"
                      }
                    , elementAttributes = fromList
                      [
                       ( Name
                         { nameLocalName = "name"
                         , nameNamespace = Nothing
                         , namePrefix = Nothing
                         }
                       , "LineItems"
                       )
                      ,
                       ( Name
                         { nameLocalName = "type"
                         , nameNamespace = Nothing
                         , namePrefix = Nothing
                         }
                       , "LineItemsType"
                       )
                      ,
                       ( Name
                         { nameLocalName = "SQLName"
                         , nameNamespace = Just "http://xmlns.oracle.com/xdb"
                         , namePrefix = Just "xdb"
                         }
                       , "LINEITEMS"
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
          , namePrefix = Just "xs"
          }
        , elementAttributes = fromList
          [
           ( Name
             { nameLocalName = "name"
             , nameNamespace = Nothing
             , namePrefix = Nothing
             }
           , "LineItemsType"
           )
          ,
           ( Name
             { nameLocalName = "SQLType"
             , nameNamespace = Just "http://xmlns.oracle.com/xdb"
             , namePrefix = Just "xdb"
             }
           , "LINEITEMS_T"
           )
          ]
        , elementNodes =
          [ NodeContent ""
          , NodeElement
            ( Element
              { elementName = Name
                { nameLocalName = "sequence"
                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                , namePrefix = Just "xs"
                }
              , elementAttributes = fromList []
              , elementNodes =
                [ NodeContent ""
                , NodeElement
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
                       , "LineItem"
                       )
                      ,
                       ( Name
                         { nameLocalName = "type"
                         , nameNamespace = Nothing
                         , namePrefix = Nothing
                         }
                       , "LineItemType"
                       )
                      ,
                       ( Name
                         { nameLocalName = "SQLCollType"
                         , nameNamespace = Just "http://xmlns.oracle.com/xdb"
                         , namePrefix = Just "xdb"
                         }
                       , "LINEITEM_V"
                       )
                      ,
                       ( Name
                         { nameLocalName = "SQLName"
                         , nameNamespace = Just "http://xmlns.oracle.com/xdb"
                         , namePrefix = Just "xdb"
                         }
                       , "LINEITEM"
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
          , namePrefix = Just "xs"
          }
        , elementAttributes = fromList
          [
           ( Name
             { nameLocalName = "name"
             , nameNamespace = Nothing
             , namePrefix = Nothing
             }
           , "LineItemType"
           )
          ,
           ( Name
             { nameLocalName = "SQLType"
             , nameNamespace = Just "http://xmlns.oracle.com/xdb"
             , namePrefix = Just "xdb"
             }
           , "LINEITEM_T"
           )
          ]
        , elementNodes =
          [ NodeContent ""
          , NodeElement
            ( Element
              { elementName = Name
                { nameLocalName = "sequence"
                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                , namePrefix = Just "xs"
                }
              , elementAttributes = fromList []
              , elementNodes =
                [ NodeContent ""
                , NodeElement
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
                       , "Description"
                       )
                      ,
                       ( Name
                         { nameLocalName = "type"
                         , nameNamespace = Nothing
                         , namePrefix = Nothing
                         }
                       , "DescriptionType"
                       )
                      ,
                       ( Name
                         { nameLocalName = "SQLName"
                         , nameNamespace = Just "http://xmlns.oracle.com/xdb"
                         , namePrefix = Just "xdb"
                         }
                       , "DESCRIPTION"
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
                      , namePrefix = Just "xs"
                      }
                    , elementAttributes = fromList
                      [
                       ( Name
                         { nameLocalName = "name"
                         , nameNamespace = Nothing
                         , namePrefix = Nothing
                         }
                       , "Part"
                       )
                      ,
                       ( Name
                         { nameLocalName = "type"
                         , nameNamespace = Nothing
                         , namePrefix = Nothing
                         }
                       , "PartType"
                       )
                      ,
                       ( Name
                         { nameLocalName = "SQLName"
                         , nameNamespace = Just "http://xmlns.oracle.com/xdb"
                         , namePrefix = Just "xdb"
                         }
                       , "PART"
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
                , namePrefix = Just "xs"
                }
              , elementAttributes = fromList
                [
                 ( Name
                   { nameLocalName = "name"
                   , nameNamespace = Nothing
                   , namePrefix = Nothing
                   }
                 , "ItemNumber"
                 )
                ,
                 ( Name
                   { nameLocalName = "type"
                   , nameNamespace = Nothing
                   , namePrefix = Nothing
                   }
                 , "xs:integer"
                 )
                ,
                 ( Name
                   { nameLocalName = "SQLName"
                   , nameNamespace = Just "http://xmlns.oracle.com/xdb"
                   , namePrefix = Just "xdb"
                   }
                 , "ITEMNUMBER"
                 )
                ,
                 ( Name
                   { nameLocalName = "SQLType"
                   , nameNamespace = Just "http://xmlns.oracle.com/xdb"
                   , namePrefix = Just "xdb"
                   }
                 , "NUMBER"
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
          , namePrefix = Just "xs"
          }
        , elementAttributes = fromList
          [
           ( Name
             { nameLocalName = "name"
             , nameNamespace = Nothing
             , namePrefix = Nothing
             }
           , "PartType"
           )
          ,
           ( Name
             { nameLocalName = "SQLType"
             , nameNamespace = Just "http://xmlns.oracle.com/xdb"
             , namePrefix = Just "xdb"
             }
           , "PART_T"
           )
          ]
        , elementNodes =
          [ NodeContent ""
          , NodeElement
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
                 , "Id"
                 )
                ,
                 ( Name
                   { nameLocalName = "SQLName"
                   , nameNamespace = Just "http://xmlns.oracle.com/xdb"
                   , namePrefix = Just "xdb"
                   }
                 , "PART_NUMBER"
                 )
                ,
                 ( Name
                   { nameLocalName = "SQLType"
                   , nameNamespace = Just "http://xmlns.oracle.com/xdb"
                   , namePrefix = Just "xdb"
                   }
                 , "VARCHAR2"
                 )
                ]
              , elementNodes =
                [ NodeContent ""
                , NodeElement
                  ( Element
                    { elementName = Name
                      { nameLocalName = "simpleType"
                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                      , namePrefix = Just "xs"
                      }
                    , elementAttributes = fromList []
                    , elementNodes =
                      [ NodeContent ""
                      , NodeElement
                        ( Element
                          { elementName = Name
                            { nameLocalName = "restriction"
                            , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                            , namePrefix = Just "xs"
                            }
                          , elementAttributes = fromList
                            [
                             ( Name
                               { nameLocalName = "base"
                               , nameNamespace = Nothing
                               , namePrefix = Nothing
                               }
                             , "xs:string"
                             )
                            ]
                          , elementNodes =
                            [ NodeContent ""
                            , NodeElement
                              ( Element
                                { elementName = Name
                                  { nameLocalName = "minLength"
                                  , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                  , namePrefix = Just "xs"
                                  }
                                , elementAttributes = fromList
                                  [
                                   ( Name
                                     { nameLocalName = "value"
                                     , nameNamespace = Nothing
                                     , namePrefix = Nothing
                                     }
                                   , "10"
                                   )
                                  ]
                                , elementNodes = []
                                }
                              )
                            , NodeContent ""
                            , NodeElement
                              ( Element
                                { elementName = Name
                                  { nameLocalName = "maxLength"
                                  , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                  , namePrefix = Just "xs"
                                  }
                                , elementAttributes = fromList
                                  [
                                   ( Name
                                     { nameLocalName = "value"
                                     , nameNamespace = Nothing
                                     , namePrefix = Nothing
                                     }
                                   , "14"
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
                 , "Quantity"
                 )
                ,
                 ( Name
                   { nameLocalName = "type"
                   , nameNamespace = Nothing
                   , namePrefix = Nothing
                   }
                 , "quantityType"
                 )
                ,
                 ( Name
                   { nameLocalName = "SQLName"
                   , nameNamespace = Just "http://xmlns.oracle.com/xdb"
                   , namePrefix = Just "xdb"
                   }
                 , "QUANTITY"
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
                , namePrefix = Just "xs"
                }
              , elementAttributes = fromList
                [
                 ( Name
                   { nameLocalName = "name"
                   , nameNamespace = Nothing
                   , namePrefix = Nothing
                   }
                 , "UnitPrice"
                 )
                ,
                 ( Name
                   { nameLocalName = "type"
                   , nameNamespace = Nothing
                   , namePrefix = Nothing
                   }
                 , "moneyType"
                 )
                ,
                 ( Name
                   { nameLocalName = "SQLName"
                   , nameNamespace = Just "http://xmlns.oracle.com/xdb"
                   , namePrefix = Just "xdb"
                   }
                 , "UNITPRICE"
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
          { nameLocalName = "simpleType"
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
           , "ReferenceType"
           )
          ]
        , elementNodes =
          [ NodeContent ""
          , NodeElement
            ( Element
              { elementName = Name
                { nameLocalName = "restriction"
                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                , namePrefix = Just "xs"
                }
              , elementAttributes = fromList
                [
                 ( Name
                   { nameLocalName = "base"
                   , nameNamespace = Nothing
                   , namePrefix = Nothing
                   }
                 , "xs:string"
                 )
                ]
              , elementNodes =
                [ NodeContent ""
                , NodeElement
                  ( Element
                    { elementName = Name
                      { nameLocalName = "minLength"
                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                      , namePrefix = Just "xs"
                      }
                    , elementAttributes = fromList
                      [
                       ( Name
                         { nameLocalName = "value"
                         , nameNamespace = Nothing
                         , namePrefix = Nothing
                         }
                       , "18"
                       )
                      ]
                    , elementNodes = []
                    }
                  )
                , NodeContent ""
                , NodeElement
                  ( Element
                    { elementName = Name
                      { nameLocalName = "maxLength"
                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                      , namePrefix = Just "xs"
                      }
                    , elementAttributes = fromList
                      [
                       ( Name
                         { nameLocalName = "value"
                         , nameNamespace = Nothing
                         , namePrefix = Nothing
                         }
                       , "30"
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
          , namePrefix = Just "xs"
          }
        , elementAttributes = fromList
          [
           ( Name
             { nameLocalName = "name"
             , nameNamespace = Nothing
             , namePrefix = Nothing
             }
           , "ActionsType"
           )
          ,
           ( Name
             { nameLocalName = "SQLType"
             , nameNamespace = Just "http://xmlns.oracle.com/xdb"
             , namePrefix = Just "xdb"
             }
           , "ACTIONS_T"
           )
          ]
        , elementNodes =
          [ NodeContent ""
          , NodeElement
            ( Element
              { elementName = Name
                { nameLocalName = "sequence"
                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                , namePrefix = Just "xs"
                }
              , elementAttributes = fromList []
              , elementNodes =
                [ NodeContent ""
                , NodeElement
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
                       , "4"
                       )
                      ,
                       ( Name
                         { nameLocalName = "name"
                         , nameNamespace = Nothing
                         , namePrefix = Nothing
                         }
                       , "Action"
                       )
                      ,
                       ( Name
                         { nameLocalName = "SQLCollType"
                         , nameNamespace = Just "http://xmlns.oracle.com/xdb"
                         , namePrefix = Just "xdb"
                         }
                       , "ACTION_V"
                       )
                      ,
                       ( Name
                         { nameLocalName = "SQLName"
                         , nameNamespace = Just "http://xmlns.oracle.com/xdb"
                         , namePrefix = Just "xdb"
                         }
                       , "ACTION"
                       )
                      ]
                    , elementNodes =
                      [ NodeContent ""
                      , NodeElement
                        ( Element
                          { elementName = Name
                            { nameLocalName = "complexType"
                            , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                            , namePrefix = Just "xs"
                            }
                          , elementAttributes = fromList
                            [
                             ( Name
                               { nameLocalName = "SQLType"
                               , nameNamespace = Just "http://xmlns.oracle.com/xdb"
                               , namePrefix = Just "xdb"
                               }
                             , "ACTION_T"
                             )
                            ]
                          , elementNodes =
                            [ NodeContent ""
                            , NodeElement
                              ( Element
                                { elementName = Name
                                  { nameLocalName = "sequence"
                                  , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                                  , namePrefix = Just "xs"
                                  }
                                , elementAttributes = fromList []
                                , elementNodes =
                                  [ NodeContent ""
                                  , NodeElement
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
                                         , "User"
                                         )
                                        ,
                                         ( Name
                                           { nameLocalName = "type"
                                           , nameNamespace = Nothing
                                           , namePrefix = Nothing
                                           }
                                         , "UserType"
                                         )
                                        ,
                                         ( Name
                                           { nameLocalName = "SQLName"
                                           , nameNamespace = Just "http://xmlns.oracle.com/xdb"
                                           , namePrefix = Just "xdb"
                                           }
                                         , "ACTIONED_BY"
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
                                         , "Date"
                                         )
                                        ,
                                         ( Name
                                           { nameLocalName = "type"
                                           , nameNamespace = Nothing
                                           , namePrefix = Nothing
                                           }
                                         , "DateType"
                                         )
                                        ,
                                         ( Name
                                           { nameLocalName = "SQLName"
                                           , nameNamespace = Just "http://xmlns.oracle.com/xdb"
                                           , namePrefix = Just "xdb"
                                           }
                                         , "DATE_ACTIONED"
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
          ]
        }
      )
    , NodeContent ""
    , NodeElement
      ( Element
        { elementName = Name
          { nameLocalName = "complexType"
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
           , "RejectionType"
           )
          ,
           ( Name
             { nameLocalName = "SQLType"
             , nameNamespace = Just "http://xmlns.oracle.com/xdb"
             , namePrefix = Just "xdb"
             }
           , "REJECTION_T"
           )
          ]
        , elementNodes =
          [ NodeContent ""
          , NodeElement
            ( Element
              { elementName = Name
                { nameLocalName = "all"
                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                , namePrefix = Just "xs"
                }
              , elementAttributes = fromList []
              , elementNodes =
                [ NodeContent ""
                , NodeElement
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
                       , "User"
                       )
                      ,
                       ( Name
                         { nameLocalName = "type"
                         , nameNamespace = Nothing
                         , namePrefix = Nothing
                         }
                       , "UserType"
                       )
                      ,
                       ( Name
                         { nameLocalName = "SQLName"
                         , nameNamespace = Just "http://xmlns.oracle.com/xdb"
                         , namePrefix = Just "xdb"
                         }
                       , "REJECTED_BY"
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
                       , "Date"
                       )
                      ,
                       ( Name
                         { nameLocalName = "type"
                         , nameNamespace = Nothing
                         , namePrefix = Nothing
                         }
                       , "DateType"
                       )
                      ,
                       ( Name
                         { nameLocalName = "SQLName"
                         , nameNamespace = Just "http://xmlns.oracle.com/xdb"
                         , namePrefix = Just "xdb"
                         }
                       , "DATE_REJECTED"
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
                       , "Comments"
                       )
                      ,
                       ( Name
                         { nameLocalName = "type"
                         , nameNamespace = Nothing
                         , namePrefix = Nothing
                         }
                       , "CommentsType"
                       )
                      ,
                       ( Name
                         { nameLocalName = "SQLName"
                         , nameNamespace = Just "http://xmlns.oracle.com/xdb"
                         , namePrefix = Just "xdb"
                         }
                       , "REASON_REJECTED"
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
          , namePrefix = Just "xs"
          }
        , elementAttributes = fromList
          [
           ( Name
             { nameLocalName = "name"
             , nameNamespace = Nothing
             , namePrefix = Nothing
             }
           , "ShippingInstructionsType"
           )
          ,
           ( Name
             { nameLocalName = "SQLType"
             , nameNamespace = Just "http://xmlns.oracle.com/xdb"
             , namePrefix = Just "xdb"
             }
           , "SHIPPING_INSTRUCTIONS_T"
           )
          ]
        , elementNodes =
          [ NodeContent ""
          , NodeElement
            ( Element
              { elementName = Name
                { nameLocalName = "sequence"
                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                , namePrefix = Just "xs"
                }
              , elementAttributes = fromList []
              , elementNodes =
                [ NodeContent ""
                , NodeElement
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
                       , "name"
                       )
                      ,
                       ( Name
                         { nameLocalName = "type"
                         , nameNamespace = Nothing
                         , namePrefix = Nothing
                         }
                       , "NameType"
                       )
                      ,
                       ( Name
                         { nameLocalName = "SQLName"
                         , nameNamespace = Just "http://xmlns.oracle.com/xdb"
                         , namePrefix = Just "xdb"
                         }
                       , "SHIP_TO_NAME"
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
                       , "address"
                       )
                      ,
                       ( Name
                         { nameLocalName = "type"
                         , nameNamespace = Nothing
                         , namePrefix = Nothing
                         }
                       , "AddressType"
                       )
                      ,
                       ( Name
                         { nameLocalName = "SQLName"
                         , nameNamespace = Just "http://xmlns.oracle.com/xdb"
                         , namePrefix = Just "xdb"
                         }
                       , "SHIP_TO_ADDRESS"
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
                       , "telephone"
                       )
                      ,
                       ( Name
                         { nameLocalName = "type"
                         , nameNamespace = Nothing
                         , namePrefix = Nothing
                         }
                       , "TelephoneType"
                       )
                      ,
                       ( Name
                         { nameLocalName = "SQLName"
                         , nameNamespace = Just "http://xmlns.oracle.com/xdb"
                         , namePrefix = Just "xdb"
                         }
                       , "SHIP_TO_PHONE"
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
          { nameLocalName = "simpleType"
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
           , "moneyType"
           )
          ]
        , elementNodes =
          [ NodeContent ""
          , NodeElement
            ( Element
              { elementName = Name
                { nameLocalName = "restriction"
                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                , namePrefix = Just "xs"
                }
              , elementAttributes = fromList
                [
                 ( Name
                   { nameLocalName = "base"
                   , nameNamespace = Nothing
                   , namePrefix = Nothing
                   }
                 , "xs:decimal"
                 )
                ]
              , elementNodes =
                [ NodeContent ""
                , NodeElement
                  ( Element
                    { elementName = Name
                      { nameLocalName = "fractionDigits"
                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                      , namePrefix = Just "xs"
                      }
                    , elementAttributes = fromList
                      [
                       ( Name
                         { nameLocalName = "value"
                         , nameNamespace = Nothing
                         , namePrefix = Nothing
                         }
                       , "2"
                       )
                      ]
                    , elementNodes = []
                    }
                  )
                , NodeContent ""
                , NodeElement
                  ( Element
                    { elementName = Name
                      { nameLocalName = "totalDigits"
                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                      , namePrefix = Just "xs"
                      }
                    , elementAttributes = fromList
                      [
                       ( Name
                         { nameLocalName = "value"
                         , nameNamespace = Nothing
                         , namePrefix = Nothing
                         }
                       , "12"
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
          { nameLocalName = "simpleType"
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
           , "quantityType"
           )
          ]
        , elementNodes =
          [ NodeContent ""
          , NodeElement
            ( Element
              { elementName = Name
                { nameLocalName = "restriction"
                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                , namePrefix = Just "xs"
                }
              , elementAttributes = fromList
                [
                 ( Name
                   { nameLocalName = "base"
                   , nameNamespace = Nothing
                   , namePrefix = Nothing
                   }
                 , "xs:decimal"
                 )
                ]
              , elementNodes =
                [ NodeContent ""
                , NodeElement
                  ( Element
                    { elementName = Name
                      { nameLocalName = "fractionDigits"
                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                      , namePrefix = Just "xs"
                      }
                    , elementAttributes = fromList
                      [
                       ( Name
                         { nameLocalName = "value"
                         , nameNamespace = Nothing
                         , namePrefix = Nothing
                         }
                       , "4"
                       )
                      ]
                    , elementNodes = []
                    }
                  )
                , NodeContent ""
                , NodeElement
                  ( Element
                    { elementName = Name
                      { nameLocalName = "totalDigits"
                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                      , namePrefix = Just "xs"
                      }
                    , elementAttributes = fromList
                      [
                       ( Name
                         { nameLocalName = "value"
                         , nameNamespace = Nothing
                         , namePrefix = Nothing
                         }
                       , "8"
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
          { nameLocalName = "simpleType"
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
           , "UserType"
           )
          ]
        , elementNodes =
          [ NodeContent ""
          , NodeElement
            ( Element
              { elementName = Name
                { nameLocalName = "restriction"
                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                , namePrefix = Just "xs"
                }
              , elementAttributes = fromList
                [
                 ( Name
                   { nameLocalName = "base"
                   , nameNamespace = Nothing
                   , namePrefix = Nothing
                   }
                 , "xs:string"
                 )
                ]
              , elementNodes =
                [ NodeContent ""
                , NodeElement
                  ( Element
                    { elementName = Name
                      { nameLocalName = "minLength"
                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                      , namePrefix = Just "xs"
                      }
                    , elementAttributes = fromList
                      [
                       ( Name
                         { nameLocalName = "value"
                         , nameNamespace = Nothing
                         , namePrefix = Nothing
                         }
                       , "0"
                       )
                      ]
                    , elementNodes = []
                    }
                  )
                , NodeContent ""
                , NodeElement
                  ( Element
                    { elementName = Name
                      { nameLocalName = "maxLength"
                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                      , namePrefix = Just "xs"
                      }
                    , elementAttributes = fromList
                      [
                       ( Name
                         { nameLocalName = "value"
                         , nameNamespace = Nothing
                         , namePrefix = Nothing
                         }
                       , "10"
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
          { nameLocalName = "simpleType"
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
           , "RequestorType"
           )
          ]
        , elementNodes =
          [ NodeContent ""
          , NodeElement
            ( Element
              { elementName = Name
                { nameLocalName = "restriction"
                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                , namePrefix = Just "xs"
                }
              , elementAttributes = fromList
                [
                 ( Name
                   { nameLocalName = "base"
                   , nameNamespace = Nothing
                   , namePrefix = Nothing
                   }
                 , "xs:string"
                 )
                ]
              , elementNodes =
                [ NodeContent ""
                , NodeElement
                  ( Element
                    { elementName = Name
                      { nameLocalName = "minLength"
                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                      , namePrefix = Just "xs"
                      }
                    , elementAttributes = fromList
                      [
                       ( Name
                         { nameLocalName = "value"
                         , nameNamespace = Nothing
                         , namePrefix = Nothing
                         }
                       , "0"
                       )
                      ]
                    , elementNodes = []
                    }
                  )
                , NodeContent ""
                , NodeElement
                  ( Element
                    { elementName = Name
                      { nameLocalName = "maxLength"
                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                      , namePrefix = Just "xs"
                      }
                    , elementAttributes = fromList
                      [
                       ( Name
                         { nameLocalName = "value"
                         , nameNamespace = Nothing
                         , namePrefix = Nothing
                         }
                       , "128"
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
          { nameLocalName = "simpleType"
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
           , "CostCenterType"
           )
          ]
        , elementNodes =
          [ NodeContent ""
          , NodeElement
            ( Element
              { elementName = Name
                { nameLocalName = "restriction"
                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                , namePrefix = Just "xs"
                }
              , elementAttributes = fromList
                [
                 ( Name
                   { nameLocalName = "base"
                   , nameNamespace = Nothing
                   , namePrefix = Nothing
                   }
                 , "xs:string"
                 )
                ]
              , elementNodes =
                [ NodeContent ""
                , NodeElement
                  ( Element
                    { elementName = Name
                      { nameLocalName = "minLength"
                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                      , namePrefix = Just "xs"
                      }
                    , elementAttributes = fromList
                      [
                       ( Name
                         { nameLocalName = "value"
                         , nameNamespace = Nothing
                         , namePrefix = Nothing
                         }
                       , "1"
                       )
                      ]
                    , elementNodes = []
                    }
                  )
                , NodeContent ""
                , NodeElement
                  ( Element
                    { elementName = Name
                      { nameLocalName = "maxLength"
                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                      , namePrefix = Just "xs"
                      }
                    , elementAttributes = fromList
                      [
                       ( Name
                         { nameLocalName = "value"
                         , nameNamespace = Nothing
                         , namePrefix = Nothing
                         }
                       , "4"
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
          { nameLocalName = "simpleType"
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
           , "VendorType"
           )
          ]
        , elementNodes =
          [ NodeContent ""
          , NodeElement
            ( Element
              { elementName = Name
                { nameLocalName = "restriction"
                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                , namePrefix = Just "xs"
                }
              , elementAttributes = fromList
                [
                 ( Name
                   { nameLocalName = "base"
                   , nameNamespace = Nothing
                   , namePrefix = Nothing
                   }
                 , "xs:string"
                 )
                ]
              , elementNodes =
                [ NodeContent ""
                , NodeElement
                  ( Element
                    { elementName = Name
                      { nameLocalName = "minLength"
                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                      , namePrefix = Just "xs"
                      }
                    , elementAttributes = fromList
                      [
                       ( Name
                         { nameLocalName = "value"
                         , nameNamespace = Nothing
                         , namePrefix = Nothing
                         }
                       , "0"
                       )
                      ]
                    , elementNodes = []
                    }
                  )
                , NodeContent ""
                , NodeElement
                  ( Element
                    { elementName = Name
                      { nameLocalName = "maxLength"
                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                      , namePrefix = Just "xs"
                      }
                    , elementAttributes = fromList
                      [
                       ( Name
                         { nameLocalName = "value"
                         , nameNamespace = Nothing
                         , namePrefix = Nothing
                         }
                       , "20"
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
          { nameLocalName = "simpleType"
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
           , "PurchaseOrderNumberType"
           )
          ]
        , elementNodes =
          [ NodeContent ""
          , NodeElement
            ( Element
              { elementName = Name
                { nameLocalName = "restriction"
                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                , namePrefix = Just "xs"
                }
              , elementAttributes = fromList
                [
                 ( Name
                   { nameLocalName = "base"
                   , nameNamespace = Nothing
                   , namePrefix = Nothing
                   }
                 , "xs:integer"
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
          { nameLocalName = "simpleType"
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
           , "SpecialInstructionsType"
           )
          ]
        , elementNodes =
          [ NodeContent ""
          , NodeElement
            ( Element
              { elementName = Name
                { nameLocalName = "restriction"
                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                , namePrefix = Just "xs"
                }
              , elementAttributes = fromList
                [
                 ( Name
                   { nameLocalName = "base"
                   , nameNamespace = Nothing
                   , namePrefix = Nothing
                   }
                 , "xs:string"
                 )
                ]
              , elementNodes =
                [ NodeContent ""
                , NodeElement
                  ( Element
                    { elementName = Name
                      { nameLocalName = "minLength"
                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                      , namePrefix = Just "xs"
                      }
                    , elementAttributes = fromList
                      [
                       ( Name
                         { nameLocalName = "value"
                         , nameNamespace = Nothing
                         , namePrefix = Nothing
                         }
                       , "0"
                       )
                      ]
                    , elementNodes = []
                    }
                  )
                , NodeContent ""
                , NodeElement
                  ( Element
                    { elementName = Name
                      { nameLocalName = "maxLength"
                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                      , namePrefix = Just "xs"
                      }
                    , elementAttributes = fromList
                      [
                       ( Name
                         { nameLocalName = "value"
                         , nameNamespace = Nothing
                         , namePrefix = Nothing
                         }
                       , "2048"
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
          { nameLocalName = "simpleType"
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
           , "NameType"
           )
          ]
        , elementNodes =
          [ NodeContent ""
          , NodeElement
            ( Element
              { elementName = Name
                { nameLocalName = "restriction"
                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                , namePrefix = Just "xs"
                }
              , elementAttributes = fromList
                [
                 ( Name
                   { nameLocalName = "base"
                   , nameNamespace = Nothing
                   , namePrefix = Nothing
                   }
                 , "xs:string"
                 )
                ]
              , elementNodes =
                [ NodeContent ""
                , NodeElement
                  ( Element
                    { elementName = Name
                      { nameLocalName = "minLength"
                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                      , namePrefix = Just "xs"
                      }
                    , elementAttributes = fromList
                      [
                       ( Name
                         { nameLocalName = "value"
                         , nameNamespace = Nothing
                         , namePrefix = Nothing
                         }
                       , "1"
                       )
                      ]
                    , elementNodes = []
                    }
                  )
                , NodeContent ""
                , NodeElement
                  ( Element
                    { elementName = Name
                      { nameLocalName = "maxLength"
                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                      , namePrefix = Just "xs"
                      }
                    , elementAttributes = fromList
                      [
                       ( Name
                         { nameLocalName = "value"
                         , nameNamespace = Nothing
                         , namePrefix = Nothing
                         }
                       , "20"
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
          { nameLocalName = "simpleType"
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
           , "AddressType"
           )
          ]
        , elementNodes =
          [ NodeContent ""
          , NodeElement
            ( Element
              { elementName = Name
                { nameLocalName = "restriction"
                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                , namePrefix = Just "xs"
                }
              , elementAttributes = fromList
                [
                 ( Name
                   { nameLocalName = "base"
                   , nameNamespace = Nothing
                   , namePrefix = Nothing
                   }
                 , "xs:string"
                 )
                ]
              , elementNodes =
                [ NodeContent ""
                , NodeElement
                  ( Element
                    { elementName = Name
                      { nameLocalName = "minLength"
                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                      , namePrefix = Just "xs"
                      }
                    , elementAttributes = fromList
                      [
                       ( Name
                         { nameLocalName = "value"
                         , nameNamespace = Nothing
                         , namePrefix = Nothing
                         }
                       , "1"
                       )
                      ]
                    , elementNodes = []
                    }
                  )
                , NodeContent ""
                , NodeElement
                  ( Element
                    { elementName = Name
                      { nameLocalName = "maxLength"
                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                      , namePrefix = Just "xs"
                      }
                    , elementAttributes = fromList
                      [
                       ( Name
                         { nameLocalName = "value"
                         , nameNamespace = Nothing
                         , namePrefix = Nothing
                         }
                       , "256"
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
          { nameLocalName = "simpleType"
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
           , "TelephoneType"
           )
          ]
        , elementNodes =
          [ NodeContent ""
          , NodeElement
            ( Element
              { elementName = Name
                { nameLocalName = "restriction"
                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                , namePrefix = Just "xs"
                }
              , elementAttributes = fromList
                [
                 ( Name
                   { nameLocalName = "base"
                   , nameNamespace = Nothing
                   , namePrefix = Nothing
                   }
                 , "xs:string"
                 )
                ]
              , elementNodes =
                [ NodeContent ""
                , NodeElement
                  ( Element
                    { elementName = Name
                      { nameLocalName = "minLength"
                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                      , namePrefix = Just "xs"
                      }
                    , elementAttributes = fromList
                      [
                       ( Name
                         { nameLocalName = "value"
                         , nameNamespace = Nothing
                         , namePrefix = Nothing
                         }
                       , "1"
                       )
                      ]
                    , elementNodes = []
                    }
                  )
                , NodeContent ""
                , NodeElement
                  ( Element
                    { elementName = Name
                      { nameLocalName = "maxLength"
                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                      , namePrefix = Just "xs"
                      }
                    , elementAttributes = fromList
                      [
                       ( Name
                         { nameLocalName = "value"
                         , nameNamespace = Nothing
                         , namePrefix = Nothing
                         }
                       , "24"
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
          { nameLocalName = "simpleType"
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
           , "DateType"
           )
          ]
        , elementNodes =
          [ NodeContent ""
          , NodeElement
            ( Element
              { elementName = Name
                { nameLocalName = "restriction"
                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                , namePrefix = Just "xs"
                }
              , elementAttributes = fromList
                [
                 ( Name
                   { nameLocalName = "base"
                   , nameNamespace = Nothing
                   , namePrefix = Nothing
                   }
                 , "xs:date"
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
          { nameLocalName = "simpleType"
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
           , "CommentsType"
           )
          ]
        , elementNodes =
          [ NodeContent ""
          , NodeElement
            ( Element
              { elementName = Name
                { nameLocalName = "restriction"
                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                , namePrefix = Just "xs"
                }
              , elementAttributes = fromList
                [
                 ( Name
                   { nameLocalName = "base"
                   , nameNamespace = Nothing
                   , namePrefix = Nothing
                   }
                 , "xs:string"
                 )
                ]
              , elementNodes =
                [ NodeContent ""
                , NodeElement
                  ( Element
                    { elementName = Name
                      { nameLocalName = "minLength"
                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                      , namePrefix = Just "xs"
                      }
                    , elementAttributes = fromList
                      [
                       ( Name
                         { nameLocalName = "value"
                         , nameNamespace = Nothing
                         , namePrefix = Nothing
                         }
                       , "1"
                       )
                      ]
                    , elementNodes = []
                    }
                  )
                , NodeContent ""
                , NodeElement
                  ( Element
                    { elementName = Name
                      { nameLocalName = "maxLength"
                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                      , namePrefix = Just "xs"
                      }
                    , elementAttributes = fromList
                      [
                       ( Name
                         { nameLocalName = "value"
                         , nameNamespace = Nothing
                         , namePrefix = Nothing
                         }
                       , "2048"
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
          { nameLocalName = "simpleType"
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
           , "DescriptionType"
           )
          ]
        , elementNodes =
          [ NodeContent ""
          , NodeElement
            ( Element
              { elementName = Name
                { nameLocalName = "restriction"
                , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                , namePrefix = Just "xs"
                }
              , elementAttributes = fromList
                [
                 ( Name
                   { nameLocalName = "base"
                   , nameNamespace = Nothing
                   , namePrefix = Nothing
                   }
                 , "xs:string"
                 )
                ]
              , elementNodes =
                [ NodeContent ""
                , NodeElement
                  ( Element
                    { elementName = Name
                      { nameLocalName = "minLength"
                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                      , namePrefix = Just "xs"
                      }
                    , elementAttributes = fromList
                      [
                       ( Name
                         { nameLocalName = "value"
                         , nameNamespace = Nothing
                         , namePrefix = Nothing
                         }
                       , "1"
                       )
                      ]
                    , elementNodes = []
                    }
                  )
                , NodeContent ""
                , NodeElement
                  ( Element
                    { elementName = Name
                      { nameLocalName = "maxLength"
                      , nameNamespace = Just "http://www.w3.org/2001/XMLSchema"
                      , namePrefix = Just "xs"
                      }
                    , elementAttributes = fromList
                      [
                       ( Name
                         { nameLocalName = "value"
                         , nameNamespace = Nothing
                         , namePrefix = Nothing
                         }
                       , "256"
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
, documentEpilogue = []
}
