{-# Language ExistentialQuantification #-}
{-| Time-stamp: <2019-06-12 16:21:53 CDT>

Module      : Main
Copyright   : Robert Lee, © 2017-2019
License     : ISC

Maintainer  : robert.lee@chicago.vc
Stability   : None
Portability : non-portable (GHC extensions)

Contumacy   : Best viewed with unbroken/unwrapped 154 column display.

Description : Provide testing for lexical and value correct types for use with XML Schema 1.1.

3.3 Primitive datatypes       Support    Haskell type
    3.3.1  string             QC         Stringxs
    3.3.2  boolean            QC         Boolean
    3.3.3  decimal            QC         Decimal
    3.3.4  float              QC         Floatxs
    3.3.5  double             QC         Doublexs
    3.3.6  duration           QC         Durationxs
    3.3.7  dateTime
    3.3.8  time
    3.3.9  date
    3.3.10 gYearMonth
    3.3.11 gYear
    3.3.12 gMonthDay
    3.3.13 gDay
    3.3.14 gMonth
    3.3.15 hexBinary          QC         HexBinary
    3.3.16 base64Binary       QC         Base64Binary
    3.3.17 anyURI
    3.3.18 QName              QC         QName
    3.3.19 NOTATION           QC         NOTATION

3.4 Derived datatypes         Support    Haskell type
    3.4.1  normalizedString   QC         NormalizedString
    3.4.2  token              QC         Token
    3.4.3  language           QC         Language
    3.4.4  NMTOKEN            QC         NMTOKEN
    3.4.5  NMTOKENS                      NMTOKENS
    3.4.6  Name               QC         Name
    3.4.7  NCName             QC         NCName
    3.4.8  ID                 QC         ID
    3.4.9  IDREF              QC         IDREF
    3.4.10 IDREFS                        IDREFS
    3.4.11 ENTITY             QC         ENTITY
    3.4.12 ENTITIES                      ENTITIES
    3.4.13 integer            Unit       Integer
    3.4.14 nonPositiveInteger Unit       NonPositiveInteger
    3.4.15 negativeInteger    Unit       NegativeInteger
    3.4.16 long               Unit       Long
    3.4.17 int                Unit       Intxs
    3.4.18 short              Unit       Short
    3.4.19 byte               Unit       Byte
    3.4.20 nonNegativeInteger Unit       NonNegativeInteger
    3.4.21 unsignedLong       Unit       UnsignedLong
    3.4.22 unsignedInt        Unit       UnsignedInt
    3.4.23 unsignedShort      Unit       UnsignedShort
    3.4.24 unsignedByte       Unit       UnsignedByte
    3.4.25 positiveInteger    Unit       PositiveInteger
    3.4.26 yearMonthDuration  QC         YearMonthDuration
    3.4.27 dayTimeDuration    QC         DayTimeDuration
    3.4.28 dateTimeStamp                 DateTimeStampxs
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

-- Local Imports

import Builtin

-- Explicit Imports

import Data.Char  ( chr, isDigit, ord              )
import Data.Int   ( Int16, Int8                    )
import Data.Ix    ( inRange, range                 )
import Data.List  ( head, length, maximum, minimum )
import Data.Maybe ( fromJust                       )
import Data.Word  ( Word64, Word32, Word16, Word8  )

-- Qualified Imports

import qualified Data.ByteString           as B
import qualified Data.ByteString.Base16    as Hex
import qualified Data.ByteString.Base64    as B64
import qualified Data.Ix                   as Ix
import qualified Data.List                 as DL
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as TE
import qualified System.Environment        as SE
import qualified Test.QuickCheck           as Q
import qualified Test.QuickCheck.Gen       as Q
import qualified Test.QuickCheck.Modifiers as Q
import qualified Test.SmallCheck           as S
import qualified Test.Tasty.Options        as TO
import qualified Test.Tasty.QuickCheck     as QC
import qualified Test.Tasty.SmallCheck     as TS

-- Undisciplined Imports

import ClassyPrelude hiding (head, length, maximum, minimum)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Runners

-- End of Imports
-- -----------------------------------------------------------------------------------------------------------------------------------------------------

(?) :: Bool -> a -> a -> a
(?) True t _ = t
(?) False _ f = f

infixl 1 ?

nrpt :: anytype
nrpt = error "NRPT"

main :: IO ()
main = do
  SE.setEnv "TASTY_NUM_THREADS" "4"
  defaultMainWithIngredients [ listingTests, consoleTestReporter] $ TestGroup "all-tests" tests
  -- NB: defaultMain exits the program so anything in the do stanza following defaultMain will not run.

ramp :: Int -> Int
ramp n = n * multiplier

multiplier :: Int
multiplier = 100 -- This should be determined by an environment variable or command line option.

tests :: [TestTree]
tests =
  [ testGroup "\n\n---------------------- MultiTest QC Tests ----------------------"    multiTests
  , testGroup "\n\n---------------------- Value QC tests ----------------------"        valTests
  , testGroup "\n\n---------------------- Integer Unit tests ----------------------"    integerTests
  , testGroup "\n\n---------------------- FiniteBits Unit tests ----------------------" finiteBitsTests
  ]

-- scTests :: [TestTree]
-- scTests = [ TS.testProperty "Vacuous" True
--           ]

-- signG :: QC.Gen Char
-- signG = QC.elements "+-"

-- -----------------------------------------------------------------------------------------------------------------------------------------------------
-- This section contains the string type tests and Boolean.

-- NB: forAll :: (Show a, Testable prop) => Gen a -> (a -> prop) -> Property

prop_legal_val :: (Transformatio a, Show a, Eq a) => QC.Gen (Text, a) -> (Text -> Maybe a) -> QC.Property
prop_legal_val gen fac_base = QC.forAll gen $ \(t,a) -> fac_base t == Just a

prop_legal :: (Transformatio a, Eq a) => QC.Gen Text -> (Text -> Maybe a) -> QC.Property
prop_legal gen fac_base = QC.forAll gen (isJust . fac_base)

prop_illegal :: (Transformatio a, Eq a) => QC.Gen Text -> (Text -> Maybe a) -> QC.Property
prop_illegal gen fac_base = QC.forAll gen (isNothing . fac_base)

idempotent :: (Transformatio a, Eq a) => (a -> Text) -> Text -> Maybe a
idempotent toText candidate = case fac candidate of
                                j@(Just entity) -> j == fac (toText entity) ? j $ Nothing
                                Nothing -> Nothing

qctests :: QC.Testable testable => Int -> String -> testable -> TestTree
qctests i label tree = PlusTestOptions (TO.setOption (QC.QuickCheckTests $ ramp i)) $ QC.testProperty label tree

valTests :: [TestTree]
valTests =
  [ qctests 1 "Vacuous" True
  , qctests 100 "Boolean Legal Text"    (prop_legal_val gen_boolean_legal_val (fac               :: Text -> Maybe Boolean ))
  , qctests 100 "Boolean Idempotent"    (prop_legal_val gen_boolean_legal_val (idempotent canon  :: Text -> Maybe Boolean ))
  , qctests 100 "Boolean Legal Text WS" (prop_legal_val gen_boolean_legal_WS  (fac               :: Text -> Maybe Boolean ))
  , qctests 100 "Boolean Idempotent WS" (prop_legal_val gen_boolean_legal_WS  (idempotent scribe :: Text -> Maybe Boolean ))
  ]

-- Illegal generators must be capable of generating at least one non-xmlWhite char or the illegal suffix test will not halt.
-- Set the suffixP bool to False in calls to multi if xmlSuffix tests may result in errors.

multiTests :: [TestTree]
multiTests = join
  [ [qctests 1 "Vacuous" True]
  , multi 1000    10 defT  "Base64Binary"      gen_64_legal                gen_64_illegal                (fac :: Text -> Maybe Base64Binary      )
  , multi 100    100 defT  "Boolean"           gen_boolean_legal           gen_boolean_illegal           (fac :: Text -> Maybe Boolean           )
  , multi 1000    10 defT  "DayTimeDuration"   gen_DayTimeDuration_legal   gen_DayTimeDuration_illegal   (fac :: Text -> Maybe DayTimeDuration   )
  , multi 1000    10 defT  "Durationxs"        gen_Durationxs_legal        gen_Durationxs_illegal        (fac :: Text -> Maybe Durationxs        )
  , multi 1000    10 defT  "YearMonthDuration" gen_YearMonthDuration_legal gen_YearMonthDuration_illegal (fac :: Text -> Maybe YearMonthDuration )
  , multi 1000    10 noSuf "Decimal"           gen_Decimal_legal           gen_Decimal_illegal           (fac :: Text -> Maybe Decimal           )
  , multi 10000   10 noSuf "Doublexs"          gen_Floatxs_legal           gen_Floatxs_illegal           (fac :: Text -> Maybe Doublexs          )
  , multi 100     10 defT  "ENTITY"            gen_NCName_legal            gen_NCName_illegal            (fac :: Text -> Maybe ENTITY            )
  , multi 10000   10 noSuf "Floatxs"           gen_Floatxs_legal           gen_Floatxs_illegal           (fac :: Text -> Maybe Floatxs           )
  , multi 1000    10 defT  "HexBinary"         gen_hex_legal               gen_hex_illegal               (fac :: Text -> Maybe HexBinary         )
  , multi 100     10 defT  "ID"                gen_NCName_legal            gen_NCName_illegal            (fac :: Text -> Maybe ID                )
  , multi 100     10 defT  "IDREF"             gen_NCName_legal            gen_NCName_illegal            (fac :: Text -> Maybe IDREF             )
  , multi 100     10 noSuf "Language"          gen_language_legal          gen_language_illegal          (fac :: Text -> Maybe Language          )
  , multi 1000    10 defT  "NCName"            gen_NCName_legal            gen_NCName_illegal            (fac :: Text -> Maybe NCName            )
  , multi 1000    10 defT  "NMTOKEN"           gen_NMTOKEN_legal           gen_NMTOKEN_illegal           (fac :: Text -> Maybe NMTOKEN           )
  , multi 1000    10 noSuf "NOTATION"          gen_QName_legal             gen_QName_illegal             (fac :: Text -> Maybe NOTATION          )
  , multi 1000    10 defT  "Name"              gen_Name_legal              gen_Name_illegal              (fac :: Text -> Maybe Name              )
  , multi 1000    10 defT  "NormalizedString"  gen_stringxs_legal          gen_stringxs_illegal          (fac :: Text -> Maybe NormalizedString  )
  , multi 1000    10 noSuf "QName"             gen_QName_legal             gen_QName_illegal             (fac :: Text -> Maybe QName             )
  , multi 1000    10 defT  "Stringxs"          gen_stringxs_legal          gen_stringxs_illegal          (fac :: Text -> Maybe Stringxs          )
  , multi 1000    10 defT  "Token"             gen_token_legal             gen_stringxs_illegal          (fac :: Text -> Maybe Token             )
  ]

multi :: (Transformatio a, Eq a) => Int -> Int -> TestOpts a -> String -> QC.Gen Text -> QC.Gen Text -> (Text -> Maybe a) -> [] TestTree
multi tests          -- Int
      m              -- Int
      TestOpts {..}  -- TestOpts a
      title          -- String
      gen_legal_pp   -- QC.Gen Text
      gen_illegal_pp -- QC.Gen Text
      fac_pp         -- (Text -> Maybe a)
    =
    [ qctests tests       (title ++ " Idempotent canon WS"  ) (prop_legal   (gen_WS gen_legal_pp)   idempotentC )
    , qctests tests       (title ++ " Idempotent canon"     ) (prop_legal   gen_legal_pp            idempotentC )
    , qctests tests       (title ++ " Idempotent scribe WS" ) (prop_legal   (gen_WS gen_legal_pp)   idempotentS )
    , qctests tests       (title ++ " Idempotent scribe"    ) (prop_legal   gen_legal_pp            idempotentS )
    , qctests (tests * m) (title ++ " Illegal Text WS"      ) (prop_illegal (gen_WS gen_illegal_pp) fac_pp      )
    , qctests (tests * m) (title ++ " Illegal Text"         ) (prop_illegal gen_illegal_pp          fac_pp      )
    , qctests tests       (title ++ " Legal Text WS"        ) (prop_legal   (gen_WS gen_legal_pp)   fac_pp      )
    , qctests tests       (title ++ " Legal Text"           ) (prop_legal   gen_legal_pp            fac_pp      )
    ] ++ if suffixTestP
         then [ qctests (tests * m) (title ++ " Illegal Suffix"   ) (prop_illegal illegal_suffix          fac_pp )
              , qctests (tests * m) (title ++ " Illegal Suffix WS") (prop_illegal (gen_WS illegal_suffix) fac_pp )
              ]
         else []
  where
    idempotentC candidate = do
      faced      <- fac_pp candidate
      facedCanon <- fac_pp $ canon faced
      res        <- faced `eq` facedCanon ? Just faced $ Nothing
      pure res
    idempotentS = idempotentF scribe
    idempotentF toText candidate =
      case fac_pp candidate of
        j@(Just entity) -> j == fac_pp (toText entity) ? j $ Nothing
        Nothing -> Nothing
    illegal_suffix = do
      legal   <- gen_legal_pp
      illegal <- QC.suchThat gen_illegal_pp (T.any (\c -> all (/= c) xmlWhite)) -- At least one non-xmlWhite.                                        -- ⚡
      pure $ T.append legal illegal

data (Transformatio a, Eq a) => TestOpts a =
  TestOpts { eq :: a -> a -> Bool
           , suffixTestP :: Bool
           }

defT :: (Transformatio a, Eq a) => TestOpts a
defT = TestOpts { eq = (==)
                , suffixTestP = True
                }

noSuf :: (Transformatio a, Eq a) => TestOpts a
noSuf = defT { suffixTestP = False }

-- durT :: TestOpts Durationxs
-- durT = TestOpts { eq = (\a b -> durationNormalize a == durationNormalize b)
--                 , suffixTestP = True
--                 }

-- | The ·lexical representations· of duration are more or less based on the pattern:
--   PnYnMnDTnHnMnS

data DuraOrder = Y_do
               | M_do
               | D_do
               | T_do
               | H_do
               | N_do -- miNutes
               | S_do
                 deriving (Eq, Ord, Enum, Show, Ix.Ix)

doShow :: DuraOrder -> T.Text
doShow N_do = "M"
doShow _do = take 1 $ tshow _do

gen_DuraOrder :: QC.Gen DuraOrder
gen_DuraOrder = QC.elements $ enumFrom Y_do

gen_DuraOrder_legal :: QC.Gen [DuraOrder]
gen_DuraOrder_legal = do                                                                                                                             -- ➿
  a <- gen_DuraOrder
  b <- gen_DuraOrder
  let [start_candidate,end] = sort [a,b]
      start = min start_candidate T_do
  if start == T_do && end == T_do
  then gen_DuraOrder_legal                                                                                                                           -- ↺
  else pure $ range (start, end == T_do ? succ(T_do) $ end)                                                                                          -- 🔚

gen_duration_where :: ([] DuraOrder -> Bool) -> (DuraOrder -> QC.Gen Int) -> QC.Gen Text
gen_duration_where f genIntF = do
  l    <- QC.suchThat gen_DuraOrder_legal f
  ls   <- mapM mF l
  sign <- QC.elements ["-", ""]
  pure $ T.concat ([sign, "P"] ++ ls)
  where
    mF T_do = pure "T"
    mF S_do = do
      ns <- QC.choose ((0,999999999) :: (Word64, Word64))
      zs <- QC.elements [T.replicate x "0" | x <- [0..7]]
      n  <- genIntF S_do
      pure $ T.concat [tshow n, ".", T.take 9 (T.append zs (tshow ns)), doShow S_do]
    mF x_do = do
      n <- genIntF x_do
      pure $ T.concat [tshow n, doShow x_do]

gen_Durationxs_legal :: QC.Gen Text
gen_Durationxs_legal = QC.oneof [ simples, dura, dura, dura, dura ]
  where
    simples = QC.elements [ "P2Y6M5DT12H35M30S" -- simples should be made into unit tests instead of QC.                                             -- ⚠
                          , "P1DT2H"
                          , "P20M"
                          , "PT20M"
                          , "P0Y20M0D"
                          , "P0Y"
                          , "-P60D"
                          , "PT1M30.5S"
                          ]
    dura = gen_duration_where (const True) (const $ QC.choose (0,9999))

gen_Durationxs_illegal :: QC.Gen Text
gen_Durationxs_illegal = QC.oneof [ simples, reversed, reversed, reversed, reversed ]
  where
    simples = QC.elements [ "P-20M" -- simples should be made into unit tests instead of QC.                                                         -- ⚠
                          , "P20MT"
                          , "P1YM5D"
                          , "P15.5Y"
                          , "P1D2H"
                          , "1Y2M"
                          , "P2M1Y"
                          , "P"
                          , "PT15.S"
                          ]
    reversed = do
      l <- QC.suchThat gen_DuraOrder_legal ((> 1) . length) >>= pure . reverse
      ls <- mapM mF l
      sign <- QC.elements ["-", ""]
      pure $ T.concat ([sign, "P"] ++ ls)
      where
        mF T_do = pure "T"
        mF S_do = do
          ns <- QC.choose ((0,999999999) :: (Word64, Word64))
          zs <- QC.elements [T.replicate x "0" | x <- [0..7]]
          n <- QC.choose ((0,999) :: (Word32, Word32))
          pure $ T.concat [tshow n, ".", T.take 9 (T.append zs (tshow ns)), doShow S_do]
        mF x_do = do
          n <- QC.choose ((0,999) :: (Word32, Word32))
          pure $ T.concat [tshow n, doShow x_do]

gen_YearMonthDuration_legal :: QC.Gen Text
gen_YearMonthDuration_legal = QC.oneof [ simples, dura, dura, dura, dura ]
  where
    simples = QC.elements [ "P2Y6M" -- simples should be made into unit tests instead of QC.                                                         -- ⚠
                          , "P20M"
                          , "P0Y20M0D"
                          , "P0Y"
                          ]
    dura = gen_duration_where (all (<= M_do)) (const $ QC.choose (0,9999))

gen_YearMonthDuration_illegal :: QC.Gen Text
gen_YearMonthDuration_illegal = QC.oneof [ simples, dura, dura, dura, dura ]
  where
    simples = QC.elements [ "P-20M" -- simples should be made into unit tests instead of QC.                                                         -- ⚠
                          , "P20MT"
                          , "P1YM5D"
                          , "P15.5Y"
                          , "P1D2H"
                          , "1Y2M"
                          , "P2M1Y"
                          , "P"
                          , "PT15.S"
                          , "P2Y6M5DT12H35M30S"
                          , "P1DT2H"
                          , "PT20M"
                          , "-P60D"
                          , "PT1M30.5S"
                          ]
    dura = gen_duration_where (\(start:_) -> start > M_do) genF
    genF T_do = error "Calling for a number here is rubbish."                                                                                        -- ⛞
    genF x_do | x_do <= M_do = QC.choose (0,9999)
              | otherwise    = QC.choose (1,9999)


gen_DayTimeDuration_legal :: QC.Gen Text
gen_DayTimeDuration_legal = QC.oneof [ simples, dura, dura, dura, dura ]
  where
    simples = QC.elements [ "P0Y" -- simples should be made into unit tests instead of QC.                                                           -- ⚠
                          , "P1DT2H"
                          , "PT20M"
                          , "-P60D"
                          , "PT1M30.5S"
                          , "PT20M"
                          , "P1DT2H"
                          ]
    dura = gen_duration_where (\(start:_) -> start >= D_do) (const $ QC.choose (0,9999))

gen_DayTimeDuration_illegal :: QC.Gen Text
gen_DayTimeDuration_illegal = QC.oneof [ simples, dura, dura, dura, dura ]
  where
    simples = QC.elements [ "P2Y6M" -- simples should be made into unit tests instead of QC.                                                         -- ⚠
                          , "P20M"
                          , "P0Y20M0D"
                          , "P-20M"
                          , "P20MT"
                          , "P1YM5D"
                          , "P15.5Y"
                          , "1Y2M"
                          , "P2M1Y"
                          , "P"
                          , "PT15.S"
                          , "P2Y6M5DT12H35M30S"
                          ]
    dura = gen_duration_where (\(start:_) -> start < D_do) genF
    genF T_do = error "Calling for a number here is rubbish."                                                                                        -- ⛞
    genF x_do | x_do <= M_do = QC.choose (1,9999)
              | otherwise    = QC.choose (0,9999)

gen_Floatxs_legal :: QC.Gen Text
gen_Floatxs_legal = QC.oneof [ gen_Decimal_legal, simples, decimalPlus]
  where
    simples = QC.elements
              [ "0.0", "0.", "0", ".0", "+0.0", "+0.", "+0", "+.0", "-0.0", "-0.", "-0", "-.0"
              , "0.0e0", "0.e0", "0e0", ".0e0", "+0.0e0", "+0.e0", "+0e0", "+.0e0", "-0.0e0", "-0.e0", "-0e0", "-.0e0"
              , "0.0e-0", "0.e-0", "0e-0", ".0e-0", "+0.0e-0", "+0.e-0", "+0e-0", "+.0e-0", "-0.0e-0", "-0.e-0", "-0e-0", "-.0e-0"
              , "00.00", "00.", "00", ".00", "+00.00", "+00.", "+00", "+.00", "-00.00", "-00.", "-00", "-.00"
              , "00.00e00", "00.e00", "00e00", ".00e00", "+00.00e00", "+00.e00", "+00e00", "+.00e00", "-00.00e00", "-00.e00"
              , "-00e00", "-.00e00"
              , "00.00e-00", "00.e-00", "00e-00", ".00e-00", "+00.00e-00", "+00.e-00", "+00e-00", "+.00e-00", "-00.00e-00"
              , "-00.e-00", "-00e-00", "-.00e-00"
              , "INF", "+INF", "-INF"
              ]
    decimalPlus = do
      legal    <- gen_Decimal_legal
      expo     <- QC.elements ["e", "E"]
      expoSign <- QC.elements ["+", "-", ""]
      expoN    <- gen_num >>= pure . T.singleton
      pure $ T.concat [legal, expo, expoSign, expoN]

gen_Floatxs_illegal :: QC.Gen Text
gen_Floatxs_illegal = QC.oneof [ gen_Decimal_illegal, simples ]
  where
    simples = QC.elements
              [ "0..0", "0..", "..0", "+0..0", "+0..", "+..0", "-0..0", "-0..", "-..0"
              , "0..0e0", "0..e0", "..0e0", "+0..0e0", "+0..e0", "+..0e0", "-0..0e0", "-0..e0", "-..0e0"
              , "0..0e-0", "0..e-0", "..0e-0", "+0..0e-0", "+0..e-0", "+..0e-0", "-0..0e-0", "-0..e-0", "-..0e-0"
              , "00..00", "00..", "..00", "+00..00", "+00..", "+..00", "-00..00", "-00..", "-..00"
              , "00..00e00", "00..e00", "..00e00", "+00..00e00", "+00..e00", "+..00e00", "-00..00e00", "-00..e00"
              , "-..00e00"
              , "00..00e-00", "00..e-00", "..00e-00", "+00..00e-00", "+00..e-00", "+..00e-00", "-00..00e-00"
              , "-00..e-00", "-..00e-00"
              , "0.0ee0", "0.ee0", "0ee0", ".0ee0", "+0.0ee0", "+0.ee0", "+0ee0", "+.0ee0", "-0.0ee0", "-0.ee0", "-0ee0", "-.0ee0"
              , "0.0ee-0", "0.ee-0", "0ee-0", ".0ee-0", "+0.0ee-0", "+0.ee-0", "+0ee-0", "+.0ee-0", "-0.0ee-0", "-0.ee-0", "-0ee-0"
              , "-.0ee-0"
              , "00.00ee00", "00.ee00", "00ee00", ".00ee00", "+00.00ee00", "+00.ee00", "+00ee00", "+.00ee00", "-00.00ee00", "-00.ee00"
              , "-00ee00", "-.00ee00"
              , "00.00ee-00", "00.ee-00", "00ee-00", ".00ee-00", "+00.00ee-00", "+00.ee-00", "+00ee-00", "+.00ee-00", "-00.00ee-00"
              , "-00.ee-00", "-00ee-00", "-.00ee-00"
              , "INF0", "++INF", "--INF", "+-INF", "-+INF", "0INF", "INF+", "INF-", "+INF-", "-INF+"
              , "0.0e", "0.e", "0e", ".0e", "+0.0e", "+0.e", "+0e", "+.0e", "-0.0e", "-0.e", "-0e", "-.0e"
              , "-", "+", "--", "++", "-+", "+-"
              , "e", "E", "e+", "E+", "e-", "E-", "+e", "-E"
              ]

gen_Decimal_legal :: QC.Gen Text
gen_Decimal_legal = do
  decimal <- QC.oneof [ digits, digitsDecimal, digitsFractional ]
  zeros <- QC.listOf $ QC.elements "0"
  sign <- QC.oneof [ QC.elements "+-" >>= pure . (:[]), pure "" ]
  pure . T.pack $ sign ++ zeros ++ decimal -- Add 0 or more leading zeros
  where
    digits = QC.listOf1 gen_num
    digitsDecimal = digits >>= pure . (++ ".")
    digitsFractional = do
      init <- QC.listOf gen_num >>= pure . (++ ".") -- Zero leading digits is legal as long as there is a fractional value.
      digits >>= pure . (init ++)

gen_Decimal_illegal :: QC.Gen Text
gen_Decimal_illegal = QC.oneof [ nonNum, prefix, doublePlusMinus, simples ]
  where
    nonNum = do
      illegal <- QC.listOf1 $ QC.suchThat gen_non_xmlWhite (not . inRange ('0','9'))
      pure $ T.pack illegal

    prefix = do
      legal <- gen_Decimal_legal
      illegal <- QC.suchThat nonNum (T.all (not . flip DL.elem ("+-." :: [] Char)))
      pure $ T.append illegal legal

    doublePlusMinus = do
      legal <- gen_Decimal_legal
      let firstLegal = T.head legal
      doublePrefix <- if isDigit firstLegal || firstLegal == '.'
                      then QC.suchThat (QC.resize 8 . QC.listOf1 $ QC.elements "+-") (\s -> length s > 1)
                      else QC.listOf1 $ QC.elements "+-"
      pure $ T.append (T.pack doublePrefix) legal

    simples = QC.elements ["++1.0", "--1.0", "+-1.0", "0+1.0", "0-1.0", ".", "-", "+"]

gen_QName_legal :: QC.Gen Text
gen_QName_legal =  QC.oneof [ singleGen, doubleGen ]
  where
    singleGen = gen_NCName_legal
    doubleGen = do init <- singleGen
                   end  <- singleGen
                   pure $ T.intercalate ":" [init, end]

gen_QName_illegal :: QC.Gen Text
gen_QName_illegal = QC.oneof [ singleGen, doubleGen, colonGen , colonGen' ]
  where
    singleGen = gen_NCName_illegal
    doubleGen = do init <- gen_NCName_legal
                   end  <- gen_NCName_illegal
                   pure $ T.intercalate ":" [init, end]

    colonGen = do end <- gen_QName_legal -- NB colonGen can induce an improper result if auto suffix test is run.
                  pure $ T.append ":" end

    colonGen' = do init <- gen_QName_legal
                   pure $ T.append init ":"

gen_language_legal :: QC.Gen Text
gen_language_legal = do
  init <- eight_alpha >>= pure . T.pack
  m <- more >>= pure . T.pack
  pure $ T.append init m
  where
    eight :: QC.Gen String
    eight = QC.resize 8 (QC.listOf1 gen_alpha_num)

    eight_alpha :: QC.Gen String
    eight_alpha = QC.resize 8 (QC.listOf1 gen_alpha)

    more :: QC.Gen String
    more = QC.listOf (eight >>= pure . ('-' :)) >>= pure . join

gen_language_illegal :: QC.Gen Text
gen_language_illegal = QC.oneof [ doA, doB, doC ]
  where
    doA = QC.listOf gen_non_alpha_num >>= pure . T.pack -- Illegal chars plus empty.

    doB = do legalinit <- eight_alpha >>= pure . T.pack -- NB doB can fail if auto suffix test is run. Maybe use classify and cover.
             dash <- QC.listOf  (QC.elements "-") >>= pure . T.pack
             more <- QC.listOf1 gen_non_alpha_num >>= pure . T.pack
             pure $ T.concat [legalinit, dash, more]

    doC = do init <- nine_alpha >>= pure . T.pack
             m    <- more       >>= pure . T.pack
             pure $ T.append init m

    eight_alpha :: QC.Gen String
    eight_alpha = QC.resize 8 $ QC.listOf1 gen_alpha

    nine :: QC.Gen String
    nine = QC.suchThat (QC.resize 30 (QC.listOf1 gen_alpha_num)) ((8 <) . length)

    nine_alpha :: QC.Gen String
    nine_alpha = QC.suchThat (QC.resize 30 (QC.listOf1 gen_alpha)) ((8 <) . length)

    more :: QC.Gen String
    more = QC.listOf (nine >>= pure . ('-' :)) >>= pure . join

gen_token_legal :: QC.Gen Text
gen_token_legal = do
  nw     <- QC.suchThat gen_non_xmlWhite $ \e -> not (e == chr 0xFFFE || e == chr 0xFFFF)
  legal  <- QC.oneof [QC.suchThat gen_text (T.null . filter (\e -> e == chr 0xFFFE || e == chr 0xFFFF)), pure ""]
  legal' <- QC.oneof [QC.suchThat gen_text (T.null . filter (\e -> e == chr 0xFFFE || e == chr 0xFFFF)), pure ""]
  pure $ T.append legal' $ T.cons nw legal

gen_stringxs_legal :: QC.Gen Text
gen_stringxs_legal = QC.suchThat gen_text (T.null . filter (\e -> e == chr 0xFFFE || e == chr 0xFFFF))

gen_stringxs_illegal :: QC.Gen Text
gen_stringxs_illegal = do
  legal   <- gen_stringxs_legal
  illegal <- QC.listOf1 (QC.elements [chr 0xFFFE, chr 0xFFFF]) >>= pure . T.pack
  pure $ T.append legal illegal

gen_boolean_illegal :: QC.Gen Text
gen_boolean_illegal =
  QC.suchThat gen_text (\t -> case t of
                                "true"  -> False
                                "false" -> False
                                "1"     -> False
                                "0"     -> False
                                _       -> True
                       )

gen_boolean_legal :: QC.Gen Text
gen_boolean_legal = QC.elements [ "true", "false", "1", "0" ]

gen_text :: QC.Gen Text
gen_text = QC.listOf1 Q.chooseAny >>= pure . T.pack

gen_boolean_legal_val :: QC.Gen (Text, Boolean)
gen_boolean_legal_val =
  QC.elements [ ( "true"  , fromJust $ recipe True  )
              , ( "false" , fromJust $ recipe False )
              , ( "1"     , fromJust $ recipe True  )
              , ( "0"     , fromJust $ recipe False )
              ]

gen_boolean_legal_WS :: QC.Gen (Text, Boolean)
gen_boolean_legal_WS = gen_val_WS gen_boolean_legal_val

gen_val_WS :: Transformatio a => QC.Gen (Text, a) -> QC.Gen (Text, a)
gen_val_WS gen_pp = do
  (legaltx,b)   <- gen_pp
  legaltxWithWS <- gen_WS (pure legaltx)
  pure (legaltxWithWS,b)

gen_bytestring :: QC.Gen B.ByteString
gen_bytestring = QC.listOf Q.chooseAny >>= pure . B.pack -- Empty string is legal by specification.

-- | The ·value space· of Base64Binary is the set of finite-length
--   sequences of zero or more binary octets.
--   The length of a value is the number of octets.

gen_64_legal :: QC.Gen Text
gen_64_legal = gen_bytestring >>= pure . TE.decodeUtf8 . B64.encode

{-

     Lexical space of base64Binary

[27] Base64Binary ::= (B64quad* B64final)?
[28] B64quad      ::= (B64 B64 B64 B64)                    /* B64quad represents three octets of binary data. */
[29] B64final     ::= B64finalquad | Padded16 | Padded8
[30] B64finalquad ::= (B64 B64 B64 B64char)                /* B64finalquad represents three octets of binary data without trailing space. */
[31] Padded16     ::= B64 B64 B16 '='                      /* Padded16 represents a two-octet at the end of the data. */
[32] Padded8      ::= B64 B04 '=' #x20? '='                /* Padded8 represents a single octet at the end of the data. */
[33] B64          ::= B64char #x20?
[34] B64char      ::= [A-Za-z0-9+/]
[35] B16          ::= B16char #x20?
[36] B16char      ::= [AEIMQUYcgkosw048]                   /* Base64 characters whose bit-string value ends in '00' */
[37] B04          ::= B04char #x20?
[38] B04char      ::= [AQgw]                               /* Base64 characters whose bit-string value ends in '0000' */

The Base64Binary production is equivalent to the following regular expression.

    ((([A-Za-z0-9+/] ?){4})*(([A-Za-z0-9+/] ?){3}[A-Za-z0-9+/]|([A-Za-z0-9+/] ?){2}[AEIMQUYcgkosw048] ?=|[A-Za-z0-9+/] ?[AQgw] ?= ?=))?

Note that each '?' except the last is preceded by a single space character.

Note that this grammar requires the number of non-whitespace
characters in the ·lexical representation· to be a multiple of four,
and for equals signs to appear only at the end of the ·lexical
representation·; literals which do not meet these constraints are not
legal ·lexical representations· of base64Binary.

-}

gen_64_illegal :: QC.Gen Text
gen_64_illegal = QC.oneof [ gen_64_legal >>= pure . flip T.append "="                        -- This needs to be smarter.
                          , gen_64_legal >>= \text -> pure (T.null text ? "➙" $ T.init text) -- This needs to be smarter.
                          ]

-- | The ·value space· of hexBinary is the set of finite-length
--   sequences of zero or more binary octets.
--   The length of a value is the number of octets.

gen_hex_legal :: QC.Gen Text
gen_hex_legal = gen_bytestring >>= pure . TE.decodeUtf8 . Hex.encode

gen_hex_illegal :: QC.Gen Text
gen_hex_illegal = QC.oneof [ gen_hex_legal >>= pure . flip T.append "ag"
                           , gen_hex_legal >>= \text -> pure (T.null text ? "➙" $ T.init text)
                           ]

gen_NCName_legal :: QC.Gen Text
gen_NCName_legal = do startChar <- QC.suchThat nameStartChar (':' /=)
                      chars     <- QC.suchThat nameChars (all (':' /=))
                      pure $ T.pack (startChar:chars)

gen_NCName_illegal :: QC.Gen Text
gen_NCName_illegal = do startChar <- nameNotStartChar
                        chars     <- nameNotChars
                        pure $ T.pack (startChar:chars)

gen_NMTOKEN_legal :: QC.Gen Text
gen_NMTOKEN_legal = nameChars >>= pure . T.pack

gen_NMTOKEN_illegal :: QC.Gen Text
gen_NMTOKEN_illegal = nameNotChars >>= pure . T.pack

gen_Name_legal :: QC.Gen Text
gen_Name_legal = do startChar <- nameStartChar
                    chars     <- nameChars
                    pure $ T.pack (startChar:chars)

gen_Name_illegal :: QC.Gen Text
gen_Name_illegal = do startChar <- nameNotStartChar
                      chars     <- nameNotChars
                      pure $ T.pack (startChar:chars)

gen_alpha_num :: QC.Gen Char
gen_alpha_num = QC.oneof [ QC.choose ('A','Z'), QC.choose ('a','z'), QC.choose ('0','9') ]

gen_alpha :: QC.Gen Char
gen_alpha = QC.oneof [ QC.choose ('A','Z'), QC.choose ('a','z') ]

gen_num :: QC.Gen Char
gen_num = QC.choose ('0','9')

gen_non_alpha_num :: QC.Gen Char
gen_non_alpha_num = QC.suchThat Q.chooseAny (\c -> not $ any (flip inRange c) ranges)
  where
    ranges = [ (,) 'A' 'Z'
             , (,) 'a' 'z'
             , (,) '0' '9'
             ]

gen_WS :: QC.Gen Text -> QC.Gen Text
gen_WS gen_pp = do
  tx <- gen_pp
  prefixWS <- QC.listOf (QC.elements xmlWhite) >>= pure . T.pack
  suffixWS <- QC.listOf (QC.elements xmlWhite) >>= pure . T.pack
  pure $ T.concat [prefixWS, tx, suffixWS]

gen_non_xmlWhite :: QC.Gen Char
gen_non_xmlWhite = QC.suchThat Q.chooseAny (not . flip DL.elem xmlWhite)

nameStartChar :: QC.Gen Char
nameStartChar = QC.oneof ranges
  where
    ranges = [ QC.elements ":_"
             , QC.choose ('A','Z')
             , QC.choose ('a','z')
             , QC.choose ( chr 0xC0    , chr 0xD6    )
             , QC.choose ( chr 0xD8    , chr 0xF6    )
             , QC.choose ( chr 0xF8    , chr 0x2FF   )
             , QC.choose ( chr 0x370   , chr 0x37D   )
             , QC.choose ( chr 0x37F   , chr 0x1FFF  )
             , QC.choose ( chr 0x200C  , chr 0x200D  )
             , QC.choose ( chr 0x2070  , chr 0x218F  )
             , QC.choose ( chr 0x2C00  , chr 0x2FEF  )
             , QC.choose ( chr 0x3001  , chr 0xD7FF  )
             , QC.choose ( chr 0xF900  , chr 0xFDCF  )
             , QC.choose ( chr 0xFDF0  , chr 0xFFFD  )
             , QC.choose ( chr 0x10000 , chr 0xEFFFF )
             ]

nameChars :: QC.Gen [Char]
nameChars = QC.listOf1 $ QC.oneof ranges
  where
    ranges = [ nameStartChar
             , QC.elements "-.·"
             , QC.choose ('0','9')
             , QC.choose ( chr 0x300  , chr 0x36F  ) -- Combining Diacritical Marks
             , QC.choose ( chr 0x203F , chr 0x2040 ) -- Part of the General Punctuation block
             ]

nameNotStartChar :: QC.Gen Char
nameNotStartChar = QC.oneof ranges
  where
    ranges = [ QC.choose ( chr 0x0     , chr 0x8      )
             , QC.choose ( chr 0xE     , chr 0x1F     )
             , QC.choose ( chr 0x21    , chr 0x2C     )
             , QC.choose ( chr 0x2F    , chr 0x39     )
             , QC.choose ( chr 0x3B    , chr 0x40     )
             , QC.choose ( chr 0x5B    , chr 0x5E     )
             , QC.choose ( chr 0x60    , chr 0x60     )
             , QC.choose ( chr 0x7B    , chr 0xBF     )
             , QC.choose ( chr 0xD7    , chr 0xD7     )
             , QC.choose ( chr 0xF7    , chr 0xF7     )
             , QC.choose ( chr 0x300   , chr 0x36F    ) -- Combining Diacritical Marks
             , QC.choose ( chr 0x37E   , chr 0x37E    )
             , QC.choose ( chr 0x2000  , chr 0x200B   )
             , QC.choose ( chr 0x200E  , chr 0x206F   )
             , QC.choose ( chr 0x2190  , chr 0x2BFF   )
             , QC.choose ( chr 0x2FFF  , chr 0x3000   )
             , QC.choose ( chr 0xE000  , chr 0xF8FF   )
             , QC.choose ( chr 0xFDD0  , chr 0xFDEF   )
             , QC.choose ( chr 0xFFFE  , chr 0xFFFF   )
             , QC.choose ( chr 0xF0000 , chr 0x10FFFF )
             ]

nameNotChars :: QC.Gen [Char]
nameNotChars = QC.listOf1 nameNotChar
  where
    nameNotChar = QC.oneof [ QC.choose ( chr 0x0     , chr 0x8      )
                           , QC.choose ( chr 0xE     , chr 0x1F     )
                           , QC.choose ( chr 0x21    , chr 0x2C     )
                           , QC.choose ( chr 0x2F    , chr 0x2F     )
                           , QC.choose ( chr 0x3B    , chr 0x40     )
                           , QC.choose ( chr 0x5B    , chr 0x5E     )
                           , QC.choose ( chr 0x60    , chr 0x60     )
                           , QC.choose ( chr 0x7B    , chr 0xB6     )
                           , QC.choose ( chr 0xB8    , chr 0xBF     )
                           , QC.choose ( chr 0xD7    , chr 0xD7     )
                           , QC.choose ( chr 0xF7    , chr 0xF7     )
                           , QC.choose ( chr 0x37E   , chr 0x37E    )
                           , QC.choose ( chr 0x2000  , chr 0x200B   )
                           , QC.choose ( chr 0x200E  , chr 0x2039   )
                           , QC.choose ( chr 0x2190  , chr 0x2BFF   )
                           , QC.choose ( chr 0x2FFF  , chr 0x3000   )
                           , QC.choose ( chr 0xE000  , chr 0xF8FF   )
                           , QC.choose ( chr 0xFDD0  , chr 0xFDEF   )
                           , QC.choose ( chr 0xFFFE  , chr 0xFFFF   )
                           , QC.choose ( chr 0xF0000 , chr 0x10FFFF )
                           ]

-- -----------------------------------------------------------------------------------------------------------------------------------------------------
-- This section contains the FiniteBits types tests. Boilerplate world. Could use some love.

finiteBitsTests :: [TestTree]
finiteBitsTests =
  [ testCase "UnsignedByte Legal Text"                  case_UnsignedByte_LegalText
  , testCase "UnsignedByte Legal Text With WS"          case_UnsignedByte_LegalTextWithWS
  , testCase "UnsignedByte Legal Text With Plus"        case_UnsignedByte_LegalTextWithPlus
  , testCase "UnsignedByte Legal Text With Minus"       case_UnsignedByte_LegalTextWithMinus
  , testCase "UnsignedByte Legal Text Idempotent"       case_UnsignedByte_Idempotent
  , testCase "UnsignedByte Illegal Text Range"          case_UnsignedByte_IllegalTextRange
  , testCase "UnsignedByte Illegal Text Range With WS"  case_UnsignedByte_IllegalTextRangeWithWS
  , testCase "UnsignedByte Illegal Text With Other"     case_UnsignedByte_IllegalTextWithOther

  , testCase "UnsignedShort Legal Text"                 case_UnsignedShort_LegalText
  , testCase "UnsignedShort Legal Text With WS"         case_UnsignedShort_LegalTextWithWS
  , testCase "UnsignedShort Legal Text With Plus"       case_UnsignedShort_LegalTextWithPlus
  , testCase "UnsignedShort Legal Text With Minus"      case_UnsignedShort_LegalTextWithMinus
  , testCase "UnsignedShort Legal Text Idempotent"      case_UnsignedShort_Idempotent
  , testCase "UnsignedShort Illegal Text Range"         case_UnsignedShort_IllegalTextRange
  , testCase "UnsignedShort Illegal Text Range With WS" case_UnsignedShort_IllegalTextRangeWithWS
  , testCase "UnsignedShort Illegal Text With Other"    case_UnsignedShort_IllegalTextWithOther

  , testCase "UnsignedInt Legal Text"                   case_UnsignedInt_LegalText
  , testCase "UnsignedInt Legal Text With WS"           case_UnsignedInt_LegalTextWithWS
  , testCase "UnsignedInt Legal Text With Plus"         case_UnsignedInt_LegalTextWithPlus
  , testCase "UnsignedInt Legal Text With Minus"        case_UnsignedInt_LegalTextWithMinus
  , testCase "UnsignedInt Legal Text Idempotent"        case_UnsignedInt_Idempotent
  , testCase "UnsignedInt Illegal Text Range"           case_UnsignedInt_IllegalTextRange
  , testCase "UnsignedInt Illegal Text Range With WS"   case_UnsignedInt_IllegalTextRangeWithWS
  , testCase "UnsignedInt Illegal Text With Other"      case_UnsignedInt_IllegalTextWithOther

  , testCase "UnsignedLong Legal Text"                  case_UnsignedLong_LegalText
  , testCase "UnsignedLong Legal Text With WS"          case_UnsignedLong_LegalTextWithWS
  , testCase "UnsignedLong Legal Text With Plus"        case_UnsignedLong_LegalTextWithPlus
  , testCase "UnsignedLong Legal Text With Minus"       case_UnsignedLong_LegalTextWithMinus
  , testCase "UnsignedLong Legal Text Idempotent"       case_UnsignedLong_Idempotent
  , testCase "UnsignedLong Illegal Text Range"          case_UnsignedLong_IllegalTextRange
  , testCase "UnsignedLong Illegal Text Range With WS"  case_UnsignedLong_IllegalTextRangeWithWS
  , testCase "UnsignedLong Illegal Text With Other"     case_UnsignedLong_IllegalTextWithOther

  , testCase "Byte Legal Text"                          case_Byte_LegalText
  , testCase "Byte Legal Text With WS"                  case_Byte_LegalTextWithWS
  , testCase "Byte Legal Text With Plus"                case_Byte_LegalTextWithPlus
  , testCase "Byte Legal Text With Minus"               case_Byte_LegalTextWithMinus
  , testCase "Byte Legal Text Idempotent"               case_Byte_Idempotent
  , testCase "Byte Illegal Text Range"                  case_Byte_IllegalTextRange
  , testCase "Byte Illegal Text Range With WS"          case_Byte_IllegalTextRangeWithWS
  , testCase "Byte Illegal Text With Other"             case_Byte_IllegalTextWithOther

  , testCase "Short Legal Text"                         case_Short_LegalText
  , testCase "Short Legal Text With WS"                 case_Short_LegalTextWithWS
  , testCase "Short Legal Text With Plus"               case_Short_LegalTextWithPlus
  , testCase "Short Legal Text With Minus"              case_Short_LegalTextWithMinus
  , testCase "Short Legal Text Idempotent"              case_Short_Idempotent
  , testCase "Short Illegal Text Range"                 case_Short_IllegalTextRange
  , testCase "Short Illegal Text Range With WS"         case_Short_IllegalTextRangeWithWS
  , testCase "Short Illegal Text With Other"            case_Short_IllegalTextWithOther

  , testCase "Intxs Legal Text"                         case_Intxs_LegalText
  , testCase "Intxs Legal Text With WS"                 case_Intxs_LegalTextWithWS
  , testCase "Intxs Legal Text With Plus"               case_Intxs_LegalTextWithPlus
  , testCase "Intxs Legal Text With Minus"              case_Intxs_LegalTextWithMinus
  , testCase "Intxs Legal Text Idempotent"              case_Intxs_Idempotent
  , testCase "Intxs Illegal Text Range"                 case_Intxs_IllegalTextRange
  , testCase "Intxs Illegal Text Range With WS"         case_Intxs_IllegalTextRangeWithWS
  , testCase "Intxs Illegal Text With Other"            case_Intxs_IllegalTextWithOther

  , testCase "Long Legal Text"                          case_Long_LegalText
  , testCase "Long Legal Text With WS"                  case_Long_LegalTextWithWS
  , testCase "Long Legal Text With Plus"                case_Long_LegalTextWithPlus
  , testCase "Long Legal Text With Minus"               case_Long_LegalTextWithMinus
  , testCase "Long Legal Text Idempotent"               case_Long_Idempotent
  , testCase "Long Illegal Text Range"                  case_Long_IllegalTextRange
  , testCase "Long Illegal Text Range With WS"          case_Long_IllegalTextRangeWithWS
  , testCase "Long Illegal Text With Other"             case_Long_IllegalTextWithOther
  ]

byteNonNegativeIntegers :: [] Integer
byteNonNegativeIntegers = range (fromIntegral (minBound :: Word8), fromIntegral (maxBound :: Word8))

byteNonNegativeIntegerOut :: Integer
byteNonNegativeIntegerOut = fromIntegral (maxBound :: Word8) + 1

case_UnsignedByte_LegalText :: Assertion
case_UnsignedByte_LegalText = assertBool "Not All" (all id $ map f testRange)
  where
    testRange = byteNonNegativeIntegers
    f :: Integer -> Bool
    f n = let text = tshow n
              mUnsignedByte :: Maybe UnsignedByte
              mUnsignedByte = fac text
          in case mUnsignedByte of
               Nothing -> False
               Just (UnsignedByte word8) -> fromIntegral word8 == n

case_UnsignedByte_LegalTextWithWS :: Assertion
case_UnsignedByte_LegalTextWithWS = assertBool "Not All" (all id $ map f testRange)
  where
    testRange = byteNonNegativeIntegers
    f :: Integer -> Bool
    f n = let text = tshow n
              textWS = T.concat ["\n\r\t", text, "\t  \r\n  "]
              mUnsignedByte :: Maybe UnsignedByte
              mUnsignedByte = fac textWS
          in case mUnsignedByte of
               Nothing -> False
               Just (UnsignedByte word8) -> fromIntegral word8 == n

case_UnsignedByte_LegalTextWithPlus :: Assertion
case_UnsignedByte_LegalTextWithPlus = assertBool "Not All" (all id $ map f testRange)
  where
    testRange = byteNonNegativeIntegers
    f :: Integer -> Bool
    f n = let text = tshow n
              textWS = T.append "+" text
              mUnsignedByte :: Maybe UnsignedByte
              mUnsignedByte = fac textWS
          in case mUnsignedByte of
               Nothing -> False
               Just (UnsignedByte word8) -> fromIntegral word8 == n

case_UnsignedByte_LegalTextWithMinus :: Assertion
case_UnsignedByte_LegalTextWithMinus = assertBool "Borked" minusTest
  where
    text = "-0"
    mUnsignedByte :: Maybe UnsignedByte
    mUnsignedByte = fac text
    minusTest = case mUnsignedByte of
                  Nothing -> False
                  Just (UnsignedByte word8) -> word8 == 0

case_UnsignedByte_Idempotent :: Assertion
case_UnsignedByte_Idempotent = assertBool "Not All" (all id $ map f testRange)
  where
    testRange = byteNonNegativeIntegers
    f :: Integer -> Bool
    f n = let text = tshow n
              mUnsignedByte = fac' text
              fac' :: Text -> Maybe UnsignedByte
              fac' = fac
          in case mUnsignedByte of
               Nothing -> False
               Just ub@(UnsignedByte word8) -> if fromIntegral word8 == n
                                               then case fac' (canon ub) >>= fac' . canon of
                                                      Nothing -> False
                                                      Just (UnsignedByte word8') -> fromIntegral word8' == n
                                               else False

case_UnsignedByte_IllegalTextRange :: Assertion
case_UnsignedByte_IllegalTextRange = assertBool "Some" (not . any id $ map f testRange)
  where
    testRange = (map (\n -> (n + 1) * byteNonNegativeIntegerOut) $ byteNonNegativeIntegers)
                ++
                (map (\n -> (n + 1) * (-1)) $ byteNonNegativeIntegers)
    f :: Integer -> Bool
    f n = let text = tshow n
              mUnsignedByte :: Maybe UnsignedByte
              mUnsignedByte = fac text
          in case mUnsignedByte of
               Nothing -> False
               Just (UnsignedByte word8) -> fromIntegral word8 == n

case_UnsignedByte_IllegalTextRangeWithWS :: Assertion
case_UnsignedByte_IllegalTextRangeWithWS = assertBool "Some" (not . any id $ map f testRange)
  where
    testRange = (map (\n -> (n + 1) * byteNonNegativeIntegerOut) $ byteNonNegativeIntegers)
                ++
                (map (\n -> (n + 1) * (-1)) $ byteNonNegativeIntegers)
    f :: Integer -> Bool
    f n = let text = tshow n
              textWS = T.concat ["\n\r\t", text, "\t  \r\n  "]
              mUnsignedByte :: Maybe UnsignedByte
              mUnsignedByte = fac textWS
          in case mUnsignedByte of
               Nothing -> False
               Just (UnsignedByte word8) -> fromIntegral word8 == n

case_UnsignedByte_IllegalTextWithOther :: Assertion
case_UnsignedByte_IllegalTextWithOther = assertBool "Some" (not . any id $ map f testRange)
  where
    testRange = byteNonNegativeIntegers
    f :: Integer -> Bool
    f n = let text = tshow n
              textWS = T.concat ["\n\r\t", text, "✓\t  \r\n  "]
              mUnsignedByte :: Maybe UnsignedByte
              mUnsignedByte = fac textWS
          in case mUnsignedByte of
               Nothing -> False
               Just (UnsignedByte word8) -> fromIntegral word8 == n

shortNonNegativeIntegers :: [] Integer
shortNonNegativeIntegers = range (fromIntegral (minBound :: Word16), fromIntegral (maxBound :: Word16))

shortNonNegativeIntegerOut :: Integer
shortNonNegativeIntegerOut = fromIntegral (maxBound :: Word16) + 1

case_UnsignedShort_LegalText :: Assertion
case_UnsignedShort_LegalText = assertBool "Not All" (all id $ map f testRange)
  where
    testRange = shortNonNegativeIntegers
    f :: Integer -> Bool
    f n = let text = tshow n
              mUnsignedShort :: Maybe UnsignedShort
              mUnsignedShort = fac text
          in case mUnsignedShort of
               Nothing -> False
               Just (UnsignedShort word16) -> fromIntegral word16 == n

case_UnsignedShort_LegalTextWithWS :: Assertion
case_UnsignedShort_LegalTextWithWS = assertBool "Not All" (all id $ map f testRange)
  where
    testRange = shortNonNegativeIntegers
    f :: Integer -> Bool
    f n = let text = tshow n
              textWS = T.concat ["\n\r\t", text, "\t  \r\n  "]
              mUnsignedShort :: Maybe UnsignedShort
              mUnsignedShort = fac textWS
          in case mUnsignedShort of
               Nothing -> False
               Just (UnsignedShort word16) -> fromIntegral word16 == n

case_UnsignedShort_LegalTextWithPlus :: Assertion
case_UnsignedShort_LegalTextWithPlus = assertBool "Not All" (all id $ map f testRange)
  where
    testRange = shortNonNegativeIntegers
    f :: Integer -> Bool
    f n = let text = tshow n
              textWS = T.append "+" text
              mUnsignedShort :: Maybe UnsignedShort
              mUnsignedShort = fac textWS
          in case mUnsignedShort of
               Nothing -> False
               Just (UnsignedShort word16) -> fromIntegral word16 == n

case_UnsignedShort_LegalTextWithMinus :: Assertion
case_UnsignedShort_LegalTextWithMinus = assertBool "Borked" minusTest
  where
    text = "-0"
    mUnsignedShort :: Maybe UnsignedShort
    mUnsignedShort = fac text
    minusTest = case mUnsignedShort of
                  Nothing -> False
                  Just (UnsignedShort word16) -> word16 == 0

case_UnsignedShort_Idempotent :: Assertion
case_UnsignedShort_Idempotent = assertBool "Not All" (all id $ map f testRange)
  where
    testRange = shortNonNegativeIntegers
    f :: Integer -> Bool
    f n = let text = tshow n
              mUnsignedShort = fac' text
              fac' :: Text -> Maybe UnsignedShort
              fac' = fac
          in case mUnsignedShort of
               Nothing -> False
               Just ub@(UnsignedShort word16) -> if fromIntegral word16 == n
                                                 then case fac' (canon ub) >>= fac' . canon of
                                                        Nothing -> False
                                                        Just (UnsignedShort word16') -> fromIntegral word16' == n
                                                 else False

case_UnsignedShort_IllegalTextRange :: Assertion
case_UnsignedShort_IllegalTextRange = assertBool "Some" (not . any id $ map f testRange)
  where
    testRange = (map (\n -> (n + 1) * shortNonNegativeIntegerOut) $ shortNonNegativeIntegers)
                ++
                (map (\n -> (n + 1) * (-1)) $ shortNonNegativeIntegers)
    f :: Integer -> Bool
    f n = let text = tshow n
              mUnsignedShort :: Maybe UnsignedShort
              mUnsignedShort = fac text
          in case mUnsignedShort of
               Nothing -> False
               Just (UnsignedShort word16) -> fromIntegral word16 == n

case_UnsignedShort_IllegalTextRangeWithWS :: Assertion
case_UnsignedShort_IllegalTextRangeWithWS = assertBool "Some" (not . any id $ map f testRange)
  where
    testRange = (map (\n -> (n + 1) * shortNonNegativeIntegerOut) $ shortNonNegativeIntegers)
                ++
                (map (\n -> (n + 1) * (-1)) $ shortNonNegativeIntegers)
    f :: Integer -> Bool
    f n = let text = tshow n
              textWS = T.concat ["\n\r\t", text, "\t  \r\n  "]
              mUnsignedShort :: Maybe UnsignedShort
              mUnsignedShort = fac textWS
          in case mUnsignedShort of
               Nothing -> False
               Just (UnsignedShort word16) -> fromIntegral word16 == n

case_UnsignedShort_IllegalTextWithOther :: Assertion
case_UnsignedShort_IllegalTextWithOther = assertBool "Some" (not . any id $ map f testRange)
  where
    testRange = shortNonNegativeIntegers
    f :: Integer -> Bool
    f n = let text = tshow n
              textWS = T.concat ["\n\r\t", text, "✓\t  \r\n  "]
              mUnsignedShort :: Maybe UnsignedShort
              mUnsignedShort = fac textWS
          in case mUnsignedShort of
               Nothing -> False
               Just (UnsignedShort word16) -> fromIntegral word16 == n

intNonNegativeIntegers :: [] Integer
intNonNegativeIntegers =
  range (fromIntegral (minBound :: Word32), fromIntegral (maxBound :: Word8)) -- Shorten the range or wait forever
  ++
  range (fromIntegral (maxBound :: Word32) - fromIntegral (maxBound :: Word8), fromIntegral (maxBound :: Word32))

intNonNegativeIntegerOut :: Integer
intNonNegativeIntegerOut = fromIntegral (maxBound :: Word32) + 1

case_UnsignedInt_LegalText :: Assertion
case_UnsignedInt_LegalText = assertBool "Not All" (all id $ map f testRange)
  where
    testRange = intNonNegativeIntegers
    f :: Integer -> Bool
    f n = let text = tshow n
              mUnsignedInt :: Maybe UnsignedInt
              mUnsignedInt = fac text
          in case mUnsignedInt of
               Nothing -> False
               Just (UnsignedInt word32) -> fromIntegral word32 == n

case_UnsignedInt_LegalTextWithWS :: Assertion
case_UnsignedInt_LegalTextWithWS = assertBool "Not All" (all id $ map f testRange)
  where
    testRange = intNonNegativeIntegers
    f :: Integer -> Bool
    f n = let text = tshow n
              textWS = T.concat ["\n\r\t", text, "\t  \r\n  "]
              mUnsignedInt :: Maybe UnsignedInt
              mUnsignedInt = fac textWS
          in case mUnsignedInt of
               Nothing -> False
               Just (UnsignedInt word32) -> fromIntegral word32 == n

case_UnsignedInt_LegalTextWithPlus :: Assertion
case_UnsignedInt_LegalTextWithPlus = assertBool "Not All" (all id $ map f testRange)
  where
    testRange = intNonNegativeIntegers
    f :: Integer -> Bool
    f n = let text = tshow n
              textWS = T.append "+" text
              mUnsignedInt :: Maybe UnsignedInt
              mUnsignedInt = fac textWS
          in case mUnsignedInt of
               Nothing -> False
               Just (UnsignedInt word32) -> fromIntegral word32 == n

case_UnsignedInt_LegalTextWithMinus :: Assertion
case_UnsignedInt_LegalTextWithMinus = assertBool "Borked" minusTest
  where
    text = "-0"
    mUnsignedInt :: Maybe UnsignedInt
    mUnsignedInt = fac text
    minusTest = case mUnsignedInt of
                  Nothing -> False
                  Just (UnsignedInt word32) -> word32 == 0

case_UnsignedInt_Idempotent :: Assertion
case_UnsignedInt_Idempotent = assertBool "Not All" (all id $ map f testRange)
  where
    testRange = intNonNegativeIntegers
    f :: Integer -> Bool
    f n = let text = tshow n
              mUnsignedInt = fac' text
              fac' :: Text -> Maybe UnsignedInt
              fac' = fac
          in case mUnsignedInt of
               Nothing -> False
               Just ub@(UnsignedInt word32) -> if fromIntegral word32 == n
                                               then case fac' (canon ub) >>= fac' . canon of
                                                      Nothing -> False
                                                      Just (UnsignedInt word32') -> fromIntegral word32' == n
                                               else False

case_UnsignedInt_IllegalTextRange :: Assertion
case_UnsignedInt_IllegalTextRange = assertBool "Some" (not . any id $ map f testRange)
  where
    testRange = (map (\n -> (n + 1) * intNonNegativeIntegerOut) $ intNonNegativeIntegers)
                ++
                (map (\n -> (n + 1) * (-1)) $ intNonNegativeIntegers)
    f :: Integer -> Bool
    f n = let text = tshow n
              mUnsignedInt :: Maybe UnsignedInt
              mUnsignedInt = fac text
          in case mUnsignedInt of
               Nothing -> False
               Just (UnsignedInt word32) -> fromIntegral word32 == n

case_UnsignedInt_IllegalTextRangeWithWS :: Assertion
case_UnsignedInt_IllegalTextRangeWithWS = assertBool "Some" (not . any id $ map f testRange)
  where
    testRange = (map (\n -> (n + 1) * intNonNegativeIntegerOut) $ intNonNegativeIntegers)
                ++
                (map (\n -> (n + 1) * (-1)) $ intNonNegativeIntegers)
    f :: Integer -> Bool
    f n = let text = tshow n
              textWS = T.concat ["\n\r\t", text, "\t  \r\n  "]
              mUnsignedInt :: Maybe UnsignedInt
              mUnsignedInt = fac textWS
          in case mUnsignedInt of
               Nothing -> False
               Just (UnsignedInt word32) -> fromIntegral word32 == n

case_UnsignedInt_IllegalTextWithOther :: Assertion
case_UnsignedInt_IllegalTextWithOther = assertBool "Some" (not . any id $ map f testRange)
  where
    testRange = intNonNegativeIntegers
    f :: Integer -> Bool
    f n = let text = tshow n
              textWS = T.concat ["\n\r\t", text, "✓\t  \r\n  "]
              mUnsignedInt :: Maybe UnsignedInt
              mUnsignedInt = fac textWS
          in case mUnsignedInt of
               Nothing -> False
               Just (UnsignedInt word32) -> fromIntegral word32 == n

longNonNegativeIntegers :: [] Integer
longNonNegativeIntegers =
  range (fromIntegral (minBound :: Word64), fromIntegral (maxBound :: Word8)) -- Shorten the range or wait forever
  ++
  range (fromIntegral (maxBound :: Word64) - fromIntegral (maxBound :: Word8), fromIntegral (maxBound :: Word64))

longNonNegativeIntegerOut :: Integer
longNonNegativeIntegerOut = fromIntegral (maxBound :: Word64) + 1

case_UnsignedLong_LegalText :: Assertion
case_UnsignedLong_LegalText = assertBool "Not All" (all id $ map f testRange)
  where
    testRange = longNonNegativeIntegers
    f :: Integer -> Bool
    f n = let text = tshow n
              mUnsignedLong :: Maybe UnsignedLong
              mUnsignedLong = fac text
          in case mUnsignedLong of
               Nothing -> False
               Just (UnsignedLong word64) -> fromIntegral word64 == n

case_UnsignedLong_LegalTextWithWS :: Assertion
case_UnsignedLong_LegalTextWithWS = assertBool "Not All" (all id $ map f testRange)
  where
    testRange = longNonNegativeIntegers
    f :: Integer -> Bool
    f n = let text = tshow n
              textWS = T.concat ["\n\r\t", text, "\t  \r\n  "]
              mUnsignedLong :: Maybe UnsignedLong
              mUnsignedLong = fac textWS
          in case mUnsignedLong of
               Nothing -> False
               Just (UnsignedLong word64) -> fromIntegral word64 == n

case_UnsignedLong_LegalTextWithPlus :: Assertion
case_UnsignedLong_LegalTextWithPlus = assertBool "Not All" (all id $ map f testRange)
  where
    testRange = longNonNegativeIntegers
    f :: Integer -> Bool
    f n = let text = tshow n
              textWS = T.append "+" text
              mUnsignedLong :: Maybe UnsignedLong
              mUnsignedLong = fac textWS
          in case mUnsignedLong of
               Nothing -> False
               Just (UnsignedLong word64) -> fromIntegral word64 == n

case_UnsignedLong_LegalTextWithMinus :: Assertion
case_UnsignedLong_LegalTextWithMinus = assertBool "Borked" minusTest
  where
    text = "-0"
    mUnsignedLong :: Maybe UnsignedLong
    mUnsignedLong = fac text
    minusTest = case mUnsignedLong of
                  Nothing -> False
                  Just (UnsignedLong word64) -> word64 == 0

case_UnsignedLong_Idempotent :: Assertion
case_UnsignedLong_Idempotent = assertBool "Not All" (all id $ map f testRange)
  where
    testRange = longNonNegativeIntegers
    f :: Integer -> Bool
    f n = let text = tshow n
              mUnsignedLong = fac' text
              fac' :: Text -> Maybe UnsignedLong
              fac' = fac
          in case mUnsignedLong of
               Nothing -> False
               Just ub@(UnsignedLong word64) -> if fromIntegral word64 == n
                                                then case fac' (canon ub) >>= fac' . canon of
                                                       Nothing -> False
                                                       Just (UnsignedLong word64') -> fromIntegral word64' == n
                                                else False

case_UnsignedLong_IllegalTextRange :: Assertion
case_UnsignedLong_IllegalTextRange = assertBool "Some" (not . any id $ map f testRange)
  where
    testRange = (map (\n -> (n + 1) * longNonNegativeIntegerOut) $ longNonNegativeIntegers)
                ++
                (map (\n -> (n + 1) * (-1)) $ longNonNegativeIntegers)
    f :: Integer -> Bool
    f n = let text = tshow n
              mUnsignedLong :: Maybe UnsignedLong
              mUnsignedLong = fac text
          in case mUnsignedLong of
               Nothing -> False
               Just (UnsignedLong word64) -> fromIntegral word64 == n

case_UnsignedLong_IllegalTextRangeWithWS :: Assertion
case_UnsignedLong_IllegalTextRangeWithWS = assertBool "Some" (not . any id $ map f testRange)
  where
    testRange = (map (\n -> (n + 1) * longNonNegativeIntegerOut) $ longNonNegativeIntegers)
                ++
                (map (\n -> (n + 1) * (-1)) $ longNonNegativeIntegers)
    f :: Integer -> Bool
    f n = let text = tshow n
              textWS = T.concat ["\n\r\t", text, "\t  \r\n  "]
              mUnsignedLong :: Maybe UnsignedLong
              mUnsignedLong = fac textWS
          in case mUnsignedLong of
               Nothing -> False
               Just (UnsignedLong word64) -> fromIntegral word64 == n

case_UnsignedLong_IllegalTextWithOther :: Assertion
case_UnsignedLong_IllegalTextWithOther = assertBool "Some" (not . any id $ map f testRange)
  where
    testRange = longNonNegativeIntegers
    f :: Integer -> Bool
    f n = let text = tshow n
              textWS = T.concat ["\n\r\t", text, "✓\t  \r\n  "]
              mUnsignedLong :: Maybe UnsignedLong
              mUnsignedLong = fac textWS
          in case mUnsignedLong of
               Nothing -> False
               Just (UnsignedLong word64) -> fromIntegral word64 == n

byteIntegers :: [] Integer
byteIntegers = range (fromIntegral (minBound :: Int8), fromIntegral (maxBound :: Int8))

byteIntegerOut :: Integer
byteIntegerOut = fromIntegral (minBound :: Int8) - 1

case_Byte_LegalText :: Assertion
case_Byte_LegalText = assertBool "Not All" (all id $ map f testRange)
  where
    testRange = byteIntegers
    f :: Integer -> Bool
    f n = let text = tshow n
              mByte :: Maybe Byte
              mByte = fac text
          in case mByte of
               Nothing -> False
               Just (Byte int8) -> fromIntegral int8 == n

case_Byte_LegalTextWithWS :: Assertion
case_Byte_LegalTextWithWS = assertBool "Not All" (all id $ map f testRange)
  where
    testRange = byteIntegers
    f :: Integer -> Bool
    f n = let text = tshow n
              textWS = T.concat ["\n\r\t", text, "\t  \r\n  "]
              mByte :: Maybe Byte
              mByte = fac textWS
          in case mByte of
               Nothing -> False
               Just (Byte int8) -> fromIntegral int8 == n

case_Byte_LegalTextWithPlus :: Assertion
case_Byte_LegalTextWithPlus = assertBool "Not All" (all id $ map f testRange)
  where
    testRange = byteIntegers
    f :: Integer -> Bool
    f n = let text = tshow n
              textWS = if n >= 0
                       then T.append "+" text
                       else text
              mByte :: Maybe Byte
              mByte = fac textWS
          in case mByte of
               Nothing -> False
               Just (Byte int8) -> fromIntegral int8 == n

case_Byte_LegalTextWithMinus :: Assertion
case_Byte_LegalTextWithMinus = assertBool "Borked" minusTest
  where
    text = "-0"
    mByte :: Maybe Byte
    mByte = fac text
    minusTest = case mByte of
                  Nothing -> False
                  Just (Byte int8) -> int8 == 0

case_Byte_Idempotent :: Assertion
case_Byte_Idempotent = assertBool "Not All" (all id $ map f testRange)
  where
    testRange = byteIntegers
    f :: Integer -> Bool
    f n = let text = tshow n
              mByte = fac' text
              fac' :: Text -> Maybe Byte
              fac' = fac
          in case mByte of
               Nothing -> False
               Just ub@(Byte int8) -> if fromIntegral int8 == n
                                      then case fac' (canon ub) >>= fac' . canon of
                                             Nothing -> False
                                             Just (Byte int8') -> fromIntegral int8' == n
                                      else False

case_Byte_IllegalTextRange :: Assertion
case_Byte_IllegalTextRange = assertBool "Some" (not . any id $ map f testRange)
  where
    testRange = map (* byteIntegerOut) $ filter (/= 0) byteIntegers
    f :: Integer -> Bool
    f n = let text = tshow n
              mByte :: Maybe Byte
              mByte = fac text
          in case mByte of
               Nothing -> False
               Just (Byte int8) -> fromIntegral int8 == n

case_Byte_IllegalTextRangeWithWS :: Assertion
case_Byte_IllegalTextRangeWithWS = assertBool "Some" (not . any id $ map f testRange)
  where
    testRange = (map (* byteIntegerOut) $ filter (/= 0) byteIntegers)
    f :: Integer -> Bool
    f n = let text = tshow n
              textWS = T.concat ["\n\r\t", text, "\t  \r\n  "]
              mByte :: Maybe Byte
              mByte = fac textWS
          in case mByte of
               Nothing -> False
               Just (Byte int8) -> fromIntegral int8 == n

case_Byte_IllegalTextWithOther :: Assertion
case_Byte_IllegalTextWithOther = assertBool "Some" (not . any id $ map f testRange)
  where
    testRange = byteIntegers
    f :: Integer -> Bool
    f n = let text = tshow n
              textWS = T.concat ["\n\r\t", text, "✓\t  \r\n  "]
              mByte :: Maybe Byte
              mByte = fac textWS
          in case mByte of
               Nothing -> False
               Just (Byte int8) -> fromIntegral int8 == n

shortIntegers :: [] Integer
shortIntegers = range (fromIntegral (minBound :: Int16), fromIntegral (maxBound :: Int16))

shortIntegerOut :: Integer
shortIntegerOut = fromIntegral (minBound :: Int16) - 1

case_Short_LegalText :: Assertion
case_Short_LegalText = assertBool "Not All" (all id $ map f testRange)
  where
    testRange = shortIntegers
    f :: Integer -> Bool
    f n = let text = tshow n
              mShort :: Maybe Short
              mShort = fac text
          in case mShort of
               Nothing -> False
               Just (Short int16) -> fromIntegral int16 == n

case_Short_LegalTextWithWS :: Assertion
case_Short_LegalTextWithWS = assertBool "Not All" (all id $ map f testRange)
  where
    testRange = shortIntegers
    f :: Integer -> Bool
    f n = let text = tshow n
              textWS = T.concat ["\n\r\t", text, "\t  \r\n  "]
              mShort :: Maybe Short
              mShort = fac textWS
          in case mShort of
               Nothing -> False
               Just (Short int16) -> fromIntegral int16 == n

case_Short_LegalTextWithPlus :: Assertion
case_Short_LegalTextWithPlus = assertBool "Not All" (all id $ map f testRange)
  where
    testRange = shortIntegers
    f :: Integer -> Bool
    f n = let text = tshow n
              textWS = if n >= 0
                       then T.append "+" text
                       else text
              mShort :: Maybe Short
              mShort = fac textWS
          in case mShort of
               Nothing -> False
               Just (Short int16) -> fromIntegral int16 == n

case_Short_LegalTextWithMinus :: Assertion
case_Short_LegalTextWithMinus = assertBool "Borked" minusTest
  where
    text = "-0"
    mShort :: Maybe Short
    mShort = fac text
    minusTest = case mShort of
                  Nothing -> False
                  Just (Short int16) -> int16 == 0

case_Short_Idempotent :: Assertion
case_Short_Idempotent = assertBool "Not All" (all id $ map f testRange)
  where
    testRange = shortIntegers
    f :: Integer -> Bool
    f n = let text = tshow n
              mShort = fac' text
              fac' :: Text -> Maybe Short
              fac' = fac
          in case mShort of
               Nothing -> False
               Just ub@(Short int16) ->
                 if fromIntegral int16 == n
                 then case fac' (canon ub) >>= fac' . canon of
                        Nothing -> False
                        Just (Short int16') -> fromIntegral int16' == n
                 else False

case_Short_IllegalTextRange :: Assertion
case_Short_IllegalTextRange = assertBool "Some" (not . any id $ map f testRange)
  where
    testRange = map (* shortIntegerOut) $ filter (/= 0) shortIntegers
    f :: Integer -> Bool
    f n = let text = tshow n
              mShort :: Maybe Short
              mShort = fac text
          in case mShort of
               Nothing -> False
               Just (Short int16) -> fromIntegral int16 == n

case_Short_IllegalTextRangeWithWS :: Assertion
case_Short_IllegalTextRangeWithWS = assertBool "Some" (not . any id $ map f testRange)
  where
    testRange = (map (* shortIntegerOut) $ filter (/= 0) shortIntegers)
    f :: Integer -> Bool
    f n = let text = tshow n
              textWS = T.concat ["\n\r\t", text, "\t  \r\n  "]
              mShort :: Maybe Short
              mShort = fac textWS
          in case mShort of
               Nothing -> False
               Just (Short int16) -> fromIntegral int16 == n

case_Short_IllegalTextWithOther :: Assertion
case_Short_IllegalTextWithOther = assertBool "Some" (not . any id $ map f testRange)
  where
    testRange = shortIntegers
    f :: Integer -> Bool
    f n = let text = tshow n
              textWS = T.concat ["\n\r\t", text, "✓\t  \r\n  "]
              mShort :: Maybe Short
              mShort = fac textWS
          in case mShort of
               Nothing -> False
               Just (Short int16) -> fromIntegral int16 == n

intxsIntegers :: [] Integer
intxsIntegers = join
  [ range ( fromIntegral (minBound :: Int32)
          , fromIntegral (minBound :: Int32) + fromIntegral (maxBound :: Int8)
          )
  , byteIntegers -- Cover the -int8 to +int8 range 0 must be covered
  , range ( fromIntegral (maxBound :: Int32) - fromIntegral (maxBound :: Int8)
          , fromIntegral (maxBound :: Int32)
          )
  ]

intxsIntegerOut :: Integer
intxsIntegerOut = fromIntegral (minBound :: Int32) - 1

case_Intxs_LegalText :: Assertion
case_Intxs_LegalText = assertBool "Not All" (all id $ map f testRange)
  where
    testRange = intxsIntegers
    f :: Integer -> Bool
    f n = let text = tshow n
              mIntxs :: Maybe Intxs
              mIntxs = fac text
          in case mIntxs of
               Nothing -> False
               Just (Intxs int32) -> fromIntegral int32 == n

case_Intxs_LegalTextWithWS :: Assertion
case_Intxs_LegalTextWithWS = assertBool "Not All" (all id $ map f testRange)
  where
    testRange = intxsIntegers
    f :: Integer -> Bool
    f n = let text = tshow n
              textWS = T.concat ["\n\r\t", text, "\t  \r\n  "]
              mIntxs :: Maybe Intxs
              mIntxs = fac textWS
          in case mIntxs of
               Nothing -> False
               Just (Intxs int32) -> fromIntegral int32 == n

case_Intxs_LegalTextWithPlus :: Assertion
case_Intxs_LegalTextWithPlus = assertBool "Not All" (all id $ map f testRange)
  where
    testRange = intxsIntegers
    f :: Integer -> Bool
    f n = let text = tshow n
              textWS = if n >= 0
                       then T.append "+" text
                       else text
              mIntxs :: Maybe Intxs
              mIntxs = fac textWS
          in case mIntxs of
               Nothing -> False
               Just (Intxs int32) -> fromIntegral int32 == n

case_Intxs_LegalTextWithMinus :: Assertion
case_Intxs_LegalTextWithMinus = assertBool "Borked" minusTest
  where
    text = "-0"
    mIntxs :: Maybe Intxs
    mIntxs = fac text
    minusTest = case mIntxs of
                  Nothing -> False
                  Just (Intxs int32) -> int32 == 0

case_Intxs_Idempotent :: Assertion
case_Intxs_Idempotent = assertBool "Not All" (all id $ map f testRange)
  where
    testRange = intxsIntegers
    f :: Integer -> Bool
    f n = let text = tshow n
              mIntxs = fac' text
              fac' :: Text -> Maybe Intxs
              fac' = fac
          in case mIntxs of
               Nothing -> False
               Just ub@(Intxs int32) -> if fromIntegral int32 == n
                                        then case fac' (canon ub) >>= fac' . canon of
                                               Nothing -> False
                                               Just (Intxs int32') -> fromIntegral int32' == n
                                        else False

case_Intxs_IllegalTextRange :: Assertion
case_Intxs_IllegalTextRange = assertBool "Some" (not . any id $ map f testRange)
  where
    testRange = map (* intxsIntegerOut) $ filter (/= 0) intxsIntegers
    f :: Integer -> Bool
    f n = let text = tshow n
              mIntxs :: Maybe Intxs
              mIntxs = fac text
          in case mIntxs of
               Nothing -> False
               Just (Intxs int32) -> fromIntegral int32 == n

case_Intxs_IllegalTextRangeWithWS :: Assertion
case_Intxs_IllegalTextRangeWithWS = assertBool "Some" (not . any id $ map f testRange)
  where
    testRange = (map (* intxsIntegerOut) $ filter (/= 0) intxsIntegers)
    f :: Integer -> Bool
    f n = let text = tshow n
              textWS = T.concat ["\n\r\t", text, "\t  \r\n  "]
              mIntxs :: Maybe Intxs
              mIntxs = fac textWS
          in case mIntxs of
               Nothing -> False
               Just (Intxs int32) -> fromIntegral int32 == n

case_Intxs_IllegalTextWithOther :: Assertion
case_Intxs_IllegalTextWithOther = assertBool "Some" (not . any id $ map f testRange)
  where
    testRange = intxsIntegers
    f :: Integer -> Bool
    f n = let text = tshow n
              textWS = T.concat ["\n\r\t", text, "✓\t  \r\n  "]
              mIntxs :: Maybe Intxs
              mIntxs = fac textWS
          in case mIntxs of
               Nothing -> False
               Just (Intxs int32) -> fromIntegral int32 == n

longIntegers :: [] Integer
longIntegers = range (fromIntegral (minBound :: Int64), fromIntegral (minBound :: Int64) + fromIntegral (maxBound :: Int8))
               ++
               byteIntegers -- Cover the -int8 to +int8 range 0 must be covered
               ++
               range (fromIntegral (maxBound :: Int64) - fromIntegral (maxBound :: Int8), fromIntegral (maxBound :: Int64))

longIntegerOut :: Integer
longIntegerOut = fromIntegral (minBound :: Int64) - 1

case_Long_LegalText :: Assertion
case_Long_LegalText = assertBool "Not All" (all id $ map f testRange)
  where
    testRange = longIntegers
    f :: Integer -> Bool
    f n = let text = tshow n
              mLong :: Maybe Long
              mLong = fac text
          in case mLong of
               Nothing -> False
               Just (Long int64) -> fromIntegral int64 == n

case_Long_LegalTextWithWS :: Assertion
case_Long_LegalTextWithWS = assertBool "Not All" (all id $ map f testRange)
  where
    testRange = longIntegers
    f :: Integer -> Bool
    f n = let text = tshow n
              textWS = T.concat ["\n\r\t", text, "\t  \r\n  "]
              mLong :: Maybe Long
              mLong = fac textWS
          in case mLong of
               Nothing -> False
               Just (Long int64) -> fromIntegral int64 == n

case_Long_LegalTextWithPlus :: Assertion
case_Long_LegalTextWithPlus = assertBool "Not All" (all id $ map f testRange)
  where
    testRange = longIntegers
    f :: Integer -> Bool
    f n = let text = tshow n
              textWS = n >= 0 ? T.append "+" text $ text
              mLong :: Maybe Long
              mLong = fac textWS
          in case mLong of
               Nothing -> False
               Just (Long int64) -> fromIntegral int64 == n

case_Long_LegalTextWithMinus :: Assertion
case_Long_LegalTextWithMinus = assertBool "Borked" minusTest
  where
    text = "-0"
    mLong :: Maybe Long
    mLong = fac text
    minusTest = case mLong of
                  Nothing -> False
                  Just (Long int64) -> int64 == 0

case_Long_Idempotent :: Assertion
case_Long_Idempotent = assertBool "Not All" (all id $ map f testRange)
  where
    testRange = longIntegers
    f :: Integer -> Bool
    f n = let text = tshow n
              mLong = fac' text
              fac' :: Text -> Maybe Long
              fac' = fac
          in case mLong of
               Nothing -> False
               Just ub@(Long int64) ->
                 if fromIntegral int64 == n
                 then case fac' (canon ub) >>= fac' . canon of
                        Nothing -> False
                        Just (Long int64') -> fromIntegral int64' == n
                 else False

case_Long_IllegalTextRange :: Assertion
case_Long_IllegalTextRange = assertBool "Some" (not . any id $ map f testRange)
  where
    testRange = map (* longIntegerOut) $ filter (/= 0) longIntegers
    f :: Integer -> Bool
    f n = let text = tshow n
              mLong :: Maybe Long
              mLong = fac text
          in case mLong of
               Nothing -> False
               Just (Long int64) -> fromIntegral int64 == n

case_Long_IllegalTextRangeWithWS :: Assertion
case_Long_IllegalTextRangeWithWS = assertBool "Some" (not . any id $ map f testRange)
  where
    testRange = (map (* longIntegerOut) $ filter (/= 0) longIntegers)
    f :: Integer -> Bool
    f n = let text = tshow n
              textWS = T.concat ["\n\r\t", text, "\t  \r\n  "]
              mLong :: Maybe Long
              mLong = fac textWS
          in case mLong of
               Nothing -> False
               Just (Long int64) -> fromIntegral int64 == n

case_Long_IllegalTextWithOther :: Assertion
case_Long_IllegalTextWithOther = assertBool "Some" (not . any id $ map f testRange)
  where
    testRange = longIntegers
    f :: Integer -> Bool
    f n = let text = tshow n
              textWS = T.concat ["\n\r\t", text, "✓\t  \r\n  "]
              mLong :: Maybe Long
              mLong = fac textWS
          in case mLong of
               Nothing -> False
               Just (Long int64) -> fromIntegral int64 == n

-- -----------------------------------------------------------------------------------------------------------------------------------------------------
-- This section contains the Integer types tests. Boilerplate world. Could use some love.

integerTests :: [TestTree]
integerTests = [ testCase "Integer Legal Text"                            case_Integer_LegalText
               , testCase "Integer Legal Text With WS"                    case_Integer_LegalTextWithWS
               , testCase "Integer Legal Text With Plus"                  case_Integer_LegalTextWithPlus
               , testCase "Integer Legal Text With Minus"                 case_Integer_LegalTextWithMinus
               , testCase "Integer Legal Text Idempotent"                 case_Integer_Idempotent
               , testCase "Integer Illegal Text With Other"               case_Integer_IllegalTextWithOther

               , testCase "NonPositiveInteger Legal Text"                 case_NonPositiveInteger_LegalText
               , testCase "NonPositiveInteger Legal Text With WS"         case_NonPositiveInteger_LegalTextWithWS
               , testCase "NonPositiveInteger Legal Text With Plus"       case_NonPositiveInteger_LegalTextWithPlus
               , testCase "NonPositiveInteger Legal Text With Minus"      case_NonPositiveInteger_LegalTextWithMinus
               , testCase "NonPositiveInteger Legal Text Idempotent"      case_NonPositiveInteger_Idempotent
               , testCase "NonPositiveInteger Illegal Text Range"         case_NonPositiveInteger_IllegalTextRange
               , testCase "NonPositiveInteger Illegal Text Range With WS" case_NonPositiveInteger_IllegalTextRangeWithWS
               , testCase "NonPositiveInteger Illegal Text With Other"    case_NonPositiveInteger_IllegalTextWithOther

               , testCase "NegativeInteger Legal Text"                    case_NegativeInteger_LegalText
               , testCase "NegativeInteger Legal Text With WS"            case_NegativeInteger_LegalTextWithWS
               , testCase "NegativeInteger Legal Text With Plus"          case_NegativeInteger_LegalTextWithPlus
               , testCase "NegativeInteger Legal Text With Minus"         case_NegativeInteger_LegalTextWithMinus
               , testCase "NegativeInteger Legal Text Idempotent"         case_NegativeInteger_Idempotent
               , testCase "NegativeInteger Illegal Text Range"            case_NegativeInteger_IllegalTextRange
               , testCase "NegativeInteger Illegal Text Range With WS"    case_NegativeInteger_IllegalTextRangeWithWS
               , testCase "NegativeInteger Illegal Text With Other"       case_NegativeInteger_IllegalTextWithOther

               , testCase "NonNegativeInteger Legal Text"                 case_NonNegativeInteger_LegalText
               , testCase "NonNegativeInteger Legal Text With WS"         case_NonNegativeInteger_LegalTextWithWS
               , testCase "NonNegativeInteger Legal Text With Plus"       case_NonNegativeInteger_LegalTextWithPlus
               , testCase "NonNegativeInteger Legal Text With Minus"      case_NonNegativeInteger_LegalTextWithMinus
               , testCase "NonNegativeInteger Legal Text Idempotent"      case_NonNegativeInteger_Idempotent
               , testCase "NonNegativeInteger Illegal Text Range"         case_NonNegativeInteger_IllegalTextRange
               , testCase "NonNegativeInteger Illegal Text Range With WS" case_NonNegativeInteger_IllegalTextRangeWithWS
               , testCase "NonNegativeInteger Illegal Text With Other"    case_NonNegativeInteger_IllegalTextWithOther

               , testCase "PositiveInteger Legal Text"                    case_PositiveInteger_LegalText
               , testCase "PositiveInteger Legal Text With WS"            case_PositiveInteger_LegalTextWithWS
               , testCase "PositiveInteger Legal Text With Plus"          case_PositiveInteger_LegalTextWithPlus
               , testCase "PositiveInteger Legal Text With Minus"         case_PositiveInteger_LegalTextWithMinus
               , testCase "PositiveInteger Legal Text Idempotent"         case_PositiveInteger_Idempotent
               , testCase "PositiveInteger Illegal Text Range"            case_PositiveInteger_IllegalTextRange
               , testCase "PositiveInteger Illegal Text Range With WS"    case_PositiveInteger_IllegalTextRangeWithWS
               , testCase "PositiveInteger Illegal Text With Other"       case_PositiveInteger_IllegalTextWithOther
               ]

integers :: [] Integer -- The integers range is arbitrary since Integer is unbounded.
integers = range (fromIntegral (minBound :: Int64), fromIntegral (minBound :: Int64) + fromIntegral (maxBound :: Int8))
           ++
           byteIntegers -- Cover the -int8 to +int8 range 0 must be covered
           ++
           range (fromIntegral (maxBound :: Int64) - fromIntegral (maxBound :: Int8), fromIntegral (maxBound :: Int64))

case_Integer_LegalText :: Assertion
case_Integer_LegalText = assertBool "Not All" (all id $ map f testRange)
  where
    testRange = integers
    f :: Integer -> Bool
    f n = let text = tshow n
              mInteger :: Maybe Integer
              mInteger = fac text
          in case mInteger of
               Nothing -> False
               Just integer -> integer == n

case_Integer_LegalTextWithWS :: Assertion
case_Integer_LegalTextWithWS = assertBool "Not All" (all id $ map f testRange)
  where
    testRange = integers
    f :: Integer -> Bool
    f n = let text = tshow n
              textWS = T.concat ["\n\r\t", text, "\t  \r\n  "]
              mInteger :: Maybe Integer
              mInteger = fac textWS
          in case mInteger of
               Nothing -> False
               Just integer -> integer == n

case_Integer_LegalTextWithPlus :: Assertion
case_Integer_LegalTextWithPlus = assertBool "Not All" (all id $ map f testRange)
  where
    testRange = integers
    f :: Integer -> Bool
    f n = let text = tshow n
              textWS = if n >= 0
                       then T.append "+" text
                       else text
              mInteger :: Maybe Integer
              mInteger = fac textWS
          in case mInteger of
               Nothing -> False
               Just integer -> integer == n

case_Integer_LegalTextWithMinus :: Assertion
case_Integer_LegalTextWithMinus = assertBool "Borked" minusTest
  where
    text = "-0"
    mInteger :: Maybe Integer
    mInteger = fac text
    minusTest = case mInteger of
                  Nothing -> False
                  Just integer -> integer == 0

case_Integer_Idempotent :: Assertion
case_Integer_Idempotent = assertBool "Not All" (all id $ map f testRange)
  where
    testRange = integers
    f :: Integer -> Bool
    f n = let text = tshow n
              mInteger = fac' text
              fac' :: Text -> Maybe Integer
              fac' = fac
          in case mInteger of
               Nothing -> False
               Just ub@integer -> if integer == n
                                       then case fac' (canon ub) >>= fac' . canon of
                                              Nothing -> False
                                              Just integer' -> integer' == n
                                       else False

case_Integer_IllegalTextWithOther :: Assertion
case_Integer_IllegalTextWithOther = assertBool "Some" (not . any id $ map f testRange)
  where
    testRange = integers
    f :: Integer -> Bool
    f n = let text = tshow n
              textWS = T.concat ["\n\r\t", text, "✓\t  \r\n  "]
              mInteger :: Maybe Integer
              mInteger = fac textWS
          in case mInteger of
               Nothing -> False
               Just integer -> integer == n

nonPositiveIntegers :: [] Integer
nonPositiveIntegers = filter (<= 0) integers

case_NonPositiveInteger_LegalText :: Assertion
case_NonPositiveInteger_LegalText = assertBool "Not All" (all id $ map f testRange)
  where
    testRange = nonPositiveIntegers
    f :: Integer -> Bool
    f n = let text = tshow n
              mNonPositiveInteger :: Maybe NonPositiveInteger
              mNonPositiveInteger = fac text
          in case mNonPositiveInteger of
               Nothing -> False
               Just nonPositiveInteger -> redde nonPositiveInteger == n

case_NonPositiveInteger_LegalTextWithWS :: Assertion
case_NonPositiveInteger_LegalTextWithWS = assertBool "Not All" (all id $ map f testRange)
  where
    testRange = nonPositiveIntegers
    f :: Integer -> Bool
    f n = let text = tshow n
              textWS = T.concat ["\n\r\t", text, "\t  \r\n  "]
              mNonPositiveInteger :: Maybe NonPositiveInteger
              mNonPositiveInteger = fac textWS
          in case mNonPositiveInteger of
               Nothing -> False
               Just nonPositiveInteger -> redde nonPositiveInteger == n

case_NonPositiveInteger_LegalTextWithPlus :: Assertion
case_NonPositiveInteger_LegalTextWithPlus = assertBool "Borked" plusTest
  where
    text = "+0"
    mNonPositiveInteger :: Maybe NonPositiveInteger
    mNonPositiveInteger = fac text
    plusTest = case mNonPositiveInteger of
                 Nothing -> False
                 Just nonPositiveInteger -> redde nonPositiveInteger == (0 :: Integer)

case_NonPositiveInteger_LegalTextWithMinus :: Assertion
case_NonPositiveInteger_LegalTextWithMinus = assertBool "Borked" minusTest
  where
    text = "-0"
    mNonPositiveInteger :: Maybe NonPositiveInteger
    mNonPositiveInteger = fac text
    minusTest = case mNonPositiveInteger of
                  Nothing -> False
                  Just nonPositiveInteger -> redde nonPositiveInteger == (0 :: Integer)

case_NonPositiveInteger_Idempotent :: Assertion
case_NonPositiveInteger_Idempotent = assertBool "Not All" (all id $ map f testRange)
  where
    testRange = nonPositiveIntegers
    f :: Integer -> Bool
    f n = let text = tshow n
              mNonPositiveInteger = fac' text
              fac' :: Text -> Maybe NonPositiveInteger
              fac' = fac
          in case mNonPositiveInteger of
               Nothing -> False
               Just nonPositiveInteger ->
                 if redde nonPositiveInteger == n
                 then case fac' (canon nonPositiveInteger) >>= fac' . canon of
                        Nothing -> False
                        Just nonPositiveInteger' -> redde nonPositiveInteger' == n
                 else False

case_NonPositiveInteger_IllegalTextRange :: Assertion
case_NonPositiveInteger_IllegalTextRange = assertBool "Some" (not . any id $ map f testRange)
  where
    testRange = (map (\n -> n * (-1)) $ filter (/= 0) nonPositiveIntegers)
    f :: Integer -> Bool
    f n = let text = tshow n
              mNonPositiveInteger :: Maybe NonPositiveInteger
              mNonPositiveInteger = fac text
          in case mNonPositiveInteger of
               Nothing -> False
               Just nonPositiveInteger -> redde nonPositiveInteger == n

case_NonPositiveInteger_IllegalTextRangeWithWS :: Assertion
case_NonPositiveInteger_IllegalTextRangeWithWS = assertBool "Some" (not . any id $ map f testRange)
  where
    testRange = (map (\n -> n * (-1)) $ filter (/= 0) nonPositiveIntegers)
    f :: Integer -> Bool
    f n = let text = tshow n
              textWS = T.concat ["\n\r\t", text, "\t  \r\n  "]
              mNonPositiveInteger :: Maybe NonPositiveInteger
              mNonPositiveInteger = fac textWS
          in case mNonPositiveInteger of
               Nothing -> False
               Just nonPositiveInteger -> redde nonPositiveInteger == n

case_NonPositiveInteger_IllegalTextWithOther :: Assertion
case_NonPositiveInteger_IllegalTextWithOther = assertBool "Some" (not . any id $ map f testRange)
  where
    testRange = nonPositiveIntegers
    f :: Integer -> Bool
    f n = let text = tshow n
              textWS = T.concat ["\n\r\t", text, "✓\t  \r\n  "]
              mNonPositiveInteger :: Maybe NonPositiveInteger
              mNonPositiveInteger = fac textWS
          in case mNonPositiveInteger of
               Nothing -> False
               Just nonPositiveInteger -> redde nonPositiveInteger == n

negativeIntegers :: [] Integer
negativeIntegers = filter (< 0) integers

case_NegativeInteger_LegalText :: Assertion
case_NegativeInteger_LegalText = assertBool "Not All" (all id $ map f testRange)
  where
    testRange = negativeIntegers
    f :: Integer -> Bool
    f n = let text = tshow n
              mNegativeInteger :: Maybe NegativeInteger
              mNegativeInteger = fac text
          in case mNegativeInteger of
               Nothing -> False
               Just negativeInteger -> redde negativeInteger == n

case_NegativeInteger_LegalTextWithWS :: Assertion
case_NegativeInteger_LegalTextWithWS = assertBool "Not All" (all id $ map f testRange)
  where
    testRange = negativeIntegers
    f :: Integer -> Bool
    f n = let text = tshow n
              textWS = T.concat ["\n\r\t", text, "\t  \r\n  "]
              mNegativeInteger :: Maybe NegativeInteger
              mNegativeInteger = fac textWS
          in case mNegativeInteger of
               Nothing -> False
               Just negativeInteger -> redde negativeInteger == n

case_NegativeInteger_LegalTextWithPlus :: Assertion
case_NegativeInteger_LegalTextWithPlus = assertBool "Borked" (not plusTest)
  where
    text = "+0"
    mNegativeInteger :: Maybe NegativeInteger
    mNegativeInteger = fac text
    plusTest = case mNegativeInteger of
                 Nothing -> False
                 Just negativeInteger -> redde negativeInteger == (0 :: Integer)

case_NegativeInteger_LegalTextWithMinus :: Assertion
case_NegativeInteger_LegalTextWithMinus = assertBool "Borked" (not minusTest)
  where
    text = "-0"
    mNegativeInteger :: Maybe NegativeInteger
    mNegativeInteger = fac text
    minusTest = case mNegativeInteger of
                  Nothing -> False
                  Just negativeInteger -> redde negativeInteger == (0 :: Integer)

case_NegativeInteger_Idempotent :: Assertion
case_NegativeInteger_Idempotent = assertBool "Not All" (all id $ map f testRange)
  where
    testRange = negativeIntegers
    f :: Integer -> Bool
    f n = let text = tshow n
              mNegativeInteger = fac' text
              fac' :: Text -> Maybe NegativeInteger
              fac' = fac
          in case mNegativeInteger of
               Nothing -> False
               Just negativeInteger ->
                 if redde negativeInteger == n
                 then case fac' (canon negativeInteger) >>= fac' . canon of
                        Nothing -> False
                        Just negativeInteger' -> redde negativeInteger' == n
                 else False

case_NegativeInteger_IllegalTextRange :: Assertion
case_NegativeInteger_IllegalTextRange = assertBool "Some" (not . any id $ map f testRange)
  where
    testRange = (map (\n -> n * (-1)) $ filter (/= 0) negativeIntegers)
    f :: Integer -> Bool
    f n = let text = tshow n
              mNegativeInteger :: Maybe NegativeInteger
              mNegativeInteger = fac text
          in case mNegativeInteger of
               Nothing -> False
               Just negativeInteger -> redde negativeInteger == n

case_NegativeInteger_IllegalTextRangeWithWS :: Assertion
case_NegativeInteger_IllegalTextRangeWithWS = assertBool "Some" (not . any id $ map f testRange)
  where
    testRange = (map (\n -> n * (-1)) $ filter (/= 0) negativeIntegers)
    f :: Integer -> Bool
    f n = let text = tshow n
              textWS = T.concat ["\n\r\t", text, "\t  \r\n  "]
              mNegativeInteger :: Maybe NegativeInteger
              mNegativeInteger = fac textWS
          in case mNegativeInteger of
               Nothing -> False
               Just negativeInteger -> redde negativeInteger == n

case_NegativeInteger_IllegalTextWithOther :: Assertion
case_NegativeInteger_IllegalTextWithOther = assertBool "Some" (not . any id $ map f testRange)
  where
    testRange = negativeIntegers
    f :: Integer -> Bool
    f n = let text = tshow n
              textWS = T.concat ["\n\r\t", text, "✓\t  \r\n  "]
              mNegativeInteger :: Maybe NegativeInteger
              mNegativeInteger = fac textWS
          in case mNegativeInteger of
               Nothing -> False
               Just negativeInteger -> redde negativeInteger == n

nonNegativeIntegers :: [] Integer
nonNegativeIntegers = filter (>= 0) integers

case_NonNegativeInteger_LegalText :: Assertion
case_NonNegativeInteger_LegalText = assertBool "Not All" (all id $ map f testRange)
  where
    testRange = nonNegativeIntegers
    f :: Integer -> Bool
    f n = let text = tshow n
              mNonNegativeInteger :: Maybe NonNegativeInteger
              mNonNegativeInteger = fac text
          in case mNonNegativeInteger of
               Nothing -> False
               Just nonNegativeInteger -> redde nonNegativeInteger == n

case_NonNegativeInteger_LegalTextWithWS :: Assertion
case_NonNegativeInteger_LegalTextWithWS = assertBool "Not All" (all id $ map f testRange)
  where
    testRange = nonNegativeIntegers
    f :: Integer -> Bool
    f n = let text = tshow n
              textWS = T.concat ["\n\r\t", text, "\t  \r\n  "]
              mNonNegativeInteger :: Maybe NonNegativeInteger
              mNonNegativeInteger = fac textWS
          in case mNonNegativeInteger of
               Nothing -> False
               Just nonNegativeInteger -> redde nonNegativeInteger == n

case_NonNegativeInteger_LegalTextWithPlus :: Assertion
case_NonNegativeInteger_LegalTextWithPlus = assertBool "Borked" plusTest
  where
    text = "+0"
    mNonNegativeInteger :: Maybe NonNegativeInteger
    mNonNegativeInteger = fac text
    plusTest = case mNonNegativeInteger of
                 Nothing -> False
                 Just nonNegativeInteger -> redde nonNegativeInteger == (0 :: Integer)

case_NonNegativeInteger_LegalTextWithMinus :: Assertion
case_NonNegativeInteger_LegalTextWithMinus = assertBool "Borked" minusTest
  where
    text = "-0"
    mNonNegativeInteger :: Maybe NonNegativeInteger
    mNonNegativeInteger = fac text
    minusTest = case mNonNegativeInteger of
                  Nothing -> False
                  Just nonNegativeInteger -> redde nonNegativeInteger == (0 :: Integer)

case_NonNegativeInteger_Idempotent :: Assertion
case_NonNegativeInteger_Idempotent = assertBool "Not All" (all id $ map f testRange)
  where
    testRange = nonNegativeIntegers
    f :: Integer -> Bool
    f n = let text = tshow n
              mNonNegativeInteger = fac' text
              fac' :: Text -> Maybe NonNegativeInteger
              fac' = fac
          in case mNonNegativeInteger of
               Nothing -> False
               Just nonNegativeInteger ->
                 if redde nonNegativeInteger == n
                 then case fac' (canon nonNegativeInteger) >>= fac' . canon of
                        Nothing -> False
                        Just nonNegativeInteger' -> redde nonNegativeInteger' == n
                 else False

case_NonNegativeInteger_IllegalTextRange :: Assertion
case_NonNegativeInteger_IllegalTextRange = assertBool "Some" (not . any id $ map f testRange)
  where
    testRange = (map (\n -> n * (-1)) $ filter (/= 0) nonNegativeIntegers)
    f :: Integer -> Bool
    f n = let text = tshow n
              mNonNegativeInteger :: Maybe NonNegativeInteger
              mNonNegativeInteger = fac text
          in case mNonNegativeInteger of
               Nothing -> False
               Just nonNegativeInteger -> redde nonNegativeInteger == n

case_NonNegativeInteger_IllegalTextRangeWithWS :: Assertion
case_NonNegativeInteger_IllegalTextRangeWithWS = assertBool "Some" (not . any id $ map f testRange)
  where
    testRange = (map (\n -> n * (-1)) $ filter (/= 0) nonNegativeIntegers)
    f :: Integer -> Bool
    f n = let text = tshow n
              textWS = T.concat ["\n\r\t", text, "\t  \r\n  "]
              mNonNegativeInteger :: Maybe NonNegativeInteger
              mNonNegativeInteger = fac textWS
          in case mNonNegativeInteger of
               Nothing -> False
               Just nonNegativeInteger -> redde nonNegativeInteger == n

case_NonNegativeInteger_IllegalTextWithOther :: Assertion
case_NonNegativeInteger_IllegalTextWithOther = assertBool "Some" (not . any id $ map f testRange)
  where
    testRange = nonNegativeIntegers
    f :: Integer -> Bool
    f n = let text = tshow n
              textWS = T.concat ["\n\r\t", text, "✓\t  \r\n  "]
              mNonNegativeInteger :: Maybe NonNegativeInteger
              mNonNegativeInteger = fac textWS
          in case mNonNegativeInteger of
               Nothing -> False
               Just nonNegativeInteger -> redde nonNegativeInteger == n

positiveIntegers :: [] Integer
positiveIntegers = filter (> 0) integers

case_PositiveInteger_LegalText :: Assertion
case_PositiveInteger_LegalText = assertBool "Not All" (all id $ map f testRange)
  where
    testRange = positiveIntegers
    f :: Integer -> Bool
    f n = let text = tshow n
              mPositiveInteger :: Maybe PositiveInteger
              mPositiveInteger = fac text
          in case mPositiveInteger of
               Nothing -> False
               Just positiveInteger -> redde positiveInteger == n

case_PositiveInteger_LegalTextWithWS :: Assertion
case_PositiveInteger_LegalTextWithWS = assertBool "Not All" (all id $ map f testRange)
  where
    testRange = positiveIntegers
    f :: Integer -> Bool
    f n = let text = tshow n
              textWS = T.concat ["\n\r\t", text, "\t  \r\n  "]
              mPositiveInteger :: Maybe PositiveInteger
              mPositiveInteger = fac textWS
          in case mPositiveInteger of
               Nothing -> False
               Just positiveInteger -> redde positiveInteger == n

case_PositiveInteger_LegalTextWithPlus :: Assertion
case_PositiveInteger_LegalTextWithPlus = assertBool "Borked" (not plusTest)
  where
    text = "+0"
    mPositiveInteger :: Maybe PositiveInteger
    mPositiveInteger = fac text
    plusTest = case mPositiveInteger of
                 Nothing -> False
                 Just positiveInteger -> redde positiveInteger == (0 :: Integer)

case_PositiveInteger_LegalTextWithMinus :: Assertion
case_PositiveInteger_LegalTextWithMinus = assertBool "Borked" (not minusTest)
  where
    text = "-0"
    mPositiveInteger :: Maybe PositiveInteger
    mPositiveInteger = fac text
    minusTest = case mPositiveInteger of
                  Nothing -> False
                  Just positiveInteger -> redde positiveInteger == (0 :: Integer)

case_PositiveInteger_Idempotent :: Assertion
case_PositiveInteger_Idempotent = assertBool "Not All" (all id $ map f testRange)
  where
    testRange = positiveIntegers
    f :: Integer -> Bool
    f n = let text = tshow n
              mPositiveInteger = fac' text
              fac' :: Text -> Maybe PositiveInteger
              fac' = fac
          in case mPositiveInteger of
               Nothing -> False
               Just positiveInteger ->
                 if redde positiveInteger == n
                 then case fac' (canon positiveInteger) >>= fac' . canon of
                        Nothing -> False
                        Just positiveInteger' -> redde positiveInteger' == n
                 else False

case_PositiveInteger_IllegalTextRange :: Assertion
case_PositiveInteger_IllegalTextRange = assertBool "Some" (not . any id $ map f testRange)
  where
    testRange = (map (\n -> n * (-1)) $ filter (/= 0) positiveIntegers)
    f :: Integer -> Bool
    f n = let text = tshow n
              mPositiveInteger :: Maybe PositiveInteger
              mPositiveInteger = fac text
          in case mPositiveInteger of
               Nothing -> False
               Just positiveInteger -> redde positiveInteger == n

case_PositiveInteger_IllegalTextRangeWithWS :: Assertion
case_PositiveInteger_IllegalTextRangeWithWS = assertBool "Some" (not . any id $ map f testRange)
  where
    testRange = (map (\n -> n * (-1)) $ filter (/= 0) positiveIntegers)
    f :: Integer -> Bool
    f n = let text = tshow n
              textWS = T.concat ["\n\r\t", text, "\t  \r\n  "]
              mPositiveInteger :: Maybe PositiveInteger
              mPositiveInteger = fac textWS
          in case mPositiveInteger of
               Nothing -> False
               Just positiveInteger -> redde positiveInteger == n

case_PositiveInteger_IllegalTextWithOther :: Assertion
case_PositiveInteger_IllegalTextWithOther = assertBool "Some" (not . any id $ map f testRange)
  where
    testRange = positiveIntegers
    f :: Integer -> Bool
    f n = let text = tshow n
              textWS = T.concat ["\n\r\t", text, "✓\t  \r\n  "]
              mPositiveInteger :: Maybe PositiveInteger
              mPositiveInteger = fac textWS
          in case mPositiveInteger of
               Nothing -> False
               Just positiveInteger -> redde positiveInteger == n

