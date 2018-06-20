{-# Language ExistentialQuantification, MultiParamTypeClasses, FlexibleInstances, GeneralizedNewtypeDeriving, NegativeLiterals #-}
{-| Time-stamp: <2018-06-20 13:45:19 CDT>

Module      : Parsers
Copyright   : (c) Robert Lee, 2017-2018
License     : ISC

Maintainer  : robert.lee@chicago.vc
Stability   : None
Portability : non-portable (GHC extensions)

Contumacy   : Best viewed with unbroken/unwrapped 154 column display.

Description : Provide parsing support for lexical and value correct types for use with XML Schema 1.1.

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

module Parsers
    ( aLPHA
    , absoluteIRI
    , anchorParser
    , asMany
    , cinClass
    , collapse
    , dIGIT
    , decOctet
    , extrasParser
    , genDelims
    , h16
    , hEXDIG
    , iPliteral
    , iPv4address
    , iPv6address
    , iPvFuture
    , iauthority
    , ifragment
    , ihierPart
    , ihost
    , ipath
    , ipathABEmpty
    , ipathAbsolute
    , ipathNoScheme
    , ipathRootless
    , ipchar
    , iprivate
    , iquery
    , iregName
    , irelativePart
    , irelativeRef
    , iriParser
    , iriReference
    , isegment
    , isegmentNz
    , iunreserved
    , iuserinfo
    , ls32
    , minMax
    , pORT
    , parse2
    , parseCollapse
    , parsedP
    , pctEncoded
    , preFillWith
    , replaceXmlWhite
    , reserved
    , scheme
    , subDelims
    , ucschar
    , unreserved
    , xmlWhite
    , xmlWhiteTx
    , zeroMax
    )
where

-- Local Imports

import Lading

-- Explicit Imports

import Data.Either (isRight)
import Data.Ix     (inRange)
import Numeric     (readHex)

-- Qualified Imports

import qualified Data.Char              as C
import qualified Data.List              as DL
import qualified Data.Text              as T

-- Undisciplined Imports

import ClassyPrelude hiding (IO)
import Data.Attoparsec.Text
-- import Text.XML hiding (Name)

-- End of Imports
-- -----------------------------------------------------------------------------------------------------------------------------------------------------

-- | asMany tries to complete as much of its parser list as it can.
--   asMany will fail if no parser (assuming the parser list is not empty) succeeds.
asMany :: [Parser a] -> Parser [a]
asMany [] = pure []
asMany (p:[]) = p >>= pure . (:[])
asMany (p:ps) = do a <- p
                   rest <- asMany ps <|> pure []
                   pure (a:rest)

-- | zeroMax does not fail, but is constrained by maxi.
zeroMax :: Int -> Parser a -> Parser [a]
zeroMax maxi parsi = asMany (replicate maxi parsi) <|> pure []

minMax :: Int -> Int -> Parser a -> Parser [a]
minMax mini maxi parsi | mini < maxi = do minis <- count mini parsi
                                          rest <- zeroMax (maxi - mini) parsi
                                          pure $ minis ++ rest
                       | mini == maxi = count mini parsi
                       | otherwise = fail "minimum is greater than maximum"

preFillWith :: Show showable => Char -> Int -> showable -> String
preFillWith c n s = let ss = show s
                        lng = length ss
                    in if lng < n
                       then replicate (n - lng) c ++ ss
                       else ss

cinClass :: String -> Parser Char
cinClass = satisfy . inClass

parsedP :: forall b. Parser b -> T.Text -> Bool
parsedP parser = isRight . parseOnly parser

parse2 :: (Parser a, Parser a) -> Parser [a] -- Helper function for those two something parsers.
parse2 (firstC, secondC) = do fc <- firstC
                              sc <- secondC
                              pure [fc,sc]

anchorParser :: forall b. Parser b -> Parser b
anchorParser parserToAnchor = do
  result <- parserToAnchor
  endOfInput
  pure result

extrasParser :: forall b. Parser b -> Parser [b]
extrasParser parser = many' (char ' ' >> parser)

parseCollapse :: forall b. Parser b -> Text -> Maybe b
parseCollapse parser candidate = eitherToMaybe . parseOnly (anchorParser parser) $ collapse candidate

xmlWhite :: [] Char
xmlWhite = " \t\r\n"

xmlWhiteTx :: Text
xmlWhiteTx = " \t\r\n"

replaceXmlWhite :: Text -> Text
replaceXmlWhite = T.map (\c -> T.any (== c) " \t\r\n" ? ' ' $ c)

-- collapse = T.unwords . T.words -- This is not correct due to the broader reach of whitespace (isSpace) in unicode than in the XS spec.

collapse :: Text -> Text
collapse = T.dropWhileEnd (' ' ==) . T.dropWhile (' ' ==) . collapseG                               -- 4.  Eliminate leading and trailing spaces.
  where collapseG = T.concat . DL.map (\t -> if T.isPrefixOf " " t
                                             then T.pack . DL.nubBy (\a _ -> ' ' == a) $ T.unpack t -- 3a. Reduce multiple spaces to a single space.
                                             else t                                                 -- 3b. Leave non-spaces unmolested.
                                      )
                  . T.groupBy (\a b -> a == ' ' && b == ' ')                                        -- 2.  Separate spaces from the rest.
                  . replaceXmlWhite                                                                 -- 1.  Replace xmlWhite Chars with ' '.


data IAddrTx = IAddrTxIPv4       (T.Text, T.Text, T.Text, T.Text)
             | IAddrTxIPv4Future (T.Text, T.Text)
             | IAddrTxIPv6       ([T.Text], Maybe (Either (T.Text,T.Text) IAddrTx))
             | IAddrTxRegName    T.Text
               deriving (Show)


-- AnyURI parsers -----------------------------------------------------------------------------------------------------------------------------------

{- foo://example.com:8042/over/there?name=ferret#nose
   \_/   \______________/\_________/ \_________/ \__/
    |           |            |            |        |
 scheme     authority       path        query   fragment
    |   _____________________|__
   / \ /                        \
   urn:example:animal:ferret:nose

  See: RFC 3986 Uniform Resource Identifier (URI): Generic Syntax
       RFC 3987 Internationalized Resource Identifiers (IRIs)
       RFC 2234 Augmented BNF for Syntax Specifications: ABNF
       W3C XML Schema Definition Language (XSD) 1.1 Part 2: Datatypes (3.3.17 anyURI)
-}

pORT :: Parser T.Text
pORT = many dIGIT >>= pure . T.pack

-- | DIGIT = %x30-39 ; 0-9
dIGIT :: Parser Char
dIGIT = satisfy $ inRange ('0','9')

-- | ALPHA = %x41-5A / %x61-7A ; A-Z / a-z
aLPHA :: Parser Char
aLPHA = choice [ satisfy $ inRange ('A','Z')
               , satisfy $ inRange ('a','z')
               ]

-- | IRI-reference = IRI / irelative-ref
iriReference :: Parser (Either (T.Text, (Maybe (Maybe T.Text, IAddrTx, Maybe T.Text), [T.Text]), Maybe T.Text, Maybe T.Text)
                               ((Maybe (Maybe T.Text, IAddrTx, Maybe T.Text), [T.Text]), Maybe T.Text, Maybe T.Text)
                       )
iriReference = choice [ iriParser    >>= pure . Left
                      , irelativeRef >>= pure . Right
                      ]

-- | IRI = scheme ":" ihier-part [ "?" iquery ] [ "#" ifragment ]
iriParser :: Parser (T.Text, (Maybe (Maybe T.Text, IAddrTx, Maybe T.Text), [T.Text]), Maybe T.Text, Maybe T.Text)
iriParser = do schemeL <- scheme
               void ":"
               ihP <- ihierPart
               mIquery   <- option Nothing (char '?' >> iquery    >>= pure . Just)
               mFragment <- option Nothing (char '#' >> ifragment >>= pure . Just)
               pure (schemeL, ihP, mIquery, mFragment)

-- | absolute-IRI = scheme ":" ihier-part [ "?" iquery ]
absoluteIRI :: Parser (T.Text, (Maybe (Maybe T.Text, IAddrTx, Maybe T.Text), [T.Text]), Maybe T.Text)
absoluteIRI = do schemeL <- scheme
                 void ":"
                 ihP <- ihierPart
                 mIquery <- option Nothing (char '?' >> iquery >>= pure . Just)
                 pure (schemeL, ihP, mIquery)

{- | ihier-part = "//" iauthority ipath-abempty
                / ipath-absolute
                / ipath-rootless
                / ipath-empty
-}
ihierPart :: Parser (Maybe (Maybe T.Text, IAddrTx, Maybe T.Text), [T.Text])
ihierPart = do choice [ auth, option (Nothing, []) ipaths ] -- option (Nothing, []) accounts for ipath-empty
  where auth = do void "//"
                  iauth <- iauthority
                  segs <- ipathABEmpty
                  pure (Just iauth, segs)
        ipaths = do segs <- choice [ipathAbsolute, ipathRootless]
                    pure (Nothing, segs)

-- | irelative-ref = irelative-part [ "?" iquery ] [ "#" ifragment ]
irelativeRef :: Parser ((Maybe (Maybe T.Text, IAddrTx, Maybe T.Text), [T.Text]), Maybe T.Text, Maybe T.Text)
irelativeRef = do irelPart   <- irelativePart
                  mIquery    <- option Nothing (char '?' >> iquery >>= pure . Just)
                  mIfragment <- option Nothing (char '#' >> ifragment >>= pure . Just)
                  pure (irelPart, mIquery, mIfragment)

{- | irelative-part = "//" iauthority ipath-abempty
                    / ipath-absolute
                    / ipath-noscheme
                    / ipath-empty
-}
irelativePart :: Parser (Maybe (Maybe T.Text, IAddrTx, Maybe T.Text), [T.Text])
irelativePart = do choice [ auth, option (Nothing, []) ipaths ] -- option (Nothing, []) accounts for ipath-empty
  where auth = do void "//"
                  iauth <- iauthority
                  segs  <- ipathABEmpty
                  pure (Just iauth, segs)
        ipaths = do segs <- choice [ipathAbsolute, ipathNoScheme]
                    pure (Nothing, segs)

-- | iauthority = [ iuserinfo "@" ] ihost [ ":" port ]
iauthority :: Parser (Maybe T.Text, IAddrTx, Maybe T.Text)
iauthority = do mUinfo <- option Nothing ( do uinfo <- iuserinfo
                                              void $ char '@'
                                              pure $ Just uinfo
                                         )
                host   <- ihost
                mPort  <- option Nothing ( do ":" >> pORT >>= pure . Just )
                pure (mUinfo, host, mPort)

-- | iuserinfo = *( iunreserved / pct-encoded / sub-delims / ":" )
iuserinfo :: Parser T.Text
iuserinfo = many (choice [iunreserved, pctEncoded, subDelims, char ':']) >>= pure . T.pack

-- | ihost = IP-literal / IPv4address / ireg-name
ihost :: Parser IAddrTx
ihost = choice [iPliteral, iPv4address, iregName]

-- | ireg-name = *( iunreserved / pct-encoded / sub-delims )
iregName :: Parser IAddrTx
iregName = many (choice [iunreserved, pctEncoded, subDelims]) >>= pure . IAddrTxRegName . T.pack

{- | ipath = ipath-abempty   ; begins with "/" or is empty
           / ipath-absolute  ; begins with "/" but not "//"
           / ipath-noscheme  ; begins with a non-colon segment
           / ipath-rootless  ; begins with a segment
           / ipath-empty     ; zero ipchars
-}
ipath :: Parser (Either [T.Text] [T.Text]) -- (Left absolute) (Right relative)
ipath = option (Right []) $ choice [ ipathAbsolute >>= pure . Left
                                   , ipathABEmpty  >>= pure . Left
                                   , ipathNoScheme >>= pure . Right
                                   , ipathRootless >>= pure . Right
                                   ] -- option (Right []) accounts for ipath-empty.

-- | ipath-abempty  = *( "/" isegment )
ipathABEmpty :: Parser [T.Text] -- Can be zero length. ABEmpty means absolute or empty.
ipathABEmpty = many (char '/' >> isegment)

-- | ipath-absolute = "/" [ isegment-nz *( "/" isegment ) ]
ipathAbsolute :: Parser [T.Text]
ipathAbsolute = char '/' >> option [] ipathRootless

-- | ipath-noscheme = isegment-nz-nc *( "/" isegment )
ipathNoScheme :: Parser [T.Text]
ipathNoScheme = do isegNzNc <- isegmentNzNc
                   segs <- many (char '/' >> isegment)
                   pure $ isegNzNc:segs

-- | ipath-rootless = isegment-nz *( "/" isegment )
ipathRootless :: Parser [T.Text]
ipathRootless = do isegNz <- isegmentNz
                   segs <- many (char '/' >> isegment)
                   pure $ isegNz:segs

-- | isegment = *ipchar
isegment :: Parser T.Text -- Can have zero length
isegment = many ipchar >>= pure . T.pack

-- | isegment-nz = 1*ipchar
isegmentNz :: Parser T.Text -- Non-zero length (Nz)
isegmentNz = many1 ipchar >>= pure . T.pack

-- | isegment-nz-nc = 1*( iunreserved / pct-encoded / sub-delims / "@" ) ; non-zero-length segment without any colon ":"
isegmentNzNc :: Parser T.Text -- Non-zero length (Nz) and no-colon (Nc)
isegmentNzNc = many1 (choice [iunreserved, pctEncoded, subDelims, char '@']) >>= pure . T.pack

-- | ipchar = iunreserved / pct-encoded / sub-delims / ":" / "@"
ipchar :: Parser Char
ipchar = choice [ iunreserved, pctEncoded, subDelims, char ':', char '@' ]

-- | iquery = *( ipchar / iprivate / "/" / "?" )
iquery :: Parser T.Text
iquery = many (choice [ ipchar, iprivate, char '/', char '?' ]) >>= pure . T.pack

-- | ifragment = *( ipchar / "/" / "?" )
ifragment :: Parser T.Text
ifragment = many (choice [ ipchar, char '/', char '?' ]) >>= pure . T.pack

-- | iunreserved = ALPHA / DIGIT / "-" / "." / "_" / "~" / ucschar
iunreserved :: Parser Char
iunreserved = choice [ aLPHA, dIGIT, char '-', char '.', char '_', char '~', ucschar]

-- | gen-delims = ":" / "/" / "?" / "#" / "[" / "]" / "@"
genDelims :: Parser Char
genDelims  = satisfy $ inClass ":/?#[]@"

-- | sub-delims = "!" / "$" / "&" / "'" / "(" / ")" / "*" / "+" / "," / ";" / "="
subDelims :: Parser Char
subDelims = satisfy $ inClass "!$&\'()*+,;="

-- | unreserved = ALPHA / DIGIT / "-" / "." / "_" / "~"
unreserved :: Parser Char
unreserved = choice [ aLPHA, dIGIT, char '-', char '.', char '_', char '~' ]

-- | reserved = gen-delims / sub-delims
reserved :: Parser Char
reserved = choice [ genDelims, subDelims ]

-- | HEXDIG = DIGIT / "A" / "B" / "C" / "D" / "E" / "F"
hEXDIG :: Parser Char
hEXDIG = choice [ dIGIT, satisfy $ inClass "ABCDEF" ]

-- | pct-encoded = "%" HEXDIG HEXDIG
pctEncoded :: Parser Char
pctEncoded = do void $ char '%'
                hexDigs <- count 2 hEXDIG
                let reads = readHex hexDigs
                case reads of
                  [] -> fail "hex read failed"
                  (c, _):_ -> pure $ C.chr c

{- | dec-octet = DIGIT                 ; 0-9
               / %x31-39 DIGIT         ; 10-99
               / "1" 2DIGIT            ; 100-199
               / "2" %x30-34 DIGIT     ; 200-249
               / "25" %x30-35          ; 250-255
-}
decOctet :: Parser T.Text
decOctet = choice [ twoHun50
                  , twoHun49
                  , oneHun
                  , ten99
                  , dIGIT >>= pure . T.singleton
                  ]
  where ten99 = do numeral <- satisfy $ inClass "123456789"
                   d <- dIGIT
                   pure $ T.pack [numeral, d]

        oneHun = do void "1"
                    d10 <- dIGIT
                    d1 <- dIGIT
                    pure $ T.pack ['1',d10,d1]
        twoHun49 = do void "2"
                      d10 <- satisfy $ inClass "01234"
                      d1 <- dIGIT
                      pure $ T.pack ['2',d10,d1]

        twoHun50 = do void "25"
                      d1 <- satisfy $ inClass "012345"
                      pure $ T.snoc "25" d1

-- | IPv4address = dec-octet "." dec-octet "." dec-octet "." dec-octet
iPv4address :: Parser IAddrTx
iPv4address = do
  a <-        decOctet
  b <- "." >> decOctet
  c <- "." >> decOctet
  d <- "." >> decOctet
  pure $ IAddrTxIPv4 (a,b,c,d)

-- | scheme = ALPHA *( ALPHA / DIGIT / "+" / "-" / "." )
scheme :: Parser T.Text
scheme = do
  one <- aLPHA
  rest <- many $ choice [ aLPHA, dIGIT, char '+', char '-', char '.' ]
  pure $ T.pack $ one:rest

-- | IP-literal = "[" ( IPv6address / IPvFuture  ) "]"
iPliteral :: Parser IAddrTx -- (Either ([T.Text], Maybe (Either (T.Text,T.Text) (T.Text, T.Text, T.Text, T.Text))) (T.Text,T.Text))
iPliteral = do
  void "["
  res <- choice [ iPv6address
                , iPvFuture
                ]
  void "]"
  pure res

-- | IPvFuture = "v" 1*HEXDIG "." 1*( unreserved / sub-delims / ":" )
iPvFuture :: Parser IAddrTx
iPvFuture = do
  void "v"
  hexDigs <- many1 hEXDIG
  void "."
  rest <- many1 $ choice [ unreserved, subDelims, char ':' ]
  pure $ IAddrTxIPv4Future (T.pack hexDigs, T.pack rest)

{- | IPv6address =                            6( h16 ":" ) ls32
                 /                       "::" 5( h16 ":" ) ls32
                 / [               h16 ] "::" 4( h16 ":" ) ls32
                 / [ *1( h16 ":" ) h16 ] "::" 3( h16 ":" ) ls32
                 / [ *2( h16 ":" ) h16 ] "::" 2( h16 ":" ) ls32
                 / [ *3( h16 ":" ) h16 ] "::"    h16 ":"   ls32
                 / [ *4( h16 ":" ) h16 ] "::"              ls32
                 / [ *5( h16 ":" ) h16 ] "::"              h16
                 / [ *6( h16 ":" ) h16 ] "::"
-}
iPv6address :: Parser IAddrTx -- ([T.Text], Maybe (Either (T.Text,T.Text) (T.Text, T.Text, T.Text, T.Text)))
iPv6address = do choice [v6a,v6b,v6c,v6d,v6e,v6f,v6g,v6h,v6i] >>= pure . IAddrTxIPv6
  where
    hPat = do h <- h16
              void ":"
              pure h

    v6a = do hs <- count 6 hPat                                            --                           6( h16 ":" ) ls32
             ls <- ls32
             pure (hs, Just ls)

    v6b = do void "::"                                                     --                      "::" 5( h16 ":" ) ls32
             hs <- count 5 hPat
             ls <- ls32
             pure (hs, Just ls)

    v6c = do h1 <- option "" h16                                           -- [               h16 ] "::" 4( h16 ":" ) ls32
             void "::"
             hs <- count 4 hPat
             ls <- ls32
             pure (filter (not . T.null) $ h1:hs, Just ls)

    v6d = do hi <- option [] $ do                                          -- [ *1( h16 ":" ) h16 ] "::" 3( h16 ":" ) ls32
                     hp <- option "" hPat
                     hz <- h16
                     pure [hp,hz]
             void "::"
             hs <- count 3 hPat
             ls <- ls32
             pure (filter (not . T.null) $ hi++hs, Just ls)

    v6e = do hi <- option [] $ do                                          -- [ *2( h16 ":" ) h16 ] "::" 2( h16 ":" ) ls32
                     hp <- minMax 0 2 hPat
                     hz <- h16
                     pure $ hp ++ [hz]
             void "::"
             hs <- count 2 hPat
             ls <- ls32
             pure (filter (not . T.null) $ hi++hs, Just ls)

    v6f = do hi <- option [] $ do                                          -- [ *3( h16 ":" ) h16 ] "::"    h16 ":"   ls32
                     hp <- minMax 0 3 hPat
                     hz <- h16
                     pure $ hp ++ [hz]
             void "::"
             hn <- hPat
             ls <- ls32
             pure (filter (not . T.null) $ hi++[hn], Just ls)

    v6g = do hi <- option [] $ do                                          -- [ *4( h16 ":" ) h16 ] "::"              ls32
                     hp <- minMax 0 4 hPat
                     hz <- h16
                     pure $ hp ++ [hz]
             void "::"
             ls <- ls32
             pure (filter (not . T.null) hi, Just ls)

    v6h = do hi <- option [] $ do                                          -- [ *5( h16 ":" ) h16 ] "::"              h16
                     hp <- minMax 0 5 hPat
                     hz <- h16
                     pure $ hp ++ [hz]
             void "::"
             h <- h16
             pure (filter (not . T.null) $ hi ++ [h], Nothing)

    v6i = do hi <- option [] $ do                                          -- [ *6( h16 ":" ) h16 ] "::"
                     hp <- minMax 0 6 hPat
                     hz <- h16
                     pure $ hp ++ [hz]
             void "::"
             pure (filter (not . T.null) $ hi, Nothing)

-- | h16 = 1*4HEXDIG
h16 :: Parser T.Text
h16 = minMax 1 4 hEXDIG >>= pure . T.pack

-- | ls32 = ( h16 ":" h16 ) / IPv4address
ls32 :: Parser (Either (T.Text,T.Text) IAddrTx)
ls32 = choice [ do a <- h16
                   b <- ":" >> h16
                   pure $ Left (a,b)
              , iPv4address >>= pure . Right
              ]

{- | ucschar = %xA0-D7FF / %xF900-FDCF / %xFDF0-FFEF
             / %x10000-1FFFD / %x20000-2FFFD / %x30000-3FFFD
             / %x40000-4FFFD / %x50000-5FFFD / %x60000-6FFFD
             / %x70000-7FFFD / %x80000-8FFFD / %x90000-9FFFD
             / %xA0000-AFFFD / %xB0000-BFFFD / %xC0000-CFFFD
             / %xD0000-DFFFD / %xE1000-EFFFD
-}
ucschar :: Parser Char
ucschar = choice [ satisfy $ inRange ( C.chr 0xA0    , C.chr 0xD7FF  )
                 , satisfy $ inRange ( C.chr 0xF900  , C.chr 0xFDCF  )
                 , satisfy $ inRange ( C.chr 0xFDF0  , C.chr 0xFFEF  )
                 , satisfy $ inRange ( C.chr 0x10000 , C.chr 0x1FFFD )
                 , satisfy $ inRange ( C.chr 0x20000 , C.chr 0x2FFFD )
                 , satisfy $ inRange ( C.chr 0x30000 , C.chr 0x3FFFD )
                 , satisfy $ inRange ( C.chr 0x40000 , C.chr 0x4FFFD )
                 , satisfy $ inRange ( C.chr 0x50000 , C.chr 0x5FFFD )
                 , satisfy $ inRange ( C.chr 0x60000 , C.chr 0x6FFFD )
                 , satisfy $ inRange ( C.chr 0x70000 , C.chr 0x7FFFD )
                 , satisfy $ inRange ( C.chr 0x80000 , C.chr 0x8FFFD )
                 , satisfy $ inRange ( C.chr 0x90000 , C.chr 0x9FFFD )
                 , satisfy $ inRange ( C.chr 0xA0000 , C.chr 0xAFFFD )
                 , satisfy $ inRange ( C.chr 0xB0000 , C.chr 0xBFFFD )
                 , satisfy $ inRange ( C.chr 0xC0000 , C.chr 0xCFFFD )
                 , satisfy $ inRange ( C.chr 0xD0000 , C.chr 0xDFFFD )
                 , satisfy $ inRange ( C.chr 0xE1000 , C.chr 0xEFFFD )
                 ]

-- | iprivate = %xE000-F8FF / %xF0000-FFFFD / %x100000-10FFFD
iprivate :: Parser Char
iprivate = choice [ satisfy $ inRange ( C.chr 0xE000   , C.chr 0xF8FF   )
                  , satisfy $ inRange ( C.chr 0xF0000  , C.chr 0xFFFFD  )
                  , satisfy $ inRange ( C.chr 0x100000 , C.chr 0x10FFFD )
                  ]

-- -- | irelative-ref  = irelative-part [ "?" iquery ] [ "#" ifragment ]
-- data IrelativeRef = IrelativeRef { irefIrelative_part :: T.Text
--                                  , irefIqueries       :: [] T.Text
--                                  , irefIfragments     :: [] T.Text
--                                  }

-- -- | IRI-reference  = IRI / irelative-ref
-- data IRIReference = IRIRefIRI IRI
--                   | IRIRefRelative T.Text

-- -- | IRI = scheme ":" ihier-part [ "?" iquery ] [ "#" ifragment ]
-- data IRI = IRI { ischeme    :: T.Text
--                , ihier_part :: IhierPart
--                , iqueries   :: [] T.Text
--                , ifragments :: [] T.Text
--                }

-- -- | ihier-part = "//" iauthority ipath-abempty
-- --              / ipath-absolute
-- --              / ipath-rootless
-- --              / ipath-empty
-- data IhierPart = IhierPartIAuth T.Text T.Text
--                | IhierPart_absolute T.Text -- | ipath-absolute = "/" [ isegment-nz *( "/" isegment ) ]
--                | IhierPart_rootless T.Text -- | ipath-rootless = isegment-nz *( "/" isegment )
--                | IhierPart_empty    T.Text -- | ipath-empty    = 0<ipchar>

-- -- | irelative-part = "//" iauthority ipath-abempty
-- --                  / ipath-absolute
-- --                  / ipath-noscheme
-- --                  / ipath-empty
-- data IrelativePart = IrelPartIauthority     T.Text T.Text -- | ipath-abempty  = *( "/" isegment )
--                    | IrelPartIpath_absolute T.Text -- | ipath-absolute = "/" [ isegment-nz *( "/" isegment ) ]
--                    | IrelPartIpath_noscheme T.Text -- | ipath-noscheme = isegment-nz-nc *( "/" isegment )
--                    | IrelPartIpath_empty    T.Text -- | ipath-empty    = 0<ipchar>


-- -- | iauthority = [ iuserinfo "@" ] ihost [ ":" port ]
-- data IAuthority = IAuthority { iauthMIuserinfo :: Maybe T.Text
--                              , iauthIhost :: T.Text
--                              , iauthMPort :: Maybe T.Text
--                              }

-- -- | iuserinfo = *( iunreserved / pct-encoded / sub-delims / ":" )
-- data IUserInfo = IUserInfoIunreserved T.Text
--                | IUserInfoPCT_encoded T.Text
--                | IUserInfoSUB_delims  T.Text

-- -- | ihost = IP-literal / IPv4address / ireg-name
-- data IHost = IHostIP_literal  T.Text
--            | IHostIPV4address T.Text
--            | IHostIReg_name   T.Text

-- -- | ipath = ipath-abempty   ; begins with "/" or is empty
-- --         / ipath-absolute  ; begins with "/" but not "//"
-- --         / ipath-noscheme  ; begins with a non-colon segment
-- --         / ipath-rootless  ; begins with a segment
-- --         / ipath-empty     ; zero characters
-- data IPath = IPath_abempty   T.Text -- | ipath-abempty  = *( "/" isegment )
--            | IPath_absolute  T.Text -- | ipath-absolute = "/" [ isegment-nz *( "/" isegment ) ]
--            | IPath_noscheme  T.Text -- | ipath-noscheme = isegment-nz-nc *( "/" isegment )
--            | IPath_rootless  T.Text -- | ipath-rootless = isegment-nz *( "/" isegment )
--            | IPath_empty     T.Text -- | ipath-empty    = 0<ipchar>


-- newtype IRI = IRI Text -- Placeholder for Internationalized Resource Identifiers (IRIs)

{-

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
W3C XML Schema Definition Language (XSD) 1.1 Part 2: Datatypes

3.3.17 anyURI

[Definition:] anyURI represents an Internationalized Resource
Identifier Reference (IRI).  An anyURI value can be absolute or
relative, and may have an optional fragment identifier (i.e., it may
be an IRI Reference).  This type should be used when the value
fulfills the role of an IRI, as defined in [RFC 3987] or its
successor(s) in the IETF Standards Track.

    Note: IRIs may be used to locate resources or simply to identify
    them. In the case where they are used to locate resources using a URI,
    applications should use the mapping from anyURI values to URIs given
    by the reference escaping procedure defined in [LEIRI] and in Section
    3.1 Mapping of IRIs to URIs of [RFC 3987] or its successor(s) in the
    IETF Standards Track.  This means that a wide range of
    internationalized resource identifiers can be specified when an anyURI
    is called for, and still be understood as URIs per [RFC 3986] and its
    successor(s).

3.3.17.1 Value Space

The value space of anyURI is the set of finite-length sequences of
zero or more characters (as defined in [XML]) that ·match· the Char
production from [XML].

3.3.17.2 Lexical Mapping

The ·lexical space· of anyURI is the set of finite-length sequences of
zero or more characters (as defined in [XML]) that ·match· the Char
production from [XML].

    Note: For an anyURI value to be usable in practice as an IRI, the
    result of applying to it the algorithm defined in Section 3.1 of [RFC
    3987] should be a string which is a legal URI according to [RFC
    3986]. (This is true at the time this document is published; if in the
    future [RFC 3987] and [RFC 3986] are replaced by other specifications
    in the IETF Standards Track, the relevant constraints will be those
    imposed by those successor specifications.)

    Each URI scheme imposes specialized syntax rules for URIs in that
    scheme, including restrictions on the syntax of allowed fragment
    identifiers. Because it is impractical for processors to check that a
    value is a context-appropriate URI reference, neither the syntactic
    constraints defined by the definitions of individual schemes nor the
    generic syntactic constraints defined by [RFC 3987] and [RFC 3986] and
    their successors are part of this datatype as defined
    here. Applications which depend on anyURI values being legal according
    to the rules of the relevant specifications should make arrangements
    to check values against the appropriate definitions of IRI, URI, and
    specific schemes.

    Note: Spaces are, in principle, allowed in the ·lexical space· of
    anyURI, however, their use is highly discouraged (unless they are
    encoded by '%20').

The ·lexical mapping· for anyURI is the identity mapping.

    Note: The definitions of URI in the current IETF specifications define
    certain URIs as equivalent to each other. Those equivalences are not
    part of this datatype as defined here: if two "equivalent" URIs or
    IRIs are different character sequences, they map to different values
    in this datatype.





-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
RFC 3987

2.  IRI Syntax

   This section defines the syntax of Internationalized Resource
   Identifiers (IRIs).

   As with URIs, an IRI is defined as a sequence of characters, not as a
   sequence of octets.  This definition accommodates the fact that IRIs
   may be written on paper or read over the radio as well as stored or
   transmitted digitally.  The same IRI may be represented as different
   sequences of octets in different protocols or documents if these
   protocols or documents use different character encodings (and/or
   transfer encodings).  Using the same character encoding as the
   containing protocol or document ensures that the characters in the
   IRI can be handled (e.g., searched, converted, displayed) in the same
   way as the rest of the protocol or document.

2.1.  Summary of IRI Syntax

   IRIs are defined similarly to URIs in [RFC3986], but the class of
   unreserved characters is extended by adding the characters of the UCS
   (Universal Character Set, [ISO10646]) beyond U+007F, subject to the
   limitations given in the syntax rules below and in section 6.1.

   Otherwise, the syntax and use of components and reserved characters
   is the same as that in [RFC3986].  All the operations defined in
   [RFC3986], such as the resolution of relative references, can be
   applied to IRIs by IRI-processing software in exactly the same way as
   they are for URIs by URI-processing software.

   Characters outside the US-ASCII repertoire are not reserved and
   therefore MUST NOT be used for syntactical purposes, such as to
   delimit components in newly defined schemes.  For example, U+00A2,
   CENT SIGN, is not allowed as a delimiter in IRIs, because it is in
   the 'iunreserved' category. This is similar to the fact that it is
   not possible to use '-' as a delimiter in URIs, because it is in the
   'unreserved' category.

2.2.  ABNF for IRI References and IRIs

   Although it might be possible to define IRI references and IRIs
   merely by their transformation to URI references and URIs, they can
   also be accepted and processed directly.  Therefore, an ABNF
   definition for IRI references (which are the most general concept and
   the start of the grammar) and IRIs is given here.  The syntax of this
   ABNF is described in [RFC2234].  Character numbers are taken from the
   UCS, without implying any actual binary encoding.  Terminals in the
   ABNF are characters, not bytes.

   The following grammar closely follows the URI grammar in [RFC3986],
   except that the range of unreserved characters is expanded to include
   UCS characters, with the restriction that private UCS characters can
   occur only in query parts.  The grammar is split into two parts:
   Rules that differ from [RFC3986] because of the above-mentioned
   expansion, and rules that are the same as those in [RFC3986].  For
   rules that are different than those in [RFC3986], the names of the
   non-terminals have been changed as follows.  If the non-terminal
   contains 'URI', this has been changed to 'IRI'.  Otherwise, an 'i'
   has been prefixed.

   The following rules are different from those in [RFC3986]:

   IRI            = scheme ":" ihier-part [ "?" iquery ]
                         [ "#" ifragment ]

   ihier-part     = "//" iauthority ipath-abempty
                  / ipath-absolute
                  / ipath-rootless
                  / ipath-empty

   IRI-reference  = IRI / irelative-ref

   absolute-IRI   = scheme ":" ihier-part [ "?" iquery ]

   irelative-ref  = irelative-part [ "?" iquery ] [ "#" ifragment ]

   irelative-part = "//" iauthority ipath-abempty
                       / ipath-absolute
                       / ipath-noscheme
                       / ipath-empty

   iauthority     = [ iuserinfo "@" ] ihost [ ":" port ]
   iuserinfo      = *( iunreserved / pct-encoded / sub-delims / ":" )
   ihost          = IP-literal / IPv4address / ireg-name

   ireg-name      = *( iunreserved / pct-encoded / sub-delims )

   ipath          = ipath-abempty   ; begins with "/" or is empty
                  / ipath-absolute  ; begins with "/" but not "//"
                  / ipath-noscheme  ; begins with a non-colon segment
                  / ipath-rootless  ; begins with a segment
                  / ipath-empty     ; zero characters

   ipath-abempty  = *( "/" isegment )
   ipath-absolute = "/" [ isegment-nz *( "/" isegment ) ]
   ipath-noscheme = isegment-nz-nc *( "/" isegment )
   ipath-rootless = isegment-nz *( "/" isegment )
   ipath-empty    = 0<ipchar>

   isegment       = *ipchar
   isegment-nz    = 1*ipchar
   isegment-nz-nc = 1*( iunreserved / pct-encoded / sub-delims
                        / "@" )
                  ; non-zero-length segment without any colon ":"

   ipchar         = iunreserved / pct-encoded / sub-delims / ":"
                  / "@"

   iquery         = *( ipchar / iprivate / "/" / "?" )

   ifragment      = *( ipchar / "/" / "?" )

   iunreserved    = ALPHA / DIGIT / "-" / "." / "_" / "~" / ucschar

   ucschar        = %xA0-D7FF / %xF900-FDCF / %xFDF0-FFEF
                  / %x10000-1FFFD / %x20000-2FFFD / %x30000-3FFFD
                  / %x40000-4FFFD / %x50000-5FFFD / %x60000-6FFFD
                  / %x70000-7FFFD / %x80000-8FFFD / %x90000-9FFFD
                  / %xA0000-AFFFD / %xB0000-BFFFD / %xC0000-CFFFD
                  / %xD0000-DFFFD / %xE1000-EFFFD

   iprivate       = %xE000-F8FF / %xF0000-FFFFD / %x100000-10FFFD

   Some productions are ambiguous.  The "first-match-wins" (a.k.a.
   "greedy") algorithm applies.  For details, see [RFC3986].



   The following rules are the same as those in [RFC3986]:

   scheme         = ALPHA *( ALPHA / DIGIT / "+" / "-" / "." )

   port           = *DIGIT

   IP-literal     = "[" ( IPv6address / IPvFuture  ) "]"

   IPvFuture      = "v" 1*HEXDIG "." 1*( unreserved / sub-delims / ":" )

   IPv6address    =                            6( h16 ":" ) ls32
                  /                       "::" 5( h16 ":" ) ls32
                  / [               h16 ] "::" 4( h16 ":" ) ls32
                  / [ *1( h16 ":" ) h16 ] "::" 3( h16 ":" ) ls32
                  / [ *2( h16 ":" ) h16 ] "::" 2( h16 ":" ) ls32
                  / [ *3( h16 ":" ) h16 ] "::"    h16 ":"   ls32
                  / [ *4( h16 ":" ) h16 ] "::"              ls32
                  / [ *5( h16 ":" ) h16 ] "::"              h16
                  / [ *6( h16 ":" ) h16 ] "::"

   h16            = 1*4HEXDIG
   ls32           = ( h16 ":" h16 ) / IPv4address

   IPv4address    = dec-octet "." dec-octet "." dec-octet "." dec-octet

   dec-octet      = DIGIT                 ; 0-9
                  / %x31-39 DIGIT         ; 10-99
                  / "1" 2DIGIT            ; 100-199
                  / "2" %x30-34 DIGIT     ; 200-249
                  / "25" %x30-35          ; 250-255

   pct-encoded    = "%" HEXDIG HEXDIG

   unreserved     = ALPHA / DIGIT / "-" / "." / "_" / "~"
   reserved       = gen-delims / sub-delims
   gen-delims     = ":" / "/" / "?" / "#" / "[" / "]" / "@"
   sub-delims     = "!" / "$" / "&" / "'" / "(" / ")"
                  / "*" / "+" / "," / ";" / "="

   This syntax does not support IPv6 scoped addressing zone identifiers.

3.  Relationship between IRIs and URIs

   IRIs are meant to replace URIs in identifying resources for
   protocols, formats, and software components that use a UCS-based
   character repertoire.  These protocols and components may never need
   to use URIs directly, especially when the resource identifier is used
   simply for identification purposes.  However, when the resource
   identifier is used for resource retrieval, it is in many cases
   necessary to determine the associated URI, because currently most
   retrieval mechanisms are only defined for URIs.  In this case, IRIs
   can serve as presentation elements for URI protocol elements.  An
   example would be an address bar in a Web user agent.  (Additional
   rationale is given in section 3.1.)

3.1.  Mapping of IRIs to URIs

   This section defines how to map an IRI to a URI.  Everything in this
   section also applies to IRI references and URI references, as well as
   to components thereof (for example, fragment identifiers).

   This mapping has two purposes:

   Syntaxical. Many URI schemes and components define additional
      syntactical restrictions not captured in section 2.2.
      Scheme-specific restrictions are applied to IRIs by converting
      IRIs to URIs and checking the URIs against the scheme-specific
      restrictions.

   Interpretational. URIs identify resources in various ways.  IRIs also
      identify resources.  When the IRI is used solely for
      identification purposes, it is not necessary to map the IRI to a
      URI (see section 5).  However, when an IRI is used for resource
      retrieval, the resource that the IRI locates is the same as the
      one located by the URI obtained after converting the IRI according
      to the procedure defined here.  This means that there is no need
      to define resolution separately on the IRI level.

   Applications MUST map IRIs to URIs by using the following two steps.

   Step 1.  Generate a UCS character sequence from the original IRI
            format.  This step has the following three variants,
            depending on the form of the input:

            a. If the IRI is written on paper, read aloud, or otherwise
               represented as a sequence of characters independent of
               any character encoding, represent the IRI as a sequence
               of characters from the UCS normalized according to
               Normalization Form C (NFC, [UTR15]).

            b. If the IRI is in some digital representation (e.g., an
               octet stream) in some known non-Unicode character
               encoding, convert the IRI to a sequence of characters
               from the UCS normalized according to NFC.

            c. If the IRI is in a Unicode-based character encoding (for
               example, UTF-8 or UTF-16), do not normalize (see section
               5.3.2.2 for details).  Apply step 2 directly to the
               encoded Unicode character sequence.

   Step 2.  For each character in 'ucschar' or 'iprivate', apply steps
            2.1 through 2.3 below.

       2.1.  Convert the character to a sequence of one or more octets
             using UTF-8 [RFC3629].

       2.2.  Convert each octet to %HH, where HH is the hexadecimal
             notation of the octet value.  Note that this is identical
             to the percent-encoding mechanism in section 2.1 of
             [RFC3986].  To reduce variability, the hexadecimal notation
             SHOULD use uppercase letters.

       2.3.  Replace the original character with the resulting character
             sequence (i.e., a sequence of %HH triplets).

   The above mapping from IRIs to URIs produces URIs fully conforming to
   [RFC3986].  The mapping is also an identity transformation for URIs
   and is idempotent;  applying the mapping a second time will not
   change anything.  Every URI is by definition an IRI.

   Systems accepting IRIs MAY convert the ireg-name component of an IRI
   as follows (before step 2 above) for schemes known to use domain
   names in ireg-name, if the scheme definition does not allow
   percent-encoding for ireg-name:

   Replace the ireg-name part of the IRI by the part converted using the
   ToASCII operation specified in section 4.1 of [RFC3490] on each
   dot-separated label, and by using U+002E (FULL STOP) as a label
   separator, with the flag UseSTD3ASCIIRules set to TRUE, and with the
   flag AllowUnassigned set to FALSE for creating IRIs and set to TRUE
   otherwise.
   The ToASCII operation may fail, but this would mean that the IRI
   cannot be resolved.  This conversion SHOULD be used when the goal is
   to maximize interoperability with legacy URI resolvers.  For example,
   the IRI

   "http://r&#xE9;sum&#xE9;.example.org"

   may be converted to

   "http://xn--rsum-bpad.example.org"

   instead of

   "http://r%C3%A9sum%C3%A9.example.org".

   An IRI with a scheme that is known to use domain names in ireg-name,
   but where the scheme definition does not allow percent-encoding for
   ireg-name, meets scheme-specific restrictions if either the
   straightforward conversion or the conversion using the ToASCII
   operation on ireg-name result in an URI that meets the scheme-
   specific restrictions.

   Such an IRI resolves to the URI obtained after converting the IRI and
   uses the ToASCII operation on ireg-name.  Implementations do not have
   to do this conversion as long as they produce the same result.

   Note: The difference between variants b and c in step 1 (using
      normalization with NFC, versus not using any normalization)
      accounts for the fact that in many non-Unicode character
      encodings, some text cannot be represented directly. For example,
      the word "Vietnam" is natively written "Vi&#x1EC7;t Nam"
      (containing a LATIN SMALL LETTER E WITH CIRCUMFLEX AND DOT BELOW)
      in NFC, but a direct transcoding from the windows-1258 character
      encoding leads to "Vi&#xEA;&#x323;t Nam" (containing a LATIN SMALL
      LETTER E WITH CIRCUMFLEX followed by a COMBINING DOT BELOW).
      Direct transcoding of other 8-bit encodings of Vietnamese may lead
      to other representations.

   Note: The uniform treatment of the whole IRI in step 2 is important
      to make processing independent of URI scheme.  See [Gettys] for an
      in-depth discussion.

   Note: In practice, whether the general mapping (steps 1 and 2) or the
      ToASCII operation of [RFC3490] is used for ireg-name will not be
      noticed if mapping from IRI to URI and resolution is tightly
      integrated (e.g., carried out in the same user agent).  But

      conversion using [RFC3490] may be able to better deal with
      backwards compatibility issues in case mapping and resolution are
      separated, as in the case of using an HTTP proxy.

   Note: Internationalized Domain Names may be contained in parts of an
      IRI other than the ireg-name part.  It is the responsibility of
      scheme-specific implementations (if the Internationalized Domain
      Name is part of the scheme syntax) or of server-side
      implementations (if the Internationalized Domain Name is part of
      'iquery') to apply the necessary conversions at the appropriate
      point.  Example: Trying to validate the Web page at
      http://r&#xE9;sum&#xE9;.example.org would lead to an IRI of
      http://validator.w3.org/check?uri=http%3A%2F%2Fr&#xE9;sum&#xE9;.
      example.org, which would convert to a URI of
      http://validator.w3.org/check?uri=http%3A%2F%2Fr%C3%A9sum%C3%A9.
      example.org.  The server side implementation would be responsible
      for making the necessary conversions to be able to retrieve the
      Web page.

   Systems accepting IRIs MAY also deal with the printable characters in
   US-ASCII that are not allowed in URIs, namely "<", ">", '"', space,
   "{", "}", "|", "\", "^", and "`", in step 2 above.  If these
   characters are found but are not converted, then the conversion
   SHOULD fail.  Please note that the number sign ("#"), the percent
   sign ("%"), and the square bracket characters ("[", "]") are not part
   of the above list and MUST NOT be converted.  Protocols and formats
   that have used earlier definitions of IRIs including these characters
   MAY require percent-encoding of these characters as a preprocessing
   step to extract the actual IRI from a given field.  This
   preprocessing MAY also be used by applications allowing the user to
   enter an IRI.

   Note: In this process (in step 2.3), characters allowed in URI
      references and existing percent-encoded sequences are not encoded
      further.  (This mapping is similar to, but different from, the
      encoding applied when arbitrary content is included in some part
      of a URI.)  For example, an IRI of
      "http://www.example.org/red%09ros&#xE9;#red" (in XML notation) is
      converted to
      "http://www.example.org/red%09ros%C3%A9#red", not to something
      like
      "http%3A%2F%2Fwww.example.org%2Fred%2509ros%C3%A9%23red".

   Note: Some older software transcoding to UTF-8 may produce illegal
      output for some input, in particular for characters outside the
      BMP (Basic Multilingual Plane).  As an example, for the IRI with
      non-BMP characters (in XML Notation):
      "http://example.com/&#x10300;&#x10301;&#x10302";
      which contains the first three letters of the Old Italic alphabet,
      the correct conversion to a URI is
      "http://example.com/%F0%90%8C%80%F0%90%8C%81%F0%90%8C%82"

3.2.  Converting URIs to IRIs

   In some situations, converting a URI into an equivalent IRI may be
   desirable.  This section gives a procedure for this conversion.  The
   conversion described in this section will always result in an IRI
   that maps back to the URI used as an input for the conversion (except
   for potential case differences in percent-encoding and for potential
   percent-encoded unreserved characters).  However, the IRI resulting
   from this conversion may not be exactly the same as the original IRI
   (if there ever was one).

   URI-to-IRI conversion removes percent-encodings, but not all
   percent-encodings can be eliminated.  There are several reasons for
   this:

   1.  Some percent-encodings are necessary to distinguish percent-
       encoded and unencoded uses of reserved characters.

   2.  Some percent-encodings cannot be interpreted as sequences of
       UTF-8 octets.

       (Note: The octet patterns of UTF-8 are highly regular.
       Therefore, there is a very high probability, but no guarantee,
       that percent-encodings that can be interpreted as sequences of
       UTF-8 octets actually originated from UTF-8.  For a detailed
       discussion, see [Duerst97].)

   3.  The conversion may result in a character that is not appropriate
       in an IRI.  See sections 2.2, 4.1, and 6.1 for further details.

   Conversion from a URI to an IRI is done by using the following steps
   (or any other algorithm that produces the same result):

   1.  Represent the URI as a sequence of octets in US-ASCII.

   2.  Convert all percent-encodings ("%" followed by two hexadecimal
       digits) to the corresponding octets, except those corresponding
       to "%", characters in "reserved", and characters in US-ASCII not
       allowed in URIs.

   3.  Re-percent-encode any octet produced in step 2 that is not part
       of a strictly legal UTF-8 octet sequence.

   4. Re-percent-encode all octets produced in step 3 that in UTF-8
      represent characters that are not appropriate according to
      sections 2.2, 4.1, and 6.1.

   5. Interpret the resulting octet sequence as a sequence of characters
      encoded in UTF-8.

   This procedure will convert as many percent-encoded characters as
   possible to characters in an IRI.  Because there are some choices
   when step 4 is applied (see section 6.1), results may vary.

   Conversions from URIs to IRIs MUST NOT use any character encoding
   other than UTF-8 in steps 3 and 4, even if it might be possible to
   guess from the context that another character encoding than UTF-8 was
   used in the URI.  For example, the URI
   "http://www.example.org/r%E9sum%E9.html" might with some guessing be
   interpreted to contain two e-acute characters encoded as iso-8859-1.
   It must not be converted to an IRI containing these e-acute
   characters.  Otherwise, in the future the IRI will be mapped to
   "http://www.example.org/r%C3%A9sum%C3%A9.html", which is a different
   URI from "http://www.example.org/r%E9sum%E9.html".

3.2.1.  Examples

   This section shows various examples of converting URIs to IRIs.  Each
   example shows the result after each of the steps 1 through 5 is
   applied.  XML Notation is used for the final result.  Octets are
   denoted by "<" followed by two hexadecimal digits followed by ">".

   The following example contains the sequence "%C3%BC", which is a
   strictly legal UTF-8 sequence, and which is converted into the actual
   character U+00FC, LATIN SMALL LETTER U WITH DIAERESIS (also known as
   u-umlaut).

   1.  http://www.example.org/D%C3%BCrst

   2.  http://www.example.org/D<c3><bc>rst

   3.  http://www.example.org/D<c3><bc>rst

   4.  http://www.example.org/D<c3><bc>rst

   5.  http://www.example.org/D&#xFC;rst

   The following example contains the sequence "%FC", which might
   represent U+00FC, LATIN SMALL LETTER U WITH DIAERESIS, in the
   iso-8859-1 character encoding.  (It might represent other characters
   in other character encodings.  For example, the octet <fc> in
   iso-8859-5 represents U+045C, CYRILLIC SMALL LETTER KJE.)  Because
   <fc> is not part of a strictly legal UTF-8 sequence, it is
   re-percent-encoded in step 3.

   1.  http://www.example.org/D%FCrst

   2.  http://www.example.org/D<fc>rst

   3.  http://www.example.org/D%FCrst

   4.  http://www.example.org/D%FCrst

   5.  http://www.example.org/D%FCrst

   The following example contains "%e2%80%ae", which is the percent-
   encoded UTF-8 character encoding of U+202E, RIGHT-TO-LEFT OVERRIDE.
   Section 4.1 forbids the direct use of this character in an IRI.
   Therefore, the corresponding octets are re-percent-encoded in step 4.
   This example shows that the case (upper- or lowercase) of letters
   used in percent-encodings may not be preserved.  The example also
   contains a punycode-encoded domain name label (xn--99zt52a), which is
   not converted.

   1.  http://xn--99zt52a.example.org/%e2%80%ae

   2.  http://xn--99zt52a.example.org/<e2><80><ae>

   3.  http://xn--99zt52a.example.org/<e2><80><ae>

   4.  http://xn--99zt52a.example.org/%E2%80%AE

   5.  http://xn--99zt52a.example.org/%E2%80%AE

   Implementations with scheme-specific knowledge MAY convert
   punycode-encoded domain name labels to the corresponding characters
   by using the ToUnicode procedure.  Thus, for the example above, the
   label "xn--99zt52a" may be converted to U+7D0D U+8C46 (Japanese
   Natto), leading to the overall IRI of
   "http://&#x7D0D;&#x8C46;.example.org/%E2%80%AE".

-}

