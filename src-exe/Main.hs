
-- Local Imports

import Builtin

-- Explicit Imports

-- Qualified Imports
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Char              as C

-- Undisciplined Imports

import ClassyPrelude

main :: IO ()
main = pMain

pMain :: IO ()
pMain = do
  putStrLn "What are ya? "
  let qcandidate = " \t  foo‿⁀:x⁀barωω\n\n\t\t\r \r"
      mQname = fac qcandidate :: Maybe QName
  TIO.putStrLn $ T.concat ["<", qcandidate, ">"]
  case mQname of
    Just qname@(PrefixedName prefix localpart) -> do TIO.putStrLn $ T.intercalate " " [scribe prefix, scribe localpart]
                                                     TIO.putStrLn $ scribe qname
                                                     print (fac $ scribe qname :: Maybe QName)
    Just qname@(UnprefixedName localpart) -> do TIO.putStrLn $ scribe localpart
                                                TIO.putStrLn $ scribe qname
                                                print (fac $ scribe qname :: Maybe QName)
    Nothing -> print mQname
  print $ (fac "false" :: Maybe Boolean)
       
  -- print $ (fac (T.singleton $ toEnum 0xFFCE) :: Maybe Stringxs)
  case fac $ T.singleton $ C.chr 0xF800 :: Maybe Stringxs of
    Nothing -> TIO.putStrLn "Nothing"
    Just stringXML -> do TIO.putStrLn $ T.append "Just " $ scribe stringXML

  let languageCandidate = "auoe-sotuh-tttttttt"
  print $ (fac languageCandidate :: Maybe Language)

  let nonNegativeIntegerCandidate = "+000"
  print $ (fac nonNegativeIntegerCandidate :: Maybe NonNegativeInteger)

  let integerCandidate = "-0"
  print $ (fac integerCandidate :: Maybe Integer)

  let unsignedIntCandidate = "+0"
  print $ (redde :: UnsignedInt -> Word32) <$> (fac unsignedIntCandidate :: Maybe UnsignedInt)

  let negativeIntegerCandidate = "+000"
  print $ (fac negativeIntegerCandidate :: Maybe NegativeInteger)

  let nonPositiveIntegerCandidate = "+000"
  print $ (fac nonPositiveIntegerCandidate :: Maybe NonPositiveInteger)

  let positiveIntegerCandidate = "1"
  print $ (fac positiveIntegerCandidate :: Maybe PositiveInteger)

  let intxsCandidate = "2147483647"
  print $ (fac intxsCandidate :: Maybe Intxs)
