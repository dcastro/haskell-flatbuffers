{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module FlatBuffers.Integration.HaskellToScalaSpec where

import           Data.Aeson                (Value (..), object, (.=))
import qualified Data.Aeson                as J
import qualified Data.ByteString.Lazy      as BSL
import qualified Data.ByteString.Lazy.UTF8 as BSLU
import           Data.Int
import           Data.Word
import           Examples.HandWritten
import           FlatBuffers.Write         (WriteTable, encode, none)
import           Network.HTTP.Client
import           Network.HTTP.Types.Status (statusCode)
import           Test.Hspec
import           TestUtils

spec :: Spec
spec =
  describe "Haskell encoders should be consistent with Scala decoders" $
    it "VectorOfUnions" $ do
      test
        "VectorOfUnions"
        (encode $ vectorOfUnions (Just [weapon (sword (Just "hi"))]) [weapon (sword (Just "hi2"))])
        (object
          [ "xs" .= [object ["x" .= String "hi"]]
          , "xsReq" .= [object ["x" .= String "hi2"]]
          ]
        )
      test
        "VectorOfUnions"
        (encode $ vectorOfUnions (Just [weapon (sword Nothing)]) [weapon (axe Nothing)])
        (object ["xs" .= [object ["x" .= Null]], "xsReq" .= [object ["y" .= Number 0]]])
      test
        "VectorOfUnions"
        (encode $ vectorOfUnions
          (Just
            [ weapon (sword (Just "hi"))
            , none
            , weapon (axe (Just maxBound))
            , weapon (sword (Just "oi"))
            ]
          )
          [ weapon (sword (Just "hi2"))
          , none
          , weapon (axe (Just minBound))
          , weapon (sword (Just "oi2"))
          ]
        )
        (object
          [ "xs" .=
            [ object ["x" .= String "hi"]
            , String "NONE"
            , object ["y" .= maxBound @Int32]
            , object ["x" .= String "oi"]
            ]
          , "xsReq" .=
            [ object ["x" .= String "hi2"]
            , String "NONE"
            , object ["y" .= minBound @Int32]
            , object ["x" .= String "oi2"]
            ]
          ])
      test
        "VectorOfUnions"
        (encode $ vectorOfUnions (Just []) [])
        (object ["xs" .= [] @Value, "xsReq" .= [] @Value])
      test
        "VectorOfUnions"
        (encode $ vectorOfUnions Nothing [])
        (object ["xs" .= [] @Value, "xsReq" .= [] @Value])


test :: String -> BSL.ByteString -> J.Value -> IO ()
test flatbufferName bs expectedJson = do
  man <- newManager defaultManagerSettings
  req <- parseRequest ("http://localhost:8080/" ++ flatbufferName)
  let req' = req {method = "POST", requestBody = RequestBodyLBS bs}
  rsp <- httpLbs req' man
  case statusCode $ responseStatus rsp of
    200 ->
      (PrettyJson <$> J.decode @J.Value (responseBody rsp)) `shouldBe`
      Just (PrettyJson expectedJson)
    _ -> expectationFailure ("Failed: " ++ BSLU.toString (responseBody rsp))
