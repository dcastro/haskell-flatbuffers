{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module FlatBuffers.Integration.HaskellToScalaSpec where

import           Data.Aeson                (Value (..), object, (.=))
import qualified Data.Aeson                as J
import           Data.Aeson.Encode.Pretty  (encodePretty)
import qualified Data.ByteString.Lazy      as BSL
import qualified Data.ByteString.Lazy.UTF8 as BSLU
import           Data.Int
import           Examples.HandWritten
import           FlatBuffers.Write         (WriteTable, encode, none)
import           Network.HTTP.Client
import           Network.HTTP.Types.Status (statusCode)
import           Test.Hspec

spec :: Spec
spec =
  describe "Haskell encoders should be consistent with Scala decoders" $
  parallel $
  it "VectorOfUnions" $ do
    test
      "VectorOfUnions"
      (vectorOfUnions (Just [union (unionA (Just "hi"))]))
      (object ["xs" .= [object ["x" .= String "hi"]]])
    test
      "VectorOfUnions"
      (vectorOfUnions (Just [union (unionA Nothing)]))
      (object ["xs" .= [object ["x" .= Null]]])
    test
      "VectorOfUnions"
      (vectorOfUnions
         (Just
            [ union (unionA (Just "hi"))
            , none
            , union (unionB (Just maxBound))
            , union (unionA (Just "oi"))
            ]))
      (object
         [ "xs" .=
           [ object ["x" .= String "hi"]
           , String "NONE"
           , object ["y" .= maxBound @Int32]
           , object ["x" .= String "oi"]
           ]
         ])
    test
      "VectorOfUnions"
      (vectorOfUnions (Just []))
      (object ["xs" .= [] @Value])
    test
      "VectorOfUnions"
      (vectorOfUnions Nothing)
      (object ["xs" .= [] @Value])

newtype Pretty =
  Pretty J.Value
  deriving (Eq)

instance Show Pretty where
  show (Pretty v) = BSLU.toString (encodePretty v)

test :: String -> WriteTable a -> J.Value -> IO ()
test flatbufferName table expectedJson = do
  man <- newManager defaultManagerSettings
  req <- parseRequest ("http://localhost:8080/" ++ flatbufferName)
  let req' = req {method = "POST", requestBody = RequestBodyLBS (encode table)}
  rsp <- httpLbs req' man
  case statusCode $ responseStatus rsp of
    200 ->
      (Pretty <$> J.decode @J.Value (responseBody rsp)) `shouldBe`
      Just (Pretty expectedJson)
    _ -> expectationFailure ("Failed: " ++ BSLU.toString (responseBody rsp))
