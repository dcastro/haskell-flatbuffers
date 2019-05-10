{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}

module FlatBuffers.Integration.IntegrationSpec where

import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy       as BSL
import qualified Data.ByteString.Lazy.UTF8  as BSLU
import           Data.Int
import           Data.Word
import           FlatBuffers.FileIdentifier (unsafeFileIdentifier)
import           FlatBuffers.Internal.Write
import           Network.HTTP.Client
import           Network.HTTP.Types.Status  (statusCode)
import           Test.Hspec

newtype Pretty =
  Pretty Value
  deriving (Eq)

instance Show Pretty where
  show (Pretty v) = BSLU.toString (encodePretty v)

spec :: Spec
spec =
  describe "FlatBuffers" $
  parallel $
  forM_ cases $ \Case {..} ->
    it name $ do
      man <- newManager defaultManagerSettings
      req <- parseRequest ("http://localhost:8080/" ++ flatbufferName)
      let req' =
            req {method = "POST", requestBody = RequestBodyLBS rootByteString}
      rsp <- httpLbs req' man
      case statusCode $ responseStatus rsp of
        200 ->
          (Pretty <$> decode @Value (responseBody rsp)) `shouldBe`
          Just (Pretty expectedJson)
        _ -> expectationFailure ("Failed: " ++ BSLU.toString (responseBody rsp))

data Case = Case
  { name           :: String
  , flatbufferName :: String
  , rootByteString :: BSL.ByteString
  , expectedJson   :: Value
  }

cases :: [Case]
cases =
  [ Case
      "ManyTables"
      "ManyTables"
      (root $
       table
         [ inline int32 12
         , table [inline int32 23, text "hi"]
         , missing
         , table [inline int32 34, text "bye"]
         ])
      (object
         [ "n" .= Number 12
         , "x" .= object ["n" .= Number 23, "s" .= String "hi"]
         , "y" .= Null
         , "z" .= object ["n" .= Number 34, "s" .= String "bye"]
         ])
  , Case
      "Structs"
      "Structs"
      (root $
       table
         [ struct 4 [word32 maxBound, int32 maxBound]
         , missing
         , struct 8
             [ padded 7 $ bool True
             , int64 maxBound
             , padded 3 $ word8 maxBound
             , int32 maxBound
             ]
         , struct 8
             [ padded 7 $ bool True
             , int64 maxBound
             , padded 3 $ word8 maxBound
             , int32 maxBound
             , word32 maxBound
             , int32 maxBound
             ]
         ])
      (object
         [ "w" .= object ["x" .= maxBound @Int32, "y" .= maxBound @Word32]
         , "x" .= Null
         , "y" .=
           object
             [ "w" .= maxBound @Int32
             , "x" .= maxBound @Word8
             , "y" .= maxBound @Int64
             , "z" .= True
             ]
         , "z" .=
           object
             [ "x" .= object ["x" .= maxBound @Int32, "y" .= maxBound @Word32]
             , "y" .=
               object
                 [ "w" .= maxBound @Int32
                 , "x" .= maxBound @Word8
                 , "y" .= maxBound @Int64
                 , "z" .= True
                 ]
             ]
         ])
  ]

-- In the presence of OverloadedLists, [] can sometimes be ambiguous, so
-- we use this here to help disambiguate such scenarios.
array :: [Value] -> [Value]
array = id
