{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module IntegrationSpec where

import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import           Data.ByteString.Builder   (Builder, toLazyByteString)
import qualified Data.ByteString.Builder   as B
import qualified Data.ByteString.Lazy      as BSL
import qualified Data.ByteString.Lazy.UTF8 as BSLU
import           Data.Int
import           Data.Word
import           FlatBuffers
import           Network.HTTP.Client
import           Network.HTTP.Types.Status (statusCode)
import           Test.Hspec

newtype Pretty = Pretty Value
  deriving Eq
  
instance Show Pretty where
  show (Pretty v) = BSLU.toString (encodePretty v)

spec :: Spec
spec =
  describe "FlatBuffers" $
    parallel $
      forM_ cases $ \Case{..} ->
        it name $ do
          man <- newManager defaultManagerSettings
          req <- parseRequest ("http://localhost:8080/" ++ flatbufferName)
          let req' = req { method = "POST", requestBody = RequestBodyLBS (toLazyByteString builder) }
          rsp <- httpLbs req' man
          case statusCode $ responseStatus rsp of
            200 -> (Pretty <$> decode @Value (responseBody rsp)) `shouldBe` Just (Pretty expectedJson)
            _   -> expectationFailure ("Failed: " ++ BSLU.toString (responseBody rsp))

data Case = Case
  { name           :: String
  , flatbufferName :: String
  , builder        :: Builder
  , expectedJson   :: Value
  }
cases :: [Case]
cases =
  [ Case
      "Simple"
      "Simple"
      (root [scalar int32 12, string "hi"])
      (object ["n" .= Number 12, "s" .= String "hi"])
  , Case
      "FiveFields"
      "FiveFields"
      (root
         [ scalar int32 12
         , string "hi"
         , scalar int64 23
         , string "bye"
         , scalar double 12.23
         ])
      (object
         [ "n1" .= Number 12
         , "s1" .= String "hi"
         , "n2" .= Number 23
         , "s2" .= String "bye"
         , "n3" .= Number 12.23
         ])
  , Case
      "ManyTables"
      "ManyTables"
      (root
         [ scalar int32 12
         , table [scalar int32 23, string "hi"]
         , table [scalar int32 34, string "bye"]
         ])
      (object
         [ "n" .= Number 12
         , "x" .= object ["n" .= Number 23, "s" .= String "hi"]
         , "y" .= object ["n" .= Number 34, "s" .= String "bye"]
         ])
  , Case
      "UnionByteBool"
      "UnionByteBool"
      (root
         [ scalar word8 5
         , scalar word8 1
         , table [string "hi"]
         , scalar word8 0
         , missing
         , scalar word8 2
         , table [scalar int32 99]
         , scalar bool True
         ])
      (object
         [ "color" .= String "Gray"
         , "uni1" .= object ["x" .= String "hi"]
         , "uni2" .= String "NONE"
         , "uni3" .= object ["y" .= Number 99]
         , "boo" .= True
         ])
  , Case
      "Vectors"
      "Vectors"
      (root
         [ vector [scalar int32 1, scalar int32 2]
         , vector
             [ text "hi 👬"
             , lazyText "hi 👬"
             , string "hi 👬"
             , byteString "hi"
             , lazyByteString "hi"
             ]
         , vector [scalar int64 3, scalar int64 4]
         ])
      (object
         [ "x" .= [Number 1, Number 2]
         , "y" .=
           [ String "hi 👬"
           , String "hi 👬"
           , String "hi 👬"
           , String "hi"
           , String "hi"
           ]
         , "z" .= [Number 3, Number 4]
         ])
  , Case
      "Structs"
      "Structs"
      (root
         [ scalar struct [int32 maxBound, word32 maxBound]
         , scalar
             struct
             [ int32 maxBound
             , padded 3 $ word8 maxBound
             , int64 maxBound
             , padded 7 $ bool True
             ]
         , scalar
             struct
             [ int32 maxBound
             , word32 maxBound
             , int32 maxBound
             , padded 3 $ word8 maxBound
             , int64 maxBound
             , padded 7 $ bool True
             ]
         ])
      (object
         [ "x" .=
           object ["x" .= (maxBound :: Int32), "y" .= (maxBound :: Word32)]
         , "y" .=
           object
             [ "w" .= (maxBound :: Int32)
             , "x" .= (maxBound :: Word8)
             , "y" .= (maxBound :: Int64)
             , "z" .= True
             ]
         , "z" .=
           object
             [ "x" .=
               object ["x" .= (maxBound :: Int32), "y" .= (maxBound :: Word32)]
             , "y" .=
               object
                 [ "w" .= (maxBound :: Int32)
                 , "x" .= (maxBound :: Word8)
                 , "y" .= (maxBound :: Int64)
                 , "z" .= True
                 ]
             ]
         ])
  ]
