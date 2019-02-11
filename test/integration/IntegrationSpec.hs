{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}

module IntegrationSpec where

import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy      as BSL
import qualified Data.ByteString.Lazy.UTF8 as BSLU
import           Data.Int
import           Data.Word
import           FlatBuffers
import           Network.HTTP.Client
import           Network.HTTP.Types.Status (statusCode)
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
      "Simple"
      "Simple"
      (root $ table [scalar int32 12, string "hi"])
      (object ["n" .= Number 12, "s" .= String "hi"])
  , Case
      "Primitives - maxBound"
      "Primitives"
      (root $
       table
         [ scalar word8 maxBound
         , scalar word16 maxBound
         , scalar word32 maxBound
         , scalar word64 maxBound
         , scalar int8 maxBound
         , scalar int16 maxBound
         , scalar int32 maxBound
         , scalar int64 maxBound
         , scalar float 2873242.8
         , scalar double 2873242.82782
         , scalar bool True
         ])
      (object
         [ "a" .= maxBound @Word8
         , "b" .= maxBound @Word16
         , "c" .= maxBound @Word32
         , "d" .= maxBound @Word64
         , "e" .= maxBound @Int8
         , "f" .= maxBound @Int16
         , "g" .= maxBound @Int32
         , "h" .= maxBound @Int64
         , "i" .= Number 2873242.8
         , "j" .= Number 2873242.82782
         , "k" .= True
         ])
  , Case
      "Primitives - minBound"
      "Primitives"
      (root $
       table
         [ scalar word8 minBound
         , scalar word16 minBound
         , scalar word32 minBound
         , scalar word64 minBound
         , scalar int8 minBound
         , scalar int16 minBound
         , scalar int32 minBound
         , scalar int64 minBound
         , missing
         , missing
         , scalar bool False
         ])
      (object
         [ "a" .= minBound @Word8
         , "b" .= minBound @Word16
         , "c" .= minBound @Word32
         , "d" .= minBound @Word64
         , "e" .= minBound @Int8
         , "f" .= minBound @Int16
         , "g" .= minBound @Int32
         , "h" .= minBound @Int64
         , "i" .= Number 0
         , "j" .= Number 0
         , "k" .= False
         ])
  , Case
      "Primitives - missing fields"
      "Primitives"
      (root $
       table
         [ missing
         , missing
         , missing
         , missing
         , missing
         , missing
         , missing
         , missing
         , missing
         , missing
         , missing
         ])
      (object
         [ "a" .= Number 0
         , "b" .= Number 0
         , "c" .= Number 0
         , "d" .= Number 0
         , "e" .= Number 0
         , "f" .= Number 0
         , "g" .= Number 0
         , "h" .= Number 0
         , "i" .= Number 0
         , "j" .= Number 0
         , "k" .= False
         ])
  , Case
      "ManyTables"
      "ManyTables"
      (root $
       table
         [ scalar int32 12
         , table [scalar int32 23, string "hi"]
         , missing
         , table [scalar int32 34, string "bye"]
         ])
      (object
         [ "n" .= Number 12
         , "x" .= object ["n" .= Number 23, "s" .= String "hi"]
         , "y" .= Null
         , "z" .= object ["n" .= Number 34, "s" .= String "bye"]
         ])
  , Case
      "Enums"
      "Enums"
      (root $ table [scalar word8 5])
      (object ["color" .= String "Gray"])
  , Case
      "Enums - missing"
      "Enums"
      (root $ table [missing])
      (object ["color" .= String "Blue"])
  , Case
      "TableWithUnion with UnionA"
      "TableWithUnion"
      (root $ table [scalar word8 1, table [string "hi"]])
      (object ["uni" .= object ["x" .= String "hi"]])
  , Case
      "TableWithUnion with UnionB"
      "TableWithUnion"
      (root $ table [scalar word8 2, table [scalar int32 99]])
      (object ["uni" .= object ["y" .= Number 99]])
  , Case
      "TableWithUnion with union type = None"
      "TableWithUnion"
      (root $ table [scalar word8 0, missing])
      (object ["uni" .= String "NONE"])
  , Case
      "TableWithUnion with missing union type"
      "TableWithUnion"
      (root $ table [missing, missing])
      (object ["uni" .= String "NONE"])
  , Case
      "Vectors"
      "Vectors"
      (root $
       table
         [ missing
         , vector [scalar int32 1, scalar int32 2]
         , vector
             [ text ""
             , text "hi 👬"
             , lazyText "hi 👬"
             , string "hi 👬"
             , byteString "hi"
             , lazyByteString "hi"
             ]
         , vector [scalar int64 3, scalar int64 4]
         ])
      (object
         [ "w" .= [] @Value
         , "x" .= [Number 1, Number 2]
         , "y" .=
           [ String ""
           , String "hi 👬"
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
      (root $
       table
         [ struct (int32 maxBound) [word32 maxBound]
         , missing
         , struct
             (int32 maxBound)
             [padded 3 $ word8 maxBound, int64 maxBound, padded 7 $ bool True]
         , struct
             (int32 maxBound)
             [ word32 maxBound
             , int32 maxBound
             , padded 3 $ word8 maxBound
             , int64 maxBound
             , padded 7 $ bool True
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
  , Case
      "VectorOfTables"
      "VectorOfTables"
      (root $
       table
         [ vector
             [ table [scalar int32 1, string "a"]
             , table [scalar int32 2, string "b"]
             , table [scalar int32 minBound, string "c"]
             ]
         ])
      (object
         [ "xs" .=
           [ object ["n" .= Number 1, "s" .= String "a"]
           , object ["n" .= Number 2, "s" .= String "b"]
           , object ["n" .= minBound @Int32, "s" .= String "c"]
           ]
         ])
  , Case
      "VectorOfStructs"
      "VectorOfStructs"
      (root $
       table
         [ vector
             [ struct (word8 1) [word8 2, word8 3]
             , struct (word8 4) [word8 5, word8 6]
             , struct (word8 7) [word8 8, word8 9]
             ]
         ])
      (object
         [ "xs" .=
           [ object ["x" .= Number 1, "y" .= Number 2, "z" .= Number 3]
           , object ["x" .= Number 4, "y" .= Number 5, "z" .= Number 6]
           , object ["x" .= Number 7, "y" .= Number 8, "z" .= Number 9]
           ]
         ])
  ]
