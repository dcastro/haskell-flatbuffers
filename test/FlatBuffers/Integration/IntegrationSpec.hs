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
      "Simple"
      "Simple"
      (root $ table [inline int32 12, text "hi"])
      (object ["n" .= Number 12, "s" .= String "hi"])
  , Case
      "Primitives - maxBound"
      "Primitives"
      (rootWithFileIdentifier (unsafeFileIdentifier "PRIM") $
       table
         [ inline word8 maxBound
         , inline word16 maxBound
         , inline word32 maxBound
         , inline word64 maxBound
         , inline int8 maxBound
         , inline int16 maxBound
         , inline int32 maxBound
         , inline int64 maxBound
         , inline float 2873242.8
         , inline double 2873242.82782
         , inline bool True
         , text "hello"
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
         , "l" .= String "hello"
         ])
  , Case
      "Primitives - minBound"
      "Primitives"
      (rootWithFileIdentifier (unsafeFileIdentifier "PRIM") $
       table
         [ inline word8 minBound
         , inline word16 minBound
         , inline word32 minBound
         , inline word64 minBound
         , inline int8 minBound
         , inline int16 minBound
         , inline int32 minBound
         , inline int64 minBound
         , missing
         , missing
         , inline bool False
         , text ""
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
         , "i" .= Number 1
         , "j" .= Number 1
         , "k" .= False
         , "l" .= String ""
         ])
  , Case
      "Primitives - missing fields"
      "Primitives"
      (rootWithFileIdentifier (unsafeFileIdentifier "PRIM") $
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
         , missing
         ])
      (object
         [ "a" .= Number 1
         , "b" .= Number 1
         , "c" .= Number 1
         , "d" .= Number 1
         , "e" .= Number 1
         , "f" .= Number 1
         , "g" .= Number 1
         , "h" .= Number 1
         , "i" .= Number 1
         , "j" .= Number 1
         , "k" .= False
         , "l" .= Null
         ])
  , Case
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
      "Enums"
      "Enums"
      (root $ table [inline word16 5])
      (object ["x" .= String "Gray", "y" .= Null, "xs" .= array [], "ys" .= array []])
  , Case
      "Enums - missing"
      "Enums"
      (root $ table [missing])
      (object ["x" .= String "Blue", "y" .= Null, "xs" .= array [], "ys" .= array []])
  , Case
      "TableWithUnion with UnionA"
      "TableWithUnion"
      (root $ table [inline word8 1, table [text "hi"]])
      (object ["uni" .= object ["x" .= String "hi"]])
  , Case
      "TableWithUnion with UnionB"
      "TableWithUnion"
      (root $ table [inline word8 2, table [inline int32 99]])
      (object ["uni" .= object ["y" .= Number 99]])
  , Case
      "TableWithUnion with union type = None"
      "TableWithUnion"
      (root $ table [inline word8 0, missing])
      (object ["uni" .= String "NONE"])
  , Case
      "TableWithUnion with missing union type"
      "TableWithUnion"
      (root $ table [missing, missing])
      (object ["uni" .= String "NONE"])
  , Case
      "Structs"
      "Structs"
      (root $
       table
         [ struct 4 [int32 maxBound, word32 maxBound]
         , missing
         , struct 8
             [ int32 maxBound
             , padded 3 $ word8 maxBound
             , int64 maxBound
             , padded 7 $ bool True
             ]
         , struct 8
             [ int32 maxBound
             , word32 maxBound
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
      "VectorOfStructs"
      "VectorOfStructs"
      (root $
       table
         [ vector @[]
             [ struct 1 [word8 1, word8 2, word8 3]
             , struct 1 [word8 4, word8 5, word8 6]
             , struct 1 [word8 7, word8 8, word8 9]
             ]
         ])
      (object
         [ "xs" .= array
           [ object ["x" .= Number 1, "y" .= Number 2, "z" .= Number 3]
           , object ["x" .= Number 4, "y" .= Number 5, "z" .= Number 6]
           , object ["x" .= Number 7, "y" .= Number 8, "z" .= Number 9]
           ]
         ])
  ]

-- In the presence of OverloadedLists, [] can sometimes be ambiguous, so
-- we use this here to help disambiguate such scenarios.
array :: [Value] -> [Value]
array = id
