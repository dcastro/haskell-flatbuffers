{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module FlatBuffers.Integration.HaskellToScalaSpec where

import           Data.Aeson                (Value (..), object, (.=))
import qualified Data.Aeson                as J
import           Data.Aeson.Encode.Pretty  (encodePretty)
import qualified Data.ByteString.Lazy      as BSL
import qualified Data.ByteString.Lazy.UTF8 as BSLU
import           Data.Int
import           Data.Word
import           Examples.HandWritten
import           FlatBuffers.Write         (WriteTable, encode, encodeWithFileIdentifier, none)
import           Network.HTTP.Client
import           Network.HTTP.Types.Status (statusCode)
import           Test.Hspec

spec :: Spec
spec =
  describe "Haskell encoders should be consistent with Scala decoders" $
  parallel $ do
    it "Primitives" $
      test
        "Primitives"
        (encodeWithFileIdentifier $ primitives
          (Just maxBound) (Just maxBound) (Just maxBound) (Just maxBound)
          (Just maxBound) (Just maxBound) (Just maxBound) (Just maxBound)
          (Just 1234.56) (Just 2873242.82782) (Just True))
        (object
          [ "a" .= maxBound @Word8
          , "b" .= maxBound @Word16
          , "c" .= maxBound @Word32
          , "d" .= maxBound @Word64
          , "e" .= maxBound @Int8
          , "f" .= maxBound @Int16
          , "g" .= maxBound @Int32
          , "h" .= maxBound @Int64
          , "i" .= Number 1234.56
          , "j" .= Number 2873242.82782
          , "k" .= True
          ])

    it "Enums" $ do
      test
        "Enums"
        (encode $ enums
          (Just (fromColor ColorGray))
          (Just (structWithEnum 11 (fromColor ColorRed) 22))
          [fromColor ColorBlack, fromColor ColorBlue, fromColor ColorGreen]
          (Just [structWithEnum 33 (fromColor ColorRed) 44, structWithEnum 55 (fromColor ColorGreen) 66])
        )
        (object
          [ "x" .= String "Gray"
          , "y" .= object [ "x" .= Number 11, "y" .= String "Red", "z" .= Number 22 ]
          , "xs" .= [ String "Black", String "Blue", String "Green" ]
          , "ys" .=
            [ object [ "x" .= Number 33, "y" .= String "Red", "z" .= Number 44 ]
            , object [ "x" .= Number 55, "y" .= String "Green", "z" .= Number 66 ]
            ]
          ]
        )
      test
        "Enums"
        (encode $ enums Nothing Nothing [] Nothing)
        (object
          [ "x" .= String "Blue"
          , "y" .= Null
          , "xs" .= [] @Value
          , "ys" .= [] @Value
          ]
        )

    it "VectorOfUnions" $ do
      test
        "VectorOfUnions"
        (encode $ vectorOfUnions (Just [weapon (sword (Just "hi"))]))
        (object ["xs" .= [object ["x" .= String "hi"]]])
      test
        "VectorOfUnions"
        (encode $ vectorOfUnions (Just [weapon (sword Nothing)]))
        (object ["xs" .= [object ["x" .= Null]]])
      test
        "VectorOfUnions"
        (encode $ vectorOfUnions
          (Just
              [ weapon (sword (Just "hi"))
              , none
              , weapon (axe (Just maxBound))
              , weapon (sword (Just "oi"))
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
        (encode $ vectorOfUnions (Just []))
        (object ["xs" .= [] @Value])
      test
        "VectorOfUnions"
        (encode $ vectorOfUnions Nothing)
        (object ["xs" .= [] @Value])

    it "Align" $ do
      test
        "AlignT"
        (encode $ alignT
          (Just (align1 11))
          (Just (align2 22 33 44))
          (Just [align1 101, align1 102, align1 103])
          (Just [align2 104 105 106, align2 107 108 109, align2 110 111 112])
        )
        (object
          [ "x" .= object ["x" .= Number 11]
          , "y" .= object
            [ "x" .= object ["x" .= Number 22]
            , "y" .= Number 33
            , "z" .= Number 44
            ]
          , "xs" .=
            [ object ["x" .= Number 101]
            , object ["x" .= Number 102]
            , object ["x" .= Number 103]
            ]
          , "ys" .=
            [ object
              [ "x" .= object ["x" .= Number 104]
              , "y" .= Number 105
              , "z" .= Number 106
              ]
            , object
              [ "x" .= object ["x" .= Number 107]
              , "y" .= Number 108
              , "z" .= Number 109
              ]
            , object
              [ "x" .= object ["x" .= Number 110]
              , "y" .= Number 111
              , "z" .= Number 112
              ]
            ]
          ]
        )
      test
        "AlignT"
        (encode $ alignT Nothing Nothing Nothing Nothing)
        (object ["x" .= Null, "y" .= Null, "xs" .= [] @Value, "ys" .= [] @Value])


newtype Pretty =
  Pretty J.Value
  deriving (Eq)

instance Show Pretty where
  show (Pretty v) = BSLU.toString (encodePretty v)

test :: String -> BSL.ByteString -> J.Value -> IO ()
test flatbufferName bs expectedJson = do
  man <- newManager defaultManagerSettings
  req <- parseRequest ("http://localhost:8080/" ++ flatbufferName)
  let req' = req {method = "POST", requestBody = RequestBodyLBS bs}
  rsp <- httpLbs req' man
  case statusCode $ responseStatus rsp of
    200 ->
      (Pretty <$> J.decode @J.Value (responseBody rsp)) `shouldBe`
      Just (Pretty expectedJson)
    _ -> expectationFailure ("Failed: " ++ BSLU.toString (responseBody rsp))
