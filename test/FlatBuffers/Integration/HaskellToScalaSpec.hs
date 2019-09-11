{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module FlatBuffers.Integration.HaskellToScalaSpec where

import           Data.Aeson                ( (.=), Value(..), object )
import qualified Data.Aeson                as J
import qualified Data.ByteString.Lazy      as BSL
import qualified Data.ByteString.Lazy.UTF8 as BSLU
import           Data.Int

import           Examples

import           FlatBuffers
import qualified FlatBuffers.Vector        as Vec

import           Network.HTTP.Client
import           Network.HTTP.Types.Status ( statusCode )

import           TestImports


spec :: Spec
spec =
  describe "Haskell encoders should be consistent with Scala decoders" $
    it "VectorOfUnions" $ do
      testCase
        "VectorOfUnions"
        (encode $ vectorOfUnions
          (Just (Vec.singleton (weaponSword (sword (Just "hi")))))
          (Vec.singleton (weaponSword (sword (Just "hi2"))))
          )
        (object
          [ "xs" .= [object ["x" .= String "hi"]]
          , "xsReq" .= [object ["x" .= String "hi2"]]
          ]
        )
      testCase
        "VectorOfUnions"
        (encode $ vectorOfUnions
          (Just (Vec.singleton (weaponSword (sword Nothing))))
          (Vec.singleton (weaponAxe (axe Nothing)))
          )
        (object ["xs" .= [object ["x" .= Null]], "xsReq" .= [object ["y" .= Number 0]]])
      testCase
        "VectorOfUnions"
        (encode $ vectorOfUnions
          (Just $ Vec.fromList'
            [ weaponSword (sword (Just "hi"))
            , none
            , weaponAxe (axe (Just maxBound))
            , weaponSword (sword (Just "oi"))
            ]
          )
          (Vec.fromList'
            [ weaponSword (sword (Just "hi2"))
            , none
            , weaponAxe (axe (Just minBound))
            , weaponSword (sword (Just "oi2"))
            ]
          )
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
      testCase
        "VectorOfUnions"
        (encode $ vectorOfUnions (Just Vec.empty) Vec.empty)
        (object ["xs" .= [] @Value, "xsReq" .= [] @Value])
      testCase
        "VectorOfUnions"
        (encode $ vectorOfUnions Nothing Vec.empty)
        (object ["xs" .= [] @Value, "xsReq" .= [] @Value])


testCase :: HasCallStack => String -> BSL.ByteString -> J.Value -> IO ()
testCase flatbufferName bs expectedJson = do
  man <- newManager defaultManagerSettings
  req <- parseRequest ("http://localhost:8080/" ++ flatbufferName)
  let req' = req {method = "POST", requestBody = RequestBodyLBS bs}
  rsp <- httpLbs req' man
  case statusCode $ responseStatus rsp of
    200 ->
      (PrettyJson <$> J.decode @J.Value (responseBody rsp)) `shouldBe`
      Just (PrettyJson expectedJson)
    _ -> expectationFailure ("Failed: " ++ BSLU.toString (responseBody rsp))
