{-# LANGUAGE OverloadedStrings #-}

module Examples where

import           Data.ByteString.Builder (Builder, toLazyByteString)
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy    as BSL
import           FlatBuffers

go :: IO ()
go =
  putStrLn "Dumping" >>
  BSL.writeFile "bs.txt" (toLazyByteString nested)

st =
  root [
    scalar struct [
      padded 4 $ int32 9876,
      struct [
        int64 765,
        int64 98760
      ]
    ]
  ]

vectors =
  root [
    vector [text "bye 👬", string "bye 👬", byteString "bye 👬", lazyByteString "bye 👬", lazyText "bye 👬"],
    vector [scalar int32 12, scalar int32 34],
    vector [scalar int64 23, scalar int64 45]
  ]

variety =
  root [
    scalar word8 8,
    scalar word8 2,
    table [
      scalar int32 670
    ],
    scalar bool True
  ]

obj =
  root [
    scalar int32 1,
    text "hello",
    scalar int64 978,
    text "hello",
    missing
  ]

nested = 
  root [
    scalar int32 99,
    table [
      scalar int32 111,
      string "hello"
    ],
    table [
      scalar int32 111,
      string "hello"
    ]
  ]

lowLevelSimple2 =
  runRoot $ do
    hello <- string' "hello"
    root'
      [ int32 1
      , hello
      , int64 978
      , hello
      , missing'
      ]

lowLevelNested =
  runRoot $ do
    hello <- string' "hello"
    obj <- table'
      [ int32 111
      , hello
      ]
    root'
      [ int32 99
      , obj
      , obj
      ]
  