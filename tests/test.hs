{-# LANGUAGE OverloadedStrings #-}
import Test.Tasty
import Test.Tasty.HUnit
import System.Posix.Directory.Stream
import Control.Monad.Trans.Resource
import System.Posix.ByteString.FilePath
import Streaming (Stream, Of)
import Streaming as S
import Streaming.Prelude as S
import Data.List (sort)

test_dir :: RawFilePath
test_dir = "tests/test_dir"

main = defaultMain $ testGroup "Tests"
  [ testCase "streamDirectory" $ do
      entries <- runResourceT $ S.toList_ $ streamDirectory test_dir
      sort entries @?=
        [ "063bc44da76b0b97"
        , "25c56f3e069348af"
        , "442c148184ca5a5d"
        , "465977447b3a4d17"
        , "81bfd7a8dcaef727"
        ]
  , testCase "find" $ do
      entries <- runResourceT $ S.toList_ $ find test_dir
      sort entries @?=
        [ "tests/test_dir"
        , "tests/test_dir/063bc44da76b0b97"
        , "tests/test_dir/25c56f3e069348af"
        , "tests/test_dir/442c148184ca5a5d"
        , "tests/test_dir/465977447b3a4d17"
        , "tests/test_dir/81bfd7a8dcaef727"
        , "tests/test_dir/81bfd7a8dcaef727/5f3b29eec4177700"
        , "tests/test_dir/81bfd7a8dcaef727/5f3b29eec4177700/4018d585bd46f368"
        , "tests/test_dir/81bfd7a8dcaef727/5f3b29eec4177700/40fb9256c3a30ad9"
        , "tests/test_dir/81bfd7a8dcaef727/5f3b29eec4177700/555e756c8fe5fb04"
        , "tests/test_dir/81bfd7a8dcaef727/5f3b29eec4177700/7ada53549054e02e"
        , "tests/test_dir/81bfd7a8dcaef727/5f3b29eec4177700/f6b02c5f4450d007"
        , "tests/test_dir/81bfd7a8dcaef727/80f525b10d380c09"
        , "tests/test_dir/81bfd7a8dcaef727/a36fe770ce4315e9"
        ]
  ]
