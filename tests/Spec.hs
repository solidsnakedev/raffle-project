{-# LANGUAGE NumericUnderscores  #-}

module Main where

import Prelude
import Test.Tasty
import qualified Test.MintRaffleTest as MintRaffleTest
import qualified Test.LotteryTest    as LotteryTest

main = defaultMain tests

tests = testGroup "Raffle" [
        MintRaffleTest.tests ,
        LotteryTest.tests
    ]