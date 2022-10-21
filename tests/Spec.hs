{-# LANGUAGE NumericUnderscores  #-}

module Main where

import Prelude
import Test.Tasty
import qualified Spec.MintRaffleTest as MintRaffleTest
import qualified Spec.LotteryTest    as LotteryTest

main = defaultMain tests

tests = testGroup "Raffle" [
        MintRaffleTest.tests ,
        LotteryTest.tests
    ]