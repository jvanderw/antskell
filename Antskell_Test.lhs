Copyright (c) 2012 Jess VanDerwalker <washu@sonic.net>. All rights reserved.

Unit tests for the Antskell module using HUnit.

> module Antskell_Test where

> import Antskell
> import Test.HUnit

--------------------------------------------------------------------------------

Unit tests for functions determining Worker roles.

> ls1In = [Worker (Ant adultAge 10) Larva 0.0]
> ls1Out = [Worker (Ant adultAge 10) Harvester 0.0]

> testLarvaToHarvester1 :: Test
> testLarvaToHarvester1 = TestCase $ assertEqual
>                         "Larva should become a Harvester" ls1Out
>                         ( larvaToHarvester ls1Out )

> ls2In = [Worker (Ant (adultAge - 1) 10) Larva 0.0]

> testLarvaToHarvester2 :: Test
> testLarvaToHarvester2 = TestCase $ assertEqual
>                         "Larva should remain a Larva" ls2In
>                         ( larvaToHarvester ls2In )

> testLarvaToHarvester3 :: Test
> testLarvaToHarvester3 = TestCase $ assertEqual
>                         "Harvester should remain a Harvester" ls1Out
>                         ( larvaToHarvester ls1Out )

> larvaToHarvesterTests :: Test
> larvaToHarvesterTests = TestList [ testLarvaToHarvester1
>                                  , testLarvaToHarvester2
>                                  , testLarvaToHarvester3 ]

> workerList = ls1In ++ ls1Out ++  ls2In ++
>              [ Worker (Ant 10 10) Nursery 0.0
>              , Worker (Ant 10 10) Nursery 0.0 ]

> getRoleNumbersTest :: Test
> getRoleNumbersTest = TestCase $ assertEqual
>                      "Incorrect numbers for roles returned" [ 2, 2, 1]
>                      ( getRoleNumbers workerList )

> getRoleNumbersTests :: Test
> getRoleNumbersTests = TestList [ getRoleNumbersTest ]

--------------------------------------------------------------------------------

Run all the given tests

> main :: IO Counts
> main = runTestTT $ TestList [ larvaToHarvesterTests
>                             , getRoleNumbersTests ]
