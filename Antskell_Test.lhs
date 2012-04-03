Copyright (c) 2012 Jess VanDerwalker <washu@sonic.net>. All rights reserved.

Unit tests for the Antskell module using HUnit.

> module Antskell_Test where

> import Antskell
> import Test.HUnit

--------------------------------------------------------------------------------

Data for use in tests

> wr1  = Worker (Ant 0 5) Harvester 0.0
> wr2  = Worker (Ant 1 1) Harvester 0.0
> wr3  = Worker (Ant 1 9) Nursery 0.0
> wr4  = Worker (Ant 1 9) Larva 0.0
> wr5  = Worker (Ant adultAge 10) Larva 0.0
> wr6  = Worker (Ant adultAge 10) Harvester 0.0
> wr7  = Worker (Ant (adultAge - 1) 10) Larva 0.0
> wr8  = Worker (Ant 10 10) Nursery 0.0
> wr9  = Worker (Ant 10 10) Nursery 0.0 
> wr10 = wr4
> wrls = [w1,w2,w3,w4]

> ls1In = [wr5]
> ls1Out = [wr6]
> ls2In = [Worker (Ant (adultAge - 1) 10) Larva 0.0]
> workerList1 = ls1In ++ ls1Out ++  ls2In ++
>               [ wr8, wr9]
> workerList2 = ls1In ++ ls1Out
> workerListOfLists = [ls1In, ls1Out]
> larvaTestList = [wr1, wr2, wr3, wr4, wr5, wr7, wr10]

--------------------------------------------------------------------------------

Unit tests for functions determining Worker roles.


> testLarvaToHarvester1 :: Test
> testLarvaToHarvester1 = TestCase $ assertEqual
>                         "Larva should become a Harvester" ls1Out
>                         ( larvaToHarvester ls1Out )

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

> getRoleNumbersTest1 :: Test
> getRoleNumbersTest1 = TestCase $ assertEqual
>                      "Incorrect numbers for roles returned"
>                      [ (Larva,2), (Nursery,2), (Harvester,1) ]
>                      ( getRoleNumbers workerList1 )

> getRoleNumbersTest2 :: Test
> getRoleNumbersTest2 = TestCase $ assertEqual
>                      "Incorrect numbers for roles returned"
>                      [ (Larva,1), (Harvester,1) ]
>                      ( getRoleNumbers workerList2 )

> getRoleOfListsTest :: Test
> getRoleOfListsTest = TestCase $ assertEqual
>                     "Incorrect Role for lists returned"
>                     [ (Larva,ls1In), (Harvester,ls1Out) ]
>                     ( getRoleOfLists workerListOfLists )

> numInRoleTest :: Test
> numInRoleTest = TestCase $ assertEqual
>                 "Incorrect number of Workers in Role"
>                 2
>                 ( numInRole Nursery (getRoleNumbers workerList1) )

> getRoleNumbersTests :: Test
> getRoleNumbersTests = TestList [ getRoleNumbersTest1
>                                , getRoleNumbersTest2
>                                , getRoleOfListsTest
>                                , numInRoleTest ]

--------------------------------------------------------------------------------

Unit tests for Larva production functions.

> nurseryStaffingTest1 :: Test
> nurseryStaffingTest1 = TestCase $ assertEqual
>                       "Ratio of Nursery workers to Larva incorrect"
>                       2
>                       ( nurseryStaffing workerList1 )

> nurseryStaffingTest2 :: Test
> nurseryStaffingTest2 = TestCase $ assertEqual
>                       "Ratio of Nursery workers to Larva incorrect"
>                       0.5
>                       ( nurseryStaffing  larvaTestList )

> larvaTests :: Test
> larvaTests = TestList [ nurseryStaffingTest1
>                       , nurseryStaffingTest2 ]

--------------------------------------------------------------------------------

Run all the given tests

> main :: IO Counts
> main = runTestTT $ TestList [ larvaToHarvesterTests
>                             , getRoleNumbersTests
>                             , larvaTests ]
