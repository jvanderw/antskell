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
> wr8  = Worker (Ant adultAge 10) Nursery 0.0
> wr9  = Worker (Ant 10 10) Nursery 0.0 
> wr10 = wr4
> wrls = [w1,w2,w3,w4]
> wH = wr6
> wN = wr8
> wL = wr7

> newLarva = Worker (Ant 0 10) Larva 0.0

> ls1In = [wr5]
> ls1Out = [wr6]
> ls2In = [Worker (Ant (adultAge - 1) 10) Larva 0.0]
> workerList1 = ls1In ++ ls1Out ++  ls2In ++
>               [ wr8, wr9]
> workerList2 = ls1In ++ ls1Out
> workerListOfLists = [ls1In, ls1Out]
> larvaTestList1 = [wr1, wr2, wr3, wr4, wr5, wr7, wr10]
> larvaTestList2 = [wr3, wr8, wr4, wr7, wr4, wr7]

> testNest1 = Nest larvaTestList1 (Queen (Ant 1 10) 10) 20
> testNest2 = Nest larvaTestList2 (Queen (Ant 1 10) 10) 20

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

> numEggsTest1 :: Test
> numEggsTest1 = TestCase $ assertEqual
>                "Incorrect number of eggs" 0
>                ( numEggs larvaTestList1 )

> numEggsTest2 :: Test
> numEggsTest2 = TestCase $ assertEqual
>                "Incorrect number of eggs" 1
>                ( numEggs larvaTestList2 )

> createLarvaTest1 :: Test
> createLarvaTest1 = TestCase $ assertEqual
>                    "Did not add new Larva correctly"
>                    [newLarva, newLarva]
>                    ( createLarva [] 2 )

> createLarvaTest2 :: Test
> createLarvaTest2 = TestCase $ assertEqual
>                    "Did not add new Larva correctly"
>                    (larvaTestList1 ++ [newLarva, newLarva])
>                    ( createLarva larvaTestList1 2 )

> layEggsTest1 :: Test
> layEggsTest1 = TestCase $ assertEqual
>                "Incorrect number of Larva created in layEggs"
>                ( testNest1 )
>                ( layEggs testNest1 )


> layEggsTest2 :: Test
> layEggsTest2 = TestCase $ assertEqual
>                "Incorrect number of Larva created in layEggs"
>                ( testNest2 { workers = larvaTestList2 ++ [newLarva] } )
>                ( layEggs testNest2 )

> larvaTests :: Test
> larvaTests = TestList [ numEggsTest1
>                       , numEggsTest2
>                       , createLarvaTest1
>                       , createLarvaTest2
>                       , layEggsTest1 ]

--------------------------------------------------------------------------------

Unit tests for determining the number of Harvesters

> idealNumHarvsTest1 :: Test
> idealNumHarvsTest1 = TestCase $ assertEqual
>                      "Incorrect ideal number of Harvesters"
>                      ( 1 )
>                      ( idealNumHarvs
>                        (larvaTestList1 ++ larvaTestList2) 10 )

> idealNumHarvsTest2 :: Test
> idealNumHarvsTest2 = TestCase $ assertEqual
>                      "Incorrect ideal number of Harvesters"
>                      ( 5 )
>                      ( idealNumHarvs
>                        (larvaTestList1 ++ larvaTestList2) 0 )

> setHarvsTest1 :: Test
> setHarvsTest1 = TestCase $ assertEqual
>                       "Wrong number of workers set to Harvesters"
>                       ( [wL, wL, wL, wH, wN, wH] )
>                       ( setHarvs [wL, wL, wL, wN, wN, wH] 0 )

> harvesterRoleTests :: Test
> harvesterRoleTests = TestList [ idealNumHarvsTest1
>                               , idealNumHarvsTest2
>                               , setHarvsTest1 ]

--------------------------------------------------------------------------------

Unit tests for determining and setting number of Nursery workers.

> idealNumNurseryTest1 :: Test
> idealNumNurseryTest1 = TestCase $ assertEqual
>                        "Incorrect ideal number of Nursery workers"
>                        ( 2 )
>                        ( idealNumNursery [wL, wL, wL, wH, wN, wH] )

> idealNumNurseryTest2 :: Test
> idealNumNurseryTest2 = TestCase $ assertEqual
>                        "Incorrect ideal number of Nursery workers"
>                        ( 0 )
>                        ( idealNumNursery [wH, wH, wH, wH, wN, wN] )

> setNurseryTest1 :: Test
> setNurseryTest1 = TestCase $ assertEqual
>                   "setNurseryTest1: Incorrect number of Nursery workers created"
>                   ( [wL, wL, wL, wN, wN, wH] )
>                   ( setNursery [wL, wL, wL, wH, wN, wH] )


> setNurseryTest2 :: Test
> setNurseryTest2 = TestCase $ assertEqual
>                   "setNurseryTest2: Incorrect number of Nursery workers created"
>                   ( [wH, wH, wH, wN, wN, wH] )
>                   ( setNursery [wH, wH, wH, wN, wN, wH] )

> nurseryRoleTests = TestList [ idealNumNurseryTest1
>                             , idealNumNurseryTest2
>                             , setNurseryTest1 
>                             , setNurseryTest2 ]


--------------------------------------------------------------------------------

Run all the given tests

> main :: IO Counts
> main = runTestTT $ TestList [ larvaToHarvesterTests
>                             , getRoleNumbersTests
>                             , larvaTests
>                             , harvesterRoleTests
>                             , nurseryRoleTests ]
