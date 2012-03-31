Copyright (c) 2012 Jess VanDerwalker. All rights reserved.

Antskell is an ant population simulator which outputs its results to a
graph so the population over time can be visualized.

> module Antskell where

A data type handle the different roles that worker ants can assume.

> data Role = Larva | Nursery | Harvester
>           deriving Show

There are three data types used to model ants, one to cover common
attributes for all ants, one for workers, and one for the queen.

> data Ant = Ant { age      :: Integer
>                , food     :: Integer
>                } deriving Show

The Worker has a feeding weight associated, which is what is going to
determine the ants access to the nest's food store.

> data Worker = Worker { workerAttrs :: Ant
>                      , role        :: Role
>                      , feedingW    :: Float
>                      } deriving Show

The queen doesn't have a role, since we know what it is. The 'maxEggs'
is the maximum number of eggsthat the queen can lay in a day - given
ideal conditions.

> data Queen = Queen { queenAttrs :: Ant
>                    , maxEggs    :: Integer
>                    } deriving Show

Data type for the nest. The 'foodStore' is the total amount of food
the nest has available.

> data Nest = Nest { workers   :: [Worker]
>                  , queen     :: Queen
>                  , foodStore :: Integer
>                  } deriving Show

Age and use up some of the food for all the workers in the list and
remove the ones that have hit the max age, or have starved.

> timeStepWorkers :: [Worker] -> [Worker]
> timeStepWorkers = filter notDead . map ageAndEat

> notDead   :: Worker -> Bool
> notDead w = if ((age(workerAttrs w)) > maxAge) || ((food(workerAttrs w)) <= 0)
>             then False
>             else True

> ageAndEat   :: Worker -> Worker
> ageAndEat w = w{ workerAttrs = Ant{ age = (age(workerAttrs w) + 1)
>                                , food = (food(workerAttrs w)
>                                          - foodBurnRate)}}

Use up food in the queen's stockpile.

> timeStepQueen   :: Queen -> Queen
> timeStepQueen q = q{ queenAttrs = Ant { age = (age(queenAttrs q))
>                                       , food = (food(queenAttrs q)
>                                                 - foodBurnRate)}}

Time step the entire nest.

> timeStepNest :: Nest -> Nest
> timeStepNest n = n{ workers = timeStepWorkers (workers n)
>                   , queen = timeStepQueen (queen n) }

Feed the ants from the stockpile. The ants are weighted and the lower
the weight, the more likely the ant is going to be fed. The queen is a
special case and not considered here.

Things that effect the weight:
1. The role. Eggs/Larva have the lowest weight, then Nusery, with
   Workers at the top.
2. Hunger. Ants with a lower food level have a lower weight.
3. Age. Older ants have a higher weight.

A data type is used to hold the weight factors

> data Weights = Weights { roleLarva     :: Float
>                        , roleNursery   :: Float
>                        , roleHarvester :: Float
>                        , hungerW       :: Float
>                        , ageW          :: Float
>                        } deriving Show


> weightWorkers   :: Weights -> [Worker] -> [Worker]
> weightWorkers x ws = map (weightWorker x) ws
>     where weightWorker x w = w { feedingW = ((roleWeight x (role w))
>                                              + ((hungerW x)
>                                                 * fromIntegral (food (workerAttrs w)))
>                                              + ((ageW x)
>                                                 * fromIntegral (age (workerAttrs w)))) };
>           roleWeight x r = case r of
>                              Larva     -> roleLarva x
>                              Nursery   -> roleNursery x
>                              Harvester -> roleHarvester x

> feedNest :: Nest -> Nest
> feedNest = feedWorkers . feedQueen

> feedQueen   :: Nest -> Nest
> feedQueen n = n { queen = feedQueen' (fst f) (queen n)
>                 , foodStore = foodStore (snd f) }
>     where f = foodAvailable (maxFood - (food (queenAttrs (queen n)))) n

> feedQueen'     :: Integer -> Queen -> Queen
> feedQueen' f q = Queen (Ant (age (queenAttrs q)) ((food (queenAttrs q)) + f))
>                        (maxEggs q)

> feedWorkers   :: Nest -> Nest
> feedWorkers n = n {workers = feedWorkers' n (workers n)}

> feedWorkers'      :: Nest -> [Worker] -> [Worker]
> feedWorkers' n ws = undefined
>     where t = splitAt numCanFeed n

> feedWorkers2   :: Nest -> Nest
> feedWorkers2 n = n { workers = fst t
>                    , foodStore = snd t }
>     where t = feedWorkers2' (workers n, foodStore n)

> feedWorkers2          :: [Worker] -> Integer -> [Worker]
> feedWorkers2 [] _     = []
> feedWorkers2 xs 0     = xs
> feedWorkers2 (x:xs) f = (feedAnt x : feedWorkers2 xs (f - (food (workerAttrs x) - maxFood) )

Get the number of ants in list that can be fed, and the amount of food
out of the food store that will be used.

> numCanFeed      :: Nest -> (Integer, Integer)
> numCanFeed n = (fromIntegral (length fs), last fs)
>     where fs = takeWhile (\x -> x <= foodStore n)
>                   $ scanl (\x y -> (maxFood - (food (workerAttrs y))) + x ) 0
>                     (workers n)

Request a certain amount of food from the Nest stockpile. Return what
of the request can be fullfiled, and the Nest with the food returned
removed from the stockpile.

> foodAvailable     :: Integer -> Nest -> (Integer, Nest)
> foodAvailable r n = if r <= foodStore n
>                     then (r, n {foodStore = (foodStore n) - r})
>                     else (foodStore n, n {foodStore = 0})

Determine the roles for the work ants


Constants: 
    maxAge: The maximum age that an ant can have. Dies after
            it reaches this age.
    maxFood: The maximum amount of food an ant can have.
    foodBurnRate: The amount of food an ant uses in one day.
    
> maxAge = 20
> maxFood = 10
> foodBurnRate = 1

A few ants for testing

> w1 = Worker (Ant 0 5) Harvester 0.0
> w2 = Worker (Ant 1 1) Harvester 0.0
> w3 = Worker (Ant 1 9) Nursery 0.0
> wls = [w1,w2,w3]

Some weights for testing

> weightsD = Weights 0.0 5.0 10.0 5.0 5.0

Finally, a nest

> aNest = Nest wls (Queen (Ant 0 10) 20) 10
