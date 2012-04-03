Copyright (c) 2012 Jess VanDerwalker. All rights reserved.

Antskell is an ant population simulator which outputs its results to a
graph so the population over time can be visualized.

> module Antskell where

> import Data.List

A data type handle the different roles that worker ants can assume.

> data Role = Larva | Nursery | Harvester
>           deriving (Eq, Ord, Show)

There are three data types used to model ants, one to cover common
attributes for all ants, one for workers, and one for the queen.

> data Ant = Ant { age      :: Integer
>                , food     :: Integer
>                } deriving (Eq, Show)

The Worker has a feeding weight associated, which is what is going to
determine the ants access to the nest's food store.

> data Worker = Worker { workerAttrs :: Ant
>                      , role        :: Role
>                      , feedingW    :: Float
>                      } deriving (Eq, Show)

The queen doesn't have a role, since we know what it is. The 'maxEggs'
is the maximum number of eggsthat the queen can lay in a day - given
ideal conditions.

> data Queen = Queen { queenAttrs :: Ant
>                    , maxEggs    :: Integer
>                    } deriving (Eq, Show)

Data type for the nest. The 'foodStore' is the total amount of food
the nest has available.

> data Nest = Nest { workers   :: [Worker]
>                  , queen     :: Queen
>                  , foodStore :: Integer
>                  } deriving (Eq, Show)

--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------

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

> weightWorkers   :: Weights -> Nest -> Nest
> weightWorkers w n = n { workers =
>                             sortBy compareWeight
>                                        (weightWorkers' w (workers n)) }
>     where compareWeight x y = compare (feedingW x) (feedingW y)

> weightWorkers'   :: Weights -> [Worker] -> [Worker]
> weightWorkers' x ws = map (weightWorker x) ws
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
> feedNest = feedWorkers . weightWorkers weightsD  . feedQueen

> feedQueen   :: Nest -> Nest
> feedQueen n = n { queen = feedQueen' (fst f) (queen n)
>                 , foodStore = foodStore (snd f) }
>     where f = foodAvailable (maxFood - (food (queenAttrs (queen n)))) n

> feedQueen'     :: Integer -> Queen -> Queen
> feedQueen' f q = Queen (Ant (age (queenAttrs q)) ((food (queenAttrs q)) + f))
>                        (maxEggs q)

Feed all the workers in the nest. Use foodRequested to determine how
much food the workers in the nest require. If they require more food
than the Nest has, then the remaining store will be zero, otherwise
remove the amount requested from the store.

> feedWorkers   :: Nest -> Nest
> feedWorkers n = n {workers = feedWorkers' (workers n) (foodStore n)
>                              , foodStore = newStore n}
>     where newStore n = if foodStore n > foodRequested n
>                        then foodStore n - foodRequested n
>                        else 0

> feedWorkers'          :: [Worker] -> Integer -> [Worker]
> feedWorkers' [] _     = []
> feedWorkers' ws 0     = ws
> feedWorkers' (w:ws) f = (fst (feedAnt w f) : feedWorkers' ws (snd (feedAnt w f)))

Feed a single Worker ant. Checks the food store to see if the ant can
be fully fed, or if it can only eat the reamining store, which may be
less than it needs. We return both the fed worker and the amount left
in the foodStore of the Nest.

> feedAnt     :: Worker -> Integer -> (Worker, Integer)
> feedAnt w f = if f > (maxFood - food (workerAttrs w))
>                 then ((Worker
>                        (Ant (age (workerAttrs w)) (maxFood))
>                        (role w) (feedingW w))
>                      , (f - (maxFood - food (workerAttrs w)) ))
>                 else ((Worker
>                        (Ant (age (workerAttrs w)) ((food (workerAttrs w)) + f))
>                        (role w) (feedingW w))
>                      , 0)

Determine the amount of food the Nest would need to fully feed all the
workers in the nest.

> foodRequested   :: Nest -> Integer
> foodRequested n = last
>                   $ scanl (\x y ->
>                                (maxFood - (food (workerAttrs y))) + x )
>                         0 (workers n)

Request a certain amount of food from the Nest stockpile. Return what
of the request can be fullfiled, and the Nest with the food returned
removed from the stockpile.
FIXME: Need to roll foodAvailable and foodRequested into one function
       that can be used by both feedQueen and feedWorkers

> foodAvailable     :: Integer -> Nest -> (Integer, Nest)
> foodAvailable r n = if r <= foodStore n
>                     then (r, n {foodStore = (foodStore n) - r})
>                     else (foodStore n, n {foodStore = 0})

--------------------------------------------------------------------------------

Determine the roles for the worker ants. This requires us to look at a
couple of different factors:

1. How many Larva are there?
   - This will determine the need for Nursery workers. The workerPerLarva
     number is used to determine if the Nest is under/overstaffed.
2. What is the status of the food store?
   - First we have to determine if the food level of the Queen, if she is
     below a certain level, then we must gather food to keep the Queen alive.
   - Ideally, there should be enough food in the store to allow one unit of
     food for every Worker.

Determine the age of all the Larva, and turn the Larva that are old
enough into Harvesters.

> larvaToHarvester    :: [Worker] -> [Worker]
> larvaToHarvester ws = map oldEnough ws
>     where oldEnough w = if role w == Larva && age (workerAttrs w) >= adultAge
>                         then w { role = Harvester }
>                         else w

Get the number of workers in each Role. Pack it in a tuple so the Role
and the number are together.

> getRoleNumbers :: [Worker] -> [(Role,Integer)]
> getRoleNumbers = map numRole
>                  . getRoleOfLists
>                  . groupBy sameRole . sortBy compareRole
>     where compareRole x y = compare (role x) (role y);
>           sameRole x y = role x == role y;
>           numRole (r,ws) = (r, fromIntegral $ length ws)

Get the role assoicated with a list a workers - this assumes that the
list only contains Workers of one Role.

> getRoleOfLists :: [[Worker]] -> [(Role, [Worker])]
> getRoleOfLists = map (\(x:xs) -> (role x, (x:xs)))


--------------------------------------------------------------------------------

Put everything together:
1. Time step the nest
   - Every ant ages and uses up some of its food store. Ants that should be
     dead are removed from the list.
2. Feed the nest.
   - Use up the food store and feed as many ants as possible.
3. Determine roles for ants

> simulateNest :: Nest -> Nest
> simulateNest = feedNest . timeStepNest

--------------------------------------------------------------------------------

Constants: 
    maxAge: The maximum age that an ant can have. Dies aftertestLarvaToHarvester3
            it reaches this age.
    maxFood: The maximum amount of food an ant can have.
    foodBurnRate: The amount of food an ant uses in one day.
    adultAge: The age a Larva can assume another role
    larvaPerNursery : Number of Larva that can be cared for by a Nursery worker.
    
> maxAge = 20
> maxFood = 10
> foodBurnRate = 1
> adultAge = 5
> larvaPerNursery = 2

A few ants for testing

> w1 = Worker (Ant 0 5) Harvester 0.0
> w2 = Worker (Ant 1 1) Harvester 0.0
> w3 = Worker (Ant 1 9) Nursery 0.0
> wls = [w1,w2,w3]

Some weights for testing

> weightsD = Weights 0.0 5.0 10.0 5.0 5.0

Finally, a nest

> aNest = Nest wls (Queen (Ant 0 10) 20) 10
