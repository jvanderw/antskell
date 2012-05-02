Copyright (c) 2012 Jess VanDerwalker. All rights reserved.

Antskell.lhs

Antskell is an ant population simulator which outputs its results to a
graph so the population over time can be visualized.

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

--------------------------------------------------------------------------------

> module Antskell where

> import Data.List
> import System.Random

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

Use an exponential distribution to determine the probabality that an
ant will die, given an age.

> probOfDeath   :: Integer -> Float
> probOfDeath x = 1 - e ** (-1/expectedLife * (fromIntegral x))
>     where e = exp 1

In order to get a random number, we're forced to use IO, since there
is no other way other than to use a random number generator that
always returns the same random numbers - i.e. mkStdGen with a given
seed.

> antDies x = do
>   y <- (randomRIO (0,1) :: IO Float)
>   if probOfDeath x > y
>   then print True
>   else print False

> getRandom :: IO Float
> getRandom = randomRIO (0, 1)

> ageAndEat   :: Worker -> Worker
> ageAndEat w = w{ workerAttrs = Ant{ age = (age(workerAttrs w) + 1)
>                                   , food = (food(workerAttrs w)
>                                             - foodBurnRate)}}

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

Have the Queen lay eggs - which means creating new Worker ants with
the role of Larva. Number of Larva created depends on two factors:

1. The number of Nursery workers.
   - If the Nursery is already understaffed, then the Queen will not
     create the full number of Larva, since there is a good chance
     that they would die from lack of care.
   - For this we use the the nurseryStaffing level, if it is above
     the minNurseryStaffing - then we can lay enough eggs to bring
     it to the minimum staffing level.

> layEggs   :: Nest -> Nest
> layEggs n = n { workers = createLarva (workers n) x }
>             where x = layEggs' n (numEggs (workers n))

> layEggs'     :: Nest -> Integer -> Integer
> layEggs' n x | x <= 0                = 0
>              | x > maxEggs (queen n) = maxEggs (queen n)
>              | otherwise             = x

Decide on the total number of eggs the Queen could lay, if any.  This
is based on the the amount of Nursery workers above the
'minNurseryStaffing' percentage there are. This can return negative
numbers, which indicates that the nursery is understaffed.

> numEggs :: [Worker] -> Integer
> numEggs ws = floor ( (n * larvaPerNursery) / minNurseryStaffing - l )
>     where rs = getRoleNumbers ws;
>           l  = fromIntegral $ numInRole Larva rs;
>           n  = fromIntegral $ numInRole Nursery rs;


Add the number of larva specified to the list of Workers.

> createLarva      :: [Worker] -> Integer -> [Worker]
> createLarva ws n = ws ++ (genericReplicate n newLarva)
>     where newLarva = Worker (Ant 0 10) Larva 0.0

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
enough into adults.

> larvaToHarvester    :: [Worker] -> [Worker]
> larvaToHarvester ws = map oldEnough ws
>     where oldEnough w = if role w == Larva && age (workerAttrs w) >= adultAge
>                         then w { role = Nursery }
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

Find the number of Workers assigned to a given role.

> numInRole      :: Role -> [(Role,Integer)] -> Integer
> numInRole r rs = num $ filter (\x -> fst x == r) rs
>     where num xs =  case xs of
>                             [] -> 0
>                             (y:ys) -> snd y

Setting the number of Harvesters:

Check the food store, and compare that to the number of Workers set to
be Havrvesters. If it is too low, switch some Nursery workers to the Harvesters

Find the ideal number of worker that should be occupying the Harvester role.

> idealNumHarvs      :: [Worker] -> Integer -> Integer
> idealNumHarvs ws f = ceiling
>                      (fromIntegral
>                       (fromIntegral (length ws) * foodBurnRate - f)
>                       / fromIntegral harvestPerWorker)

Get the difference between our ideal number, and the the actual number
of Harvesters. Then set as many Nursery workers as we can to meet that
number.

> setHarvs      :: [Worker] -> Integer -> [Worker]
> setHarvs ws f = if x < idealNumHarvs ws f
>                 then setHarvs' ws (idealNumHarvs ws f - x)
>                 else ws
>     where x = numInRole Harvester (getRoleNumbers ws);

> setHarvs'          :: [Worker] -> Integer -> [Worker]
> setHarvs' [] _                 = []
> setHarvs' (w:ws) n | n <= 0    = (w:ws)
>                    | otherwise = (fst t : setHarvs' ws (snd t))
>     where t = if role w == Nursery
>               then (w { role = Harvester }, n - 1)
>               else (w, n)

Check the number of Larva and Nursery workers. Find the ideal number,
and change Harvesters to Nursery workers if needed. Need to add the 1
so that we can create a situation where the queen can at least lay
some eggs every turn.

> idealNumNursery    :: [Worker] -> Integer
> idealNumNursery ws = ceiling (l/larvaPerNursery) + 1
>     where l = fromIntegral $ numInRole Larva (getRoleNumbers ws)

> setNursery    :: [Worker] -> [Worker]
> setNursery ws = if x < idealNumNursery ws
>                 then setNursery' ws (idealNumNursery ws - x)
>                 else ws
>     where x = numInRole Nursery (getRoleNumbers ws);

FIXME: setNursery' and setHarvs' are very similar, could be roled into
one function.

> setNursery'      :: [Worker] -> Integer -> [Worker]
> setNursery' [] _                 = []
> setNursery' (w:ws) n | n <= 0    = (w:ws)
>                      | otherwise = (fst t : setNursery' ws (snd t))
>     where t = if role w == Harvester
>               then (w { role = Nursery }, n - 1)
>               else (w, n)

Set the roles for the nest. Possible problems with this technique is
that the need for Harvesters will always override the need for Nursery
workers - which may not always be what is desired.

> setRoles   :: Nest -> Nest
> setRoles n = n { workers = setHarvs
>                  (setNursery (larvaToHarvester (workers n))) (foodStore n) }

--------------------------------------------------------------------------------

Gather food for the nest. Every Harvester will simply bring back the
amount of food specified by 'harvestPerWorker'. Later, this will be
limited by the environment, which will have a limited amount of food
and a regeneration rate at which new food is created.

> gatherFood :: Nest -> Nest
> gatherFood n = n { foodStore = (foodStore n) + f }
>     where f = (numInRole Harvester (getRoleNumbers (workers n)))
>                * harvestPerWorker

--------------------------------------------------------------------------------

Kill off some Larva if there are not enough Nursery workers to care for them.
FIXME: Should try and remove the youngest first, gives a better chance of 
creating new workers.

> killUnattendLarva :: Nest -> Nest
> killUnattendLarva n = n { workers = removeLarva (workers n) }

> removeLarva :: [Worker] -> [Worker]
> removeLarva ws = removeLarva' ws x
>     where x = if numEggs ws < 0
>               then abs (numEggs ws)
>               else 0

> removeLarva'          :: [Worker] -> Integer -> [Worker]
> removeLarva' [] _     = []
> removeLarva' ws 0     = ws
> removeLarva' (w:ws) n = if role w == Larva
>                         then removeLarva' ws (n - 1)
>                         else (w : removeLarva' ws n)

--------------------------------------------------------------------------------

Put everything together:
1. Time step the nest
   - Every ant ages and uses up some of its food store. Ants that should be
     dead are removed from the list.
2. Feed the nest.
   - Use up the food store and feed as many ants as possible.
3. Determine roles for ants
4. Lay eggs
5. Gather food
6. Kill off unattended Larva

> simulateNest :: Nest -> Nest
> simulateNest = killUnattendLarva . gatherFood . layEggs . setRoles . feedNest . timeStepNest

--------------------------------------------------------------------------------

Use a main with IO so that we can introduce 'random' numbers to model ant death.

> main   :: Integer -> IO ()
> main x = do 
>   print "Hello"

--------------------------------------------------------------------------------

Constants:
    FIXME: These should be handled in some kind of data type that can be
           "fed" in.
    maxAge: The maximum age that an ant can have. Dies after
            it reaches this age.
    maxFood: The maximum amount of food an ant can have.
    foodBurnRate: The amount of food an ant uses in one day.
    adultAge: The age a Larva can assume another role
    larvaPerNursery : Number of Larva that can be cared for by a
                      Nursery worker.
    harvestPerWorker : The amount of food one Worker can collect in
                       one time step.
    
> maxAge = 20
> expectedLife = 30
> maxFood = 10
> foodBurnRate = 1::Integer
> adultAge = 5
> larvaPerNursery = 2
> minNurseryStaffing = 0.8
> harvestPerWorker = 3::Integer

A few ants for testing

> w1 = Worker (Ant 5 5) Harvester 0.0
> w2 = Worker (Ant 5 5) Harvester 0.0
> w3 = Worker (Ant 5 9) Nursery 0.0
> w4 = Worker (Ant 1 9) Larva 0.0
> wls = [w1,w2,w3,w4]

Some weights for testing

> weightsD = Weights 0.0 5.0 10.0 5.0 5.0

Finally, a nest

> aNest = Nest wls (Queen (Ant 0 10) 20) 10
