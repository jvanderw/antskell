Copyright (c) 2012 Jess VanDerwalker. All rights reserved.

Antskell is an ant population simulator which outputs its results to a
graph so the population over time can be visualized.

> module Antskell where

A data type handle the different roles that worker ants can assume.

> data Role = Larva | Nursery | Harvester
>           deriving Show

There are three data types used to model ants, one to cover common
attributes for all ants, one for workers, and one for the queen.

> data Ant = Ant { age  :: Integer
>                , food :: Integer
>                } deriving Show

> data Worker = Worker { workerAttrs :: Ant
>                      , role        :: Role
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

Constants: 
    maxAge: The maximum age that an ant can have. Dies after
            it reaches this age.
    maxFood: The maximum amount of food an ant can have.
    foodBurnRate: The amount of food an ant uses in one day.
    
> maxAge = 20
> maxFood = 10
> foodBurnRate = 1

Age all the workers in the list and remove the ones that have hit the
max age.

> ageWorkers    :: [Worker] -> [Worker]
> ageWorkers ws = undefined --map (\x -> x{workerAttrs = Ant {age = (age(workerAttrs x) + 1), food = food(workerAttrs x)}}) ws

> incAge   :: Worker -> Worker
> incAge w = undefined