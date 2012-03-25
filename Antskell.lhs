Copyright (c) 2012 Jess VanDerwalker. All rights reserved.

Antskell is an ant population simulator which outputs its results to a
graph so the population over time can be visualized.

> module Antskell where

A data type handle the different roles that worker ants can assume.

> data Role = Larva | Nursery | Harvester
>           deriving Show

There are three data types used to model ants, one to cover common
attributes for all ants, one for workers, and one for the queen.

> data Ant = Ant { age :: Integer
>                , food :: Integer
>                , role :: Role
>                } deriving Show

> data Worker = Worker { attrs :: Ant
>                      , role :: Role
>                      } deriving Show

The queen doesn't have a role, since we know what it is. The 'maxEggs'
is the maximum number of eggsthat the queen can lay in a day - given
ideal conditions.

> data Queen = Queen { attrs :: Ant
>                    ,  maxEggs :: Integer
>                    } deriving Show

Data type for the nest

> data Nest = Nest { workers :: [Worker]
>                  , queen :: Queen
>                  } deriving Show

