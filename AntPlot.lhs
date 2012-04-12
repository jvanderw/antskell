Copyright (c) 2012 Jess VanDerwalker. All rights reserved.

AntPlot.lhs

AntPlot uses the results from running Antskell functions to simulate
an ant population to plot a series of graphs to visulalize the growth
of the population over time.

> module AntPlot where

> import Antskell

> import qualified Graphics.Gnuplot.Advanced as Plot
> import qualified Graphics.Gnuplot.Terminal.X11 as X11

> import qualified Graphics.Gnuplot.Frame.OptionSet as Opts
> import qualified Graphics.Gnuplot.Frame as Frame
> import qualified Graphics.Gnuplot.LineSpecification as LineSpec

> import qualified Graphics.Gnuplot.Plot.TwoDimensional as Plot2D
> import qualified Graphics.Gnuplot.Graph.TwoDimensional as Graph2D
> import Graphics.Gnuplot.Plot.TwoDimensional (linearScale, )

> import Data.Monoid

--------------------------------------------------------------------------------

Run the Antskell simulation for a given number of time steps with a
given starting nest.

> runSim     :: Int -> Nest -> [Nest]
> runSim x n = take x $ iterate simulateNest n

Create a list of the number of members of a given Role.

> getRoleList :: [Nest] -> Role -> [Integer]
> getRoleList ns r = map (\x -> roleFromNest x) ns
>     where roleFromNest n = numInRole r (getRoleNumbers (workers n))

Plot a given Role over a list of Nests.

> plotRole      :: [Nest] -> Role -> Plot2D.T Int Integer
> plotRole ns r = Plot2D.list Graph2D.listLines
>                 (getRoleList ns r)

Plot the Roles in the list

> plotRoles       :: [Nest] -> [Role] -> Frame.T (Graph2D.T Int Integer)
> plotRoles ns rs = Frame.cons Opts.deflt $ mconcat
>                   $ map (\(role,xs) ->
>                              fmap (Graph2D.lineSpec
>                                    (LineSpec.title (show role)
>                                    $ LineSpec.deflt))
>                                       $ Plot2D.list Graph2D.listLines xs)
>                         $ map (\x -> (x, getRoleList ns x)) rs


--------------------------------------------------------------------------------

Main routine to generate desired plots.

--> main :: IO

> main = Plot.plot X11.cons $ plotRoles (runSim 100 aNest) [Larva, Nursery, Harvester]