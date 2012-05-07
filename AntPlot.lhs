Copyright (c) 2012 Jess VanDerwalker. All rights reserved.

AntPlot.lhs

AntPlot uses the results from running Antskell functions to simulate
an ant population to plot a series of graphs to visulalize the growth
of the population over time.

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

> module AntPlot where

> import Antskell
> import System.Random

> import qualified Graphics.Gnuplot.Advanced as Plot
> import qualified Graphics.Gnuplot.Terminal.X11 as X11

> import qualified Graphics.Gnuplot.Frame.OptionSet as Opts
> import qualified Graphics.Gnuplot.Frame.OptionSet.Histogram as Histogram
> import qualified Graphics.Gnuplot.Frame as Frame
> import qualified Graphics.Gnuplot.LineSpecification as LineSpec

> import qualified Graphics.Gnuplot.Plot.TwoDimensional as Plot2D
> import qualified Graphics.Gnuplot.Graph.TwoDimensional as Graph2D
> import Graphics.Gnuplot.Plot.TwoDimensional (linearScale, )

> import Data.Monoid
> import GHC.IO.Exception

--------------------------------------------------------------------------------

Run the Antskell simulation for a given number of time steps with a
given starting nest.

> runSim        :: Int -> Nest -> [Float] -> [Nest]
> runSim x n fs = fst $ unzip $ take x $ iterate simulateNest (n, fs)

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

Plot the total population of the nest

> plotPop    :: [Nest] -> Plot2D.T Int Int
> plotPop ns = Plot2D.list Graph2D.listLines
>              (map (\x -> length (workers x)) ns)



--------------------------------------------------------------------------------

Main routine to generate desired plots.

> main :: Int -> IO ()
> main x = do
>          ns <- return (runSim x aNest (randomRs (0,1 :: Float) (mkStdGen 12)))
>          roles <-  return (plotRoles ns [Larva, Nursery, Harvester])
>          pop <- return (plotPop ns)
>          sequence_ $
>                    Plot.plot X11.cons roles :
>                    Plot.plot X11.cons pop :
>                    []