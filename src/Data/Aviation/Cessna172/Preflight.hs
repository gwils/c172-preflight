{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Aviation.Cessna172.Preflight where

import Prelude
import Diagrams.Prelude(V2, local, _fontSize, rotateBy, (#))
import Diagrams.Backend.Rasterific.CmdLine(B)
import Diagrams.TwoD.Text(TextAlignment(BoxAlignedText))
import Plots(Axis, r2Axis, r2AxisMain, xLabel, yLabel, xMin, yMin, xMax, yMax, xAxis, yAxis, axisLabelPosition, axisLabelTextFunction, (&=), AxisLabelPosition(MiddleAxisLabel), axisLabelStyle, tickLabelStyle, scaleAspectRatio, minorGridLines, visible)
import Control.Lens((.=), (&~), (%=))


----

plot :: Axis B V2 Double
plot =
  r2Axis &~ do
        
    xLabel .= "Loaded Airplane Moment/1000 (Pounds - Inches)"
    yLabel .= "Loaded Airplane Weight (Pounds)"

    xMin .= Just 50
    yMin .= Just 1500

    xMax .= Just 130
    yMax .= Just 2600

    xAxis &= do
      axisLabelPosition .= MiddleAxisLabel
      axisLabelStyle . _fontSize .= local 8.5
      tickLabelStyle . _fontSize .= local 8.5
      scaleAspectRatio .= Just 11
      minorGridLines . visible .= True

    yAxis . axisLabelTextFunction %= \f _ s -> f (BoxAlignedText 0.5 0.5) s # rotateBy (1/4)
    yAxis &= do
      axisLabelPosition .= MiddleAxisLabel
      axisLabelStyle . _fontSize .= local 8.5
      tickLabelStyle . _fontSize .= local 8.5
      minorGridLines . visible .= True

main :: IO ()
main = r2AxisMain plot
