{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Data.Aviation.Cessna172.Preflight where

import Prelude
import Diagrams.Prelude(V2, local, _fontSize, rotateBy, (#), _lineWidth)
import Diagrams.Backend.Rasterific.CmdLine(B)
import Diagrams.TwoD.Text(TextAlignment(BoxAlignedText))
import Plots(tickLabelFunction, atMajorTicks, majorTicksLength, majorTicksFunction, gridLinesStyle, Axis, r2Axis, r2AxisMain, xLabel, yLabel, xMin, yMin, xMax, yMax, xAxis, yAxis, axisLabelPosition, axisLabelTextFunction, axisLabelGap, (&=), AxisLabelPosition(MiddleAxisLabel), axisLabelStyle, tickLabelStyle, scaleAspectRatio, minorGridLines, visible)
import Control.Lens((.=), (&~), (%=), (*=))

plot :: Axis B V2 Double
plot = r2Axis &~ do
          
        xLabel .= "Loaded Airplane Moment/1000 (Pounds - Inches)"
        yLabel .= "Loaded Airplane Weight (Pounds)"

        xMin .= Just 50
        yMin .= Just 1500

        xMax .= Just 130
        yMax .= Just 2600

        xAxis &= do
          scaleAspectRatio .= Just 11
          majorTicksFunction .= \_ -> [50, 60, 70, 80, 90, 100, 110, 120, 130]

        yAxis &= do
          axisLabelGap *= 2
          axisLabelTextFunction %= \f _ s -> f (BoxAlignedText 0.5 0.5) s # rotateBy (1/4)
          majorTicksFunction .= \_ -> [1500, 1600, 1700, 1800, 1900, 2000, 2100, 2200, 2300, 2400, 2500, 2600]

        axisLabelStyle . _fontSize .= local 8.5
        tickLabelStyle . _fontSize .= local 8.5
        minorGridLines . visible .= True
        gridLinesStyle . _lineWidth .= local 0.5
        tickLabelFunction .= atMajorTicks (show . round)
        axisLabelPosition .= MiddleAxisLabel

main :: IO ()
main = r2AxisMain plot