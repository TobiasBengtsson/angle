{- |
Module      :  Data.Angle
Description :  Library for calculating with angles in a type-safe manner.
Copyright   :  (c) Tobias Bengtsson 2018
License     :  BSD3

Maintainer  :  bo.tobias.bengtsson@gmail.com
Stability   :  stable
Portability :  portable
-}

{-# LANGUAGE UnicodeSyntax #-}

module Data.Angle
    (
      Angle, radians, degrees
    , (∠+∠), (∠-∠), (∠*), (∠/), (∠/∠)
    , normalizeAngle
    , getRadians, getDegrees
    , sine, cosine, tangent
    , arcsine, arccosine, arctangent
    ) where

import Data.Fixed (mod')

π :: (Floating a) => a
π = pi

π2 :: (Floating a) => a
π2 = 2 * π

newtype Angle a = Angle { getAngle :: a }
  deriving (Show, Eq)

radians :: Floating a => a -> Angle a
radians = Angle

degrees :: Floating a => a -> Angle a
degrees = Angle . (* π2) . (/ 360)

-- |Adds two angles.
(∠+∠) :: (Floating a) => Angle a → Angle a → Angle a
(Angle α) ∠+∠ (Angle β) = Angle (α + β)

-- |Subtracts two angles.
(∠-∠) :: (Floating a) => Angle a → Angle a → Angle a
(Angle α) ∠-∠ (Angle β) = Angle (α - β)

-- |Multiplies an angle by a number.
(∠*) :: (Floating a) => Angle a → a → Angle a
(Angle α) ∠* x = Angle (α * x)

-- |Divides an angle by a number.
(∠/) :: (Floating a) => Angle a → a → Angle a
(Angle α) ∠/ x = Angle (α / x)

-- |Divides an angle by another angle.
(∠/∠) :: (Floating a) => Angle a → Angle a → a
(Angle α) ∠/∠ (Angle β) = α / β

-- |Normalizes an angle to between 0 and 2π rad.
normalizeAngle :: (Floating a, Real a) => Angle a → Angle a
normalizeAngle (Angle α) = Angle (α `mod'` π2)

-- |Returns an angle as radians.
getRadians :: Floating a => Angle a -> a
getRadians = getAngle

-- |Returns an angle as degrees.
getDegrees :: Floating a => Angle a -> a
getDegrees = (* 360) . (/ π2) . getAngle

-- |Calculates the sine of an angle.
sine :: (Floating a) => Angle a → a
sine (Angle α) = sin α

-- |Calculates the cosine of an angle.
cosine :: (Floating a) => Angle a → a
cosine (Angle α) = cos α

-- |Calculates the tangent of an angle.
tangent :: (Floating a) => Angle a -> a
tangent (Angle α) = tan α

-- |Calculates the inverse sine of a number (returned as an angle).
arcsine :: (Floating a) => a → Angle a
arcsine x = Angle $ asin x

-- |Calculates the inverse cosine of a number (returned as an angle).
arccosine :: (Floating a) => a → Angle a
arccosine x = Angle $ acos x

-- |Calculates the inverse tangent of a number (returned as an angle).
arctangent :: (Floating a) => a → Angle a
arctangent x = Angle $ atan x
